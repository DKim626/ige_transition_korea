#### ==== create code ====
#' Create family relation codes of father and mother of the respondent
#'
#' This code will create the family relation code of respondents' parents
#' - parent_code_h: family relation code of father of the respondent
#' - parent_code_s: family relation code of mother of the respondent
#' These two codes will help track parent-child relationship if they have lived together within the survey waves. If the respondent and their parents/childs had never lived together, will return NA.
#'
#' @param data individual level KLIPS data
#'
#' @return tibble with parent_code_h and parent_code_s
create_code <- function (data) {
  data <- data %>%
    select(year, pid, fam_rel, fam_rel_code, contains('hid')) %>%
    mutate(
      # fam_rel_code records the relationship with household head with digits:
      # - For (grand)parents of head/spouse, it is recorded with single digit.
      #   + Grandfather and grandmother of head: 1 and 2, respectively.
      #   + Grandfather and grandmother of spouse: 3 and 4.
      #   + Father and mother of head: 5 and 6.
      #   + Father and mother of spouse: 7 and 8.
      # - Head has code of 10, their siblings have code starting with 3.
      #   + For example, fifth sibling has code of 35.
      # - For spouses, either 1 or 2 is added at the first digit of the code.
      #   + Generally, 1 is added.
      #     * For example, head and spouse's code are 10 and 20, respectively.
      #     * Third child and their spouse would have the code of 13 and 23.
      #   + For siblings of head/spouse, 2 is added
      #     * For example, head's first sibling and their spouse would be 31, 51.
      #     * Spouse's second sibling and their spouse would be 42, 62.
      # - For children, it is denoted by the number after their parents
      #   + For example, if the parents have the code of 111, then their first and second child's fam_rel_code will be 1111 and 1112.
      parent_code_h = fcase(
        fam_rel == "Children", '10',
        fam_rel == 'Grandchildren', stri_sub(fam_rel_code, 1, 2),
        fam_rel %in% c('Head', "Head's Sibling"), '5',
        fam_rel %in% c('Spouse', "Spouse's Sibling"), '7',
        fam_rel == "Grand-grand children", stri_sub(fam_rel_code, 1, 3),
        fam_rel == 'Nephew', stri_sub(fam_rel_code, 1, 2)
      ),
      parent_code_s = fcase(
        fam_rel %in% c('Head', "Head's Sibling"), '6',
        fam_rel %in% c('Spouse', "Spouse's Sibling"), '8',
        fam_rel %in% c('Children', 'Grandchildren', 'Grand-grand children'),
        stri_replace_all_regex(parent_code_h, '^1', '2'),
        fam_rel == 'Nephew' & stri_detect_regex(parent_code_h, '^3'),
        stri_replace_all_regex(parent_code_h, '^3', '5'),
        fam_rel == 'Nephew' & stri_detect_regex(parent_code_h, '^4'),
        stri_replace_all_regex(parent_code_h, '^4', '6')
      ),
      fam_rel_code = as.character(fam_rel_code)
    )
  
  data <- select(data, year, pid, fam_rel, fam_rel_code,
                 parent_code_h, parent_code_s, everything()
  )
  
  return (data)
}

#### ==== match_key_parent_child ====
#' Match parent-child relationship by year
#'
#' This function will concatenate parent with child per year
#' - pid: personal ID for the respondent
#' - pid_child: personal ID of the respondent's child
#' 
#' @param wave survey wave (numeric)
#' @param data individual level KLIPS data
#'
#' @return tibble with parent-child match for one wave
match_year_parent_child <- function(wave, data) {
  assert_that(is.numeric(wave))
  if (!(wave %in% 1:26)) stop ("Survey wave out of bound")
  survey_year <- 1997 + wave
  survey_wave <- ifelse(stri_length(wave) == 2,
                        as.character(wave), paste0(0, wave)
  )
  
  hhid_year <- paste0('hhid', survey_wave)
  sym_hhid <- sym(hhid_year)
  
  data <- filter(data, year == survey_year)
  
  base <- select(data, year, !!sym_hhid, pid, fam_rel_code)
  parent_h <- select(data, year, !!sym_hhid, pid, parent_code_h) %>%
    filter(!is.na(parent_code_h))
  parent_s <- select(data, year, !!sym_hhid, pid, parent_code_s) %>%
    filter(!is.na(parent_code_s))
  
  result <- base %>%
    left_join(parent_h,
              by = c('year', hhid_year, 'fam_rel_code' = 'parent_code_h'),
              suffix = c('', '_child_h')
    ) %>%
    left_join(parent_s,
              by = c('year', hhid_year, 'fam_rel_code' = 'parent_code_s'),
              suffix = c('', '_child_s')
    ) %>%
    mutate(
      pid_child = fcase(
        !is.na(pid_child_h), pid_child_h,
        !is.na(pid_child_s), pid_child_s
      )
    ) %>%
    select(year, pid, pid_child) %>%
    filter(!is.na(pid_child))
  
  return (result)
}
#### ==== track_parent_child ====
#' Track parent-child relationship across survey waves
#' 
#' This function will concatenate parent with child across survey waves
#' - pid: personal ID for the respondent
#' - pid_child: personal ID of the respondent's child
#' Note that if the respondent had never lived with their parent/child within the survey waves, it cannot be tracked with this function. For example, if one of the household member is a cohabitant, it will be impossible to track their parents whatsoever.
#' 
#' @param start_wave start of survey wave (e.g. start_wave = 1)
#' @param end_wave end of survey wave (e.g. end_wave = 26)
#' @param data individual level KLIPS data
#' 
#' @return tibble with PID of parent and child side by side
track_parent_child <- function(start_wave, end_wave, dir_path) {
  on.exit(gc())
  assert_that(is.numeric(start_wave) & is.numeric(end_wave))
  if (!all(c(start_wave, end_wave) %in% 1:26)) {
    stop ("[ERROR] Wave out of bound: start_wave = ", start_wave,
          ', end_wave = ', end_wave)
  }
  
  message("[INFO] Creating key")
  
  data <- readRDS(here::here('data/KLIPS/clean/clean_klips_individual.rds'))
  key_base <- create_code(data)
  
  safe_match <- safely(match_year_parent_child)
  
  message("[INFO] Extracting parent-child relationship by each year")
  results <- map(start_wave:end_wave,
                 ~ safe_match(.x, key_base)
  )
  
  names(results) <- start_wave:end_wave
  
  failure <- map(results, 'error') %>% keep(~ !is.null(.x))
  
  if (length(failure) > 0) {
    message("[ERROR] Failure in match at following year")
    walk2(names(failure), failure,
          ~ message(' - ', .x, ': ', .y$message))
  }
  
  result <- map_dfr(results, 'result')
  
  save_overwrite_rds(result, dir_path, 'relation_key.rds', overwrite = TRUE)
  
  message("[DONE] Extracted parent-child key")
  return(result)
}
