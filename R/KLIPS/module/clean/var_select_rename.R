source(here::here('R', 'KLIPS', 'module', 'clean', 'var_map.R'), local = environment())

#### ==== select_var ====
#' Selects KLIPS variables
#' 
#' This function returns tibble with selected variables only
#' 
#' @param data KLIPS tibble3
#' @param level unit of observation
#'
#' @return tibble
#'
#' @example
#' select_var(KLIPS_2023, 'household')
#' select_var(KLIPS_2021, 'individual')
#' @export
select_klips_var <- function(data, level) {
  assert_that(is.character(level))
  if(!(level %in% c('household', 'individual', 'supplementary'))) {
    stop('Invalid level name.')
  }
  
  if (level == 'household') {
    mapping <- mapping_house_var
    list_var <- mapping[!stri_detect_regex(mapping, '[sn]?w')]
    list_weight <- mapping[stri_detect_regex(mapping, '[sn]?w')]
    
    pattern_var <- paste0('(', paste0(list_var, collapse = '|'), ')')
    pattern_weight <- paste0('(', paste0(list_weight, collapse = '|'), ')')
    
    data <- select(data,
      # Household ID
      matches('^orghid'), # Original Household ID
      matches('^hhid'), # Household ID of each year
      matches(paste0('h[0-9]{2}', pattern_var)),
      matches(paste0(pattern_weight, '[0-9]{2}h'))
    )
  } else if (level == 'individual') {
    mapping <- mapping_ind_var
    list_var <- mapping[!stri_detect_regex(mapping, '[sn]?w')]
    list_weight <- mapping[stri_detect_regex(mapping, '[sn]?w')]
    
    pattern_var <- paste0('(', paste0(list_var, collapse = '|'), ')')
    pattern_weight <- paste0('([sn]?w|', paste0(list_weight, collapse = '|'), ')')
    
    data <- select(data,
      # ID Number
      pid, matches('orghid'), matches('hhid'),
      matches(paste0('p[0-9]{2}', pattern_var)),
      matches(paste0(pattern_weight, '[0-9]{2}p'))
    )
  } else {
    pattern_var <- paste0(mapping_supp_var, collapse = '|')
    
    data <- select(data, pid,
                   matches(paste0('p[0-9]{2}', pattern_var))
    )
  }
  
  return(data)
}

#### ==== rename_var ====
#' Renames KLIPS variables
#' 
#' This function returns tibble with renamed columns
#' 
#' @param data KLIPS tibble
#' @param level unit of observation
#' @param survey_year four digit year of survey
#' 
#' @return tibble
#' 
#' @example
#' rename_var(KLIPS_2023, 'household'. 2023)
#' rename_var(KLIPS_2022, 'individual', 2022)
#' @export
rename_klips_var <- function(data, level, survey_year) {
  # 1. Tests whether parameters have intended values
  assert_that(is_tibble(data))
  assert_that(is.character(level))
  assert_that(is.numeric(survey_year))
  if(!(level %in% c('household', 'individual', 'supplementary'))) {
    stop('Invalid level name.')
  }
  
  # 2. Decides which mapping to use
  mapping <- if(level == 'household') {
    mapping_house_var
    } else if(level == 'individual') {
      mapping_ind_var
    } else mapping_supp_var
  
  # 3. Handle for specific year issues
  # - 1998, 2009, and 2018 are years that new samples are added
  #   - Does not have longitudinal weights (with suffix '_l')
  #   - Cross-sectional weights does not have suffix '_c'
  if(level == 'individual') {
    if(survey_year == 1998){
      colnames(data) <- stri_replace_all_fixed(colnames(data), 'w01p', 'w01p_c')
      data$w01p_l <- data$w01p_c
    } else if(survey_year == 2009){
      colnames(data) <- stri_replace_all_fixed(colnames(data), 'sw12p', 'sw12p_c')
      data$sw12p_l <- data$sw12p_c
    } else if(survey_year == 2018){
      colnames(data) <- stri_replace_all_fixed(colnames(data), 'nw21p', 'nw21p_c')
      data$nw21p_l <- data$nw21p_c
    }
  }
  
  # 4. Standardizes variable name before mapping is used
  colnames(data) <- stri_replace_first_regex(colnames(data), '[hp][0-9]{2}', '')
  colnames(data) <- stri_replace_first_regex(colnames(data),
                                             '[hp][0-9]{2}[hp]', '')
  
  # 5. Renames columns using mapping
  data <- rename(data, any_of(mapping))
  
  # 6. Return final tibble
  return(data)
}

