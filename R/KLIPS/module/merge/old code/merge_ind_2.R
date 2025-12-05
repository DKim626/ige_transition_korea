
source(here::here('R/module/merge/extract_data.R'), local = environment())

#### ==== calc_avg_inc ====
#' Calculate average pre/post-tax labor income
#' 
#' @param data tibble of extracted child/parent data
#' 
#' @return tibble with average pre/post-tax labor income
calc_avg_inc <- function(data) {
  # === 1. Calculate average labor income, both pre/post tax ===
  avg_inc <- data %>%
    select(pid, last_income_labor_pre, last_income_labor_post) %>%
    group_by(pid) %>%
    summarize(across(everything(), ~ mean(.x, na.rm = TRUE))) %>%
    mutate(
      across(contains('income'),
             ~ ifelse(is.nan(.x), 0, .x)))
  
  # === 2. Extract latest information available ===
  latest_info <- data %>%
    arrange(desc(year)) %>%
    group_by(pid) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    select(pid, sex, age, educ_self, place_curr,
           employ_stat, employ_type, employ_regular, employ_position)
  
  # === 3. Join the results and return ===
  result <- right_join(latest_info, avg_inc, by = 'pid')
  return (result)
}

#### ==== merge_ind ====
#' Merge individual level data of parent and child
#'
#' This function will extract and merge children and parent data, with average pre/post-tax labor income and demographic variables such as gender, age, education level, current address, employment releavant variables, parental education level, and place of birth and at age 14.
#' 
#' @param child_min_age minimum age of children at the year of basis (here, child_max_year)
#' @param child_max_age maximum age of children at the year of basis (here, child_max_year)
#' @param parent_min_age minimum age of parent to be included in the sample
#' @param parent_max_age maximum age of parent to be included in the sample
#' @param child_min_year minimum year of children data
#' @param child_max_year maximum year of children data
#' @param parent_min_year minimum year of parent data
#' @param parent_max_year maximum year of parent data
#' @param dir_path directory for saving merged data (default: data/merge)
#'
#' @return tibble
merge_ind <- function(child_base_age,
                      child_min_age, child_max_age,
                      parent_min_age, parent_max_age,
                      child_min_year, child_max_year,
                      parent_min_year, parent_max_year, relation_key,
                      dir_path) {
  on.exit(gc())
  # === Input validation ===
  assert_that(is.numeric(child_min_year) & is.numeric(child_max_year),
              msg = paste0("[ERROR] Invalid range of years given to ",
                           "child_min_year and/or child_max_year"))
  assert_that(is.numeric(parent_min_year) & is.numeric(parent_max_year),
              msg = paste0("[ERROR] ",
                           "parent_min_year and/or parent_max_year not numeric"))
  assert_that(is.numeric(parent_min_age) & is.numeric(parent_max_age),
              msg = "[ERROR] parent_max_age and/or parent_max_age type is not numeric")
  
  # === Load individual level KLIPS data ===
  data <- readRDS(here('data/clean/clean_klips_individual.rds'))
  
  # === Extract list of children within the given range ===
  list_child <- extract_child_list(data, child_min_age, child_max_age,
                                   child_max_year)
  
  # === Extract the mapping between parent and child within the sample ===
  key_parent_child <- filter(relation_key, pid_child %in% list_child)
  
  # === Extract children and parent data ===
  data_child <- extract_child_data(data, child_min_year, child_max_year,
                                   list_child, dir_path)
  
  data_parent <- extract_parent_data(data, parent_min_year, parent_max_year,
                                     parent_min_age, parent_max_age,
                                     key_parent_child, dir_path)
  
  list_parent <- pull(data_parent, pid) %>% unique
  
  key_parent_child <- filter(key_parent_child, pid %in% list_parent)
  
  # === Filter children data to match parent for later use ===
  data_child <- filter(data_child, pid %in% key_parent_child$pid_child)
  
  # === Save the intermediate results for later use ===
  if (!dir.exists(here::here(dir_path))) {
    message('[INFO] Directory created: ', dir_path)
    dir.create(here::here(dir_path))
  }
  
  file_path <- file.path(dir_path, 'data_child.rds')
  msg <- if(!file.exists(here::here(file_path))) "Saved" else "Overwriting"
  message("[SAVE] ", msg, ' children data at ', file_path)
  saveRDS(data_child, file = here::here(file_path))
  
  file_path <- file.path(dir_path, 'data_parent.rds')
  msg <- if(!file.exists(here::here(file_path))) "Saved" else "Overwriting"
  message("[SAVE] ", msg, ' parent data at ', file_path)
  saveRDS(data_parent, file = here::here(file_path))
  
  file_path <- file.path(dir_path, 'map_parent_child.rds')
  msg <- if(!file.exists(here::here(file_path))) "Saved" else "Overwriting"
  message("[SAVE] ", msg, ' parent-child map at ', file_path)
  saveRDS(key_parent_child, file = here::here(file_path))
  
  # === Merge the supplementary data ===
  data_supp <- readRDS(file = 'data/clean/clean_klips_supp.rds')
  
  data_child_avg <- calc_avg_inc(data_child)
  data_parent_avg <- calc_avg_inc(data_parent)
  
  
  data_child_avg <- left_join(data_child_avg, data_supp, by = 'pid')
  data_parent_avg <- left_join(data_parent_avg, data_supp, by = 'pid')
  
  result <- left_join(data_parent_avg, key_parent_child, by = 'pid') %>%
    left_join(data_child_avg,
              by = c('pid_child' = 'pid'),
              suffix = c('_parent', '_child'))
  
  # === Save the results ===
  if (!dir.exists(here::here(dir_path))) {
    message('[INFO] Directory created: ', dir_path)
    dir.create(here::here(dir_path))
  }
  
  file_path <- file.path(dir_path, 'data_merge_ind.rds')
  msg <- if (!file.exists(here::here(file_path))) "Saved" else "Overwriting"
  message('[SAVE] ', msg, ' individual level merged data at ', file_path)
  saveRDS(result, file = here::here(file_path))
  
  # === Return the results ===
  return(result)
}
