source(here::here('R/module/utils/get_util.R'), local = environment())
source(here::here('R/module/merge/extract_data.R'), local = environment())
source(here::here('R/module/merge/extract_hhid.R'), local = environment())

#### ==== calc_house_avg ====
#'
#'
#'
#'
#'
#'
calc_house_avg <- function (data, col = NULL) {
  on.exit(gc())
  col_sym <- if(!is.null(col)) sym(col) else NULL
  
  # Extract hhid from the data
  years <- min(data$year):max(data$year)
  hhid_data <- map_dfr(years, ~ extract_hhid(data, .x))
  
  # Extract latest information
  latest_info <- data %>%
    select(year, pid, sex, age, educ_self, place_curr,
           employ_stat, employ_type, employ_regular, employ_position) %>%
    extract_latest()
  
  col_g <- if(!is.null(col)) quos(hhid, !!col_sym) else quos(hhid)
  
  # Merge sample weights
  waves <- years - 1997
  weights <- gather_weight(data, 'individual',
                           waves[1], waves[length(waves)], type = 'c')
  
  # Merge hhid onto original data set
  data <- data %>%
    {
      if (!is.null(col_sym)) select(., year, pid, !!col_sym,
                                last_income_labor_pre, last_income_labor_post)
      else select(., year, pid, last_income_labor_pre, last_income_labor_post)
    } %>%
    left_join(hhid_data, by = c('year', 'pid'),
              relationship = 'many-to-many') %>%
    left_join(weights, by = c('year', 'hhid', 'pid'))
  
  
  
  # Calculate average labor income
  avg_inc <- data %>%
    {
      if (!is.null(col_sym)) select(., year, hhid, pid, !!col_sym,
                                    last_income_labor_pre,
                                    last_income_labor_post,
                                    w_p_c, sw_p_c, nw_p_c)
      else select(., year, hhid, pid,
                  last_income_labor_pre, last_income_labor_post,
                  w_p_c, sw_p_c, nw_p_c)
    } %>%
    mutate(
      across(c(year, last_income_labor_pre, last_income_labor_post),
             ~ ifelse(is.na(.x), 0, .x))
    ) %>%
    group_by(!!!col_g) %>%
    summarize(
      avg_labor_pre_w = weighted.mean(last_income_labor_pre,
                                      w_p_c, na.rm = TRUE),
      avg_labor_pre_sw = weighted.mean(last_income_labor_pre,
                                       sw_p_c, na.rm = TRUE),
      avg_labor_pre_nw = weighted.mean(last_income_labor_pre,
                                       nw_p_c, na.rm = TRUE),
      avg_labor_post_w = weighted.mean(last_income_labor_pre,
                                       w_p_c, na.rm = TRUE),
      avg_labor_post_sw = weighted.mean(last_income_labor_pre,
                                        sw_p_c, na.rm = TRUE),
      avg_labor_post_nw = weighted.mean(last_income_labor_pre,
                                        nw_p_c, na.rm = TRUE),
      .groups = 'drop'
    )
  
  # Merge the data
  
  result <- data %>%
    {
     if (!is.null(col_sym)) select(., hhid, pid, !!col_sym)
      else select(., hhid, pid)
    } %>%
    unique() %>%
    left_join(avg_inc, by = c('hhid', if (!is.null(col)) col)) %>%
    left_join(latest_info, by = 'pid')
  
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
#' @param dir_path directory for saving merged data (default: data/merge)
#'
#' @return tibble
merge_ind <- function(child_base_age, age_bandwidth,
                      child_min_age, child_max_age,
                      parent_min_age, parent_max_age,
                      child_base_year,
                      relation_key, dir_path) {
  on.exit(gc())
  # === Input validation ===
  assert_that(is.numeric(child_base_year),
              msg = paste0("[ERROR] Invalid range of years given to ",
                           "child_base_year"))
  assert_that(is.numeric(parent_min_age) & is.numeric(parent_max_age),
              msg = "[ERROR] parent_min_age and/or parent_max_age type is not numeric")
  
  # === Load individual level KLIPS data ===
  data <- readRDS(here('data/clean/clean_klips_individual.rds'))
  
  # === Extract list of children within the given range ===
  # For example, age within 30 ~ 40 at the year of 2023
  list_child <- extract_child_list(data, child_min_age, child_max_age,
                                   child_base_year)
  
  # === Extract the mapping between parent and child within the sample ===
  # By this step, those who meet the following condition remains in the key
  # - Has parent in the survey
  # - Is between age of child_min_age and child_max_age at child_max_year
  #   - For example, age range 30 ~ 40 at year of 2023
  key_parent_child <- filter(relation_key, pid_child %in% list_child)
  
  # === Extract children and parent data ===
  # data_child extracts a list of children who meet the above criteria
  data_child <- extract_child_data(data, age_bandwidth,
                                   child_base_year,
                                   list_child, dir_path)
  
  # This only considers age of parents and list of child for now
  data_parent <- extract_parent_data(data,
                                     parent_min_age, parent_max_age,
                                     key_parent_child, dir_path)
  
  # Extract list of parents
  list_parent <- pull(data_parent, pid) %>% unique
  
  # Renews the key with parent data as well
  key_parent_child <- filter(key_parent_child, pid %in% list_parent)
  
  # === Filter children data to match parent for later use ===
  data_child <- filter(data_child, pid %in% key_parent_child$pid_child)
  
  # === Merge with base year data ===
  # Provides the basis merge set
  # - It provides year when the child was at child_base_age (e.g. 14)
  # - Also considers range of age with bandwidth (+- 2 years)
  child_base <- set_child_base(data_child,
                               child_base_age, age_bandwidth)
  child_base <- left_join(child_base, key_parent_child,
                          by = c('pid_child'),
                          relationship = 'many-to-many')
  
  data_parent <- inner_join(data_parent, child_base,
                            by = c('year' = 'base_year', 'pid'),
                            relationship = 'many-to-many')
  
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
  
  data_child_avg <- calc_house_avg(data_child)
  data_parent_avg <- calc_house_avg(data_parent, col = 'pid_child')
  
  result <- data_parent_avg %>%
    left_join(data_child_avg,
              by = c('pid_child' = 'pid'),
              suffix = c('_parent', '_child'),
              relationship = 'many-to-many') %>%
    unique()
  
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
