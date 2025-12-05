source(here::here('R/module/utils/get_util.R'), local = environment())
source(here::here('R/module/merge/extract_hhid.R'), local = environment())
source(here::here('R/module/merge/extract_data.R'), local = environment())


#### ==== merge_house ====
#' Merges household data
#'
#' This function will merge the household level data. It integrates the household information such as income and amount of asset of children and parent.
#'
#' @param child_min_year minimum year of children data
#' @param child_max_year maximum year of children data
#' @param parent_min_year minimum year of parent data
#' @param parent_max_year maximum year of parent data
#' @param relation_key mapping between children and parent derived by track_parent_child()
#' @param dir_path directory of merged data (data/merge)
#' 
#' @return tibble of merged household data with information on income and asset
merge_house <- function (child_base_age, age_bandwidth,
                         parent_min_age, parent_max_age,
                         child_base_year,
                         dir_path) {
  on.exit(gc())
  
  #### === Load data necessary for extraction ===
  data_child <- readRDS(here::here('data/merge/data_child.rds'))
  data_parent <- readRDS(here::here('data/merge/data_parent.rds'))
  data_house <- readRDS(here::here('data/clean/clean_klips_house.rds'))
  relation_key <- readRDS(here::here('data/merge/map_parent_child.rds'))
  
  #### === Derive moving-out basis ===
  basis <- compare_hhid(data_child, data_parent,
                        child_base_year, age_bandwidth, relation_key)
  
  #### === Extract household info by time ===
  house_info_child <- match_house_info(data_house = data_house,
                                       data = data_child)
  
  house_info_parent <- match_house_info(data_house = data_house,
                                        data = data_parent)
  
  #### === Merge child household info ===
  basis_child <- basis %>%
    select(year, pid_child, hhid_child, cohabit) %>%
    unique() %>%
    left_join(house_info_child, by = c('year',
                                       'hhid_child' = 'hhid',
                                       'pid_child' = 'pid'))
  
  #### === Average the household info === 
  avg_house_info <- function (data, cols) {
    
    g_cols <- syms(cols)
    
    data <- data %>%
      mutate(across(income:asset_fin, ~ ifelse(is.na(.x), 0, .x)))
    
    result <- data %>%
      group_by(!!!g_cols) %>%
      summarize(income_w = weighted.mean(income, w, na.rm = TRUE),
                income_sw = weighted.mean(income, sw, na.rm = TRUE),
                income_nw = weighted.mean(income, nw, na.rm = TRUE),
                income_labor_w = weighted.mean(income_labor, w, na.rm = TRUE),
                income_labor_sw = weighted.mean(income_labor, sw, na.rm = TRUE),
                income_labor_nw = weighted.mean(income_labor, nw, na.rm = TRUE),
                eq_income_w = weighted.mean(eq_income, w, na.rm = TRUE),
                eq_income_sw = weighted.mean(eq_income, sw, na.rm = TRUE),
                eq_income_nw = weighted.mean(eq_income, nw, na.rm = TRUE),
                eq_income_labor_w = weighted.mean(eq_income_labor, 
                                                  w, na.rm = TRUE),
                eq_income_labor_sw = weighted.mean(eq_income_labor, 
                                                   sw, na.rm = TRUE),
                eq_income_labor_nw = weighted.mean(eq_income_labor, 
                                                   nw, na.rm = TRUE),
                .groups = 'drop')
    
    if ('cohabit' %in% colnames(data)) {
      cohabit <- data %>%
        group_by(!!!g_cols) %>%
        slice_max(order_by = year, n = 1)%>%
        ungroup()
      result <- left_join(result, cohabit, by = c(cols)) %>%
        unique()
    }
    
    return(result)
  }
  
  house_child_avg <- avg_house_info(basis_child,
                                    cols = c('pid_child', 'hhid_child'))
  house_parent_avg <- avg_house_info(house_info_parent,
                                     cols = c('pid', 'hhid', 'pid_child'))
  
  
  #### === Merge the final results ===
  # result <- basis %>%
  #   select(-year) %>%
  #   unique() %>%
  #   left_join(house_parent_avg, by = c('pid', 'hhid')) %>%
  #   left_join(house_child_avg,
  #             by = c('pid_child', 'hhid_child'),
  #             suffix = c('_parent', '_child'),
  #             relationship = 'many-to-many'
  #   )
  result <- house_parent_avg %>%
    left_join(house_child_avg, by = 'pid_child',
              suffix = c('_parent', '_child'),
              relationship = 'many-to-many') %>%
    unique()
  
  #### === Save and return the results ===
  if (!dir.exists(here::here(dir_path))) {
    message('[INFO] Directory created: ', dir_path)
    dir.create(here::here(dir_path), recursive = TRUE)
  }
  
  file_path <- file.path(dir_path, 'data_merge_house.rds')
  
  msg <- if (file.exists(here::here(file_path))) "Overwriting" else "Saved"
  message('[SAVE] ', msg, ' household level merged data at ', file_path)
  saveRDS(result, file = here::here(file_path))
  
  return (result)
}
