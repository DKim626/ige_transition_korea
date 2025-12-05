source(here::here('R/module/merge/extract_hhid.R'))


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
merge_house <- function (child_min_year, child_max_year,
                         parent_min_age, parent_max_age,
                         parent_min_year, parent_max_year,
                         dir_path) {
  on.exit(gc())
  
  #### === Load data necessary for extraction ===
  data_child <- readRDS(here::here('data/merge/data_child.rds'))
  data_parent <- readRDS(here::here('data/merge/data_parent.rds'))
  data_house <- readRDS(here::here('data/clean/clean_klips_house.rds'))
  relation_key <- readRDS(here::here('data/merge/map_parent_child.rds'))
  
  #### === Derive moving-out basis ===
  basis <- compare_move_out(data_child, relation_key,
                            child_min_year, child_max_year)
  
  #### === Extract household info by time ===
  extract_house_by_time <- function(years) {
    map_dfr(years, ~ extract_house(data_house, .x))
  }
  
  house_info_cur <- extract_house_by_time(child_min_year:child_max_year)
  house_info_past <- extract_house_by_time(parent_min_year:parent_max_year)
  
  #### === Merge child household info ===
  basis_child <- basis %>%
    select(year, pid_child, hhid_child) %>%
    left_join(house_info_cur, by = c('year', 'hhid_child' = 'hhid'))
  
  #### === Merge parent household info ===
  parent_hhid <- map_dfr(parent_min_year:parent_max_year,
                         ~ extract_hhid(data_parent, .x)) %>%
    filter(pid %in% unique(basis$pid))
  
  basis_parent <- parent_hhid %>%
    left_join(house_info_past, by = c('year', 'hhid'))
  
  #### === Average the household info === 
  avg_house_info <- function (data, pid_col, hhid_col) {
    data %>%
      select(-year) %>%
      group_by(across(all_of(c(pid_col, hhid_col)))) %>%
      summarize(across(everything(), ~ mean(.x, na.rm = TRUE)),
                .groups = 'drop')
  }
  house_child_avg <- avg_house_info(basis_child, 'pid_child', 'hhid_child')
  house_parent_avg <- avg_house_info(basis_parent, 'pid', 'hhid')
  
  #### === Merge the final results ===
  result <- basis %>%
    select(-year) %>%
    unique() %>%
    left_join(house_parent_avg, by = c('pid', 'hhid')) %>%
    left_join(house_child_avg,
              by = c('pid_child', 'hhid_child'),
              suffix = c('_parent', '_child'),
              relationship = 'many-to-many'
    )
  
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


