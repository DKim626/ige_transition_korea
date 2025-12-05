#### ==== merge_two ====
#' Merge individual and household level data
#' 
#' This function will merge household and individual data.
#' 
#' @param data_ind individual-level merged data
#' @param data_house household-level merged data
#'
#' @return tibble with household and individual level information of both child and parent
merge_two <- function (data_ind, data_house, dir_path) {
  merged_data <- left_join(data_house, data_ind,
                           by = c('pid', 'pid_child',
                                  'hhid' = 'hhid_parent', 'hhid_child'),
                           suffix = c('_house', '_ind')) %>%
    select(pid, hhid, contains('parent'), contains('child'), cohabit) %>%
    mutate(cohabit = ifelse(is.na(cohabit), 0, cohabit))
  
  file_path <- file.path(dir_path, 'data_merge.rds')
  msg <- if (!file.exists(file_path)) "Saved" else "Overwritting"
  message("[SAVE] ", msg, " final merged data at ", file_path)
  
  saveRDS(merged_data, file = file_path)
  
  return(merged_data)
}

#### ==== merge_rank ====
merge_rank <- function (merged_data, rank_data, dir_path) {
  left_join(merged_data, rank_data, by = c('hhid', 'year'))
}