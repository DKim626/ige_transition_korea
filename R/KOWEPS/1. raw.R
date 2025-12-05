source(here::here('R/KOWEPS/module/raw/get_data.R'), local = environment())

extract_raw_data <- function (dir_path) {
  years <- 2006:2024

  data <- map(years, \(year) {
    get_data(dir_path, year)
  }) %>%
    list_rbind()
  
  saveRDS(data, file = here::here('data/KOWEPS/raw/data_all.rds'))
}

