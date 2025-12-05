


get_price <- function (dir_path) {
  
  assert_that(dir.exists(here::here(dir_path)),
              msg = paste0("[ERROR] Directory ", dir_path, " does not exist."))
  
  file_CPI <- file.path(dir_path, 'CPI.txt')
  file_GDP <- file.path(dir_path, 'GDP Deflator.csv')
  
  assert_that(file.exists(here::here(file_CPI)),
              msg = paste0("[ERROR] Directory ", file_CPI, " does not exist."))
  assert_that(file.exists(here::here(file_GDP)),
              msg = paste0("[ERROR] Directory ", file_GDP, " does not exist."))
  
  
  CPI <- read.table(file = here::here(file_CPI),
                    skip = 1, col.names = c('year', 'CPI'))
  
  GDP <- read_csv(file = 'data/KLIPS/external/GDP Deflator.csv', skip = 4) %>%
    select(`Country Name`, `1997`:`2023`) %>%
    filter(`Country Name` == "Korea, Rep.") %>%
    pivot_longer(`1997`:`2023`, names_to = 'year', values_to = 'GDP') %>%
    select(year, GDP) %>%
    mutate(year = as.numeric(year))
  
  price <- left_join(CPI, GDP, by = 'year') %>%
    mutate(year = year + 1)
  
  return (price)
}
