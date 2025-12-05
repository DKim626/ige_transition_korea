source(here::here('R/module/utils/get_util.R'))

calc_rank_one <- function(data_house, year, income_var, weight) {
  sym_i <- sym(income_var)
  
  wave <- get_wave(year)
  survey_wave <- wave$survey_wave
  
  target_weight <- paste0(weight, survey_wave, 'h')
  target_hhid <- paste0('hhid', survey_wave)
  
  data <- data_house %>%
    filter(year == !!year) %>%
    mutate(
      !!income_var := .data[[income_var]],
      weight = if (target_weight %in% colnames(.))
        .data[[target_weight]] else NA,
      hhid = .data[[target_hhid]]
    ) %>%
    filter(!is.na(hhid) & !is.na(!!income_var) & !is.na(weight)) %>%
    arrange(!!income_var) %>%
    mutate(
      !!paste0('rank_', weight) := cumsum(weight) / sum(weight) * 100
    ) %>%
    select(year, hhid, !!income_var, !!paste0('rank_', weight))
  
  return(data)
}

calc_rank_all <- function(start_wave, end_wave, income_var, dir_path) {
  on.exit(gc())
  message('[INFO] Calculating ranks for ', income_var)
  data_house <- readRDS('data/clean/clean_klips_house.rds')
  
  weights <- c('w', 'sw', 'nw')
  years <- 1997 + start_wave:end_wave
  
  combination <- expand.grid(year = years, weight = weights,
                             stringsAsFactors = FALSE)
  
  rank_list <- pmap(combination, \(year, weight) {
      calc_rank_one(data_house, year, income_var, weight)
  })
  
  rank_merged <- bind_rows(rank_list)
  
  rank_w <- rank_merged %>%
    select(year, hhid, !!income_var, rank_w) %>%
    filter(!is.na(rank_w))
  
  rank_sw <- rank_merged %>%
    select(year, hhid, rank_sw) %>%
    filter(!is.na(rank_sw))
  
  rank_nw <- rank_merged %>%
    select(year, hhid, rank_nw) %>%
    filter(!is.na(rank_nw))
  
  result <- left_join(rank_w, rank_sw, by = c('year', 'hhid')) %>%
    left_join(rank_nw, by = c('year', 'hhid'))
  
  file_path <- file.path(dir_path, paste0('rank_', income_var, '.rds'))
  message('[SAVE] Saving rank data for ', income_var, ' at ', dir_path)
  
  save(result, file = file_path)
  
}
