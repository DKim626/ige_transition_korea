source(here::here('R/KLIPS/module/utils/get_util.R'), local = environment())

#### ==== extract_hhid ====
#' Extract household ID from given data
#'
#' This function will extract household ID, or hhid for the relevant year
#'
#' @param data KLIPS data
#' @param target_year year to match hhid with
#' 
#' @return tibble with relevant hhid for the year
extract_hhid <- function(data, target_year) {
  data <- filter(data, year == !!target_year)
  wave <- get_wave(target_year)
  survey_wave <- wave$survey_wave
  
  sym_p <- if ('pid' %in% colnames(data)) sym('pid') else NULL
  
  # === Extract hhid for each year ===
  data[['hhid']] <- data[[paste0('hhid', survey_wave)]]
  
  data <- select(data, year, !!sym_p, hhid)
  
  return(data)
}

#### ==== extract_hhid_with_all ====
#' Extract household ID from given data
#'
#' This function will extract household ID, or hhid for the relevant year
#'
#' @param data KLIPS data
#' @param target_year year to match hhid with
#' 
#' @return tibble with relevant hhid for the year
extract_hhid_with_all <- function(data, target_year) {
  data <- filter(data, year == !!target_year)
  wave <- get_wave(target_year)
  survey_wave <- wave$survey_wave
  
  sym_p <- if ('pid' %in% colnames(data)) sym('pid') else NULL
  
  # === Extract hhid for each year ===
  data[['hhid']] <- data[[paste0('hhid', survey_wave)]]
  
  data <- select(data, year, !!sym_p, hhid, everything(),
                 -matches('hid(\\d){2}'))
  
  return(data)
}

#### ==== compare_hhid ====
#' Indicate whether children are living together or not
#'
#' This function will exclude the children living together with their parents
#'
#' @param data_child children's data using extract_child_data()
#' @param relation_key mapping between children and parent derived by track_parent_child()
#' @param child_min_year minimum year of children data
#' @param child_max_year maximum year of children data
#' 
#' @return tibble
compare_hhid <- function(data_child, data_parent,
                         child_base_year, age_bandwidth,
                         relation_key
) {
  
  child_years <- (child_base_year-age_bandwidth):(child_base_year+age_bandwidth)
  # === From the individual level data, extract parent data ===
  data_parent_cur <- readRDS(
    here::here('data/KLIPS/clean/clean_klips_individual.rds')) %>%
    filter(year %in% child_years &
             pid %in% data_parent$pid) %>%
    select(year, pid, contains('hhid'))
  
  # === Extract hhid information of parent ===
  parent_hhid <- map_dfr(child_years,
                         ~ extract_hhid(data_parent_cur, .x))
  
  # === Extract hhid information of child ===
  child_hhid <- map_dfr(child_years,
                        ~ extract_hhid(data_child, .x))
  
  compare_hhid <- parent_hhid %>%
    left_join(relation_key, by = c('pid'), relationship = 'many-to-many') %>%
    left_join(child_hhid,
              by = c('year', 'pid_child' = 'pid'),
              suffix = c('', '_child')
    ) %>%
    mutate(cohabit = ifelse(hhid == hhid_child, TRUE, FALSE))
  
  return(compare_hhid)
}

#### ==== extract_house ====
#' Extract house information of given year
#'
#' This function will extract relevant household information such as income and asset on the given year.
#' 
#' @param data household data
#' @param target_year year to extract household information from
#' 
#' @return tibble
extract_house <- function(data, target_year) {
  data <- data %>% 
    filter(year == target_year) %>%
    select(year, num_fam,
           income, asset,
           income_labor, income_fin, income_ra, income_t, income_o,
           asset_ra, asset_fin, eq_income, eq_income_labor,
           contains('hhid'), matches('[sn]?w\\d{2}'))
  wave <- get_wave(target_year)
  survey_wave <- wave$survey_wave
  
  # === Extract hhid for each year
  data[['hhid']] <- data[[paste0('hhid', survey_wave)]]
  
  # === Extract weight for each year, if possible ===
  data[['w']] <- data[[paste0('w', survey_wave, 'h')]]
  if (paste0('nw', survey_wave, 'h') %in% colnames(data)) {
    data[['nw']] <- data[[paste0('nw', survey_wave, 'h')]]
  } else {
    data[['nw']] <- NA
  }
  if (paste0('sw', survey_wave, 'h') %in% colnames(data)) {
    data[['sw']] <- data[[paste0('sw', survey_wave, 'h')]]
  } else {
    data[['sw']] <- NA
  }
  
  data <- select(data, year, hhid, num_fam,
                 income, asset,
                 income_labor, income_fin, income_ra, income_t, income_o,
                 asset_ra, asset_fin,
                 eq_income, eq_income_labor,
                 w, sw, nw) %>%
    filter(!is.na(hhid))
  
  return(data)
}

#### ==== match_house_info ====
#'
#'
#' This function will extract hhid of the given data and extract relevant household information.
#' 
#' @param data_house household level KLIPS data
#' @param data child or parent KLIPS data
#' 
#' @return tibble
match_house_info <- function(data_house, data) {
  
  data_year <- unique(data$year)
  
  hhid_key <- map_dfr(data_year, ~ extract_hhid(data, .x))
  
  if ('pid_child' %in% colnames(data)) {
    data <- select(data, year, pid, pid_child)
    hhid_key <- left_join(hhid_key, data,
                          by = c('year', 'pid'),
                          relationship = 'many-to-many') %>%
      unique()
  }
  
  house_info <- map_dfr(data_year, ~ extract_house(data_house, .x))
  
  result <- left_join(hhid_key, house_info, by = c('year', 'hhid'))
  
  
  
  return (result)
}
