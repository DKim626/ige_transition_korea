#### ==== get_wave ====
#' Get wave from survey year
#'
#' This function yields survey wave number relevant to the given year
#'
#' @param year survey year to retrieve wave number
#' 
#' @return list of numeric and character type of wave number
get_wave <- function (year) {
  
  result <- list()
  
  wave_num <- year - 1997
  result[['wave_num']] <- wave_num
  result[['survey_wave']] <- ifelse(stri_length(wave_num) == 2,
                                    as.character(wave_num), paste0(0, wave_num))
  
  return(result)
}

#### ==== get_weight ====
#' Get sample weights from given year and form of data
#' 
#' This function derives weight from KLIPS survey data.
#' 
#' @param data KLIPS data
#' @param obs unit of observation of KLIPS data (e.g. 'individual')
#' @param year year of survey
#' @param weight type of weight (e.g. w, sw, nw)
#' @param type longitudinal or cross_sectional (individual level data only)
#' 
#' @return tibble of weight

get_weight <- function(data, obs, year, weight, type = NULL) {
  
  suffix <- if (obs == 'household') 'h' else if (obs == 'individual') 'p'
  
  if (is.null(type)) { 
  } else if (obs == 'individual' & type == 'c') {
    suffix <- paste0(suffix, '_c')
  } else if (obs == 'individual' & type == 'l') {
    suffix <- paste0(suffix, '_l')
  }
  sym_p <- if (obs == 'individual') sym('pid') else NULL
  
  wave <- get_wave(year)
  survey_wave <- wave$survey_wave
  
  target_weight <- paste0(weight, survey_wave, suffix)
  target_hhid <- paste0('hhid', survey_wave)
  
  weight_name <- paste0(weight, '_', suffix)
  
  if (target_weight %in% colnames(data)) {
    data <- data %>%
      filter(year == !!year) %>%
      mutate(
        hhid = .data[[target_hhid]],
        !!weight_name := .data[[target_weight]]
      ) %>%
      select(year, hhid, !!sym_p, !!weight_name) %>%
      filter(!is.na(!!weight_name))
  } else if (obs == 'individual') {
    return(tibble(year = year, hhid = NA,
                  !!sym_p := NA, !!weight_name := NA) %>% filter(FALSE))
  } else {
    return(tibble(year = year, hhid = NA,
                  !!weight_name := NA) %>% filter(FALSE))
  }
  
  return (data)
}


#### ==== gather_weight ====
#' Gathers weight from given data
#'
#' This function will gather all sampling weight information
#'
#' @param 
gather_weight <- function (data, obs, start_wave, end_wave, type = NULL) {
  on.exit(gc())
  years <- start_wave:end_wave + 1997
  weights <- c('w', 'sw', 'nw')
  
  suffix <- if (obs == 'household') 'h' else if (obs == 'individual') 'p'
  
  if (is.null(type)) { 
    } else if (obs == 'individual' & type == 'c') {
    suffix <- paste0(suffix, '_c')
  } else if (obs == 'individual' & type == 'l') {
    suffix <- paste0(suffix, '_l')
  }
  p <- if (obs == 'individual') 'pid' else NULL
  sym_p <- if (obs == 'individual') sym('pid') else NULL
  
  weight_name <- paste0(weights, '_', suffix)
  
  combination <- expand.grid(year = years, weight = weights,
                             stringsAsFactors = FALSE)
  
  weight_data <- pmap(combination, \(year, weight) {
    get_weight(data, obs, year, weight, type)
  })
  
  results <- bind_rows(weight_data)
  
  sym_w <- sym(weight_name[1])
  sym_sw <- sym(weight_name[2])
  sym_nw <- sym(weight_name[3])
  
  result_w <- results %>%
    select(year, hhid, !!sym_p, !!sym_w) %>%
    filter(!is.na(!!sym_w)) %>%
    unique()
  
  result_sw <- results %>%
    select(year, hhid, !!sym_p, !!sym_sw) %>%
    filter(!is.na(!!sym_sw)) %>%
    unique()
  
  result_nw <- results %>%
    select(year, hhid, !!sym_p, !!sym_nw) %>%
    filter(!is.na(!!sym_nw)) %>%
    unique()
  
  result <- full_join(result_w, result_sw, by = c('year', 'hhid', p)) %>%
    full_join(result_nw, by = c('year', 'hhid', p))
  
  return(result)
}

