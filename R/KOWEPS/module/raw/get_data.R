source(here::here('R/KOWEPS/module/raw/var_map.R'), local = environment())

read_house_data <- function (dir_path, year) {
  wave_num <- year - 2005
  edition <- fcase(
    wave_num %in% 1:2, 21,
    wave_num %in% 3, 19,
    wave_num %in% 4, 18,
    wave_num %in% 5, 17,
    wave_num %in% 6:7, 15,
    wave_num %in% 8, 13,
    wave_num >= 9, 20 - wave_num
  )
  wave_num <- ifelse(stri_length(as.character(wave_num)) == 1,
                     paste0(0, wave_num), as.character(wave_num))
  
  file_name <- paste0('koweps_h', wave_num, '_', year, '_beta', edition, '.dta')
  
  var_map <- var_map_house %>%
    mutate(code = paste0('h', wave_num, code))
  
  name_vec <- var_map$code
  names(name_vec) <- var_map$varname
  
  data <- read_stata(here::here(file.path(dir_path, file_name))) %>%
    select(any_of(var_map$code)) %>%
    rename(any_of(name_vec))
  
  col_weight <- c('weight_sample_hh', 'weight_general_hh',
                  'weight_sample_hh_1', 'weight_general_hh_1',
                  'weight_sample_hh_2', 'weight_general_hh_2')
  
  data_base <- data %>%
    select(hhid, hh_key, hmem_num, income_disposable, income_current,
           area_5, area_7, any_of(col_weight))
  
  pivot_data <- function (data, varname) {
    data %>%
      select(hhid, hh_key, contains(varname)) %>%
      group_by(hhid, hh_key) %>%
      pivot_longer(all_of(contains(varname)),
                   values_to = varname) %>%
      mutate(name = stri_extract_last_regex(name, '\\d$'))
  }
  
  var_list <- c('pid', 'h_num', 'rel_head', 'sex', 'year_birth', 'educ',
                'econ_act', 'ind', 'occ')
  
  data_list <- map(var_list, \(varname) {
    pivot_data(data, varname)
  })
  
  data_info <- reduce(data_list, \(x, y) {
    left_join(x, y, by = c('hhid', 'hh_key', 'name'))
  }) %>%
    select(-name) %>%
    filter(!is.na(pid)) %>%
    mutate(
      rel_head = ifelse(rel_head == 999, NA_real_, rel_head),
      sex = ifelse(sex == 9, NA_real_, sex),
      year_birth = ifelse(year_birth == 9999, NA_real_, year_birth),
      educ = ifelse(educ == 99, NA_real_, educ),
      econ_act = ifelse(econ_act %in% c(0, 99), NA_real_, econ_act),
      ind = ifelse(ind == 999, NA_real_, ind),
      occ = ifelse(occ == 9999, NA_real_, occ),
    )
  
  final_data <- left_join(data_base, data_info,
                          by = c('hhid', 'hh_key'),
                          relationship = 'one-to-many') %>%
    mutate(year = !!year) %>%
    select(year, everything())
  
  return (final_data)
}

read_ind_data <- function (dir_path, year) {
  wave_num <- year - 2005
  edition <- fcase(
    wave_num %in% 1:2, 21,
    wave_num %in% 3, 19,
    wave_num %in% 4, 18,
    wave_num %in% 5, 17,
    wave_num %in% 6:7, 15,
    wave_num %in% 8, 13,
    wave_num >= 9, 20 - wave_num
  )
  wave_num <- ifelse(stri_length(as.character(wave_num)) == 1,
                     paste0(0, wave_num), as.character(wave_num))
  
  file_name <- paste0('koweps_p', wave_num, '_', year, '_beta', edition, '.dta')
  
  var_map <- var_map_ind %>%
    mutate(code = fcase(
      code == '_pid', paste0('h', wave_num, code),
      default = paste0('p', wave_num, code)))
  
  name_vec <- var_map$code
  names(name_vec) <- var_map$varname
  
  data <- read_stata(here::here(file.path(dir_path, file_name))) %>%
    select(any_of(var_map$code)) %>%
    rename(any_of(name_vec))
  
  return (data)
}

get_data <- function (dir_path, year) {
  message('[INFO] Reading raw data of year: ', year)
  data_house <- read_house_data(dir_path, year)
  data_ind <- read_ind_data(dir_path, year)
  
  data_year <- left_join(data_house, data_ind, by = 'pid')
  
  if (year == 2006) {
    data_year <- data_year %>%
      mutate(
        weight_sample_ind_c = weight_sample_ind,
        weight_sample_ind_l = weight_sample_ind,
        weight_general_ind_c = weight_general_ind,
        weight_general_ind_l = weight_general_ind
      ) %>%
      select(-weight_sample_ind, -weight_general_ind)
  }
  
  return (data_year)
}
