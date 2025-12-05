source(here::here('R/KLIPS/module/utils/get_util.R'), local = environment())
source(here::here('R/KLIPS/module/utils/utility.R'), local = environment())
source(here::here('R/KLIPS/module/merge/extract_hhid.R'), local = environment())
source(here::here('R/KLIPS/module/merge/get_price.R'), local = environment())


#### ==== extract_major ====
#'
#'
#'
extract_major <- function (data_ind, data_supp) {
  base <- data_ind %>%
    select(year, pid, age, educ_self, educ_degree_curr, educ_major_curr) %>%
    left_join(data_supp, by = 'pid')
  
  data_list <- list()
  
  for (y in 1998:2022) {
    if (y == 1998) {
      data_1 <- base %>%
        filter(year == y) %>%
        mutate(
          educ_major = fcase(
            is.na(educ_major_curr) &
              year >= educ_grad_year_past_4 &
              !is.na(educ_major_past_4_b3), educ_major_past_4_b3,
            is.na(educ_major_curr) &
              year >= educ_grad_year_past_4 &
              !is.na(educ_major_past_4_a4), educ_major_past_4_a4,
            
            is.na(educ_major_curr) &
              year >= educ_grad_year_past_3 &
              !is.na(educ_major_past_3_b3), educ_major_past_3_b3,
            is.na(educ_major_curr) &
              year >= educ_grad_year_past_3 &
              !is.na(educ_major_past_3_a4), educ_major_past_3_a4,
            
            is.na(educ_major_curr) &
              year >= educ_grad_year_past_2 &
              !is.na(educ_major_past_2_b3), educ_major_past_2_b3,
            is.na(educ_major_curr) &
              year >= educ_grad_year_past_2 &
              !is.na(educ_major_past_2_a4), educ_major_past_2_a4,
            
            is.na(educ_major_curr) &
              year >= educ_grad_year_past_1 &
              !is.na(educ_major_past_1_b3), educ_major_past_1_b3,
            is.na(educ_major_curr) &
              year >= educ_grad_year_past_1 &
              !is.na(educ_major_past_1_a4), educ_major_past_1_a4,
            
            !is.na(educ_major_curr), educ_major_curr
          )
        ) %>%
        select(year, pid, educ_major) %>%
        filter(!is.na(educ_major))
      
      data_list[[as.character(y)]] <- data_1
    } else {
      data_1 <- data_list[[as.character(y)]]
    }
    
    data_2 <- base %>%
      filter(year == y + 1) %>%
      select(-year) %>%
      full_join(data_1, by = 'pid') %>%
      mutate(
        year = y + 1,
        educ_major = fcase(
          is.na(educ_major_curr) &
            year >= educ_grad_year_past_4 &
            !is.na(educ_major_past_4_b3), educ_major_past_4_b3,
          is.na(educ_major_curr) &
            year >= educ_grad_year_past_4 &
            !is.na(educ_major_past_4_a4), educ_major_past_4_a4,
          
          is.na(educ_major_curr) &
            year >= educ_grad_year_past_3 &
            !is.na(educ_major_past_3_b3), educ_major_past_3_b3,
          is.na(educ_major_curr) &
            year >= educ_grad_year_past_3 &
            !is.na(educ_major_past_3_a4), educ_major_past_3_a4,
          
          is.na(educ_major_curr) &
            year >= educ_grad_year_past_2 &
            !is.na(educ_major_past_2_b3), educ_major_past_2_b3,
          is.na(educ_major_curr) &
            year >= educ_grad_year_past_2 &
            !is.na(educ_major_past_2_a4), educ_major_past_2_a4,
          
          is.na(educ_major_curr) &
            year >= educ_grad_year_past_1 &
            !is.na(educ_major_past_1_b3), educ_major_past_1_b3,
          is.na(educ_major_curr) &
            year >= educ_grad_year_past_1 &
            !is.na(educ_major_past_1_a4), educ_major_past_1_a4,
          
          is.na(educ_major_curr) & !is.na(educ_major), educ_major,
          !is.na(educ_major_curr), educ_major_curr
        )
      ) %>%
      select(year, pid, educ_major) %>%
      filter(!is.na(educ_major))
    
    data_list[[as.character(y+1)]] <- data_2
    
  }
  
  data_new <- bind_rows(data_list)
  
  return (data_new)
}

#### ==== extract_parent ====
#'
#'
#'
#'
extract_parent <- function (data) {
  
  relation_key <- readRDS('data/KLIPS/merge/relation_key.rds')
  
  data <- data %>%
    select(pid, sex) %>%
    distinct()
  
  key <- left_join(relation_key, data, by = 'pid') %>%
    mutate(parent = ifelse(sex == 'Male', 'f', 'm')) %>%
    select(pid_child, pid, parent) %>%
    arrange(pid_child) %>%
    group_by(pid_child, parent) %>%
    mutate(n = row_number()) %>%
    ungroup() %>%
    group_by(pid_child) %>%
    pivot_wider(values_from = pid,
                names_from = c(parent, n),
                names_glue = 'pid_{.name}') %>%
    ungroup() %>%
    select(pid_child, contains('pid_f'), contains('pid_m'))
}

#### ==== merge_all ====
#'
#'
#'
#'
merge_all <- function (start_wave, end_wave,
                       cols_household, cols_individual,
                       cols_income,
                       dir_path) {
  on.exit(gc())
  
  #### === 1. Load data set ===
  message('[INFO] Loading individual and household KLIPS data')
  data_house <- readRDS('data/KLIPS/clean/clean_klips_house.rds')
  data_ind <- readRDS('data/KLIPS/clean/clean_klips_individual.rds')
  data_supp <- readRDS('data/KLIPS/clean/clean_klips_supp.rds')
  price <- get_price('data/KLIPS/external')
  
  
  refine_data <- function (data, start_wave, end_wave, cols, obs, type = NULL) {
    on.exit(gc())

    sym_cols <- syms(cols)
    
    suffix <- if (obs == 'household') 'h' else 'p'
    join_key <- c('year', 'hhid')
    if (obs == 'individual') join_key[3] <- 'pid'
    drop_weight <- paste0('[ns]?w(\\d){2}', suffix)
    
    #### === 2. Create years with wave ===
    years <- 1997 + start_wave:end_wave
    
    #### === 3. Extract household information along with hhid ===
    data_map <- map(years, \(.x) {
      result <- extract_hhid_with_all(data, .x)
      return(result)
    })
    data_map <- bind_rows(data_map) %>%
      select(-matches(drop_weight))
    
    #### === 4. Extract sample weights ====
    weight <- gather_weight(data, obs,
                            start_wave, end_wave, type)
    
    #### === 5. Merge the separate data set ===
    result <- left_join(data_map, weight, by = c(join_key)) %>%
      select(!!!sym_cols)
    
    #### === 6. Return the results ===
    return(result)
  }
  
  message('[INFO] Refining the data sets')
  household <- refine_data(data_house,
                           start_wave, end_wave,
                           cols = cols_household,
                           obs = 'household')
  individual <- refine_data(data_ind,
                            start_wave, end_wave,
                            cols = cols_individual,
                            obs = 'individual', type = 'c')
  
  message('[INFO] Calculating real income')
  merged_all <- left_join(individual, household,
                          by = c('year', 'hhid'))
  
  major_data <- extract_major(data_ind, data_supp)
  
  merged_all <- left_join(merged_all, major_data,
                          by = c('year', 'pid'))
  
  data_supp <- data_supp %>%
    select(pid:educ_mother)
  
  merged_all <- left_join(merged_all, data_supp, by = c('pid'))
  
  merged_all <- left_join(merged_all, price, by = 'year')
  
  for (inc in cols_income) {
    inc_price <- paste(inc, c('CPI', 'GDP'), sep = '_')
    merged_all <- merged_all %>%
      mutate(
        !!inc_price[1] := .data[[inc]] / CPI * 100,
        !!inc_price[2] := .data[[inc]] / GDP * 100,
      )
  }

  message('[INFO] Merge complete')
  
  save_overwrite_rds(merged_all, dir_path, 'merged_all.rds')
  
  return(merged_all)
}



