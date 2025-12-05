#### ==== calc_rank_years ====
#'
#'
#'
#'
calc_rank_years <- function (data, base_year,
                             cols_income,
                             weight_sum_avg = 'sum',
                             exclude_zero = FALSE) {
  ### === Sanity check ===
  assert_that(weight_sum_avg %in% c('sum', 'avg'),
              msg = "[ERROR] value of weight_sum_avg must be either 'sum' or 'avg'")
  
  ### === Initial set-up
  w_name <- 'wt_ind_l'
  id <- 'pid'
  sym_select <- syms(c(id, cols_income, w_name))
  years <- seq(base_year - 2, base_year + 2, by = 1)
  
  ### === Filtering the target years ===
  data <- data %>%
    filter(year %in% years) %>%
    select(year, !!!sym_select) %>%
    distinct()
  
  ### === Computation of ranks ===
  comb_inc_w <- expand.grid(cols_income = cols_income,
                            weights = w_name,
                            stringsAsFactors = FALSE)
  
  rank_results <- pmap(comb_inc_w, \(cols_income, weights) {
    avg_name <- paste('avg', cols_income, weights, sep = '_')
    rank_name <- paste('rank_years', avg_name, sep = '_')
    
    data <- data %>%
      mutate(
        w_value = if (weights %in% colnames(.)) .data[[weights]] else NA
      ) %>%
      filter(!is.na(w_value))
    if (exclude_zero == TRUE) data <- filter(data, !!sym(cols_income) > 0)
    
    data <- data %>%
      group_by(!!sym(id)) %>%
      summarize(
        !!avg_name := mean(!!sym(cols_income), na.rm = TRUE),
        w_sum_avg = if (weight_sum_avg == 'sum') sum(w_value, na.rm = TRUE)
        else mean(w_value, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      arrange(!!sym(avg_name)) %>%
      mutate(w_cumsum = cumsum(w_sum_avg)) %>%
      group_by(!!sym(avg_name)) %>%
      mutate(w_cumsum = mean(w_cumsum)) %>%
      ungroup() %>%
      mutate(
        !!rank_name := w_cumsum / sum(w_sum_avg) * 100
      ) %>%
      select(!!sym(id), !!sym(avg_name), !!sym(rank_name))
  })
  
  rank_no_w <- map(cols_income, \(col_inc) {
    avg_name <- paste('avg', col_inc, sep = '_')
    rank_name <- paste('rank_years', avg_name, sep = '_')
    
    if (exclude_zero == TRUE) data <- filter(data, !!sym(col_inc) > 0)
    data <- data %>%
      group_by(!!sym(id)) %>%
      summarize(
        !!avg_name := mean(.data[[col_inc]], na.rm = TRUE),
        n = 1,
        .groups = 'drop'
      ) %>%
      arrange(!!sym(avg_name)) %>%
      mutate(
        cumul_w = cumsum(n)
      ) %>%
      group_by(!!sym(avg_name)) %>%
      mutate(w_final = mean(cumul_w)) %>%
      ungroup() %>%
      mutate(
        !!rank_name := w_final / sum(n) * 100
      ) %>%
      select(!!sym(id), !!sym(avg_name), !!sym(rank_name))
  }) %>%
    reduce(.f = \(x, y) full_join(x, y, by = id)) %>%
    mutate(year = base_year) %>%
    select(year, !!sym(id), contains('rank'))
  
  ### === Merge and return results ===
  rank_result <- reduce(rank_results,
                        .f = \(x, y) full_join(x, y, by = id)) %>%
    mutate(year = base_year) %>%
    select(year, everything()) %>%
    left_join(rank_no_w, by = c('year', id))
  
  return (rank_result)
  
}

#### ==== calc_rank ====
#'
#'
#'
#'
#'
calc_rank_eq <- function (data_avg,
                          child_base_year, age_bandwidth,
                          cols_income_p, exclude_zero = FALSE) {
  
  data <- readRDS(file = here::here('data/PSID/clean/data_all.rds'))
  
  cols_income_p <- c('eq_inc_total')
  
  cols_income_p <- paste(cols_income_p, 'cpi', sep = '_')
  
  message("[INFO] Deriving distributions and ranks")
  ### === Deriving rank distibution ===
  rank_dist_list <- map(c(1996:2012, 2021), \(year) {
    message('[INFO] Deriving the rank of year ', year)
    calc_rank_years(data, year, cols_income_p)
  })
  
  rank_dist <- list_rbind(rank_dist_list) %>%
    select(year, pid, contains('rank'))
  
  rank_data <- data_avg %>%
    left_join(
      rank_dist %>%
        filter(year != 2021) %>%
        rename_with(~paste0(.x, '_past')),
      by = c('pid' = 'pid_past', 'base_year' = 'year_past')
    ) %>%
    left_join(
      rank_dist %>%
        filter(year == 2021) %>%
        select(-year) %>%
        rename_with(~paste0(.x, '_curr')),
      by = c('pid' = 'pid_curr')
    )
  
  # save_overwrite_rds(rank_dist, 'R/PSID/merged/rank_dist.rds')
  
  return(rank_data)
}


calc_rank_cohort <- function (data, age_bandwidth) {
  
  income <- c('eq_inc_total_cpi_curr', 'eq_inc_total_cpi_past')
  
  child_rank <- map(income, \(inc){
    s_inc <- sym(inc)
    rank_name <- paste0('rank_cohort_', inc)
    
    data %>%
      select(pid, !!s_inc) %>%
      arrange(!!s_inc) %>%
      mutate(
        w = 1,
        cumul_w = cumsum(w)
      ) %>%
      group_by(!!s_inc) %>%
      mutate(cumul_w = mean(cumul_w)) %>%
      ungroup() %>%
      mutate(!!rank_name := cumul_w / sum(w) * 100) %>%
      select(pid, !!sym(rank_name))
  }) %>%
    reduce(\(x, y) left_join(x, y, by = 'pid'))
  
  child_rank_3040 <- map(income, \(inc){
    s_inc <- sym(inc)
    rank_name <- paste0('rank_cohort_3040_', inc)
    
    data %>%
      filter(cohort %in% c(1,2)) %>%
      select(pid, !!s_inc) %>%
      arrange(!!s_inc) %>%
      mutate(
        w = 1,
        cumul_w = cumsum(w)
      ) %>%
      group_by(!!s_inc) %>%
      mutate(cumul_w = mean(cumul_w)) %>%
      ungroup() %>%
      mutate(!!rank_name := cumul_w / sum(w) * 100) %>%
      select(pid, !!sym(rank_name))
  }) %>%
    reduce(\(x, y) left_join(x, y, by = 'pid'))
  
  result <- left_join(data, child_rank, by = 'pid') %>%
    left_join(child_rank_3040, by = 'pid')
  
  return (result)
}
