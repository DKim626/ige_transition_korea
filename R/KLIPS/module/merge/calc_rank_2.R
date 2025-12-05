source(here::here('R/KLIPS/module/utils/get_util.R'), local = environment())
source(here::here('R/KLIPS/module/utils/utility.R'), local = environment())

#### ==== calc_rank_years ====
#'
#'
#'
#'
calc_rank_years <- function (data, base_year,
                             obs, cols_income,
                             weight_sum_avg = 'sum',
                             exclude_zero = FALSE) {
  ### === Sanity check ===
  assert_that(weight_sum_avg %in% c('sum', 'avg'),
              msg = "[ERROR] value of weight_sum_avg must be either 'sum' or 'avg'")
  
  weight <- c('w', 'sw', 'nw')
  
  ### === Initial set-up
  if (obs == 'household') {
    w_name <- paste0(weight, '_h')
  } else {
    w_name <- paste0(weight, '_p_c')
  }
  id <- if (obs == 'household') 'hhid' else 'pid'
  
  sym_select <- syms(c(id, cols_income, w_name))
  
  years <- seq(base_year-1, base_year+2, by = 1)
  
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
    avg_name <- paste('avg', cols_income, sep = '_')
    rank_name <- paste('rank_years', avg_name, weights, sep = '_')
    
    data <- data %>%
      mutate(
        w_value = if (weights %in% colnames(.)) .data[[weights]] else NA
      ) %>%
      filter(!is.na(w_value))
    if (exclude_zero == TRUE) data <- filter(data, !!sym(cols_income) > 0)
    data <- data %>%
      group_by(!!sym(id)) %>%
      summarize(
        !!avg_name := mean(.data[[cols_income]], na.rm = TRUE),
        w_sum_avg = if (weight_sum_avg == 'sum') sum(w_value, na.rm = TRUE)
        else mean(w_value, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      arrange(!!sym(avg_name)) %>%
      mutate(
        cumul_w = cumsum(w_sum_avg)
      ) %>%
      group_by(!!sym(avg_name)) %>%
      mutate(w_final = mean(cumul_w)) %>%
      ungroup() %>%
      mutate(
        !!rank_name := w_final / sum(w_sum_avg) * 100
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
  })
  
  rank_no_w <- reduce(rank_no_w,
                      .f = \(x, y) full_join(x, y, by = id)) %>%
    mutate(year = base_year) %>%
    select(year, !!sym(id), contains('rank'))
  
  ### === Merge and return results ===
  rank_result <- reduce(rank_results,
                        .f = \(x, y) full_join(x, y, by = id)) %>%
    mutate(year = base_year) %>%
    select(year, !!sym(id), contains('rank'))
  
  rank_result <- full_join(rank_no_w, rank_result,
                           by = c('year', id))
  
  return (rank_result)
  
}

#### ==== get_approx_rank ====
#'
#'
#'
#'
#'
get_approx_rank <- function (rank_table, value_col, rank_col, data, values) {
  on.exit(gc())
  ### === Select variables ===
  table_sorted <- rank_table %>%
    arrange(.data[[value_col]]) %>%
    select(all_of(value_col), all_of(rank_col)) %>%
    distinct()
  
  ### === Define linear interpolation of ranks ===
  interp_fun <- approxfun(
    x = table_sorted[[value_col]],
    y = table_sorted[[rank_col]],
    rule = 2,
    ties = 'mean',
    na.rm = TRUE
  )
  
  rank_name <- paste('rank', values, sep = '_')
  
  ### === Interpolate and return output ===
  data <- data %>%
    filter(!is.na(.data[[values]])) %>%
    mutate(!!rank_name := interp_fun(.data[[values]]))  %>%
    select(-all_of(value_col))
  
  return (data)
}

#### ==== calc_rank ====
#'
#'
#'
#'
#'
# 자녀의 과거 데이터를 기반으로 5년 평균한 소득의 순위를 산출할 것.
# 즉, child age가 target age와 동일한 연도를 기준으로, +- 2년 데이터를 가져올 것.
# 이는 target age에 해당하는 연도의 +-2 년을 start_year와 end_year로 넣고,
# - 확인 결과, 15살 이후 데이터만 존재하므로 이를 고려해서 적절히 조절할 필요 있음.
# 각 순위표를 row-wise merge 한 다음에,
# 이를 바로 left_join(by = id, base_year)으로 묶자
calc_rank <- function (age_bandwidth,
                       cols_income_h, cols_income_p,
                       dir_path) {
  message("[INFO] Initiated rank calculation")
  ### === Initial setup ===
  load_or_use(merged_all, dir_path)
  
  child_start <- 2021 - age_bandwidth
  child_end <- 2021 + age_bandwidth
  
  cols_income_h <- paste(cols_income_h, 'CPI', sep = '_')
  cols_income_p <- paste(cols_income_p, 'CPI', sep = '_')
  
  message("[INFO] Deriving distributions and ranks")
  ### === Deriving rank distibution ===

  years_rank_list_h <- map(c(1997:2012, 2021), \(year) {
    message("[INFO] Deriving household level distribution for ", year)
    calc_rank_years(data = merged_all, base_year = year,
                    obs = 'household', cols_income = cols_income_h)
  }) %>%
    list_rbind()
  
  years_rank_list_p <- map(c(1997:2012, 2021), \(year) {
    message("[INFO] Deriving individual level distribution for ", year)
    calc_rank_years(data = merged_all, base_year = year,
                    obs = 'individual', cols_income = cols_income_p)
  }) %>%
    list_rbind()
  
  result <- list(
    'household' = years_rank_list_h,
    'individual' = years_rank_list_p
  )

  save_overwrite_rds(result, dir_path, 'rank_2.rds')
  
  return(result)
}

calc_rank_cohort <- function (age_bandwidth,
                              cols_income_h, cols_income_p,
                              dir_path) {
  
  load_or_use(merged_child_parent_2021, dir_path)
  
  income <- c('income_2_CPI_past', 'eq_income_2_CPI_past',
              'income_2_CPI_curr', 'eq_income_2_CPI_curr')
  
  child_rank <- map(income, \(inc){
    s_inc <- sym(inc)
    rank_name <- paste0('rank_cohort_', inc)
    
    merged_child_parent_2021 %>%
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
    
    merged_child_parent_2021 %>%
      filter(cohort %in% c(1, 2)) %>%
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
  
  result <- left_join(merged_child_parent_2021, child_rank, by = 'pid') %>%
    left_join(child_rank_3040, by = 'pid')
  
  return (result)
}
