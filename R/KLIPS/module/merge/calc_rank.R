source(here::here('R/KLIPS/module/utils/get_util.R'))
source(here::here('R/KLIPS/module/utils/utility.R'), local = environment())

#### ==== calc_rank_one ====
#'
#'
#'
#'
calc_rank_one <- function (data, target_year, obs, cols_income, weight,
                           exclude_zero = TRUE) {
  ### === Initial set-up ===
  if (obs == 'household') {
    w_name <- paste0(weight, '_h')
  } else {
    w_name <- paste0(weight, '_p_c')
  }
  id <- if (obs == 'household') 'hhid' else 'pid'
  
  sym_select <- syms(c(id, cols_income, w_name))
  
  ### === Filtering with target year only ===
  data <- data %>%
    filter(year == !!target_year) %>%
    select(year, !!!sym_select) %>%
    distinct()
  
  ### === Computation of ranks ===
  comb_inc_w <- expand.grid(cols_income = cols_income,
                            weights = w_name,
                            stringsAsFactors = FALSE)
  
  rank_results <- pmap(comb_inc_w, \(cols_income, weights) {
    
    rank_name <- paste('rank', cols_income, weight, sep = '_')
    data <- data %>%
      mutate(
        w_value = if (weights %in% colnames(.)) .data[[weights]] else NA
      ) %>%
      filter(!is.na(w_value))
    if (exclude_zero == TRUE) data <- filter(data, !!sym(cols_income) > 0)
    data <- data %>%
      arrange(.data[[cols_income]]) %>%
      mutate(
        cumul_w = cumsum(w_value)
      ) %>%
      group_by(.data[[cols_income]]) %>%
      mutate(cumul_w = mean(cumul_w)) %>%
      ungroup() %>%
      mutate(
        !!rank_name := cumul_w / sum(w_value) * 100
      ) %>%
      select(year, !!sym(id), !!sym(cols_income), !!sym(rank_name))
  })
  
  ### === Merge and return final outputs ===
  join_keys <- c('year', id)
  
  rank_result <- reduce(rank_results,
                        .f = \(x, y) full_join(x, y, by = join_keys))
  
  return (rank_result)
}


#### ==== calc_rank_years ====
#'
#'
#'
#'
calc_rank_years <- function (data, start_year, end_year,
                             obs, cols_income, weight,
                             weight_sum_avg = 'sum',
                             exclude_zero = TRUE) {
  ### === Sanity check ===
  assert_that(weight_sum_avg %in% c('sum', 'avg'),
              msg = "[ERROR] value of weight_sum_avg must be either 'sum' or 'avg'")
  
  ### === Initial set-up
  if (obs == 'household') {
    w_name <- paste0(weight, '_h')
  } else {
    w_name <- paste0(weight, '_p_c')
  }
  id <- if (obs == 'household') 'hhid' else 'pid'

  sym_select <- syms(c(id, cols_income, w_name))
  
  ### === Filtering the target years ===
  data <- data %>%
    filter(year %in% start_year:end_year) %>%
    select(year, !!!sym_select) %>%
    distinct()
  
  ### === Computation of ranks ===
  comb_inc_w <- expand.grid(cols_income = cols_income,
                            weights = w_name,
                            stringsAsFactors = FALSE)
  
  rank_results <- pmap(comb_inc_w, \(cols_income, weights) {
    avg_name <- paste('avg', cols_income, sep = '_')
    rank_name <- paste('rank_years', avg_name, weight, sep = '_')

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
      group_by(!!sym(avg_name)) %>%
      mutate(w_final = mean(w_sum_avg)) %>%
      ungroup() %>%
      mutate(
        !!rank_name := cumsum(w_final) / sum(w_final) * 100
      ) %>%
      select(!!sym(id), !!sym(avg_name), !!sym(rank_name))
  })
  
  ### === Merge and return results ===
  rank_result <- reduce(rank_results,
                        .f = \(x, y) full_join(x, y, by = id))
  
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
  
  ### === Interpoate and return output ===
  data <- data %>%
    filter(!is.na(.data[[values]])) %>%
    mutate(!!rank_name := interp_fun(.data[[values]])) 
  
  return (data)
}

#### ==== calc_rank ====
#'
#'
#'
#'
#'
calc_rank <- function (merged_data,
                         child_base_year, age_bandwidth,
                         cols_income_h, cols_income_p,
                         dir_path
                         ) {
  message("[INFO] Initiated calc_rank")
  ### === Initial setup ===
  load_or_use(merged_data, dir_path)
  load_or_use(merged_child_parent, dir_path)
  load_or_use(merged_all, dir_path)
  
  child_start <- child_base_year - age_bandwidth
  child_end <- child_base_year + age_bandwidth
  parent_base_year <- round(mean(merged_child_parent$year_parent))
  parent_start <- min(merged_child_parent$year_parent)
  parent_end <- min(merged_child_parent$year_parent)
  rm(merged_child_parent)
  
  cols_income_h <- c(cols_income_h,
                   paste(cols_income_h, 'CPI', sep = '_'),
                   paste(cols_income_h, 'GDP', sep = '_'))
  
  message("[INFO] Deriving distributions and ranks")
  ### === Deriving rank distibution ===
  base_rank_child_year <- calc_rank_one(data = merged_all,
                                   target_year = child_base_year,
                                   obs = 'household',
                                   cols_income = cols_income_h,
                                   weight = 'w')
  
  base_rank_parent_year <- calc_rank_one(data = merged_all,
                                    target_year = parent_base_year,
                                    obs = 'household',
                                    cols_income = cols_income_h,
                                    weight = 'w')
  
  base_rank_child_years <- calc_rank_years(data = merged_all,
                                           start_year = child_start,
                                           end_year = child_end,
                                           obs = 'household',
                                           cols_income = cols_income_h,
                                           weight = 'w')
  
  base_rank_parent_years <- calc_rank_years(data = merged_all,
                                           start_year = parent_start,
                                           end_year = parent_end,
                                           obs = 'household',
                                           cols_income = cols_income_h,
                                           weight = 'w')
  
  message("[INFO] Interpolating the ranks")
  ### === Interpolation ===
  ### === Children data ===
  df <- data.frame(
    value_col = cols_income_h,
    rank_col = paste('rank', cols_income_h, 'w', sep = '_'),
    values = paste('avg', cols_income_h, 'child', 'w', sep = '_')
  )
  
  results <- pmap(df,
                  \(value_col, rank_col, values) {
                    data <- merged_data %>%
                      select(hhid_child, pid_child, !!values) %>%
                      distinct()
                    
                    get_approx_rank(base_rank_child_year,
                                    value_col, rank_col,
                                    data, values) %>%
                      select(hhid_child, pid_child, matches('rank'))
                  })
  
  child_one_result <- reduce(results,
                             \(x, y) {
                               full_join(x, y,
                                         by = c('hhid_child', 'pid_child'))
                               })
  
  df <- data.frame(
    value_col = paste('avg', cols_income_h, sep = '_'),
    rank_col = paste('rank', 'years', 'avg', cols_income_h, 'w', sep = '_'),
    values = paste('avg', cols_income_h, 'child', 'w', sep = '_')
  )
  
  results <- pmap(df,
                  \(value_col, rank_col, values) {
                    data <- merged_data %>%
                      select(hhid_child, pid_child, !!values) %>%
                      distinct()
                    
                    get_approx_rank(base_rank_child_years,
                                    value_col, rank_col,
                                    data, values) %>%
                      select(hhid_child, pid_child, matches('rank'))
                  })
  
  child_years_result <- reduce(results,
                                \(x, y) {
                                  full_join(x, y,
                                            by = c('hhid_child', 'pid_child'))
                                })
  
  child_rank <- full_join(child_one_result, child_years_result,
                          by = c('hhid_child', 'pid_child'),
                          suffix = c('_one', '_mult'))
  
  ### === Parent data ===
  df <- data.frame(
    value_col = cols_income_h,
    rank_col = paste('rank', cols_income_h, 'w', sep = '_'),
    values = paste('avg', cols_income_h, 'parent', 'w', sep = '_')
  )
  
  results <- pmap(df,
                  \(value_col, rank_col, values) {
                    data <- merged_data %>%
                      select(hhid_parent, pid_child, !!values) %>%
                      distinct()
                    
                    get_approx_rank(base_rank_parent_year,
                                    value_col, rank_col,
                                    data, values) %>%
                      select(hhid_parent, pid_child, matches('rank'))
                  })
  
  parent_one_result <- reduce(results,
                             \(x, y) {
                               full_join(x, y,
                                         by = c('hhid_parent', 'pid_child'))
                             })
  
  df <- data.frame(
    value_col = paste('avg', cols_income_h, sep = '_'),
    rank_col = paste('rank', 'years', 'avg', cols_income_h, 'w', sep = '_'),
    values = paste('avg', cols_income_h, 'parent', 'w', sep = '_')
  )
  
  results <- pmap(df,
                  \(value_col, rank_col, values) {
                    data <- merged_data %>%
                      select(hhid_parent, pid_child, !!values) %>%
                      distinct()
                    
                    get_approx_rank(base_rank_parent_years,
                                    value_col, rank_col,
                                    data, values) %>%
                      select(hhid_parent, pid_child, matches('rank'))
                  })
  
  parent_years_result <- reduce(results,
                               \(x, y) {
                                 full_join(x, y,
                                           by = c('hhid_parent', 'pid_child'))
                               })
  
  parent_rank <- full_join(parent_one_result, parent_years_result,
                          by = c('hhid_parent', 'pid_child'),
                          suffix = c('_one', '_mult'))
  
  message("[INFO] Merging the parent and child ranks")
  rank <- full_join(child_rank, parent_rank,
                    by = c('pid_child'),
                    relationship = 'many-to-many') %>%
    distinct()
  
  save_overwrite_rds(rank, dir_path, 'rank.rds')
  
  return(rank)
}

