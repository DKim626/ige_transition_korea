koweps_list <- readRDS('data/KOWEPS/merge/list_data_child.rds')
klips_list <- readRDS('data/KLIPS/merge/list_data_child_2021.rds')

rel_ratio <- function (data, data_name, col_type, age, col) {
  
  s_inc <- sym(col)
  
  data <- data %>%
    select(pid, !!s_inc)
  
  get_mean_other <- function (inc) {
    
    sum_other <- sum(inc) - inc
    n_other <- sum(!is.na(inc)) - 1
    
    result <- sum_other / n_other
    
    return (result)
  }
  
  mean_other_data <- data %>%
    group_by(pid) %>%
    mutate(
      mean_other = get_mean_other(!!s_inc)
    ) %>%
    ungroup()
  
  lambda_1 <- cor(pull(mean_other_data, !!s_inc),
                  mean_other_data$mean_other,
                  use = 'complete.obs')
  dimnames(lambda_1) <- NULL
  
  var_components <- data %>%
    group_by(pid) %>%
    summarize(
      mean_y = mean(!!s_inc, na.rm = TRUE),
      within_var = var(!!s_inc, na.rm = TRUE)
    )
  
  between_var <- var(var_components$mean_y, na.rm = TRUE)
  
  within_var  <- mean(var_components$within_var, na.rm = TRUE)
  
  lambda_2 <- between_var / (between_var + within_var)
  
  result <- tibble(
    "Data" = data_name,
    "Income" = col_type,
    "Children's Age" = age,
    "Repeated Measure" = lambda_1,
    "Variance Decomposition" = lambda_2
  )
  
  return (result)
}



rel_tab <- function (klips_list, koweps_list) {
  
  klips_past <- klips_list$past_data
  klips_curr <- klips_list$current_data
  koweps_past <- koweps_list$past_data %>%
    filter(year_birth <= 1991)
  koweps_curr <- koweps_list$current_data %>%
    filter(year_birth <= 1991)
  
  klips_past_33 <- klips_past %>%
    filter(birth_year >= 1988)
  klips_past_40 <- klips_past %>%
    filter(birth_year <= 1991)
  klips_curr_33 <- klips_curr %>%
    filter(base_year_age <= 33)
  klips_curr_40 <- klips_curr %>%
    filter(base_year_age >= 30)
  
  ratio_klips_parent <- rel_ratio(klips_past, "KLIPS", "Parental Income",
                                  "25--40", "log_eq_income_2_CPI")
  ratio_klips_parent_33 <- rel_ratio(klips_past_33, "KLIPS", "Parental Income",
                                     "25--33", "log_eq_income_2_CPI")
  ratio_klips_parent_40 <- rel_ratio(klips_past_40, "KLIPS", "Parental Income",
                                     "30--40", "log_eq_income_2_CPI")
  ratio_klips_child <- rel_ratio(klips_curr, "KLIPS", "Child Income",
                                 "25--40", "log_eq_income_2_CPI")
  ratio_klips_child_33 <- rel_ratio(klips_curr_33, "KLIPS", "Child Income",
                                    "25--33", "log_eq_income_2_CPI")
  ratio_klips_child_40 <- rel_ratio(klips_curr_40, "KLIPS", "Child Income",
                                    "30--40", "log_eq_income_2_CPI")
  
  ratio_koweps_parent <- rel_ratio(koweps_past, "KOWEPS", "Parental Income",
                                   "25--33", "log_eq_inc_dis_CPI")
  ratio_koweps_child <- rel_ratio(koweps_curr, "KOWEPS", "Child Income",
                                   "25--33", "log_eq_inc_dis_CPI")
  
  table <- bind_rows(
    ratio_klips_parent, ratio_klips_parent_33, ratio_klips_parent_40,
    ratio_klips_child, ratio_klips_child_33, ratio_klips_child_40,
    ratio_koweps_parent, ratio_koweps_child
  )
  
  return (table)
}

table_ratio <- xtable(rel_tab(klips_list, koweps_list))
print(table_ratio, booktabs = TRUE, include.rownames = FALSE)
