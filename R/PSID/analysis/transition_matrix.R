create_basis_data_trans <- function () {
  data <- readRDS(here::here('data/PSID/clean/data_rank.rds'))
  
  data <- data %>%
    mutate(
      educ_level_2 = fcase(
        educ_level == 'High School or Lower', 'High School or Lower',
        educ_level %in% c('STEM', 'Non-STEM',
                          'Health', 'College Grad (Major Unknown)'), 
        'College Grad'
      ),
      educ_level_2_f = fcase(
        educ_level_f == 'High School or Lower', 'High School or Lower',
        educ_level_f %in% c('STEM', 'Non-STEM',
                            'Health', 'College Grad (Major Unknown)'), 
        'College Grad'
      ),
      educ_level = fcase(
        educ_level == 'High School or Lower', 'High School or Lower',
        educ_level %in% c('STEM', 'Health'), 'STEM',
        educ_level %in% c('Non-STEM'), 'Non-STEM'
      ),
      educ_level_f = fcase(
        educ_level_f == 'High School or Lower', 'High School or Lower',
        educ_level_f %in% c('STEM', 'Health'), 'STEM',
        educ_level_f %in% c('Non-STEM'), 'Non-STEM'
      )
    ) %>%
    select(pid,
           rank_years_avg_eq_inc_total_cpi_past,
           occ,
           educ_level, educ_level_2,
           sex_curr,
           rank_years_avg_eq_inc_total_cpi_curr,
           race_f,
           educ_level_f, educ_level_2_f,
           occ_f,
           wt_ind_l_past, wt_ind_l_curr
    ) %>%
    distinct() %>%
    mutate_at(vars(matches('occ')), .funs = \(x) ifelse(is.na(x), 'unemp', x))
  
  cols <- colnames(data)[stri_detect_fixed(colnames(data), 'rank')]
  for (col in cols) {
    data[[col]] <- fcase(
      data[[col]] <= 30, 'low',
      data[[col]] <= 70, 'mid',
      data[[col]] <= 100, 'high'
    )
    
    data[[col]] <- factor(data[[col]], levels = c('low', 'mid', 'high'))
  }
  
  cols <- colnames(data)[stri_detect_regex(colnames(data), 'occ')]
  for (col in cols) {
      data[[col]] <- factor(data[[col]],
                            levels = c('unemp', 'Farming', 'Low', 'Middle', 'High'))
  }
  
  data <- data %>%
    mutate(
      educ_level = factor(educ_level,
                          levels = c('High School or Lower',
                                     'Non-STEM', 'STEM')),
      educ_level_f = factor(educ_level_f,
                            levels = c('High School or Lower',
                                       'Non-STEM', 'STEM')),
      educ_level_2 = factor(educ_level_2,
                            levels = c('High School or Lower',
                                       'College Grad')),
      educ_level_2_f = factor(educ_level_2_f,
                              levels = c('High School or Lower',
                                         'College Grad'))
    )
  
  # cols <- colnames(data)[stri_detect_regex(colnames(data), 'educ')]
  # for (col in cols) {
  #   
  #   data[[col]] <- factor(data[[col]],
  #                         levels = c('High School or Lower',
  #                                    'Non-STEM', 'STEM'))
  # }
  
  return (data)
}

data <- create_basis_data_trans()

data_trans <- data %>%
  rename(
    Y_c = 'rank_years_avg_eq_inc_total_cpi_curr',
    J_c = 'occ',
    E_c = 'educ_level_2',
    Y_p = 'rank_years_avg_eq_inc_total_cpi_past',
    J_p = 'occ_f',
    E_p = 'educ_level_2_f'
  ) %>%
  filter(!is.na(E_c) & !is.na(E_p))

data_trans <- data %>%
  rename(
    Y_c = 'rank_years_avg_eq_inc_total_cpi_curr',
    J_c = 'occ',
    E_c = 'educ_level',
    Y_p = 'rank_years_avg_eq_inc_total_cpi_past',
    J_p = 'occ_f',
    E_p = 'educ_level_f'
  ) %>%
  filter(!is.na(E_c) & !is.na(E_p))


sample_prop_mat <- function (data, cond_vars, depen_var, weight, type) {
  
  X <- data[,c(cond_vars, weight)]
  Y <- data[,c(depen_var, cond_vars, weight)]
  sym_weight <- sym(weight)
  cond_prob_name <- paste0("cond_prob_", type)
  
  group_cond <- syms(cond_vars)
  group_all <- syms(c(depen_var, cond_vars))
  
  Y_prop <- Y %>%
    group_by(!!!group_all) %>%
    summarize(n = sum(!!sym_weight), .groups = 'drop') %>%
    mutate(Y_prob = n / sum(n)) %>%
    select(-n)
  
  X_prop <- X %>%
    group_by(!!!group_cond) %>%
    summarize(n = sum(!!sym_weight), .groups = 'drop') %>%
    mutate(X_prob = n / sum(n)) %>%
    select(-n)
  
  result <- left_join(Y_prop, X_prop, by = cond_vars) %>%
    mutate(!!cond_prob_name := Y_prob / X_prob)
  
  return (result[,c(depen_var, cond_vars, cond_prob_name)])
}

sample_prop_diri <- function (data, cond_vars, depen_var, weight, alpha, type) {
  
  Y <- data[,c(depen_var, cond_vars, weight)]
  sym_weight <- sym(weight)
  cond_prob_name <- paste0("cond_prob_", type)
  
  group_cond <- syms(cond_vars)
  group_all <- syms(c(depen_var, cond_vars))
  
  result <- Y %>%
    group_by(!!!group_all) %>%
    summarize(
      n = sum(!!sym_weight),
      .groups = 'drop'
    ) %>%
    complete(!!!group_all, fill = list(n = 0)) %>%
    mutate(
      n = ifelse(is.na(n), 0, n),
      alpha = !!alpha,
      Y_prob = (n + alpha) / (sum(n) + sum(alpha))
    ) %>%
    group_by(!!!group_cond) %>%
    mutate(
      X_prob = sum(Y_prob),
      !!cond_prob_name := Y_prob / X_prob
    ) %>%
    select(-n, -alpha)
  
  return (result[,c(depen_var, cond_vars, cond_prob_name)])
  
}

create_trans_data <- function (data, mode = 'sample', alpha) {
  
  weight <- 'wt_ind_l_curr'
  
  cond_vars_inc <- c('J_c', 'E_c')
  depen_var_inc <- 'Y_c'
  cond_vars_occ <- c('E_c', 'Y_p', 'J_p')
  depen_var_occ <- 'J_c'
  cond_vars_edu <- c('Y_p', 'J_p', 'E_p')
  depen_var_edu <- 'E_c'
  
  if (mode == 'sample') {
    inc_m <- sample_prop_mat(data, cond_vars_inc, depen_var_inc, weight, 'inc')
    occ_m <- sample_prop_mat(data, cond_vars_occ, depen_var_occ, weight, 'occ')
    edu_m <- sample_prop_mat(data, cond_vars_edu, depen_var_edu, weight, 'edu')
  } else if (mode == 'smooth' & is.numeric(alpha)) {
    inc_m <- sample_prop_diri(data, cond_vars_inc,
                              depen_var_inc, weight, alpha, 'inc')
    occ_m <- sample_prop_diri(data, cond_vars_occ, 
                              depen_var_occ, weight, alpha, 'occ')
    edu_m <- sample_prop_diri(data, cond_vars_edu, 
                              depen_var_edu, weight, alpha, 'edu')
  }
  
  basis <- expand.grid(
    Y_c = levels(data[['Y_c']]),
    J_c = levels(data[['J_c']]),
    E_c = levels(data[['E_c']]),
    Y_p = levels(data[['Y_p']]),
    J_p = levels(data[['J_p']]),
    E_p = levels(data[['E_p']])
  ) %>% 
    as_tibble()
  
  result <- list()
  
  result[['transition probability data']] <- basis %>%
    left_join(inc_m,
              by = c(depen_var_inc, cond_vars_inc)) %>%
    left_join(occ_m,
              by = c(depen_var_occ, cond_vars_occ)) %>%
    left_join(edu_m,
              by = c(depen_var_edu, cond_vars_edu)) %>%
    mutate(across(c(cond_prob_inc, cond_prob_occ, cond_prob_edu),
                  \(x) ifelse(is.na(x), 0, x))) %>%
    mutate(
      trans_prob = cond_prob_inc * cond_prob_occ * cond_prob_edu
    ) %>%
    select(Y_c:E_p, trans_prob)
  
  result[['transition probability full data']] <- basis %>%
    left_join(inc_m,
              by = c(depen_var_inc, cond_vars_inc)) %>%
    left_join(occ_m,
              by = c(depen_var_occ, cond_vars_occ)) %>%
    left_join(edu_m,
              by = c(depen_var_edu, cond_vars_edu)) %>%
    mutate(across(c(cond_prob_inc, cond_prob_occ, cond_prob_edu),
                  \(x) ifelse(is.na(x), 0, x))) %>%
    mutate(
      trans_prob = cond_prob_inc * cond_prob_occ * cond_prob_edu
    )
  
  return (result)
}

create_trans_mat <- function (trans_data, normalize = FALSE) {
  trans_data <- trans_data %>%
    mutate(across(Y_c:E_p, as.numeric)) %>%
    unite('child_state', Y_c, J_c, E_c, sep = '_') %>%
    unite('parent_state', Y_p, J_p, E_p, sep = '_') %>%
    pivot_wider(
      names_from = parent_state,
      values_from = trans_prob,
      values_fill = list(trans_prob = 0)
    )
  
  trans_mat <- as.matrix(trans_data %>% select(-child_state))
  rownames(trans_mat) <- trans_data[['child_state']]
  
  if (normalize == TRUE) {
    for (col in 1:ncol(trans_mat)) {
      trans_mat[,col] <- trans_mat[,col] / colSums(trans_mat)[col]
    }
  }
  
  return (trans_mat)
}


trans_data <- create_trans_data(data_trans)
trans_data_smooth <- create_trans_data(data_trans, mode = 'smooth', alpha = 0.02)
trans_matrix <- create_trans_mat(trans_data[[1]])
trans_matrix_smooth <- create_trans_mat(trans_data_smooth[[1]])
eigen_decomp <- eigen(trans_matrix)
eigen_decomp$values %>% round(2)
eigen_decomp_smooth <- eigen(trans_matrix_smooth)
eigen_decomp_smooth$values %>% round(2)

trans_matrix <- create_trans_mat(trans_data[[1]])
trans_matrix <- create_trans_mat(trans_data[[1]], normalize = TRUE)
eigen_decomp <- eigen(trans_matrix)
eigen_decomp$values %>% round(2)
colSums(trans_matrix)
colSums(trans_matrix_smooth)

n <- ncol(trans_matrix_smooth)
shorrocks <- (n - sum(diag(trans_matrix_smooth))) / (n-1)
norm(trans_matrix - trans_matrix_smooth, type = 'F')
