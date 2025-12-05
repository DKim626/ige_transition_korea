create_basis_data_trans <- function () {
  data <- readRDS('data/KOWEPS/merge/merged_rank.rds')
  
  data <- data %>%
    filter(hh_key_past != hh_key_curr) %>%
    mutate(
      educ_2 = educ,
      educ = fcase(
        educ == 'High School or Below', 'High school or below',
        educ_major %in% c('STEM', 'Health'), 'STEM',
        educ_major %in% c('Non-STEM'), 'Non-STEM'
      ),
      educ_2_f = educ_f,
      educ_f = fcase(
        educ_f == 'High School or Below', 'High school or below',
        educ_major_f %in% c('STEM', 'Health'), 'STEM',
        educ_major_f %in% c('Non-STEM'), 'Non-STEM'
      ),
      educ_2_m = educ_m,
      educ_m = fcase(
        educ_m == 'High School or Below', 'High school or below',
        educ_major_m %in% c('STEM', 'Health'), 'STEM',
        educ_major_m %in% c('Non-STEM'), 'Non-STEM'
      ),
      educ_parent = fcase(
        !is.na(educ_f), educ_f,
        is.na(educ_f) & !is.na(educ_m), educ_m
      ),
      educ_2_parent = fcase(
        !is.na(educ_2_f), educ_2_f,
        is.na(educ_2_f) & !is.na(educ_2_m), educ_2_m
      ),
      occ_parent = fcase(
        !is.na(occ_f), occ_f,
        is.na(occ_f) & !is.na(occ_m), occ_m
      ),
      rank_years_avg_eq_inc_cur_weight_general_ind_c_parent = fcase(
        !is.na(rank_years_avg_eq_inc_cur_weight_general_ind_c_f),
        rank_years_avg_eq_inc_cur_weight_general_ind_c_f,
        is.na(rank_years_avg_eq_inc_cur_weight_general_ind_c_f) &
          !is.na(rank_years_avg_eq_inc_cur_weight_general_ind_c_m),
        rank_years_avg_eq_inc_cur_weight_general_ind_c_m
      )
    ) %>%
    filter(!is.na(educ_2) & !is.na(educ_2_f)) %>%
    select(pid,
           rank_years_avg_eq_inc_cur_weight_general_ind_c_2_curr,
           occ,
           educ, educ_2,
           sex,
           area_5,
           rank_years_avg_eq_inc_cur_weight_general_ind_c_f,
           rank_years_avg_eq_inc_cur_weight_general_ind_c_m,
           rank_years_avg_eq_inc_cur_weight_general_ind_c_parent,
           occ_f, occ_m, occ_parent,
           educ_f, educ_2_f,
           educ_m, educ_2_m,
           educ_parent, educ_2_parent,
           weight_general_ind_c_curr,
           weight_general_ind_c_1_curr,
           weight_general_ind_c_2_curr
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
                          levels = c('unemp', 'Low', 'Middle', 'High'))
  }
  
  data <- data %>%
    mutate(
      educ_2 = factor(educ_2,
                      levels = c('High School or Below',
                                 'Above College')),
      educ_2_f = factor(educ_2_f,
                        levels = c('High School or Below',
                                   'Above College')),
      educ_2_m = factor(educ_2_m,
                        levels = c('High School or Below',
                                   'Above College')),
      educ_2_parent = factor(educ_2_parent,
                             levels = c('High School or Below',
                                        'Above College')),
      educ = factor(educ,
                    levels = c('High school or below',
                               'Non-STEM', 'STEM')),
      educ_f = factor(educ_f,
                      levels = c('High school or below',
                                 'Non-STEM', 'STEM')),
      educ_m = factor(educ_m,
                      levels = c('High school or below',
                                 'Non-STEM', 'STEM')),
      educ_parent = factor(educ_parent,
                           levels = c('High school or below',
                                      'Non-STEM', 'STEM'))
    )
  
  return (data)
}

data <- create_basis_data_trans()

data_trans <- data %>%
  rename(
    Y_c = 'rank_years_avg_eq_inc_cur_weight_general_ind_c_2_curr',
    J_c = 'occ',
    E_c = 'educ',
    Y_p = 'rank_years_avg_eq_inc_cur_weight_general_ind_c_f',
    J_p = 'occ_f',
    E_p = 'educ_f'
  )

data_trans <- data %>%
  rename(
    Y_c = 'rank_years_avg_eq_inc_cur_weight_general_ind_c_2_curr',
    J_c = 'occ',
    E_c = 'educ_2',
    Y_p = 'rank_years_avg_eq_inc_cur_weight_general_ind_c_parent',
    J_p = 'occ_parent',
    E_p = 'educ_2_parent'
  ) %>%
  select(pid, Y_c, J_c, E_c, Y_p, J_p, E_p, weight_general_ind_c_2_curr) %>%
  filter(complete.cases(.))

merge <- readRDS('data/KOWEPS/merge/merged_rank.rds')

temp <- merge %>%
  filter(log_eq_inc_cur_CPI_curr > 0 & log_eq_inc_cur_CPI_past > 0) %>%
  filter(pid %in% data_trans$pid)

temp <- merge %>%
  filter(log_eq_inc_cur_CPI_curr > 0 & log_eq_inc_cur_CPI_past > 0)

summary(lm(log_eq_inc_cur_CPI_curr ~ log_eq_inc_cur_CPI_past,
           data = temp, weights = weight_general_ind_c_2_curr))

sample_prop_mat <- function (data, cond_vars, depen_var, weight, type) {
  
  X <- data[,c(cond_vars, weight)]
  Y <- data[,c(depen_var, cond_vars, weight)]
  symeight <- sym(weight)
  cond_prob_name <- paste0("cond_prob_", type)
  
  group_cond <- syms(cond_vars)
  group_all <- syms(c(depen_var, cond_vars))
  
  Y_prop <- Y %>%
    group_by(!!!group_all) %>%
    summarize(n = sum(!!symeight), .groups = 'drop') %>%
    mutate(Y_prob = n / sum(n)) %>%
    select(-n)
  
  X_prop <- X %>%
    group_by(!!!group_cond) %>%
    summarize(n = sum(!!symeight), .groups = 'drop') %>%
    mutate(X_prob = n / sum(n)) %>%
    select(-n)
  
  result <- left_join(Y_prop, X_prop, by = cond_vars) %>%
    mutate(!!cond_prob_name := Y_prob / X_prob)
  
  return (result[,c(depen_var, cond_vars, cond_prob_name)])
}

sample_prop_diri <- function (data, cond_vars, depen_var, weight, alpha, type) {
  
  Y <- data[,c(depen_var, cond_vars, weight)]
  symeight <- sym(weight)
  cond_prob_name <- paste0("cond_prob_", type)
  
  group_cond <- syms(cond_vars)
  group_all <- syms(c(depen_var, cond_vars))
  
  result <- Y %>%
    group_by(!!!group_all) %>%
    summarize(
      n = sum(!!symeight),
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
    select(-n, -alpha) %>%
    ungroup()
  
  return (result[,c(depen_var, cond_vars, cond_prob_name)])
  
}

create_trans_data <- function (data, mode = 'sample', alpha = 0) {
  
  weight <- 'weight_general_ind_c_2_curr'
  
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


sample_prop <- sample_prop_mat(data = data_trans,
                               cond_vars = c('Y_p', 'J_p', 'E_p'),
                               depen_var = c('Y_c', 'J_c', 'E_c'),
                               weight = 'weight_general_ind_c_2_curr', type = NULL) %>%
  complete(Y_p, J_p, E_p, Y_c, J_c, E_c) %>%
  mutate(cond_prob_ = ifelse(is.na(cond_prob_), 0, cond_prob_))

sample_mat <- sample_prop %>%
  mutate(across(Y_p:E_c, as.numeric)) %>%
  unite('child_state', Y_c, J_c, E_c, sep = '_') %>%
  unite('parent_state', Y_p, J_p, E_p, sep = '_') %>%
  pivot_wider(
    names_from = parent_state,
    values_from = cond_prob_,
    values_fill = list(cond_prob_ = 0)
  ) %>%
  {
    temp <- .
    result_mat <- temp %>%
      select(-child_state) %>%
      as.matrix()
    rownames(result_mat) <- temp$child_state
    
    result_mat
  }

sample_prop_diri_list <- sample_prop_diri(data = data_trans,
                                          cond_vars = c('Y_p', 'J_p', 'E_p'),
                                          depen_var = c('Y_c', 'J_c', 'E_c'),
                                          weight = 'weight_general_ind_c_2_curr',alpha = 1,
                                          type = NULL) %>%
  mutate(cond_prob_ = ifelse(is.na(cond_prob_), 0, cond_prob_))

sample_mat_diri <- sample_prop_diri_list %>%
  mutate(across(Y_c:E_p, as.numeric)) %>%
  unite('child_state', Y_c, J_c, E_c, sep = '_') %>%
  unite('parent_state', Y_p, J_p, E_p, sep = '_') %>%
  pivot_wider(
    names_from = parent_state,
    values_from = cond_prob_,
    values_fill = list(cond_prob_ = 0)
  ) %>%
  {
    temp <- .
    result_mat <- temp %>%
      select(-child_state) %>%
      as.matrix()
    rownames(result_mat) <- temp$child_state
    
    result_mat
  }

colSums(sample_mat)
colSums(trans_matrix)
colSums(trans_matrix_smooth)
norm(sample_mat - trans_matrix, type = 'F')
norm(sample_mat - trans_matrix_smooth, type = 'F')
norm(trans_matrix - trans_matrix_smooth, type = 'F')

trans_data <- create_trans_data(data_trans)
trans_data_smooth <- create_trans_data(data_trans, mode = 'smooth', alpha = 0.02)
trans_matrix <- create_trans_mat(trans_data[[1]])
trans_matrix_smooth <- create_trans_mat(trans_data_smooth[[1]])
eigen_decomp <- eigen(trans_matrix)
eigen_decomp$values %>% round(2)
eigen_decomp_smooth <- eigen(t(trans_matrix_smooth))
eigen_decomp_smooth$values %>% round(2)

trans_matrix_smooth_koweps <- trans_matrix_smooth
trans_matrix_smooth_klips <- trans_matrix_smooth

frobenius_norm <- sqrt(sum((trans_matrix - trans_matrix_smooth)^2))
norm(trans_matrix - trans_matrix_smooth, type = 'F')
norm(trans_matrix_smooth_klips - trans_matrix_smooth_koweps, type = 'F')

summary(as.vector(trans_matrix_smooth_koweps - trans_matrix_smooth_klips))
quantile(as.vector(trans_matrix_smooth_koweps - trans_matrix_smooth_klips),
         probs = seq(0, 1, 0.01)) %>% round(2)
diff <- trans_matrix_smooth_koweps - trans_matrix_smooth_klips

trans_matrix <- create_trans_mat(trans_data[[1]])
trans_matrix <- create_trans_mat(trans_data[[1]], normalize = TRUE)
eigen_decomp <- eigen(trans_matrix)
eigen_decomp$values %>% round(2)
colSums(trans_matrix)

n <- ncol(trans_matrix_smooth)
shorrocks <- (n - sum(diag(trans_matrix_smooth))) / (n-1)