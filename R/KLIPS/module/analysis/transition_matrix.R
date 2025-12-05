# 이전 코드들에서 대대적인 수정이 들어갔으니, 이를 참고하여 다시 작성할 것
# - 우선 rank 부분이 변경되었음.
# 
# 한편, sample transition matrix를 처음부터 builiding하는 과정이 필요함.

create_basis_data_trans <- function () {
  data <- readRDS(here::here('data/KLIPS/merge/merged_final.rds'))
  
  data <- data %>%
    filter(hhid_child != hhid_parent) %>%
    mutate(
      educ_child = fcase(
        educ_self_child == 'High school or below', 'High school or below',
        educ_major_child %in% c('STEM', 'Health'), 'STEM',
        educ_major_child %in% c('Non-STEM'), 'Non-STEM'
      ),
      educ_parent = fcase(
        educ_self_parent == 'High school or below', 'High school or below',
        educ_major_parent %in% c('STEM', 'Health'), 'STEM',
        educ_major_parent %in% c('Non-STEM'), 'Non-STEM'
      )
    ) %>%
    filter(!is.na(educ_child) & !is.na(educ_parent)) %>%
    mutate(year_parent = as.factor(year_parent)) %>%
    select(rank_avg_eq_income_child_mult,
           employ_job_5_child, employ_job_6_child, employ_job_7_child,
           employ_job_5_alt_child, employ_job_6_alt_child, employ_job_7_alt_child,
           educ_child,
           sex_child, place_curr_child,
           rank_avg_eq_income_parent_mult,
           employ_job_5_parent,
           employ_job_5_alt_parent,
           educ_parent,
           w_p_c_child
    ) %>%
    distinct() %>%
    mutate_at(vars(matches('employ')), .funs = \(x) ifelse(is.na(x), 'unemp', x)) %>%
    filter(complete.cases(.))
  
  cols <- colnames(data)[stri_detect_fixed(colnames(data), 'rank')]
  for (col in cols) {
    data[[col]] <- fcase(
      data[[col]] <= 30, 'low',
      data[[col]] <= 70, 'mid',
      data[[col]] <= 100, 'high'
    )
    
    data[[col]] <- factor(data[[col]], levels = c('low', 'mid', 'high'))
  }
  
  cols <- colnames(data)[stri_detect_regex(colnames(data), 'employ_job')]
  for (col in cols) {
    if (stri_detect_fixed(col, 'alt')) {
      data[[col]] <- factor(data[[col]],
                            levels = c('unemp', 'Low', 'Middle', 'High'))
    } else {
      data[[col]] <- factor(data[[col]],
                            levels = c('unemp', 'Farming', 'Low', 'Middle', 'High'))
    }
    
  }
  cols <- colnames(data)[stri_detect_regex(colnames(data), 'educ')]
  for (col in cols) {
    
    data[[col]] <- factor(data[[col]],
                          levels = c('High school or below',
                                     'Non-STEM', 'STEM'))
    
  }

  return (data)
}

data <- create_basis_data_trans()

data_trans <- data %>%
  rename(
    Y_c = 'rank_avg_eq_income_child_mult',
    J_c = 'employ_job_5_child',
    E_c = 'educ_child',
    Y_p = 'rank_avg_eq_income_parent_mult',
    J_p = 'employ_job_5_parent',
    E_p = 'educ_parent'
  )

data_trans <- data %>%
  rename(
    Y_c = 'rank_avg_eq_income_child_mult',
    J_c = 'employ_job_5_alt_child',
    E_c = 'educ_child',
    Y_p = 'rank_avg_eq_income_parent_mult',
    J_p = 'employ_job_5_alt_parent',
    E_p = 'educ_parent'
  )


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
  
  weight <- 'w_p_c_child'
  
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
                               weight = 'w_p_c_child', type = NULL) %>%
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

sample_prop_diri <- sample_prop_diri(data = data_trans,
                               cond_vars = c('Y_p', 'J_p', 'E_p'),
                               depen_var = c('Y_c', 'J_c', 'E_c'),
                               weight = 'w_p_c_child',alpha = 1,
                               type = NULL) %>%
  mutate(cond_prob_ = ifelse(is.na(cond_prob_), 0, cond_prob_))

sample_mat_diri <- sample_prop_diri %>%
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
eigen_decomp_smooth <- eigen(trans_matrix_smooth)
eigen_decomp_smooth$values %>% round(2)

frobenius_norm <- sqrt(sum((trans_matrix - trans_matrix_smooth)^2))
norm(trans_matrix - trans_matrix_smooth, type = 'F')

trans_matrix <- create_trans_mat(trans_data[[1]])
trans_matrix <- create_trans_mat(trans_data[[1]], normalize = TRUE)
eigen_decomp <- eigen(trans_matrix)
eigen_decomp$values %>% round(2)
colSums(trans_matrix)

n <- ncol(trans_matrix_smooth)
shorrocks <- (n - sum(diag(trans_matrix_smooth))) / (n-1)


row_JS <- function(P, Q, eps=1e-12) {
  P <- pmax(P, eps); Q <- pmax(Q, eps)
  P <- P / rowSums(P); Q <- Q / rowSums(Q)
  M <- 0.5*(P+Q)
  kl <- function(a,b) rowSums(a* (log(a) - log(b)))
  0.5*kl(P,M) + 0.5*kl(Q,M)
}
row_TV <- function(P, Q) 0.5*rowSums(abs(P - Q))  # assumes rows sum to 1



JS_emp_vs_fact <- row_JS(sample_mat, trans_matrix)
TV_emp_vs_fact <- row_TV(sample_mat, trans_matrix)
summary(JS_emp_vs_fact); summary(TV_emp_vs_fact)

JS_emp_vs_fact <- row_JS(t(sample_mat), t(trans_matrix))
TV_emp_vs_fact <- row_TV(t(sample_mat), t(trans_matrix))

summary(JS_emp_vs_fact); summary(TV_emp_vs_fact)

JS_emp_vs_fact <- row_JS(sample_mat_diri, trans_matrix)
TV_emp_vs_fact <- row_TV(sample_mat_diri, trans_matrix)
summary(JS_emp_vs_fact); summary(TV_emp_vs_fact)

JS_emp_vs_fact <- row_JS(t(sample_mat_diri), t(trans_matrix))
TV_emp_vs_fact <- row_TV(t(sample_mat_diri), t(trans_matrix))

summary(JS_emp_vs_fact); summary(TV_emp_vs_fact)

              