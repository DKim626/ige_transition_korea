# 이전 코드들에서 대대적인 수정이 들어갔으니, 이를 참고하여 다시 작성할 것
# - 우선 rank 부분이 변경되었음.
# 
# 한편, sample transition matrix를 처음부터 builiding하는 과정이 필요함.

create_basis_data_trans <- function () {
  data <- readRDS(here::here('data/KLIPS/merge/merged_final_2021_16_2.rds'))
  
  data <- data %>%
    filter(hhid_past != hhid_curr) %>%
    filter(base_year_age <= 33) %>%
    mutate(
      educ = fcase(
        educ_self == 'High school or below', 'High school or below',
        educ_major %in% c('STEM', 'Health'), 'STEM',
        educ_major %in% c('Non-STEM'), 'Non-STEM'
      ),
      educ_f = fcase(
        educ_self_f == 'High school or below', 'High school or below',
        educ_major_f %in% c('STEM', 'Health'), 'STEM',
        educ_major_f %in% c('Non-STEM'), 'Non-STEM'
      ),
      educ_m = fcase(
        educ_self_m == 'High school or below', 'High school or below',
        educ_major_m %in% c('STEM', 'Health'), 'STEM',
        educ_major_m %in% c('Non-STEM'), 'Non-STEM'
      ),
      educ_parent = fcase(
        !is.na(educ_f), educ_f,
        is.na(educ_f) & !is.na(educ_m), educ_m
      ),
      employ_job_5_parent = fcase(
        !is.na(employ_job_5_f), employ_job_5_f,
        is.na(employ_job_5_f) & !is.na(employ_job_5_m), employ_job_5_m
      ),
      employ_job_5_alt_parent = fcase(
        !is.na(employ_job_5_alt_f), employ_job_5_alt_f,
        is.na(employ_job_5_alt_f) & !is.na(employ_job_5_alt_m), employ_job_5_alt_m
      )
    ) %>%
    filter(!is.na(educ) & !is.na(educ_f)) %>%
    select(pid,
           rank_years_avg_eq_income_2_nw_p_c_curr,
           employ_job_5, employ_job_6, employ_job_7,
           employ_job_5_alt, employ_job_6_alt, employ_job_7_alt,
           educ,
           sex, place_curr,
           rank_years_avg_eq_income_2_w_p_c_past,
           employ_job_5_f, employ_job_5_parent,
           employ_job_5_alt_f, employ_job_5_alt_parent,
           educ_f, educ_m, educ_parent,
           w_p_c_curr, sw_p_c_curr, nw_p_c_curr
    ) %>%
    distinct() %>%
    mutate_at(vars(matches('employ')), .funs = \(x) ifelse(is.na(x), 'unemp', x))
  
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
  
  data <- data %>%
    mutate(
      educ_2 = fcase(
        educ == 'High school or below', 'High school or below',
        educ %in% c('Non-STEM', 'STEM'), 'College Graduate'
      ),
      educ_2_f = fcase(
        educ_f == 'High school or below', 'High school or below',
        educ_f %in% c('Non-STEM', 'STEM'), 'College Graduate'
      ),
      educ_2_m = fcase(
        educ_m == 'High school or below', 'High school or below',
        educ_m %in% c('Non-STEM', 'STEM'), 'College Graduate'
      ),
      educ_2_parent = fcase(
        !is.na(educ_2_f), educ_2_f,
        is.na(educ_2_f) & !is.na(educ_2_m), educ_2_m
      ),
      educ_2 = factor(educ_2,
                    levels = c('High school or below',
                               'College Graduate')),
      educ_2_f = factor(educ_2_f,
                      levels = c('High school or below',
                                 'College Graduate')),
      educ_2_m = factor(educ_2_m,
                        levels = c('High school or below',
                                   'College Graduate')),
      educ_2_parent = factor(educ_2_parent,
                             levels = c('High school or below',
                                        'College Graduate')),
      educ = factor(educ,
                    levels = c('High school or below',
                               'Non-STEM', 'STEM')),
      educ_f = factor(educ_f,
                      levels = c('High school or below',
                                 'Non-STEM', 'STEM'))
    )
  
  return (data)
}

data <- create_basis_data_trans()

data_trans <- data %>%
  rename(
    Y_c = 'rank_years_avg_eq_income_2_nw_p_c_curr',
    J_c = 'employ_job_5',
    E_c = 'educ',
    Y_p = 'rank_years_avg_eq_income_2_w_p_c_past',
    J_p = 'employ_job_5_f',
    E_p = 'educ_f'
  )

data_trans <- data %>%
  rename(
    Y_c = 'rank_years_avg_eq_income_2_nw_p_c_curr',
    J_c = 'employ_job_5_alt',
    E_c = 'educ',
    Y_p = 'rank_years_avg_eq_income_2_w_p_c_past',
    J_p = 'employ_job_5_alt_f',
    E_p = 'educ_f'
  )

data_trans <- data %>%
  rename(
    Y_c = 'rank_years_avg_eq_income_2_nw_p_c_curr',
    J_c = 'employ_job_5_alt',
    E_c = 'educ_2',
    Y_p = 'rank_years_avg_eq_income_2_w_p_c_past',
    J_p = 'employ_job_5_alt_f',
    E_p = 'educ_2_f'
  ) %>%
  select(Y_c, J_c, E_c, Y_p, J_p, E_p, w_p_c_curr) %>%
  filter(complete.cases(.))

data_trans <- data %>%
  rename(
    Y_c = 'rank_years_avg_eq_income_2_nw_p_c_curr',
    J_c = 'employ_job_5_alt',
    E_c = 'educ_2',
    Y_p = 'rank_years_avg_eq_income_2_w_p_c_past',
    J_p = 'employ_job_5_alt_parent',
    E_p = 'educ_2_parent'
  ) %>%
  select(Y_c, J_c, E_c, Y_p, J_p, E_p, nw_p_c_curr) %>%
  filter(complete.cases(.))


merged_final_2021 <- readRDS(here::here('data/KLIPS/merge/merged_final_2021_16_2.rds'))


temp <- merged_final_2021 %>%
  filter(hhid_curr != hhid_past) %>%
  mutate(cohort = as.factor(cohort)) %>%
  filter(base_year_age <= 33)

temp <- merged_final_2021 %>%
  filter(hhid_curr != hhid_past) %>%
  mutate(cohort = as.factor(cohort)) %>%
  filter(log_eq_income_2_CPI_curr > 0 & log_eq_income_2_CPI_past > 0 &
           base_year_age >= 30)

data <- data %>%
  filter(pid %in% temp$pid)

temp <- temp %>%
  filter(pid %in% data$pid)

summary(lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
           weights = nw_p_c_curr, data = temp))

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
  
  weight <- 'nw_p_c_curr'
  
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
                               weight = 'nw_p_c_curr', type = NULL) %>%
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
                                          weight = 'nw_p_c_curr',alpha = 1,
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

model <- lm(income_CPI_curr ~ income_CPI_past, data)
summary(model)
model <- lm(eq_income_CPI_curr ~ eq_income_CPI_past, data)
summary(model)
model <- lm(log_income_CPI_curr ~ log_income_CPI_past, data)
summary(model)
model <- lm(log_eq_income_CPI_curr ~ log_eq_income_CPI_past, data)
summary(model)

model <- lm(rank_years_avg_income_w_curr ~ rank_years_avg_income_w_past, data)
summary(model)

model <- lm(rank_years_avg_eq_income_w_curr ~ rank_years_avg_eq_income_w_past, data)
summary(model)

mix_time_tv <- function(P,
                        eps = 0.01,
                        max_t = 1000,
                        lazy = FALSE,          # TRUE면 P'=(I+P)/2로 aperiodic 보정
                        renormalize = TRUE,    # 수치적 잡음 보정
                        tol = 1e-10,
                        verbose = TRUE) {
  
  stopifnot(is.matrix(P), nrow(P) == ncol(P))
  n <- nrow(P)
  
  # 0) column- vs row-stochastic 감지 및 통일
  row_sums <- rowSums(P)
  col_sums <- colSums(P)
  row_ok <- max(abs(row_sums - 1)) < 1e-6
  col_ok <- max(abs(col_sums - 1)) < 1e-6
  
  if (!row_ok && col_ok) {
    if (verbose) message("Detected column-stochastic; transposing to row-stochastic internally.")
    P <- t(P)
    row_sums <- rowSums(P)
    row_ok <- max(abs(row_sums - 1)) < 1e-6
  }
  
  if (!row_ok && renormalize) {
    if (verbose) message("Row sums not exactly 1; renormalizing rows.")
    P <- P / row_sums
  } else if (!row_ok) {
    stop("P does not appear to be (row- or column-) stochastic to tolerance.")
  }
  
  # 1) (선택) lazy화로 aperiodicity 확보
  if (lazy) {
    P <- 0.5 * (diag(n) + P)
  }
  
  # 2) 정상분포 pi 계산 (왼쪽 고정분포: pi^T P = pi^T)
  stationary_dist <- function(P) {
    # 선형시스템으로 안정적 계산: t(P) - I 의 한 줄을 1로 교체하고 sum(pi)=1 강제
    A <- t(P) - diag(n)
    A[1, ] <- 1
    b <- c(1, rep(0, n - 1))
    # solve가 singular 경고 시 MASS::ginv 대체 가능
    pi <- as.numeric(solve(A, b))
    # 수치 오차 보정
    pi[pi < 0 & pi > -1e-12] <- 0
    pi <- pi / sum(pi)
    if (any(pi < -1e-8)) warning("Stationary distribution has negative entries (chain may be reducible).")
    pi
  }
  pi <- stationary_dist(P)
  
  # 3) TV 거리 계산: P^t의 각 row와 pi 사이의 0.5 L1 거리
  tv_from_Pt <- function(Pt, pi) {
    0.5 * apply(abs(Pt - matrix(pi, n, n, byrow = TRUE)), 1, sum) |> max()
  }
  
  # 4) 반복 곱으로 P^t 누적, d(t) 모니터링
  tv_path <- numeric(max_t)
  Pt <- diag(n)
  for (t in 1:max_t) {
    Pt <- Pt %*% P
    # 작은 음수/초과를 수치적으로 잘라내고 재정규화(확률해석 안정화)
    if (renormalize) {
      Pt[Pt < 0 & Pt > -1e-15] <- 0
      Pt <- Pt / rowSums(Pt)
    }
    tv_path[t] <- tv_from_Pt(Pt, pi)
    if (verbose && (t %% 10 == 0 || tv_path[t] <= eps)) {
      message(sprintf("t=%d, TV=%.6f", t, tv_path[t]))
    }
    if (tv_path[t] <= eps) {
      return(list(
        t_mix = t,
        epsilon = eps,
        tv_path = tv_path[1:t],
        pi = pi,
        lazy = lazy
      ))
    }
  }
  
  # 도달 실패
  warning(sprintf("Did not reach TV <= %.3g within max_t=%d.", eps, max_t))
  list(t_mix = NA_integer_, epsilon = eps, tv_path = tv_path, pi = pi, lazy = lazy)
}

mix_time_tv(trans_matrix_smooth, eps = 0.0001, lazy =FALSE)
