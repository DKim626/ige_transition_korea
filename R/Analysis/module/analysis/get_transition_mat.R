
#### create_basis_data_trans ####
#'
#'
#'
#'
#'
create_basis_data_trans <- function () {
  
  
  ## List of Functions ##
  get_inc_3 <- function(rank) {
    
    result <- fcase(
      rank <= 30, 'Low',
      rank <= 70, 'Middle',
      rank <= 100, 'High'
    )
    
    result <- factor(result,
                     levels = c('Low', 'Middle', 'High'))
    
    return (result)
  }
  
  get_inc_5 <- function(rank) {
    
    result <- fcase(
      rank <= 20, 'Bottom 20%',
      rank <= 40, 'Bottom 40%',
      rank <= 60, 'Bottom 60%',
      rank <= 80, 'Bottom 80%',
      rank <= 100, 'Bottom 100%'
    )
    
    result <- factor(result,
                     levels = c('Bottom 20%', 'Bottom 40%', 'Bottom 60%', 
                                'Bottom 80%', 'Bottom 100%'))
    
    return (result)
  }
  
  merge_educ <- function (educ, major) {
    
    result <- fcase(
      educ %in% c('High school or below',
                  'High School or Below',
                  'High School or Lower'),
      'High school or below',
      major %in% c('STEM', 'Health'), 'STEM',
      major %in% c('Non-STEM'), 'Non-STEM',
      !(educ %in% c('High school or below',
                    'High School or Below',
                    'High School or Lower')) & is.na(major),
      'College Grad (Major Unknown)',
      default = educ
    )
    
    return (result)
  }
  
  merge_educ_2 <- function (educ) {
    
    result <- fcase(
      educ %in% c('High school or below', 'High School or Lower'),
      'High school or below',
      educ %in% c('Non-STEM', 'STEM', 'Health',
                  'College Grad (Major Unknown)'), 'College'
    )
    
    result <- factor(result,
                     levels = c('High school or below', 'College'))
    
    return (result)
  }
  
  merge_parent <- function(var_f, var_m) {
    
    result <- fcase(
      !is.na(var_f), var_f,
      is.na(var_f) & !is.na(var_m), var_m
    )
    
    return (result)
  }
  
  factor_occ <- function(occ) {
    
    result <- factor(occ,
                     levels = c('Unemp', 'Low', 'Middle', 'High'))
    
    return (result)
  }
  
  # KLIPS
  klips_data <- readRDS(here::here('data/KLIPS/merge/merged_final_2021_16_2.rds'))
  
  klips_data <- klips_data %>%
    filter(hhid_past != hhid_curr) %>%
    mutate(
      educ = merge_educ(educ_self, educ_major),
      educ_f = merge_educ(educ_self_f, educ_major_f),
      educ_m = merge_educ(educ_self_m, educ_major_m),
      educ_parent = merge_parent(educ_f, educ_m),
      employ_job_5_parent = merge_parent(employ_job_5_f, employ_job_5_m),
      employ_job_5_alt_parent = merge_parent(employ_job_5_alt_f,
                                             employ_job_5_alt_m),
      educ_2 = merge_educ_2(educ),
      educ_2_f = merge_educ_2(educ_f),
      educ_2_m = merge_educ_2(educ_m),
      educ_2_parent = merge_parent(educ_2_f, educ_2_m)
    ) %>%
    filter(!is.na(educ) & !is.na(educ_f)) %>%
    select(pid,
           rank_cohort_3040_eq_income_2_CPI_curr,
           employ_job_7,
           employ_job_7_alt,
           educ_2,
           rank_cohort_3040_eq_income_2_CPI_past,
           employ_job_5_alt_parent,
           educ_2_parent
    ) %>%
    distinct() %>%
    mutate_at(vars(matches('employ')),
              .funs = \(x) ifelse(is.na(x), 'Unemp', x)) %>%
    rename(
      Y_c = rank_cohort_3040_eq_income_2_CPI_curr,
      J_c = employ_job_7_alt,
      E_c = educ_2,
      Y_p = rank_cohort_3040_eq_income_2_CPI_past,
      J_p = employ_job_5_alt_parent,
      E_p = educ_2_parent
    ) %>%
    mutate(
      Y_c_5 = get_inc_5(Y_c),
      Y_p_5 = get_inc_5(Y_p),
      Y_c = get_inc_3(Y_c),
      Y_p = get_inc_3(Y_p),
      J_c = factor_occ(J_c),
      J_p = factor_occ(J_p)
    ) %>%
    select(Y_c, Y_c_5, J_c, E_c, Y_p, Y_p_5, J_p, E_p) %>%
    filter(complete.cases(.))
  
  # KOWEPS
  koweps_data <- readRDS('data/KOWEPS/merge/merged_rank.rds')
  
  koweps_data <- koweps_data %>%
    filter(hh_key_past != hh_key_curr) %>%
    mutate(
      educ = merge_educ(educ, educ_major),
      educ_f = merge_educ(educ_f, educ_major_f),
      educ_m = merge_educ(educ_m, educ_major_m),
      educ_parent = merge_parent(educ_f, educ_m),
      educ_2 = merge_educ_2(educ),
      educ_2_parent = merge_educ_2(educ_parent),
      occ_parent = merge_parent(occ_f, occ_m),
      rank_years_avg_eq_inc_cur_weight_general_ind_c_parent = fcase(
        !is.na(rank_years_avg_eq_inc_cur_weight_general_ind_c_f),
        rank_years_avg_eq_inc_cur_weight_general_ind_c_f,
        is.na(rank_years_avg_eq_inc_cur_weight_general_ind_c_f) &
          !is.na(rank_years_avg_eq_inc_cur_weight_general_ind_c_m),
        rank_years_avg_eq_inc_cur_weight_general_ind_c_m
      )
    ) %>%
    filter(!is.na(educ_2) & !is.na(educ_2_parent)) %>%
    select(pid,
           rank_years_avg_eq_inc_cur_weight_general_ind_c_2_curr,
           occ,
           educ_2,
           rank_years_avg_eq_inc_cur_weight_general_ind_c_parent,
           occ_parent,
           educ_2_parent
    ) %>%
    distinct() %>%
    mutate_at(vars(matches('occ')),
              .funs = \(x) ifelse(is.na(x), 'Unemp', x)) %>%
    rename(
      Y_c = rank_years_avg_eq_inc_cur_weight_general_ind_c_2_curr,
      J_c = occ,
      E_c = educ_2,
      Y_p = rank_years_avg_eq_inc_cur_weight_general_ind_c_parent,
      J_p = occ_parent,
      E_p = educ_2_parent
    ) %>%
    mutate(
      Y_c_5 = get_inc_5(Y_c),
      Y_p_5 = get_inc_5(Y_p),
      Y_c = get_inc_3(Y_c),
      Y_p = get_inc_3(Y_p),
      J_c = factor_occ(J_c),
      J_p = factor_occ(J_p)
    ) %>%
    select(Y_c, Y_c_5, J_c, E_c, Y_p, Y_p_5, J_p, E_p) %>%
    filter(complete.cases(.))
  
  # PSID
  psid_data <- readRDS(here::here('data/PSID/clean/data_rank.rds'))
  
  psid_data <- psid_data %>%
    mutate(
      educ_level_2 = merge_educ_2(educ_level),
      educ_level_2_f = merge_educ_2(educ_level_f),
      educ_level_2_m = merge_educ_2(educ_level_m),
      educ_level_2_parent = merge_parent(educ_level_2_f, educ_level_2_m),
      educ_level = merge_educ(educ_level, educ_level),
      educ_level_f = merge_educ(educ_level_f, educ_level_f),
      educ_level_m = merge_educ(educ_level_m, educ_level_m),
      educ_level_parent = merge_parent(educ_level_f, educ_level_m),
      occ_parent = merge_parent(occ_f, occ_m)
    ) %>%
    select(pid,
           rank_cohort_3040_eq_inc_total_cpi_curr,
           occ,
           educ_level_2,
           rank_cohort_3040_eq_inc_total_cpi_past,
           occ_parent,
           educ_level_2_parent,
    ) %>%
    distinct() %>%
    mutate_at(vars(matches('occ')),
              .funs = \(x) ifelse(is.na(x), 'Unemp', x)) %>%
    rename(
      Y_c = rank_cohort_3040_eq_inc_total_cpi_curr,
      J_c = occ,
      E_c = educ_level_2,
      Y_p = rank_cohort_3040_eq_inc_total_cpi_past,
      J_p = occ_parent,
      E_p = educ_level_2_parent
    ) %>%
    mutate(
      Y_c_5 = get_inc_5(Y_c),
      Y_p_5 = get_inc_5(Y_p),
      Y_c = get_inc_3(Y_c),
      Y_p = get_inc_3(Y_p),
      J_c = factor_occ(J_c),
      J_p = factor_occ(J_p)
    ) %>%
    select(Y_c, Y_c_5, J_c, E_c, Y_p, Y_p_5, J_p, E_p) %>%
    filter(complete.cases(.))
  
  
  data_list <- list(
    klips = klips_data,
    koweps = koweps_data,
    psid = psid_data
  )
  
  return (data_list)
}

#### get_parent_dist ####
#'
#'
#'
#'
get_parent_dist <- function (data, inc_5 = FALSE) {
  
  if (inc_5 == TRUE) {
    data <- data %>%
      select(-Y_p) %>%
      rename(Y_p = Y_p_5)
  }
  
  result <- data %>%
    group_by(Y_p, J_p, E_p) %>%
    summarize(n = n(), .groups = 'drop') %>%
    complete(fill = list(n = 0)) %>%
    mutate(prop = n / sum(n))
  
  if (inc_5 == FALSE) {
    state <- NULL
    for(i in c('L', 'M', 'H')){
      for(j in c('U', 'L', 'M', 'H')) {
        for(k in c('H', 'C')) {
          state <- c(state, paste(i, j, k, sep = '-'))
        }
      }
    }
    
    result <- result %>%
      arrange(Y_p, J_p, E_p) %>%
      mutate(
        parent_state = paste(
          substr(as.character(Y_p), 1, 1),
          substr(as.character(J_p), 1, 1),
          substr(as.character(E_p), 1, 1),
          sep = '-'
        ),
        parent_state = factor(parent_state, levels = state)
      ) %>%
      select(parent_state, prop)
  } else if (inc_5 == TRUE) {
    state <- NULL
    for(i in 1:5){
      for(j in c('U', 'L', 'M', 'H')) {
        for(k in c('H', 'C')) {
          state <- c(state, paste(i, j, k, sep = '-'))
        }
      }
    }
    
    result <- result %>%
      arrange(Y_p, J_p, E_p) %>%
      mutate(
        parent_state = paste(
          as.numeric(stri_extract_all_regex(Y_p, '\\d{2,3}')) / 20,
          substr(as.character(J_p), 1, 1),
          substr(as.character(E_p), 1, 1),
          sep = '-'
        ),
        parent_state = factor(parent_state, levels = state)
      ) %>%
      select(parent_state, prop)
  }
  
  vector <- result$prop
  names(vector) <- result$parent_state
  
  return (vector)
}

#### create_trans_data ####
#'
#'
#'
#'
#'
create_trans_data <- function (data, alpha = 0, min_sample_size = 0,
                               inc_five = FALSE,
                               alt_inc_model = FALSE,
                               alt_occ_model = FALSE) {
  
  if (alt_inc_model == TRUE) {
    cond_vars_inc <- c('J_c', 'E_c', 'Y_p')
  } else if (alt_inc_model == FALSE) {
    cond_vars_inc <- c('J_c', 'E_c')
  }
  depen_var_inc <- 'Y_c'
  
  if (alt_occ_model == TRUE) {
    cond_vars_occ <- c('E_c', 'Y_p', 'J_p', 'E_p')
  } else if (alt_occ_model == FALSE) {
    cond_vars_occ <- c('E_c', 'Y_p', 'J_p')
  }
  depen_var_occ <- 'J_c'
  
  cond_vars_edu <- c('Y_p', 'J_p', 'E_p')
  depen_var_edu <- 'E_c'
  
  if (inc_five == TRUE) {
    data <- data %>%
      select(-Y_p, -Y_c) %>%
      rename(Y_p = Y_p_5, Y_c = Y_c_5)
  }
  
  sample_prop_mat <- function (data, cond_vars, depen_var, type,
                               min_sample_size = 0) {
    
    X <- data[,c(cond_vars)]
    Y <- data[,c(depen_var, cond_vars)]
    cond_prob_name <- paste0("cond_prob_", type)
    
    group_cond <- syms(cond_vars)
    group_all <- syms(c(depen_var, cond_vars))
    
    Y_prop <- Y %>%
      group_by(!!!group_all) %>%
      summarize(
        sample_n = n(),
        .groups = 'drop') %>%
      filter(sample_n >= !!min_sample_size) %>%
      mutate(Y_prob = sample_n / sum(sample_n)) %>%
      select(-sample_n)
    
    X_prop <- X %>%
      group_by(!!!group_cond) %>%
      summarize(
        sample_n = n(),
        .groups = 'drop') %>%
      filter(sample_n >= !!min_sample_size) %>%
      mutate(X_prob = sample_n / sum(sample_n)) %>%
      select(-sample_n)
    
    result <- full_join(Y_prop, X_prop, by = cond_vars) %>%
      mutate(!!cond_prob_name := Y_prob / X_prob)
    
    return (result[,c(depen_var, cond_vars, cond_prob_name)])
  }
  
  sample_prop_diri <- function (data, cond_vars, depen_var, alpha, type,
                                min_sample_size = 0) {
    
    Y <- data[,c(depen_var, cond_vars)]
    cond_prob_name <- paste0("cond_prob_", type)
    
    group_cond <- syms(cond_vars)
    group_all <- syms(c(depen_var, cond_vars))
    
    result <- Y %>%
      group_by(!!!group_all) %>%
      summarize(
        sample_n = n(),
        .groups = 'drop'
      ) %>%
      filter(sample_n >= !!min_sample_size) %>%
      complete(!!!group_all, fill = list(sample_n = 0)) %>%
      mutate(
        alpha = !!alpha,
        Y_prob = (sample_n + alpha) / (sum(sample_n) + sum(alpha))
      ) %>%
      group_by(!!!group_cond) %>%
      mutate(
        X_prob = sum(Y_prob),
        !!cond_prob_name := Y_prob / X_prob
      ) %>%
      select(-alpha) %>%
      ungroup()
    
    return (result[,c(depen_var, cond_vars, cond_prob_name)])
    
  }
  
  inc_m <- sample_prop_mat(data, cond_vars_inc, depen_var_inc, 'inc',
                           min_sample_size)
  occ_m <- sample_prop_mat(data, cond_vars_occ, depen_var_occ, 'occ',
                           min_sample_size)
  edu_m <- sample_prop_mat(data, cond_vars_edu, depen_var_edu, 'edu',
                           min_sample_size)
  
  inc_m_s <- sample_prop_diri(data, cond_vars_inc,
                              depen_var_inc, alpha, 'inc',
                              min_sample_size)
  occ_m_s <- sample_prop_diri(data, cond_vars_occ, 
                              depen_var_occ, alpha, 'occ',
                              min_sample_size)
  edu_m_s <- sample_prop_diri(data, cond_vars_edu, 
                              depen_var_edu, alpha, 'edu',
                              min_sample_size)
  
  basis <- expand.grid(
    Y_c = levels(data[['Y_c']]),
    J_c = levels(data[['J_c']]),
    E_c = levels(data[['E_c']]),
    Y_p = levels(data[['Y_p']]),
    J_p = levels(data[['J_p']]),
    E_p = levels(data[['E_p']])
  ) %>% 
    as_tibble()
  
  sample_data <- sample_prop_mat(data,
                                 cond_vars = c('Y_p', 'J_p', 'E_p'),
                                 depen_var = c('Y_c', 'J_c', 'E_c'),
                                 'sample')
  
  sample_data <- left_join(basis,
                           sample_data,
                           by = c('Y_c', 'J_c', 'E_c', 'Y_p', 'J_p', 'E_p'))
  
  full_data <- basis %>%
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
  
  full_data_smooth <- basis %>%
    left_join(inc_m_s,
              by = c(depen_var_inc, cond_vars_inc)) %>%
    left_join(occ_m_s,
              by = c(depen_var_occ, cond_vars_occ)) %>%
    left_join(edu_m_s,
              by = c(depen_var_edu, cond_vars_edu)) %>%
    mutate(across(c(cond_prob_inc, cond_prob_occ, cond_prob_edu),
                  \(x) ifelse(is.na(x), 0, x))) %>%
    mutate(
      trans_prob = cond_prob_inc * cond_prob_occ * cond_prob_edu
    )
  
  trans_data <- list(
    sample_data = sample_data,
    trans_data = full_data %>% select(Y_c:E_p, trans_prob),
    smooth_trans_data = full_data_smooth %>% select(Y_c:E_p, trans_prob)
  )
  trans_data_full <- list(
    trans_data = full_data,
    smooth_trans_data = full_data_smooth
  )
  
  result <- list(
    "Transition matrix" = trans_data,
    "Full information" = trans_data_full
  )
  
  return (result)
}

#### get_trans_mat_list ####
#'
#'
#'
#'
#'
get_trans_mat_list <- function (data, alpha = 1, min_sample_size = 0) {
  
  mat_list <- list(
    'Regular' = list(
      'N-N' = create_trans_data(base_list[[data]], alpha = alpha,
                                min_sample_size = min_sample_size),
      'N-A' = create_trans_data(base_list[[data]], alpha = alpha,
                                min_sample_size = min_sample_size,
                                alt_occ_model = TRUE),
      'A-A' = create_trans_data(base_list[[data]], alpha = alpha,
                                min_sample_size = min_sample_size,
                                alt_inc_model = TRUE,
                                alt_occ_model = TRUE)
    ),
    'Income 5' = list(
      'N-N' = create_trans_data(base_list[[data]], alpha = alpha,
                                min_sample_size = min_sample_size,
                                inc_five = TRUE),
      'N-A' = create_trans_data(base_list[[data]], alpha = alpha,
                                min_sample_size = min_sample_size,
                                inc_five = TRUE,
                                alt_occ_model = TRUE),
      'A-A' = create_trans_data(base_list[[data]], alpha = alpha,
                                min_sample_size = min_sample_size,
                                inc_five = TRUE,
                                alt_inc_model = TRUE,
                                alt_occ_model = TRUE)
    )
  )
  
  return (mat_list)
}

#### create_trans_mat ####
#'
#'
#'
#'
#'
create_trans_mat <- function (trans_data, normalize = FALSE) {
  
  trans_data <- trans_data %>%
    mutate(across(Y_c:E_p, as.numeric)) %>%
    arrange(Y_c, J_c, E_c, Y_p, J_p, E_p) %>%
    unite('child_state', Y_c, J_c, E_c, sep = '_') %>%
    unite('parent_state', Y_p, J_p, E_p, sep = '_') %>%
    select(child_state, parent_state, trans_prob) %>%
    pivot_wider(
      names_from = child_state,
      values_from = trans_prob,
      values_fill = list(trans_prob = 0)
    )
  
  trans_mat <- as.matrix(trans_data %>% select(-parent_state))
  rownames(trans_mat) <- trans_data[['parent_state']]
  
  if (normalize == TRUE) {
    for (col in 1:ncol(trans_mat)) {
      trans_mat[,col] <- trans_mat[,col] / colSums(trans_mat)[col]
    }
  }
  
  return (trans_mat)
}

#### get_inc_occ_data ####
#'
#'
#'
get_inc_occ_edu_data <- function (data) {
  
  # Refining the data
  inc_data <- data %>%
    group_by(Y_c, Y_p) %>%
    summarize(n = n(), .groups = 'drop') %>%
    group_by(Y_p) %>%
    mutate(trans_prob = n / sum(n)) %>%
    ungroup() %>%
    select(-n)
  
  inc_data_mat <- inc_data %>%
    pivot_wider(
      names_from = Y_c,
      values_from = trans_prob,
      values_fill = list(trans_prob = 0)
    )
  
  inc_5_data <- data %>%
    group_by(Y_c_5, Y_p_5) %>%
    summarize(n = n(), .groups = 'drop') %>%
    group_by(Y_p_5) %>%
    mutate(trans_prob = n / sum(n)) %>%
    ungroup() %>%
    select(-n) %>%
    mutate(
      Y_c_5 = as.character(Y_c_5),
      Y_c_5 = fcase(
        Y_c_5 == "Bottom 100%", "Top 20%",
        Y_c_5 == "Bottom 80%", "Top 40%",
        default = Y_c_5
      ),
      Y_c_5 = factor(Y_c_5,
                     levels = c("Bottom 20%", "Bottom 40%",
                                "Bottom 60%", "Top 40%", "Top 20%")),
      Y_p_5 = as.character(Y_p_5),
      Y_p_5 = fcase(
        Y_p_5 == "Bottom 100%", "Top 20%",
        Y_p_5 == "Bottom 80%", "Top 40%",
        default = Y_p_5
      ),
      Y_p_5 = factor(Y_p_5,
                     levels = c("Bottom 20%", "Bottom 40%",
                                "Bottom 60%", "Top 40%", "Top 20%"))
    ) 
  
  inc_5_data_mat <- inc_5_data %>%
    pivot_wider(
      names_from = Y_c_5,
      values_from = trans_prob,
      values_fill = list(trans_prob = 0)
    )
  
  occ_data <- data %>%
    group_by(J_c, J_p) %>%
    summarize(n = n(), .groups = 'drop') %>%
    group_by(J_p) %>%
    mutate(trans_prob = n / sum(n)) %>%
    ungroup() %>%
    select(-n)
  
  occ_data_mat <- occ_data %>%
    pivot_wider(
      names_from = J_c,
      values_from = trans_prob,
      values_fill = list(trans_prob = 0)
    )
  
  edu_data <- data %>%
    group_by(E_c, E_p) %>%
    summarize(n = n(), .groups = 'drop') %>%
    group_by(E_p) %>%
    mutate(trans_prob = n / sum(n)) %>%
    ungroup() %>%
    select(-n)
  
  edu_data_mat <- edu_data %>%
    pivot_wider(
      names_from = E_c,
      values_from = trans_prob,
      values_fill = list(trans_prob = 0)
    )
    
  
  # Extracting parent distribution
  inc_parent_dist <- data %>%
    group_by(Y_p) %>%
    summarize(n = n(), .groups = 'drop') %>%
    complete(fill = list(n = 0)) %>%
    mutate(trans_prob = n / sum(n)) %>%
    pull(trans_prob)
  
  inc_5_parent_dist <- data %>%
    group_by(Y_p_5) %>%
    summarize(n = n(), .groups = 'drop') %>%
    complete(fill = list(n = 0)) %>%
    mutate(trans_prob = n / sum(n)) %>%
    pull(trans_prob)
  
  occ_parent_dist <- data %>%
    group_by(J_p) %>%
    summarize(n = n(), .groups = 'drop') %>%
    complete(fill = list(n = 0)) %>%
    mutate(trans_prob = n / sum(n)) %>%
    pull(trans_prob)
  
  edu_parent_dist <- data %>%
    group_by(E_p) %>%
    summarize(n = n(), .groups = 'drop') %>%
    complete(fill = list(n = 0)) %>%
    mutate(trans_prob = n / sum(n)) %>%
    pull(trans_prob)
  
  # Transforming the data to transition matrices
  inc_mat <- as.matrix(inc_data_mat %>% select(-Y_p))
  rownames(inc_mat) <- inc_5_data_mat[['Y_p']]
  
  inc_5_mat <- as.matrix(inc_5_data_mat %>% select(-Y_p_5))
  rownames(inc_5_mat) <- inc_5_data_mat[['Y_p_5']]
  
  occ_mat <- as.matrix(occ_data_mat %>% select(-J_p))
  rownames(occ_mat) <- occ_data_mat[['J_p']]
  
  edu_mat <- as.matrix(edu_data_mat %>% select(-E_p))
  rownames(edu_mat) <- edu_data_mat[['J_p']]
  
  inc_data <- inc_data %>%
    rename(child_state = Y_c, parent_state = Y_p)
  
  inc_5_data <- inc_5_data %>%
    rename(child_state = Y_c_5, parent_state = Y_p_5)
  
  occ_data <- occ_data %>%
    rename(child_state = J_c, parent_state = J_p)
  
  edu_data <- edu_data %>%
    rename(child_state = E_c, parent_state = E_p)
  
  # Aggregate the results
  list_mat <- list(inc_data, inc_mat, inc_parent_dist,
                   inc_5_data, inc_5_mat, inc_5_parent_dist,
                   occ_data, occ_mat, occ_parent_dist,
                   edu_data, edu_mat, edu_parent_dist)
  
  return(list_mat)
}

#### get_measures ####
#'
#'
#'
#'
#'
get_measures <- function (trans_data, trans_mat = NULL,
                          parent_dist, marginals = FALSE) {
  
  if (marginals == FALSE) {
    
    trans_mat <- create_trans_mat(trans_data)
    
  }
  
  total_var <- function (x, y) {
    
    result <- sum(abs(x - y)) / 2
    
    return (result)
  }
  
  mat_mult_t <- function(trans_mat, t) {
    
    i <- 1
    result <- trans_mat
    
    while (i <= t) {
      result <- result %*% trans_mat
      i <- i + 1
    }
    
    return (result)
  }
  
  get_worst_origin <- function (trans_mat, t) {
    
    results <- c()
    n_row <- nrow(trans_mat)
    
    for (i in 1:n_row) {
      
      row_i_t <- mat_mult_t(trans_mat, t)[i, ]
      row_i_steady <- mat_mult_t(trans_mat, 50)[i, ]
      
      updated_info <- total_var(row_i_t, row_i_steady)
      
      results <- c(results, updated_info)
      
    }
    
    result <- max(results)
    
    return (result)
  }
  
  get_worst_case <- function (trans_mat, t) {
    
    results <- c()
    n_row <- nrow(trans_mat)
    mat_t <- mat_mult_t(trans_mat, t)
    
    for (i in 1:(n_row-1)) {
      for (j in (i+1):n_row) {
        updated_info <- total_var(mat_t[i, ], mat_t[j, ])
        results <- c(results, updated_info)
      }
    }
    
    result <- max(results)
    
    return (result)
    
  }
  
  get_mdo <- function (trans_data, parent_dist, marginals) {
    
    get_manhattan_dist <- function (x, y) {
      
      result <- sum(abs(x - y))
      
      return (result)
      
    }
    
    get_euclidean_dist <- function (x, y) {
      
      result <- sqrt(sum((x-y)^2))
      
      return (result)
    }
    
    if (marginals == FALSE) {
      
      data <- trans_data %>%
        rowwise() %>%
        mutate(
          child_point = list(as.numeric(c(Y_c, J_c, E_c))),
          parent_point = list(as.numeric(c(Y_p, J_p, E_p)))
        ) %>%
        ungroup() %>%
        group_by(Y_c, J_c, E_c, Y_p, J_p, E_p) %>%
        mutate(
          dist_m = get_manhattan_dist(unlist(child_point), unlist(parent_point)),
          dist_e = get_euclidean_dist(unlist(child_point), unlist(parent_point)),
        ) %>%
        ungroup() %>%
        mutate(
          dist_m = dist_m / max(dist_m),
          dist_e = dist_e / max(dist_e)
        ) %>%
        group_by(Y_p, J_p, E_p) %>%
        summarize(
          mdo_m = sum(trans_prob * dist_m),
          mdo_e = sum(trans_prob * dist_e),
          .groups = 'drop'
        )
      
    } else if (marginals == TRUE) {
      
      data <- trans_data %>%
        mutate(
          child_point = c(as.numeric(child_state)),
          parent_point = c(as.numeric(parent_state))
        ) %>%
        group_by(child_state, parent_state) %>%
        mutate(
          dist_m = get_manhattan_dist(child_point, parent_point),
          dist_e = get_euclidean_dist(child_point, parent_point),
        ) %>%
        ungroup() %>%
        mutate(
          dist_m = dist_m / max(dist_m),
          dist_e = dist_e / max(dist_e)
        ) %>%
        group_by(parent_state) %>%
        summarize(
          mdo_m = sum(trans_prob * dist_m),
          mdo_e = sum(trans_prob * dist_e),
          .groups = 'drop'
        )
      
    }
    
    mdo_m <- pull(data, mdo_m)
    mdo_e <- pull(data, mdo_e)
    
    max_mdo_m <- max(mdo_m)
    max_mdo_e <- max(mdo_e)
    
    mdo_results <- tibble(
      parent_mdo_m = sum(parent_dist * mdo_m),
      mean_mdo_m = mean(mdo_m),
      sd_mdo_m = sd(mdo_m),
      max_mdo_m = max(mdo_m),
      parent_mdo_e = sum(parent_dist * mdo_e),
      mean_mdo_e = mean(mdo_e),
      sd_mdo_e = sd(mdo_e),
      max_mdo_e = max(mdo_e)
    )
    
    return (mdo_results)
    
  }
  
  get_memory <- function (trans_mat, parent_dist) {
    
    overall_mobs <- c()
    structural_mobs <- c()
    exchange_mobs <- c()
    
    for (t in 1:20) {
      
      if (t == 1) {
        dist_t <- parent_dist
        dist_t_p1 <- dist_t %*% trans_mat
      } else {
        dist_t <- parent_dist %*% mat_mult_t(trans_mat, t-1)
        dist_t_p1 <- dist_t %*% trans_mat
      }
      
      overall_mob <- 1 - sum(dist_t * diag(trans_mat))
      structural_mob <- total_var(dist_t, dist_t_p1)
      exchange_mob <- overall_mob - structural_mob
      
      overall_mobs <- c(overall_mobs, overall_mob)
      structural_mobs <- c(structural_mobs, structural_mob)
      exchange_mobs <- c(exchange_mobs, exchange_mob)
      
      if (structural_mob <= 1e-04) break
    }
    
    result <- list(
      "Overall Mobility" = overall_mobs,
      "Structual Mobility" = structural_mobs,
      "Exchange Mobility" = exchange_mobs
    )
    
    return (result)
  }
  
  eigen_list <- eigen(trans_mat)
  
  # Eigenvalues
  second_eigen <- eigen_list$values[2]
  abs_eigen <- abs(second_eigen)
  
  # Mean Exit Time
  mean_time_exit <- 1 / (1 - diag(trans_mat))
  
  # Worst-cases
  worst_origin <- get_worst_origin(trans_mat, t = 1)
  dobrushin <- get_worst_case(trans_mat, t = 1)
  
  # Mean Distance from Origin
  mdo <- get_mdo(trans_data, parent_dist, marginals)
  
  # Memory Curve
  memory <- get_memory(trans_mat, parent_dist)
  
  # Results
  result <- list(
    "Second eigenvalue" = second_eigen,
    "Absolute value of second eigenvalue" = abs_eigen,
    'Mean Time to Exit' = mean_time_exit,
    'Worst-Origin' = worst_origin,
    'Dobrushin' = dobrushin,
    "Mean Distance from Origin" = mdo,
    "Memory Curve" = memory
  )
  
  return (result)
}

#### order_states ####
#'
#'
#'
#'
#'
order_states <- function (trans_data, prob, inc_5 = FALSE, alt = FALSE) {
  
  if (inc_5 == FALSE) {
    
    state <- NULL
    
    if (alt == FALSE) {
      
      for(i in c('L', 'M', 'H')){
        for(j in c('U', 'L', 'M', 'H')) {
          for(k in c('H', 'C')) {
            state <- c(state, paste(i, j, k, sep = '-'))
          }
        }
      }
      
    } else if (alt == 'inc_2') {
      
      for(i in c('L', 'M', 'H')){
        for(k in c('H', 'C')) {
          for(j in c('U', 'L', 'M', 'H')) {
            state <- c(state, paste(i, j, k, sep = '-'))
          }
        }
      }
    } else if (alt == 'occ') {
      
      for(j in c('U', 'L', 'M', 'H')){
        for(i in c('L', 'M', 'H')) {
          for(k in c('H', 'C')) {
            state <- c(state, paste(i, j, k, sep = '-'))
          }
        }
      }
      
    } else if (alt == 'occ_2') {
      
      for(j in c('U', 'L', 'M', 'H')){
        for(k in c('H', 'C')) {
          for(i in c('L', 'M', 'H')) {
            state <- c(state, paste(i, j, k, sep = '-'))
          }
        }
      }
      
    } else if (alt == 'educ') {
      
      for(k in c('H', 'C')) {
        for(i in c('L', 'M', 'H')){
          for(j in c('U', 'L', 'M', 'H')) {
          
            state <- c(state, paste(i, j, k, sep = '-'))
            
          }
        }
      }
      
    } else if (alt == 'educ_2') {
      
      for(k in c('H', 'C')) {
          for(j in c('U', 'L', 'M', 'H')) {
            for(i in c('L', 'M', 'H')){
            
            state <- c(state, paste(i, j, k, sep = '-'))
            
          }
        }
      }
    }
    
    result <- trans_data %>%
      arrange(Y_c, J_c, E_c, Y_p, J_p, E_p) %>%
      mutate(
        child_state = paste(
          substr(as.character(Y_c), 1, 1),
          substr(as.character(J_c), 1, 1),
          substr(as.character(E_c), 1, 1),
          sep = '-'
        ),
        child_state = factor(child_state, levels = state),
        parent_state = paste(
          substr(as.character(Y_p), 1, 1),
          substr(as.character(J_p), 1, 1),
          substr(as.character(E_p), 1, 1),
          sep = '-'
        ),
        parent_state = factor(parent_state, levels = state)
      ) %>%
      select(child_state, parent_state, !!sym(prob))
    
  } else if (inc_5 == TRUE) {
    
    state <- NULL
    
    if (alt == FALSE) {
      
      for(i in 1:5){
        for(j in c('U', 'L', 'M', 'H')) {
          for(k in c('H', 'C')) {
            state <- c(state, paste(i, j, k, sep = '-'))
          }
        }
      }
      
    } else if (alt == 'inc_2') {
      
      for(i in 1:5){
        for(k in c('H', 'C')) {
          for(j in c('U', 'L', 'M', 'H')) {
            state <- c(state, paste(i, j, k, sep = '-'))
          }
        }
      }
      
    } else if (alt == 'occ') {
      
      for(j in c('U', 'L', 'M', 'H')){
        for(i in 1:5) {
          for(k in c('H', 'C')) {
            state <- c(state, paste(i, j, k, sep = '-'))
          }
        }
      }
      
    } else if (alt == 'occ_2') {
      
      for(j in c('U', 'L', 'M', 'H')){
        for(k in c('H', 'C')) {
          for(i in 1:5) {
            state <- c(state, paste(i, j, k, sep = '-'))
          }
        }
      }
      
    } else if (alt == 'educ') {
      
      for(k in c('H', 'C')) {
        for(i in 1:5){
          for(j in c('U', 'L', 'M', 'H')) {
            state <- c(state, paste(i, j, k, sep = '-'))
          }
        }
      }
      
    } else if (alt == 'educ_2') {
      
      for(k in c('H', 'C')) {
        for(j in c('U', 'L', 'M', 'H')) {
          for(i in 1:5){
            state <- c(state, paste(i, j, k, sep = '-'))
          }
        }
      }
      
    }
    
    result <- trans_data %>%
      arrange(Y_c, J_c, E_c, Y_p, J_p, E_p) %>%
      mutate(
        child_state = paste(
          as.numeric(stri_extract_all_regex(Y_c, '\\d{2,3}')) / 20,
          substr(as.character(J_c), 1, 1),
          substr(as.character(E_c), 1, 1),
          sep = '-'
        ),
        child_state = factor(child_state, levels = state),
        parent_state = paste(
          as.numeric(stri_extract_all_regex(Y_p, '\\d{2,3}')) / 20,
          substr(as.character(J_p), 1, 1),
          substr(as.character(E_p), 1, 1),
          sep = '-'
        ),
        parent_state = factor(parent_state, levels = state)
      ) %>%
      select(child_state, parent_state, !!sym(prob))
  }
  
  return (result)
}

#### get_heat_map ####
#'
#'
#'
#'
#'
get_heat_map <- function(trans_data, prob, inc_5 = FALSE, alt = FALSE) {
  
  n_length <- ifelse(inc_5 == FALSE, 24, 40)
  div_1 <- fcase(
    (alt == FALSE | alt == 'inc_2') & inc_5 == FALSE, 3,
    (alt == FALSE | alt == 'inc_2') & inc_5 == TRUE, 5,
    alt %in% c('occ', 'occ_2'), 4,
    alt %in% c('educ', 'educ_2'), 2
  )
  div_2 <- fcase(
    alt == FALSE & inc_5 == FALSE, 12,
    alt == 'inc_2' & inc_5 == FALSE, 6,
    alt == FALSE & inc_5 == TRUE, 20,
    alt == 'inc_2' & inc_5 == TRUE, 10,
    alt == 'occ' & inc_5 == FALSE, 12,
    alt == 'occ_2' & inc_5 == FALSE, 8,
    alt == 'occ' & inc_5 == TRUE, 20,
    alt == 'occ_2' & inc_5 == TRUE, 8,
    alt == 'educ' & inc_5 == FALSE, 6,
    alt == 'educ_2' & inc_5 == FALSE, 8,
    alt == 'educ' & inc_5 == TRUE, 10,
    alt == 'educ_2' & inc_5 == TRUE, 8
  )
  
  blocks_size_1 <- seq(0, n_length, n_length/div_1) + 0.5
  blocks_size_2 <- seq(0, n_length, n_length/div_2) + 0.5
  
  blocks_1 <- as_tibble(expand.grid(x = 1:(length(blocks_size_1) - 1),
                                  y = 1:(length(blocks_size_1) - 1))) %>%
    mutate(
      x_min = blocks_size_1[x],
      x_max = blocks_size_1[x+1],
      y_min = blocks_size_1[y],
      y_max = blocks_size_1[y+1],
    ) %>%
    select(x_min:y_max)
  
  blocks_2 <- as_tibble(expand.grid(x = 1:(length(blocks_size_2) - 1),
                                    y = 1:(length(blocks_size_2) - 1))) %>%
    mutate(
      x_min = blocks_size_2[x],
      x_max = blocks_size_2[x+1],
      y_min = blocks_size_2[y],
      y_max = blocks_size_2[y+1],
    ) %>%
    select(x_min:y_max)
  
  if (inc_5 == FALSE) {
    limits <- c(0, 0.7)
    breaks <- seq(0, 0.7, 0.1)
  } else if (inc_5 == TRUE) {
    limits <- c(0, 0.4)
    breaks <- seq(0, 0.4, 0.1)
  }
  
  heat_map <- trans_data %>%
    order_states(prob = prob, inc_5, alt) %>%
    ggplot(aes(x = child_state, y = parent_state, fill = !!sym(prob))) +
    geom_tile(color = "white", lwd = 0.5, linetype = 1) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90),
      legend.position = 'bottom'
    ) +
    scale_fill_gradient2(
      low = "grey", mid = 'white', high = "red",
      limits = limits,
      breaks = breaks
      ) +
    labs(y = "Parent States (Income-Occupation-Education)",
         x = 'Child States (Income-Occupation-Education)',
         fill = 'Probability')+
    geom_rect(data = blocks_2,
              mapping = aes(xmin = x_min, xmax = x_max,
                            ymin = y_min, ymax = y_max),
              color = 'grey', fill = NA, linewidth = 0.4, inherit.aes = FALSE) +
    geom_rect(data = blocks_1,
              mapping = aes(xmin = x_min, xmax = x_max,
                            ymin = y_min, ymax = y_max),
              color = 'red', fill = NA, linewidth = 0.4, inherit.aes = FALSE) 
  
  return (heat_map)
}

#### get_inc_heat_map ####
#'
#'
#'
get_inc_heat_map <- function(data) {
  data %>%
    group_by(Y_c_5, Y_p_5) %>%
    summarize(n = n(), .groups = 'drop') %>%
    group_by(Y_p_5) %>%
    mutate(prop = n / sum(n)) %>%
    ungroup() %>%
    mutate(
      Y_c_5 = as.character(Y_c_5),
      Y_c_5 = fcase(
        Y_c_5 == "Bottom 100%", "Top 20%",
        Y_c_5 == "Bottom 80%", "Top 40%",
        default = Y_c_5
      ),
      Y_c_5 = factor(Y_c_5,
                     levels = c("Bottom 20%", "Bottom 40%",
                                "Bottom 60%", "Top 40%", "Top 20%")),
      Y_p_5 = as.character(Y_p_5),
      Y_p_5 = fcase(
        Y_p_5 == "Bottom 100%", "Top 20%",
        Y_p_5 == "Bottom 80%", "Top 40%",
        default = Y_p_5
      ),
      Y_p_5 = factor(Y_p_5,
                     levels = c("Bottom 20%", "Bottom 40%",
                                "Bottom 60%", "Top 40%", "Top 20%"))
    ) %>%
    ggplot(aes(x = Y_c_5, y = Y_p_5, fill = prop)) +
    geom_tile(color = "white",
              lwd = 0.5,
              linetype = 1) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90),
      legend.position = 'bottom'
    ) +
    scale_fill_gradient(low = "white", high = "steelblue") +
    labs(y = "Parent Income Rank",
         x = 'Child Income Rank',
         fill = 'Probability')
}

#### get_occ_heat_map ####
#'
#'
#'
get_occ_heat_map <- function(data) {
  data %>%
    group_by(J_c, J_p) %>%
    summarize(n = n(), .groups = 'drop') %>%
    group_by(J_p) %>%
    mutate(prop = n / sum(n)) %>%
    ungroup() %>%
    ggplot(aes(x = J_c, y = J_p, fill = prop)) +
    geom_tile(color = "white",
              lwd = 0.5,
              linetype = 1) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90),
      legend.position = 'bottom'
    ) +
    scale_fill_gradient(low = "white", high = "steelblue") +
    labs(y = "Parent Occupation",
         x = 'Child Occupation',
         fill = 'Probability')
}

#### get_mobility_plot ####
#'
#'
#'
get_mobility_plot <- function (data, inc_5 = FALSE, alt = FALSE) {
  
  if (inc_5 == FALSE) {
    
    state <- NULL
    
    if (alt == FALSE) {
      
      for(i in c('L', 'M', 'H')){
        for(j in c('U', 'L', 'M', 'H')) {
          for(k in c('H', 'C')) {
            state <- c(state, paste(i, j, k, sep = '-'))
          }
        }
      }
      
    } else if (alt == 'inc_2') {
      
      for(i in c('L', 'M', 'H')){
        for(k in c('H', 'C')) {
          for(j in c('U', 'L', 'M', 'H')) {
            state <- c(state, paste(i, j, k, sep = '-'))
          }
        }
      }
    } else if (alt == 'occ') {
      
      for(j in c('U', 'L', 'M', 'H')){
        for(i in c('L', 'M', 'H')) {
          for(k in c('H', 'C')) {
            state <- c(state, paste(i, j, k, sep = '-'))
          }
        }
      }
      
    } else if (alt == 'occ_2') {
      
      for(j in c('U', 'L', 'M', 'H')){
        for(k in c('H', 'C')) {
          for(i in c('L', 'M', 'H')) {
            state <- c(state, paste(i, j, k, sep = '-'))
          }
        }
      }
      
    } else if (alt == 'educ') {
      
      for(k in c('H', 'C')) {
        for(i in c('L', 'M', 'H')){
          for(j in c('U', 'L', 'M', 'H')) {
            
            state <- c(state, paste(i, j, k, sep = '-'))
            
          }
        }
      }
      
    } else if (alt == 'educ_2') {
      
      for(k in c('H', 'C')) {
        for(j in c('U', 'L', 'M', 'H')) {
          for(i in c('L', 'M', 'H')){
            
            state <- c(state, paste(i, j, k, sep = '-'))
            
          }
        }
      }
    }
    
    states <- data %>%
      select(Y_p, J_p, E_p) %>%
      distinct() %>%
      arrange(Y_p, J_p, E_p) %>%
      mutate(
        parent_state = paste(
          substr(as.character(Y_p), 1, 1),
          substr(as.character(J_p), 1, 1),
          substr(as.character(E_p), 1, 1),
          sep = '-'
        ),
        parent_state = factor(parent_state, levels = state)
      )
    
  } else if (inc_5 == TRUE) {
    
    state <- NULL
    
    if (alt == FALSE) {
      
      for(i in 1:5){
        for(j in c('U', 'L', 'M', 'H')) {
          for(k in c('H', 'C')) {
            state <- c(state, paste(i, j, k, sep = '-'))
          }
        }
      }
      
    } else if (alt == 'inc_2') {
      
      for(i in 1:5){
        for(k in c('H', 'C')) {
          for(j in c('U', 'L', 'M', 'H')) {
            state <- c(state, paste(i, j, k, sep = '-'))
          }
        }
      }
      
    } else if (alt == 'occ') {
      
      for(j in c('U', 'L', 'M', 'H')){
        for(i in 1:5) {
          for(k in c('H', 'C')) {
            state <- c(state, paste(i, j, k, sep = '-'))
          }
        }
      }
      
    } else if (alt == 'occ_2') {
      
      for(j in c('U', 'L', 'M', 'H')){
        for(k in c('H', 'C')) {
          for(i in 1:5) {
            state <- c(state, paste(i, j, k, sep = '-'))
          }
        }
      }
      
    } else if (alt == 'educ') {
      
      for(k in c('H', 'C')) {
        for(i in 1:5){
          for(j in c('U', 'L', 'M', 'H')) {
            state <- c(state, paste(i, j, k, sep = '-'))
          }
        }
      }
      
    } else if (alt == 'educ_2') {
      
      for(k in c('H', 'C')) {
        for(j in c('U', 'L', 'M', 'H')) {
          for(i in 1:5){
            state <- c(state, paste(i, j, k, sep = '-'))
          }
        }
      }
      
    }
    
    states <- data %>%
      select(Y_p, J_p, E_p) %>%
      distinct() %>%
      mutate(
        parent_state = paste(
          as.numeric(stri_extract_all_regex(Y_p, '\\d{2,3}')) / 20,
          substr(as.character(J_p), 1, 1),
          substr(as.character(E_p), 1, 1),
          sep = '-'
        ),
        parent_state = factor(parent_state, levels = state)
      )
  }
  
  mobility_data <- data %>%
    mutate(
      yc = as.numeric(Y_c),
      jc = as.numeric(J_c),
      ec = as.numeric(E_c),
      yp = as.numeric(Y_p),
      jp = as.numeric(J_p),
      ep = as.numeric(E_p),
      up_y = ifelse(yc > yp, 1, 0),
      up_j = ifelse(jc > jp, 1, 0),
      up_e = ifelse(ec > ep, 1, 0),
      down_y = ifelse(yc < yp, 1, 0),
      down_j = ifelse(jc < jp, 1, 0),
      down_e = ifelse(ec < ep, 1, 0),
      upward_1 = ifelse(up_y + up_j + up_e >= 1 &
                          down_y + down_j + down_e == 0, 1, 0),
      upward_2 = ifelse(up_y + up_j + up_e >= 2 &
                          down_y + down_j + down_e == 0, 1, 0),
      upward_3 = ifelse(up_y + up_j + up_e == 3 &
                          down_y + down_j + down_e == 0, 1, 0),
      downward_1 = ifelse(up_y + up_j + up_e == 0 &
                            down_y + down_j + down_e >= 1, 1, 0),
      downward_2 = ifelse(up_y + up_j + up_e == 0 &
                            down_y + down_j + down_e >= 2, 1, 0),
      downward_3 = ifelse(up_y + up_j + up_e == 0 &
                            down_y + down_j + down_e == 3, 1, 0)
    ) %>%
    group_by(Y_p, J_p, E_p) %>%
    summarize(
      up_mobility_1 = sum(trans_prob * upward_1),
      up_mobility_2 = sum(trans_prob * upward_2),
      up_mobility_3 = sum(trans_prob * upward_3),
      down_mobility_1 = sum(trans_prob * downward_1),
      down_mobility_2 = sum(trans_prob * downward_2),
      down_mobility_3 = sum(trans_prob * downward_3),
      .groups = 'drop'
    ) %>%
    pivot_longer(up_mobility_1:down_mobility_3,
                 names_to = 'mobility', values_to = 'values')
  
  mobility_data <- left_join(states, mobility_data,
                             by = c('Y_p', 'J_p', 'E_p'))
  
  upward_plot <- mobility_data %>%
    filter(stri_detect_fixed(mobility, "up")) %>%
    mutate(
      mobility = fcase(
        mobility == 'up_mobility_1', "At least 1 better",
        mobility == 'up_mobility_2', "At least 2 better",
        mobility == 'up_mobility_3', "All better"
      ),
      mobility = factor(mobility,
                        levels = c("At least 1 better", "At least 2 better",
                                   "All better"))
    ) %>%
    ggplot(mapping = aes(x = parent_state, y = values, fill = mobility)) +
    geom_col(position = 'dodge') +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90),
      legend.position = 'bottom',
      panel.grid.minor.x = element_blank()
    ) +
    labs(x = "Parent States (Income-Occupation-Education)",
         y = "Upward Mobility",
         fill = "Type of Measure") +
    scale_y_continuous(limits = c(0, 1))
  
  downward_plot <- mobility_data %>%
    filter(stri_detect_fixed(mobility, "down")) %>%
    mutate(
      mobility = fcase(
        mobility == 'down_mobility_1', "At least 1 worse",
        mobility == 'down_mobility_2', "At least 2 worse",
        mobility == 'down_mobility_3', "All worse"
      ),
      mobility = factor(mobility,
                        levels = c("At least 1 worse", "At least 2 worse",
                                   "All worse"))
    ) %>%
    ggplot(mapping = aes(x = parent_state, y = values, fill = mobility)) +
    geom_col(position = 'dodge') +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90),
      legend.position = 'bottom',
      panel.grid.minor.x = element_blank()
    ) +
    labs(x = "Parent States (Income-Occupation-Education)",
         y = "Downward Mobility",
         fill = "Type of Measure") +
    scale_y_continuous(limits = c(0, 1))
  
  result <- list(upward_plot, downward_plot)
  
  return (result)

}

#### get_measure_table ####
#'
#'
#'
get_measure_table <- function (klips_measure, psid_measure) {
  
  extract_measure <- function (list_measure, data_name) {
    
    eigen <- list_measure[[2]]
    
    worst_origin <- list_measure[[4]]
    
    dobrushin <- list_measure[[5]]
    
    mdo <- list_measure[[6]]
    
    memory <- list_measure[["Memory Curve"]]
    
    result <- tibble("Data" = data_name,
                     "Eigen" = eigen,
                     "Worst Origin" = worst_origin,
                     "Dobrushin" = dobrushin,
                     "MDO_1" = mdo$parent_mdo_m,
                     "MDO_2" = mdo$parent_mdo_e,
                     "OM" = memory[[1]][1],
                     "SM" = memory[[2]][1],
                     "EM" = memory[[3]][1])
    
    return (result)
  }
  
  klips <- extract_measure(klips_measure, "KLIPS")
  psid <- extract_measure(psid_measure, "PSID")
  
  result <- bind_rows(klips, psid)
  
  return (result)
}


#### get_memory_curve ####
#'
#'
#'
get_memory_curve <- function (measure_tab) {
  
  memory_curve <- measure_tab[["Memory Curve"]]
  
  overall <- memory_curve[[1]]
  structural <- memory_curve[[2]]
  exchange <- memory_curve[[3]]
  t <- length(overall)
  
  data <- tibble(
    time = rep(seq(1, t, by = 1), 3),
    type = c(rep('Overall', t), rep('Structural', t), rep('Exchange', t)),
    values = c(overall, structural, exchange)
  )
  
  plot <- data %>%
    ggplot(aes(x = time, y = values, color = type)) +
    geom_point() +
    geom_line(linewidth = 1) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      panel.grid.minor.x = element_blank()
    ) +
    labs(x = "Generation (t)", y = "Mobility Index", color = "Type") +
    scale_x_continuous(breaks = 1:t) +
    scale_y_continuous(limits = c(0, 1))
  
  return (plot)
}
