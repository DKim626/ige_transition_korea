organize_klips <- function (data) {
  
  data %>%
    filter(log_eq_income_2_CPI_curr > 0 & log_eq_income_2_CPI_past > 0 &
             hhid_past != hhid_curr) %>%
    rename(
      log_child_dis = log_eq_income_2_CPI_curr,
      log_parent_dis = log_eq_income_2_CPI_past,
      weight = nw_p_c_curr
    )
}

organize_koweps <- function (data) {
  data %>%
    filter(log_eq_inc_dis_CPI_curr > 0 & log_eq_inc_dis_CPI_past > 0) %>%
    filter(hh_key_past != hh_key_curr & age <= 33) %>%
    rename(
      log_child_dis = log_eq_inc_dis_CPI_curr,
      log_parent_dis = log_eq_inc_dis_CPI_past,
      log_child_cur = log_eq_inc_cur_CPI_curr,
      log_parent_cur = log_eq_inc_cur_CPI_past,
      weight = weight_general_ind_c_2_curr
    )
}

organize_psid <- function (data) {
  data %>%
    filter(log_eq_inc_total_cpi_curr > 0 & log_eq_inc_total_cpi_past > 0) %>%
    rename(
      log_child_cur = log_eq_inc_total_cpi_curr,
      log_parent_cur = log_eq_inc_total_cpi_past,
      weight = wt_ind_l_curr
    )
}

organize_vars <- function(klips_data, koweps_data, psid_data) {
  
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
  
  klips_data <- klips_data %>%
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
           log_child_dis,
           employ_job_7_alt,
           educ_2,
           log_parent_dis,
           employ_job_5_alt_parent,
           educ_2_parent,
           weight
    ) %>%
    distinct() %>%
    mutate_at(vars(matches('employ')),
              .funs = \(x) ifelse(is.na(x), 'Unemp', x)) %>%
    rename(
      occ = employ_job_7_alt,
      educ = educ_2,
      occ_parent = employ_job_5_alt_parent,
      educ_parent = educ_2_parent
    ) %>%
    mutate(
      occ = factor_occ(occ),
      occ_parent = factor_occ(occ_parent)
    )
  
  koweps_data <- koweps_data %>%
    filter(hh_key_past != hh_key_curr) %>%
    mutate(
      educ = merge_educ(educ, educ_major),
      educ_f = merge_educ(educ_f, educ_major_f),
      educ_m = merge_educ(educ_m, educ_major_m),
      educ_parent = merge_parent(educ_f, educ_m),
      educ_2 = merge_educ_2(educ),
      educ_2_parent = merge_educ_2(educ_parent),
      occ_parent = merge_parent(occ_f, occ_m)
    ) %>%
    filter(!is.na(educ_2) & !is.na(educ_2_parent)) %>%
    select(pid,
           log_child_dis,
           log_child_cur,
           occ,
           educ_2,
           log_parent_dis,
           log_parent_cur,
           occ_parent,
           educ_2_parent,
           weight
    ) %>%
    distinct() %>%
    mutate_at(vars(matches('occ')),
              .funs = \(x) ifelse(is.na(x), 'Unemp', x)) %>%
    rename(
      educ = educ_2,
      educ_parent = educ_2_parent
    ) %>%
    mutate(
      occ = factor_occ(occ),
      occ_parent = factor_occ(occ_parent)
    )
  
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
           log_child_cur,
           occ,
           educ_level_2,
           log_parent_cur,
           occ_parent,
           educ_level_2_parent,
           weight
    ) %>%
    distinct() %>%
    mutate_at(vars(matches('occ')),
              .funs = \(x) ifelse(is.na(x), 'Unemp', x)) %>%
    rename(
      educ = educ_level_2,
      educ_parent = educ_level_2_parent
    ) %>%
    mutate(
      occ = factor_occ(occ),
      occ_parent = factor_occ(occ_parent)
    )
  
  data_list <- list(klips_data, koweps_data, psid_data)
  
  return (data_list)
}