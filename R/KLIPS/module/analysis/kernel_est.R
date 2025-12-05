merged_final <- readRDS(here::here('data/KLIPS/merge/merged_final.rds'))

data <- merged_final %>%
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
  filter(hhid_child != hhid_parent) %>%
  filter(!is.na(educ_child) & !is.na(educ_parent)) %>%
  select(-c(pid_parent:employ_position_parent), -hhid_parent) %>%
  mutate(year_parent = as.factor(year_parent)) %>%
  distinct()

cols <- colnames(data)[stri_detect_fixed(colnames(data), 'rank')]
for (col in cols) {
  data[[col]] <- fcase(
    data[[col]] <= 30, 'low',
    data[[col]] <= 70, 'mid',
    data[[col]] <= 100, 'high'
  )
  
  data[[col]] <- factor(data[[col]], levels = c('low', 'mid', 'high'))
}

data_2 <- data %>%
  select(rank_avg_eq_income_child_w_mult,
         employ_job_5_child, employ_job_6_child, employ_job_7_child,
         employ_job_5_alt_child, employ_job_6_alt_child, employ_job_7_alt_child,
         educ_child,
         sex_child, place_curr_child,
         rank_avg_eq_income_parent_w_mult,
         employ_job_5_parent,
         employ_job_5_alt_parent,
         educ_parent
         ) %>%
  mutate_at(vars(matches('employ')), .funs = \(x) ifelse(is.na(x), 'unemp', x)) %>%
  filter(complete.cases(.))

cols <- colnames(data_2)[stri_detect_regex(colnames(data_2), 'employ_job')]
for (col in cols) {
  if (stri_detect_fixed(col, 'alt')) {
    data_2[[col]] <- factor(data_2[[col]],
                          levels = c('unemp', 'Low', 'Middle', 'High'))
  } else {
    data_2[[col]] <- factor(data_2[[col]],
                          levels = c('unemp', 'Farming', 'Low', 'Middle', 'High'))
  }
  
}
cols <- colnames(data_2)[stri_detect_regex(colnames(data_2), 'educ')]
for (col in cols) {
  
  data_2[[col]] <- factor(data_2[[col]],
                          levels = c('High school or below',
                                     'Non-STEM', 'STEM'))
  
}

data_2 %>% select(employ_job_5_child, educ_child) %>% table()
data_2 %>% select(educ_child, rank_avg_eq_income_parent_w_mult,
                  employ_job_5_parent) %>% table()
data_2 %>% select(
                  employ_job_5_parent,
                  rank_avg_eq_income_parent_w_mult,  educ_parent) %>% table()

data_2 %>% select(employ_job_5_alt_child, educ_child) %>% table()
data_2 %>% select(educ_child, rank_avg_eq_income_parent_w_mult,
                  employ_job_5_alt_parent) %>% table()
data_2 %>% select(
  employ_job_5_alt_parent,
  rank_avg_eq_income_parent_w_mult,  educ_parent) %>% table()


get_density <- function (target_var, cond_vars) {
  X <- data_2 %>% 
    select(!!!syms(cond_vars))
  
  Y <- data_2 %>%
    select(!!sym(target_var))
  
  bandwidth <- npcdensbw(ydat = Y, xdat = X)
  
  cond_dens <- npcdens(bws = bandwidth)
  
  return (cond_dens)
}

result <- get_density(target_var, cond_vars)

target_var <- 'rank_avg_eq_income_child_w_mult'
cond_vars <- c('employ_job_5_child', 'educ_child')

X <- data_2 %>% 
  select(!!!syms(cond_vars)) %>%
  as.data.frame()

Y <- data_2 %>%
  select(!!sym(target_var)) %>%
  as.data.frame()

bandwidth <- npcdensbw(ydat = Y, xdat = X)

cond_dens <- npcdens(bws = bandwidth)


kernel_aa <- function (vector, base, lambda, list) {
  m <- length(list)
  equal_base <- as.integer(vector == base)
  result <- (lambda) * equal_base + ((1 - lambda) / (m - 1)) * (1 - equal_base)
  
  return (result)
}

kernel_wv <- function (vector, base_num, rho) {
  (1 - rho) * (rho ^ abs(as.integer(vector) - as.integer(base)))
}


