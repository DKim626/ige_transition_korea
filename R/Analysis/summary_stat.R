library(xtable)

source(here::here("R/Analysis/module/organize_functions.R"),
       local = environment())

summarize_data <- function(digit = 4) {
  
  klips_data <- readRDS(here::here('data/KLIPS/merge/merged_final_2021_16_2.rds'))
  koweps_data <- readRDS('data/KOWEPS/merge/merged_rank.rds')
  psid_data <- readRDS(here::here('data/PSID/clean/data_rank.rds'))
  
  klips_data <- organize_klips(klips_data)
  koweps_data <- organize_koweps(koweps_data)
  psid_data <- organize_psid(psid_data)
  
  data_list <- organize_vars(klips_data, koweps_data, psid_data)
  klips_data <- data_list[[1]]
  koweps_data <- data_list[[2]]
  psid_data <- data_list[[3]]
  
  get_summary_table <- function (data, digit) {
    
    get_mean_sd <- function(var, digit) {
      mean <- round(mean(var), digit)
      sd <- round(sd(var), digit)
      
      result <- paste0(mean, " (", sd, ")")
      
      return (result)
    }
    
    inc <- data %>%
      select(any_of(c('log_child_cur', 'log_child_dis',
                      'log_parent_cur', 'log_parent_dis'))) %>%
      summarize(across(everything(), ~ get_mean_sd(.x, digit))) %>%
      pivot_longer(any_of(c('log_child_cur', 'log_child_dis',
                            'log_parent_cur', 'log_parent_dis')),
                   values_to = "value", names_to = 'var') %>%
      mutate(
        var = fcase(
          var == 'log_child_cur', "Log children pre-tax income",
          var == 'log_child_dis', "Log children post-tax income",
          var == 'log_parent_cur', "Log parent pre-tax income",
          var == 'log_parent_dis', "Log parent post-tax income"
        )
      )
    
    educ <- data %>%
      group_by(educ) %>%
      summarize(n = n()) %>%
      mutate(value = n / sum(n) * 100) %>%
      select(educ, value) %>%
      mutate(
        var = 'Child Education Level',
        value = as.character(round(value, digit))
        ) %>%
      rename(category = educ)
    
    educ_parent <- data %>%
      group_by(educ_parent) %>%
      summarize(n = n()) %>%
      mutate(value = n / sum(n) * 100) %>%
      select(educ_parent, value) %>%
      mutate(
        var = 'Parent Education Level',
        value = as.character(round(value, digit))
        ) %>%
      rename(category = educ_parent)
    
    occ <- data %>%
      group_by(occ) %>%
      summarize(n = n()) %>%
      mutate(value = n / sum(n) * 100) %>%
      select(occ, value) %>%
      mutate(
        var = 'Child Occupation',
        value = as.character(round(value, digit))
        ) %>%
      rename(category = occ)
    
    occ_parent <- data %>%
      group_by(occ_parent) %>%
      summarize(n = n()) %>%
      mutate(value = n / sum(n) * 100) %>%
      select(occ_parent, value) %>%
      mutate(
        var = 'Parent Occupation',
        value = as.character(round(value, digit))
        ) %>%
      rename(category = occ_parent)
    
    result <- bind_rows(inc, educ, educ_parent,
                        occ, occ_parent) %>%
      select(var, category, value)
    
    return (result)
    
  }
  
  klips_summary <- get_summary_table(klips_data, digit) %>%
    rename(KLIPS = value)
  koweps_summary <- get_summary_table(koweps_data, digit) %>%
    rename(KOWEPS = value)
  psid_summary <- get_summary_table(psid_data, digit) %>%
    rename(PSID = value)
  
  base <- koweps_summary %>% select(var, category)
  
  result <- left_join(base, klips_summary, by = c('var', 'category')) %>%
    left_join(koweps_summary, by = c('var', 'category')) %>%
    left_join(psid_summary, by = c('var', 'category')) %>%
    rename(
      'Variable' = 'var',
      "Category" = 'category'
    )
  
  return (result)
}

summary_table <- summarize_data(digit = 2)
summary_table_latex <- xtable(summary_table)
print(summary_table_latex, booktabs = TRUE, include.rownames = FALSE)
