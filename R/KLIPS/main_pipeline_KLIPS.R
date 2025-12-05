library(dplyr)
library(tidyr)
library(readr)
library(tibble)
library(rlang)
library(purrr)
library(stringi)
library(data.table)
library(here)
library(assertthat)
library(haven)
library(quantreg)
library(np)
library(ggplot2)

env_list <- list(
  clean = new.env(),
  merge = new.env()
)

source(here::here('R/KLIPS/data_clean.R'), local = env_list$clean)
for (level in c('household', 'individual', 'supplementary')) {
  env_list$clean$clean_klips_data(file_path = 'data/KLIPS/raw/',
                                  dir_path = 'data/KLIPS/clean',
                                  start_wave = 26, end_wave = 1,
                                  level)
}

source(here::here('R/KLIPS/data_merge_2.R'), local = env_list$merge)
env_list$merge$merge_data(start_wave = 1, end_wave = 26,
                          child_base_age = 16, age_bandwidth = 2,
                          child_min_age = 25, child_max_age = 40,
                          parent_min_age = 35, parent_max_age = 55,
                          child_base_years = 2016:2023, IMF_filter = FALSE,
                          cols_household, cols_individual,
                          cols_income_h, cols_incom_p, cols_income_t,
                          cols_info,
                          dir_path = 'data/KLIPS/merge')


start_wave = 1
end_wave = 26
child_base_age = 16
age_bandwidth = 2
child_min_age = 25
child_max_age = 40
parent_min_age = 35
parent_max_age = 55
child_base_year = 2021
IMF_filter = FALSE
dir_path = 'data/KLIPS/merge'
cols_household <- c('year', 'hhid', 'num_fam',
                    'income_2',
                    'eq_income_2',
                    'log_income_2',
                    'log_eq_income_2',
                    'w_h', 'sw_h', 'nw_h')
cols_individual <- c('year', 'pid', 'hhid',
                     'fam_rel', 'fam_rel_code',
                     'sex', 'birth_year', 'age',
                     'educ_self',
                     'place_curr',
                     'employ_stat', 'employ_type', 'employ_position',
                     'employ_regular',
                     'employ_job_5', 'employ_job_6', 'employ_job_7',
                     'employ_job_5_alt', 'employ_job_6_alt', 'employ_job_7_alt',
                     'w_p_c', 'sw_p_c', 'nw_p_c')
cols_income_h <- c('income_2', 'log_income_2')
cols_income_p <- c('eq_income_2', 'log_eq_income_2')
cols_income_t <- c(cols_income_h, cols_income_p)
cols_info <- c('sex', 'birth_year', 'age', 'educ_self',
               'place_birth', 'place_14', 
               'educ_major',
               'place_curr', 'employ_stat',
               'employ_type', 'employ_regular', 'employ_position',
               'employ_job_5', 'employ_job_6', 'employ_job_7',
               'employ_job_5_alt', 'employ_job_6_alt', 'employ_job_7_alt')
