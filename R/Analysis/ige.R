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
library(patchwork)
library(modelsummary)
library(stargazer)
library(texreg)

#### Comparison of IGE estimates ####
##### 0. Data preparation #####
source(here::here("R/Analysis/module/organize_functions.R"),
       local = environment())

klips_data <- readRDS(here::here('data/KLIPS/merge/merged_final_2021_16_2.rds'))
k_2016 <- readRDS(here::here('data/KLIPS/merge/merged_final_2016_16_2.rds'))
k_2017 <- readRDS(here::here('data/KLIPS/merge/merged_final_2017_16_2.rds'))
k_2018 <- readRDS(here::here('data/KLIPS/merge/merged_final_2018_16_2.rds'))
k_2019 <- readRDS(here::here('data/KLIPS/merge/merged_final_2019_16_2.rds'))
k_2020 <- readRDS(here::here('data/KLIPS/merge/merged_final_2020_16_2.rds'))

koweps_data <- readRDS('data/KOWEPS/merge/merged_rank.rds')
psid_data <- readRDS(here::here('data/PSID/clean/data_rank.rds'))


klips_data <- organize_klips(klips_data)
k_2016 <- organize_klips(k_2016)
k_2017 <- organize_klips(k_2017)
k_2018 <- organize_klips(k_2018)
k_2019 <- organize_klips(k_2019)
k_2020 <- organize_klips(k_2020)

koweps_data <- organize_koweps(koweps_data)
psid_data <- organize_psid(psid_data)

##### 1. KLIPS vs. KOWEPS vs. PSID #####
# Solon Method, OLS and WLS
model_klips_ols_dis <- lm(log_child_dis ~ log_parent_dis, data = klips_data)
model_klips_wls_dis <- lm(log_child_dis ~ log_parent_dis, weights = weight,
                          data = klips_data)
model_koweps_ols_dis <- lm(log_child_dis ~ log_parent_dis, data = koweps_data)
model_koweps_wls_dis <- lm(log_child_dis ~ log_parent_dis, weights = weight,
                           data = koweps_data)
model_koweps_ols_cur <- lm(log_child_cur ~ log_parent_cur, data = koweps_data)
model_koweps_wls_cur <- lm(log_child_cur ~ log_parent_cur, weights = weight,
                           data = koweps_data)
model_psid_ols_cur <- lm(log_child_cur ~ log_parent_cur, data = psid_data)
model_psid_wls_cur <- lm(log_child_cur ~ log_parent_cur, weights = weight,
                         data = psid_data)

# Regression table
table_ols <- list(
  "KLIPS" = model_klips_ols_dis,
  "KLIPS" = model_klips_wls_dis,
  "KOWEPS_dis" = model_koweps_ols_dis,
  "KOWEPS_dis" = model_koweps_wls_dis,
  "KOWEPS_cur" = model_koweps_ols_cur,
  "KOWEPS_cur" = model_koweps_wls_cur,
  "PSID" = model_psid_ols_cur,
  "PSID" = model_psid_wls_cur
)

ols_results <- texreg(table_ols,
       custom.model.names = c("KLIPS (OLS)", "KLIPS (WLS)",
                              "KOWEPS, post-tax (OLS)", "KOWEPS, post-tax (WLS)",
                              "KOWEPS, pre-tax (OLS)", "KOWEPS, pre-tax (WLS)",
                              "PSID (OLS)", "PSID (WLS)"),
       custom.coef.names = c("Intercept",
                             "Log Parental Income (Post-tax)",
                             "Log Parental Income (Pre-tax)")
)

print(ols_results)

summary(model_klips_ols_dis)
summary(model_koweps_ols_dis)
summary(model_koweps_ols_cur)
summary(model_psid_ols_cur)

summary(model_klips_wls_dis)
summary(model_koweps_wls_dis)
summary(model_koweps_wls_cur)
summary(model_psid_wls_cur)


##### 2. KLIPS vs. KOWEPS #####
compare_klips <- klips_data %>%
  filter(base_year_age <= 33)

compare_koweps <- koweps_data

compare_psid <- psid_data %>%
  filter(base_year_age <= 33)

# Solon Method
model_klips_comp_ols <- lm(log_child_dis ~ log_parent_dis, data = compare_klips)
model_koweps_comp_ols_dis <- lm(log_child_dis ~ log_parent_dis,
                                data = compare_koweps)
model_koweps_comp_ols_cur <- lm(log_child_cur ~ log_parent_cur,
                                data = compare_koweps)
model_psid_comp_ols <- lm(log_child_cur ~ log_parent_cur, data = compare_psid)
model_klips_comp_wls <- lm(log_child_dis ~ log_parent_dis, data = compare_klips,
                           weights = weight)
model_koweps_comp_wls_dis <- lm(log_child_dis ~ log_parent_dis,
                            data = compare_koweps, weights = weight)
model_koweps_comp_wls_cur <- lm(log_child_cur ~ log_parent_cur,
                                data = compare_koweps, weights = weight)
model_psid_comp_wls <- lm(log_child_cur ~ log_parent_cur, data = compare_psid,
                          weights = weight)

summary(model_klips_comp_ols)
summary(model_koweps_comp_ols)
summary(model_klips_comp_wls)
summary(model_koweps_comp_wls)

# Lee and Solon Method
model_klips_lee_ols <- lm(log_child_dis ~ log_parent_dis +
                               age + I(age^2) + I(age^3) +
                               age_f + I(age_f^2) + I(age_f^3),
                             data = compare_klips)
model_klips_lee_wls <- lm(log_child_dis ~ log_parent_dis +
                               age + I(age^2) + I(age^3) +
                               age_f + I(age_f^2) + I(age_f^3),
                             data = compare_klips, weights = weight)
model_koweps_lee_ols_dis <- lm(log_child_dis ~ log_parent_dis +
                                 age + I(age^2) + I(age^3) +
                                 age_f + I(age_f^2) + I(age_f^3),
                               data = compare_koweps)
model_koweps_lee_wls_dis <- lm(log_child_dis ~ log_parent_dis +
                                 age + I(age^2) + I(age^3) +
                                 age_f + I(age_f^2) + I(age_f^3),
                               data = compare_koweps, weights = weight)
model_koweps_lee_ols_cur <- lm(log_child_cur ~ log_parent_cur +
                                 age + I(age^2) + I(age^3) +
                                 age_f + I(age_f^2) + I(age_f^3),
                               data = compare_koweps)
model_koweps_lee_wls_cur <- lm(log_child_cur ~ log_parent_cur +
                                 age + I(age^2) + I(age^3) +
                                 age_f + I(age_f^2) + I(age_f^3),
                               data = compare_koweps, weights = weight)
summary(model_klips_lee_ols)
summary(model_klips_lee_wls)
summary(model_koweps_lee_ols)
summary(model_koweps_lee_wls)

# Regression table
table_compare <- list(
  "KLIPS (OLS)" = model_klips_comp_ols,
  "KLIPS (WLS)" = model_klips_comp_wls,
  "KOWEPS, post-tax (OLS)" = model_koweps_comp_ols_dis,
  "KOWEPS, post-tax (WLS)" = model_koweps_comp_wls_dis,
  "KOWEPS, pre-tax (OLS)" = model_koweps_comp_ols_cur,
  "KOWEPS, pre-tax (WLS)" = model_koweps_comp_wls_cur,
  "PSID (OLS)" = model_psid_comp_ols,
  "PSID (WLS)" = model_psid_comp_wls
)

compare <- texreg(table_compare,
                  custom.coef.names = c("Intercept",
                                        "Log Parental Income (Post-tax)",
                                        "Log Parental Income (Pre-tax)"))

##### 3. Cohort Effect #####

k_2016_c1 <- k_2016 %>% filter(cohort == 1)
k_2016_c2 <- k_2016 %>% filter(cohort == 2)
k_2016_c3 <- k_2016 %>% filter(cohort == 3)

k_2017_c1 <- k_2017 %>% filter(cohort == 1)
k_2017_c2 <- k_2017 %>% filter(cohort == 2)
k_2017_c3 <- k_2017 %>% filter(cohort == 3)

k_2018_c1 <- k_2018 %>% filter(cohort == 1)
k_2018_c2 <- k_2018 %>% filter(cohort == 2)
k_2018_c3 <- k_2018 %>% filter(cohort == 3)

k_2019_c1 <- k_2019 %>% filter(cohort == 1)
k_2019_c2 <- k_2019 %>% filter(cohort == 2)
k_2019_c3 <- k_2019 %>% filter(cohort == 3)

k_2020_c1 <- k_2020 %>% filter(cohort == 1)
k_2020_c2 <- k_2020 %>% filter(cohort == 2)
k_2020_c3 <- k_2020 %>% filter(cohort == 3)

klips_data_c1 <- klips_data %>% filter(cohort == 1)
klips_data_c2 <- klips_data %>% filter(cohort == 2)
klips_data_c3 <- klips_data %>% filter(cohort == 3)

model_2016_cohort_1_ols <- lm(log_child_dis ~ log_parent_dis,
                              data = k_2016_c1)
model_2017_cohort_1_ols <- lm(log_child_dis ~ log_parent_dis,
                              data = k_2017_c1)
model_2018_cohort_1_ols <- lm(log_child_dis ~ log_parent_dis,
                              data = k_2018_c1)
model_2019_cohort_1_ols <- lm(log_child_dis ~ log_parent_dis,
                              data = k_2019_c1)
model_2020_cohort_1_ols <- lm(log_child_dis ~ log_parent_dis,
                              data = k_2020_c1)
model_2021_cohort_1_ols <- lm(log_child_dis ~ log_parent_dis,
                              data = klips_data_c1)

model_2016_cohort_2_ols <- lm(log_child_dis ~ log_parent_dis,
                              data = k_2016_c2)
model_2017_cohort_2_ols <- lm(log_child_dis ~ log_parent_dis,
                              data = k_2017_c2)
model_2018_cohort_2_ols <- lm(log_child_dis ~ log_parent_dis,
                              data = k_2018_c2)
model_2019_cohort_2_ols <- lm(log_child_dis ~ log_parent_dis,
                              data = k_2019_c2)
model_2020_cohort_2_ols <- lm(log_child_dis ~ log_parent_dis,
                              data = k_2020_c2)
model_2021_cohort_2_ols <- lm(log_child_dis ~ log_parent_dis,
                              data = klips_data_c2)

model_2021_cohort_3_ols <- lm(log_child_dis ~ log_parent_dis,
                              data = klips_data_c3)

model_psid_cohort_1 <- lm(log_child_cur ~ log_parent_cur,
                          data = psid_data, subset = (cohort == 1))
model_psid_cohort_2 <- lm(log_child_cur ~ log_parent_cur,
                          data = psid_data, subset = (cohort == 2))
model_psid_cohort_3 <- lm(log_child_cur ~ log_parent_cur,
                          data = psid_data, subset = (cohort == 3))

model_koweps_cohort_2_dis <- lm(log_child_dis ~ log_parent_dis,
                                data = koweps_data, subset = (base_year_age >= 30))
model_koweps_cohort_3_dis <- lm(log_child_dis ~ log_parent_dis,
                                data = koweps_data, subset = (base_year_age < 30))
model_koweps_cohort_2_cur <- lm(log_child_cur ~ log_parent_cur,
                            data = koweps_data, subset = (base_year_age >= 30))
model_koweps_cohort_3_cur <- lm(log_child_cur ~ log_parent_cur,
                            data = koweps_data, subset = (base_year_age < 30))

model_cohorts <- list(
  "25-29" = model_2021_cohort_3_ols,
  "30-35" = model_2021_cohort_2_ols,
  "36-40" = model_2021_cohort_1_ols,
  "25-29" = model_koweps_cohort_2_dis,
  "30-33" = model_koweps_cohort_3_dis,
  "25-29" = model_koweps_cohort_2_cur,
  "30-33" = model_koweps_cohort_3_cur,
  "25-29" = model_psid_cohort_3,
  "30-35" = model_psid_cohort_2,
  "36-40" = model_psid_cohort_1
)

model_cohort_1 <- list(
  "2016" = model_2016_cohort_1_ols,
  "2017" = model_2017_cohort_1_ols,
  "2018" = model_2018_cohort_1_ols,
  "2019" = model_2019_cohort_1_ols,
  "2020" = model_2020_cohort_1_ols,
  "2021" = model_2021_cohort_1_ols
)
model_cohort_2 <- list(
  "2016" = model_2016_cohort_2_ols,
  "2017" = model_2017_cohort_2_ols,
  "2018" = model_2018_cohort_2_ols,
  "2019" = model_2019_cohort_2_ols,
  "2020" = model_2020_cohort_2_ols,
  "2021" = model_2021_cohort_2_ols
)
texreg(model_cohorts)
texreg(model_cohort_1)
texreg(model_cohort_2)
