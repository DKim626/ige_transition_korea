library(modelsummary)
library(stargazer)
library(texreg)

merged_final_2016 <- readRDS(here::here('data/KLIPS/merge/merged_final_2016_16_2.rds'))
merged_final_2017 <- readRDS(here::here('data/KLIPS/merge/merged_final_2017_16_2.rds'))
merged_final_2018 <- readRDS(here::here('data/KLIPS/merge/merged_final_2018_16_2.rds'))
merged_final_2019 <- readRDS(here::here('data/KLIPS/merge/merged_final_2019_16_2.rds'))
merged_final_2020 <- readRDS(here::here('data/KLIPS/merge/merged_final_2020_16_2.rds'))
merged_final_2021 <- readRDS(here::here('data/KLIPS/merge/merged_final_2021_16_2.rds'))
merged_final_2022 <- readRDS(here::here('data/KLIPS/merge/merged_final_2022_16_2.rds'))
merged_final_2023 <- readRDS(here::here('data/KLIPS/merge/merged_final_2023_16_2.rds'))

merged_final_2021_no_imf <- readRDS(here::here('data/KLIPS/merge/merged_final_2021_16_2_no_imf.rds'))

winsor <- function (inc, percent) {
  
  low_q <- quantile(inc, percent)
  high_q <- quantile(inc, 1 - percent)
  
  inc <- fcase(
    inc <= low_q, low_q,
    inc >= high_q, high_q,
    default = inc
  )
  
  return (inc)
}

winsor_per <- 0.02

data_2016 <- merged_final_2016 %>%
  filter(hhid_curr != hhid_past) %>%
  mutate(cohort = as.factor(cohort)) %>%
  mutate(
    log_eq_income_2_CPI_curr = winsor(log_eq_income_2_CPI_curr, winsor_per),
    log_eq_income_2_CPI_past = winsor(log_eq_income_2_CPI_past, winsor_per),
  )
data_2017 <- merged_final_2017 %>%
  filter(hhid_curr != hhid_past) %>%
  mutate(cohort = as.factor(cohort)) %>%
  mutate(
    log_eq_income_2_CPI_curr = winsor(log_eq_income_2_CPI_curr, winsor_per),
    log_eq_income_2_CPI_past = winsor(log_eq_income_2_CPI_past, winsor_per),
  )
data_2018 <- merged_final_2018 %>%
  filter(hhid_curr != hhid_past) %>%
  mutate(cohort = as.factor(cohort)) %>%
  mutate(
    log_eq_income_2_CPI_curr = winsor(log_eq_income_2_CPI_curr, winsor_per),
    log_eq_income_2_CPI_past = winsor(log_eq_income_2_CPI_past, winsor_per),
  )
data_2019 <- merged_final_2019 %>%
  filter(hhid_curr != hhid_past) %>%
  mutate(cohort = as.factor(cohort)) %>%
  mutate(
    log_eq_income_2_CPI_curr = winsor(log_eq_income_2_CPI_curr, winsor_per),
    log_eq_income_2_CPI_past = winsor(log_eq_income_2_CPI_past, winsor_per),
  )
data_2020 <- merged_final_2020 %>%
  filter(hhid_curr != hhid_past) %>%
  mutate(cohort = as.factor(cohort)) %>%
  mutate(
    log_eq_income_2_CPI_curr = winsor(log_eq_income_2_CPI_curr, winsor_per),
    log_eq_income_2_CPI_past = winsor(log_eq_income_2_CPI_past, winsor_per),
  )
data_2021 <- merged_final_2021 %>%
  filter(hhid_curr != hhid_past) %>%
  mutate(cohort = as.factor(cohort)) %>%
  mutate(
    log_eq_income_2_CPI_curr = winsor(log_eq_income_2_CPI_curr, winsor_per),
    log_eq_income_2_CPI_past = winsor(log_eq_income_2_CPI_past, winsor_per),
  )
data_2022 <- merged_final_2022 %>%
  filter(hhid_curr != hhid_past) %>%
  mutate(cohort = as.factor(cohort)) %>%
  mutate(
    log_eq_income_2_CPI_curr = winsor(log_eq_income_2_CPI_curr, winsor_per),
    log_eq_income_2_CPI_past = winsor(log_eq_income_2_CPI_past, winsor_per),
  )
data_2023 <- merged_final_2023 %>%
  filter(hhid_curr != hhid_past) %>%
  mutate(cohort = as.factor(cohort)) %>%
  mutate(
    log_eq_income_2_CPI_curr = winsor(log_eq_income_2_CPI_curr, winsor_per),
    log_eq_income_2_CPI_past = winsor(log_eq_income_2_CPI_past, winsor_per),
  )

data_2021_no <- merged_final_2021_no_imf %>%
  filter(hhid_curr != hhid_past) %>%
  mutate(cohort = as.factor(cohort))

#### Household Income ####
data_2016_b <- data_2016 %>%
  filter(log_eq_income_2_CPI_curr > 0 & log_eq_income_2_CPI_past > 0)
data_2017_b <- data_2017 %>%
  filter(log_eq_income_2_CPI_curr > 0 & log_eq_income_2_CPI_past > 0)
data_2018_b <- data_2018 %>%
  filter(log_eq_income_2_CPI_curr > 0 & log_eq_income_2_CPI_past > 0)
data_2019_b <- data_2019 %>%
  filter(log_eq_income_2_CPI_curr > 0 & log_eq_income_2_CPI_past > 0)
data_2020_b <- data_2020 %>%
  filter(log_eq_income_2_CPI_curr > 0 & log_eq_income_2_CPI_past > 0)
data_2021_b <- data_2021 %>%
  filter(log_eq_income_2_CPI_curr > 0 & log_eq_income_2_CPI_past > 0)
data_2022_b <- data_2022 %>%
  filter(log_eq_income_2_CPI_curr > 0 & log_eq_income_2_CPI_past > 0)
data_2023_b <- data_2023 %>%
  filter(log_eq_income_2_CPI_curr > 0 & log_eq_income_2_CPI_past > 0)

data_2021_no_b <- data_2021_no %>%
  filter(log_eq_income_2_CPI_curr > 0 & log_eq_income_2_CPI_past > 0)

data_2016_t <- data_2016 %>%
  filter(log_eq_income_2_CPI_curr > 0 & log_eq_income_2_CPI_past > 0) %>%
  filter(base_year_age >= 25 & base_year_age <= 35)
data_2017_t <- data_2017 %>%
  filter(log_eq_income_2_CPI_curr > 0 & log_eq_income_2_CPI_past > 0) %>%
  filter(base_year_age >= 26 & base_year_age <= 36)
data_2018_t <- data_2018 %>%
  filter(log_eq_income_2_CPI_curr > 0 & log_eq_income_2_CPI_past > 0) %>%
  filter(base_year_age >= 27 & base_year_age <= 37)
data_2019_t <- data_2019 %>%
  filter(log_eq_income_2_CPI_curr > 0 & log_eq_income_2_CPI_past > 0) %>%
  filter(base_year_age >= 28 & base_year_age <= 38)
data_2020_t <- data_2020 %>%
  filter(log_eq_income_2_CPI_curr > 0 & log_eq_income_2_CPI_past > 0) %>%
  filter(base_year_age >= 29 & base_year_age <= 39)
data_2021_t <- data_2021 %>%
  filter(log_eq_income_2_CPI_curr > 0 & log_eq_income_2_CPI_past > 0) %>%
  filter(base_year_age >= 30 & base_year_age <= 40)
data_2022_t <- data_2022 %>%
  filter(log_eq_income_2_CPI_curr > 0 & log_eq_income_2_CPI_past > 0) %>%
  filter(base_year_age >= 31 & base_year_age <= 41)
data_2023_t <- data_2023 %>%
  filter(log_eq_income_2_CPI_curr > 0 & log_eq_income_2_CPI_past > 0) %>%
  filter(base_year_age >= 32 & base_year_age <= 42)

data_2021_no_t <- data_2021_no %>%
  filter(log_eq_income_2_CPI_curr > 0 & log_eq_income_2_CPI_past > 0) %>%
  filter(base_year_age >= 30 & base_year_age <= 40)

data_2016_1 <- data_2016 %>%
  filter(log_eq_income_2_CPI_curr > 0 & log_eq_income_2_CPI_past > 0) %>%
  filter(base_year_age >= 25 & base_year_age <= 30)
data_2017_1 <- data_2017 %>%
  filter(log_eq_income_2_CPI_curr > 0 & log_eq_income_2_CPI_past > 0) %>%
  filter(base_year_age >= 26 & base_year_age <= 31)
data_2018_1 <- data_2018 %>%
  filter(log_eq_income_2_CPI_curr > 0 & log_eq_income_2_CPI_past > 0) %>%
  filter(base_year_age >= 27 & base_year_age <= 32)
data_2019_1 <- data_2019 %>%
  filter(log_eq_income_2_CPI_curr > 0 & log_eq_income_2_CPI_past > 0) %>%
  filter(base_year_age >= 28 & base_year_age <= 33)
data_2020_1 <- data_2020 %>%
  filter(log_eq_income_2_CPI_curr > 0 & log_eq_income_2_CPI_past > 0) %>%
  filter(base_year_age >= 29 & base_year_age <= 34)
data_2021_1 <- data_2021 %>%
  filter(log_eq_income_2_CPI_curr > 0 & log_eq_income_2_CPI_past > 0) %>%
  filter(base_year_age >= 30 & base_year_age <= 35)
data_2022_1 <- data_2022 %>%
  filter(log_eq_income_2_CPI_curr > 0 & log_eq_income_2_CPI_past > 0) %>%
  filter(base_year_age >= 31 & base_year_age <= 36)
data_2023_1 <- data_2023 %>%
  filter(log_eq_income_2_CPI_curr > 0 & log_eq_income_2_CPI_past > 0) %>%
  filter(base_year_age >= 32 & base_year_age <= 37)

data_2021_no_1 <- data_2021_no %>%
  filter(log_eq_income_2_CPI_curr > 0 & log_eq_income_2_CPI_past > 0) %>%
  filter(base_year_age >= 30 & base_year_age <= 35)

data_2016_2 <- data_2016 %>%
  filter(log_eq_income_2_CPI_curr > 0 & log_eq_income_2_CPI_past > 0) %>%
  filter(base_year_age >= 31 & base_year_age <= 35)
data_2017_2 <- data_2017 %>%
  filter(log_eq_income_2_CPI_curr > 0 & log_eq_income_2_CPI_past > 0) %>%
  filter(base_year_age >= 32 & base_year_age <= 36)
data_2018_2 <- data_2018 %>%
  filter(log_eq_income_2_CPI_curr > 0 & log_eq_income_2_CPI_past > 0) %>%
  filter(base_year_age >= 33 & base_year_age <= 37)
data_2019_2 <- data_2019 %>%
  filter(log_eq_income_2_CPI_curr > 0 & log_eq_income_2_CPI_past > 0) %>%
  filter(base_year_age >= 34 & base_year_age <= 38)
data_2020_2 <- data_2020 %>%
  filter(log_eq_income_2_CPI_curr > 0 & log_eq_income_2_CPI_past > 0) %>%
  filter(base_year_age >= 35 & base_year_age <= 39)
data_2021_2 <- data_2021 %>%
  filter(log_eq_income_2_CPI_curr > 0 & log_eq_income_2_CPI_past > 0) %>%
  filter(base_year_age >= 36 & base_year_age <= 40)
data_2022_2 <- data_2022 %>%
  filter(log_eq_income_2_CPI_curr > 0 & log_eq_income_2_CPI_past > 0) %>%
  filter(base_year_age >= 37 & base_year_age <= 41)
data_2023_2 <- data_2023 %>%
  filter(log_eq_income_2_CPI_curr > 0 & log_eq_income_2_CPI_past > 0) %>%
  filter(base_year_age >= 38 & base_year_age <= 42)

data_2021_no_2 <- data_2021_no %>%
  filter(log_eq_income_2_CPI_curr > 0 & log_eq_income_2_CPI_past > 0) %>%
  filter(base_year_age >= 36 & base_year_age <= 40)

data_2016_3 <- data_2016 %>%
  filter(log_eq_income_2_CPI_curr > 0 & log_eq_income_2_CPI_past > 0) %>%
  filter(base_year_age >= 25 & base_year_age <= 32)
data_2017_3 <- data_2017 %>%
  filter(log_eq_income_2_CPI_curr > 0 & log_eq_income_2_CPI_past > 0) %>%
  filter(base_year_age >= 26 & base_year_age <= 33)
data_2018_3 <- data_2018 %>%
  filter(log_eq_income_2_CPI_curr > 0 & log_eq_income_2_CPI_past > 0) %>%
  filter(base_year_age >= 27 & base_year_age <= 34)
data_2019_3 <- data_2019 %>%
  filter(log_eq_income_2_CPI_curr > 0 & log_eq_income_2_CPI_past > 0) %>%
  filter(base_year_age >= 28 & base_year_age <= 35)
data_2020_3 <- data_2020 %>%
  filter(log_eq_income_2_CPI_curr > 0 & log_eq_income_2_CPI_past > 0) %>%
  filter(base_year_age >= 29 & base_year_age <= 36)
data_2021_3 <- data_2021 %>%
  filter(log_eq_income_2_CPI_curr > 0 & log_eq_income_2_CPI_past > 0) %>%
  filter(base_year_age >= 30 & base_year_age <= 37)
data_2022_3 <- data_2022 %>%
  filter(log_eq_income_2_CPI_curr > 0 & log_eq_income_2_CPI_past > 0) %>%
  filter(base_year_age >= 31 & base_year_age <= 38)
data_2023_3 <- data_2023 %>%
  filter(log_eq_income_2_CPI_curr > 0 & log_eq_income_2_CPI_past > 0) %>%
  filter(base_year_age >= 32 & base_year_age <= 39)

data_2021_no_3 <- data_2021_no %>%
  filter(log_eq_income_2_CPI_curr > 0 & log_eq_income_2_CPI_past > 0) %>%
  filter(base_year_age >= 30 & base_year_age <= 37)

data_2021_koweps <- data_2021 %>%
  filter(log_eq_income_2_CPI_curr > 0 & log_eq_income_2_CPI_past > 0) %>%
  filter(base_year_age <= 31)

model_2016 <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
                   weights = nw_p_c_curr, data = data_2016)
model_2017 <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
                   weights = nw_p_c_curr, data = data_2017)
model_2018 <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
                   weights = nw_p_c_curr, data = data_2018)
model_2019 <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
                   weights = nw_p_c_curr, data = data_2019)
model_2020 <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
                   weights = nw_p_c_curr, data = data_2020)
model_2021 <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
                   weights = nw_p_c_curr, data = data_2021)
model_2022 <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
                 weights = nw_p_c_curr, data = data_2022)
model_2023 <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
                 weights = nw_p_c_curr, data = data_2023)

model_2021_no <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
                    weights = nw_p_c_curr, data = data_2021_no)

model_2016_b <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
                   weights = nw_p_c_curr, data = data_2016_b)
model_2017_b <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
                   weights = nw_p_c_curr, data = data_2017_b)
model_2018_b <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
                   weights = nw_p_c_curr, data = data_2018_b)
model_2019_b <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
                   weights = nw_p_c_curr, data = data_2019_b)
model_2020_b <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
                   weights = nw_p_c_curr, data = data_2020_b)
model_2021_b <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
                   weights = nw_p_c_curr, data = data_2021_b)
model_2022_b <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
                   weights = nw_p_c_curr, data = data_2022_b)
model_2023_b <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
                   weights = nw_p_c_curr, data = data_2023_b)

model_2021_no_b <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
                      weights = nw_p_c_curr, data = data_2021_no_b)

model_2016_t <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
                   weights = nw_p_c_curr, data = data_2016_t)
model_2017_t <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
                   weights = nw_p_c_curr, data = data_2017_t)
model_2018_t <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
                   weights = nw_p_c_curr, data = data_2018_t)
model_2019_t <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
                   weights = nw_p_c_curr, data = data_2019_t)
model_2020_t <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
                   weights = nw_p_c_curr, data = data_2020_t)
model_2021_t <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
                   weights = nw_p_c_curr, data = data_2021_t)
model_2022_t <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
                   weights = nw_p_c_curr, data = data_2022_t)
model_2023_t <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
                   weights = nw_p_c_curr, data = data_2023_t)

model_2021_no_t <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
                      weights = nw_p_c_curr, data = data_2021_no_t)

model_2016_1 <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
                   weights = nw_p_c_curr, data = data_2016_1)
model_2017_1 <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
                   weights = nw_p_c_curr, data = data_2017_1)
model_2018_1 <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
                   weights = nw_p_c_curr, data = data_2018_1)
model_2019_1 <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
                   weights = nw_p_c_curr, data = data_2019_1)
model_2020_1 <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
                   weights = nw_p_c_curr, data = data_2020_1)
model_2021_1 <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
                   weights = nw_p_c_curr, data = data_2021_1)
model_2022_1 <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
                   weights = nw_p_c_curr, data = data_2022_1)
model_2023_1 <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
                   weights = nw_p_c_curr, data = data_2023_1)

model_2021_no_1 <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
                      weights = nw_p_c_curr, data = data_2021_no_1)

model_2016_2 <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
                   weights = nw_p_c_curr, data = data_2016_2)
model_2017_2 <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
                   weights = nw_p_c_curr, data = data_2017_2)
model_2018_2 <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
                   weights = nw_p_c_curr, data = data_2018_2)
model_2019_2 <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
                   weights = nw_p_c_curr, data = data_2019_2)
model_2020_2 <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
                   weights = nw_p_c_curr, data = data_2020_2)
model_2021_2 <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
                   weights = nw_p_c_curr, data = data_2021_2)
model_2022_2 <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
                   weights = nw_p_c_curr, data = data_2022_2)
model_2023_2 <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
                   weights = nw_p_c_curr, data = data_2023_2)

model_2021_no_2 <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
                      weights = nw_p_c_curr, data = data_2021_no_2)

model_2016_3 <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
                   weights = nw_p_c_curr, data = data_2016_3)
model_2017_3 <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
                   weights = nw_p_c_curr, data = data_2017_3)
model_2018_3 <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
                   weights = nw_p_c_curr, data = data_2018_3)
model_2019_3 <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
                   weights = nw_p_c_curr, data = data_2019_3)
model_2020_3 <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
                   weights = nw_p_c_curr, data = data_2020_3)
model_2021_3 <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
                   weights = nw_p_c_curr, data = data_2021_3)
model_2022_3 <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
                   weights = nw_p_c_curr, data = data_2022_3)
model_2023_3 <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
                   weights = nw_p_c_curr, data = data_2023_3)

model_2021_no_3 <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
                      weights = nw_p_c_curr, data = data_2021_no_3)

model_2021_koweps <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
                   weights = nw_p_c_curr, data = data_2021_koweps)

summary(model_2016)
summary(model_2017)
summary(model_2018)
summary(model_2019)
summary(model_2020)
summary(model_2021)
summary(model_2022)
summary(model_2023)

summary(model_2021_no)

summary(model_2016_b)
summary(model_2017_b)
summary(model_2018_b)
summary(model_2019_b)
summary(model_2020_b)
summary(model_2021_b)
summary(model_2022_b)
summary(model_2023_b)

summary(model_2021_no_b)

models <- list(
  "2016" = model_2016_b,
  "2017" = model_2017_b,
  "2018" = model_2018_b,
  "2019" = model_2019_b,
  "2020" = model_2020_b,
  "2021" = model_2021_b,
  "2022" = model_2022_b,
  "2023" = model_2023_b
)
texreg(models)


summary(model_2016_t)
summary(model_2017_t)
summary(model_2018_t)
summary(model_2019_t)
summary(model_2020_t)
summary(model_2021_t)
summary(model_2022_t)
summary(model_2023_t)

summary(model_2021_no_t)

models <- list(
  "2016" = model_2016_t,
  "2017" = model_2017_t,
  "2018" = model_2018_t,
  "2019" = model_2019_t,
  "2020" = model_2020_t,
  "2021" = model_2021_t,
  "2022" = model_2022_t,
  "2023" = model_2023_t
)
texreg(models)

summary(model_2016_1)
summary(model_2017_1)
summary(model_2018_1)
summary(model_2019_1)
summary(model_2020_1)
summary(model_2021_1)
summary(model_2022_1)
summary(model_2023_1)

models <- list(
  "2016" = model_2016_1,
  "2017" = model_2017_1,
  "2018" = model_2018_1,
  "2019" = model_2019_1,
  "2020" = model_2020_1,
  "2021" = model_2021_1,
  "2022" = model_2022_1,
  "2023" = model_2023_1
)
texreg(models)

summary(model_2021_no_1)

summary(model_2016_2)
summary(model_2017_2)
summary(model_2018_2)
summary(model_2019_2)
summary(model_2020_2)
summary(model_2021_2)
summary(model_2022_2)
summary(model_2023_2)

models <- list(
  "2016" = model_2016_2,
  "2017" = model_2017_2,
  "2018" = model_2018_2,
  "2019" = model_2019_2,
  "2020" = model_2020_2,
  "2021" = model_2021_2,
  "2022" = model_2022_2,
  "2023" = model_2023_2
)
texreg(models)

summary(model_2021_no_2)

summary(model_2016_3)
summary(model_2017_3)
summary(model_2018_3)
summary(model_2019_3)
summary(model_2020_3)
summary(model_2021_3)
summary(model_2022_3)
summary(model_2023_3)

models <- list(
  "2016" = model_2016_3,
  "2017" = model_2017_3,
  "2018" = model_2018_3,
  "2019" = model_2019_3,
  "2020" = model_2020_3,
  "2021" = model_2021_3,
  "2022" = model_2022_3,
  "2023" = model_2023_3
)
texreg(models)

summary(model_2021_no_3)

summary(model_2021_koweps)

data_2021_2529 <- data_2021_b %>%
  filter(base_year_age >= 25 & base_year_age <= 29)
data_2021_3035 <- data_2021_b %>%
  filter(base_year_age >= 30 & base_year_age <= 35)
data_2021_3640 <- data_2021_b %>%
  filter(base_year_age >= 36 & base_year_age <= 40)
data_2021_3040 <- data_2021_b %>%
  filter(base_year_age >= 30 & base_year_age <= 40)
model_1 <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
              weights = nw_p_c_curr, data = data_2021_b)
model_2 <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
              weights = nw_p_c_curr, data = data_2021_3040)
model_3 <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
              weights = nw_p_c_curr, data = data_2021_2529)
model_4 <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
              weights = nw_p_c_curr, data = data_2021_3035)
model_5 <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
              weights = nw_p_c_curr, data = data_2021_3640)
models <- list(
  "Overall" = model_1,
  "30-40" = model_2,
  "25-29" = model_3,
  "30-35" = model_4,
  "36-40" = model_5
)
texreg(models)

data_2021_b %>%
  ggplot(aes(x = log_eq_income_2_CPI_past, y = log_eq_income_2_CPI_curr, color = base_year_age)) +
  geom_point() +
  geom_abline(slope = 1) +
  theme_bw() +
  windows(15, 15)

data_2021_koweps %>%
  mutate(base_year_age = factor(base_year_age)) %>%
  ggplot(aes(x = log_eq_income_2_CPI_past, y = log_eq_income_2_CPI_curr, color = base_year_age)) +
  geom_point() +
  geom_abline(slope = 1) +
  theme_bw() +
  windows(15, 15)

#### Household Income ####
data_2016 <- merged_final_2016 %>%
  filter(hhid_curr != hhid_past) %>%
  filter(sex == 'Male') %>%
  mutate(cohort = as.factor(cohort)) %>%
  filter(income_labor_CPI_curr > 0 & income_labor_CPI_past > 0 &
           quantile(income_labor_CPI_curr, 0.01) <= income_labor_CPI_curr &
           quantile(income_labor_CPI_curr, 0.99) >= income_labor_CPI_curr &
           quantile(income_labor_CPI_past, 0.01) <= income_labor_CPI_past &
           quantile(income_labor_CPI_past, 0.99) >= income_labor_CPI_past) %>%
  mutate(log_income_labor_CPI_past = log(income_labor_CPI_past),
         log_income_labor_CPI_curr = log(income_labor_CPI_curr))
data_2017 <- merged_final_2017 %>%
  filter(hhid_curr != hhid_past) %>%
  filter(sex == 'Male') %>%
  mutate(cohort = as.factor(cohort)) %>%
  filter(income_labor_CPI_curr > 0 & income_labor_CPI_past > 0 &
           quantile(income_labor_CPI_curr, 0.01) <= income_labor_CPI_curr &
           quantile(income_labor_CPI_curr, 0.99) >= income_labor_CPI_curr &
           quantile(income_labor_CPI_past, 0.01) <= income_labor_CPI_past &
           quantile(income_labor_CPI_past, 0.99) >= income_labor_CPI_past) %>%
  mutate(log_income_labor_CPI_past = log(income_labor_CPI_past),
         log_income_labor_CPI_curr = log(income_labor_CPI_curr))
data_2018 <- merged_final_2018 %>%
  filter(hhid_curr != hhid_past) %>%
  filter(sex == 'Male') %>%
  mutate(cohort = as.factor(cohort)) %>%
  filter(income_labor_CPI_curr > 0 & income_labor_CPI_past > 0 &
           quantile(income_labor_CPI_curr, 0.01) <= income_labor_CPI_curr &
           quantile(income_labor_CPI_curr, 0.99) >= income_labor_CPI_curr &
           quantile(income_labor_CPI_past, 0.01) <= income_labor_CPI_past &
           quantile(income_labor_CPI_past, 0.99) >= income_labor_CPI_past) %>%
  mutate(log_income_labor_CPI_past = log(income_labor_CPI_past),
         log_income_labor_CPI_curr = log(income_labor_CPI_curr))
data_2019 <- merged_final_2019 %>%
  filter(hhid_curr != hhid_past) %>%
  filter(sex == 'Male') %>%
  mutate(cohort = as.factor(cohort)) %>%
  filter(income_labor_CPI_curr > 0 & income_labor_CPI_past > 0 &
           quantile(income_labor_CPI_curr, 0.01) <= income_labor_CPI_curr &
           quantile(income_labor_CPI_curr, 0.99) >= income_labor_CPI_curr &
           quantile(income_labor_CPI_past, 0.01) <= income_labor_CPI_past &
           quantile(income_labor_CPI_past, 0.99) >= income_labor_CPI_past) %>%
  mutate(log_income_labor_CPI_past = log(income_labor_CPI_past),
         log_income_labor_CPI_curr = log(income_labor_CPI_curr))
data_2020 <- merged_final_2020 %>%
  filter(hhid_curr != hhid_past) %>%
  filter(sex == 'Male') %>%
  mutate(cohort = as.factor(cohort)) %>%
  filter(income_labor_CPI_curr > 0 & income_labor_CPI_past > 0 &
           quantile(income_labor_CPI_curr, 0.01) <= income_labor_CPI_curr &
           quantile(income_labor_CPI_curr, 0.99) >= income_labor_CPI_curr &
           quantile(income_labor_CPI_past, 0.01) <= income_labor_CPI_past &
           quantile(income_labor_CPI_past, 0.99) >= income_labor_CPI_past) %>%
  mutate(log_income_labor_CPI_past = log(income_labor_CPI_past),
         log_income_labor_CPI_curr = log(income_labor_CPI_curr))
data_2021 <- merged_final_2021 %>%
  filter(hhid_curr != hhid_past) %>%
  filter(sex == 'Male') %>%
  mutate(cohort = as.factor(cohort)) %>%
  filter(income_labor_CPI_curr > 0 & income_labor_CPI_past > 0 &
           quantile(income_labor_CPI_curr, 0.01) <= income_labor_CPI_curr &
           quantile(income_labor_CPI_curr, 0.99) >= income_labor_CPI_curr &
           quantile(income_labor_CPI_past, 0.01) <= income_labor_CPI_past &
           quantile(income_labor_CPI_past, 0.99) >= income_labor_CPI_past) %>%
  mutate(log_income_labor_CPI_past = log(income_labor_CPI_past),
         log_income_labor_CPI_curr = log(income_labor_CPI_curr))
data_2022 <- merged_final_2022 %>%
  filter(hhid_curr != hhid_past) %>%
  filter(sex == 'Male') %>%
  mutate(cohort = as.factor(cohort)) %>%
  filter(income_labor_CPI_curr > 0 & income_labor_CPI_past > 0 &
           quantile(income_labor_CPI_curr, 0.01) <= income_labor_CPI_curr &
           quantile(income_labor_CPI_curr, 0.99) >= income_labor_CPI_curr &
           quantile(income_labor_CPI_past, 0.01) <= income_labor_CPI_past &
           quantile(income_labor_CPI_past, 0.99) >= income_labor_CPI_past) %>%
  mutate(log_income_labor_CPI_past = log(income_labor_CPI_past),
         log_income_labor_CPI_curr = log(income_labor_CPI_curr))
data_2023 <- merged_final_2023 %>%
  filter(hhid_curr != hhid_past) %>%
  filter(sex == 'Male') %>%
  mutate(cohort = as.factor(cohort)) %>%
  filter(income_labor_CPI_curr > 0 & income_labor_CPI_past > 0 &
           quantile(income_labor_CPI_curr, 0.01) <= income_labor_CPI_curr &
           quantile(income_labor_CPI_curr, 0.99) >= income_labor_CPI_curr &
           quantile(income_labor_CPI_past, 0.01) <= income_labor_CPI_past &
           quantile(income_labor_CPI_past, 0.99) >= income_labor_CPI_past) %>%
  mutate(log_income_labor_CPI_past = log(income_labor_CPI_past),
         log_income_labor_CPI_curr = log(income_labor_CPI_curr))


data_2016_b <- data_2016 %>%
  filter(log_income_labor_CPI_curr > 0 & log_income_labor_CPI_past > 0)
data_2017_b <- data_2017 %>%
  filter(log_income_labor_CPI_curr > 0 & log_income_labor_CPI_past > 0)
data_2018_b <- data_2018 %>%
  filter(log_income_labor_CPI_curr > 0 & log_income_labor_CPI_past > 0)
data_2019_b <- data_2019 %>%
  filter(log_income_labor_CPI_curr > 0 & log_income_labor_CPI_past > 0)
data_2020_b <- data_2020 %>%
  filter(log_income_labor_CPI_curr > 0 & log_income_labor_CPI_past > 0)
data_2021_b <- data_2021 %>%
  filter(log_income_labor_CPI_curr > 0 & log_income_labor_CPI_past > 0)
data_2022_b <- data_2022 %>%
  filter(log_income_labor_CPI_curr > 0 & log_income_labor_CPI_past > 0)
data_2023_b <- data_2023 %>%
  filter(log_income_labor_CPI_curr > 0 & log_income_labor_CPI_past > 0)

data_2016_w <- data_2016 %>%
  filter(log_income_labor_CPI_curr > 0 & log_income_labor_CPI_past > 0 &
           quantile(log_income_labor_CPI_curr, 0.01) <= log_income_labor_CPI_curr &
           quantile(log_income_labor_CPI_curr, 0.99) >= log_income_labor_CPI_curr &
           quantile(log_income_labor_CPI_past, 0.01) <= log_income_labor_CPI_past &
           quantile(log_income_labor_CPI_past, 0.99) >= log_income_labor_CPI_past)
data_2017_w <- data_2017 %>%
  filter(log_income_labor_CPI_curr > 0 & log_income_labor_CPI_past > 0 &
           quantile(log_income_labor_CPI_curr, 0.01) <= log_income_labor_CPI_curr &
           quantile(log_income_labor_CPI_curr, 0.99) >= log_income_labor_CPI_curr &
           quantile(log_income_labor_CPI_past, 0.01) <= log_income_labor_CPI_past &
           quantile(log_income_labor_CPI_past, 0.99) >= log_income_labor_CPI_past)
data_2018_w <- data_2018 %>%
  filter(log_income_labor_CPI_curr > 0 & log_income_labor_CPI_past > 0 &
           quantile(log_income_labor_CPI_curr, 0.01) <= log_income_labor_CPI_curr &
           quantile(log_income_labor_CPI_curr, 0.99) >= log_income_labor_CPI_curr &
           quantile(log_income_labor_CPI_past, 0.01) <= log_income_labor_CPI_past &
           quantile(log_income_labor_CPI_past, 0.99) >= log_income_labor_CPI_past)
data_2019_w <- data_2019 %>%
  filter(log_income_labor_CPI_curr > 0 & log_income_labor_CPI_past > 0 &
           quantile(log_income_labor_CPI_curr, 0.01) <= log_income_labor_CPI_curr &
           quantile(log_income_labor_CPI_curr, 0.99) >= log_income_labor_CPI_curr &
           quantile(log_income_labor_CPI_past, 0.01) <= log_income_labor_CPI_past &
           quantile(log_income_labor_CPI_past, 0.99) >= log_income_labor_CPI_past)
data_2020_w <- data_2020 %>%
  filter(log_income_labor_CPI_curr > 0 & log_income_labor_CPI_past > 0 &
           quantile(log_income_labor_CPI_curr, 0.01) <= log_income_labor_CPI_curr &
           quantile(log_income_labor_CPI_curr, 0.99) >= log_income_labor_CPI_curr &
           quantile(log_income_labor_CPI_past, 0.01) <= log_income_labor_CPI_past &
           quantile(log_income_labor_CPI_past, 0.99) >= log_income_labor_CPI_past)
data_2021_w <- data_2021 %>%
  filter(log_income_labor_CPI_curr > 0 & log_income_labor_CPI_past > 0 &
           quantile(log_income_labor_CPI_curr, 0.01) <= log_income_labor_CPI_curr &
           quantile(log_income_labor_CPI_curr, 0.99) >= log_income_labor_CPI_curr &
           quantile(log_income_labor_CPI_past, 0.01) <= log_income_labor_CPI_past &
           quantile(log_income_labor_CPI_past, 0.99) >= log_income_labor_CPI_past)
data_2022_w <- data_2022 %>%
  filter(log_income_labor_CPI_curr > 0 & log_income_labor_CPI_past > 0 &
           quantile(log_income_labor_CPI_curr, 0.01) <= log_income_labor_CPI_curr &
           quantile(log_income_labor_CPI_curr, 0.99) >= log_income_labor_CPI_curr &
           quantile(log_income_labor_CPI_past, 0.01) <= log_income_labor_CPI_past &
           quantile(log_income_labor_CPI_past, 0.99) >= log_income_labor_CPI_past)
data_2023_w <- data_2023 %>%
  filter(log_income_labor_CPI_curr > 0 & log_income_labor_CPI_past > 0 &
           quantile(log_income_labor_CPI_curr, 0.01) <= log_income_labor_CPI_curr &
           quantile(log_income_labor_CPI_curr, 0.99) >= log_income_labor_CPI_curr &
           quantile(log_income_labor_CPI_past, 0.01) <= log_income_labor_CPI_past &
           quantile(log_income_labor_CPI_past, 0.99) >= log_income_labor_CPI_past)

data_2016_t <- data_2016 %>%
  filter(log_income_labor_CPI_curr > 0 & log_income_labor_CPI_past > 0) %>%
  filter(base_year_age >= 25 & base_year_age <= 35)
data_2017_t <- data_2017 %>%
  filter(log_income_labor_CPI_curr > 0 & log_income_labor_CPI_past > 0) %>%
  filter(base_year_age >= 26 & base_year_age <= 36)
data_2018_t <- data_2018 %>%
  filter(log_income_labor_CPI_curr > 0 & log_income_labor_CPI_past > 0) %>%
  filter(base_year_age >= 27 & base_year_age <= 37)
data_2019_t <- data_2019 %>%
  filter(log_income_labor_CPI_curr > 0 & log_income_labor_CPI_past > 0) %>%
  filter(base_year_age >= 28 & base_year_age <= 38)
data_2020_t <- data_2020 %>%
  filter(log_income_labor_CPI_curr > 0 & log_income_labor_CPI_past > 0) %>%
  filter(base_year_age >= 29 & base_year_age <= 39)
data_2021_t <- data_2021 %>%
  filter(log_income_labor_CPI_curr > 0 & log_income_labor_CPI_past > 0) %>%
  filter(base_year_age >= 30 & base_year_age <= 40)
data_2022_t <- data_2022 %>%
  filter(log_income_labor_CPI_curr > 0 & log_income_labor_CPI_past > 0) %>%
  filter(base_year_age >= 31 & base_year_age <= 41)
data_2023_t <- data_2023 %>%
  filter(log_income_labor_CPI_curr > 0 & log_income_labor_CPI_past > 0) %>%
  filter(base_year_age >= 32 & base_year_age <= 42)

data_2016_1 <- data_2016 %>%
  filter(log_income_labor_CPI_curr > 0 & log_income_labor_CPI_past > 0) %>%
  filter(base_year_age >= 25 & base_year_age <= 30)
data_2017_1 <- data_2017 %>%
  filter(log_income_labor_CPI_curr > 0 & log_income_labor_CPI_past > 0) %>%
  filter(base_year_age >= 26 & base_year_age <= 31)
data_2018_1 <- data_2018 %>%
  filter(log_income_labor_CPI_curr > 0 & log_income_labor_CPI_past > 0) %>%
  filter(base_year_age >= 27 & base_year_age <= 32)
data_2019_1 <- data_2019 %>%
  filter(log_income_labor_CPI_curr > 0 & log_income_labor_CPI_past > 0) %>%
  filter(base_year_age >= 28 & base_year_age <= 33)
data_2020_1 <- data_2020 %>%
  filter(log_income_labor_CPI_curr > 0 & log_income_labor_CPI_past > 0) %>%
  filter(base_year_age >= 29 & base_year_age <= 34)
data_2021_1 <- data_2021 %>%
  filter(log_income_labor_CPI_curr > 0 & log_income_labor_CPI_past > 0) %>%
  filter(base_year_age >= 30 & base_year_age <= 35)
data_2022_1 <- data_2022 %>%
  filter(log_income_labor_CPI_curr > 0 & log_income_labor_CPI_past > 0) %>%
  filter(base_year_age >= 31 & base_year_age <= 36)
data_2023_1 <- data_2023 %>%
  filter(log_income_labor_CPI_curr > 0 & log_income_labor_CPI_past > 0) %>%
  filter(base_year_age >= 32 & base_year_age <= 37)

data_2016_2 <- data_2016 %>%
  filter(log_income_labor_CPI_curr > 0 & log_income_labor_CPI_past > 0) %>%
  filter(base_year_age >= 31 & base_year_age <= 35)
data_2017_2 <- data_2017 %>%
  filter(log_income_labor_CPI_curr > 0 & log_income_labor_CPI_past > 0) %>%
  filter(base_year_age >= 32 & base_year_age <= 36)
data_2018_2 <- data_2018 %>%
  filter(log_income_labor_CPI_curr > 0 & log_income_labor_CPI_past > 0) %>%
  filter(base_year_age >= 33 & base_year_age <= 37)
data_2019_2 <- data_2019 %>%
  filter(log_income_labor_CPI_curr > 0 & log_income_labor_CPI_past > 0) %>%
  filter(base_year_age >= 34 & base_year_age <= 38)
data_2020_2 <- data_2020 %>%
  filter(log_income_labor_CPI_curr > 0 & log_income_labor_CPI_past > 0) %>%
  filter(base_year_age >= 35 & base_year_age <= 39)
data_2021_2 <- data_2021 %>%
  filter(log_income_labor_CPI_curr > 0 & log_income_labor_CPI_past > 0) %>%
  filter(base_year_age >= 36 & base_year_age <= 40)
data_2022_2 <- data_2022 %>%
  filter(log_income_labor_CPI_curr > 0 & log_income_labor_CPI_past > 0) %>%
  filter(base_year_age >= 37 & base_year_age <= 41)
data_2023_2 <- data_2023 %>%
  filter(log_income_labor_CPI_curr > 0 & log_income_labor_CPI_past > 0) %>%
  filter(base_year_age >= 38 & base_year_age <= 42)

data_2016_3 <- data_2016 %>%
  filter(log_income_labor_CPI_curr > 0 & log_income_labor_CPI_past > 0) %>%
  filter(base_year_age >= 25 & base_year_age <= 32)
data_2017_3 <- data_2017 %>%
  filter(log_income_labor_CPI_curr > 0 & log_income_labor_CPI_past > 0) %>%
  filter(base_year_age >= 26 & base_year_age <= 33)
data_2018_3 <- data_2018 %>%
  filter(log_income_labor_CPI_curr > 0 & log_income_labor_CPI_past > 0) %>%
  filter(base_year_age >= 27 & base_year_age <= 34)
data_2019_3 <- data_2019 %>%
  filter(log_income_labor_CPI_curr > 0 & log_income_labor_CPI_past > 0) %>%
  filter(base_year_age >= 28 & base_year_age <= 35)
data_2020_3 <- data_2020 %>%
  filter(log_income_labor_CPI_curr > 0 & log_income_labor_CPI_past > 0) %>%
  filter(base_year_age >= 29 & base_year_age <= 36)
data_2021_3 <- data_2021 %>%
  filter(log_income_labor_CPI_curr > 0 & log_income_labor_CPI_past > 0) %>%
  filter(base_year_age >= 30 & base_year_age <= 37)
data_2022_3 <- data_2022 %>%
  filter(log_income_labor_CPI_curr > 0 & log_income_labor_CPI_past > 0) %>%
  filter(base_year_age >= 31 & base_year_age <= 38)
data_2023_3 <- data_2023 %>%
  filter(log_income_labor_CPI_curr > 0 & log_income_labor_CPI_past > 0) %>%
  filter(base_year_age >= 32 & base_year_age <= 39)

model_2016 <- lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past,
                 weights = nw_p_c_curr, data = data_2016)
model_2017 <- lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past,
                 weights = nw_p_c_curr, data = data_2017)
model_2018 <- lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past,
                 weights = nw_p_c_curr, data = data_2018)
model_2019 <- lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past,
                 weights = nw_p_c_curr, data = data_2019)
model_2020 <- lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past,
                 weights = nw_p_c_curr, data = data_2020)
model_2021 <- lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past,
                 weights = nw_p_c_curr, data = data_2021)
model_2022 <- lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past,
                 weights = nw_p_c_curr, data = data_2022)
model_2023 <- lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past,
                 weights = nw_p_c_curr, data = data_2023)

model_2016_b <- lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past,
                   weights = nw_p_c_curr, data = data_2016_b)
model_2017_b <- lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past,
                   weights = nw_p_c_curr, data = data_2017_b)
model_2018_b <- lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past,
                   weights = nw_p_c_curr, data = data_2018_b)
model_2019_b <- lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past,
                   weights = nw_p_c_curr, data = data_2019_b)
model_2020_b <- lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past,
                   weights = nw_p_c_curr, data = data_2020_b)
model_2021_b <- lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past,
                   weights = nw_p_c_curr, data = data_2021_b)
model_2022_b <- lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past,
                   weights = nw_p_c_curr, data = data_2022_b)
model_2023_b <- lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past,
                   weights = nw_p_c_curr, data = data_2023_b)

model_2016_w <- lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past,
                   weights = nw_p_c_curr, data = data_2016_w)
model_2017_w <- lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past,
                   weights = nw_p_c_curr, data = data_2017_w)
model_2018_w <- lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past,
                   weights = nw_p_c_curr, data = data_2018_w)
model_2019_w <- lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past,
                   weights = nw_p_c_curr, data = data_2019_w)
model_2020_w <- lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past,
                   weights = nw_p_c_curr, data = data_2020_w)
model_2021_w <- lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past,
                   weights = nw_p_c_curr, data = data_2021_w)
model_2022_w <- lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past,
                   weights = nw_p_c_curr, data = data_2022_w)
model_2023_w <- lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past,
                   weights = nw_p_c_curr, data = data_2023_w)

model_2016_t <- lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past,
                   weights = nw_p_c_curr, data = data_2016_t)
model_2017_t <- lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past,
                   weights = nw_p_c_curr, data = data_2017_t)
model_2018_t <- lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past,
                   weights = nw_p_c_curr, data = data_2018_t)
model_2019_t <- lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past,
                   weights = nw_p_c_curr, data = data_2019_t)
model_2020_t <- lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past,
                   weights = nw_p_c_curr, data = data_2020_t)
model_2021_t <- lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past,
                   weights = nw_p_c_curr, data = data_2021_t)
model_2022_t <- lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past,
                   weights = nw_p_c_curr, data = data_2022_t)
model_2023_t <- lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past,
                   weights = nw_p_c_curr, data = data_2023_t)

model_2016_1 <- lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past,
                   weights = nw_p_c_curr, data = data_2016_1)
model_2017_1 <- lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past,
                   weights = nw_p_c_curr, data = data_2017_1)
model_2018_1 <- lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past,
                   weights = nw_p_c_curr, data = data_2018_1)
model_2019_1 <- lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past,
                   weights = nw_p_c_curr, data = data_2019_1)
model_2020_1 <- lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past,
                   weights = nw_p_c_curr, data = data_2020_1)
model_2021_1 <- lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past,
                   weights = nw_p_c_curr, data = data_2021_1)
model_2022_1 <- lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past,
                   weights = nw_p_c_curr, data = data_2022_1)
model_2023_1 <- lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past,
                   weights = nw_p_c_curr, data = data_2023_1)

model_2016_2 <- lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past,
                   weights = nw_p_c_curr, data = data_2016_2)
model_2017_2 <- lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past,
                   weights = nw_p_c_curr, data = data_2017_2)
model_2018_2 <- lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past,
                   weights = nw_p_c_curr, data = data_2018_2)
model_2019_2 <- lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past,
                   weights = nw_p_c_curr, data = data_2019_2)
model_2020_2 <- lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past,
                   weights = nw_p_c_curr, data = data_2020_2)
model_2021_2 <- lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past,
                   weights = nw_p_c_curr, data = data_2021_2)
model_2022_2 <- lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past,
                   weights = nw_p_c_curr, data = data_2022_2)
model_2023_2 <- lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past,
                   weights = nw_p_c_curr, data = data_2023_2)

model_2016_3 <- lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past,
                   weights = nw_p_c_curr, data = data_2016_3)
model_2017_3 <- lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past,
                   weights = nw_p_c_curr, data = data_2017_3)
model_2018_3 <- lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past,
                   weights = nw_p_c_curr, data = data_2018_3)
model_2019_3 <- lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past,
                   weights = nw_p_c_curr, data = data_2019_3)
model_2020_3 <- lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past,
                   weights = nw_p_c_curr, data = data_2020_3)
model_2021_3 <- lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past,
                   weights = nw_p_c_curr, data = data_2021_3)
model_2022_3 <- lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past,
                   weights = nw_p_c_curr, data = data_2022_3)
model_2023_3 <- lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past,
                   weights = nw_p_c_curr, data = data_2023_3)

summary(model_2016)
summary(model_2017)
summary(model_2018)
summary(model_2019)
summary(model_2020)
summary(model_2021)
summary(model_2022)
summary(model_2023)

summary(model_2016_b)
summary(model_2017_b)
summary(model_2018_b)
summary(model_2019_b)
summary(model_2020_b)
summary(model_2021_b)
summary(model_2022_b)
summary(model_2023_b)

summary(model_2016_w)
summary(model_2017_w)
summary(model_2018_w)
summary(model_2019_w)
summary(model_2020_w)
summary(model_2021_w)
summary(model_2022_w)
summary(model_2023_w)

summary(model_2016_t)
summary(model_2017_t)
summary(model_2018_t)
summary(model_2019_t)
summary(model_2020_t)
summary(model_2021_t)
summary(model_2022_t)
summary(model_2023_t)

summary(model_2016_1)
summary(model_2017_1)
summary(model_2018_1)
summary(model_2019_1)
summary(model_2020_1)
summary(model_2021_1)
summary(model_2022_1)
summary(model_2023_1)

summary(model_2016_2)
summary(model_2017_2)
summary(model_2018_2)
summary(model_2019_2)
summary(model_2020_2)
summary(model_2021_2)
summary(model_2022_2)
summary(model_2023_2)

summary(model_2016_3)
summary(model_2017_3)
summary(model_2018_3)
summary(model_2019_3)
summary(model_2020_3)
summary(model_2021_3)
summary(model_2022_3)
summary(model_2023_3)

#### Past codes ####

temp <- merged_final_2021 %>%
  filter(hhid_curr != hhid_past) %>%
  mutate(cohort = as.factor(cohort)) %>%
  filter(base_year_age <= 33)

data_2021_t %>%
  mutate(base_year_age = factor(base_year_age)) %>%
  ggplot(aes(x = eq_income_2_CPI_past, y = eq_income_2_CPI_curr, color = base_year_age)) +
  geom_point() +
  geom_abline(slope = 1) +
  theme_bw() +
  windows(15, 15)

temp <- data_2021 %>%
  filter(base_year_age <= 33)

model <- lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
            weights = nw_p_c_curr, data = temp)
summary(model)


data_a <- merged_final_2016 %>%
  filter(hhid_curr != hhid_past) %>%
  mutate(cohort = as.factor(cohort))
data_a <- merged_final_2021 %>%
  filter(hhid_curr != hhid_past) %>%
  mutate(cohort = as.factor(cohort))

merged_final %>% select(year_child, age_child, cohort) %>% table()
merged_final %>% select(year_parent, age_parent, cohort) %>% table()


data_b <- data_a %>%
  filter(income_2_CPI_curr > 0 & income_2_CPI_past > 0)

data_b <- data_a %>%
  filter(income_2_CPI_curr > 0 & income_2_CPI_past > 0) %>%
  filter(quantile(log_eq_income_2_CPI_curr, 0.05) <= log_eq_income_2_CPI_curr &
           quantile(log_eq_income_2_CPI_curr, 0.95) >= log_eq_income_2_CPI_curr &
           quantile(log_eq_income_2_CPI_past, 0.05) <= log_eq_income_2_CPI_past &
           quantile(log_eq_income_2_CPI_past, 0.95) >= log_eq_income_2_CPI_past &
           base_year_age >= 30)

data_b <- data_a %>%
  filter(income_CPI_curr > 0 & income_CPI_past > 0 &
           base_year_age <= 33)

data_b <- data_a %>%
  filter(income_2_CPI_curr > 0 & income_2_CPI_past > 0 & base_year_age >= 30)

data_b <- data_a %>%
  filter(income_CPI_curr > 0 & income_CPI_past > 0) %>%
  filter(base_year_age >= 31 & base_year_age <= 35)
data_b <- data_a %>%
  filter(income_CPI_curr > 0 & income_CPI_past > 0) %>%
  filter(base_year_age >= 36 & base_year_age <= 40)

data_b <- data_a %>%
  filter(income_CPI_curr > 0 & income_CPI_past > 0) %>%
  filter(base_year_age >= 25 & base_year_age <= 30)
data_b <- data_a %>%
  filter(income_CPI_curr > 0 & income_CPI_past > 0) %>%
  filter(base_year_age >= 30 & base_year_age <= 35)

model <- with(data_b, lm(income_CPI_curr ~ income_CPI_past))
summary(model)

model <- with(data_b, lm(eq_income_CPI_curr ~ eq_income_CPI_past))
summary(model)

model <- with(data_b, lm(log_income_CPI_curr ~ log_income_CPI_past))
summary(model)
model <- with(data_b, lm(log_income_CPI_curr ~ log_income_CPI_past,
                         weights = w_p_c_curr))
summary(model)
model <- with(data_b, lm(log_eq_income_CPI_curr ~ log_eq_income_CPI_past,
                         weights = w_p_c_curr))
summary(model)

model <- with(data_b, lm(log_income_CPI_curr ~ log_income_CPI_past,
                         weights = nw_p_c_curr))
summary(model)
model <- with(data_b, lm(log_eq_income_CPI_curr ~ log_eq_income_CPI_past,
                         weights = nw_p_c_curr))
summary(model)

model <- with(data_b, lm(log_income_2_CPI_curr ~ log_income_2_CPI_past,
                         weights = nw_p_c_curr))
summary(model)
model <- with(data_b, lm(log_eq_income_2_CPI_curr ~ log_eq_income_2_CPI_past,
                         weights = nw_p_c_curr))
summary(model)

model <- with(data_b, lm(log_eq_income_CPI_curr ~ log_eq_income_CPI_past +
                           base_year_age + I(base_year_age^2) +
                           age_f + I(age_f^2),
                         weights = nw_p_c_curr))
summary(model)

model <- with(data_b, lm(log_eq_income_CPI_curr ~ log_eq_income_CPI_past))
summary(model)
model <- with(data_b, lm(log_eq_income_GDP_curr ~ log_eq_income_GDP_past))
summary(model)
model <- with(data_b, lm(log_eq_income_curr ~ log_eq_income_past))
summary(model)

model <- with(data_b, lm(log_eq_income_CPI_curr ~ log_eq_income_CPI_past + age + I(age^2) +
                           age_f + I(age_f^2) + age:log_eq_income_CPI_past))
summary(model)
model <- with(data_b, lm(log_income_CPI_curr ~ log_income_CPI_past + age + I(age^2) +
                           age_f + I(age_f^2) + age:log_eq_income_CPI_past))
summary(model)

taus <- c(0.1, 0.25, 0.5, 0.75, 0.9)
model <- with(data_b, rq(income_CPI_curr ~ income_CPI_past, tau = taus))
summary(model)

taus <- c(0.1, 0.25, 0.5, 0.75, 0.9)
model <- with(data_b, rq(log_income_CPI_curr ~ log_income_CPI_past, tau = taus))
summary(model)

taus <- c(0.1, 0.25, 0.5, 0.75, 0.9)
model <- with(data_b, rq(eq_income_CPI_curr ~ eq_income_CPI_past, tau = taus))
summary(model)

taus <- c(0.1, 0.25, 0.5, 0.75, 0.9)
model <- with(data_b, rq(log_eq_income_CPI_curr ~ log_eq_income_CPI_past, tau = taus))
summary(model)

ggplot(data_b, aes(x = log_income_CPI_past, y = log_income_CPI_curr)) +
  geom_point() +
  stat_smooth() +
  theme_minimal() +
  windows(15, 15)

ggplot(data_b, aes(x = log_eq_income_CPI_past, y = log_eq_income_CPI_curr)) +
  geom_point(aes(color = cohort)) +
  stat_smooth() +
  theme_minimal() +
  windows(15, 15)

ggplot(data_b, aes(x = eq_income_CPI_past, y = eq_income_CPI_curr)) +
  geom_point(aes(color = cohort)) +
  stat_smooth() +
  theme_minimal() +
  windows(15, 15)

ggplot(data_b, aes(x = log_income_past, y = log_income_curr)) +
  geom_point(aes(color = cohort)) +
  stat_smooth() +
  theme_minimal() +
  windows(15, 15)

ggplot(data_b, aes(x = log_eq_income_past, y = log_eq_income_curr)) +
  geom_point(aes(color = cohort)) +
  stat_smooth() +
  theme_minimal() +
  windows(15, 15)


merged_final %>%
  filter(hhid_child != hhid_parent) %>%
  filter(avg_income_labor_CPI_child > 0 &
           avg_income_labor_CPI_parent > 0) %>%
  select(age_parent, age_child, cohort) %>% 
  table()

data_c <- data_b %>%
  filter(cohort == 2)

model <- with(data_c, lm(income_CPI_curr ~ income_CPI_past))
summary(model)

model <- with(data_c, lm(eq_income_CPI_curr ~ eq_income_CPI_past))
summary(model)

model <- with(data_c, lm(log_income_CPI_curr ~ log_income_CPI_past))
summary(model)

model <- with(data_c, lm(log_eq_income_CPI_curr ~ log_eq_income_CPI_past))
summary(model)

model <- with(data_c, lm(log_income_labor_CPI_curr ~ log_income_labor_CPI_past))
summary(model)

model <- with(data_c, lm(log_eq_income_labor_CPI_curr ~ log_eq_income_labor_CPI_past))
summary(model)


taus <- c(0.1, 0.25, 0.5, 0.75, 0.9)
model <- with(data_c, rq(avg_eq_income_CPI_child ~ avg_eq_income_CPI_parent, tau = taus))
summary(model, se = 'boot')
model <- with(data_c, npreg(avg_eq_income_CPI_child ~ avg_eq_income_CPI_parent))
windows(15, 10); plot(model, errors = TRUE, boot.num = 500)


ggplot(data_c, aes(x = avg_eq_income_CPI_parent, y = avg_eq_income_CPI_child)) +
  geom_point(aes(color = year_parent)) +
  stat_smooth() +
  theme_minimal() +
  windows(15, 15)


data_b <- data_a %>%
  filter(avg_last_income_labor_post_CPI_child > 0 &
           avg_last_income_labor_post_CPI_parent > 0)
model <- with(data_b,
              lm(log_avg_last_income_labor_post_CPI_child ~
                   log_avg_last_income_labor_post_CPI_parent))
summary(model)

data_b <- data_b %>%
  mutate(across(c(matches('income')), ~ log(.x)))

data_c <- data_b %>%
  filter(cohort == 1)

model <- with(data_c,
              lm(log_avg_last_income_labor_post_CPI_child ~
                   log_avg_last_income_labor_post_CPI_parent))
summary(model)

model <- with(data_c,
              lm(avg_last_income_labor_pre_CPI_child ~
                   avg_last_income_labor_pre_CPI_parent))
summary(model)

taus <- c(0.1, 0.25, 0.5, 0.75, 0.9)
model <- with(data_c,
              rq(avg_last_income_labor_post_CPI_child ~
                   avg_last_income_labor_post_CPI_parent, tau = taus))
summary(model, se = 'boot')
model <- with(data_c,
              npreg(avg_last_income_labor_post_CPI_child ~
                      avg_last_income_labor_post_CPI_parent))
windows(15, 10); plot(model, errors = TRUE, boot.num = 500)


ggplot(data_c, aes(x = avg_last_income_labor_post_CPI_parent,
                   y = avg_last_income_labor_post_CPI_child)) +
  geom_point(aes(color = year_parent)) +
  stat_smooth() +
  theme_minimal() +
  windows(15, 15)


data_a <- merged_final %>%
  filter(hhid_child != hhid_parent) %>%
  select(-c(pid_parent:employ_position_parent), -hhid_parent) %>%
  mutate(year_parent = as.factor(year_parent)) %>%
  distinct()

data_b <- data_a %>%
  filter(avg_income_CPI_child > 0 & avg_income_CPI_parent > 0)

data_b %>%
  select(rank_avg_eq_income_CPI_child_one, rank_avg_eq_income_CPI_parent_one) %>%
  summary()

model <- with(data_b, lm(rank_avg_eq_income_CPI_child_one ~ rank_avg_eq_income_CPI_parent_one))
summary(model)

model <- with(data_b, lm(rank_avg_eq_income_CPI_child_mult ~ rank_avg_eq_income_CPI_parent_mult))
summary(model)

ggplot(data_b, aes(x = rank_avg_eq_income_CPI_parent_one, y = rank_avg_eq_income_CPI_child_one)) +
  geom_point(aes(color = year_parent)) +
  stat_smooth() +
  theme_minimal() +
  windows(15, 15)

ggplot(data_b, aes(x = rank_avg_eq_income_CPI_parent_mult, y = rank_avg_eq_income_CPI_child_mult)) +
  geom_point(aes(color = year_parent)) +
  stat_smooth() +
  theme_minimal() +
  windows(15, 15)

#### old code ####


source(here::here('R/module/utils/get_util.R'), local = environment())
data_merge <- readRDS('data/merge/data_merge.rds')

data_merge <- data_merge %>%
  select(
    pid, pid_child, hhid, contains('hhid_child'), contains('year'),
    contains('child'), contains('parent'), 
    everything()
  )

data_merge_1 <- data_merge %>%
  select(-pid) %>%
  select(pid_child, hhid_child,
         income_child, income_sw_child, income_nw_child,
         income_labor_child, income_labor_sw_child, income_labor_nw_child,
         income_parent, income_sw_parent, income_nw_parent,
         income_labor_parent, income_labor_sw_parent, income_labor_nw_parent,
         eq_income_child, eq_income_sw_child, eq_income_nw_child,
         eq_income_labor_child, eq_income_labor_sw_child, 
         eq_income_labor_nw_child,
         eq_income_parent, eq_income_sw_parent, eq_income_nw_parent,
         eq_income_labor_parent, eq_income_labor_sw_parent, 
         eq_income_labor_nw_parent
  ) %>%
  unique()

data_merge_1

model_1 <- lm(income_child ~ income_parent, data = data_merge_1)
summary(model_1)


data_merge_2 <- data_merge %>%
  select(-pid) %>%
  select(pid_child, hhid_child,
         income_child, income_sw_child, income_nw_child,
         income_labor_child, income_labor_sw_child, income_labor_nw_child,
         income_parent, income_sw_parent, income_nw_parent,
         income_labor_parent, income_labor_sw_parent, income_labor_nw_parent,
         eq_income_child, eq_income_sw_child, eq_income_nw_child,
         eq_income_labor_child, eq_income_labor_sw_child, 
         eq_income_labor_nw_child,
         eq_income_parent, eq_income_sw_parent, eq_income_nw_parent,
         eq_income_labor_parent, eq_income_labor_sw_parent, 
         eq_income_labor_nw_parent,
         cohabit,
         employ_type_child, employ_stat_child, employ_position_child,
         employ_position_parent
  ) %>%
  unique() %>%
  filter(income_child > 0 & income_parent > 0 & cohabit == 0
         & employ_position_child == 'Full-time' &
           employ_position_parent == 'Full-time')
model_2 <- lm(income_child ~ income_parent, data = data_merge_2)
summary(model_2)

model_2 <- lm(income_labor_child ~ income_labor_parent, data = data_merge_2)
summary(model_2)

data_merge_2 <- data_merge_2 %>%
  mutate(across(income_child:eq_income_labor_nw_parent,
                ~ ifelse(is.na(.x), NA, log(.x))))

model_2 <- lm(income_child ~ income_parent, data = data_merge_2)
summary(model_2)

model_2 <- lm(income_labor_child ~ income_labor_parent, data = data_merge_2)
summary(model_2)

data_merge_2 <- data_merge %>%
  select(-pid) %>%
  select(pid_child, hhid_child,
         income_child, income_sw_child, income_nw_child,
         income_labor_child, income_labor_sw_child, income_labor_nw_child,
         income_parent, income_sw_parent, income_nw_parent,
         income_labor_parent, income_labor_sw_parent, income_labor_nw_parent,
         eq_income_child, eq_income_sw_child, eq_income_nw_child,
         eq_income_labor_child, eq_income_labor_sw_child, 
         eq_income_labor_nw_child,
         eq_income_parent, eq_income_sw_parent, eq_income_nw_parent,
         eq_income_labor_parent, eq_income_labor_sw_parent, 
         eq_income_labor_nw_parent,
         cohabit, sex_child, educ_self_child, place_curr_child, age_child,
         employ_type_child, employ_stat_child, employ_position_child,
         sex_parent, educ_self_parent, place_curr_parent, age_parent,
         employ_type_parent, employ_stat_parent, employ_position_parent,
  ) %>%
  unique() %>%
  filter(eq_income_child > 0 & eq_income_parent > 0 &
           eq_income_labor_child > 0 & eq_income_labor_parent > 0 &
           employ_stat_child == 'Employed' & employ_stat_parent == 'Employed')
model_2 <- lm(eq_income_child ~ eq_income_parent, data = data_merge_2)
summary(model_2)

model_2 <- lm(eq_income_labor_child ~ eq_income_labor_parent, data = data_merge_2)
summary(model_2)

data_merge_2 <- data_merge_2 %>%
  mutate(across(income_child:eq_income_labor_nw_parent,
                ~ ifelse(is.na(.x), NA, log(.x))))

model_2 <- lm(eq_income_child ~ eq_income_parent, data = data_merge_2)
summary(model_2)

model_2 <- lm(eq_income_labor_child ~ eq_income_labor_parent, data = data_merge_2)
summary(model_2)

model_3 <- lm(eq_income_child ~ eq_income_parent + sex_child + age_child + educ_self_child
              + employ_stat_child + place_curr_child, data = data_merge_2)
summary(model_3)

data_merge_3 <- data_merge %>%
  select(-pid) %>%
  select(pid_child, hhid_child,
         avg_labor_pre_child, avg_labor_pre_sw_child, avg_labor_pre_nw_child,
         avg_labor_post_child, avg_labor_post_sw_child, avg_labor_post_nw_child,
         avg_labor_pre_parent, avg_labor_pre_sw_parent, avg_labor_pre_nw_parent,
         avg_labor_post_parent, avg_labor_post_sw_parent, avg_labor_post_nw_parent
  ) %>%
  unique() %>%
  filter(avg_labor_pre_child > 0 & avg_labor_pre_parent > 0)
model_4 <- lm(avg_labor_pre_child ~ avg_labor_pre_parent, data = data_merge_3)
summary(model_4)

summary(data_merge$avg_labor_post_child)

sd(log(data_merge_2$income_labor_child))
sd(log(data_merge_2$income_labor_parent))
cor(log(data_merge_2$income_labor_child), log(data_merge_2$income_labor_parent))

diagnose_IGE_data(data_merge)



library(splines)
model_spline <- lm(income_labor_child ~ ns(income_labor_parent, df = 4), data = data_merge)
summary(model_spline)
