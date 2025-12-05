psid_all <- readRDS(data, file = here::here('data/PSID/raw/psid_data.rds'))
codelong <- readRDS(file = here::here('data/PSID/clean/codebook_value.rds'))
var_maps <- readRDS(here::here('data/PSID/clean/var_maps.rds'))

source(here::here('R/PSID/clean/recode_var.R'), local = environment())
source(here::here('R/PSID/clean/merge_data.R'), local = environment())

data_renamed <- create_coded_data(psid_all, codelong, var_maps)
data_all <- data_renamed

# data_all$sex %>% table()
# data_all$sex_rp %>% table()
# data_all$sex_sp %>% table()
# data_all$race %>% table()
# data_all$race_rp_1 %>% table()
# data_all$ind_rp %>% table()
# data_all$occ_rp %>% table()
# data_all$occ_sp %>% table()
# data_all$educ_highest %>% table()
# data_all$educ_bach_major_1 %>% table()
# data_all$educ_bach_major_2 %>% table()
# data_all$educ_bach_major %>% table()
# data_all$educ_highest_major_1 %>% table()
# data_all$educ_highest_major_2 %>% table()
# data_all$educ_highest_major %>% table()
# data_all$educ_highest_major_1_digit %>% table()
# data_all$educ_highest_major_digit %>% table()
# data_all$educ_highest_major_rp_1 %>% table()
# data_all$educ_highest_major_rp_2 %>% table()
# data_all$educ_highest_major_sp_1 %>% table()
# data_all$educ_highest_major_sp_2 %>% table()
# data_all$educ_highest_major_update_rp_1 %>% table()
# data_all$educ_highest_major_update_rp_2 %>% table()
# data_all$educ_highest_major_update_sp_1 %>% table()
# data_all$educ_highest_major_update_sp_2 %>% table()
# data_all$educ_bach_major_rp %>% table()
# data_all$educ_bach_major_sp %>% table()
# data_all$educ_highest_major_rp %>% table()
# data_all$educ_highest_major_sp %>% table()
# data_all$educ_hs_rp %>% table()
# data_all$curr_state %>% table()
# data_all %>% select(race_rp_1, year) %>% table()
# 
# data_all %>% filter(year == 2023) %>% pull(educ_highest_major_1) %>% table()

data_all <- data_all %>%
  mutate(
    year = as.numeric(year),
    # Individual demographic information
    sex_sp = recode_na(sex_sp),
    birth_year = ifelse(birth_year <= 2025, birth_year, NA_real_),
    age_birth = year - birth_year,
    race_rp_1 = recode_race(race_rp_1),
    race_rp_2 = recode_race(race_rp_2),
    race_rp_3 = recode_race(race_rp_3),
    race_rp_4 = recode_race(race_rp_4),
    race_rp = fcase(
      !is.na(race_rp_1), race_rp_1,
      !is.na(race_rp_2), race_rp_2,
      !is.na(race_rp_3), race_rp_3,
      !is.na(race_rp_4), race_rp_4
    ),
    race_sp_1 = recode_race(race_sp_1),
    race_sp_2 = recode_race(race_sp_2),
    race_sp_3 = recode_race(race_sp_3),
    race_sp_4 = recode_race(race_sp_4),
    race_sp = fcase(
      !is.na(race_sp_1), race_sp_1,
      !is.na(race_sp_2), race_sp_2,
      !is.na(race_sp_3), race_sp_3,
      !is.na(race_sp_4), race_sp_4
    ),
    # Industry and occupation of reference and spouse
    ind_rp = recode_na(ind_rp),
    ind_sp = recode_na(ind_sp),
    occ_rp = recode_na(occ_rp),
    occ_sp = recode_na(occ_sp),
    occ_rp = recode_occ_2(occ_rp),
    occ_sp = recode_occ_2(occ_sp),
    # Education level of individual
    ## Bachelor
    educ_bach_major_1 = recode_na(educ_bach_major_1),
    educ_bach_major_2 = recode_na(educ_bach_major_2),
    educ_bach_major_1_digit = recode_na(educ_bach_major_1_digit),
    educ_bach_major_2_digit = recode_na(educ_bach_major_2_digit),
    educ_bach_major_digit = fcase(
      is.na(educ_bach_major_1_digit), educ_bach_major_2_digit,
      default = educ_bach_major_1_digit
    ),
    educ_bach_major_digit = recode_major(educ_bach_major_digit),
    educ_highest = recode_na(educ_highest),
    educ_highest_major_1 = recode_na(educ_highest_major_1),
    educ_highest_major_2 = recode_na(educ_highest_major_2),
    educ_bach_major = fcase(
      is.na(educ_bach_major_1), educ_bach_major_2,
      default = educ_bach_major_1
    ),
    educ_bach_major = recode_major(educ_bach_major),
    educ_bach_major = fcase(
      !is.na(educ_bach_major), educ_bach_major,
      is.na(educ_bach_major) & !is.na(educ_bach_major_digit),
      educ_bach_major_digit
    ),
    ## Highest
    educ_highest_major = fcase(
      is.na(educ_highest_major_1), educ_highest_major_2,
      default = educ_highest_major_1
    ),
    educ_highest_major = recode_major(educ_highest_major),
    educ_highest_major_1_digit = recode_na(educ_highest_major_1_digit),
    educ_highest_major_2_digit = recode_na(educ_highest_major_2_digit),
    educ_highest_major_digit = fcase(
      is.na(educ_highest_major_1_digit), educ_highest_major_2_digit,
      default = educ_highest_major_1_digit
    ),
    educ_highest_major_digit = recode_major(educ_highest_major_digit),
    educ_complete = recode_na_educ(educ_complete),
    
    # Education level of reference and spouse
    ## Bachelor
    educ_bach_major_1_rp = recode_na(educ_bach_major_1_rp),
    educ_bach_major_2_rp = recode_na(educ_bach_major_2_rp),
    educ_bach_major_rp = fcase(
      is.na(educ_bach_major_1_rp), educ_bach_major_2_rp,
      default = educ_bach_major_1_rp
    ),
    educ_bach_major_1_update_rp = recode_na(educ_bach_major_1_update_rp),
    educ_bach_major_2_update_rp = recode_na(educ_bach_major_2_update_rp),
    educ_bach_major_update_rp = fcase(
      is.na(educ_bach_major_1_update_rp), educ_bach_major_2_update_rp,
      default = educ_bach_major_1_update_rp
    ),
    educ_bach_major_rp = fcase(
      is.na(educ_bach_major_rp), educ_bach_major_update_rp,
      default = educ_bach_major_rp
    ),
    educ_bach_major_rp = recode_major(educ_bach_major_rp),
    educ_bach_major_1_sp = recode_na(educ_bach_major_1_sp),
    educ_bach_major_2_sp = recode_na(educ_bach_major_2_sp),
    educ_bach_major_sp = fcase(
      is.na(educ_bach_major_1_sp), educ_bach_major_2_sp,
      default = educ_bach_major_1_sp
    ),
    educ_bach_major_1_update_sp = recode_na(educ_bach_major_1_update_sp),
    educ_bach_major_2_update_sp = recode_na(educ_bach_major_2_update_sp),
    educ_bach_major_update_sp = fcase(
      is.na(educ_bach_major_1_update_sp), educ_bach_major_2_update_sp,
      default = educ_bach_major_1_update_sp
    ),
    educ_bach_major_sp = fcase(
      is.na(educ_bach_major_sp), educ_bach_major_update_sp,
      default = educ_bach_major_sp
    ),
    educ_bach_major_sp = recode_major(educ_bach_major_sp),
    ## Highest
    educ_highest_major_rp_1 = recode_na(educ_highest_major_rp_1),
    educ_highest_major_rp_2 = recode_na(educ_highest_major_rp_2),
    educ_highest_major_rp = fcase(
      is.na(educ_highest_major_rp_1), educ_highest_major_rp_2,
      default = educ_highest_major_rp_1
    ),
    educ_highest_major_sp_1 = recode_na(educ_highest_major_sp_1),
    educ_highest_major_sp_2 = recode_na(educ_highest_major_sp_2),
    educ_highest_major_sp = fcase(
      is.na(educ_highest_major_sp_1), educ_highest_major_sp_2,
      default = educ_highest_major_sp_1
    ),
    educ_highest_major_update_rp_1 = recode_na(educ_highest_major_update_rp_1),
    educ_highest_major_update_rp_2 = recode_na(educ_highest_major_update_rp_2),
    educ_highest_major_update_rp = fcase(
      is.na(educ_highest_major_update_rp_1), educ_highest_major_update_rp_2,
      default = educ_highest_major_update_rp_1
    ),
    educ_highest_major_update_sp_1 = recode_na(educ_highest_major_update_sp_1),
    educ_highest_major_update_sp_2 = recode_na(educ_highest_major_update_sp_2),
    educ_highest_major_update_sp = fcase(
      is.na(educ_highest_major_update_sp_1), educ_highest_major_update_sp_2,
      default = educ_highest_major_update_sp_1
    ),
    educ_highest_major_rp = fcase(
      is.na(educ_highest_major_rp), educ_highest_major_update_rp,
      default = educ_highest_major_rp
    ),
    educ_highest_major_sp = fcase(
      is.na(educ_highest_major_sp), educ_highest_major_update_sp,
      default = educ_highest_major_sp
    ),
    educ_highest_major_rp = recode_major(educ_highest_major_rp),
    educ_highest_major_sp = recode_major(educ_highest_major_sp),
    educ_hs_rp = recode_na(educ_hs_rp),
    educ_hs_sp = recode_na(educ_hs_sp),
    eq_inc_total = inc_total / sqrt(fam_size),
    eq_inc_taxable_rs = inc_taxable_rs / sqrt(fam_size)
  ) %>%
  select(year:sex_rp, sex_sp, age:age_sp, age_birth,
         race, race_rp, race_sp, curr_state, fam_size,
         employ_stat:occ_sp,
         educ_complete,
         educ_complete_rp, educ_complete_sp,
         educ_bach_major, educ_highest,
         educ_highest_major, educ_highest_major_digit,
         educ_highest_year,
         educ_bach_major_rp,
         educ_bach_major_sp,
         educ_highest_major_rp,
         educ_highest_major_sp,
         educ_highest_year_rp, educ_highest_year_sp,
         educ_hs_rp, educ_hs_sp,
         eq_inc_total, eq_inc_taxable_rs,
         inc_total:wt_ind_l,
         )

get_price_adj(data_all)

data_all <- readRDS(file = here::here('data/PSID/clean/data_all.rds'))

data_child <- create_child_data(data_all) %>%
  mutate(base_year = year - age + 16) %>%
  group_by(pid) %>%
  mutate(base_year = round(mean(base_year)))

child_list <- create_child_list(data_all, min_age = 30, max_age = 40,
                                base_year = 2021, bandwidth = 2)

data_avg <- create_avg_data(data_all, data_child)

saveRDS(data_avg, file = here::here('data/PSID/clean/data_avg.rds'))

data_rank <- calc_rank_eq(data_avg,
                          child_base_year = 2021, age_bandwidth = 2,
                          cols_income_p = c('eq_inc_total'),
                          exclude_zero = FALSE)

data_rank <- calc_rank_cohort(data_rank, age_bandwidth = 2)

saveRDS(data_rank, file = here::here('data/PSID/clean/data_rank.rds'))

temp <- data_avg %>%
  filter(inc_total_cpi_past > 0 & inc_total_cpi_curr > 0)

model <- lm(log(inc_total_cpi_curr) ~ log(inc_total_cpi_past), temp, weights = wt_fam_l_curr)
summary(model)
model <- lm(log(eq_inc_total_cpi_curr) ~ log(eq_inc_total_cpi_past), temp, weights = wt_ind_l_curr)
summary(model)
model <- lm(eq_inc_total_cpi_curr ~ eq_inc_total_cpi_past, temp, weights = wt_ind_l_curr)
summary(model)
model <- lm(log_eq_inc_total_cpi_curr ~ log_eq_inc_total_cpi_past, temp, weights = wt_ind_l_curr)
summary(model)
taus <- c(0.1, 0.25, 0.5, 0.75, 0.9)
model <- with(temp, rq(eq_inc_total_cpi_curr ~ eq_inc_total_cpi_past, tau = taus))
summary(model)
model <- with(temp, rq(log_eq_inc_total_cpi_curr ~ log_eq_inc_total_cpi_past, tau = taus))
summary(model)

data_overall <- data_avg %>%
  filter(inc_total_cpi_past > 0 & inc_total_cpi_curr > 0)
data_3040 <- data_avg %>%
  filter(inc_total_cpi_past > 0 & inc_total_cpi_curr > 0 &
           base_year_age >= 30)
data_2529 <- data_avg %>%
  filter(inc_total_cpi_past > 0 & inc_total_cpi_curr > 0 &
           base_year_age >= 25 & base_year_age <= 29)
data_3035 <- data_avg %>%
  filter(inc_total_cpi_past > 0 & inc_total_cpi_curr > 0 &
           base_year_age >= 30 & base_year_age <= 35)
data_3640 <- data_avg %>%
  filter(inc_total_cpi_past > 0 & inc_total_cpi_curr > 0 &
           base_year_age >= 36 & base_year_age <= 40)
model_1 <- lm(log_eq_inc_total_cpi_curr ~ log_eq_inc_total_cpi_past,
              data = data_overall, weights = wt_ind_l_curr)
model_2 <- lm(log_eq_inc_total_cpi_curr ~ log_eq_inc_total_cpi_past,
              data = data_3040, weights = wt_ind_l_curr)
model_3 <- lm(log_eq_inc_total_cpi_curr ~ log_eq_inc_total_cpi_past,
              data = data_2529, weights = wt_ind_l_curr)
model_4 <- lm(log_eq_inc_total_cpi_curr ~ log_eq_inc_total_cpi_past,
              data = data_3035, weights = wt_ind_l_curr)
model_5 <- lm(log_eq_inc_total_cpi_curr ~ log_eq_inc_total_cpi_past,
              data = data_3640, weights = wt_ind_l_curr)
models <- list(
  'Overall' = model_1,
  '30-40' = model_2,
  '25-29' = model_3,
  '30-35' = model_4,
  '36-40' = model_5
)
texreg(models)


temp <- data_avg %>%
  filter(inc_labor_f_cpi > 0 & inc_labor_cpi > 0) %>%
  filter(sex_curr == 'Male')
model <- lm(log(inc_labor_cpi) ~ log(inc_labor_f_cpi), temp)
summary(model)
temp <- data_avg %>%
  filter(inc_labor_f_cpi > 0 & inc_labor_cpi > 0)
model <- lm(log(inc_labor_cpi) ~ log(inc_labor_f_cpi), temp)
summary(model)

temp <- data_avg %>%
  filter(inc_labor_p_cpi > 0 & inc_labor_cpi > 0) %>%
  filter(sex_past == 'Female')
model <- lm(log(eq_inc_labor_cpi) ~ log(eq_inc_labor_p_cpi), temp)
summary(model)
temp <- data_avg %>%
  filter(inc_labor_p_cpi > 0 & inc_labor_cpi > 0)
model <- lm(log(eq_inc_labor_cpi) ~ log(eq_inc_labor_p_cpi), temp)
summary(model)



data_avg %>%
  filter(!is.na(educ_level) & !is.na(educ_level_f) &
           !is.na(occ) & !is.na(occ_f))
data_avg %>%
  filter(!is.na(educ_level_2) & !is.na(educ_level_2_f) &
           !is.na(occ) & !is.na(occ_f))

data_avg %>%
  filter(!is.na(occ) & !is.na(occ_f))

data_avg %>%
  mutate(
    occ = fcase(
      is.na(occ), 'unemp',
      default = occ
    ),
    occ_f = fcase(
      is.na(occ_f), 'unemp',
      default = occ_f
    ),
    occ_m = fcase(
      is.na(occ_m), 'unemp',
      default = occ_m
    )
  ) %>%
  filter(!is.na(educ_level) & (!is.na(educ_level_f)| !is.na(educ_level_m)))

data_avg %>%
  filter(!is.na(educ_level) & !is.na(educ_level_f) &
           educ_level != 'College Grad (Major Unknown)' &
           educ_level_f != 'College Grad (Major Unknown)')

data_avg %>%
  mutate(
    occ = fcase(
      is.na(occ), 'unemp',
      default = occ
    ),
    occ_f = fcase(
      is.na(occ_f), 'unemp',
      default = occ_f
    ),
    occ_m = fcase(
      is.na(occ_m), 'unemp',
      default = occ_m
    )
  ) %>%
  filter(!is.na(educ_level_2) & (!is.na(educ_level_2_f) | !is.na(educ_level_2_m)))
  
