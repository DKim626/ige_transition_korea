merged_final <- readRDS(here::here('data/KLIPS/merge/merged_final.rds'))


data_a <- merged_final %>%
  filter(hhid_child != hhid_parent) %>%
  select(-c(pid_parent:employ_position_parent), -hhid_parent) %>%
  mutate(year_parent = as.factor(year_parent)) %>%
  distinct()

merged_final %>% select(year_child, age_child, cohort) %>% table()
merged_final %>% select(year_parent, age_parent, cohort) %>% table()

data_b <- data_a %>%
  filter(avg_income_CPI_child > 0 & avg_income_CPI_parent > 0)

model <- with(data_b, lm(avg_income_CPI_child ~ avg_income_CPI_parent))
summary(model)

model <- with(data_b, lm(avg_eq_income_CPI_child ~ avg_eq_income_CPI_parent))
summary(model)

model <- with(data_b, lm(log_avg_income_CPI_child ~ log_avg_income_CPI_parent))
summary(model)

model <- with(data_b, lm(log_avg_eq_income_CPI_child ~ log_avg_eq_income_CPI_parent))
summary(model)

taus <- c(0.1, 0.25, 0.5, 0.75, 0.9)
model <- with(data_b, rq(avg_income_CPI_child ~ avg_income_CPI_parent, tau = taus))
summary(model)

taus <- c(0.1, 0.25, 0.5, 0.75, 0.9)
model <- with(data_b, rq(log_avg_income_CPI_child ~ log_avg_income_CPI_parent, tau = taus))
summary(model)

data_b <- data_a %>%
  filter(avg_income_labor_CPI_child > 0 & avg_income_labor_CPI_parent > 0)

model <- with(data_b, lm(avg_income_labor_CPI_child ~ avg_income_labor_CPI_parent))
summary(model)

model <- with(data_b, lm(avg_eq_income_labor_CPI_child ~ avg_eq_income_labor_CPI_parent))
summary(model)

data_b <- data_b %>%
  select(-matches('rank')) %>%
  mutate(across(c(matches('income')), ~ log(.x)))

merged_final %>%
  filter(hhid_child != hhid_parent) %>%
  filter(avg_income_labor_CPI_child > 0 &
           avg_income_labor_CPI_parent > 0) %>%
  select(age_parent, age_child, cohort) %>% 
  table()

data_c <- data_b %>%
  filter(cohort == 2)

model <- with(data_c, lm(avg_income_CPI_child ~ avg_income_CPI_parent))
summary(model)

model <- with(data_c, lm(avg_eq_income_CPI_child ~ avg_eq_income_CPI_parent))
summary(model)

model <- with(data_c, lm(avg_income_labor_CPI_child ~ avg_income_labor_CPI_parent))
summary(model)

model <- with(data_c, lm(avg_eq_income_labor_CPI_child ~ avg_eq_income_labor_CPI_parent))
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
