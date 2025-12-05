source(here::here('R/KOWEPS/module/clean/recode_var.R'))

raw_data <- readRDS('data/KOWEPS/raw/data_all.rds')

get_clean_data <- function (data) {
  
  eq_inc <- function (income, hmem_num) {
    result <- ifelse(hmem_num  != 0, income / sqrt(hmem_num ), 0)
    return(result)
  }
  
  calc_log_inc <- function (inc) {
    inc <- ifelse(inc < 0, NA_real_, inc)
    inc <- ifelse(inc == 0, 1, inc)
    
    log_inc <- log(inc)
    
    return (log_inc)
  }
  
  recoded_data <- data %>%
    # Error in the data set
    mutate(
      rel_head = fcase(
        pid == 588203 & year == 2009, 2,
        pid == 112003 & year == 2009, 12,
        default = rel_head
      )
    ) %>%
    mutate(
      area_5 = recode_area_5(area_5),
      area_7 = recode_area_7(area_7),
      rel_head = recode_rel_head(rel_head),
      sex = recode_sex(sex),
      age = year - year_birth,
      educ = recode_educ(educ),
      occ = recode_occ(occ),
      econ_act = recode_econ_act(econ_act),
      educ_level = recode_educ_2(educ_level),
      educ_major = recode_educ_major(educ_major),
      eq_inc_dis = eq_inc(income_disposable, hmem_num),
      eq_inc_cur = eq_inc(income_current, hmem_num),
      log_inc_dis = calc_log_inc(income_disposable),
      log_inc_cur = calc_log_inc(income_current),
      log_eq_inc_dis = calc_log_inc(eq_inc_dis),
      log_eq_inc_cur = calc_log_inc(eq_inc_cur),
    )
  
  get_price <- function (dir_path) {
    
    assert_that(dir.exists(here::here(dir_path)),
                msg = paste0("[ERROR] Directory ", dir_path, " does not exist."))
    
    file_CPI <- file.path(dir_path, 'CPI.txt')
    file_GDP <- file.path(dir_path, 'GDP Deflator.csv')
    
    assert_that(file.exists(here::here(file_CPI)),
                msg = paste0("[ERROR] Directory ", file_CPI, " does not exist."))
    assert_that(file.exists(here::here(file_GDP)),
                msg = paste0("[ERROR] Directory ", file_GDP, " does not exist."))
    
    
    CPI <- read.table(file = here::here(file_CPI),
                      skip = 1, col.names = c('year', 'CPI'))
    
    GDP <- read_csv(file = 'data/KLIPS/external/GDP Deflator.csv', skip = 4) %>%
      select(`Country Name`, `1997`:`2023`) %>%
      filter(`Country Name` == "Korea, Rep.") %>%
      pivot_longer(`1997`:`2023`, names_to = 'year', values_to = 'GDP') %>%
      select(year, GDP) %>%
      mutate(year = as.numeric(year))
    
    price <- left_join(CPI, GDP, by = 'year') %>%
      mutate(year = year + 1)
    
    return (price)
  }
  
  price <- get_price('data/KLIPS/external')
  
  recoded_data <- left_join(recoded_data, price, by = 'year')
  cols_income <- c('income_disposable', 'income_current',
                   'eq_inc_dis', 'eq_inc_cur',
                   'log_inc_dis', 'log_inc_cur',
                   'log_eq_inc_dis', 'log_eq_inc_cur')
  
  for (inc in cols_income) {
    inc_price <- paste(inc, c('CPI', 'GDP'), sep = '_')
    recoded_data <- recoded_data %>%
      mutate(
        !!inc_price[1] := .data[[inc]] / CPI * 100,
        !!inc_price[2] := .data[[inc]] / GDP * 100,
      )
  }
  
  recoded_data <- recoded_data %>%
    select(-CPI, -GDP)
  
  get_educ_data <- function (data) {
    educ_data <- data %>%
      select(pid, year, educ_level, educ_major) %>%
      filter(!is.na(educ_level) | !is.na(educ_major)) %>%
      group_by(pid) %>%
      arrange(year) %>%
      mutate(num = row_number()) %>%
      pivot_wider(
        names_from = num,
        values_from = year:educ_major,
        names_glue = '{.value}_{num}'
      ) %>%
      ungroup()
    
    educ_base <- expand.grid(year = unique(data$year),
                             pid = unique(data$pid)) %>%
      tibble() %>%
      left_join(educ_data, by = 'pid') %>%
      mutate(
        educ_level = fcase(
          !is.na(educ_level_3) & year_3 <= year, educ_level_3,
          !is.na(educ_level_2) & year_2 <= year, educ_level_2,
          !is.na(educ_level_1), educ_level_1
        ),
        educ_major = fcase(
          !is.na(educ_major_3) & year_3 <= year, educ_major_3,
          !is.na(educ_major_2) & year_2 <= year, educ_major_2,
          !is.na(educ_major_1), educ_major_1
        )
      ) %>%
      select(year, pid, educ_level, educ_major) %>%
      filter(!is.na(educ_level) | !is.na(educ_major))
    
    result <- data %>%
      select(-educ_level, -educ_major) %>%
      left_join(educ_base, by = c('year', 'pid'))
    
    return (result)
  }
  
  clean_data <- get_educ_data(recoded_data)
  
  saveRDS(clean_data, 'data/KOWEPS/clean/clean_data.rds')
  
  return (clean_data)
}

clean_data <- get_clean_data(raw_data)

clean_data <- readRDS('data/KOWEPS/clean/clean_data.rds')

child_base_year = 2021
age_bandwidth = 2
child_base_age = 16
child_min_age = 25
child_max_age = 40
cols_income <- c('income_disposable', 'income_current',
                 'eq_inc_dis', 'eq_inc_cur',
                 'log_inc_dis', 'log_inc_cur',
                 'log_eq_inc_dis', 'log_eq_inc_cur')
cols_info <- c('pid', 'sex', 'age', 'educ', 'educ_level', 'educ_major',
               'econ_act', 'occ', 'area_5', 'area_7')

get_merged_data <- function (data, child_base_year, age_bandwidth,
                            child_base_age,
                            child_min_age, child_max_age) {
  bandwidth <- -age_bandwidth:age_bandwidth
  
  get_latest_info <- function(data, cols_info, time) {
    year <- 'year'
    if (time == 'past') {
      pid <- c('pid', 'pid_f', 'pid_m')
      cols_info <- cols_info[!(cols_info %in% c('sex', 'place_birth',
                                                'place_14',
                                                'educ_father', 'educ_mother'))]
      info <- c('past_year_base', 'hh_key',
                paste0(cols_info, '_f'),
                paste0(cols_info, '_m'))
    } else if (time == 'curr') {
      pid <- 'pid'
      info <- c('hh_key', 'base_year_age', cols_info, 'cohort')
    }
    pid <- 'pid'
    
    cols_select <- c(year, pid, info)
    
    sym_pid <- syms(pid)
    sym_cols <- syms(cols_select)
    sym_year <- sym(year)
    
    latest_info <- data %>%
      select(!!!sym_pid, !!sym_year, !!!sym_cols) %>%
      group_by(!!!sym_pid) %>%
      slice_max(order_by = !!sym_year, n = 1) %>%
      ungroup() %>%
      select(!!!sym_pid, !!!syms(info))
    
    return (latest_info)
  }
  
  get_children_parent_data <- function (data,
                                        child_base_year, child_base_age,
                                        age_bandwidth,
                                        child_min_age, child_max_age) {
    
    years <- child_base_year + seq(-age_bandwidth, age_bandwidth, by = 1)
    bandwidth <- -age_bandwidth:age_bandwidth
    
    lists_curr <- map(bandwidth, \(i) {
      data %>%
        filter(age <= child_max_age + i & age >= child_min_age + i &
                 year == child_base_year + i &
                 rel_head %in% c('Head', 'Spouse')) %>%
        mutate(
          base_year_age = age - i,
          cohort = fcase(
            base_year_age >= 36, 1,
            base_year_age >= 30, 2,
            base_year_age >= 25, 3
          )
        )
    })
    
    data_child_curr <- list_rbind(lists_curr)
    list_curr_pid <- data_child_curr %>%
      pull(pid) %>%
      unique()
    
    lists_past <- map(bandwidth, \(i) {
      data %>%
        filter(pid %in% list_curr_pid &
                 age == child_base_age + i &
                 rel_head == 'Children') %>%
        mutate(past_year_base = year + child_base_age - age)
    })
    
    data_child_past <- list_rbind(lists_past) %>%
      select(year, past_year_base, everything())
    list_past_pid <- data_child_past %>%
      pull(pid) %>%
      unique()
    list_past_hh_key <- data_child_past %>%
      pull(hh_key) %>%
      unique()
    
    data_parent_past <- data %>%
      filter(hh_key %in% list_past_hh_key &
               (rel_head %in% c('Head', 'Spouse'))
             # & age %in% parent_min_age:parent_max_age
      ) %>%
      select(year, hh_key, pid,
             sex, age, educ, econ_act, occ, educ_level, educ_major,
             area_5, area_7,
             income_disposable, income_current,
             income_disposable_CPI, income_current_CPI,
             income_disposable_GDP, income_current_GDP,
             weight_sample_hh, weight_general_hh,
             weight_sample_hh_1, weight_general_hh_1,
             weight_sample_hh_2, weight_general_hh_2,
             weight_sample_ind_c, weight_sample_ind_l,
             weight_general_ind_c, weight_general_ind_l,
             weight_sample_ind_c_1, weight_sample_ind_l_1,
             weight_general_ind_c_1, weight_general_ind_l_1,
             weight_sample_ind_c_2, weight_sample_ind_l_2,
             weight_general_ind_c_2, weight_general_ind_l_2) %>%
      mutate(parent = ifelse(sex == 'Male', 'f', 'm')) %>%
      select(-sex) %>%
      group_by(year, hh_key) %>%
      pivot_wider(names_from = parent,
                  values_from = pid:weight_general_ind_l_2,
                  names_vary = 'fastest') %>%
      ungroup()
    
    data_list <- list()
    data_list[['child_current']] <- data_child_curr %>%
      filter(pid %in% list_past_pid)
    data_list[['child_past']] <- data_child_past
    data_list[['parent_past']] <- data_parent_past
    
    data_list[['past_data']] <- inner_join(
      data_child_past,
      data_parent_past,
      by = c('year', 'hh_key')
    )
    data_list[['current_data']] <- data_list[['child_current']] %>%
      filter(pid %in% unique(data_list[['past_data']]$pid))
    
    return (data_list)
  }
  
  list_data_child <- get_children_parent_data(data,
                                              child_base_year, child_base_age,
                                              age_bandwidth,
                                              child_min_age, child_max_age)
  
  saveRDS(list_data_child, 'data/KOWEPS/merge/list_data_child.rds')
  
  cols_income_t <- c(cols_income,
                     paste0(cols_income, '_GDP'),
                     paste0(cols_income, '_CPI'))
  
  data_past <- list_data_child$past_data
  
  avg_past <- data_past %>%
    select(pid,
           any_of(cols_income_t),
           weight_sample_hh, weight_general_hh,
           weight_sample_hh_1, weight_general_hh_1,
           weight_sample_hh_2, weight_general_hh_2,
           weight_sample_ind_c, weight_sample_ind_l,
           weight_general_ind_c, weight_general_ind_l,
           weight_sample_ind_c_1, weight_sample_ind_l_1,
           weight_general_ind_c_1, weight_general_ind_l_1,
           weight_sample_ind_c_2, weight_sample_ind_l_2,
           weight_general_ind_c_2, weight_general_ind_l_2) %>%
    group_by(pid) %>%
    summarize(across(income_disposable:weight_general_ind_l_2,
                     ~ mean(.x, na.rm = TRUE)))
  
  data_curr <- list_data_child$current_data
  avg_curr <- data_curr %>%
    select(pid,
           any_of(cols_income_t),
           weight_sample_hh, weight_general_hh,
           weight_sample_hh_1, weight_general_hh_1,
           weight_sample_hh_2, weight_general_hh_2,
           weight_sample_ind_c, weight_sample_ind_l,
           weight_general_ind_c, weight_general_ind_l,
           weight_sample_ind_c_1, weight_sample_ind_l_1,
           weight_general_ind_c_1, weight_general_ind_l_1,
           weight_sample_ind_c_2, weight_sample_ind_l_2,
           weight_general_ind_c_2, weight_general_ind_l_2) %>%
    group_by(pid) %>%
    summarize(across(income_disposable:weight_general_ind_l_2,
                     ~ mean(.x, na.rm = TRUE)))
  
  info_past <- get_latest_info(data_past, cols_info = cols_info,
                               time = 'past')
  info_curr <- get_latest_info(data_curr, cols_info = cols_info,
                               time = 'curr')
  
  past_data <- left_join(avg_past, info_past, by = 'pid')
  curr_data <- left_join(avg_curr, info_curr, by = 'pid')
  
  merged_data <- inner_join(past_data, curr_data,
                            by = 'pid',
                            suffix = c('_past', '_curr'))
  
  saveRDS(merged_data, 'data/KOWEPS/merge/merged_data.rds')
  
  return (merged_data)
}

merge <- get_merged_data(clean_data, child_base_year, age_bandwidth,
                         child_base_age,
                         child_min_age, child_max_age)

#### ==== calc_rank ====
#'
#'
#'
#'
#'
calc_rank <- function (merged_data,
                       child_base_year, age_bandwidth,
                       cols_income) {
  message("[INFO] Initiated calc_rank")
  ### === Initial setup ===
  clean_data <- readRDS('data/KOWEPS/clean/clean_data.rds')
  
  message("[INFO] Deriving distributions and ranks")
  calc_rank_years <- function (data, base_year, cols_income,
                               exclude_zero = FALSE,
                               weight_sum_avg = 'sum') {
    id <- 'pid'
    # weight <- c('weight_sample_hh', 'weight_general_hh',
    #             'weight_sample_hh_1', 'weight_general_hh_1',
    #             'weight_sample_hh_2', 'weight_general_hh_2',
    #             'weight_sample_ind_c', 'weight_sample_ind_l',
    #             'weight_general_ind_c', 'weight_general_ind_l',
    #             'weight_sample_ind_c_1', 'weight_sample_ind_l_1',
    #             'weight_general_ind_c_1', 'weight_general_ind_l_1',
    #             'weight_sample_ind_c_2', 'weight_sample_ind_l_2',
    #             'weight_general_ind_c_2', 'weight_general_ind_l_2')
    
    weight <- c('weight_sample_ind_l', 'weight_general_ind_l',
                'weight_sample_ind_c', 'weight_general_ind_c',
                'weight_sample_ind_l_2', 'weight_general_ind_l_2',
                'weight_sample_ind_c_2', 'weight_general_ind_c_2'
                )
    
    ### === Initial set-up
    sym_select <- syms(c(id, cols_income, weight))
    
    years <- seq(base_year-2, base_year+2, by = 1)
    
    ### === Filtering the target years ===
    data <- data %>%
      filter(year %in% years) %>%
      select(year, !!!sym_select) %>%
      distinct()
    
    ### === Computation of ranks ===
    comb_inc_w <- expand.grid(cols_income = cols_income,
                              weights = weight,
                              stringsAsFactors = FALSE)
    
    rank_results <- pmap(comb_inc_w, \(cols_income, weights) {
      avg_name <- paste('avg', cols_income, sep = '_')
      rank_name <- paste('rank_years', avg_name, weights, sep = '_')
      
      data <- data %>%
        mutate(
          w_value = if (weights %in% colnames(.)) .data[[weights]] else NA
        ) %>%
        filter(!is.na(w_value))
      if (exclude_zero == TRUE) data <- filter(data, !!sym(cols_income) > 0)
      data <- data %>%
        group_by(!!sym(id)) %>%
        summarize(
          !!avg_name := mean(.data[[cols_income]], na.rm = TRUE),
          w_sum_avg = if (weight_sum_avg == 'sum') sum(w_value, na.rm = TRUE)
          else mean(w_value, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        arrange(!!sym(avg_name)) %>%
        mutate(
          cumul_w = cumsum(w_sum_avg)
        ) %>%
        group_by(!!sym(avg_name)) %>%
        mutate(w_final = mean(cumul_w)) %>%
        ungroup() %>%
        mutate(
          !!rank_name := w_final / sum(w_sum_avg) * 100
        ) %>%
        select(!!sym(id), !!sym(avg_name), !!sym(rank_name))
    })
    
    ### === Merge and return results ===
    rank_result <- reduce(rank_results,
                          .f = \(x, y) full_join(x, y, by = id)) %>%
      mutate(year = base_year) %>%
      select(year, pid, contains('rank'))
    
    return (rank_result)
    
  }
  
  ### === Deriving rank distribution ===
  past_years <- sort(unique(merged_data$past_year_base))
  years_rank_list <- map(c(past_years, child_base_year), \(year) {
    calc_rank_years(data = clean_data, base_year = year,
                    cols_income = cols_income)
  }) %>%
    list_rbind()
  
  data <- merged_data %>%
    left_join(years_rank_list %>%
                filter(year != child_base_year) %>%
                rename_with(~paste0(.x, '_f')),
              by = c('past_year_base' = 'year_f', 'pid_f')) %>%
    left_join(years_rank_list %>%
                filter(year != child_base_year) %>%
                rename_with(~paste0(.x, '_m')),
              by = c('past_year_base' = 'year_m', 'pid_m')) %>%
    left_join(years_rank_list %>%
                filter(year == child_base_year) %>%
                rename_with(~paste0(.x, '_curr')),
              by = c('pid' = 'pid_curr'))
  
  saveRDS(data, file = 'data/KOWEPS/merge/merged_rank.rds')
  
  return(data)
}

child_base_year = 2021
age_bandwidth = 2
cols_income <- c('income_disposable', 'income_current',
                 'eq_inc_dis', 'eq_inc_cur',
                 'log_inc_dis', 'log_inc_cur',
                 'log_eq_inc_dis', 'log_eq_inc_cur')

rank <- calc_rank(merge, child_base_year, age_bandwidth, cols_income)

merge <- readRDS('data/KOWEPS/merge/merged_data.rds')
rank <- readRDS('data/KOWEPS/merge/merged_rank.rds')



model <- lm(log_inc_cur_CPI_curr ~ log_inc_cur_CPI_past, data = merge)
summary(model)

model <- lm(log_inc_cur_CPI_curr ~ log_inc_cur_CPI_past, data = merge,
            weights = weight_sample_hh_curr)
summary(model)
model <- lm(log_inc_cur_CPI_curr ~ log_inc_cur_CPI_past, data = merge,
            weights = weight_general_hh_curr)
summary(model)

model <- lm(log_eq_inc_cur_CPI_curr ~ log_eq_inc_cur_CPI_past, data = merge,
            weights = weight_sample_ind_c_curr)
summary(model)
model <- lm(log_eq_inc_cur_CPI_curr ~ log_eq_inc_cur_CPI_past, data = merge,
            weights = weight_general_ind_c_curr)
summary(model)

model <- lm(log_eq_inc_cur_CPI_curr ~ log_eq_inc_cur_CPI_past, data = merge,
            weights = weight_sample_ind_l_2_curr)
summary(model)
model <- lm(log_eq_inc_cur_CPI_curr ~ log_eq_inc_cur_CPI_past, data = merge,
            weights = weight_general_ind_l_2_curr)
summary(model)

temp <- merge %>%
  filter(area_5_f %in% c('Seoul', 'Metropolitan City', 'City (Si)'))
temp <- merge %>%
  filter(base_year_age <= 31)
temp <- merge %>%
  filter(log_eq_inc_cur_CPI_curr > 0 & log_eq_inc_cur_CPI_past > 0)

temp <- merge %>%
  filter(pid %in% data_trans$pid)

model <- lm(log_inc_cur_CPI_curr ~ log_inc_cur_CPI_past, data = temp)
summary(model)

model <- lm(log_inc_cur_CPI_curr ~ log_inc_cur_CPI_past, data = temp,
            weights = weight_sample_hh_curr)
summary(model)
model <- lm(log_inc_cur_CPI_curr ~ log_inc_cur_CPI_past, data = temp,
            weights = weight_general_hh_curr)
summary(model)

model <- lm(log_eq_inc_cur_CPI_curr ~ log_eq_inc_cur_CPI_past, data = temp,
            weights = weight_sample_ind_c_curr)
summary(model)
model <- lm(log_eq_inc_cur_CPI_curr ~ log_eq_inc_cur_CPI_past, data = temp,
            weights = weight_general_ind_c_curr)
summary(model)

model <- lm(log_eq_inc_cur_CPI_curr ~ log_eq_inc_cur_CPI_past, data = temp,
            weights = weight_sample_ind_l_2_curr)
summary(model)
model <- lm(log_eq_inc_cur_CPI_curr ~ log_eq_inc_cur_CPI_past, data = temp,
            weights = weight_general_ind_l_2_curr)
summary(model)

data_2021_KOWEPS <- merge %>%
  filter(log_eq_inc_cur_CPI_curr > 0 & log_eq_inc_cur_CPI_past > 0)

data_2021_KOWEPS <- merge %>%
  filter(log_eq_inc_dis_CPI_curr > 0 & log_eq_inc_dis_CPI_past > 0)

merge_koweps <- data_2021_KOWEPS %>%
  rename(
    log_child_inc = log_eq_inc_cur_CPI_curr,
    log_parent_inc = log_eq_inc_cur_CPI_past,
    child_inc = eq_inc_cur_CPI_curr,
    parent_inc = eq_inc_cur_CPI_past,
    weight = weight_general_ind_l_2_curr
  ) %>%
  select(log_child_inc, log_parent_inc,
         child_inc, parent_inc, base_year_age, weight) %>%
  mutate(data = 'KOWEPS')

merge_koweps <- data_2021_KOWEPS %>%
  rename(
    log_child_inc = log_eq_inc_dis_CPI_curr,
    log_parent_inc = log_eq_inc_dis_CPI_past,
    child_inc = eq_inc_cur_CPI_curr,
    parent_inc = eq_inc_cur_CPI_past,
    weight = weight_general_ind_l_2_curr
  ) %>%
  select(log_child_inc, log_parent_inc,
         child_inc, parent_inc, base_year_age, weight) %>%
  mutate(data = 'KOWEPS')

model_1 <- lm(log_child_inc ~ log_parent_inc, data = merge_koweps,
            weights = weight)
summary(model_1)

merge_klips <- data_2021_b %>%
  rename(
    log_child_inc = log_eq_income_2_CPI_curr,
    log_parent_inc = log_eq_income_2_CPI_past,
    child_inc = eq_income_2_CPI_curr,
    parent_inc = eq_income_2_CPI_past,
    weight = nw_p_c_curr
  ) %>%
  select(log_child_inc, log_parent_inc,
         child_inc, parent_inc, base_year_age, weight) %>%
  mutate(data = 'KLIPS')

merge_klips_koweps <- merge_klips %>%
  filter(base_year_age <= 33)

model_2 <- lm(log_child_inc ~ log_parent_inc, data = merge_klips_koweps,
            weights = weight)
summary(model)

models <- list(
  'KOWEPS' = model_1,
  'KLIPS' = model_2
)

texreg(models)

merged_korean <- bind_rows(merge_koweps, merge_klips)

merged_korean %>%
  filter(base_year_age > 30) %>%
  ggplot(aes(x = log_parent_inc, y = log_child_inc, color = data)) +
  geom_point(alpha = 0.8) +
  theme_minimal() +
  windows(15, 15)

