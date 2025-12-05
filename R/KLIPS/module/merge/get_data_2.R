source(here::here('R/KLIPS/module/utils/utility.R'), local = environment())


get_latest_info <- function(data, cols_info, time) {
  year <- 'year'
  if (time == 'past') {
    pid <- c('pid', 'pid_f', 'pid_m')
    cols_info <- cols_info[!(cols_info %in% c('sex', 'place_birth',
                                              'place_14',
                                              'educ_father', 'educ_mother'))]
    info <- c('past_year_base', 'hhid',
              paste0(cols_info, '_f'),
              paste0(cols_info, '_m'))
  } else if (time == 'curr') {
    pid <- 'pid'
    info <- c('hhid', 'base_year_age', cols_info, 'cohort')
  }
  pid <- 'pid'
  
  cols_select <- c(year, pid, info)
  
  sym_pid <- syms(pid)
  sym_cols <- syms(cols_select)
  sym_year <- sym(year)
  
  # latest_info_list <- map(info, \(x) {
  #   data %>%
  #     select(!!!sym_pid, !!sym_year, !!sym(x)) %>%
  #     filter(!is.na(!!sym(x))) %>%
  #     group_by(!!!sym_pid) %>%
  #     slice_max(order_by = !!sym_year, n = 1) %>%
  #     ungroup() %>%
  #     select(!!!sym_pid, !!sym(x))
  # })
  # 
  # latest_info <- reduce(latest_info_list,
  #                       \(x, y) full_join(x, y, by = pid))
  
  latest_info <- data %>%
    select(!!!sym_pid, !!sym_year, !!!sym_cols) %>%
    group_by(!!!sym_pid) %>%
    slice_max(order_by = !!sym_year, n = 1) %>%
    ungroup() %>%
    select(!!!sym_pid, !!!syms(info))
  
  return (latest_info)
}

get_merged <- function (merged_all, child_base_age, child_base_year,
                        age_bandwidth, child_min_age, child_max_age,
                        parent_min_age, parent_max_age, IMF_filter,
                        cols_income_t, cols_info,
                        dir_path) {
  on.exit(gc())
  
  load_or_use(merged_all, dir_path)
  
  get_children_parent_data <- function (data,
                                        child_base_year, child_base_age,
                                        age_bandwidth,
                                        child_min_age, child_max_age,
                                        parent_min_age, parent_max_age,
                                        IMF_filter) {
    
    years <- child_base_year + seq(-age_bandwidth, age_bandwidth, by = 1)
    birth_years <- seq(child_base_year - child_max_age,
                       child_base_year - child_min_age, by = 1)
    bandwidth <- -age_bandwidth:age_bandwidth
    
    # lists_curr <- map(bandwidth, \(i) {
    #   data %>%
    #     filter(age <= child_max_age + i & age >= child_min_age + i &
    #              year == child_base_year + i &
    #              fam_rel %in% c('Head', 'Spouse')) %>%
    #     mutate(
    #       base_year_age = child_base_year - year + age,
    #       cohort = fcase(
    #         base_year_age >= 36, 1,
    #         base_year_age >= 30, 2,
    #         base_year_age >= 25, 3
    #       )
    #     )
    # })
    # 
    # data_child_curr <- list_rbind(lists_curr)
    
    data_child_curr <- data %>%
      filter(year %in% years &
               birth_year %in% birth_years &
               fam_rel %in% c('Head', 'Spouse')) %>%
      mutate(
        base_year_age = child_base_year - birth_year,
        cohort = fcase(
          birth_year %in% 1981:1985, 1,
          birth_year %in% 1986:1991, 2,
          birth_year %in% 1992:1996, 3
        )
      )
    
    list_curr_pid <- data_child_curr %>%
      pull(pid) %>%
      unique()
    
    # lists_past <- map(bandwidth, \(i) {
    #   data %>%
    #     filter(pid %in% list_curr_pid &
    #              age == child_base_age + i &
    #              fam_rel == 'Children') %>%
    #     mutate(past_year_base = year + child_base_age - age)
    # })
    # 
    # data_child_past <- list_rbind(lists_past) %>%
    #   select(year, past_year_base, everything())
    
    data_child_past <- data %>%
      filter(pid %in% list_curr_pid &
               age %in% c(child_base_age + bandwidth) &
               fam_rel == 'Children') %>%
      mutate(past_year_base = birth_year + child_base_age) %>%
      select(year, past_year_base, everything())
    
    list_past_pid <- data_child_past %>%
      pull(pid) %>%
      unique()
    list_past_hhid <- data_child_past %>%
      pull(hhid) %>%
      unique()
    
    data_parent_past <- data %>%
      filter(hhid %in% list_past_hhid &
               (fam_rel %in% c('Head', 'Spouse'))
             # & age %in% parent_min_age:parent_max_age
             ) %>%
      select(year, hhid, pid,
             sex:employ_job_7_alt, educ_major,
             w_p_c:nw_p_c) %>%
      mutate(parent = ifelse(sex == 'Male', 'f', 'm')) %>%
      select(-sex) %>%
      group_by(year, hhid) %>%
      pivot_wider(names_from = parent,
                  values_from = pid:nw_p_c,
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
      by = c('year', 'hhid')
    )
    if (IMF_filter == TRUE) {
      data_list[['past_data']] <- data_list[['past_data']] %>%
        filter(year >= 2000)
    }
    data_list[['current_data']] <- data_list[['child_current']] %>%
      filter(pid %in% unique(data_list[['past_data']]$pid))
    
    return (data_list)
  }
  
  list_data_child <- get_children_parent_data(merged_all,
                                              child_base_year, child_base_age,
                                              age_bandwidth,
                                              child_min_age, child_max_age,
                                              parent_min_age, parent_max_age,
                                              IMF_filter)
  
  file_name <- paste0('list_data_child_', child_base_year, '.rds')
  
  save_overwrite_rds(list_data_child, dir_path, file_name)
  
  cols_income <- c(cols_income_t, paste0(cols_income_t, '_CPI'))
  
  data_past <- list_data_child$past_data
  
  avg_past <- data_past %>%
    select(pid,
           any_of(cols_income),
           w_p_c:nw_p_c, w_h:nw_h) %>%
    group_by(pid) %>%
    summarize(across(income_2:nw_h, \(x) mean(x, na.rm = TRUE)))
  
  data_curr <- list_data_child$current_data
  avg_curr <- data_curr %>%
    select(pid,
           any_of(cols_income),
           w_p_c:nw_p_c, w_h:nw_h) %>%
    group_by(pid) %>%
    summarize(across(income_2:nw_h, \(x) mean(x, na.rm = TRUE)))
  
  info_past <- get_latest_info(data_past, cols_info = cols_info,
                               time = 'past')
  info_curr <- get_latest_info(data_curr, cols_info = cols_info,
                               time = 'curr')
  
  past_data <- left_join(avg_past, info_past, by = 'pid')
  curr_data <- left_join(avg_curr, info_curr, by = 'pid')
  
  merged_data <- inner_join(past_data, curr_data,
                            by = 'pid',
                            suffix = c('_past', '_curr'))
  
  file_name <- paste0('merged_child_parent_', child_base_year, '.rds')

  save_overwrite_rds(merged_data, dir_path, file_name)
  
  return(merged_data)
}

