source(here::here('R/KLIPS/module/utils/utility.R'), local = environment())

merge_child_parent <- function (merged_all, child_base_age, child_base_year,
                                age_bandwidth, child_min_age, child_max_age,
                                parent_min_age, parent_max_age, IMF_filter,
                                dir_path) {
  on.exit(gc())
  
  get_children_data <- function(data, child_base_age, child_base_year,
                                age_bandwidth,
                                child_min_age, child_max_age) {
    
    years <- child_base_year + seq(-age_bandwidth, age_bandwidth, by = 1)
    bandwidth <- -age_bandwidth:age_bandwidth
    mean_age <- as.integer(mean(c(child_max_age, child_min_age)))

    lists <- map(bandwidth, \(i) {
      data %>%
        select(year, pid, age) %>%
        filter(age <= child_max_age + i & age >= child_min_age + i &
                 year == child_base_year + i) %>%
        mutate(
          cohort = fcase(
            age <= (mean_age + i), 1,
            age > (mean_age + i), 2
          )
        ) %>%
        select(pid, cohort)
    })
    
    list_child <- list_rbind(lists) %>%
      distinct()
    
    data <- data %>%
      filter(year %in% years & pid %in% list_child$pid) %>%
      mutate(base_year = year - age + child_base_age,
             parent_base_year = base_year + year - child_base_year
             ) %>%
      left_join(list_child, by = 'pid', relationship = 'many-to-many')
    
    save_overwrite_rds(list_child, dir_path, 'list_child.rds')
    
    return(data)
  }
  
  child_data <- get_children_data(merged_all, child_base_age, child_base_year,
                                  age_bandwidth,
                                  child_min_age, child_max_age)
  
  load_or_use(relation_key, dir_path)
  
  mapping <- relation_key %>%
    filter(pid_child %in% child_data$pid)
  
  merged_data <- merged_all %>%
    inner_join(mapping, by = 'pid', relationship = 'many-to-many') %>%
    left_join(child_data,
              by = c('pid_child' = 'pid', 'year' = 'parent_base_year'),
              suffix = c('_parent', '_child'),
              relationship = 'many-to-many') %>%
    filter(!is.na(year_child)) %>%
    unique() %>%
    filter(age_parent >= parent_min_age & age_parent <= parent_max_age) %>%
    rename(
      'pid_parent' = pid,
      'year_parent' = year
    )
  
  if(IMF_filter == TRUE) {
    merged_data <- merged_data %>%
      filter(year_parent >= 2000)
  }
  
  save_overwrite_rds(merged_data, dir_path,
                     file_name = 'merged_child_parent.rds')
  
  return (merged_data)
}

calc_avg_inc <- function(data, obs, child_parent, cols_income, weight) {
  # === 1. Sanity check ===
  assert_that(obs %in% c('individual', 'household'),
              msg = '[ERROR] value of obs must be either individual or household')
  assert_that(child_parent %in% c('child', 'parent'),
              msg = '[ERROR] value of child_parent must be either child or parent')
  assert_that(weight %in% c('w', 'sw', 'nw'),
              msg = '[ERROR] value of weight must be either "w", "sw", or "nw"')
  
  # === 2. Setting variable names ===
  year <- paste0("year_", child_parent)
  hhid <- paste0("hhid_", child_parent)
  pid <- paste0("pid_", child_parent)
  pid_child <- if (child_parent == 'parent') 'pid_child' else NULL
  if (obs == 'household' | (obs == 'individual' & !is.null(pid_child))) {
    w_name <- paste0(weight, '_h')
  } else if (obs == 'individual' & child_parent == 'child') {
    w_name <- paste0(weight, '_p_c')
  } else {
    w_name <- paste0(weight, '_p')
  }
  cols_income <- c(cols_income,
                   paste(cols_income, 'CPI', sep = '_'),
                   paste(cols_income, 'GDP', sep = '_'))
  cols_income <- paste0(cols_income, '_', child_parent)
  target_weight <- paste0(w_name, '_', child_parent)
  
  # === 3. Setting variables to select ===
  cols_select <- c(year, hhid, pid)
  # if (obs == 'individual') cols_select <- c(year, hhid, pid)
  if (!is.null(pid_child)) cols_select <- c(cols_select, pid_child)
  cols_select <- c(cols_select, cols_income, target_weight)
  
  # === 4. Selecting variables ===
  data <- data %>%
    select(all_of(cols_select)) %>%
    distinct()
  
  # === Summing up individual labor income for parents ===
  # To ensure parent labor income -> child labor income linkage
  if (obs == 'individual' & !is.null(pid_child)) {
    data <- data %>%
      group_by(!!!syms(c(hhid, pid_child))) %>%
      mutate(across(all_of(cols_income), ~ sum(.x, na.rm = TRUE))) %>%
      ungroup() %>%
      distinct()
  }
  
  # === 5. Convert to real income ===
  group_cols <- if (obs == 'individual') {
    if (!is.null(pid_child)) c(hhid, pid_child) else c(pid)
  } else {
    if (!is.null(pid_child)) c(hhid, pid_child) else c(hhid, pid)
  }
  sym_cols <- syms(group_cols)
  
  # === 6. Average income ===
  
  results <- map(cols_income, \(.x) {
    
    avg_name <- sym(paste0('avg_', .x, '_', weight))
    
    data %>%
      filter(.data[[target_weight]] > 0) %>%
      group_by(!!!sym_cols) %>%
      summarize(
        !!avg_name := weighted.mean(.data[[.x]], .data[[target_weight]],
                                    na_rm = TRUE),
        .groups = 'drop'
      )
  })
  
  weight_data <- data %>%
    filter(.data[[target_weight]] > 0) %>%
    group_by(!!!sym_cols) %>%
    summarize(!!target_weight := mean(.data[[target_weight]]), .groups = 'drop')
  
  result <- reduce(results, \(.x, .y) full_join(.x, .y, by = group_cols)) %>%
    left_join(weight_data, by = group_cols)
  
  return (result)
}

get_latest_info <- function(data, cols_info, child_parent) {
  year <- paste0("year_", child_parent)
  pid <- paste0("pid_", child_parent)
  info <- paste0(cols_info, '_', child_parent)
  pid <- if (child_parent == 'parent') c(pid, 'pid_child') else pid
  
  cols_select <- c(year, pid, info)
  
  sym_pid <- syms(pid)
  sym_cols <- syms(cols_select)
  sym_year <- sym(year)
  
  
  latest_info <- data %>%
    select(!!!sym_cols) %>%
    group_by(!!!sym_pid) %>%
    slice_max(order_by = !!sym_year, n = 1) %>%
    ungroup()
  
  return (latest_info)
}

get_merged <- function (merged_all, child_base_age, child_base_year,
                        age_bandwidth, child_min_age, child_max_age,
                        parent_min_age, parent_max_age, IMF_filter,
                        cols_income_h, cols_income_p, cols_info,
                        dir_path) {
  on.exit(gc())
  
  load_or_use(merged_all, dir_path)
  
  data_child_parent <- merge_child_parent(merged_all,
                                          child_base_age, child_base_year,
                                          age_bandwidth,
                                          child_min_age, child_max_age,
                                          parent_min_age, parent_max_age,
                                          IMF_filter, dir_path)
  
  data_parent <- data_child_parent %>%
    select(year_parent, pid_parent, pid_child, hhid_parent, matches('parent')) %>%
    distinct()
  
  mapping <- data_child_parent %>%
    select(hhid_parent, hhid_child) %>%
    distinct()
  
  data_child <- data_child_parent %>%
    select(year_child, pid_child, hhid_child, matches('child')) %>%
    distinct()
  
  avg_parent_p <- calc_avg_inc(data = data_parent, obs = 'individual',
                               child_parent = 'parent',
                               cols_income = cols_income_p, weight = 'w')
  avg_parent_h <- calc_avg_inc(data = data_parent, obs = 'household',
                               child_parent = 'parent',
                               cols_income = cols_income_h, weight = 'w')
  avg_child_p <- calc_avg_inc(data = data_child, obs = 'individual',
                              child_parent = 'child',
                              cols_income = cols_income_p, weight = 'w')
  avg_child_h <- calc_avg_inc(data = data_child, obs = 'household',
                              child_parent = 'child',
                              cols_income = cols_income_h, weight = 'w')
  
  avg_child <- full_join(avg_child_h, avg_child_p, by = 'pid_child')
  avg_parent <- full_join(avg_parent_h, avg_parent_p, by = c('hhid_parent', 'pid_child'))
  
  child_latest_info <- get_latest_info(data_child, cols_info = cols_info,
                                       child_parent = 'child')
  parent_latest_info <- get_latest_info(data_parent, cols_info = cols_info,
                                        child_parent = 'parent')
  avg_child <- full_join(avg_child, child_latest_info, by = 'pid_child')
  avg_parent <- full_join(avg_parent, parent_latest_info,
                          by = 'pid_child', relationship = 'many-to-many') %>%
    distinct()
  
  avg_data <- full_join(avg_child, avg_parent,
                        by = 'pid_child', relationship = 'many-to-many') %>%
    distinct()
  
  save_overwrite_rds(avg_data, dir_path, 'merged_data.rds')
  
  return(avg_data)
}












