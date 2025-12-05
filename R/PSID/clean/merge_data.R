source(here::here('R/PSID/merged/calc_rank.R'), local = environment())

#### get_price_adj ####
#'
#'
#'
#'
get_price_adj <- function (data_all) {
  get_price <- function () {
    cpi_data <- read_xlsx(
      here::here('data/PSID/raw/SeriesReport-20250905172716_fe02e0.xlsx'),
      skip = 11
    ) %>%
      rename('cpi' = 'Annual') %>%
      mutate(year = Year + 1) %>%
      select(year, cpi)
    
    GDP <- read_csv(file = 'data/KLIPS/external/GDP Deflator.csv', skip = 4) %>%
      select(`Country Name`, `1989`:`2023`) %>%
      filter(`Country Name` == "United States") %>%
      pivot_longer(`1989`:`2023`, names_to = 'year', values_to = 'GDP') %>%
      select(year, GDP) %>%
      mutate(year = as.numeric(year) + 1)
    
    price_data <- left_join(cpi_data, GDP, by = 'year')
    
    return (price_data)
  }
  
  price_data <- get_price()
  
  data_all <- left_join(data_all, price_data, by = 'year')
  
  
  vars <- c('inc_total', 'eq_inc_total', 'eq_inc_taxable_rs',
            'inc_taxable_rs', 'inc_labor_rp', 'inc_labor_sp')
  
  adj_data <- map(vars, function (var) {
    data_all %>%
      select(year, pid, !!var, cpi, GDP) %>%
      mutate(
        !!paste0(var, '_cpi') := .[[var]] / cpi * 100,
        !!paste0(var, '_gdp') := .[[var]] / GDP * 100
      ) %>%
      select(paste0(var, '_cpi'), paste0(var, '_gdp'))
  })
  
  adj_data <- bind_cols(adj_data)
  
  data_all <- bind_cols(data_all, adj_data)
  
  saveRDS(data_all, file = here::here('data/PSID/clean/data_all.rds'))
}


#### create_child_list ####
#'
#'
#'
#'
create_child_list <- function (data, min_age, max_age, base_year, bandwidth) {
  
  bw <- -bandwidth:bandwidth
  
  list <- map(bw, \(b) {
    each_year <- base_year + b
    each_min_age <- min_age + b
    each_max_age <- max_age + b

    data %>%
      filter(
        year == each_year & ((age <= each_max_age & age >= each_min_age) |
          (!is.na(birth_year) & (age_birth <= max_age) &
             (age_birth >= min_age)))
      ) %>%
      filter(!stri_detect_fixed(rel_head, 'Inap.')) %>%
      select(pid)
  }) %>%
    bind_rows() %>%
    unique()
}


#### create_child_data ####
#'
#'
#'
#'
create_child_data <- function (data = data_all, min_age = 25, max_age = 40,
                               base_year = 2021,
                               base_age = 16, bandwidth = 2) {
  
  child_list <- create_child_list(data, min_age, max_age,
                                  base_year, bandwidth)
  
  age_band <- base_age + (-bandwidth:bandwidth)
  
  data_child_past <- data %>%
    filter(pid %in% child_list$pid &
             (age %in% age_band |
                (!is.na(birth_year) & age_birth %in% age_band))
           ) %>%
    filter(!stri_detect_fixed(rel_head, 'Inap.')) %>%
    filter(stri_detect_regex(rel_head, 'Son|[Ss]tep-?son')) %>%
    arrange(pid, year)
  
  data_child_curr <- data %>%
    filter(pid %in% data_child_past$pid & year %in% 2019:2023) %>%
    filter(!stri_detect_fixed(rel_head, 'Inap.')) %>%
    filter(stri_detect_regex(rel_head, '^(Reference|Legal Spouse)')) %>%
    mutate(
      base_year_age = fcase(
        !is.na(birth_year), year - birth_year,
        is.na(birth_year), base_year - year + age
      ),
      cohort = fcase(
        base_year_age >= 36, 1,
        base_year_age >= 30, 2,
        base_year_age >= 25, 3
      )
    ) %>%
    arrange(pid, year)
  
  data_child_past <- data_child_past %>%
    filter(pid %in% data_child_curr$pid)
  
  data_child <- bind_rows(data_child_past, data_child_curr)
  
  return (data_child)
}

#### extract_educ_history ####
#'
#'
#'
#'
#'
extract_educ_history <- function (data) {
  
  get_mode <- function (x) {
    unique_x <- unique(x)
    counts <- tabulate(match(x, unique(x)))
    
    ordered <- order(counts, decreasing = TRUE)
    sorted_x <- unique_x[ordered]
    sorted_counts <- counts[ordered]
    
    if (sorted_x[1] %in% c('High School or Lower',
                           'College Grad (Major Unknown)')) {
      if (length(sorted_x) > 1) {
        if (sorted_x[2] %in% c('High School or Lower',
                               'College Grad (Major Unknown)') &
            length(sorted_x) > 2) {
          return (sorted_x[3])
        } else {
          return (sorted_x[2])
        }
      } else {
        return (sorted_x[1])
      }
    }
    
    return (sorted_x[1])
  }
  
  educ_ind <- data %>%
    select(year, pid,
           educ_complete, educ_highest,
           educ_bach_major,
           educ_highest_major, educ_highest_major_digit, educ_highest_year) %>%
    mutate(
      educ_highest_year = ifelse(educ_highest_year %in% c(0, 9998, 9999),
                                 NA_real_, educ_highest_year)
    ) %>%
    mutate(
      educ_level = fcase(
        !is.na(educ_highest_major), educ_highest_major,
        !is.na(educ_highest_major_digit), educ_highest_major_digit,
        !is.na(educ_bach_major), educ_bach_major,
        educ_complete <= 12, 'High School or Lower',
        educ_complete > 12, 'College Grad (Major Unknown)'
      )
    ) %>%
    select(year, pid, educ_level, educ_highest_year) %>%
    filter(!is.na(educ_level))
  
  educ_rp_sp <- data %>%
    select(year, pid, rel_head,
           educ_complete_rp, educ_complete_sp,
           educ_hs_rp, educ_hs_sp,
           educ_bach_major_rp,
           educ_bach_major_sp,
           educ_highest_major_rp,
           educ_highest_year_rp,
           educ_highest_major_sp,
           educ_highest_year_sp) %>%
    filter(stri_detect_regex(rel_head, '^(Reference|Head)') |
             stri_detect_regex(rel_head,
                               '^(Legal|Uncooperative).*([Ss]pouse|partner|Husband|[Ww]ife)') |
             stri_detect_regex(rel_head,
                               '^Partner')
    ) %>%
    mutate(
      educ_highest_year_rp = ifelse(educ_highest_year_rp %in% c(0, 9998, 9999),
                                    NA_real_, educ_highest_year_rp),
      educ_highest_year_sp = ifelse(educ_highest_year_sp %in% c(0, 9998, 9999),
                                    NA_real_, educ_highest_year_sp),
      educ_complete_rp = ifelse(educ_complete_rp == 99,
                                NA_real_, educ_complete_rp),
      educ_complete_sp = ifelse(educ_complete_sp == 99,
                                NA_real_, educ_complete_sp)
    ) %>%
    distinct() %>%
    mutate(
      educ_bach_major = fcase(
        stri_detect_regex(rel_head, '^(Reference|Head)'), educ_bach_major_rp,
        stri_detect_regex(rel_head,
                          '[Ss]pouse|[Pp]artner|[Ww]ife'), educ_bach_major_sp
      ),
      educ_highest_major = fcase(
        stri_detect_regex(rel_head, '^(Reference|Head)'), educ_highest_major_rp,
        stri_detect_regex(rel_head,
                          '[Ss]pouse|[Pp]artner|[Ww]ife'), educ_highest_major_sp
      ),
      educ_highest_year = fcase(
        stri_detect_regex(rel_head, '^(Reference|Head)'), educ_highest_year_rp,
        stri_detect_regex(rel_head,
                          '[Ss]pouse|[Pp]artner|[Ww]ife'), educ_highest_year_sp
      ),
      educ_complete = fcase(
        stri_detect_regex(rel_head, '^(Reference|Head)'), educ_complete_rp,
        stri_detect_regex(rel_head,
                          '[Ss]pouse|[Pp]artner|[Ww]ife'), educ_complete_sp
      ),
      educ_level = fcase(
        !is.na(educ_highest_major), educ_highest_major,
        !is.na(educ_bach_major), educ_bach_major,
        educ_complete <= 12, 'High School or Lower',
        educ_complete > 12, 'College Grad (Major Unknown)'
      )
    ) %>%
    select(year, pid, educ_level, educ_highest_year) %>%
    filter(!is.na(educ_level))
  
  educ_comb <- full_join(educ_ind, educ_rp_sp,
                         by = c('year', 'pid'),
                         suffix = c('_ind', '_rsp')) %>%
    mutate(
      educ_level = fcase(
        is.na(educ_level_ind) & !is.na(educ_level_rsp), educ_level_rsp,
        !is.na(educ_level_ind) & is.na(educ_level_rsp), educ_level_ind,
        educ_level_ind %in%
          c('High School or Lower', 'College Grad (Major Unknown)') &
          !(educ_level_rsp %in%
              c('High School or Lower', 'College Grad (Major Unknown)')),
        educ_level_rsp,
        !(educ_level_ind %in%
            c('High School or Lower', 'College Grad (Major Unknown)')) &
          educ_level_rsp %in%
          c('High School or Lower', 'College Grad (Major Unknown)'),
        educ_level_ind,
        !(educ_level_ind %in%
            c('High School or Lower', 'College Grad (Major Unknown)')) &
          !(educ_level_rsp %in%
              c('High School or Lower', 'College Grad (Major Unknown)')),
        educ_level_rsp,
        (educ_level_ind %in%
           c('High School or Lower', 'College Grad (Major Unknown)')) &
          (educ_level_rsp %in%
             c('High School or Lower', 'College Grad (Major Unknown)')),
        educ_level_rsp
      ),
      educ_highest_year = fcase(
        is.na(educ_highest_year_ind) & !is.na(educ_highest_year_rsp), educ_highest_year_rsp,
        !is.na(educ_highest_year_ind) & is.na(educ_highest_year_rsp), educ_highest_year_ind,
        educ_highest_year_ind %in%
          c('High School or Lower', 'College Grad (Major Unknown)') &
          !(educ_highest_year_rsp %in%
              c('High School or Lower', 'College Grad (Major Unknown)')),
        educ_highest_year_rsp,
        !(educ_highest_year_ind %in%
            c('High School or Lower', 'College Grad (Major Unknown)')) &
          educ_highest_year_rsp %in%
          c('High School or Lower', 'College Grad (Major Unknown)'),
        educ_highest_year_ind,
        !(educ_highest_year_ind %in%
            c('High School or Lower', 'College Grad (Major Unknown)')) &
          !(educ_highest_year_rsp %in%
              c('High School or Lower', 'College Grad (Major Unknown)')),
        educ_highest_year_rsp,
        (educ_highest_year_ind %in%
           c('High School or Lower', 'College Grad (Major Unknown)')) &
          (educ_highest_year_rsp %in%
             c('High School or Lower', 'College Grad (Major Unknown)')),
        educ_highest_year_rsp
      )
    ) %>%
    select(year, pid, educ_level, educ_highest_year) %>%
    group_by(pid) %>%
    summarize(
      educ_level = get_mode(educ_level)
    )
  
  return (educ_comb)
}

#### create_avg_data ####
#'
#'
#'
#'
create_avg_data <- function(data_all, data_child) {
  
  data_educ <- extract_educ_history(data_all)
  
  data <- data_child
  
  get_avg_data <- function (data, income_vars) {
    
    avg_data <- data %>%
      group_by(pid) %>%
      summarize(across(all_of(c(income_vars, 'wt_fam_l', 'wt_ind_l')),
                       \(x) mean(x, na.rm = TRUE)),
                .groups = 'drop')
    lavg_data <- data %>%
      mutate(across(all_of(income_vars), function(x) {
        x <- ifelse(x > 0, x, 1)
        log(x)
        })) %>%
      group_by(pid) %>%
      summarize(across(all_of(income_vars), \(x) mean(x, na.rm = TRUE)),
        .groups = 'drop') %>%
      rename_with(~paste0('log_', .x), contains('inc'))
    
    avg_data <- full_join(avg_data, lavg_data, by = 'pid')
    
    latest_info <- data %>%
      group_by(pid) %>%
      slice_max(order_by = year) %>%
      select(-year, -all_of(income_vars), -wt_ind_l, -wt_fam_l)
    
    result <- left_join(avg_data, latest_info, by = 'pid')
    
    return (result)
  }
  
  data_past <- data %>%
    filter(year < 2019) %>%
    left_join(data_educ, by = c('pid_f' = 'pid')) %>%
    rename(educ_level_f = educ_level) %>%
    left_join(data_educ, by = c('pid_m' = 'pid')) %>%
    rename(educ_level_m = educ_level) %>%
    select(year, base_year,
           pid, pid_f, pid_m,
           fam_size,
           sex, age,
           race_rp, race_sp,
           contains('inc'),
           educ_level_f,
           educ_level_m,
           occ_rp, occ_sp,
           contains('wt')) %>%
    mutate(
      inc_labor_p_cpi = inc_labor_rp_cpi + inc_labor_sp_cpi,
      inc_labor_p_gdp = inc_labor_rp_gdp + inc_labor_sp_gdp
    ) %>%
    select(-inc_total, -inc_taxable_rs, -inc_labor_rp, -inc_labor_sp)
  
  income_past <- colnames(data_past)[stri_detect_regex(colnames(data_past),
                                                       '^inc')]
  
  colnames(data_past) <- stri_replace_all_fixed(colnames(data_past),
                                                '_rp', '_f')
  colnames(data_past) <- stri_replace_all_fixed(colnames(data_past),
                                                '_sp', '_m')
  
  data_curr <- data %>%
    filter(year >= 2019) %>%
    left_join(data_educ, by = c('pid')) %>%
    select(year,
           pid, sex, age, base_year_age, cohort,
           fam_size,
           race_rp, race_sp,
           rel_head,
           contains('inc'),
           educ_level,
           occ_rp, occ_sp,
           contains('wt')) %>%
    mutate(
      race = fcase(
        stri_detect_fixed(rel_head, 'Reference'), race_rp,
        stri_detect_fixed(rel_head, 'Spouse'), race_sp,
        default = NA_character_
      ),
      inc_labor = fcase(
        stri_detect_fixed(rel_head, 'Reference'), inc_labor_rp,
        stri_detect_fixed(rel_head, 'Spouse'), inc_labor_sp,
        default = NA_real_
      ),
      inc_labor_cpi = fcase(
        stri_detect_fixed(rel_head, 'Reference'), inc_labor_rp_cpi,
        stri_detect_fixed(rel_head, 'Spouse'), inc_labor_sp_cpi,
        default = NA_real_
      ),
      inc_labor_gdp = fcase(
        stri_detect_fixed(rel_head, 'Reference'), inc_labor_rp_gdp,
        stri_detect_fixed(rel_head, 'Spouse'), inc_labor_sp_gdp,
        default = NA_real_
      ),
      occ = fcase(
        stri_detect_fixed(rel_head, 'Reference'), occ_rp,
        stri_detect_fixed(rel_head, 'Spouse'), occ_sp,
        default = NA_character_
      )
    ) %>%
    select(year,
           pid, sex, age, base_year_age, cohort,
           fam_size,
           race,
           inc_total_cpi, inc_total_gdp,
           eq_inc_total_cpi, eq_inc_total_gdp,
           inc_taxable_rs_cpi, inc_taxable_rs_gdp,
           eq_inc_taxable_rs_cpi, eq_inc_taxable_rs_gdp,
           inc_labor_cpi, inc_labor_gdp,
           contains('inc'),
           educ_level,
           occ,
           contains('wt'))
  
  income_curr <- colnames(data_curr)[stri_detect_regex(colnames(data_curr),
                                                       '^inc')]
  
  income_past <- colnames(data_past)[stri_detect_fixed(colnames(data_past),
                                                       'inc')]
  income_curr <- colnames(data_curr)[stri_detect_fixed(colnames(data_curr),
                                                       'inc')]
  
  avg_past <- get_avg_data(data_past, income_past)
  avg_curr <- get_avg_data(data_curr, income_curr)
  
  merged <- left_join(avg_past, avg_curr,
                      by = c('pid'),
                      suffix = c('_past', '_curr'))
  
  return (merged)
}


