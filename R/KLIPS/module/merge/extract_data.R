#### ==== extract_child_list ====
#' Extract list of child for study
#' 
#' This function will extract the list of children that will be under study
#' 
#' @param data individual level KLIPS data
#' @param child_min_age minimum age of children to include in the base year
#' @param child_max_age maximum age of children to include in the base year
#' @param base_year base year to refer to child's age
#' 
#' @return tibble of list of children's PID
extract_child_list <- function(data, child_min_age, child_max_age,
                               child_base_year) {
  assert_that(is.numeric(child_min_age) & is.numeric(child_max_age),
              msg = "[ERROR] Invalid child minimum and/or maximum age.")
  assert_that(is.numeric(child_base_year),
              msg = "[ERROR] child_base_year is not numeric")
  
  list_child <- data %>%
    filter(year == child_base_year & !is.na(orghid98) &
             age %in% child_min_age:child_max_age) %>%
    pull(pid)
  
  return(list_child)
}

#### === extract_child_data ====
#' Extracts children's data with average labor income
#'
#' This function will extract data of children using the list provided by extract_child_list() function. This function will yield children's data including average labor income from child_min_year to child_max_year. Moreover, it extracts demographic information such as gender, age, education level, current address, and employment status, type, and whether they are regular workers or not. This is based on the lastest information they have provided within the given years.
#' 
#' @param data individual level KLIPS data
#' @param list_child list of children provided using extract_child_list() function.
#' 
#' @return tibble of children data
extract_child_data <- function(data, age_bandwidth,
                               child_base_year,
                               list_child, dir_path) {
  assert_that(is.numeric(child_base_year))
  assert_that(is.numeric(age_bandwidth))
  
  base_years <- (child_base_year-age_bandwidth):(child_base_year+age_bandwidth)
  # === 1. Filter the data using the provided parameters ===
  data_child <- data %>%
    filter(pid %in% list_child & year %in% base_years)
  
  # === 2. Returns output ===
  return(data_child)
}

#### ==== set_child_base ====
#'
#'
#'
#'
set_child_base <- function(data_child,
                           child_base_age, age_bandwidth) {
  
  base_year_set <- data_child %>%
    select(year, pid, age) %>%
    mutate(base_year = year - age + child_base_age) %>%
    select(base_year, pid) %>%
    rename(pid_child = pid) %>%
    unique()
  
  child_base_data <- map_dfr(-age_bandwidth:age_bandwidth,
                             \(.x) {
                               base_cumulative <- base_year_set %>%
                                 mutate(base_year = base_year + .x)
                               return(base_cumulative)
                             })
  
  return(child_base_data)
}

#### ==== extract_parent_data ====
#' Extract parent's data
#'
#' This function will extract data of parent using the list provided by extract_child_list() and track_parent_child() function. This function will yield parent data including average labor income from child_min_year to child_max_year. Moreover, it extracts demographic information such as gender, age, education level, current address, and employment status, type, and whether they are regular workers or not. This is based on the latest information they have provided within the given years.
#'
#' @param data individual level KLIPS data
#' @param parent_max_age maximum age of parents to include
#' @param list_child list of children, output by extract_child_list()
#' @param relation_key mapping of parent-child relation, output by track_parent_child()
#' 
#' @return tibble of parent data
extract_parent_data <- function(data,
                                parent_min_age, parent_max_age,
                                key_parent_child, dir_path) {
  assert_that(is.numeric(parent_max_age) & is.numeric(parent_min_age),
              msg = "[ERROR] parent_max_age and/or parent_min_age type is not numeric")
  
  # === 1. Filter the data using the provided parameters ===
  data_parent <- data %>%
    filter(
        pid %in% key_parent_child$pid &
        age <= parent_max_age & age >= parent_min_age
    )
  
  # === 2. Returns output ===
  return (data_parent)
}


#### ==== extract_latest ====
#' Extracts latest information in the given data
#' 
#' 
#' 
extract_latest <- function(data) {
  result <- data %>%
    arrange(desc(year)) %>%
    group_by(pid) %>%
    slice_head(n = 1) %>%
    ungroup() %>%
    select(-year)
  
  return (result)
}