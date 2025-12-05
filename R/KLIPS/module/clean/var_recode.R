

#### ==== check_commmon ====
#' Checks common errors
#' 
#' This function is internal function used within various recoding functions
#' 
#' @param data KLIPS data
#' @param col column name
check_common <- function(data, col) {
  assert_that(is_tibble(data))
  if(!is.character(col)) stop('[ERROR] Invalid column name ', col,
                              ' : must be character string')
  if(!all(is.numeric(data[[col]]))) {
    message('[WARNING] Some values have been coerced into NA')
  }
}

#### ==== code_area ####
#' Codes area information
#'
#' This function codes variable with area information
#' 
#' @param data KLIPS data
#' @param col column name of area
#' 
#' @return coded vector
code_area <- function(data, col) {
  check_common(data, col)
  
  area <- data[[col]]
  area <- fcase(
    area == 1, 'Seoul',
    area == 2, 'Busan',
    area == 3, 'Daegu',
    area == 4, 'Daejeon',
    area == 5, 'Incheon',
    area == 6, 'Gwangju',
    area == 7, 'Ulsan',
    area == 8, 'Gyeonggi',
    area == 9, 'Gangwon',
    area == 10, 'Chungbuk',
    area == 11, 'Chungnam',
    area == 12, 'Jeonbuk',
    area == 13, 'Jeonnam',
    area == 14, 'Gyeongbuk',
    area == 15, 'Gyeongnam',
    area == 16, 'Jeju',
    area == 17, 'North',
    area == 18, 'Foreign',
    area == 19, 'Sejong'
  )
  
  return(area)
}

#### ==== code_area_2 ####
#' Codes area information with different coding
#'
#' This function codes variable with area information.
#' However, it is coded into different cases
#'  - Capital Province: Seoul and Gyeonggi province
#'  - Metropolitan City: Busan, Daegu, Daejoen, Incheon, Gwangju, Ulsan
#'  - Non-metropolitan area: Gangwon, Chungbuk/nam, Jeonbuk/nam, Gyeonbuk/nam, Jeju
#'  - North
#'  - Foreign country
#'  - Sejong
#' 
#' @param data KLIPS data
#' @param col column name of area
#' 
#' @return coded vector
code_area_2 <- function(data, col) {
  check_common(data, col)
  
  area <- data[[col]]
  area <- fcase(
    area %in% c(1, 8), 'Capital Province',
    area %in% 2:7, 'Metropolitan City',
    area %in% 9:16, 'Non-metropolitan area',
    default = area
  )
  
  return(area)
  
}

#### ==== code_residency_type ====
#' Codes type of residency
#' 
#' This function codes residency type into 4 categories
#' - Owned by the respondent
#' - Rent (Jeonsae)
#' - Rent (Monthly)
#' - Other
#' 
#' @param data KLIPS data
#' @param col column name of type of residency
#' 
#' @return coded vector
code_residency_type <- function(data, col) {
  check_common(data, col)
  
  type <- data[[col]]
  type <- fcase(
    type == 1, 'Owned',
    type == 2, 'Rent (Jeonsae)',
    type == 3, 'Rent (Monthly)',
    type == 4, 'Other'
  )
  
  return(type)
}

#### ==== code_residency_house ====
#' Codes housing type of residency
#' 
#' This function will code the housing type of residency
#' - Detached House
#' - Apartment
#' - Multi-unit House
#' - Commercial Housing
#' - Others
#' 
#' @param data KLIPS data
#' @param col column name of type of housing
#' 
#' @return coded vector
code_residency_house <- function(data, col) {
  check_common(data, col)
  
  house <- data[[col]]
  house <- fcase(
    house == 1, 'Detached House',
    house == 2, 'Apartment',
    house == 3, 'Multi-unit House',
    house == 4, 'Commercial Housing',
    house == 5, 'Others'
  )
  
  return(house)
}

#### ==== code_interaction ====
#' Codes frequency of interaction with parents
#' 
#' This function codes the frequency of interaction with parents
#'
#' @param data KLIPS data
#' @param col column name of interaction with parents
#'
#' @return coded vector
code_interaction <- function(data, col) {
  check_common(data, col)
  
  parent_int <- data[[col]]
  parent_int <- fcase(
    parent_int == 1, 'Visited often',
    parent_int == 2, 'Visited seldomly',
    parent_int == 3, 'Visited few times'
  )
  
  return(parent_int)
}

#### ==== code_sup ====
#' Codes whether the respondents have received economic support from their parents
#'
#' This function codes whether the respondents have received any economic support from their parents
#'
#' @param data KLIPS data
#' @param col column name of support from parents
#'
#' @return coded vector
code_sup <- function(data, col) {
  check_common(data, col)
  
  parent_sup <- data[[col]]
  parent_sup <- fcase(
    parent_sup == 1, 'Received support',
    parent_sup == 2, 'No support'
  )
  
  return(parent_sup)
}

#### ==== code_sex ====
#' Codes gender of respondents
#'
#' This function codes gender of respondents
#'
#' @param data KLIPS data
#' @param col column name of gender
#'
#' @return coded vector
code_sex <- function(data, col) {
  check_common(data, col)
  
  sex <- data[[col]]
  sex <- ifelse(sex == 1, 'Male', 'Female')
  
  return(sex)
}

#### ==== code_fam_rel ====
#' Codes the respondent's relationship with the head of household
#' 
#' This function codes the respondent's relationship with the head of household
#'
#' @param data KLIPS data
#' @param col column name of relationship
#'
#' @return coded vector
code_fam_rel <- function(data, col) {
  check_common(data, col)
  
  fam_rel <- data[[col]]
  fam_rel <- fcase(
    fam_rel %in% 1:2, "Grandparent of Head",
    fam_rel %in% 3:4, "Grandparent of Head's Spouse",
    fam_rel %in% 5:6, "Parent of Head",
    fam_rel %in% 7:8, "Parent of Head's Spouse",
    fam_rel %in% 10, "Head",
    fam_rel %in% 11:19, "Children",
    fam_rel %in% 20, "Spouse",
    fam_rel %in% 21:29, "Children's Spouse",
    fam_rel %in% 31:39, "Head's Sibling",
    fam_rel %in% 41:49, "Spouse's Sibling",
    fam_rel %in% 51:59, "Spouse of Head's Sibling",
    fam_rel %in% 61:69, "Spouse of Spouse's Sibling",
    fam_rel %in% 111:199, "Grandchildren",
    fam_rel %in% 211:299, "Grandchidren's Spouse",
    fam_rel %in% 311:399, 'Nephew',
    fam_rel %in% 411:499, "Nephew's Spouse",
    fam_rel == 997, "Other Relative",
    fam_rel == 998, "Cohabitant",
    fam_rel %in% 1111:1999, "Grand-grand children"
  )
  
  return(fam_rel)
}

#### ==== calc_log_inc ====
#' Calculates log of income variables
#' 
#' @param data KLIPS data
#' @param col column name of income
#' 
#' @return log of income
calc_log_inc <- function (data, col) {
  inc <- data[[col]]
  inc <- ifelse(inc < 0, NA_real_, inc)
  inc <- ifelse(inc == 0, 1, inc)
  
  log_inc <- log(inc)
  
  return (log_inc)
}

#### ==== code_educ ====
#' Codes education level
#'
#' This function will code education level of the respondents
#' @param data KLIPS data
#' @param col column name of education level
#' 
#' @return coded vector
code_educ <- function (data, col) {
  check_common(data, col)
  
  educ <- data[[col]]
  educ <- fcase(
    educ %in% 1:5, 'High school or below',
    educ %in% 6, 'College (2 year)',
    educ %in% 7, 'University (4 year)',
    educ %in% 8:9, 'Graduate'
  )
  
  return(educ)
}

#### ==== code_educ_2 ====
#' Codes education level
#'
#' This function will code education level of the respondents
#' @param data KLIPS data
#' @param col column name of education level
#' 
#' @return coded vector
code_educ_2 <- function (data, col) {
  check_common(data, col)
  
  educ <- data[[col]]
  educ <- fcase(
    educ %in% 1, 'College (2 year)',
    educ %in% 2, 'University (4 year)',
    educ %in% 3:4, 'Graduate'
  )
  
  return(educ)
}

#### ==== code_educ_stat ====
#' Codes current status of education
#'
#' This function will code education level of the respondents
#' @param data KLIPS data
#' @param col column name of education level
#' 
#' @return coded vector
code_educ_stat <- function (data, col) {
  check_common(data, col)
  
  educ <- data[[col]]
  educ <- fcase(
    educ %in% 1, 'Graduated',
    educ %in% 2, 'Completed',
    educ %in% 3, 'Drop Out',
    educ %in% 4, 'Enrolled',
    educ %in% 5, 'Gap Year'
  )
  
  return(educ)
}

#### ==== code_major ====
#' Codes area of study
#'
#' This function will code area of study of the respondents
#' @param data KLIPS data
#' @param col column name of area of study
#' 
#' @return coded vector
code_major <- function (data, col) {
  check_common(data, col)
  
  major <- data[[col]]
  major <- fcase(
    major %in% c(1, 2, 6:10), 'Non-STEM',
    major %in% 3:4, 'STEM',
    major %in% 5, 'Health'
  )
  
  return(major)
}


#### ==== create_major_map ====
#' Creates map of area of study, between wave 1 to 3
#'
create_major_map <- function() {
  if (file.exists('data/KLIPS/external/code_map.rds')) {
    msg <- '[INFO] Code mapping for area of study already exists.'
    return(msg)
  }
  
  text <- readLines(here::here('data/KLIPS/external/subject_new.sas'))
  text <- text[15:545]
  
  pattern <- 'P01462\\s+=\\s+(\\d*)\\s+then\\s+sub01\\s+=\\s+(\\d*)'
  code_recode <- stringr::str_match(text, pattern)
  
  code_table <- tibble(code = code_recode[,2], recode = code_recode[,3]) %>%
    mutate_all(as.numeric) %>%
    mutate(
      stem = fcase(
        recode %in% c(10000:39999, 70000:79999), 'Non-STEM',
        recode %in% c(40000:59999), 'STEM',
        recode %in% c(60000:69999), 'Health'
      )
    )
  
  code_map <- setNames(code_table$stem, code_table$code)
  
  saveRDS(code_map, file = here::here('data/KLIPS/external/code_map.rds'))
}


#### ==== code_educ_past ====
#' Codes area of study
#'
#' This function will code area of study of the respondents
#' @param data KLIPS data
#' @param col column name of area of study
#' 
#' @return coded vector
code_major_past <- function (data, col) {
  check_common(data, col)
  
  create_major_map()
  
  code_map <- readRDS('data/KLIPS/external/code_map.rds')
  
  major <- data[[col]]
  major <- recode(as.character(major), !!!code_map, .default = NA_character_)
  
  return(major)
}

#### ==== code_grad ====
#' Codes area of study
#'
#' This function will code area of study of the respondents
#' @param data KLIPS data
#' @param col column name of area of study
#' 
#' @return coded vector
code_grad <- function (data, col) {
  check_common(data, col)
  
  grad <- data[[col]]
  grad <- fcase(
    grad == 1, 'Graduated',
    grad == 2, 'Enrolled',
    grad == 3, 'Drop Out'
  )
  
  return(grad)
}

#### ==== code_employ_stat ====
#' Codes employment status of respondents
#' 
#' This function will code the employment status of respondents
#' 
#' @param data KLIPS data
#' @param col column name of employment status
#'
#' @return coded vector
code_employ_stat <- function(data, col) {
  check_common(data, col)
  
  employ_stat <- data[[col]]
  employ_stat <- ifelse(employ_stat == 1, 'Employed', 'Unemployed')
  
  return(employ_stat)
}

#### ==== code_employ_type ====
#' Codes type of employment
#' 
#' This function will code the type of employment
#' 
#' @param data KLIPS data
#' @param col column name of employment type
#' 
#' @return coded vector
code_employ_type <- function(data, col) {
  check_common(data, col)
  
  employ_type <- data[[col]]
  employ_type <- fcase(
    employ_type == 1, 'Employed',
    employ_type == 2, 'Self-employed',
    employ_type == 3, 'Family Worker'
  )
  
  return(employ_type)
}

#### ==== code_employ_position ====
#' Codes type of employment
#' 
#' This function will code the type of employment
#' 
#' @param data KLIPS data
#' @param col column name of employment type
#' 
#' @return coded vector
code_employ_position <- function(data, col) {
  check_common(data, col)
  
  employ_position <- data[[col]]
  employ_position <- fcase(
    employ_position == 1, 'Full-time',
    employ_position == 2, 'Temporary',
    employ_position == 3, 'Daily',
    employ_position == 4, 'Self-employment',
    employ_position == 5, 'Family worker'
  )
  
  return(employ_position)
}

#### ==== code_occ ====
#' Codes occupation
#' 
#' This function will code the occupation
#' 
#' @param data KLIPS data
#' @param col column name of occupation
#' 
#' @return coded vector
code_occ <- function(data, col, type) {
  check_common(data, col)
  
  occupation <- data[[col]]
  occupation <- fcase(
    occupation %in% 1:299, "High",
    occupation %in% c(300:599, 700:899), 'Middle',
    occupation %in% c(600:699), 'Farming',
    occupation %in% c(920) & type == 5, 'Farming',
    occupation %in% c(991) & type == 6, 'Farming',
    occupation %in% c(991) & type == 7, 'Farming',
    occupation %in% 980:983, 'Middle',
    occupation %in% 900:999, 'Low'
  )
  
  return(occupation)
}

#### ==== code_occ_2 ====
#' Codes occupation
#' 
#' This function will code the occupation
#' 
#' @param data KLIPS data
#' @param col column name of occupation
#' 
#' @return coded vector
code_occ_2 <- function(data, col) {
  check_common(data, col)
  
  occupation <- data[[col]]
  occupation <- fcase(
    occupation %in% 1:299, "High",
    occupation %in% 300:899, 'Middle',
    occupation %in% 980:983, 'Middle',
    occupation %in% 900:999, 'Low'
  )
  
  return(occupation)
}