recode_sex <- function (col) {
  result <- fcase(
    col == 1, 'Male',
    col == 2, 'Female'
  )
  
  return (result)
}

recode_area_5 <- function (col) {
  result <- fcase(
    col == 1, 'Seoul',
    col == 2, 'Metropolitan City',
    col == 3, 'City (Si)',
    col == 4, 'County (Gun)',
    col == 5, 'Urban-rural complex'
  )
  
  return (result)
}

recode_area_7 <- function (col) {
  result <- fcase(
    col == 1, 'Seoul',
    col == 2, 'Capital Area',
    col == 3, 'Busan/Ulsan/Gyeongnam',
    col == 4, 'Daegu/Gyeongbuk',
    col == 5, 'Daejeon/Chungnam/Saejong',
    col == 6, 'Gangwon/Chungbuk',
    col == 7, 'Gwangju/Jeonnam/Jeonbuk/Jaeju'
  )
  
  return (result)
}

recode_rel_head <- function (col) {
  result <- fcase(
    col %in% 1:2, "Parent of Head",
    col %in% 3:4, "Parent of Head's Spouse",
    col %in% 5:6, "Greatparent of Head",
    col %in% 7:8, "Greatparent of Head's Spouse",
    col %in% 10, "Head",
    col %in% 11:19, "Children",
    col %in% 20, "Spouse",
    col %in% 21:29, "Children's Spouse",
    col %in% 31:39, "Head's Sibling",
    col %in% 41:49, "Spouse's Sibling",
    col %in% 51:59, "Spouse of Head's Sibling",
    col %in% 61:69, "Spouse of Spouse's Sibling",
    col %in% 111:199, "Grandchildren",
    col %in% 211:299, "Grandchidren's Spouse",
    col == 997, "Other Relative",
    col == 998, "Cohabitant"
  )
  
  return (result)
}

recode_educ <- function (col) {
  result <- fcase(
    col %in% 1:5, 'High School or Below',
    col %in% 6:8, 'Above College'
  )
  
  return (result)
}

recode_econ_act <- function (col) {
  result <- fcase(
    col %in% 1:4, 'Employed',
    col %in% 5:7, 'Self-employed',
    col %in% 8, 'Unemployed',
    col %in% 9, 'Inactive'
  )
  
  return (result)
}

recode_occ <- function (col) {
  result <- fcase(
    col %in% 1:299, 'High',
    col %in% 300:899, 'Middle',
    col %in% 900:999, 'Low',
    col %in% 1000:1013, 'Middle'
  )
  
  return (result)
}


recode_educ_2 <- function (col) {
  result <- fcase(
    col %in% 1:2, 'High School or Below',
    col %in% 3:5, 'Above College'
  )
  
  return (result)
}

recode_educ_major <- function (col) {
  result <- fcase(
    col %in% c(1:5, 11), 'Non-STEM',
    col %in% 6:7, 'STEM',
    col %in% 8:10, 'Health',
    col == 12, 'Other'
  )
  
  return (result)
}