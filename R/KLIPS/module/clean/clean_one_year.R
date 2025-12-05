# Modules
source(
  here('R', 'KLIPS', 'module', 'clean', 'var_select_rename.R'), 
  local = environment()
  )
source(
  here('R', 'KLIPS', 'module', 'clean', 'var_recode.R'), 
  local = environment()
  )
source(
  here('R', 'KLIPS', 'module', 'clean', 'aggregate_income.R'), 
  local = environment()
  )

#### ==== load_KLIPS ====
#'
#'
#'
load_KLIPS <- function (file_path, survey_wave, level) {
  
  obs_level <- if(level == 'household') 'h' else 'p'
  
  data_name <- paste0('klips', survey_wave, obs_level, '.dta')
  data_path <- here::here(file_path, data_name)
  
  assert_that(file.exists(data_path),
              msg = paste0("[ERROR] File ", data_name,
                           " not found in ", file_path)
              )
  
  data_klips <- read_stata(data_path)
  
  message("[INFO] Loading successful for wave ", survey_wave)
  return(data_klips)
}


#### ==== clean_KLIPS ====
#'
#'

clean_one_year <- function(file_path, wave, level) {
  
  #### ==== 0. Input Validation and set up ====
  assert_that(file.exists(file_path),
              msg = paste0("[ERROR] File directory ", file_path," not found.")
              )
  if(!(wave %in% 1:26 & is.numeric(wave))) {
    stop("Invalid wave number: wave number should be numeric between 1 and 26")
  }
  if(!(level %in% c('household', 'individual', 'supplementary'))) {
    stop(
    "Invalid level:
    level should be either 'household', 'individual', or 'supplementary"
    )
  }
  
  if(wave %% 4) on.exit(gc())
  
  survey_wave <- ifelse(stri_length(wave) == 2,
                        as.character(wave), paste0(0, wave))
  survey_year <- 1997 + wave
  message("[INFO] Cleaning data:\n year: ", survey_year,
          ', wave: ', wave,
          ', level: ', level)
  
  #### ==== 1. Load appropriate data set ====
  data <- load_KLIPS(file_path, survey_wave, level)
  
  #### ==== 2. Selects and renames variables ====
  data <- select_klips_var(data, level)
  data <- rename_klips_var(data, level, survey_year)
  
  #### ==== 3. Codes variables ====
  if(level == 'household') {
    
    data[['address']] <- code_area(data, 'address')
    data[['residency_type']] <- code_residency_type(data, 'residency_type')
    data[['residency_house']] <- code_residency_house(data, 'residency_house')
    
    # Parental interaction/support only recorded after 3rd wave
    if(wave > 3) {
      data[['parent_int_h']] <- code_interaction(data, 'parent_int_h')
      data[['parent_int_s']] <- code_interaction(data, 'parent_int_s')
      data[['parent_sup_h']] <- code_sup(data, 'parent_sup_h')
      data[['parent_sup_s']] <- code_sup(data, 'parent_sup_s')
    }
    
    data <- aggregate_income(data, wave)
    
    data$num_fam <- ifelse(is.na(data$num_fam), 0, data$num_fam)
    data$eq_income <- with(data, eq_inc(income, num_fam))
    data$eq_income_2 <- with(data, eq_inc(income_2, num_fam))
    data$eq_income_labor <- with(data, eq_inc(income_labor, num_fam))
    
    data[['log_income']] <- calc_log_inc(data, 'income')
    data[['log_income_labor']] <- calc_log_inc(data, 'income_labor')
    data[['log_eq_income']] <- calc_log_inc(data, 'eq_income')
    data[['log_eq_income_labor']] <- calc_log_inc(data, 'eq_income_labor')
    data[['log_income_2']] <- calc_log_inc(data, 'income_2')
    data[['log_eq_income_2']] <- calc_log_inc(data, 'eq_income_2')
    
    
  } else if (level == 'individual') {
    
    data[['sex']] <- code_sex(data, 'sex')
    data[['fam_rel_code']] <- data[['fam_rel']]
    data[['fam_rel']] <- code_fam_rel(data, 'fam_rel')
    data[['educ_self']] <- code_educ(data, 'educ_self')
    data[['educ_stat']] <- code_educ_stat(data, 'educ_stat')
    data[['place_curr']] <- code_area(data, 'place_curr')
    data[['employ_stat']] <- code_employ_stat(data, 'employ_stat')
    data[['employ_type']] <- code_employ_type(data, 'employ_type')
    data[['employ_position']] <- code_employ_position(data, 'employ_position')
    
    if(!('educ_degree_curr' %in% colnames(data))){
      data[['educ_degree_curr']] <- NA_character_
    } else {
      data[['educ_degree_curr']] <- code_educ_2(data, 'educ_degree_curr')
    }
    if(!('educ_major_curr' %in% colnames(data))){
      data[['educ_major_curr']] <- NA_character_
    } else {
      data[['educ_major_curr']] <- code_major(data, 'educ_major_curr')
    }
    
    if(!('employ_job_5' %in% colnames(data))){
      data[['employ_job_5_alt']] <- NA
    } else {
      data[['employ_job_5_alt']] <- code_occ_2(data, 'employ_job_5')
    }
    
    if(!('employ_job_6' %in% colnames(data))){
      data[['employ_job_6_alt']] <- NA
      data[['employ_job_7_alt']] <- NA
    } else {
      data[['employ_job_6_alt']] <- code_occ_2(data, 'employ_job_6')
      data[['employ_job_7_alt']] <- code_occ_2(data, 'employ_job_7')
    }
    
    if(!('employ_job_5' %in% colnames(data))){
      data[['employ_job_5']] <- NA
    } else {
      data[['employ_job_5']] <- code_occ(data, 'employ_job_5', 5)
    }
    
    if(!('employ_job_6' %in% colnames(data))){
      data[['employ_job_6']] <- NA
      data[['employ_job_7']] <- NA
    } else {
      data[['employ_job_6']] <- code_occ(data, 'employ_job_6', 6)
      data[['employ_job_7']] <- code_occ(data, 'employ_job_7', 7)
    }
    
    # In some of the wave, employ_regular is not measured
    if(!('employ_regular' %in% colnames(data))){
      data[['employ_regular']] <- NA
    }
    data$employ_regular <- ifelse(data$employ_regular == 1,
                                  'Regular', 'Non-regular')
    
  } else {
    data[['place_birth']] <- code_area(data, 'place_birth')
    data[['place_14']] <- code_area(data, 'place_14')
    data[['educ_father']] <- code_educ(data, 'educ_father')
    data[['educ_mother']] <- if(wave > 3) code_educ(data, 'educ_mother') else NA
    
    major_vars_a4 <- c('educ_major_past_1_a4',
                       'educ_major_past_2_a4',
                       'educ_major_past_3_a4',
                       'educ_major_past_4_a4')
    major_vars_b3 <- c('educ_major_past_1_b3',
                       'educ_major_past_2_b3',
                       'educ_major_past_3_b3',
                       'educ_major_past_4_b3')
    grad_vars <- c('educ_grad_past_1',
                   'educ_grad_past_2',
                   'educ_grad_past_3',
                   'educ_grad_past_4')
    grad_year_vars <- c('educ_grad_year_past_1',
                        'educ_grad_year_past_2',
                        'educ_grad_year_past_3',
                        'educ_grad_year_past_4')
    
    vars_select <- c('educ_mother',
                     major_vars_a4, major_vars_b3,
                     grad_vars, grad_year_vars)
    
    for (col in major_vars_a4) {
      if(!(col %in% colnames(data))){
        data[[col]] <- NA_character_
      } else {
        data[[col]] <- code_major(data, col)
      }
    }
    for (col in major_vars_b3) {
      if(!(col %in% colnames(data))){
        data[[col]] <- NA_character_
      } else {
        data[[col]] <- code_major_past(data, col)
      }
    }
    for (col in grad_vars) {
      if(!(col %in% colnames(data))){
        data[[col]] <- NA_character_
      } else {
        data[[col]] <- code_grad(data, col)
      }
    }
    for (col in grad_year_vars) {
      if(!(col %in% colnames(data))){
        data[[col]] <- NA_real_
      }
    }
    
    if (wave <= 3) {
      data <- filter(data, !(is.na(place_birth) & is.na(place_14) &
                               is.na(educ_father) &
                               is.na(educ_major_past_1_b3) & 
                               is.na(educ_major_past_2_b3) & 
                               is.na(educ_major_past_3_b3) & 
                               is.na(educ_major_past_4_b3)))
    } else if (wave <= 4) {
      data <- filter(data, !(is.na(place_birth) & is.na(place_14) &
                               is.na(educ_father) & is.na(educ_mother) &
                               is.na(educ_major_past_1_a4) & 
                               is.na(educ_major_past_2_a4) & 
                               is.na(educ_major_past_3_a4) & 
                               is.na(educ_major_past_4_a4)))
    } else {
      data <- filter(data, !(is.na(place_birth) & is.na(place_14) &
                               is.na(educ_father) & is.na(educ_mother) &
                               is.na(educ_major_past_1_a4) & 
                               is.na(educ_major_past_2_a4) & 
                               is.na(educ_major_past_3_a4) & 
                               is.na(educ_major_past_4_a4)))
    }
    
    data <- select(data, pid, place_birth, place_14, educ_father,
                   any_of(vars_select))
  }
  
  #### ==== 4. Return the data ====
  data$year <- survey_year
  data <- data %>% select(year, everything())
  
  return(data)
}
