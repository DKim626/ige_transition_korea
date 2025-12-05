create_coded_data <- function (psid_all, codelong, var_maps) {
  
  coded_list <- c('rel_head', 'sex', 'sex_rp', 'sex_sp',
                  'race',
                  'race_rp_1', 'race_rp_2', 'race_rp_3', 'race_rp_4',
                  'race_sp_1', 'race_sp_2', 'race_sp_3', 'race_sp_4',
                  'educ_bach_major_1', 'educ_bach_major_2',
                  'educ_bach_major_1_digit', 'educ_bach_major_2_digit',
                  'educ_highest',
                  'educ_highest_major_1', 'educ_highest_major_2',
                  'educ_bach_major_1_rp', 'educ_bach_major_2_rp',
                  'educ_bach_major_1_update_rp',
                  'educ_bach_major_2_update_rp',
                  'educ_bach_major_1_sp', 'educ_bach_major_2_sp',
                  'educ_bach_major_1_update_sp',
                  'educ_bach_major_2_update_sp',
                  'educ_highest_major_rp_1', 'educ_highest_major_rp_2',
                  'educ_highest_major_update_rp_1',
                  'educ_highest_major_update_rp_2',
                  'educ_highest_major_sp_1', 'educ_highest_major_sp_2',
                  'educ_highest_major_update_sp_1',
                  'educ_highest_major_update_sp_2',
                  'educ_highest_major_1_digit', 'educ_highest_major_2_digit',
                  "educ_hs_rp", "educ_hs_sp",
                  'employ_stat',
                  'ind_rp', 'ind_sp',
                  'occ_rp', 'occ_sp')
  
  code_var <- function (data, col, .codelong) {
    x <- data.table(
      n = seq_len(nrow(data)),
      value = as.numeric(data[[col]])
    )
    x[, start := value][, end := value]
    x <- x[!is.na(value), ]
    setkey(x, start, end)
    
    
    .codelong <- codelong %>%
      filter(varname == col) %>%
      select(varname, value_label, val_min, val_max) %>%
      as.data.table()
    
    setkey(.codelong, val_min, val_max)
    
    result <- foverlaps(x, .codelong, nomatch = NA_integer_)
    
    output <- rep(NA_character_, nrow(data))
    output[result$n] <- result$value_label
    
    return (output)
  }
  
  
  year_data <- list()
  
  for (year in as.character(1990:2023)) {
    year_list <- var_maps %>%
      filter(year == !!year | year == 'all')
    
    if (any(!(year_list$year %in% 'all'))) {
      name_list <- year_list$code
      names(name_list) <- year_list$varname
      
      data <- psid_all %>%
        select(year_list$code) %>%
        mutate(year = !!year)
      
      recode_list <- year_list %>%
        filter(varname %in% coded_list)
      
      for (var in recode_list$code) {
        data[var] <- code_var(data, var)
      }
      
      year_data[[year]] <- data %>%
        rename(any_of(name_list)) %>%
        filter(seq_num != 0)

    }
  }
  
  data_all <- bind_rows(year_data) %>%
    select(year, everything()) %>%
    mutate(
      pid = int_id * 1000 + pid,
      pid_f = int_id * 1000 + pid_f,
      pid_m = int_id * 1000 + pid_m,
    ) %>%
    select(-int_id)
  
  return (data_all)
}

recode_na <- function (var) {
  var <- fcase(
    stri_detect_fixed(var, 'Inap.'), NA_character_,
    stri_detect_fixed(var, 'NA'), NA_character_,
    default = var
  )
}

recode_na_educ <- function (var) {
  var <- fcase(
    var == 99, NA_real_,
    default = var
  )
}

recode_major <- function (var) {
  var <- fcase(
    var %in% c(
      # Major
      "Aerospace", "Agriculture", "Biochemistry",
      "Biological science (genetics)", "Biology",
      "Biomedical engineering",
      "Chemical engineering", "Chemistry", "Civil engineering",
      "Computer programming", "Computer science/ engineering",
      "Computer technology",
      "Drafting", "Electrical engineering", "Engineering",
      "Environmental engineering", "Industrial engineering",
      "Information systems", 'Marine biology',
      "Math", "Mathematical and computer sciences (n.e.c.)",
      'Mathematical and computer sciences (n.e.c)',
      "Mechanical engineering",
      "Meteorology", "Natural sciences", "Nuclear engineering",
      "Physics", "Technicians and related support majors (n.e.c.)",
      "Wildlife conservation/ environment/ ecology",
      "Zoology/ animal science",
      
      # Digits
      "Agriculture, Agriculture Operations, and Related Sciences",
      "Architecture and Related Services",
      "Biological and Biomedical Sciences",
      "Computer and Information Sciences and Support Services",
      "Engineering",
      "Engineering Technologies and Engineering-Related Fields",
      "Mathematics and Statistics",
      "Natural Resources and Conservation",
      "Physical Sciences",
      "Science Technologies/Technicians",
      
      # Reference & Spouse
      'Military Technologies and Applied Sciences',
      # Economics
      'Economics'), 'STEM',
    var %in% c(
      # Major
      "Dental hygiene", "Emergency medical technician (EMT)",
      "Health assessment and treating majors (n.e.c.)",
      "Health diagnosing majors (n.e.c.)",
      "Licensed practical nurse (LPN)",
      "Medical science", "Nursing", "Pharmacy",
      "Physical therapy/ kinesiology", "Pre-dentistry",
      "Pre-medical/ medicine", "Pre-veterinarian",
      "Public/ community health", "Speech therapy",
      "Sports medicine",
      # Digits
      "Health Professions and Related Programs",
      "Health-Related Knowledge and Skills",
      "Residency Programs"), 'Health',
    var %in% c(
      # Major
      'Accounting', 'Acting',
      'Administrative support majors (n.e.c.)',
      'Air conditioning and heating', 'Airplane pilots',
      'Anthropology', 'Architecture', 'Art/ graphic design',
      'Broadcast journalism', 'Business', 'Business financing',
      'Business management (MBA)',
      'Chef', 'Communication', 'Computer art/web design', 
      'Criminal justice', 'Criminology', 'Dancing', 
      'Education- primary', 'Education- secondary', 'Electrician', 
      'Engineering/ architecture / surveying (n.e.c.)', 
      'English/ literature', 'Ethnic studies/ Asian studies', 
      'Executive/ administrative and managerial majors (n.e.c.)', 
      'Fashion design', 'Film making/ directing', 'Firefighter', 
      'Foreign language', 'Geography', 'Hairdresser/ cosmetology', 
      'History', 'International business/ relations',
      'Journalism', 'Law (n.e.c.)', 'Marketing/advertising',
      'Mechanic', 'Mechanical and repair majors (n.e.c.)', 'Music',
      'Music theory',
      'Music as a performing art', 'Other', 'Paralegal', 'Philosophy', 
      'Photography', 'Police officer', 'Political science', 
      'Pre-law/ law', 'Professional specialty majors (n.e.c.)', 
      'Protective service (n.e.c.)', 'Psychology', 
      'Public administration/ policy', 'Real estate', 'Religion', 
      'Religious (n.e.c.)', 'Secretarial', 'Service majors (n.e.c.)', 
      'Social science (n.e.c.)', 'Social work', 'Sociology', 
      'Sports management', 'Teaching (n.e.c.)', 'Travel agent', 
      'Truck driving', 'Tool and die',
      'Writing', 'Writing/ art/ entertaining/ humanities (n.e.c.)',
      # Digits
      # "Area, Ethnic, Cultural, Gender, and Group Studies",
      "Basic Skills and Developmental/Remedial Education",
      "Business, Management, Marketing, and Related Support Services",
      "Communication, Journalism, and Related Programs",
      "Communications Technologies/Technicians and Support Services",
      "Construction Trades",
      "Education",
      "English Language and Literature/Letters",
      "Family and Consumer Sciences/Human Sciences",
      "Foreign Languages, Literatures, and Linguistics",
      "High School/Secondary Diplomas and Certificates",
      "History",
      "Homeland Security, Law Enforcement, Firefighting and Related Protective Services",
      "Interpersonal and Social Skills",
      "Legal Professions and Studies",
      "Leisure and Recreational Activities",
      "Liberal Arts and Sciences, General Studies and Humanities",
      "Library Science",
      "Mechanic and Repair Technologies/Technicians",
      "Military Science, Leadership and Operational Art",
      "Multi/Interdisciplinary Studies",
      "Parks, Recreation, Leisure, and Fitness Studies",
      "Personal and Culinary Services",
      "Philosophy and Religious Studies",
      "Precision Production",
      "Psychology",
      "Public Administration and Social Service Professions",
      "Social Sciences",
      "Theology and Religious Vocations",
      "Transportation and Materials Moving",
      "Visual and Performing Arts",
      
      # Bachelor
      'Business law', 'Transportation majors (n.e.c.)',
      
      # Reference person & spouse
      'Area, Ethnic, Cultural, Gender, and Group Studies'), "Non-STEM",
    default = var
  )
}

recode_major_digit <- function (var) {
  var <- fcase(
    var %in% c("Agriculture, Agriculture Operations, and Related Sciences",
               "Architecture and Related Services",
               "Biological and Biomedical Sciences",
               "Computer and Information Sciences and Support Services",
               "Engineering",
               "Engineering Technologies and Engineering-Related Fields",
               "Mathematics and Statistics",
               "Natural Resources and Conservation",
               "Physical Sciences",
               "Science Technologies/Technicians"), "STEM",
    var %in% c("Health Professions and Related Programs",
               "Health-Related Knowledge and Skills",
               "Residency Programs"), "Health",
    var %in% c("Area, Ethnic, Cultural, Gender, and Group Studies",
               "Basic Skills and Developmental/Remedial Education",
               "Business, Management, Marketing, and Related Support Services",
               "Communication, Journalism, and Related Programs",
               "Communications Technologies/Technicians and Support Services",
               "Construction Trades",
               "Education",
               "English Language and Literature/Letters",
               "Family and Consumer Sciences/Human Sciences",
               "Foreign Languages, Literatures, and Linguistics",
               "High School/Secondary Diplomas and Certificates",
               "History",
               "Homeland Security, Law Enforcement, Firefighting and Related Protective Services",
               "Interpersonal and Social Skills",
               "Legal Professions and Studies",
               "Leisure and Recreational Activities",
               "Liberal Arts and Sciences, General Studies and Humanities",
               "Library Science",
               "Mechanic and Repair Technologies/Technicians",
               "Military Science, Leadership and Operational Art",
               "Multi/Interdisciplinary Studies",
               "Parks, Recreation, Leisure, and Fitness Studies",
               "Personal and Culinary Services",
               "Philosophy and Religious Studies",
               "Precision Production",
               "Psychology",
               "Public Administration and Social Service Professions",
               "Social Sciences",
               "Theology and Religious Vocations",
               "Transportation and Materials Moving",
               "Visual and Performing Arts"), "Non-STEM"
  )
}


recode_occ <- function (var) {
  var <- fcase(
    var %in% c("Architecture and Engineering Occupations",
               "Arts, Design, Entertainment, Sports, and Media Occupations",
               "Business and Financial Operations Occupations",
               "Business Operations Specialists",
               "Community and Social Services Occupations",
               "Computer and Mathematical Occupations",
               "Education, Training, and Library Occupations",
               "Financial Specialists",
               "Healthcare Practitioners and Technical Occupations",
               "Legal Occupations",
               "Life, Physical, and Social Science Occupations",
               "Management Occupations",
               "Managers and Administrators, except Farm",
               "Managers and Administrators, Except Farm",
               "Professional, Technical, and Kindred Workers"), 'High',
    var %in% c("Clerical and Kindred Workers",
               "Construction and Extraction Occupations",
               "Construction Trades",
               "Craftsman and Kindred Workers",
               "Craftsmen and Kindred Workers",
               "Food Preparation and Serving Occupations",
               "Food Preparation and Serving Related Occupations",
               "Healthcare Support Occupations",
               "Installation, Maintenance, and Repair Occupations",
               "Installation, Maintenance, and Repair Workers",
               "Military Specific Occupations",
               "Office and Administrative Support Occupations",
               "Operatives, except Transport",
               "Operatives, Except Transport",
               "Personal Care and Service Occupations",
               "Production Occupations",
               "Protective Service Occupations",
               "Sales and Related Occupations",
               "Sales Occupations",
               "Sales Workers",
               "Service Workers, except Private Household",
               "Service Workers, Except Private Household",
               "Transport Equipment Operatives",
               "Transportation and Material Moving Occupations"), 'Middle',
    var %in% c("Building and Grounds Cleaning and Maintenance Occupations",
               "Extraction Workers",
               "Laborers, except Farm",
               "Laborers, Except Farm",
               "Private Household Workers"), 'Low',
    var %in% c("Farm Laborers and Farm Foremen",
               "Farmers and Farm Managers",
               "Farming, Fishing, and Forestry Occupations"), 'Farming',
    default = var
  )
  
  return (var)
}

recode_occ_2 <- function (var) {
  var <- fcase(
    var %in% c("Architecture and Engineering Occupations",
               "Arts, Design, Entertainment, Sports, and Media Occupations",
               "Business and Financial Operations Occupations",
               "Business Operations Specialists",
               "Community and Social Services Occupations",
               "Computer and Mathematical Occupations",
               "Education, Training, and Library Occupations",
               "Financial Specialists",
               "Healthcare Practitioners and Technical Occupations",
               "Legal Occupations",
               "Life, Physical, and Social Science Occupations",
               "Management Occupations",
               "Managers and Administrators, except Farm",
               "Managers and Administrators, Except Farm",
               "Farmers and Farm Managers",
               "Professional, Technical, and Kindred Workers"), 'High',
    var %in% c("Clerical and Kindred Workers",
               "Construction and Extraction Occupations",
               "Construction Trades",
               "Craftsman and Kindred Workers",
               "Craftsmen and Kindred Workers",
               "Food Preparation and Serving Occupations",
               "Food Preparation and Serving Related Occupations",
               "Healthcare Support Occupations",
               "Installation, Maintenance, and Repair Occupations",
               "Installation, Maintenance, and Repair Workers",
               "Military Specific Occupations",
               "Office and Administrative Support Occupations",
               "Operatives, except Transport",
               "Operatives, Except Transport",
               "Personal Care and Service Occupations",
               "Production Occupations",
               "Protective Service Occupations",
               "Sales and Related Occupations",
               "Sales Occupations",
               "Sales Workers",
               "Service Workers, except Private Household",
               "Service Workers, Except Private Household",
               "Transport Equipment Operatives",
               "Transportation and Material Moving Occupations"), 'Middle',
    var %in% c("Building and Grounds Cleaning and Maintenance Occupations",
               "Extraction Workers",
               "Laborers, except Farm",
               "Laborers, Except Farm",
               "Farm Laborers and Farm Foremen",
               "Farming, Fishing, and Forestry Occupations",
               "Private Household Workers"), 'Low',
    default = var
  )
  
  return (var)
}

recode_race <- function (var) {
  var <- fcase(
    stri_detect_fixed(var, 'NA'), NA_character_,
    stri_detect_fixed(var, 'Native'), 'Native American',
    stri_detect_fixed(var, 'American Indian'), 'Native American',
    stri_detect_fixed(var, 'Black'), 'Black',
    stri_detect_fixed(var, 'White'), 'White',
    stri_detect_fixed(var, 'Asian'), 'Asian',
    stri_detect_fixed(var, 'Latino'), "Latino",
    stri_detect_fixed(var, 'color', case_insensitive = TRUE), 'Other',
    default = var
  )
  
  return (var)
}