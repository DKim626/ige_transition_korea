############################################################################## #
# Data Cleaning for Korean Labor and Income Panel Study(KLIPS)
#
# Author: Dongwook Kim (dongwook1995@uchicago.edu)
#
# Pipeline:
# 1. data_clean.R: Cleans individual & household level data into panel form
# 2. data_merge.R: Merges parent and child sample data
# 3. ige_reg.R: Provides IGE results
# 4. transition.R: Provides transition probabilities
# 5. visualization.R: Provides visualization of analysis results
#
# Purpose:
# - Load raw KLIPS .dta files (household & individual level)
# - Standardize variable names across 26 waves
# - Integrate income, asset, demographic variables into panel format
# - Select and merge the household and individual level data
# - Generate three RData outputs:
#   * data_house.rdata - household-level KLIPS data
#   * data_ind.rdata - individual-level KLIPS data
#   * data_miss.rdata - supplementary (parental edu & place of birth and age 14)
#
# Notes:
# - KLIPS survey spans 26 waves (1998 - 2023)
# - Early surveys have different variable definitions.
# - Some variables may not exist for some years, which are handled via 'try()'
# - NA values for income and asset are replaced with 0
#   + Official KLIPS guidebook suggests that these NA values are 0 by
#     construction of the survey.
#   + The questionnaire asks whether the individual has income/asset before
#     asking the value of each item.
#
# Dependencies:
# - tidyverse, haven, labelled
# - .dta files must be stored in "Data" folder
#
# Output Flow:
# - Household level:
#   + klips**h.dta -> data_house.rdata (Household-level KLIPS data)
# - Individual level:
#   + klips**p.dta -> data_ind.rdata (Individual-level KLIPS data)
#   + klips**p.dta -> data_miss.rdata (parental edu & place of birth and age 14)
#
# - Next Step: Parent-child merge (data_merge.R)
#   + data_house.rdata -> data_merge_house (household-level merge)
#   + data_ind.rdata -> data_merge_ind (individual-level merge)
#   + data_merge_house + data_ind.rdata -> final merged data
#
############################################################################## #

#### ==== 0. Setting ====
# Libraries
library(dplyr)
library(tibble)
library(purrr)
library(stringi)
library(data.table)
library(here)
library(assertthat)
library(haven)

# Modules
source(here::here('R', 'KLIPS', 'module', 'clean', 'clean_one_year.R'),
       local = environment())


#### ==== 1. Household Data ====
# Loop through all KLIPS survey years (from wave 1 to 26)
# survey_number is the 2-digit wave identifier (e.g. 01, 16, 23)
# survey_year is the actual calendar year = 1997 + wave number
# For each wave:
# - Load .dta file
# - Select & rename key variables
# - Integrates variables across survey years
# - Summarize incomes and assets
# - Append to cumulative tibble (data_house)
clean_klips_data <- function (file_path, dir_path,
                              start_wave, end_wave, level) {
  #### ==== 0. Check sanity and input validity ====
  assert_that(file.exists(here(file_path)))
  if (!is.numeric(start_wave) | !is.numeric(end_wave)) {
    stop("start_wave and end_wave must be numeric.")
  }
  if (!all((c(start_wave, end_wave) %in% 1:26))) {
    stop("start_wave and/or end_wave is out of range.")
  }
  if (!(level %in% c('household', 'individual', 'supplementary'))) {
    stop(
      "Invalid level:
    level should be either 'household', 'individual', or 'supplementary"
    )
  }
  
  #### ==== 1. Clean with safe_clean ====
  safe_clean <- safely(clean_one_year)
  
  results <- map(start_wave:end_wave,
                 ~ safe_clean(file_path, .x, level)
                 )
  
  names(results) <- start_wave:end_wave
  
  failure <- map(results, 'error') |> keep(~ !is.null(.x))
  
  if (length(failure) > 0) {
    message("[ERROR] Cleaning failed at following waves:")
    walk2(names(failure), failure,
          ~ message(' - ', .x, ': ', .y$message))
  }
  
  #### ==== 2. Return successful results ====
  result <- map_dfr(results, 'result')
  
  #### ==== 3. Supplementary only ====
  if (level == 'supplementary') {
    base <- select(result, pid) %>% unique()
    
    cols_supp <- colnames(result)[3:length(colnames(result))]
    
    result_supp_1 <- base
    for (col in cols_supp) {
      update_supp_1 <- result %>%
        select(year, pid, col) %>%
        filter(!is.na(.data[[col]])) %>%
        arrange(desc(year)) %>%
        group_by(pid) %>%
        slice_head(n = 1) %>%
        ungroup() %>%
        select(-year)
      
      result_supp_1 <- left_join(result_supp_1, update_supp_1, by = 'pid')
    }
    
    # result_supp_1 <- result_supp_1 %>%
    #   mutate(
    #     educ_major_past_a4 = fcase(
    #       !is.na(educ_major_past_4_a4), educ_major_past_4_a4,
    #       !is.na(educ_major_past_3_a4), educ_major_past_3_a4,
    #       !is.na(educ_major_past_2_a4), educ_major_past_2_a4,
    #       !is.na(educ_major_past_1_a4), educ_major_past_1_a4,
    #       default = NA_character_
    #     ),
    #     educ_major_past_b3 = fcase(
    #       !is.na(educ_major_past_4_b3), educ_major_past_4_b3,
    #       !is.na(educ_major_past_3_b3), educ_major_past_3_b3,
    #       !is.na(educ_major_past_2_b3), educ_major_past_2_b3,
    #       !is.na(educ_major_past_1_b3), educ_major_past_1_b3,
    #       default = NA_character_
    #     ),
    #     educ_major_past = fcase(
    #       !is.na(educ_major_past_a4), educ_major_past_a4,
    #       !is.na(educ_major_past_b3), educ_major_past_b3
    #     ),
    #     educ_grad_year_past = fcase(
    #       !is.na(educ_grad_year_past_4), educ_grad_year_past_4,
    #       !is.na(educ_grad_year_past_3), educ_grad_year_past_3,
    #       !is.na(educ_grad_year_past_2), educ_grad_year_past_2,
    #       !is.na(educ_grad_year_past_1), educ_grad_year_past_1,
    #       default = NA_real_
    #     )
    #   ) %>%
    #   select(pid,
    #          place_birth, place_14,
    #          educ_father, educ_mother,
    #          educ_major_past, educ_grad_year_past
    #          )

    result <- result_supp_1
  }

  #### ==== 4. Save the results to "data/KLIPS/clean/" ====
  clean_file_dir <- 'data/KLIPS/clean/clean_klips_'
  if (!dir.exists(here('data/KLIPS/clean'))) {
    dir.create(here('data/KLIPS/clean'))
    message('[INFO] Directory created: data/KLIPS/clean/')
  }
  
  lev <- fcase(
    level == 'household', 'house',
    level == 'individual', 'individual',
    level == 'supplementary', 'supp'
  )
  
  clean_path <- paste0(clean_file_dir, lev, '.rds')
  
  if (file.exists(clean_path)) {
    message('[INFO] Overwriting ', clean_path)
  } else {
    message('[INFO] Saving ', level, ' data at ', clean_path)
  }
  saveRDS(result, file = here(clean_path))
}


#### ==== 2. Individual Data ====



# Loop through all KLIPS survey years (from wave 1 to 26)
# survey_number is the 2-digit wave identifier (e.g. 01, 16, 23)
# survey_year is the actual calendar year = 1997 + wave number
# For each wave:
# - Load .dta file
# - Select & rename key variables
# - Integrates variables across survey years
# - Selects and recodes individual characteristics:
#   * Gender, age, education level, employment status/type,
#     industry and job codes, pre/post tax labor income
# - Append to cumulative tibble (data_ind)

  #### ==== 2.2. Rename & recode variables ====
  
  #### ==== 2.3. Handle wave-specific differences ====
 



#### ==== 3. Deriving Missing Data ====
# As some variables, such as parental education level, place of birth and at age
# of 14, are not recorded every year, I will derive the necessary variables here.



# Loop through all KLIPS survey years (from wave 1 to 26)
# survey_number is the 2-digit wave identifier (e.g. 01, 16, 23)
# survey_year is the actual calendar year = 1997 + wave number
# For each wave:
# - Load .dta file
# - Select & rename key variables
# - Integrates variables across survey years
# - Selects and recodes parental education, palce of birth and at age 14
# - Append to cumulative tibble (data_mis) for futher process


# As there are redundancies in the data, for example, some respondents have
# answered differently about their place of birth, where they grew up
# at age 14, and the education level of their parents, we will only consider the
# latest data possible.

# data_miss <- data_mis %>%
#   select(pid) %>%
#   unique()

# Loop through the variables of data_mis
# For each variables:
#   - Extracts the latest information available per individual only
#   - Merges with the full data set



