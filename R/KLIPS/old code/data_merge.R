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
# - Extracts the children and parent data
# - Merges the data into one data set for analysis
#
# Notes:
# - 현재 개인수준 노동소득으로만 IGE 모형을 돌렸을 때, R^2가 매우 낮은 수준
#   + R^2가 0.01 ~ 0.04 수준으로 나옴 (전체 소득의 경우, 0.2 이상)
# - 이에 따라 가구소득 기준으로 진행할 필요 있음.
#   + 그러나 노동패널의 경우, 가구 전체 소득만을 기록함
#   + 최근 독립을 하지 않는 청년층이 많기에, 해당 현상은 bias의 원인이 될 것.
#   + 이를 해결하는 방법을 찾아봐야할 수도 있음.
#
# Dependencies:
# - tidyverse
# - All files must be stored in "Data" folder.
# - data_ind.rdata, data_house.rdata, data_miss.rdata are required.
#   + data_ind.rdata: Individual-level KLIPS data
#   + data_house.rdata: Household-level KLIPS data
#   + data_miss.rdata: Supplementary file for data_ind.rdata
#     * Contains area of birth and age 14, and parental education level
#   + These data are produced in Data Cleaning code (data_clean.R).
#
# Output Flow:
# - Individual level:
#   + data_ind: KLIPS individual-level data
#   + data_relation: parent-child key for all samples
#   + parent_child_key: parent-child key for samples of interest
#   + data_merge_ind: parent-child merged individual data
# - Household level:
#   + data_house.rdata: KLIPS household-level data
#   + data_merge_house: Planned
# - Merged data:
#   + data_merge: data_merge_ind + data_merge_house
#
# - Next step: Planned
#
############################################################################## #

#### ==== 0. Setting ====
library(dplyr)
library(tibble)
library(purrr)
library(stringi)
library(data.table)
library(here)
library(assertthat)
library(haven)

source(here('R/module/merge/relation_key.R'), local = environment())
source(here('R/module/merge/merge_ind.R'), local = environment())
source(here('R/module/merge/merge_house.R'), local = environment())
source(here('R/module/merge/merge_two.R'), local = environment())
source(here('R/module/merge/get_rank.R'), local = environment())
#### ==== merge_data ====
#' Merges individual and household data after averaging the relevant information
#'
#' This function will derive merged data set for analysis. Using the given parameters, it will extract the individual and household data set that meets the condition set by the parameters. Then, it will average each information, which will be merged after.
#' 
#' @param start_wave wave number of first survey to include
#' @param end_wave wave number of last survey to include
#' @param parent_max_age upper bound of parent's age
#' @param child_min_year minimum year of children data
#' @param child_max_year maximum year of children data
#' @param parent_min_year minimum year of parent data
#' @param parent_max_year maximum year of parent data
#' @param dir_path directory to store the merged output (data/merged)
#' 
#' @return tibble with both individual and household information, parent and children information set side by side
merge_data <- function(start_wave, end_wave,
                       child_base_age, age_bandwidth,
                       child_min_age, child_max_age,
                       parent_min_age, parent_max_age,
                       child_base_year, dir_path) {
  #### === 0 Sanity check & input validation ===
  assert_that(is.numeric(start_wave) & is.numeric(end_wave))
  if (!all(c(start_wave, end_wave) %in% 1:26)) stop ("Survey wave out of bound")
  
  on.exit(gc())
  #### === 1 Extract parent child relationship ===
  message("[INFO] Deriving parent-child mapping")
  relation_key <- track_parent_child(start_wave, end_wave)
  
  #### === 2 Merge individual level data ===
  message("[INFO] Merging individual level data")
  data_merge_ind <- merge_ind(child_base_age, age_bandwidth,
                              child_min_age, child_max_age,
                              parent_min_age, parent_max_age,
                              child_base_year,
                              relation_key, dir_path)
  
  #### === 3. Merge house level data ===
  message("[INFO] Merging household level data")
  data_merge_house <- merge_house(child_base_age, age_bandwidth,
                                  parent_min_age, parent_max_age,
                                  child_base_year,
                                  dir_path)
  
  #### === 4. Merge individual and household level data ===
  message("[INFO] Merging household and individual level data")
  data_merge <- merge_two(data_merge_ind, data_merge_house, dir_path)
  
  #### === 5. Merge rank data ===
  income_var <- 'eq_income'
  calc_rank_all(start_wave, end_wave, income_var, dir_path)
  income_var <- 'eq_income_labor'
  calc_rank_all(start_wave, end_wave, income_var, dir_path)
  
}

