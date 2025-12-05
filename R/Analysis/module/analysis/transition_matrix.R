library(dplyr)
library(tidyr)
library(readr)
library(tibble)
library(rlang)
library(purrr)
library(stringi)
library(data.table)
library(here)
library(assertthat)
library(haven)
library(quantreg)
library(np)
library(ggplot2)
library(patchwork)
library(xtable)

source(here::here('R/Analysis/module/analysis/get_transition_mat.R'),
       local = environment())

base_list <- create_basis_data_trans()

klips_trans_mat_list <- get_trans_mat_list('klips', alpha = 0.5, min_sample_size = 0)
psid_trans_mat_list  <- get_trans_mat_list('psid', alpha = 0.5, min_sample_size = 0)

klips_parent_dist <- get_parent_dist(base_list[['klips']])
klips_parent_dist_5 <- get_parent_dist(base_list[['klips']], inc_5 = TRUE)
psid_parent_dist <- get_parent_dist(base_list[['psid']])
psid_parent_dist_5 <- get_parent_dist(base_list[['psid']], inc_5 = TRUE)

klips_mat_regular_3 <- klips_trans_mat_list$Regular$`A-A`$`Transition matrix`$smooth_trans_data
klips_mat_5_3 <- klips_trans_mat_list$`Income 5`$`A-A`$`Transition matrix`$smooth_trans_data
psid_mat_regular_3 <- psid_trans_mat_list$Regular$`A-A`$`Transition matrix`$smooth_trans_data
psid_mat_5_3 <- psid_trans_mat_list$`Income 5`$`A-A`$`Transition matrix`$smooth_trans_data

# Heatmaps
klips_heat_r_3 <- get_heat_map(klips_mat_regular_3, prob = 'trans_prob')
klips_heat_5_3 <- get_heat_map(klips_mat_5_3, prob = 'trans_prob', inc_5 = TRUE)
psid_heat_r_3 <- get_heat_map(psid_mat_regular_3, prob = 'trans_prob')
psid_heat_5_3 <- get_heat_map(psid_mat_5_3, prob = 'trans_prob', inc_5 = TRUE)

klips_heat_r_3_alt_occ <- get_heat_map(klips_mat_regular_3, prob = 'trans_prob', alt = 'occ')
psid_heat_r_3_alt_occ <- get_heat_map(psid_mat_regular_3, prob = 'trans_prob', alt = 'occ')
klips_heat_5_3_alt_occ <- get_heat_map(klips_mat_5_3, prob = 'trans_prob', inc_5 = TRUE, alt = 'occ')
psid_heat_5_3_alt_occ <- get_heat_map(psid_mat_5_3, prob = 'trans_prob', inc_5 = TRUE, alt = 'occ')
klips_heat_r_3_alt_educ <- get_heat_map(klips_mat_regular_3, prob = 'trans_prob', alt = 'educ')
psid_heat_r_3_alt_educ <- get_heat_map(psid_mat_regular_3, prob = 'trans_prob', alt = 'educ')
klips_heat_5_3_alt_educ <- get_heat_map(klips_mat_5_3, prob = 'trans_prob', inc_5 = TRUE, alt = 'educ')
psid_heat_5_3_alt_educ <- get_heat_map(psid_mat_5_3, prob = 'trans_prob', inc_5 = TRUE, alt = 'educ')

klips_heat_r_3_alt_inc <- get_heat_map(klips_mat_regular_3, prob = 'trans_prob', alt = 'inc_2')
klips_heat_5_3_alt_inc <- get_heat_map(klips_mat_5_3, prob = 'trans_prob', inc_5 = TRUE, alt = 'inc_2')
psid_heat_r_3_alt_inc <- get_heat_map(psid_mat_regular_3, prob = 'trans_prob', alt = 'inc_2')
psid_heat_5_3_alt_inc <- get_heat_map(psid_mat_5_3, prob = 'trans_prob', inc_5 = TRUE, alt = 'inc_2')
klips_heat_r_3_alt_occ_2 <- get_heat_map(klips_mat_regular_3, prob = 'trans_prob', alt = 'occ_2')
psid_heat_r_3_alt_occ_2 <- get_heat_map(psid_mat_regular_3, prob = 'trans_prob', alt = 'occ_2')
klips_heat_5_3_alt_occ_2 <- get_heat_map(klips_mat_5_3, prob = 'trans_prob', inc_5 = TRUE, alt = 'occ_2')
psid_heat_5_3_alt_occ_2 <- get_heat_map(psid_mat_5_3, prob = 'trans_prob', inc_5 = TRUE, alt = 'occ_2')
klips_heat_r_3_alt_educ_2 <- get_heat_map(klips_mat_regular_3, prob = 'trans_prob', alt = 'educ_2')
psid_heat_r_3_alt_educ_2 <- get_heat_map(psid_mat_regular_3, prob = 'trans_prob', alt = 'educ_2')
klips_heat_5_3_alt_educ_2 <- get_heat_map(klips_mat_5_3, prob = 'trans_prob', inc_5 = TRUE, alt = 'educ_2')
psid_heat_5_3_alt_educ_2 <- get_heat_map(psid_mat_5_3, prob = 'trans_prob', inc_5 = TRUE, alt = 'educ_2')

klips_heat_r_3 + psid_heat_r_3 + windows(20, 11)
klips_heat_r_3_alt_occ + psid_heat_r_3_alt_occ + windows(20, 11)
klips_heat_r_3_alt_educ + psid_heat_r_3_alt_educ + windows(20, 11)


klips_heat_5_3 + psid_heat_5_3 + windows(20, 11)
klips_heat_5_3_alt_occ + psid_heat_5_3_alt_occ + windows(20, 11)
klips_heat_5_3_alt_educ + psid_heat_5_3_alt_educ + windows(20, 11)

klips_heat_5_3_alt_inc + psid_heat_5_3_alt_inc + windows(20, 11)
klips_heat_5_3_alt_occ_2 + psid_heat_5_3_alt_occ_2 + windows(20, 11)
klips_heat_5_3_alt_educ_2 + psid_heat_5_3_alt_educ_2 + windows(20, 11)

ggsave('KLIPS heatmap.jpeg',
       plot = klips_heat_5_3_alt_inc,
       device = 'jpeg',
       path = 'image/',
       width = 13, height = 14,
       units = 'cm',
       dpi = 600)
ggsave('PSID heatmap.jpeg',
       plot = psid_heat_5_3_alt_inc,
       device = 'jpeg',
       path = 'image/',
       width = 13, height = 14,
       units = 'cm',
       dpi = 600)
ggsave('KLIPS heatmap alternative order.jpeg',
       plot = klips_heat_5_3_alt_occ_2,
       device = 'jpeg',
       path = 'image/',
       width = 13, height = 14,
       units = 'cm',
       dpi = 600)
ggsave('PSID heatmap alternative order.jpeg',
       plot = psid_heat_5_3_alt_occ_2,
       device = 'jpeg',
       path = 'image/',
       width = 13, height = 14,
       units = 'cm',
       dpi = 600)
ggsave('KLIPS heatmap alternative order educ.jpeg',
       plot = klips_heat_5_3_alt_educ_2,
       device = 'jpeg',
       path = 'image/',
       width = 13, height = 14,
       units = 'cm',
       dpi = 600)
ggsave('PSID heatmap alternative order educ.jpeg',
       plot = psid_heat_5_3_alt_educ_2,
       device = 'jpeg',
       path = 'image/',
       width = 13, height = 14,
       units = 'cm',
       dpi = 600)

klips_inc_heat <- get_inc_heat_map(base_list[['klips']])
psid_inc_heat <- get_inc_heat_map(base_list[['psid']])
ggsave('KLIPS income heatmap.jpeg',
       plot = klips_inc_heat,
       device = 'jpeg',
       path = 'image/',
       width = 13, height = 14,
       units = 'cm',
       dpi = 600)
ggsave('PSID income heatmap.jpeg',
       plot = psid_inc_heat,
       device = 'jpeg',
       path = 'image/',
       width = 13, height = 14,
       units = 'cm',
       dpi = 600)

klips_occ_heat <- get_occ_heat_map(base_list[['klips']])
psid_occ_heat <- get_occ_heat_map(base_list[['psid']])
ggsave('KLIPS occupation heatmap.jpeg',
       plot = klips_occ_heat,
       device = 'jpeg',
       path = 'image/',
       width = 13, height = 14,
       units = 'cm',
       dpi = 600)
ggsave('PSID occupation heatmap.jpeg',
       plot = psid_occ_heat,
       device = 'jpeg',
       path = 'image/',
       width = 13, height = 14,
       units = 'cm',
       dpi = 600)

# Upward, downward mobility

klips_mobility_plot <- get_mobility_plot(klips_mat_5_3, inc_5 = TRUE, alt = 'inc_2')
psid_mobility_plot <- get_mobility_plot(psid_mat_5_3, inc_5 = TRUE, alt = 'inc_2')

ggsave('KLIPS upward mobility.jpeg',
       plot = klips_mobility_plot[[1]],
       device = 'jpeg',
       path = 'image/',
       width = 13, height = 14,
       units = 'cm',
       dpi = 600)
ggsave('KLIPS downward mobility.jpeg',
       plot = klips_mobility_plot[[2]],
       device = 'jpeg',
       path = 'image/',
       width = 13, height = 14,
       units = 'cm',
       dpi = 600)
ggsave('PSID upward mobility.jpeg',
       plot = psid_mobility_plot[[1]],
       device = 'jpeg',
       path = 'image/',
       width = 13, height = 14,
       units = 'cm',
       dpi = 600)
ggsave('PSID downward mobility.jpeg',
       plot = psid_mobility_plot[[2]],
       device = 'jpeg',
       path = 'image/',
       width = 13, height = 14,
       units = 'cm',
       dpi = 600)

klips_mobility_plot[[1]] + psid_mobility_plot[[1]] + windows(20, 11)

# Measurement of mobility
klips_measures <- get_measures(trans_data = klips_mat_regular_3,
                               parent_dist = klips_parent_dist)
psid_measures <- get_measures(trans_data = psid_mat_regular_3,
                              parent_dist = psid_parent_dist)
klips_measures_5 <- get_measures(trans_data = klips_mat_5_3,
                                 parent_dist = klips_parent_dist_5)
psid_measures_5 <- get_measures(trans_data = psid_mat_5_3,
                                parent_dist = psid_parent_dist_5)
measure_table <- get_measure_table(klips_measures, psid_measures)
measure_table_5 <- get_measure_table(klips_measures_5, psid_measures_5)

klips_inc_occ_edu_data <- get_inc_occ_edu_data(base_list[['klips']])
psid_inc_occ_edu_data <- get_inc_occ_edu_data(base_list[['psid']])
klips_inc_measures <- get_measures(trans_data = klips_inc_occ_edu_data[[1]],
                                   trans_mat = klips_inc_occ_edu_data[[2]],
                                   parent_dist = klips_inc_occ_edu_data[[3]],
                                   marginals = TRUE)
klips_inc_5_measures <- get_measures(trans_data = klips_inc_occ_edu_data[[4]],
                                     trans_mat = klips_inc_occ_edu_data[[5]],
                                     parent_dist = klips_inc_occ_edu_data[[6]],
                                     marginals = TRUE)
klips_occ_measures <- get_measures(trans_data = klips_inc_occ_edu_data[[7]],
                                   trans_mat = klips_inc_occ_edu_data[[8]],
                                   parent_dist = klips_inc_occ_edu_data[[9]],
                                   marginals = TRUE)
klips_edu_measures <- get_measures(trans_data = klips_inc_occ_edu_data[[10]],
                                   trans_mat = klips_inc_occ_edu_data[[11]],
                                   parent_dist = klips_inc_occ_edu_data[[12]],
                                   marginals = TRUE)
psid_inc_measures <- get_measures(trans_data = psid_inc_occ_edu_data[[1]],
                                   trans_mat = psid_inc_occ_edu_data[[2]],
                                   parent_dist = psid_inc_occ_edu_data[[3]],
                                   marginals = TRUE)
psid_inc_5_measures <- get_measures(trans_data = psid_inc_occ_edu_data[[4]],
                                     trans_mat = psid_inc_occ_edu_data[[5]],
                                     parent_dist = psid_inc_occ_edu_data[[6]],
                                     marginals = TRUE)
psid_occ_measures <- get_measures(trans_data = psid_inc_occ_edu_data[[7]],
                                   trans_mat = psid_inc_occ_edu_data[[8]],
                                   parent_dist = psid_inc_occ_edu_data[[9]],
                                   marginals = TRUE)
psid_edu_measures <- get_measures(trans_data = psid_inc_occ_edu_data[[10]],
                                  trans_mat = psid_inc_occ_edu_data[[11]],
                                  parent_dist = psid_inc_occ_edu_data[[12]],
                                  marginals = TRUE)
measure_table_inc <- get_measure_table(klips_inc_measures, psid_inc_measures)
measure_table_inc_5 <- get_measure_table(klips_inc_5_measures, psid_inc_5_measures)
measure_table_occ <- get_measure_table(klips_occ_measures, psid_occ_measures)
measure_table_edu <- get_measure_table(klips_edu_measures, psid_edu_measures)

measure_table_all <- bind_rows(
  measure_table_edu %>% mutate(Mode = 'Education', Income = ""),
  measure_table_occ %>% mutate(Mode = "Occupation", Income = ""),
  measure_table_inc %>% mutate(Mode = "Income", Income = "Three"),
  measure_table_inc_5 %>% mutate(Mode = "Income", Income = "Five"),
  measure_table %>% mutate(Mode = "Full", Income = "Three"),
  measure_table_5 %>% mutate(Mode = "Full", Income = "Five")) %>%
  select(Data, Mode, Income, everything()) %>%
  slice(c(1, 3, 5, 7, 9, 11, 2, 4, 6, 8, 10, 12))
measure_table_latex <- xtable(measure_table_all)
print(measure_table_latex, booktabs = TRUE, include.rownames = FALSE)



klips_mat_regular_sample <- klips_trans_mat_list$Regular$`N-N`$`Transition matrix`$sample_data
klips_mat_regular_1 <- klips_trans_mat_list$Regular$`N-N`$`Transition matrix`$smooth_trans_data
klips_mat_regular_2 <- klips_trans_mat_list$Regular$`N-A`$`Transition matrix`$smooth_trans_data
klips_mat_regular_3 <- klips_trans_mat_list$Regular$`A-A`$`Transition matrix`$smooth_trans_data

klips_mat_5_sample <- klips_trans_mat_list$`Income 5`$`N-N`$`Transition matrix`$sample_data
klips_mat_5_1 <- klips_trans_mat_list$`Income 5`$`N-N`$`Transition matrix`$smooth_trans_data
klips_mat_5_2 <- klips_trans_mat_list$`Income 5`$`N-A`$`Transition matrix`$smooth_trans_data
klips_mat_5_3 <- klips_trans_mat_list$`Income 5`$`A-A`$`Transition matrix`$smooth_trans_data


psid_mat_regular_sample <- psid_trans_mat_list$Regular$`N-N`$`Transition matrix`$sample_data
psid_mat_regular_1 <- psid_trans_mat_list$Regular$`N-N`$`Transition matrix`$smooth_trans_data
psid_mat_regular_2 <- psid_trans_mat_list$Regular$`N-A`$`Transition matrix`$smooth_trans_data
psid_mat_regular_3 <- psid_trans_mat_list$Regular$`A-A`$`Transition matrix`$smooth_trans_data

psid_mat_5_sample <- psid_trans_mat_list$`Income 5`$`N-N`$`Transition matrix`$sample_data
psid_mat_5_1 <- psid_trans_mat_list$`Income 5`$`N-N`$`Transition matrix`$smooth_trans_data
psid_mat_5_2 <- psid_trans_mat_list$`Income 5`$`N-A`$`Transition matrix`$smooth_trans_data
psid_mat_5_3 <- psid_trans_mat_list$`Income 5`$`A-A`$`Transition matrix`$smooth_trans_data






klips_heat_sample <- get_heat_map(klips_mat_regular_sample,
                                  prob = 'cond_prob_sample')
klips_heat_sample_5 <- get_heat_map(klips_mat_5_sample,
                                    prob = 'cond_prob_sample', inc_5 = TRUE)
klips_heat_sample + klips_heat_sample_5 + windows(20, 11)

klips_heat_r_1 <- get_heat_map(klips_mat_regular_1, prob = 'trans_prob')
klips_heat_r_2 <- get_heat_map(klips_mat_regular_2, prob = 'trans_prob')
klips_heat_r_3 <- get_heat_map(klips_mat_regular_3, prob = 'trans_prob')
klips_heat_r_1 + klips_heat_r_2 + klips_heat_r_3 + windows(30, 11)

klips_heat_5_1 <- get_heat_map(klips_mat_5_1, prob = 'trans_prob', inc_5 = TRUE)
klips_heat_5_2 <- get_heat_map(klips_mat_5_2, prob = 'trans_prob', inc_5 = TRUE)
klips_heat_5_3 <- get_heat_map(klips_mat_5_3, prob = 'trans_prob', inc_5 = TRUE)
klips_heat_sample_5 + klips_heat_5_1 + klips_heat_5_2 + klips_heat_5_3 + windows(30, 11)




psid_heat_sample <- get_heat_map(psid_mat_regular_sample,
                                  prob = 'cond_prob_sample')
psid_heat_sample_5 <- get_heat_map(psid_mat_5_sample,
                                    prob = 'cond_prob_sample', inc_5 = TRUE)
psid_heat_sample + psid_heat_sample_5 + windows(20, 11)

psid_heat_r_1 <- get_heat_map(psid_mat_regular_1, prob = 'trans_prob')
psid_heat_r_2 <- get_heat_map(psid_mat_regular_2, prob = 'trans_prob')
psid_heat_r_3 <- get_heat_map(psid_mat_regular_3, prob = 'trans_prob')
psid_heat_r_1 + psid_heat_r_2 + psid_heat_r_3 + windows(30, 11)

psid_heat_5_1 <- get_heat_map(psid_mat_5_1, prob = 'trans_prob', inc_5 = TRUE)
psid_heat_5_2 <- get_heat_map(psid_mat_5_2, prob = 'trans_prob', inc_5 = TRUE)
psid_heat_5_3 <- get_heat_map(psid_mat_5_3, prob = 'trans_prob', inc_5 = TRUE)
psid_heat_5_1 + psid_heat_5_2 + psid_heat_5_3 + windows(30, 11)

klips_heat_r_3 + psid_heat_r_3 + windows(20, 11)
klips_heat_5_3 + psid_heat_5_3 + windows(20, 11)

klips_heat_r_3_alt_occ <- get_heat_map(klips_mat_regular_3, prob = 'trans_prob', alt = 'occ')
psid_heat_r_3_alt_occ <- get_heat_map(psid_mat_regular_3, prob = 'trans_prob', alt = 'occ')
klips_heat_r_3_alt_occ + psid_heat_r_3_alt_occ + windows(20, 11)

klips_heat_r_3_alt_educ <- get_heat_map(klips_mat_regular_3, prob = 'trans_prob', alt = 'educ')
psid_heat_r_3_alt_educ <- get_heat_map(psid_mat_regular_3, prob = 'trans_prob', alt = 'educ')
klips_heat_r_3_alt_educ + psid_heat_r_3_alt_educ + windows(20, 11)





psid_heat_sample <- get_heat_map(psid_trans[[1]][[1]],
                                 prob = 'cond_prob_sample')
psid_heat <- get_heat_map(psid_trans[[2]][[2]], prob = 'trans_prob')

psid_heat_sample_5 <- get_heat_map(psid_trans_5[[1]][[1]],
                                 prob = 'cond_prob_sample', inc_5 = TRUE)
psid_heat_5 <- get_heat_map(psid_trans_5[[2]][[2]], prob = 'trans_prob',
                            inc_5 = TRUE)

klips_heat_sample + klips_heat + windows(20, 11)
klips_heat_sample_5 + klips_heat_5 + windows(20, 11)

psid_heat_sample + psid_heat + windows(20, 11)
psid_heat_sample_5 + psid_heat_5 + windows(20, 11)

klips_heat + psid_heat + windows(20, 9)





klips_inc_heat + psid_inc_heat + windows(20, 9)
klips_occ_heat + psid_occ_heat + windows(20, 9)
klips_heat_5_3 + psid_heat_5_3 + windows(20, 9)

klips_memory <- get_memory_curve(klips_measures)
psid_memory <- get_memory_curve(psid_measures)
klips_memory + psid_memory + windows(20, 9)

klips_memory_5 <- get_memory_curve(klips_measures_5)
psid_memory_5 <- get_memory_curve(psid_measures_5)
klips_memory_5 + psid_memory_5 + windows(20, 9)

klips_memory_inc <- get_memory_curve(klips_inc_measures)
klips_memory_inc_5 <- get_memory_curve(klips_inc_5_measures)
klips_memory_occ <- get_memory_curve(klips_occ_measures)
psid_memory_inc <- get_memory_curve(psid_inc_measures)
psid_memory_inc_5 <- get_memory_curve(psid_inc_5_measures)
psid_memory_occ <- get_memory_curve(psid_occ_measures)

klips_memory_inc + psid_memory_inc + windows(20, 9)
klips_memory_inc_5 + psid_memory_inc_5 + windows(20, 9)
klips_memory_occ + psid_memory_occ + windows(20, 9)
