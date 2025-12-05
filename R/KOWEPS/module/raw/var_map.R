
var_map_house <- tribble(
  ~code, ~varname,
  # Household info
  '_id', 'hhid',
  '_merkey', 'hh_key',
  '_reg5', 'area_5',
  '_reg7', 'area_7',
  
  # Household income info
  '_din', 'income_disposable',
  '_cin', 'income_current',
  
  # Personal ID info
  '_pid1', 'pid_1',
  '_pid2', 'pid_2',
  '_pid3', 'pid_3',
  '_pid4', 'pid_4',
  '_pid5', 'pid_5',
  '_pid6', 'pid_6',
  '_pid7', 'pid_7',
  '_pid8', 'pid_8',
  '_pid9', 'pid_9',
  
  # Number of household member
  '01_1', 'hmem_num', # 99 = NA
  
  # Demographic information on each household members
  ## 1st member
  '01_2', 'h_num_1',
  '01_3', 'rel_head_1', # 999 = NA, 998 = Cohabitant, 997 = Other relative
  '01_4', 'sex_1', # 9 = NA
  '01_5', 'year_birth_1', # 9999 = NA
  '01_6', 'educ_1', # 99 = NA
  ## 2nd member
  '01_14', 'h_num_2',
  '01_15', 'rel_head_2', # 999 = NA, 998 = Cohabitant, 997 = Other relative
  '01_16', 'sex_2', # 9 = NA
  '01_17', 'year_birth_2', # 9999 = NA
  '01_18', 'educ_2', # 99 = NA
  ## 3rd member
  '01_26', 'h_num_3',
  '01_27', 'rel_head_3', # 999 = NA, 998 = Cohabitant, 997 = Other relative
  '01_28', 'sex_3', # 9 = NA
  '01_29', 'year_birth_3', # 9999 = NA
  '01_30', 'educ_3', # 99 = NA
  ## 4th member
  '01_38', 'h_num_4',
  '01_39', 'rel_head_4', # 999 = NA, 998 = Cohabitant, 997 = Other relative
  '01_40', 'sex_4', # 9 = NA
  '01_41', 'year_birth_4', # 9999 = NA
  '01_42', 'educ_4', # 99 = NA
  ## 5th member
  '01_50', 'h_num_5',
  '01_51', 'rel_head_5', # 999 = NA, 998 = Cohabitant, 997 = Other relative
  '01_52', 'sex_5', # 9 = NA
  '01_53', 'year_birth_5', # 9999 = NA
  '01_54', 'educ_5', # 99 = NA
  ## 6th member
  '01_62', 'h_num_6',
  '01_63', 'rel_head_6', # 999 = NA, 998 = Cohabitant, 997 = Other relative
  '01_64', 'sex_6', # 9 = NA
  '01_65', 'year_birth_6', # 9999 = NA
  '01_66', 'educ_6', # 99 = NA
  ## 7th member
  '01_74', 'h_num_7',
  '01_75', 'rel_head_7', # 999 = NA, 998 = Cohabitant, 997 = Other relative
  '01_76', 'sex_7', # 9 = NA
  '01_77', 'year_birth_7', # 9999 = NA
  '01_78', 'educ_7', # 99 = NA
  ## 8th member
  '01_86', 'h_num_8',
  '01_87', 'rel_head_8', # 999 = NA, 998 = Cohabitant, 997 = Other relative
  '01_88', 'sex_8', # 9 = NA
  '01_89', 'year_birth_8', # 9999 = NA
  '01_90', 'educ_8', # 99 = NA
  ## 9th member
  '01_98', 'h_num_9',
  '01_99', 'rel_head_9', # 999 = NA, 998 = Cohabitant, 997 = Other relative
  '01_100', 'sex_9', # 9 = NA
  '01_101', 'year_birth_9', # 9999 = NA
  '01_102', 'educ_9', # 99 = NA
  
  # Economic activity of household member
  ## 1st member
  '03_4', 'econ_act_1', # NA = 0
  '03_7', 'ind_1', # NA = 999
  '03_8', 'occ_1', # NA = 9999
  ## 2nd member
  '03_14', 'econ_act_2', # NA = 0
  '03_17', 'ind_2', # NA = 999
  '03_18', 'occ_2', # NA = 9999
  ## 3rd member
  '03_24', 'econ_act_3', # NA = 0
  '03_27', 'ind_3', # NA = 999
  '03_28', 'occ_3', # NA = 9999
  ## 4th member
  '03_34', 'econ_act_4', # NA = 0
  '03_37', 'ind_4', # NA = 999
  '03_38', 'occ_4', # NA = 9999
  ## 5th member
  '03_44', 'econ_act_5', # NA = 0
  '03_47', 'ind_5', # NA = 999
  '03_48', 'occ_5', # NA = 9999
  ## 6th member
  '03_54', 'econ_act_6', # NA = 0
  '03_57', 'ind_6', # NA = 999
  '03_58', 'occ_6', # NA = 9999
  ## 7th member
  '03_64', 'econ_act_7', # NA = 0
  '03_67', 'ind_7', # NA = 999
  '03_68', 'occ_7', # NA = 9999
  ## 8th member
  '03_74', 'econ_act_8', # NA = 0
  '03_77', 'ind_8', # NA = 999
  '03_78', 'occ_8', # NA = 9999
  ## 9th member
  '03_84', 'econ_act_9', # NA = 0
  '03_87', 'ind_9', # NA = 999
  '03_88', 'occ_9', # NA = 9999
  
  # Weight
  '_ws', 'weight_sample_hh',
  '_wg', 'weight_general_hh',
  '_ws_all', 'weight_sample_hh_1',
  '_wg_all', 'weight_general_hh_1',
  '_ws_n_all', 'weight_sample_hh_2',
  '_wg_n_all', 'weight_general_hh_2',
)
var_map_ind <- tribble(
  ~code, ~varname,
  '_pid', 'pid',
  '07_3aq1', 'educ_level', # NA = 9
  '07_3aq5', 'educ_major', # NA = 99
  '_ws', 'weight_sample_ind',
  '_wg', 'weight_general_ind',
  '_ws_c', 'weight_sample_ind_c',
  '_ws_L', 'weight_sample_ind_l',
  '_wg_c', 'weight_general_ind_c',
  '_wg_L', 'weight_general_ind_l',
  '_ws_c_all', 'weight_sample_ind_c_1',
  '_ws_L_all', 'weight_sample_ind_l_1',
  '_wg_c_all', 'weight_general_ind_c_1',
  '_wg_L_all', 'weight_general_ind_l_1',
  '_ws_c_n_all', 'weight_sample_ind_c_2',
  '_ws_L_n_all', 'weight_sample_ind_l_2',
  '_wg_c_n_all', 'weight_general_ind_c_2',
  '_wg_L_n_all', 'weight_general_ind_l_2',
  
  '_ws_l', 'weight_sample_ind_l',
  '_wg_l', 'weight_general_ind_l',
  '_ws_l_all', 'weight_sample_ind_l_1',
  '_wg_l_all', 'weight_general_ind_l_1',
  '_ws_l_n_all', 'weight_sample_ind_l_2',
  '_wg_l_n_all', 'weight_general_ind_l_2',
)