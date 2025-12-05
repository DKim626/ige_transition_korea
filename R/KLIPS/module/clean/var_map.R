#### ====  Mapping of standardized variable name ====
##### ==== Household level ====
mapping_house_var <- c(
  address = '0141', # Current Address
  num_fam = '0150', # Number of family members
  parent_int_h = '1103', # Frequency of Interaction with Parents (Head)
  parent_int_s = '1204', # Frequency of Interaction with Parents (Spouse)
  parent_sup_h = '1109', # Economic Support from Parents (Head)
  parent_sup_s = '1209', # Economic Support from Parents (Spouse)
  residency_type = '1406', # Type of Occupancy
  residency_house = '1407', # Type of Housing
  residency_area = '1410', # Housing Area
  residency_price = '1412', # Housing Price
  residency_deposit = '1413', # Amount of Deposit for Rent
  residency_rent = '1414', # Monthly Rent
  income_labor = '2102', # Labor Income
  income_fin_interest_i = '2112', # Financial income: Financial Institution
  income_fin_interest_n = '2113', # Financial income: Non-institution
  income_fin_stock_profit = '2114', # Financial income: Profit on Sale
  income_fin_stock_div = '2115', # Financial income: Diviend
  income_fin_other = '2116', # Financial income: Other
  income_ra_rent = '2122', # Real Estate Income: Rent
  income_ra_sale = '2123', # Real Estate Income: Profit on Sale
  income_ra_cult = '2124', # Real Estate Income: Rights to Cultivate
  income_ra_prem = '2125', # Real Estate Income: Premium
  income_ra_other = '2126', # Real Estate Income: Other
  income_si_1 = '2134', # Social Insurance Recipient 1
  income_si_2 = '2136', # Social Insurance Recipient 2
  income_si_3 = '2138', # Social Insurance Recipient 3
  income_si_4 = '2140', # Social Insurance Recipient 4
  income_si_5 = '2142', # Social Insurance Recipient 5
  income_t_basic = '2152', # Transfer Income: Basic Living Security
  income_t_gov = '2153', # Transfer Income: Other Governmental Support
  income_t_pen = '2153a', # Transfer Income: Basic Pension
  income_t_sup_s = '2155', # Transfer Income: Support from Civil Organization
  income_t_sup_r = '2156', # Transfer Income: Relative
  income_t_sup_p = '2157', # Transfer Income: Support from Parents
  income_t_sup_c = '2158', # Transfer Income: Support from Children
  income_t_sup_or = '2159', # Transfer Income: Other Relative
  income_t_sup_o = '2160', # Transfer Income: Other
  income_o_ins = '2182', # Other income: Insurance (Only up to 8th Survey)
  income_o_ins_g = '2183', # Other income: General Insurance
  income_o_ins_s = '2184', # Other income: Savings Insurance
  income_o_ins_wl = '2185', # Other income: Whole Life Insurance
  income_o_sev = '2186', # Other income: Severance Pay
  income_o_inh = '2187', # Other income: Inheritance
  income_o_cc = '2188', # Other income: Congratulatory/Condolence
  income_o_pr = '2189', # Other income: Prize
  income_o_comp = '2190', # Other income: Accident/Disaster Compensation
  income_o_o = '2191', # Other income: Other
  asset_ra_o = '2501', # Real estate asset: Other than Residency
  asset_ra_v = '2512', # Real estate asset: Value of Owned Real Estate
  asset_ra_vc = '2513', # Real estate asset: Value of Owned Real Estate
  asset_ra_rd = '2523', # Real estate asset: Value of Rental Deposit
  asset_ra_rdc = '2524', # Real estate asset: Value of Rental Deposit (Categorized)
  asset_ra_d = '2542', # Real estate asset: Value of Deposit
  asset_ra_dc = '2543', # Real estate asset: Value of Deposit (Categorized)
  asset_fin_sav = '2562', # Financial asset: Bank Savings
  asset_fin_stock = '2564', # Financial asset: Stock/Bond/Trust
  asset_fin_si = '2566', # Financial asset: Savings Insurance
  asset_fin_gye = '2568', # Financial asset: Gye(계)
  asset_fin_lend = '2570', # Financial asset: Money lent to others
  asset_fin_o = '2572', # Financial asset: Other
  weight_h_98 = 'w', # Weight for 1998 survey households
  weight_h_09 = 'sw', # Weight for 2009 survey households
  weight_h_18 = 'nw' # Weight for 2018 survey households
)

##### ==== Individual level ====
mapping_ind_var <- c(
  sex = '0101', # Gender
  fam_rel = '0102', # Relationship with head of household
  birth_year = '0104',
  age = '0107', # Age
  educ_self = '0110', # Education Level
  educ_stat = '0111', # Current status of education
  educ_degree_curr = '5202', # Current degree in college
  educ_major_curr = '5204', # Current major
  place_curr = '0121', # Current Residency Address
  employ_stat = '0201', # Employment Status
  employ_type = '0211', # Employment Type
  employ_position = '0314', # Employment position
  employ_regular = '0317', # Regular/Non-regular Worker
  employ_ind_8 = '0340', # 8th Industry Code
  employ_ind_9 = '0341', # 9th Industry Code
  employ_ind_10 = '0342', # 10th Industry Code
  employ_job_5 = '0350', # 5th Job Code
  employ_job_6 = '0351', # 6th Job Code
  employ_job_7 = '0352', # 7th Job Code
  last_income_labor_pre = '1702', # Pre-tax Labor Income, not available until 5th
  last_income_labor_post = '1703', # Post-tax Labor Income, not available until 5th
  # Weight
  weight_long_98 = 'w_l', # Longitudinal weights for 1998 survey samples
  weight_cross_98 = 'w_c', # Cross-sectional weights for 1998 survey samples
  weight_long_09 = 'sw_l', # Longitudinal weights for 2009 survey samples
  weight_cross_09 = 'sw_c', # Cross-sectional weights for 2009 survey samples
  weight_long_18 = 'nw_l', # Longitudinal weights for 2018 survey samples
  weight_cross_18 = 'nw_c' # Cross-sectional weights for 2018 survey samples
)

##### ==== Supplementary material ====
mapping_supp_var <- c(
  educ_major_past_1_a4 = '5252', # 1st college: major, after 4th wave
  educ_major_past_2_a4 = '5272', # 2nd college: major, after 4th wave
  educ_major_past_3_a4 = '5292', # 3rd college: major, after 4th wave
  educ_major_past_4_a4 = '5312', # 4th college: major, after 4th wave
  educ_major_past_1_b3 = '5245', # 1st college: major, until 3rd wave
  educ_major_past_2_b3 = '5265', # 2nd college: major, until 3rd wave
  educ_major_past_3_b3 = '5285', # 3rd college: major, until 3rd wave
  educ_major_past_4_b3 = '5305', # 4th college: major, until 3rd wave
  educ_grad_past_1 = '5244',
  educ_grad_past_2 = '5264',
  educ_grad_past_3 = '5284',
  educ_grad_past_4 = '5304',
  educ_grad_year_past_1 = '5248',
  educ_grad_year_past_2 = '5268',
  educ_grad_year_past_3 = '5288',
  educ_grad_year_past_4 = '5308',
  place_birth = '9001', # Place of Birth
  place_14 = '9003', # Place at Age 14
  educ_father = '9051', # Education Level of Father
  educ_mother = '9053' # Education Level of Mother
)
