var_map <- list()

var_1997_fam <- c(
  'state_curr' = 'ER10004',
  'FUID' = 'ER10008',
  'age_head' = 'ER10009',
  'sex_head' = 'ER10010',
  'age_spouse' = 'ER10011',
  'econ_act_1_head' = 'ER10081', # B1: Activity status, 1st mention (head)
  'econ_act_2_head' = 'ER10082', # B1: Activity status, 2nd mention (head)
  'econ_act_3_head' = 'ER10083', # B1: Activity status, 3rd mention (head)
  'econ_work_head' = 'ER10075', # B3: Working for money? (head)
  'econ_act_1_spouse' = 'ER10563', # D1A: Activity status, 1st mention (spouse)
  'econ_act_2_spouse' = 'ER10564', # D1A: Activity status, 2nd mention (spouse)
  'econ_act_3_spouse' = 'ER10565', # D1A: Activity status, 3rd mention (spouse)
  'econ_work_spouse' = 'ER10567', # D3: Working for money? (spouse)
  'race_spouse' = 'ER11763', # K34: Race of wife
  'highschool_spouse' = 'ER11766', # K37: Graduated high school
  'ged_grad_spouse' = 'ER11769', # K39: If GED, last grade
  'univ_attend_spouse' = 'ER11777', # K44: Attended university
  'univ_highest_spouse' = 'ER11780', # K46: Highest year completed
  'univ_degree_spouse' = 'ER11781', # K47: University degree?
  'univ_degree_highest_spouse' = 'ER11782', # K48: Highest univ. degree?
  'growth_region_head' = 'ER11841', # L34: Grew up in farm, small town, etc.
  'growth_state_head' = 'ER11842', # L34A: Grew up in state?
  'race_head' = 'ER11851', # L34: Race of head
  'highschool_head' = 'ER11854', # L44: Graduated high school
  'ged_grad_head' = 'ER11857', # L46: If GED, last grade
  'univ_attend_head' = 'ER11865', # L51: Attended university
  'univ_highest_head' = 'ER11868', # L53: Highest year completed
  'univ_degree_head' = 'ER11869', # K54: University degree?
  'univ_degree_highest_head' = 'ER11870', # K48: Highest univ. degree?
  'income_hs_taxable' = 'ER12069', # Taxable income: head + spouse
  'income_family' = 'ER12079', # Total family income
  'income_labor_head' = 'ER12080', # Labor income of head
  'income_labor_spouse' = 'ER12082', # Labor income of spouse,
  'f_w' = 'ER12084', # Family weight
  'occupation_head' = 'ER12085', # B9-9A: main occupation
  'industry_head' = 'ER12086', # B10: main industry
  'occupation_spouse' = 'ER12116', # D9-9A: main occupation
  'industry_spouse' = 'ER12117', # D10: main industry
)

