

#### ==== aggregate_income ====
#'
#'
#'
#'
aggregate_income <- function (data, wave) {
  
  #### ==== 1. Replace NA with 0 ====
  data <- data %>%
  # Here, I have replaced every NA in income and asset as 0, as suggest by KLIPS
  mutate_at(
    vars(matches('income'), matches('asset')),
    .funs = function(x) ifelse(x == -1 | is.na(x), 0, x)
  ) %>%
  #### ==== 2. Set initial value for income_fin and income_ra ====
    mutate(
      income_fin = income_fin_interest_i,
      income_fin_2 = income_fin_interest_i,
      income_ra = income_ra_rent,
      income_ra_2 = income_ra_rent,
      income_t = income_t_basic
    )

  #### ==== 1.4. Summation of income and asset ====
  # Integration of transfer income across waves:
  # As waves differ in how they measure transfer income:
  # Wave 1: income_t_basic = all transfers
  # Wave 2-5: income_t_basic includes all government transfers
  # Wave 6-8: Governmental supports are split into income_t_gov
  # Wave 9 - 22: Relatives support split into
  #   - income_t_sup_p: parent
  #   - income_t_sup_c: children
  #   - income_t_sup_o: other relatives
  # Wave 23+: pension, income_t_pen is newly added
  if (wave > 1) {
    data <- mutate(data, income_t = income_t + income_t_sup_s + income_t_sup_o)
  }
  if (wave > 1 & wave <= 8) data <- mutate(data, income_t = income_t + income_t_sup_r)
  if (wave > 6) data <- mutate(data, income_t = income_t + income_t_gov)
  if (wave > 8) data <- mutate(data,
                            income_t = income_t +
                              income_t_sup_p + income_t_sup_c + income_t_sup_or
                            )
  if (wave >= 23) data <- mutate(data, income_t = income_t + income_t_pen)
  
  # First wave does not include the following variables:
  # - income_fin: financial income
  # - asset_ra: real estate asset
  # - asset_fin: financial asset
  # - income_si: social insurance
  #   * income_si may not exist for some years
  if(wave > 1){
    data <- data %>%
      mutate(
        income_fin = income_fin + income_fin_interest_n +
          income_fin_stock_profit + income_fin_stock_div + income_fin_other,
        income_fin_2 = income_fin_2 + income_fin_interest_n +
          income_fin_stock_div + income_fin_other, # income_fin_stock_profit is not current income
        asset_ra = asset_ra_o + asset_ra_v + asset_ra_rd + asset_ra_d,
        asset_fin = asset_fin_sav + asset_fin_stock + asset_fin_si + 
          asset_fin_gye + asset_fin_lend + asset_fin_o,
        asset = asset_ra + asset_fin,
        income_si = income_si_1 + income_si_2,
      )
  }
  for (i in 3:5) {
    if(paste0('income_si_' , i) %in% colnames(data)) {
      data$income_si <- data$income_si + data[[paste0('income_si_' , i)]]
    }
  }
  
  # After 8th wave, income from insurance is measured with three variables:
  # Here, some variables relevant to other income sources may not exist for
  #  some years
  if (wave > 8) {
    data <- data %>%
      mutate(
        income_o_ins = income_o_ins_g + income_o_ins_s + income_o_ins_wl,
      )
  }
  
  # Some of the variables in income are not present in some waves.
  data <- data %>%
    mutate(
      income_o = income_o_ins
    )
  for (i in c('sev', 'inh', 'cc', 'pr', 'comp', 'o')) {
    if (paste0('income_o_', i) %in% colnames(data)) {
      data$income_o <- data$income_o + data[[paste0('income_o_', i)]]
    }
  }
  for (i in c('sale', 'cult', 'prem', 'other')) {
    if (paste0('income_ra_', i) %in% colnames(data)) {
      data$income_ra <- data$income_ra + data[[paste0('income_ra_', i)]]
    }
  }
  for (i in c('cult', 'prem', 'other')) {
    if (paste0('income_ra_', i) %in% colnames(data)) {
      data$income_ra_2 <- data$income_ra_2 + data[[paste0('income_ra_', i)]] # Income from real asset does not include profits from sales of real asset
    }
  }
  
  data <- data %>%
    mutate(
      income = income_labor + income_fin + income_ra +
        income_t + income_o,
      income_2 = income_labor + income_fin_2 + income_ra_2 + income_t # Income does not include other sources of income which is given to the household one time only
    )
  
  if ('income_si' %in% colnames(data)) {
    data$income <- data$income + data$income_si
    data$income_2 <- data$income_2 + data$income_si
  }
  
  return (data)
}

#### ==== eq_inc ====
#'
#'
#'
eq_inc <- function (income, num_fam) {
  result <- ifelse(num_fam != 0, income / sqrt(num_fam), 0)
  return(result)
}