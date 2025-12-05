

code <- readRDS(file = here::here('data/PSID/clean/codebook.rds'))
codelong <- readRDS(file = here::here('data/PSID/clean/codebook_value.rds'))


find_var <- function (keyword, .code) {
  .code <- code
  
  keyword <- strsplit(keyword, split = ' ')
  keyword <- paste0('(?=.*', paste0(keyword[[1]], collapse = '.*)(?=.*'), '.*)')
  
  .code %>%
    filter(
      stri_detect_regex(label, keyword, case_insensitive = TRUE) |
        stri_detect_regex(qtext, keyword, case_insensitive = TRUE)
    )
  
}

year_var <- function (year, var, .code) {
  .code <- code
  
  .code %>%
    filter(year == !!year) %>%
    pull(!!var)
}

check_var <- function (var_list, .code) {
  .code <- code
  
  .code %>%
    filter(varname %in% var_list)
}


year_var(1997, 'label')

find_var('sequence')
find_var('interview number')

var_map <- list()

var_map[['interview_num']] <- tribble(
  ~year, ~code,
  1990, 'ER30642',
  1991, 'ER30689',
  1992, 'ER30733',
  1993, 'ER30806',
  1994, 'ER33101',
  1995, 'ER33201',
  1996, 'ER33301',
  1997, 'ER33401',
  1999, 'ER33501',
  2001, 'ER33601',
  2003, 'ER33701',
  2005, 'ER33801',
  2007, 'ER33901',
  2009, 'ER34001',
  2011, 'ER34101',
  2013, 'ER34201',
  2015, 'ER34301',
  2017, 'ER34501',
  2019, 'ER34701',
  2021, 'ER34901',
  2023, 'ER35101'
)

var_map[['seq_num']] <- tribble(
  ~year, ~code,
  1990, 'ER30643',
  1991, 'ER30690',
  1992, 'ER30734',
  1993, 'ER30807',
  1994, 'ER33102',
  1995, 'ER33202',
  1996, 'ER33302',
  1997, 'ER33402',
  1999, 'ER33502',
  2001, 'ER33602',
  2003, 'ER33702',
  2005, 'ER33802',
  2007, 'ER33902',
  2009, 'ER34002',
  2011, 'ER34102',
  2013, 'ER34202',
  2015, 'ER34302',
  2017, 'ER34502',
  2019, 'ER34702',
  2021, 'ER34902',
  2023, 'ER35102'
)

check_var(var_map[['interview_num']]$code) %>% View()
check_var(var_map[['seq_num']]$code) %>% View()

var_map[['int_id']] <- tribble(
  ~year, ~code,
  'all', 'ER30001'
)
find_var('person number')
var_map[['pid']] <- tribble(
  ~year, ~code,
  'all', 'ER30002'
)
var_map[['pid_f']] <- tribble(
  ~year, ~code,
  'all', 'ER32017'
)
var_map[['pid_m']] <- tribble(
  ~year, ~code,
  'all', 'ER32010'
)

find_var('household number')
var_map[['hhid']] <- tribble(
  ~year, ~code,
  1990, 'V18936',
  1991, 'V20236',
  1992, 'V21542',
  1993, 'V23356',
  1994, 'ER4159R',
  1995, 'ER6999R',
  1996, 'ER9250R',
  1997, 'ER12223R',
  1999, 'ER16447',
  2001, 'ER20393',
  2003, 'ER24170',
  2005, 'ER28069',
  2007, 'ER41059',
  2009, 'ER47003',
  2011, 'ER52427',
  2013, 'ER58245',
  2015, 'ER65481',
  2017, 'ER71560',
  2019, 'ER77621',
  2021, 'ER81948',
  2023, 'ER85802'
)
check_var(var_map[['hhid']]$code)

find_var('relation to head')
find_var('relation to ref')
var_map[['rel_head']] <- tribble(
  ~year, ~code,
  1990, 'ER30644',
  1991, 'ER30691',
  1992, 'ER30735',
  1993, 'ER30808',
  1994, 'ER33103',
  1995, 'ER33203',
  1996, 'ER33303',
  1997, 'ER33403',
  1999, 'ER33503',
  2001, 'ER33603',
  2003, 'ER33703',
  2005, 'ER33803',
  2007, 'ER33903',
  2009, 'ER34003',
  2011, 'ER34103',
  2013, 'ER34203',
  2015, 'ER34303',
  2017, 'ER34503',
  2019, 'ER34703',
  2021, 'ER34903',
  2023, 'ER35103'
)
check_var(var_map[['rel_head']]$code) %>% View()

##### 1. SEX #####
find_var('sex')
var_map[['sex']] <- tribble(
  ~year, ~code,
  'all', 'ER32000'
)
check_var(var_map[['sex']]$code)
find_var('sex head')
var_map[['sex_rp']] <- tribble(
  ~year, ~code,
  1990, 'V18050',
  1991, 'V19350',
  1992, 'V20652',
  1993, 'V22407',
  1994, 'ER2008',
  1995, 'ER5007',
  1996, 'ER7007',
  1997, 'ER10010',
  1999, 'ER13011',
  2001, 'ER17014',
  2003, 'ER21018',
  2005, 'ER25018',
  2007, 'ER36018',
  2009, 'ER42018',
  2011, 'ER47318',
  2013, 'ER53018',
  2015, 'ER60018',
  2017, 'ER66018',
  2019, 'ER72018',
  2021, 'ER78018',
  2023, 'ER82019'
)
check_var(var_map[['sex_rp']]$code) %>% View()

find_var('sex spouse')
var_map[['sex_sp']] <- tribble(
  ~year, ~code,
  2015, 'ER60020',
  2017, 'ER66020',
  2019, 'ER72020',
  2021, 'ER78020',
  2023, 'ER82021'
)
check_var(var_map[['sex_sp']]$code)

##### 2. AGE #####
find_var('age of individual')
var_map[['age']] <- tribble(
  ~year, ~code,
  1990, 'ER30645',
  1991, 'ER30692',
  1992, 'ER30736',
  1993, 'ER30809',
  1994, 'ER33104',
  1995, 'ER33204',
  1996, 'ER33304',
  1997, 'ER33404',
  1999, 'ER33504',
  2001, 'ER33604',
  2003, 'ER33704',
  2005, 'ER33804',
  2007, 'ER33904',
  2009, 'ER34004',
  2011, 'ER34104',
  2013, 'ER34204',
  2015, 'ER34305',
  2017, 'ER34504',
  2019, 'ER34704',
  2021, 'ER34904',
  2023, 'ER35104'
)
check_var(var_map[['age']]$code)

var_map[['birth_year']] <- tribble(
  ~year, ~code,
  1990, 'ER30647',
  1991, 'ER30694',
  1992, 'ER30738 ',
  1993, 'ER30811 ',
  1994, 'ER33106 ',
  1995, 'ER33206 ',
  1996, 'ER33306 ',
  1997, 'ER33406 ',
  1999, 'ER33506 ',
  2001, 'ER33606',
  2003, 'ER33706',
  2005, 'ER33806',
  2007, 'ER33906',
  2009, 'ER34006',
  2011, 'ER34106',
  2013, 'ER34206',
  2015, 'ER34307',
  2017, 'ER34506',
  2019, 'ER34706',
  2021, 'ER34906',
  2023, 'ER35106'
)

find_var('age of head')
find_var('age of ref')
var_map[['age_rp']] <- tribble(
  ~year, ~code,
  1990, 'V18049',
  1991, 'V19349',
  1992, 'V20651',
  1993, 'V22406',
  1994, 'ER2007',
  1995, 'ER5006',
  1996, 'ER7006',
  1997, 'ER10009',
  1999, 'ER13010',
  2001, 'ER17013',
  2003, 'ER21017',
  2005, 'ER25017',
  2007, 'ER36017',
  2009, 'ER42017',
  2011, 'ER47317',
  2013, 'ER53017',
  2015, 'ER60017',
  2017, 'ER66017',
  2019, 'ER72017',
  2021, 'ER78017',
  2023, 'ER82018'
)
check_var(var_map[['age_rp']]$code)

find_var('age of wife')
find_var('age of spouse')
var_map[['age_sp']] <- tribble(
  ~year, ~code,
  1990, 'V18051',
  1991, 'V19351',
  1992, 'V20653',
  1993, 'V22408',
  1994, 'ER2009',
  1995, 'ER5008',
  1996, 'ER7008',
  1997, 'ER10011',
  1999, 'ER13012',
  2001, 'ER17015',
  2003, 'ER21019',
  2005, 'ER25019',
  2007, 'ER36019',
  2009, 'ER42019',
  2011, 'ER47319',
  2013, 'ER53019',
  2015, 'ER60019',
  2017, 'ER66019',
  2019, 'ER72019',
  2021, 'ER78019',
  2023, 'ER82020'
)
check_var(var_map[['age_sp']]$code)

##### 3. RACE #####
find_var('race')
var_map[['race']] <- tribble(
  ~year, ~code,
  'all', 'V181'
)
check_var(var_map[['race']]$code)

find_var('race of head 1')
find_var('race of ref 1')
var_map[['race_rp_1']] <- tribble(
  ~year, ~code,
  1990, 'V18814',
  1991, 'V20114',
  1992, 'V21420',
  1993, 'V23276',
  1994, 'ER3944',
  1995, 'ER6814',
  1996, 'ER9060',
  1997, "ER11848",
  1999, "ER15928",
  2001, "ER19989",
  2003, "ER23426",
  2005, 'ER27393',
  2007, 'ER40565',
  2009, 'ER46543',
  2011, 'ER51904',
  2013, 'ER57659',
  2015, 'ER64810',
  2017, "ER70882",
  2019, "ER76897",
  2021, "ER81144",
  2023, 'ER85121'
)
var_map[['race_rp_2']] <- tribble(
  ~year, ~code,
  1990, 'V18815',
  1991, 'V20115',
  1992, 'V21421',
  1993, 'V23277',
  1994, 'ER3945',
  1995, 'ER6815',
  1996, 'ER9061',
  1997, "ER11849",
  1999, "ER15929",
  2001, "ER19990",
  2003, "ER23427",
  2005, 'ER27394',
  2007, 'ER40566',
  2009, 'ER46544',
  2011, 'ER51905',
  2013, 'ER57660',
  2015, 'ER64811',
  2017, "ER70883",
  2019, "ER76898",
  2021, "ER81145",
  2023, 'ER85122'
)
var_map[['race_rp_3']] <- tribble(
  ~year, ~code,
  1994, 'ER3946',
  1995, 'ER6816',
  1996, 'ER9062',
  1997, "ER11850",
  1999, "ER15930",
  2001, "ER19991",
  2003, "ER23428",
  2005, 'ER27395',
  2007, 'ER40567',
  2009, 'ER46545',
  2011, 'ER51906',
  2013, 'ER57661',
  2015, 'ER64812',
  2017, "ER70884",
  2019, "ER76899",
  2021, "ER81146",
  2023, 'ER85123'
)
var_map[['race_rp_4']] <- tribble(
  ~year, ~code,
  1997, "ER11851",
  1999, "ER15931",
  2001, "ER19992",
  2003, "ER23429",
  2005, 'ER27396',
  2007, 'ER40568',
  2009, 'ER46546',
  2011, 'ER51907',
  2013, 'ER57662',
  2015, 'ER64813',
  2017, "ER70885",
  2019, "ER76900",
  2021, "ER81147",
  2023, 'ER85124'
)
check_var(var_map[['race_rp_1']]$code)
check_var(var_map[['race_rp_2']]$code)
check_var(var_map[['race_rp_3']]$code)
check_var(var_map[['race_rp_4']]$code)

find_var('race of wife 1')
find_var('race of spouse 1')
var_map[['race_sp_1']] <- tribble(
  ~year, ~code,
  1990, 'V18749',
  1991, 'V20049',
  1992, 'V21355',
  1993, 'V23212',
  1994, 'ER3883',
  1995, 'ER6753',
  1996, 'ER8999',
  1997, "ER11760",
  1999, "ER15836",
  2001, "ER19897",
  2003, "ER23334",
  2005, 'ER27297',
  2007, 'ER40472',
  2009, 'ER46449',
  2011, 'ER51810',
  2013, 'ER57549',
  2015, 'ER64671',
  2017, "ER70744",
  2019, "ER76752",
  2021, "ER81017",
  2023, 'ER84994'
)
find_var('race of wf')
find_var('race of spouse 2')
var_map[['race_sp_2']] <- tribble(
  ~year, ~code,
  1990, 'V18750',
  1991, 'V20050',
  1992, 'V21356',
  1993, 'V23213',
  1994, 'ER3884',
  1995, 'ER6754',
  1996, 'ER9000',
  1997, "ER11761",
  1999, "ER15837",
  2001, "ER19898",
  2003, "ER23335",
  2005, 'ER27298',
  2007, 'ER40473',
  2009, 'ER46450',
  2011, 'ER51811',
  2013, 'ER57550',
  2015, 'ER64672',
  2017, "ER70745",
  2019, "ER76753",
  2021, "ER81018",
  2023, 'ER84995'
)
find_var('race of wife 3')
find_var('race of spouse 3')
var_map[['race_sp_3']] <- tribble(
  ~year, ~code,
  1994, 'ER3885',
  1995, 'ER6755',
  1996, 'ER9001',
  1997, "ER11762",
  1999, "ER15838",
  2001, "ER19899",
  2003, "ER23336",
  2005, 'ER27299',
  2007, 'ER40474',
  2009, 'ER46451',
  2011, 'ER51812',
  2013, 'ER57551',
  2015, 'ER64673',
  2017, "ER70746",
  2019, "ER76754",
  2021, "ER81019",
  2023, 'ER84996'
)
find_var('race of wife 4')
find_var('race of spouse 4')
var_map[['race_sp_4']] <- tribble(
  ~year, ~code,
  1997, "ER11763",
  1999, "ER15839",
  2001, "ER19900",
  2003, "ER23337",
  2005, 'ER27300',
  2007, 'ER40475',
  2009, 'ER46452',
  2011, 'ER51813',
  2013, 'ER57552',
  2015, 'ER64674',
  2017, "ER70747",
  2019, "ER76755",
  2021, "ER81020",
  2023, 'ER84997'
)
check_var(var_map[['race_sp_1']]$code)
check_var(var_map[['race_sp_2']]$code)
check_var(var_map[['race_sp_3']]$code)
check_var(var_map[['race_sp_4']]$code)

#### 4. Employment ####
find_var('employ status')
var_map[['employ_stat']] <- tribble(
  ~year, ~code,
  1990, 'ER30653',
  1991, 'ER30699',
  1992, 'ER30744',
  1993, 'ER30816',
  1994, 'ER33111',
  1995, 'ER33211',
  1996, 'ER33311',
  1997, "ER33411",
  1999, "ER33512",
  2001, "ER33612",
  2003, "ER33712",
  2005, 'ER33813',
  2007, 'ER33913',
  2009, 'ER34016',
  2011, 'ER34116',
  2013, 'ER34216',
  2015, 'ER34317',
  2017, "ER34516",
  2019, "ER34716",
  2021, "ER34916",
  2023, 'ER35116'
)
check_var(var_map[['employ_stat']]$code)

find_var('industry')
var_map[['ind_rp']] <- tribble(
  ~year, ~code,
  1990, 'V18102',
  1991, 'V19402',
  1992, 'V20702',
  1993, 'V22457',
  1994, 'ER4018',
  1995, 'ER6858',
  1996, 'ER9109',
  1997, "ER12086",
  1999, "ER13216",
  2001, "ER17227",
  2003, "ER21146",
  2005, 'ER25128',
  2007, 'ER36133',
  2009, 'ER42168',
  2011, 'ER47480',
  2013, 'ER53180',
  2015, 'ER60195',
  2017, "ER66196",
  2019, "ER72196",
  2021, "ER78199",
  2023, 'ER82182'
)
var_map[['ind_sp']] <- tribble(
  ~year, ~code,
  1990, 'V18404',
  1991, 'V19704',
  1992, 'V21004',
  1993, 'V22810',
  1994, 'ER4049',
  1995, 'ER6889',
  1996, 'ER9140',
  1997, "ER12117",
  1999, "ER13728",
  2001, "ER17797",
  2003, "ER21396",
  2005, 'ER25386',
  2007, 'ER36391',
  2009, 'ER42420',
  2011, 'ER47737',
  2013, 'ER53443',
  2015, 'ER60458',
  2017, "ER66471",
  2019, "ER72473",
  2021, "ER78513",
  2023, 'ER82501'
)
check_var(var_map[['ind_rp']]$code)
check_var(var_map[['ind_sp']]$code)

find_var('occupation h')
find_var('occupation hd')
find_var('occupation head')
find_var('occ rp')
var_map[['occ_rp']] <- tribble(
  ~year, ~code,
  1990, 'V18101',
  1991, 'V19401',
  1992, 'V20701',
  1993, 'V22456',
  1994, 'ER4017',
  1995, 'ER6857',
  1996, 'ER9108',
  1997, "ER12085",
  1999, "ER13215",
  2001, "ER17226",
  2003, "ER21145",
  2005, 'ER25127',
  2007, 'ER36132',
  2009, 'ER42167',
  2011, 'ER47479',
  2013, 'ER53179',
  2015, 'ER60194',
  2017, "ER66195",
  2019, "ER72195",
  2021, "ER78198",
  2023, 'ER82181'
)
find_var('occupation w')
find_var('occupation wf')
find_var('occupation wife')
find_var('occ sp')
var_map[['occ_sp']] <- tribble(
  ~year, ~code,
  1990, 'V18403',
  1991, 'V19703',
  1992, 'V21003',
  1993, 'V22809',
  1994, 'ER4048',
  1995, 'ER6888',
  1996, 'ER9139',
  1997, "ER12116",
  1999, "ER13727",
  2001, "ER17796",
  2003, "ER21395",
  2005, 'ER25385',
  2007, 'ER36390',
  2009, 'ER42419',
  2011, 'ER47736',
  2013, 'ER53442',
  2015, 'ER60457',
  2017, "ER66470",
  2019, "ER72472",
  2021, "ER78512",
  2023, 'ER82500'
)
check_var(var_map[['occ_rp']]$code)
check_var(var_map[['occ_sp']]$code)

#### 5. Education ####
##### Bachelor #####
find_var('college degree receive') %>% View()
find_var('major bachelor')
var_map[['educ_bach_major_1']] <- tribble(
  ~year, ~code,
  2013, 'ER34224',
  2015, 'ER34337',
  2017, 'ER34536',
  2019, 'ER34738',
  2021, 'ER34938',
  2023, 'ER35138'
)
var_map[['educ_bach_major_2']] <- tribble(
  ~year, ~code,
  2015, 'ER34338',
  2017, 'ER34537',
  2019, 'ER34739',
  2021, 'ER34939',
  2023, 'ER35139'
)
var_map[['educ_bach_major_1_digit']] <- tribble(
  ~year, ~code,
  2019, 'ER34736',
  2021, 'ER34936',
  2023, 'ER35136'
)
var_map[['educ_bach_major_1_digit']] <- tribble(
  ~year, ~code,
  2019, 'ER34737',
  2021, 'ER34937',
  2023, 'ER35137'
)

check_var(var_map[['educ_bach_major_1']]$code)
check_var(var_map[['educ_bach_major_2']]$code)
check_var(var_map[['educ_bach_major_1_digit']]$code)
check_var(var_map[['educ_bach_major_2_digit']]$code)

find_var('L55a\\.')
find_var('L78f2\\.')
var_map[['educ_bach_major_1_rp']] <- tribble(
  ~year, ~code,
  2015, 'ER64838',
  2017, 'ER70910',
  2019, 'ER76925',
  2021, 'ER81172',
  2023, 'ER85149'
)
var_map[['educ_bach_major_2_rp']] <- tribble(
  ~year, ~code,
  2015, 'ER64839',
  2017, 'ER70911',
  2019, 'ER76926',
  2021, 'ER81173',
  2023, 'ER85150'
)
var_map[['educ_bach_major_1_update_rp']] <- tribble(
  ~year, ~code,
  2015, 'ER64896',
  2017, 'ER70967',
  2019, 'ER76985',
  2021, 'ER81213',
  2023, 'ER85190'
)
var_map[['educ_bach_major_2_update_rp']] <- tribble(
  ~year, ~code,
  2015, 'ER64897',
  2017, 'ER70968',
  2019, 'ER76986',
  2021, 'ER81214',
  2023, 'ER85191'
)

find_var('k55a\\.')
var_map[['educ_bach_major_1_sp']] <- tribble(
  ~year, ~code,
  2015, 'ER64699',
  2017, 'ER70772',
  2019, 'ER76780',
  2021, 'ER81045',
  2023, 'ER85022'
)
var_map[['educ_bach_major_2_sp']] <- tribble(
  ~year, ~code,
  2015, 'ER64700',
  2017, 'ER70773',
  2019, 'ER76781',
  2021, 'ER81046',
  2023, 'ER85023'
)
find_var('k78f2\\.')
var_map[['educ_bach_major_1_update_sp']] <- tribble(
  ~year, ~code,
  2015, 'ER64757',
  2017, 'ER70829',
  2019, 'ER76840',
  2021, 'ER81086',
  2023, 'ER85063'
)
var_map[['educ_bach_major_2_update_sp']] <- tribble(
  ~year, ~code,
  2015, 'ER64758',
  2017, 'ER70830',
  2019, 'ER76841',
  2021, 'ER81087',
  2023, 'ER85064'
)

check_var(var_map[['educ_bach_major_1_rp']]$code)
check_var(var_map[['educ_bach_major_2_rp']]$code)
check_var(var_map[['educ_bach_major_1_update_rp']]$code)
check_var(var_map[['educ_bach_major_2_update_rp']]$code)
check_var(var_map[['educ_bach_major_1_sp']]$code)
check_var(var_map[['educ_bach_major_2_sp']]$code)
check_var(var_map[['educ_bach_major_1_update_sp']]$code)
check_var(var_map[['educ_bach_major_2_update_sp']]$code)

##### Highest #####
find_var('highest degree') %>% View()
var_map[['educ_highest']] <- tribble(
  ~year, ~code,
  2017, "ER34535",
  2019, "ER34735",
  2021, "ER34935",
  2023, 'ER35135'
)
find_var('highest') %>% View()
var_map[['educ_highest_major_1']] <- tribble(
  ~year, ~code,
  2013, 'ER34227',
  2015, 'ER34341',
  2017, "ER34540",
  2019, "ER34744",
  2021, "ER34944",
  2023, 'ER35144'
)
var_map[['educ_highest_major_2']] <- tribble(
  ~year, ~code,
  2015, 'ER34342',
  2017, "ER34541",
  2019, "ER34745",
  2021, "ER34945",
  2023, 'ER35145'
)
var_map[['educ_highest_major_1_digit']] <- tribble(
  ~year, ~code,
  2019, "ER34742",
  2021, "ER34942",
  2023, 'ER35142'
)
var_map[['educ_highest_major_2_digit']] <- tribble(
  ~year, ~code,
  2019, "ER34743",
  2021, "ER34943",
  2023, 'ER35143'
)
var_map[['educ_highest_year']] <- tribble(
  ~year, ~code,
  2013, 'ER34226',
  2015, 'ER34344',
  2017, "ER34543",
  2019, "ER34747",
  2021, "ER34947",
  2023, 'ER35147'
)

check_var(var_map[['educ_highest']]$code)
check_var(var_map[['educ_highest_major_1']]$code)
check_var(var_map[['educ_highest_major_2']]$code)
check_var(var_map[['educ_highest_major_1_digit']]$code)
check_var(var_map[['educ_highest_major_2_digit']]$code)
check_var(var_map[['educ_highest_year']]$code)

find_var('highest degree year')
find_var('completed')
var_map[['educ_highest_major_rp_1']] <- tribble(
  ~year, ~code,
  2013, 'ER57725',
  2015, 'ER64842',
  2017, 'ER70914',
  2019, 'ER76931',
  2021, 'ER81178',
  2023, 'ER85155'
)
var_map[['educ_highest_major_rp_2']] <- tribble(
  ~year, ~code,
  2015, 'ER64843',
  2017, 'ER70915',
  2019, 'ER76932',
  2021, 'ER81179',
  2023, 'ER85156'
)
var_map[['educ_highest_major_update_rp_1']] <- tribble(
  ~year, ~code,
  2015, 'ER64900',
  2017, 'ER70971',
  2019, 'ER76991',
  2021, 'ER81219',
  2023, 'ER85196'
)
var_map[['educ_highest_major_update_rp_2']] <- tribble(
  ~year, ~code,
  2015, 'ER64901',
  2017, 'ER70972',
  2019, 'ER76992',
  2021, 'ER81220',
  2023, 'ER85197'
)
var_map[['educ_highest_year_rp']] <- tribble(
  ~year, ~code,
  2009, 'ER46570',
  2011, 'ER51931',
  2013, 'ER57724',
  2015, 'ER64903',
  2017, 'ER70974',
  2019, 'ER76996',
  2021, 'ER81224',
  2023, 'ER85201'
)
var_map[['educ_highest_year_rp']] <- tribble(
  ~year, ~code,
  2009, 'ER46570',
  2011, 'ER51931',
  2013, 'ER57724',
  2015, 'ER64903',
  2017, 'ER70974',
  2019, 'ER76996',
  2021, 'ER81224',
  2023, 'ER85201'
)
check_var(var_map[['educ_highest_major_rp_1']]$code)
check_var(var_map[['educ_highest_major_rp_2']]$code)
check_var(var_map[['educ_highest_major_update_rp_1']]$code)
check_var(var_map[['educ_highest_major_update_rp_2']]$code)
check_var(var_map[['educ_highest_year_rp']]$code)

var_map[['educ_highest_major_sp_1']] <- tribble(
  ~year, ~code,
  2013, 'ER57615',
  2015, 'ER64703',
  2017, 'ER70776',
  2019, 'ER76786',
  2021, 'ER81051',
  2023, 'ER85028'
)
var_map[['educ_highest_major_sp_2']] <- tribble(
  ~year, ~code,
  2015, 'ER64704',
  2017, 'ER70777',
  2019, 'ER76787',
  2021, 'ER81052',
  2023, 'ER85029'
)
var_map[['educ_highest_major_update_sp_1']] <- tribble(
  ~year, ~code,
  2015, 'ER64761',
  2017, 'ER70833',
  2019, 'ER76846',
  2021, 'ER81092',
  2023, 'ER85069'
)
var_map[['educ_highest_major_update_sp_2']] <- tribble(
  ~year, ~code,
  2015, 'ER64762',
  2017, 'ER70834',
  2019, 'ER76847',
  2021, 'ER81093',
  2023, 'ER85070'
)
var_map[['educ_highest_year_sp']] <- tribble(
  ~year, ~code,
  2009, 'ER46476',
  2011, 'ER51837',
  2013, 'ER57614',
  2015, 'ER64764',
  2017, 'ER70836',
  2019, 'ER76851',
  2021, 'ER81097',
  2023, 'ER85074'
)

check_var(var_map[['educ_highest_major_sp_1']]$code)
check_var(var_map[['educ_highest_major_sp_2']]$code)
check_var(var_map[['educ_highest_major_update_sp_1']]$code)
check_var(var_map[['educ_highest_major_update_sp_2']]$code)
check_var(var_map[['educ_highest_year_sp']]$code)


find_var('completed') %>% View()
var_map[['educ_complete']] <- tribble(
  ~year, ~code,
  1990, "ER30657",
  1991, 'ER30703',
  1992, 'ER30748',
  1993, 'ER30820',
  1994, 'ER33115',
  1995, 'ER33215',
  1996, 'ER33315',
  1997, "ER33415",
  1999, "ER33516",
  2001, "ER33616",
  2003, "ER33716",
  2005, 'ER33817',
  2007, 'ER33917',
  2009, 'ER34020',
  2011, 'ER34119',
  2013, 'ER34230',
  2015, 'ER34349',
  2017, "ER34548",
  2019, "ER34752",
  2021, "ER34952",
  2023, 'ER35152'
)
find_var('completed education head') 
find_var('completed education ref')
var_map[['educ_complete_rp']] <- tribble(
  ~year, ~code,
  1991, 'V20198',
  1992, 'V21504',
  1993, 'V23333',
  1994, 'ER4158',
  1995, 'ER6998',
  1996, 'ER9249',
  1997, "ER12222",
  1999, "ER16516",
  2001, "ER20457",
  2003, "ER24148",
  2005, 'ER28047',
  2007, 'ER41037',
  2009, 'ER46981',
  2011, 'ER52405',
  2013, 'ER58223',
  2015, 'ER65459',
  2017, "ER71538",
  2019, "ER77599",
  2021, "ER81926",
  2023, 'ER85780'
)
find_var('completed education wife') 
find_var('completed education spouse')
var_map[['educ_complete_sp']] <- tribble(
  ~year, ~code,
  1991, 'V20199',
  1992, 'V21505',
  1993, 'V23334',
  1994, 'ER4159',
  1995, 'ER6999',
  1996, 'ER9250',
  1997, "ER12223",
  1999, "ER16517",
  2001, "ER20458",
  2003, "ER24149",
  2005, 'ER28048',
  2007, 'ER41038',
  2009, 'ER46982',
  2011, 'ER52406',
  2013, 'ER58224',
  2015, 'ER65460',
  2017, "ER71539",
  2019, "ER77600",
  2021, "ER81927",
  2023, 'ER85781'
)
check_var(var_map[['educ_complete']]$code)
check_var(var_map[['educ_complete_rp']]$code)
check_var(var_map[['educ_complete_sp']]$code)

var_map[['educ_hs_rp']] <- tribble(
  ~year, ~code,
  2017, "ER70949",
  2019, "ER76967",
  2021, "ER81195",
  2023, 'ER85172'
)
var_map[['educ_hs_sp']] <- tribble(
  ~year, ~code,
  2017, "ER70811",
  2019, "ER76822",
  2021, "ER81068",
  2023, 'ER85045'
)

check_var(var_map[['educ_hs_rp']]$code)
check_var(var_map[['educ_hs_sp']]$code)

find_var('current state')
var_map[['curr_state']] <- tribble(
  ~year, ~code,
  1990, "V17703",
  1991, 'V19003',
  1992, 'V20303',
  1993, 'V21603',
  1994, 'ER4157',
  1995, 'ER6997',
  1996, 'ER9248',
  1997, "ER10004",
  1999, "ER13005",
  2001, "ER17005",
  2003, "ER21004",
  2005, 'ER25004',
  2007, 'ER36004',
  2009, 'ER42004',
  2011, 'ER47304',
  2013, 'ER53004',
  2015, 'ER60004',
  2017, "ER66004",
  2019, "ER72004",
  2021, "ER78004",
  2023, 'ER82004'
)
check_var(var_map[['curr_state']]$code)


find_var('number fu')
var_map[['fam_size']] <- tribble(
  ~year, ~code,
  1990, "V18048",
  1991, 'V19348',
  1992, 'V20650',
  1993, 'V22405',
  1994, 'ER2006',
  1995, 'ER5005',
  1996, 'ER7005',
  1997, "ER10008",
  1999, "ER13009",
  2001, "ER17012",
  2003, "ER21016",
  2005, 'ER25016',
  2007, 'ER36016',
  2009, 'ER42016',
  2011, 'ER47316',
  2013, 'ER53016',
  2015, 'ER60016',
  2017, "ER66016",
  2019, "ER72016",
  2021, "ER78016",
  2023, 'ER82017'
)
check_var(var_map[['fam_size']]$code)

find_var('total family income')
var_map[['inc_total']] <- tribble(
  ~year, ~code,
  1990, "V18875",
  1991, 'V20175',
  1992, 'V21481',
  1993, 'V23322',
  1994, 'ER4153',
  1995, 'ER6993',
  1996, 'ER9244',
  1997, "ER12079",
  1999, "ER16462",
  2001, "ER20456",
  2003, "ER24099",
  2005, 'ER28037',
  2007, 'ER41027',
  2009, 'ER46935',
  2011, 'ER52343',
  2013, 'ER58152',
  2015, 'ER65349',
  2017, "ER71426",
  2019, "ER77448",
  2021, "ER81775",
  2023, 'ER85629'
)

find_var('taxable')
var_map[['inc_taxable_rs']] <- tribble(
  ~year, ~code,
  1990, "V17851",
  1991, 'V19151',
  1992, 'V20451',
  1993, 'V21959',
  1994, 'ER4146',
  1995, 'ER6986',
  1996, 'ER9237',
  1997, "ER12069",
  1999, "ER16452",
  2001, "ER20449",
  2003, "ER24100",
  2005, 'ER27953',
  2007, 'ER40943',
  2009, 'ER46851',
  2011, 'ER52259',
  2013, 'ER58060',
  2015, 'ER65253',
  2017, "ER71330",
  2019, "ER77352",
  2021, "ER81679",
  2023, 'ER85533'
)

find_var('labor income head')
find_var('labor income ref')
var_map[['inc_labor_rp']] <- tribble(
  ~year, ~code,
  1990, "V18878",
  1991, 'V20178',
  1992, 'V21484',
  1993, 'V23323',
  1994, 'ER4140',
  1995, 'ER6980',
  1996, 'ER9231',
  1997, "ER12080",
  1999, "ER16463",
  2001, "ER20443",
  2003, "ER24116",
  2005, 'ER27931',
  2007, 'ER40921',
  2009, 'ER46829',
  2011, 'ER52237',
  2013, 'ER58038',
  2015, 'ER65216',
  2017, "ER71293",
  2019, "ER77315",
  2021, "ER81642",
  2023, 'ER85496'
)
find_var('labor income wife')
find_var('labor income spouse')
var_map[['inc_labor_sp']] <- tribble(
  ~year, ~code,
  1990, "V17836",
  1991, 'V19136',
  1992, 'V20436',
  1993, 'V23324',
  1994, 'ER4144',
  1995, 'ER6984',
  1996, 'ER9235',
  1997, "ER12082",
  1999, "ER16465",
  2001, "ER20447",
  2003, "ER24135",
  2005, 'ER27943',
  2007, 'ER40933',
  2009, 'ER46841',
  2011, 'ER52249',
  2013, 'ER58050',
  2015, 'ER65244',
  2017, "ER71321",
  2019, "ER77343",
  2021, "ER81670",
  2023, 'ER85524'
)

check_var(var_map[['inc_total']]$code)
check_var(var_map[['inc_taxable_rs']]$code)
check_var(var_map[['inc_labor_rp']]$code)
check_var(var_map[['inc_labor_sp']]$code)


##### WEIGHT #####
find_var('fam cross weight')
find_var('fam weight cross')
var_map[['wt_fam_c']] <- tribble(
  ~year, ~code,
  1997, 'ER12224',
  1999, 'ER16519',
  2001, 'ER20459',
  2003, 'ER24180',
  2017, 'ER71571',
  2019, 'ER77632'
)
find_var('fam long weight')
find_var('fam weight')
var_map[['wt_fam_l']] <- tribble(
  ~year, ~code,
  1990, "V18945",
  1991, 'V20245',
  1992, 'V21549',
  1993, 'V23363',
  1994, 'ER4162',
  1995, 'ER7000B',
  1996, 'ER7000B',
  1997, 'ER12084',
  1999, 'ER16518',
  2001, 'ER20394',
  2003, 'ER24179',
  2005, 'ER28078',
  2007, 'ER41069',
  2009, 'ER47012',
  2011, 'ER52436',
  2013, 'ER58257',
  2015, 'ER65492',
  2017, 'ER71570',
  2019, 'ER77631',
  2021, 'ER81958',
  2023, 'ER85812'
)
find_var('ind cross weight')
var_map[['wt_ind_c']] <- tribble(
  ~year, ~code,
  1997, 'ER33438',
  1999, 'ER33547',
  2001, 'ER33639',
  2003, 'ER33742',
  2005, 'ER33849',
  2007, 'ER33951',
  2009, 'ER34046',
  2011, 'ER34155',
  2013, 'ER34269',
  2015, 'ER34414',
  2017, 'ER34651',
  2019, 'ER34864',
  2021, 'ER35065',
  2023, 'ER35265'
)
find_var('ind long weight')
find_var('ind weight')
var_map[['wt_ind_l']] <- tribble(
  ~year, ~code,
  1990, "ER30688",
  1991, 'ER30732',
  1992, 'ER30805',
  1993, 'ER30866',
  1994, 'ER33121',
  1995, 'ER33277',
  1997, 'ER33430',
  1999, 'ER33546',
  2001, 'ER33637',
  2003, 'ER33740',
  2005, 'ER33848',
  2007, 'ER33950',
  2009, 'ER34045',
  2011, 'ER34154',
  2013, 'ER34268',
  2015, 'ER34413',
  2017, 'ER34650',
  2019, 'ER34863',
  2021, 'ER35064',
  2023, 'ER35264'
)


check_var(var_map[[3]]$code, code)

for (i in 1:10) {
  check_var(var_map[[i]]$code, code)
}
