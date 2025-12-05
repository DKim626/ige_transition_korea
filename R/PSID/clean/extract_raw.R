library(dplyr)
library(tidyr)
library(readr)
library(tibble)
library(readxl)

data <- read_csv(here::here('data/PSID/raw/J353793.csv'))


data <- tibble(data)

saveRDS(data, file = here::here('data/PSID/raw/psid_data.rds'))

rm(data)
