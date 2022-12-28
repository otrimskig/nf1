##############
#cleaning bad dates. 

library(tidyverse)
library(lubridate)


readRDS("../necropsy files/good_dates.rds")->good_dates
readRDS("../necropsy files/bad_dates.rds")->bad_dates
readRDS("../necropsy files/colony_genos2.rds")->col_genos


view(col_genos)
