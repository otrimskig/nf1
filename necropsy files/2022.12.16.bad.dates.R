####################################################################
#resolving truly bad conflicting recorded death dates in the colony. 


library(tidyverse)
library(lubridate)

readRDS("bad_dates.rds")->bad_dates


view(bad_dates)

#first approach. Check genotypes. If not in experimental group,
#much easier to exclude. 

readRDS("colony_genos2.rds")->colony_genos

