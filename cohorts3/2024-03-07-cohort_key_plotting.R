library(tidyverse)

cohorts<-readRDS("ds/nf1_cohorts.2.dates.rds")
n_size<-cohorts%>%
  count(resultant_geno)

cohort_conditions<-cohorts%>%
  group_by(strain_injection, strain, injected_with, resultant_geno)%>%
  slice(1)%>%
  select(strain_injection, strain, injected_with, resultant_geno)


