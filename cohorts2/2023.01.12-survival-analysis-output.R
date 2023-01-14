####################################
#2023.01.12
#creating function to easily export survival analysis data. 

library(tidyverse)
library(survminer)
library(survival)

library(fmtr)
library(libr)


readRDS("cohort3_survival2.rds")->cohort_survival



cohort_survival%>%
  count(genes_ko)->genes_ko
  
genes_ko%>%
  mutate(group = 1:6)->genes_ko2


genes_ko%>%
  mutate(group = c("a", "b", "c", "d", "e", "f"))->genes_ko2




cohort_survival%>%
  left_join(genes_ko2, by = "genes_ko")->cohort_survival2









pairwise_survdiff(Surv(time = age_death2, event = event) ~ cohort, 
                  data = cohort_survival2, p.adjust.method = "BH"
                  
                  )->pairwise


#get names to be added to the results table below. 
attributes(pairwise[["p.value"]])[["dimnames"]][[1]]->cohort


pairwise[["p.value"]]%>%
  as_tibble()%>%
  mutate(cohort = cohort)%>%
  relocate(cohort)%>%
  arrange(cohort)->results



#pull metadata from elseware in results object. 
names(pairwise)%>%
  as_tibble()%>%
  rename(col1 = value)%>%
  full_join(tibble(col1 = "date.of.analysis"))%>%
  filter(col1 != "p.value")%>%
  
  mutate(col2 = c((pairwise)[[1]], 
                  (pairwise)[[2]], 
                  (pairwise)[[4]], 
                  as.character(Sys.Date())))->metadata



results

metadata


