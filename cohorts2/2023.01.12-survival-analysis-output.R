####################################
#2023.01.12
#creating function to easily export survival analysis data. 

library(tidyverse)
library(survminer)
library(survival)
library(googlesheets4)

##########################################################
#get all data into R. 

readRDS("ds/cohort3_survival2.rds")->cohort_survival

#get genes_ko df
cohort_survival%>%
  count(genes_ko)%>%
  mutate(group = 1:6)->genes_ko

#merge with cohort_survival. 
cohort_survival%>%
  left_join(genes_ko, by = "genes_ko")->cohort_survival2




#########################################################
#create survival diff object to be used for analysis. 
#set inputs for analysis here. 
#
pairwise_survdiff(Surv(time = age_death2, 
                  
                  #event information. 
                  event = event)
                 
                  #grouped by
                  ~ genes_ko, 
                  
                  #dataset
                  data = cohort_survival2, 
                  
                  #adjustment method. 
                  p.adjust.method = "none"
                  
                  )->pairwise


#get names to be added to the results table below. 
attributes(pairwise[["p.value"]])[["dimnames"]][[1]]->group_a



#############################################################
#choose output version:


#3-column version:
# pairwise[["p.value"]]%>%
#   as_tibble()%>%
#   mutate(group_a = group_a)%>%
#   relocate(group_a)%>%
#   arrange(group_a)%>%
#   pivot_longer(cols=2:6, names_to = "group_b", values_to = "p_value")%>%
#   filter(!is.na(p_value))->results



#square comparison version:
# pairwise[["p.value"]]%>%
#   as_tibble()%>%
#   mutate(group = group_a)%>%
#   relocate(group)%>%
#   arrange(group)->results
  
  


#square version with duplicated info (my personal preference)
pairwise[["p.value"]]%>%
  as_tibble()%>%
  mutate(group_a = group_a)%>%
  relocate(group_a)%>%
  arrange(group_a)%>%
  pivot_longer(cols=2:6, names_to = "group_b", values_to = "p_value")%>%
  mutate(group_c = group_a)->table


table%>%
  select(group_a, group_b, p_value)->tableab


table%>%
  select(group_b, group_c, p_value)%>%
  rename(group_a=group_b, group_b=group_c)->tablebc


full_join(tableab, tablebc)%>%
  filter(!is.na(p_value))%>%
  pivot_wider(names_from = "group_b", values_from = "p_value")%>%
  arrange(group_a)%>%
  rename(group = group_a)%>%
  mutate(num = c(1:6))%>%
  relocate(num)%>%
  rename_at(vars(3:last_col()), ~as.character(c(1:6)))%>%
  mutate_all(function(.x) (if_else(is.na(.x), "-", as.character(.x)))
             
             )->results
  


###########################################################
#pull metadata for results object. 
names(pairwise)%>%
  as_tibble()%>%
  rename(col1 = value)%>%
  full_join(tibble(col1 = c("date.of.analysis")))%>%
  filter(col1 != "p.value")%>%
  arrange(col1)%>%
  mutate(col2 = c((pairwise)[[2]],
                  as.character(Sys.time()),
                  (pairwise)[[1]],
                    (pairwise)[[4]]
                  ))->metadata




##############################################################
#output

#creates google sheet with both data tables as separate sheets.
#uses Sys.Date to name sheet with today's date. 

gs4_create(paste(as.character(Sys.time()), 
                 
    #name of output file
                 "pairwise analysis test-no adj"),
           
 
    
    
sheets = list(results = results, test_info = metadata))






       