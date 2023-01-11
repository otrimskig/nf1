####################################
#2022.12.28
#more changes to cohorts data.
#adding in necropsy data. 

library(tidyverse)
library(lubridate)
#used for paste_na
library(Kmisc)

readRDS("cohorts2.rds")->cohorts2

read_csv("../necropsy files/colony_necropsy3.csv")%>%
  mutate(mouse_num = as.character(mouse_num))->col_nec

read_csv("../necropsy files/colony_necropsy4.csv")%>%
  mutate(mouse_num = as.character(mouse_num))->col_nec4

read_csv("../necropsy files/colony_necropsy5.m.csv")%>%
  mutate(mouse_num = as.character(mouse_num))->col_nec5


#this was already done but double-checking.
#getting sac_dates from necropsy obs.
col_nec%>%
  semi_join(cohorts2, by = "mouse_num")%>%
  select(mouse_num, nec_date, sac_date)%>%
  mutate_at(c("nec_date", "sac_date"), mdy)%>%
  select(-nec_date)->nec_date

#checking dates against sketchy death_dates in main df.
#turns out there is no necropsy info for these mice. 
cohorts2%>%
  filter(!is.na(metadata))%>%
  left_join(nec_date, by = "mouse_num")%>%
  relocate(sac_date, .after = "end_date")%>%
  view()





#integrating necropsy comments and other info into main df. 

#extracting from necropsy df.
col_nec5%>%
  select(mouse_num, c(comments:last_col()))%>%
  rename(n_comments = comments)->col_nec5.sub
  
#adding to main df. 
cohorts2%>%
  left_join(col_nec5.sub, by = "mouse_num")%>%
  select(where(function(x) any(!is.na(x))))%>%
  
  #checked for mismatches - only mismatches came where 
  #animal was already excluded. 
  #filter(exclude!=xclude)%>%
  select(-xclude)%>%
  
  relocate(exclude, metadata, .after = last_col()

           )->cohorts2.1




#now separating mice into their cohorts based on
#combination of genotype and virus delivery. 
#changing pwt to +/+. If these need to be referenced in the future
#cohorts2.1 will be the last dataset that retains that info. 
#downstream df will all have +/+ instead of pwt for sake of
#analysis. 
cohorts2.1%>%
  mutate_at(vars(ntva:cic), ~gsub("pwt", "+/+", .x))%>%
  
  
  #sorting only mice with ntva. 
  mutate(cohort = if_else(ntva == "tg/+", 
                          paste0("ntva:: ",
                                 "pten ", pten,"; ", 
                                 "ink4a ", ink4a,"; ",
                                 "atrx ", atrx,"; ",
                                 "h11cas9 ", h11cas9,"; ",
                                 
                                 #excluded since all mice were
                                 #delivered cre.
                                 #should it be included??
                                 #"tyr_cre ", tyr_cre,"; ",
                                
                                  "cic ", cic),
                          NA_character_))%>%
  
  #adding nf1 info to cohort based on virus delivered. 
 #condition1
   mutate(cohort = if_else(ntva == "tg/+"&grepl("ex2", virus), 
                          paste0(cohort, "; ", "nf1 ko"),
                    #cond 2.     
                  if_else(ntva == "tg/+",
                          paste0(cohort, "; ", "nf1 +/+"),
                         #neg
                          cohort))
         
         )->cohorts2.2


cohorts2.2%>%
  
 #removing all non-wt cic from cohort per sheri's recommendation. 
  filter(cic!="f/f")%>%
  
  #removing column
  select(-cic)%>%
  
  #removing from the cohort value.
  mutate(cohort = sub("cic \\+\\/\\+\\; ", "", cohort))->cohorts2.3
  
  
  
  
  
cohorts2.3%>%

mutate(exclude = ifelse(ntva == "+/+", 1, exclude))%>%
mutate(metadata = ifelse(ntva=="+/+", paste_na("ntva-neg", metadata), metadata))->cohort2.4


cohort2.4%>%
  select(mouse_num,
         dob, 
         injection_date,
         death_date,
         exclude,
         metadata,
         cohort)%>%
  saveRDS("cohort3_survival.rds")









