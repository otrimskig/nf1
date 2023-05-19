####################################
#2023.05.18
#building data for tumor severity from cohorts ds. 

library(tidyverse)
library(lubridate)
library(googlesheets4)


readRDS("ds/cohorts4.0.rds")->cohorts4


cohorts4%>%
 
  view()






#this is a df of all the saved slide scans relating to mice in the nf1 g colony. 
#it is unclear whether all mice with necropsy files have been scanned, but I believe that is the case.
read_csv("../necropsy files/colony_nec_ihc1.csv")->ihc_files

#to confirm:
ihc_files%>%

  select(mouse_num, img_sample, staining, img_path)%>%
  
  group_by(mouse_num)%>%
  slice(1)%>%
  ungroup()%>%
  
  filter(!is.na(img_path))%>%
  select(mouse_num)%>%
  mutate(mouse_num = as.character(mouse_num))%>%

  mutate(has_slidescan = 1)->mice_w_slidescan

#getting mice that have necropsy files, using existence of 
#nec file path in df. Comparing that to the mice that have slidescans. 

cohorts4%>%
  mutate(has_nec_file = if_else(!is.na(necropsy_file_path), 1, 0))%>%
  select(mouse_num, has_nec_file)%>%
  left_join(mice_w_slidescan)%>%
  mutate(has_slidescan = if_else(is.na(has_slidescan), 0,1))%>%
  filter(has_nec_file != has_slidescan)

#ok so there are actually 199 mice that only have either a slidescan or a nec file. 


ihc_files%>%
  view()



cohorts4%>%
  select(-has_slidescan)->cohorts4.0.1

cohorts4.0.1%>%
  view()


cohorts4.0.1%>%
  filter(is.na(sheri_seen))%>%
  filter(!is.na(necropsy_file_path))%>%
  filter(is.na(exclude))%>%
  select(mouse_num,
         dob,
         death_date,
         comments, 
         n_comments,
         cohort)%>%
  group_by(cohort)%>%
  arrange(cohort, mouse_num)%>%
 

  
  sheet_write(., "1hgBGd7-q2zvlb4hE0xF7zJBYQ6R1TSnkpt0fXDwlB9E")
