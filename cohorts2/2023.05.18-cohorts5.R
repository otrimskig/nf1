####################################
#2023.05.18
#building data for tumor severity from cohorts ds. 

library(tidyverse)
library(lubridate)
library(googlesheets4)
setwd("cohorts2/")

readRDS("ds/cohorts4.0.rds")->cohorts4


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



#has_slidescan variable is inaccurate. removing. 

cohorts4%>%
  select(-has_slidescan)->cohorts4.0.1

cohorts4.0.1%>%
  saveRDS("ds/cohorts4.0.1.rds")



#for now, let's deal with the slides of mice sheri HASN'T seen. 

cohorts4.0.1%>%
  filter(is.na(sheri_seen))%>%
  filter(!is.na(necropsy_file_path))->needs_look
  
  
ihc_files%>%
  select(mouse_num, img_sample, staining, img_path)%>%
  group_by(mouse_num)%>%
  mutate(is_he = if_else(staining == "H&E",1,0))%>%
  arrange(desc(is_he))%>%
  
  slice(1)%>%
  ungroup()%>%
  select(-is_he)%>%
  mutate(mouse_num = as.character(mouse_num))%>%
  filter(!is.na(img_path))->ihc_img_paths


needs_look%>%
  left_join(ihc_img_paths)%>%
  view()
