################################################################
#2022.12.27

library(tidyverse)
library(lubridate)

readRDS("cohorts1.rds")->cohorts1
readRDS("../necropsy files/colony_genos2.rds")->colony_genos
readRDS("../necropsy files/bad_dates.RDS")->bad_dates
readRDS("../necropsy files/good_dates.RDS")->good_dates


#adding genotypes to cohort data. 
cohorts1%>%
  mutate(mouse_num=as.character(mouse_num))%>%
  left_join(colony_genos, by = "mouse_num")%>%
  rename(c_dob = dob.x)%>%
  select(-death_date.x, -dob.y, -death_date.y)%>%
  relocate(comments, g_comments, exclude, .after = last_col())%>%
  
  select(-index, -mouse_id, -cage_strain, -dtva, -cage_number, -injected_by)%>%
  
  unite("comments", c("comments", "g_comments"), na.rm = TRUE, sep = " .. ")%>%
  
  relocate(end_date, .after = "mri_imaging_date")%>%
  relocate(sex, .after = "mouse_num")%>%
  relocate(found_dead, .after = "end_date")%>%
  rename(cells_injected = number_of_cells_injected)->cohorts1.1

#adding in good_dates. 
cohorts1.1%>%
  left_join(good_dates, by = "mouse_num")%>%
  unite("exclude", c("exclude.x", "exclude.y"), na.rm=TRUE)%>%
  mutate(exclude = if_else(exclude =="", NA_character_, exclude))%>%
  select(-index, -ex_reason)->cohorts1.2
 

bad_dates%>%
  left_join(cohorts1.2, by = "mouse_num")%>%
rename(g_dob = dob.x)%>%
  select(-sex, -index, -cells_injected)%>%
  relocate(mouse_num, 
           injection_date,
           g_dob,
           g_death_date,
           g_age,
           c_dob,
           c_death_date,
           c_age,
           diff,
           end_date,
           virus,
           comments)%>%
  select(where(function(x) any(!is.na(x))))%>%
  mutate(injection_date = mdy(injection_date))%>%
  mutate(c_dob = mdy(c_dob))%>%
  select(-c_dob)%>%
  relocate(c_death_date, .after = g_death_date)%>%
  mutate(diff = c_age-g_age)%>%
  rename(dob = g_dob)%>%
  
  
  
  
  write.csv("bad_dates_print.csv", na="")
  
  
  



