#########################################################
#filling in missing information in colony,
#resolving conflicting date data. 

library(tidyverse)
library(janitor)
library(beepr)
library(stringr)
library(lubridate)

readRDS("colony_genos2.rds")->colony_genos2

#there are 11 mice without death dates. Will need to check if 
#info is available elsewhere. Will combine this task
#with getting all the dates consistent across the datasets..

#date information from genotyping file.
colony_genos2%>%
  select(index, mouse_num, dob, death_date)%>%
  mutate(mouse_num = as.character(mouse_num))%>%
#appending g_ to ensure easy identification of source. 
  rename(g_dob = dob,
         g_death_date = death_date
         
         )->dates_geno

#getting date information from cohorts file...
read_csv("../mouse cohorts/compiled_cohorts.csv")%>%
  #selecting all date data from cohort dataset. 
  select(mouse_num, dob, injection_date,
         tumor_noticed,behavior_noticed,
         mri_imaging_date,
         death_date, end_date)%>%
  #prepping mouse_num column for future joins.
  mutate(mouse_num = as.character(mouse_num))%>%
  #use this function to append prefix to columns. 
  rename_with(.cols = 2:last_col(), ~paste0("c_", .x)
              
              )->dates_cohorts


#getting information from necropsy files. 

read_csv("colony_necropsy4.csv")%>%
  select(mouse_num,
         nec_date, sac_date, injection_date,
         dob)%>%
  

  rename_with(.cols = 2:last_col(), ~paste0("n_", .x))%>%
   mutate(mouse_num = as.character(mouse_num))%>%
  
  #making sure all relevant columns are in date format. 
  mutate_at(vars(matches("n_")), ~mdy(.x)
            
            )->dates_nec


dates_geno%>%
  full_join(dates_cohorts)%>%
  full_join(dates_nec)->dates_joined
  


####################################################################  
dates_joined%>%
  #removing any duplicated observations.
  distinct()%>%
  
  
  #checked for mismatches. taking g_dob as most accurate and correct. 
  #removing extraneous columns.
  select(-c_dob, -n_dob)%>%
  rename(dob = g_dob)%>%
  
  #death_dates
  select(index, mouse_num, dob,
         g_death_date, c_death_date, n_nec_date, n_sac_date)%>%
  
  #n_nec_date is mostly unnecessary and some times inaccurate.
  #it reflects only the date that the file was filled out,
  #not the actual date the nec was performed... as that date IS the death date.
  #keeping in for now as a way to corroborate other mismatches. 
  

  #using to check any obvious input mistakes where dob>death_date.
  mutate(flag = if_else(dob>g_death_date|
                        dob>c_death_date|
                        dob>n_sac_date, "1", "0"))%>%
  
  #recognized 1 error and fixed.
  mutate(g_death_date = if_else(index =="13249", as.Date("2022-01-21"), g_death_date))%>%
  select(-flag)%>%
 
  
  #inserting some columns to get info about differences
  #between date columns. abs() is absolute value. 
  mutate(diff1 = abs(g_death_date-n_sac_date))%>% 
  mutate(diff2 = abs(c_death_date-n_sac_date))%>%
  mutate(diff3 = abs(g_death_date - c_death_date))%>%
  
  #calculating mean.  
  #first, durations need to be numeric.  
  mutate_at(c("diff1", "diff2", "diff3"), ~as.numeric(.x))%>%
  #code allows calcuation to ignore NAs. 
  mutate(diff = round(rowMeans(across(c("diff1", "diff2", "diff3")), na.rm=TRUE), 1))%>%
  #remove all but the avg - not really necessary at this point. 
  select(-diff1, -diff2, -diff3)%>%
  
  
  #adding a possible exclusion for this obs, and note to go along.
  mutate(exclude = ifelse(index == "10118", "2", NA))%>%               
  mutate(ex_reason = ifelse(index == "10118", "death date discrepency", NA))%>%                      
            
  #based on discussions with team, sac date recorded in necropsy
  #should be the most accurate. Ignoring other date observations
  #and using that as the official death date. 
  mutate(death_date = n_sac_date)%>%
  
  #checking to see what the supposed age of the mice would be
  #based on the alleged death dates. 
  mutate(g_age = g_death_date - dob)%>%
  mutate(c_age = c_death_date - dob)%>%

  relocate(diff, .after = last_col()
           
           )->all_dates
  

#saving 1 version of good_dates, removing extraneous columns. 
#where death_date is filled, discrepancies are resolved.
all_dates%>%
  filter(!is.na(death_date))%>%
  select(c(1:3, 9:last_col()))%>%
  distinct(
    
      )->good_dates1


#another way dates are resolved is where there are no 
#disagreements between dates that are recorded. 
all_dates%>%
  filter(is.na(death_date))%>%
  #where there is no discrepancy, we have the death date.--
  #unless there is NO recorded death date.
  filter(diff == 0|is.na(diff))%>%
 
  #move recorded, non-conflicting values to death_dates.
  mutate(death_date = if_else(!is.na(c_death_date), as.Date(c_death_date),
                             if_else(!is.na(g_death_date), as.Date(g_death_date),
                                   NA_Date_)))%>%
  
  #remove obs that have no recorded death dates. 
  filter(!is.na(death_date))%>%
  #remove extraneous columns. 
  select(c(1:3, 9:last_col()))%>%
  #remove duplicates and save. 
  distinct(
      
      )->good_dates2



#join the 2 good_dates together and save.  
full_join(good_dates1, good_dates2)%>%
  distinct(
    
      )->good_dates








all_dates%>%
#this leaves the issue of resolving death dates on 
  #mice that were NOT necropsied...
  #AND still have conflicting recorded dates...
  #or some other issue. 
  anti_join(good_dates, by = "mouse_num")%>%
  

  
  
  #any time both recorded dates result in mouse being older than 150 days,
  #use arbitrary date which is 150 days after birth.
  mutate(death_date = if_else(c_age>=duration(150, "days")&
                               g_age>=duration(150, "days"), 
                             as.Date(dob+150),
                             death_date))%>%
  #on these mice, add a data note. 
  mutate(metadata = if_else(!is.na(death_date),
                           "disagreement in recorded death dates, but both above 150 days",
                           NA_character_))%>%

  
  #remove any all-na columns for easier viewing. 
  select(where(function(x) any(!is.na(x))))%>%
  
  #filter for good dates, select necessary columns. 
  filter(!is.na(death_date))%>%
  select(c(1:3, "death_date", "metadata"))->good_dates3
  
  
  
#appending to good_dates. 
full_join(good_dates, good_dates3)%>%
  distinct(
     
      )->good_dates

saveRDS(good_dates, "good_dates.rds")
  


all_dates%>%
  #this leaves the issue of resolving death dates on 
  #the truly BAD DATES. 
  anti_join(good_dates, by = "mouse_num")%>%

  select(where(function(x) any(!is.na(x))))%>%
  distinct()->bad_dates
  
  
saveRDS(bad_dates, "bad_dates.rds")  
  
  
  
  # view();beep(sound=5)

