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
  
  
dates_joined%>%
  
  distinct()%>%
  #select dob and check for mismatches. 
  #select(mouse_num, contains("dob"))%>%
  #filter(g_dob!=c_dob)%>%
  
  #mismatches corrected. taking g_dob as correct. 
  select(-c_dob, -n_dob)%>%
  rename(dob = g_dob)%>%
  
  #death_dates
  select(index, mouse_num, 
         g_death_date, c_death_date, n_nec_date, n_sac_date)%>%
  
  mutate(dd = if_else(g_death_date==c_death_date&
                        g_death_date==n_nec_date&
                        g_death_date==n_sac_date, "1", "0"))%>%
  
  filter(dd ==0)%>%
  filter(n_nec_date!=n_sac_date)%>%
  
  
  distinct()%>%
  view()

