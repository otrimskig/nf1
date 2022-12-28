################################################################
#2022.12.27
#2022.12.28
#adding dates and corrected genotypes to colony info. 


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
  
  
  
  
  write.csv("out/bad_dates_print.csv", na="")
  
  
  
#adding in dob from bad_dates. 
cohorts1.2%>%
  filter(is.na(dob))%>%
  select(-dob)%>%
  left_join(bad_dates%>%select(mouse_num, dob), by = "mouse_num")%>%
  full_join(cohorts1.2%>%filter(!is.na(dob)))->cohorts1.3
  
  


cohorts1.3%>%
  
  #removing unnecessary column. 
  select(-c_dob)%>%
  
  relocate(dob, .after = "sex")%>%
  relocate(death_date, .after = "mri_imaging_date")%>%
  
  #adding exclusion data and meta notes. 
  mutate(exclude = if_else(is.na(death_date), "2", exclude))%>%
  mutate(metadata = if_else(is.na(death_date), "contested death date", metadata))%>%
  
  #based on comments, these mice were both sacked for end date - even though their 
  #death dates are disputed. 
  mutate(exclude = if_else(mouse_num == "29389"| mouse_num=="29390", 
                           NA_character_, exclude))%>%
  
  mutate(metadata = if_else(mouse_num == "29389"| mouse_num=="29390", 
                           "contested death dates but notes say sacked for endpoint", metadata)
         )->cohorts1.4
  
#not sure why some date columns aren't in date form. Fixing that.  
cohorts1.4%>%
  mutate_at(c("injection_date", "tumor_noticed", "behavior_noticed",
              "mri_imaging_date", "end_date"), mdy
            )->cohorts1.5
  
 
#resolving additional dates in separate df.
cohorts1.5%>%
  select(mouse_num, c(dob:end_date), comments)%>%
  
  filter(is.na(death_date))%>%
  select(where(function(x) any(!is.na(x))))%>%
  left_join(bad_dates%>%
              select(mouse_num, g_death_date, c_death_date),by = "mouse_num")%>%
  mutate(dd = NA)%>%
  
  #dates resolved with notes attached in metadata. 
  mutate(dd = if_else(mouse_num == "23297"|
                        mouse_num =="25034", c_death_date, as.Date(dd)))%>%
  mutate(metadata = if_else(mouse_num == "23297"|
                              mouse_num =="25034", "behavior data was after purported death date in genotyping. Used cohort dd.", 
                            NA_character_))%>%
 
  #dates resolved with notes attached in metadata. 
  mutate(metadata = if_else(is.na(dd)&!is.na(end_date), 
                            "end date in cohort data was same as cohort death date, so used cohort death date.",
                            metadata))%>%
  
  mutate(dd = if_else(is.na(dd)&!is.na(end_date), end_date, as.Date(dd)))%>%

  select(-dob, -injection_date)%>%
  
  filter(!is.na(dd))%>%
  select(where(function(x) any(!is.na(x))))%>%
  select(mouse_num, dd, metadata)->resolved_dd1


#now adding all other information to the resolved dates. 
cohorts1.5%>%
  semi_join(resolved_dd1, by = "mouse_num")%>%
  left_join(resolved_dd1, by = "mouse_num")%>%
  
  mutate(exclude = NA)%>%
  select(-death_date)%>%
  rename(death_date = dd)%>%
  
  unite("metadata", c("metadata.x", "metadata.y"), sep = " .. ")->resolved_dd2

#removing resolved date mouse numbers and replacing with updated obs
#into main dataset. 
cohorts1.5%>%
  anti_join(resolved_dd2, by = "mouse_num")%>%
  
  #now has all notes of changes made in metadata.
  #those with still-unresolved death dates are left NA, plus
  #noted with a "2" in the exclude column,
  #plus noted the issue in metadata. 
  full_join(resolved_dd2)->cohorts1.6


cohorts1.6%>%
  filter(is.na(death_date))%>%
  
  left_join(bad_dates)%>%
  
  mutate(death_date = as.Date(c_death_date))%>%
  select(-c(index:last_col()))%>%
  
  #using rick's cohort data for last remaining unresolved dates.
  #retaining 2 in exclude column until more satisfactory resolution. 
  
  mutate(metadata = paste0(metadata, " .. ", 
      "used rick's cohort data as correct death date. No other justification other than hoping this was most accurate. 
For the corrected data in this group, all cohort dds were AFTER geno dds. Retaining '2' in exclusion column until more satisfactory resolution.")
)->resolved_dd3
  


cohorts1.6%>%
  #adding resolved data to main df. 
  anti_join(resolved_dd3, by = "mouse_num")%>%
  full_join(resolved_dd3)->cohorts2


#all date obs are now fully resolved or resolved with notes in exclusion/metadata columns.  
saveRDS(cohorts2, "cohorts2.rds")

