####################################
#2022.12.27

#cohort data import and conversion.
#getting one obs per mouse number, 
#taking only 1-obs per mouse variables. 


library(tidyverse)
library(janitor)

#last unadulterated version of cohort data.
#cleaned and joined data from rick and riley.
read_csv("../mouse cohorts/compiled_cohorts3.csv")->compiled_cohorts

compiled_cohorts%>%
  clean_names()%>%
  
  select(-x2)%>%
  select(-had_tumor)%>%
  
  
  #selecting only columns that would be unique to EACH mouse. 
  
  select(mouse_num,
         dob,
         injection_date, 
         tumor_noticed,
         behavior_noticed,
         death_date,
         end_date,
         mri_imaging_date,
         notes,
         injected_by,
         src,
         necropsy_comments,
         number_of_cells_injected,
         vol_injected,
         necropsy_behavior_comments,
         brain_h_e_and_ihc_notes,
         virus,
         exclude,
         found_dead)%>%
  distinct()->cohort_each_mouse
  
#checking for duplicate mouse numbers. 
cohort_each_mouse%>%
count(mouse_num)%>%
  filter(n ==2)->duplicates 
  

#adding column to weed out duplicates. 
cohort_each_mouse%>%
  left_join(duplicates, by = "mouse_num")%>%
  filter(n == 2)%>%
  
  #all duplicated mice have missing information in the 2nd obs. 
  group_by(mouse_num)%>%
  slice(1)%>%
  ungroup()->duplicates_fixed

#removing duplicates and replacing with the fixed data. 
cohort_each_mouse%>%
  anti_join(duplicates_fixed, by = "mouse_num")%>%
  full_join(duplicates_fixed)%>%
  
  select(where(function(x) any(!is.na(x))))%>%
  
  
  #uniting all qualitative columns. 
  unite("comments", c("notes", "necropsy_comments",
                      "necropsy_behavior_comments", 
                      "brain_h_e_and_ihc_notes"), na.rm = TRUE, sep = " .. ")%>%
  relocate(comments, exclude, .after = last_col())%>%
  
  select(-n)%>%

  #has almost all unique-to-each-mouse variables, with no duplicated data. 
  #data not included in this are either messy, can be re-calculated, or both. 
  #some useful data may still remain in compiled_cohorts3.csv that is not captured here,
  #but this is a good start.
  
  saveRDS("cohorts1.RDS")
  
  
  
