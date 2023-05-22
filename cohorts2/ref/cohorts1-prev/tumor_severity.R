library(tidyverse)
library(janitor)


#creating dataset with tumor grades. 

#get structure of main dataset to check for usefull variables. 
read_csv("compiled_cohorts3.csv")%>%
  str()



#read in main dataset
read_csv("compiled_cohorts3.csv")%>%
  #filter out mice with no tumors. 
  filter(had_tumor ==1 | is.na(had_tumor))%>%
  #select potentially useful 
  select(mouse_num,
        had_tumor,
        patho_notes,
        patho_tumor_grade,
        tumor_grade_post_h_e,
        notes,
        necropsy_comments,
        necropsy_behavior_comments,
        brain_h_e_and_ihc_notes)%>%
  write.csv("tumor_grade1.csv")

  
  view()

