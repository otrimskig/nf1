####################################
#2023.01.13
#more changes to cohorts data.
#checking for wrongly-sacced mice (d147-149-ish)

library(tidyverse)
library(lubridate)



readRDS("ds/cohorts3.rds")->cohorts3


cohorts3%>%
  
mutate(aod = as.numeric(death_date-injection_date))%>%
  

  mutate(exp_endpt = NA)%>%
  relocate(mouse_num, aod, exp_endpt)%>%
  arrange(desc(aod))%>%
  
  
  
  
  mutate(exp_endpt = if_else(is.na(exp_endpt)&
                            (grepl("experimental endpoint", comments, ignore.case = TRUE)|
                              grepl("end of experiment", comments, ignore.case = TRUE)|
                               grepl("Sac'd for end date", comments, ignore.case = TRUE)|
                                grepl("sacked for end date", comments, ignore.case = TRUE)|
                            grepl("experimental endpoint", n_comments, ignore.case = TRUE)|
                             grepl("end of experiment", n_comments, ignore.case = TRUE)|
                              grepl("Sac'd for end date", n_comments, ignore.case = TRUE)|
                               grepl("sacked for end date", n_comments, ignore.case = TRUE)),
                            
                        "1", as.character(exp_endpt)))->cohorts3.1
  
  



  cohorts3.1%>%
  filter(exp_endpt == "1"&aod<147)%>%
  
  mutate(exclude = "1")%>%
  mutate(metadata = paste(metadata, "...", "exluded for conflicting information in comments and sac dates. Exp_endpt changed to \"2\" to denote uncertain status."))%>%
  mutate(exp_endpt = "2")%>%
  mutate(metadata = gsub("^NA ... ", "", metadata))->update1
    
  
    
    
  cohorts3.1%>%
    anti_join(update1, by = c("mouse_num"))%>%
    full_join(update1)->cohorts3.2

  view(cohorts3.2)
  
  
  cohorts3.2%>%
    select(mouse_num, cohort, comments, n_comments, exclude, metadata, nec_NED)%>%
    filter(is.na(nec_NED))%>%
    view()
    
    

   
 
  # 
  # cohorts3.2%>%
  # summarise_all(class) %>% 
  #   gather(col_name, col_type) %>% 
  #   mutate(desc = NA)%>%
  #   arrange(col_name)%>%
  #   write_csv("meta.csv")
    

  
  
