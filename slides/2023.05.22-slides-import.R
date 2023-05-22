#standard libraries. 
#pulling dataset for all physical slides in lab. 


library(tidyverse)
library(janitor)

library(googlesheets4)
gs4_auth(email = "gotrimski@gmail.com")


read_sheet("1bdya6WH1KA-w3ziKprTpbxd-S1zJh1zPef754b8eDKY")%>%

  mutate(across(1:last_col(), as.character))%>%
  
  #then replace all "NULL" with NA. 
  mutate(across(1:last_col(), function(x){na_if(x, "NULL")%>%
      na_if("^NA$")%>%
      tolower()
    }))%>%
  
  as_tibble()%>%
  
  clean_names()%>%
  
  filter(!is.na(mouse_id))%>%
  
  saveRDS("slides/nf1_slides.rds")
  
