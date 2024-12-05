library(tidyverse)
library(googlesheets4)
##################################
#sheet id and sheet name to read in from sheets. 

sheet_id<-"https://docs.google.com/spreadsheets/d/1RYtJR9CDKXw0JWHGN1nWcqCCIOBYPpcNaew7DsWGs6U/edit?gid=2844248#gid=2844248"
name_of_sheet<-"Sheet1"

############
#authorize user.
gs4_auth(email = "gotrimski@gmail.com")

#read input sheet
cohorts0<-read_sheet(sheet_id, 
                      sheet = name_of_sheet)%>%
  mutate(across(1:last_col(), as.character))%>%
  
  #then replace all "NULL" with NA. 
  mutate(across(1:last_col(), function(x){na_if(x, "NULL")}))%>%
  
  janitor::clean_names()



#sheet id and sheet name to read in from sheets. 

sheet_id<-"https://docs.google.com/spreadsheets/d/1Vr8u_WcXR-VIqRJc1MUXVQobB4HBGgnhg8LTLnsYwOQ/edit?gid=433483277#gid=433483277"
name_of_sheet<-"updated_slides"

############
#authorize user.
gs4_auth(email = "gotrimski@gmail.com")

#read input sheet
slides0<-read_sheet(sheet_id, 
                           sheet = name_of_sheet)%>%
  mutate(across(1:last_col(), as.character))%>%
  
  #then replace all "NULL" with NA. 
  mutate(across(1:last_col(), function(x){na_if(x, "NULL")}))%>%
  
  janitor::clean_names()






included<-cohorts0%>%
  filter(is.na(exclude)|exclude>=4)%>%
  select(mouse_num)


cohorts1<-cohorts0%>%
  semi_join(included)



slides1<-slides0%>%
  semi_join(included)%>%
  select(mouse_num, organ, stain, patho_assigned:category_2, coh, gnotes_2, sheri_notes)%>%
  select(-aod, -x12)%>%
  relocate(coh)


comb<-left_join(cohorts1, slides1)



library(googlesheets4)
gs4_auth(email = "gotrimski@gmail.com")

comb%>%
  range_write("https://docs.google.com/spreadsheets/d/1RYtJR9CDKXw0JWHGN1nWcqCCIOBYPpcNaew7DsWGs6U/edit?gid=1334785396#gid=1334785396",
              sheet = "Sheet1_mod",
              .,
               reformat=FALSE,
              range = "A1")
