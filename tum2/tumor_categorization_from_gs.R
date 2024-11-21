library(tidyverse)


##################################
#sheet id and sheet name to read in from sheets. 

sheet_id<-"https://docs.google.com/spreadsheets/d/1Vr8u_WcXR-VIqRJc1MUXVQobB4HBGgnhg8LTLnsYwOQ/edit?gid=433483277#gid=433483277"
name_of_sheet<-"updated_slides"


############
#authorize user.

library(googlesheets4)
gs4_auth(email = "gotrimski@gmail.com")

#read input sheet
sheets_df<-read_sheet(sheet_id, 
                      sheet = name_of_sheet)%>%
  mutate(across(1:last_col(), as.character))%>%
  
  #then replace all "NULL" with NA. 
  mutate(across(1:last_col(), function(x){na_if(x, "NULL")}))%>%
  
  janitor::clean_names()




df2<-sheets_df
