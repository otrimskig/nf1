#R input using Data to R sheet
#necessary libraries
library(tidyverse)
library(googlesheets4)


##################################
#sheet id and sheet name to read in from sheets. 

sheet_id<-"https://docs.google.com/spreadsheets/d/1Vr8u_WcXR-VIqRJc1MUXVQobB4HBGgnhg8LTLnsYwOQ/edit#gid=433483277"
name_of_sheet<-"updated_slides"

############
#authorize user.
gs4_auth(email = "gotrimski@gmail.com")

#read input sheet
sheets_df<-read_sheet(sheet_id, 
                      sheet = name_of_sheet)%>%
  mutate(across(1:last_col(), as.character))%>%
  
  #then replace all "NULL" with NA. 
  mutate(across(1:last_col(), function(x){na_if(x, "NULL")}))%>%
  
  janitor::clean_names()



df2<-sheets_df%>%
  mutate(definitive= if_else(patho_assigned=="1", "1", "0"))%>%
  
  mutate(definitive=if_else(grepl("\\?", category)|grepl("3\\.1", category), "0", "1"))%>%
  
  mutate(definitive=if_else(!is.na(tnp), "0", definitive))%>%
  
  mutate(definitive=if_else(is.na(category), NA_character_, definitive))
  
  


gs4_auth(email = "gotrimski@gmail.com")

df2%>%
  select(definitive)%>%
  range_write(sheet_id,
              sheet = name_of_sheet,
              .,
               reformat=FALSE,
              range = "H1")
