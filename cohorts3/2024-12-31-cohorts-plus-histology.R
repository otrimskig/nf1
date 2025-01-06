library(tidyverse)
library(googlesheets4)
##################################
#sheet id and sheet name to read in from sheets. 

sheet_id<-"https://docs.google.com/spreadsheets/d/1Vr8u_WcXR-VIqRJc1MUXVQobB4HBGgnhg8LTLnsYwOQ/edit?gid=433483277#gid=433483277"
name_of_sheet<-"updated_slides"

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




cohorts1<-cohorts0%>%
  select(-c(category_2:last_col()))%>%
  select(where(~ !all(is.na(.))))




c03_go<-cohorts0%>%
  filter(!is.na(def_hist_11_18_24_go))



c01_patho<-cohorts1%>%
  filter(patho_assigned==1)%>%
  
  mutate(hist=category_1)%>%
  group_by(mouse_num)%>%
  filter(!is.na(hist))%>%
  
  select(mouse_num, hist)%>%
  mutate(patho_assigned=1)%>%
  mutate(definitive=1)%>%
  unique()






c02_def<-cohorts1%>%
  anti_join(c01_patho, by="mouse_num")%>%
  filter(definitive==1)%>%

  mutate(hist=category_1)%>%
  group_by(mouse_num)%>%
  filter(!is.na(hist))%>%
  
  #select(mouse_num, hist)%>%
  mutate(definitive=1)%>%
  unique()


c02_def_counts<-c02_def%>%
  count(mouse_num)


c02_def_single<-c02_def%>%
  semi_join(c02_def_counts%>%filter(n==1), by="mouse_num")

c02_def_mult<-c02_def%>%
  semi_join(c02_def_counts%>%filter(n>1), by="mouse_num")





comb<-bind_rows(c01_patho, c02_def_single)


