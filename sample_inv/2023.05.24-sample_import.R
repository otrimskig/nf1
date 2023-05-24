library(googlesheets4)
library(tidyverse)
library(janitor)
library(lubridate)

#pull in data from google sheet. 

#authorize user
gs4_auth(email = "gotrimski@gmail.com")


df<-read_sheet("1iK1inudWFhjNRAjim7ygs1fJROHg-EjM-qXTQOz3uos",
               col_names = FALSE,
               col_types = "c")%>%
  clean_names()%>%
  mutate(across(1:last_col(), tolower))%>%
  mutate(index = 1:nrow(.))%>%
  relocate(index)


df%>%
  view()


df
