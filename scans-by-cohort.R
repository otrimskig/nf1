#R input using Data to R sheet
library(googlesheets4)
library(tidyverse)
library(janitor)
library(lubridate)
library(fs)


#authorize user
gs4_auth(email = "gotrimski@gmail.com")

#read input sheet
df1<-read_sheet("1_Kk3q0OkVNOFQ64izfFjL-YFCHMwSEo-pESf5rICrek", sheet = "Sheet1")%>%
  clean_names()%>%
  select(cohort, mouse_num)%>%
  filter(!is.na(cohort))%>%
  mutate(mouse_num=unlist(mouse_num))%>%
  
  mutate(cohort=sub("cohort", "co.", cohort))


names<-dir_info(path="F:/nf1 glioma scanned", recurse=F)%>%
  select(path)%>%
  rename(path1=path)%>%
  
  
  mutate(mouse_num=substr(path1, 23,27))%>%
  
  left_join(df1)%>%
  
  
  mutate(cohort=if_else(is.na(cohort), "xc", cohort))%>%
  
  
  mutate(rep=paste0(cohort, ".", mouse_num))%>%
  
  
  mutate(path2 = str_replace(path1, mouse_num, rep))%>%
  
  
  filter(!(grepl("/xc", path1)|grepl("/co", path1)))%>%
  
  
  semi_join(dir_info(path="F:/nf1 glioma scanned", recurse=F)%>%
  select(path)%>%
  rename(path1=path))%>%
    
  group_by(path1)%>%
  slice(1)







file_move(names$path1, names$path2)
         