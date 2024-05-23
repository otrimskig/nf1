#R input using Data to R sheet
library(googlesheets4)
library(tidyverse)
library(janitor)
library(lubridate)
library(fs)


#authorize user
gs4_auth(email = "gotrimski@gmail.com")

#read input sheet
df<-read_sheet("1_Kk3q0OkVNOFQ64izfFjL-YFCHMwSEo-pESf5rICrek", 
               sheet = "Sheet1")%>%
  clean_names()


df2<-df%>%
  filter(!is.na(x3))%>%
  filter(x3>38.5)%>%
  
  select(mouse_num, organ, stain, x3)%>%
  
  mutate(index=1:n())







files<-dir_info(path="F:/slide scans go", type="file")%>%
  select(path, birth_time)%>%
  
  mutate(index=1:n())%>%
  
  left_join(df2)%>%
  
  
  rename(path1=path)%>%
  
  
  
  mutate(base2= paste0(mouse_num,".", organ, ".", stain,".", as.character(date(birth_time))))%>%
  
  
  mutate(path2 = paste0("F:/nf1 glioma scanned/", base2, ".mrxs"))




dirs<-files%>%
  mutate(dir1=sub(".mrxs", "", path1))%>%
  mutate(dir2=sub(".mrxs", "", path2))

file_move(files$path1, files$path2)

file_move(dirs$dir1, dirs$dir2)




