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

rename1<-df%>%
  filter(x4>1)%>%
  
  select(mouse_num, organ, stain, x3, x4)%>%
  
  arrange(x3)%>%
  
  mutate(wrong_order = 1:n())%>%
  
  
  arrange(x4)%>%
  mutate(correct_order=1:n())%>%
  
  arrange(wrong_order)%>%
  rename(num1=mouse_num)


rename1%>%arrange(correct_order)%>%
  select(num1, correct_order)%>%
  rename(num2=num1)->names2

rename2<-rename1%>%
  left_join(names2, c("wrong_order"="correct_order"))%>%
  mutate(num1=unlist(num1))%>%
  mutate(num2=unlist(num2))



files_all<-dir_ls(path="F:/nf1 glioma scanned", recurse=F)

rename2$num2


filtered_files <- files_all[grep(paste(rename2$num1, collapse = "|"), files_all)]  




filtered_files%>%
  as_tibble()%>%
  rename(path1=value)%>%
  
  mutate(num1=substr(path1, 23,27))%>%
  
  
  left_join(rename2%>%select(num1,num2))%>%
  
  
  mutate(path2= str_replace(path1, num1, paste0(num2, "-rn")))%>%
  
  mutate(path3= str_replace(path1, num1, num2))->rnames



file_move(rnames$path1, rnames$path2)

file_move(rnames$path2, rnames$path3)
