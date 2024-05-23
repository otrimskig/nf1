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
  rename(scan_slot=x3)%>%
  filter(!is.na(scan_slot))%>%
  arrange(scan_slot)%>%
  mutate(index = 1:n())%>%
  
  mutate(new_name = paste(mouse_num, organ, stain, scan_date, sep="."))




batch1<-df2%>%
  filter(has_scan=="y")%>%
  select(index, new_name)




fp<-"F:/nf1 glioma scanned/"


files<-dir_info(fp, recurse=F, type="file")%>%
  as_tibble()%>%
  rename(path_file1 = path)%>%
  select(path_file1, birth_time)%>%
  
  mutate(path_dir1= sub(".mrxs", "", path_file1))%>%
  
  
  arrange(desc(birth_time))%>%
  slice(1:38)%>%
  
  arrange(birth_time)%>%
  
  mutate(index = 1:n())%>%
  
  
  
  
  left_join(batch1, by="index")%>%
  
  
  mutate(path_dir2 = paste0("F:/nf1 glioma scanned/", new_name, "-g"))%>%
  mutate(path_dir3 = paste0("F:/nf1 glioma scanned/", new_name))%>%
  
  
  mutate(path_file2 = paste0("F:/nf1 glioma scanned/", new_name, "-g", ".mrxs"))%>%
  mutate(path_file3 = paste0("F:/nf1 glioma scanned/", new_name, ".mrxs"))
  



file_move(files$path_dir1, files$path_dir2)
file_move(files$path_dir2, files$path_dir3)



file_move(files$path_file1, files$path_file2)
file_move(files$path_file2, files$path_file3)








# fis<-as_tibble(dir_ls(fp, type="file", recurse = F))%>%
#   rename(file1=value)%>%
#   mutate(index = 1:n())





# file_move(df3$folder1, df3$folder2)
# 
# file_move(df3$file3, df3$file2)



"F:\slide scans go\slide-2023-12-06T13-10-26-R1-S1.mrxs"
