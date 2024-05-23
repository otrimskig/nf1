#R input using Data to R sheet
#necessary libraries
library(tidyverse)
library(googlesheets4)


##################################
#sheet id and sheet name to read in from sheets. 

sheet_id<-"1O-YCDJh4WhjggWK5nRXTyJtB7gfmM7xnxBHDv1IEBEY"
name_of_sheet<-"Sheet2"



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





library(fs)



mns<-sheets_df%>%
  pull(mouse_num)

search<-paste(mns, collapse = "|")




all<-as_tibble(list(path1=dir_ls("F:/nf1 glioma scanned")))%>%
  mutate(basename=basename(path1))%>%
  
  filter(str_detect(basename, search))%>%
  
  mutate(path2 = paste0("F:/nf1 rna seq scanned/", basename))


mrxs<-all%>%
  filter(grepl(".mrxs$", basename))




new_files<-as_tibble(list(path2=dir_ls(path="F:/nf1 rna seq scanned/",type="dir")))%>%
  mutate(copied=1)


dirs<-all%>%
  filter(!grepl(".mrxs$", basename))%>%
  left_join(new_files)%>%
  
  
  
  filter(!is.na(copied))
  





#file_copy(mrxs$path1, mrxs$path2)
dir_copy(dirs$path1, dirs$path2, overwrite=TRUE)
