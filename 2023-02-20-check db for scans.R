library(fs)
library(tidyverse)





#getting info for the naming of the scans. 

#sheet id and sheet name to read in from sheets. 
#note that this is a sheet dedicated to renaming slides. 

sheet_id<-"1Vr8u_WcXR-VIqRJc1MUXVQobB4HBGgnhg8LTLnsYwOQ"
name_of_sheet<-"updated_slides"


#read input sheet
library(googlesheets4)
gs4_auth(email = "gotrimski@gmail.com")

df<-read_sheet(sheet_id, 
               sheet = name_of_sheet)%>%
  mutate(across(1:last_col(), as.character))%>%
  
  #then replace all "NULL" with NA. 
  mutate(across(1:last_col(), function(x){na_if(x, "NULL")}))%>%
  
  janitor::clean_names()%>%
  
  
  select(mouse_num, organ, stain, has_scan, coh)






#stop("check 'scans loc' line 41")
##############################

#now read in actual file and dir names. 

scans_loc<-"D:/nf1 glioma scanned"


#get list of all files and directories (non-recursive)
#for all new scans yet to be properly names.

scans1<-dir_info(scans_loc)%>%
  as_tibble()%>%
  filter(type=="directory")%>%
  mutate(scan_name=basename(path))%>%
  select(scan_name, modification_time)%>%
  rename(modified=modification_time)%>%
  
  filter(grepl("^co", scan_name))%>%
  
  mutate(name2=scan_name)%>%
  
  separate(name2, into = c("coh", "hort", "mouse_num", "organ", "stain", "date"), sep = "\\.")%>%
  
  
  mutate(cohort = paste0(coh, ".", hort))%>%select(-coh, -hort)%>%
  
  mutate(stain = toupper(stain))%>%
  
  mutate(stain=if_else(stain=="SYNAPTOPHYSIN", "SYNAP", stain))%>%
  mutate(stain=ifelse(stain=="H&E", "HE", stain))%>%
  
  
  mutate(name_nodate= paste(cohort, mouse_num, organ, stain, sep = "."))





####join to check mismatched. 

df2<-df%>%
  left_join(scans1)

df2%>%
  filter(!is.na(scan_name)&has_scan!="Y")%>%
  print(n=50)

#### updated google sheet to ensure good. 



#now working on duplicate files. 

dupes<-scans1%>%
  group_by(name_nodate)%>%
  filter(n()>1)
  
  