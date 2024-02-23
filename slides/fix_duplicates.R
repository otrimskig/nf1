library(fs)
library(tidyverse)



scans_loc<-"D:/nf1 glioma scanned"




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
  
 




#now read in actual file and dir names. 

#get list of all files and directories (non-recursive)
#for all new scans yet to be properly names.

scans1<-dir_info(scans_loc)%>%
  as_tibble()%>%
  
  #select only necessary. variables
  select(path, type, modification_time, size)%>%
  
  mutate(size = if_else(type=="directory", fs::pretty_bytes(path), size))%>%
  
  #rename for convenience.
  rename(modified=modification_time)%>%
  
  mutate(name = basename(path))%>%
  
  filter(grepl("^co", name))%>%
  
  mutate(name2=name)%>%
  
  separate(name2, into = c("coh", "hort", "mouse_num", "organ", "stain", "date"), sep = "\\.")

scans2<-scans1%>%
  
  
  mutate(cohort = paste0(coh, ".", hort))%>%select(-coh, -hort)%>%
  
  mutate(stain = toupper(stain))%>%

  mutate(stain=if_else(stain=="SYNAPTOPHYSIN", "SYNAP", stain))%>%
  mutate(stain=ifelse(stain=="H&E", "HE", stain))%>%
  
  
  mutate(name_nodate= paste(cohort, mouse_num, organ, stain, sep = "."))



counts<-scans2%>%
  count(name_nodate)



scans3<-scans2%>%
  left_join(counts)




#renaming operation, contingent upon a few checks. 


# if (any(is.na(dirs))) {
#     
#     stop("NA values detected in 'dirs' df")
#   
#     }else if (any(is.na(files))){
#       
#     stop("NA values detected in 'files' df")
#     
#       
#     }else if (nrow(dirs)!=nrow(files)){
#     
#     stop("something wonky with your files dfs.")  
# 
#       
#     }else if (nrow(new_scans)/2!=nrow(files)){
#       
#     stop("something wonky with your renaming set.")    
#     
#     }else{
#       
#     print("looks good. renaming now...")
#     
#       
#     #move and rename directories. 
#       
#     file_move(dirs$path1, dirs$path2)
#       
#       
#     #move and rename files. 
#       
#     file_move(files$path1, files$path2)
#     
#     print("done.")
#   
#     }
#   

  
   

