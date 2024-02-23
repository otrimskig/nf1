library(fs)
library(tidyverse)

#set input and output folders. 
input_loc<-"F:/nf1 glioma scanned/temp/"

output_loc<-"F:/nf1 glioma scanned/temp/"



#get list of all files and directories (non-recursive)
#for all new scans yet to be properly names.

new_scans<-dir_info(input_loc)%>%
  as_tibble()%>%
  #select only nec. variables
  select(path, type, modification_time)%>%
  #rename for convenience.
  rename(modified=modification_time, 
         path1=path)%>%
  
  
  
  
  filter(modified>"2024-01-29")







#getting info for the naming of the scans. 

#sheet id and sheet name to read in from sheets. 
#note that this is a sheet dedicated to renaming slides. 

sheet_id<-"1RV5PUwwV3wT423YkMOOYsKOj5inOAA7eSROezH_W5Z8"
name_of_sheet<-"rn"


#read input sheet
library(googlesheets4)
gs4_auth(email = "gotrimski@gmail.com")

renaming<-read_sheet(sheet_id, 
                     sheet = name_of_sheet)%>%
  mutate(across(1:last_col(), as.character))%>%
  
  #then replace all "NULL" with NA. 
  mutate(across(1:last_col(), function(x){na_if(x, "NULL")}))%>%
  
  janitor::clean_names()


#from all slide info for renaming, get the basename for the new file/dir, 
#and get the order that corresponds to the scan order. 
renames<-renaming%>%
  
  #option to use same google sheet but filter out if file has already been renamed. 
  #filter(is.na(renamed))%>%
  
  #option to filter out if was actually scanned. 
  filter(confirm_can_should_scan=="1")%>%
  
  #arranges by the order the slides were scanned
  mutate(order=as.numeric(order))%>%
  arrange(order)%>%
  
  #creates a new ordering index, in case anything was excluded with the options above. 
  mutate(index=1:n())%>%
  
  #makes a new basename for the resultant image file and directory - 
  #does not yet include the path or file extension. 
  mutate(basename=paste0("co.", coh, ".", mouse_num, ".", organ, ".", stain, ".", Sys.Date()))%>%
  
  #select only necessary vars. 
  
  filter(!is.na(basename))




#create renaming df for directories. 
dirs<-new_scans%>%
  filter(type=="directory")%>%
  #arranges by pathname, which is named based on time of scan. 
  arrange(modified)%>%
  #create ordering index
  
  
  
  mutate(index=1:n())%>%
  
#remove extraneous vars. 
  #select(path1, index)%>%
  #join with indexed new names. 
  left_join(renames)%>%
  
  #create new path, combining output directory location with basename. 
  mutate(path2 = paste0(output_loc, basename))






files<-new_scans%>%
  filter(type=="file")%>%
  arrange(modified)%>%
  mutate(index=1:n())%>%
  select(path1, index)%>%
  left_join(renames)%>%
  
  #same process as above, but add file extension to new name. 
  mutate(path2 = paste0(output_loc, basename, ".mrxs"))




#move and rename directories. 

file_move(dirs$path1, dirs$path2)


#move and rename files. 

file_move(files$path1, files$path2)


nf<-dir_info(type="directory", path=output_loc)%>%
  mutate(mrxs=paste0(path, ".mrxs"))%>%
  select(mrxs)%>%
  pull()


file_create(nf)

for (i in 1:length(nf)) {
  
  file_create(nf[i])

}


nf[1]






