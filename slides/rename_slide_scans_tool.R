library(fs)
library(tidyverse)



scans_loc<-"F:/slide scans go"




#getting info for the naming of the scans. 

#sheet id and sheet name to read in from sheets. 
#note that this is a sheet dedicated to renaming slides. 

sheet_id<-"1RV5PUwwV3wT423YkMOOYsKOj5inOAA7eSROezH_W5Z8"
name_of_sheet<-"rn"


#read input sheet
library(googlesheets4)
gs4_auth(email = "gotrimski@gmail.com")

df<-read_sheet(sheet_id, 
                      sheet = name_of_sheet)%>%
  mutate(across(1:last_col(), as.character))%>%
  
  #then replace all "NULL" with NA. 
  mutate(across(1:last_col(), function(x){na_if(x, "NULL")}))%>%
  
  janitor::clean_names()%>%
  
  
  select(order, mouse_num, organ, stain, has_scan, scan_box, sb_slot, conf_scan, renamed, coh)%>%
  
  mutate(order=as.numeric(order))%>%
  mutate(sb_slot=as.numeric(sb_slot))


#from all slide info for renaming, get the basename for the new file/dir, 
#and get the order that corresponds to the scan order. 
renames<-df%>%
  
  #option to use same google sheet but filter out if file has already been renamed. 
  filter(is.na(renamed))%>%
  
  #option to filter out if was actually scanned. 
  filter(conf_scan=="1")%>%
  
  #arranges by the order the slides were scanned
  arrange(order)%>%
  
  #creates a new ordering index, in case anything was excluded with the options above. 
  mutate(index=1:n())%>%
  
  #makes a new basename for the resultant image file and directory - 
  #does not yet include the path or file extension. 
  mutate(base=paste0("co.", coh, ".", mouse_num, ".", organ, ".", stain, ".", Sys.Date()))%>%
  
  #fixes potential duplicate names. 
  group_by(base) %>%
  mutate(suffix = ifelse(duplicated(base), paste0("-", row_number()- 1), ""))%>%
  ungroup()%>%
  
  mutate(basename=trimws(paste0(base, suffix)))%>%


  #select only necessary vars. 
  select(index,basename)





#now read in actual file and dir names. 

#set input and output folders. 

### scans_loc object set above. 

dest_path<-paste0(scans_loc, "/rn")

#get list of all files and directories (non-recursive)
#for all new scans yet to be properly names.

new_scans<-dir_info(scans_loc)%>%
  as_tibble()%>%
  
  #select only necessary. variables
  select(path, type, modification_time)%>%
  
  #rename for convenience.
  rename(modified=modification_time,  path1=path)%>%
  
  #removed /rn destination folder. 
  filter(path1!=dest_path)






#create renaming df for directories. 


dirs<-new_scans%>%
  filter(type=="directory")%>%
  
  #arranges by pathname, which is named based on time of scan. 
  arrange(modified)%>%
  
  #create ordering index
  mutate(index=1:n())%>%
  
  #remove extraneous vars. 
  select(path1, index, modified)%>%
  
  #join with indexed new names. 
  left_join(renames)%>%
  
  #create new path, combining output directory location with basename. 
  mutate(path2 = paste0(dest_path,"/", basename))






files<-new_scans%>%
  filter(type=="file")%>%
  arrange(modified)%>%
  mutate(index=1:n())%>%
  select(path1, index, modified)%>%
  left_join(renames)%>%
  mutate(path2 = paste0(dest_path,"/", basename, ".mrxs"))
  



#renaming operation, contingent upon a few checks. 


if (any(is.na(dirs))) {
    
    stop("NA values detected in 'dirs' df")
  
    }else if (any(is.na(files))){
      
    stop("NA values detected in 'files' df")
    
      
    }else if (nrow(dirs)!=nrow(files)){
    
    stop("something wonky with your files dfs.")  

      
    }else if (nrow(new_scans)/2!=nrow(files)){
      
    stop("something wonky with your renaming set.")    
    
    }else{
      
    print("looks good. renaming now...")
    
      
    #move and rename directories. 
      
    file_move(dirs$path1, dirs$path2)
      
      
    #move and rename files. 
      
    file_move(files$path1, files$path2)
    
    print("done.")
  
    }
  

  
   

