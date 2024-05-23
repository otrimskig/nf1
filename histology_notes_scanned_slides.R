#R input using Data to R sheet
library(googlesheets4)
library(tidyverse)
library(janitor)
library(lubridate)
library(fs)


#authorize user
gs4_auth(email = "gotrimski@gmail.com")

#read input sheet
df1<-read_sheet("1Vr8u_WcXR-VIqRJc1MUXVQobB4HBGgnhg8LTLnsYwOQ", sheet = "updated_slides")%>%
  clean_names()%>%
  mutate(index=1:n())

#df of all previously scanned slides, with physical slide locations. 
scans_old<-read_sheet("1_Kk3q0OkVNOFQ64izfFjL-YFCHMwSEo-pESf5rICrek", sheet = "snap_scan_match")%>%
  clean_names()%>%
  filter(has_scan=="y")%>%
  select(mouse_num, organ, stain, box, slot)





scans<-read_sheet("1_Kk3q0OkVNOFQ64izfFjL-YFCHMwSEo-pESf5rICrek", sheet = "Sheet1")%>%
  clean_names()%>%
  filter(!is.na(x3))%>%
  select(mouse_num, organ, stain, box, slot)%>%
  mutate(mouse_num = unlist(mouse_num))%>%
  mutate(slot=unlist(slot))%>%
  
  full_join(scans_old)%>%
  
  mutate(slot=as.numeric(trimws(slot)))%>%
  mutate(slot=sprintf("%03d",slot))%>%
  
  group_by(box, slot)%>%
  slice(1)%>%
  ungroup()



# #get names of all files
# scans<-dir_info(path="F:/nf1 glioma scanned", type="dir", recurse=F)%>%
#   select(path)%>%
#   filter(!grepl("xc", path))%>%
#   
#   mutate(path2=path)%>%
#   
#   separate(path2, into = c("file","cohort", "mouse_num", "organ", "stain", "scan_date"), sep="\\.")%>%
#   select(-file)%>%
#   
#   mutate(stain=toupper(stain))%>%
#   mutate(stain=sub("H&E", "HE", stain))















df2<-df1%>%
  select(index, box, slot, mouse_num, organ, stain)%>%
  left_join(scans, by=c("box", "slot"))%>%
  
  mutate(has_scan=if_else(!is.na(mouse_num.y), "Y", NA))%>%
  select(index,has_scan)%>%
  
  arrange(index)%>%
  
  select(has_scan)





library(googlesheets4)
gs4_auth(email = "gotrimski@gmail.com")

df2%>%
  range_write("1Vr8u_WcXR-VIqRJc1MUXVQobB4HBGgnhg8LTLnsYwOQ",
              sheet = "updated_slides",
              .,
              range = "R1", reformat = F)
