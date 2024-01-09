library(tidyverse)
library(fs)
library(tidyr)

blue_folder<-"F:/nf1 glioma scanned/"

czi_path<-"X:/Holmen Lab/Lab Personnel/Garrett/CZI scope files/"

dest_path<-"X:/Holmen Lab/Lab Personnel/Garrett/CZI scope files/unscanned"


dir_create(dir_ls(czi_path, type="dir", recurse=F)%>%
  as_tibble()%>%
  filter(grepl("cohort", value))%>%
  mutate(value=sub("X:/Holmen Lab/Lab Personnel/Garrett/CZI scope files/", 
                   "X:/Holmen Lab/Lab Personnel/Garrett/CZI scope files/unscanned/", value))%>%pull())



scanned<-dir_ls(blue_folder, type="dir", recurse=F)%>%
  as_tibble()%>%
  
  mutate(scans=basename(value))%>%
  select(-value)%>%
  separate(
    col = scans,
    into = c("mouse_num", "organ", "stain", "scan_date"),
    sep = "\\.",
    extra = "merge",
    fill = "right"
  )%>%
  filter(stain=="H&E")

scanned%>%
  saveRDS("scanned_slides.rds")



snaps<-dir_ls(czi_path, type="file", recurse=T)%>%
  as_tibble()%>%
  mutate(fn=basename(value))%>%
  rename(path1=value)%>%

  filter(grepl("cohort", path1, ignore.case = T))%>%
  
  mutate(mouse_num=substr(fn, 1,5))




library(googlesheets4)
gs4_auth(email = "gotrimski@gmail.com")
sheet_loc<-"1_Kk3q0OkVNOFQ64izfFjL-YFCHMwSEo-pESf5rICrek"
sheet_name<-"snap_scan_match"

sheets_df<-read_sheet(sheet_loc, 
                      sheet = sheet_name)



sheets2<-sheets_df%>%
  group_by(mouse_num)%>%
  fill(comment, .direction = "updown")%>%
  fill(`of interest`, .direction = "updown")%>%
  fill(`...12`, .direction = "updown")%>%
  fill(`scan box slot`, .direction = "updown")%>%
  fill(`scan date`, .direction = "updown")%>%
  fill(`has_scan`, .direction = "updown")%>%
  
  
  slice(1)%>%
  ungroup()



sheets2%>%
range_write(sheet_loc,
                          sheet = "Sheet1",
                          .,
                          range = "A1")











# library(googlesheets4)
# gs4_auth(email = "gotrimski@gmail.com")
# 
# df%>%
#   range_write("1DYwkZEVK39qMZl1vYrg3R7hi41nye1yikUNYN4tzWbs",
#               sheet = "output",
#               .,
#               range = "A1")


























scanned_slides<-sheets_df%>%
  filter(has_scan=="y"|!is.na(`scan box slot`))



snaps%>%
  saveRDS("snaps-01.rds")







df_files<-anti_join(snaps, scanned_slides, by="mouse_num")%>%
  mutate(path2=sub("X:/Holmen Lab/Lab Personnel/Garrett/CZI scope files/", "X:/Holmen Lab/Lab Personnel/Garrett/CZI scope files/unscanned/", path1))




file_copy(df_files$path1, df_files$path2)
