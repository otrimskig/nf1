library(tidyverse)

cohort<-readRDS("ds/cohort_mouse_nums.rds")

scanned_slides<-readRDS("ds/scanned_slides_info.rds")

sample_locs<-readRDS("ds/sample_locations.rds")%>%
  semi_join(cohort, by = "mouse_num")%>%
  semi_join(scanned_slides, by = "mouse_num")%>%
  filter(sample_type == "Slide")%>%
  mutate(serial = if_else(grepl("serial", box_label, ignore.case = TRUE),1,0))%>%
  rename(sample = organ, staining = stain)



scanned_slides2<-scanned_slides%>%
  semi_join(cohort, by = "mouse_num")%>%
  mutate(staining = toupper(staining))%>%
  mutate(staining = gsub("&", "", staining))%>%
  mutate(staining = gsub("SYNAPTOPHYSIN", "SYNAP", staining))%>%
  left_join(sample_locs, by=c("mouse_num", "sample", "staining"))%>%
  filter(unique(base1))








he_slides<-read_csv("ds/brain_he_slides_without_tumor_characterization.csv")
