library(tidyverse)


no_obs<-read_csv("ds/no_observation_cataloged_slides.csv")%>%
  mutate(mouse_num=as.character(mouse_num))

scans<-readRDS("ds/scanned_slides_info.rds")%>%
  mutate(staining = toupper(staining))%>%
  mutate(staining = gsub("&", "", staining))%>%
  filter(staining=="HE")%>%
  filter(sample=="B")

no_obs%>%
  left_join(scans,by = "mouse_num")%>%
  view()
