library(tidyverse)
setwd("C:/Users/u1413890/OneDrive - University of Utah/garrett hl-onedrive/R/nf1/cohorts3")


scanned<-read_csv("ds/scanned_slides_info.csv")

df<-readRDS("ds/nf1_cohorts.2.dates.rds")

df2<-df%>%
  semi_join(scanned, by = "mouse_num")%>%
  left_join(scanned, by = "mouse_num")


df_obs<-readRDS("ds/nf1_cohorts.1.observations.rds")%>%
  select(mouse_num, notes_he_ihc_brain)%>%
  filter(grepl("grade", notes_he_ihc_brain))

df3<-df_obs%>%
  left_join(df2, by = "mouse_num")

write_csv(df3, "ds/graded_tumor_scanned_slides.csv")
