library(tidyverse)


df<-readRDS("ds/2024-01-18 12-21 nf1 cataloged samples.rds")%>%
  filter(sample_type!="Slide")%>%
  select(1:7)

coh<-readRDS("ds/cohorts-2024-01-18.rds")

df2<-df%>%
  left_join(coh, by="mouse_num")%>%
  filter(is.na(exclude))%>%
  semi_join(coh, by="mouse_num")%>%
  select(loc_1:he_scan)%>%
  select(-loc_3)%>%
  mutate(go_notes="")

write_csv(df2, "out/samples_no_ffpe.csv", na="")

library(googlesheets4)
gs4_auth(email = "gotrimski@gmail.com")

df2%>%
  range_write("1Vr8u_WcXR-VIqRJc1MUXVQobB4HBGgnhg8LTLnsYwOQ",
              sheet = "no_ffpe",
              .,
              range = "A1", reformat = FALSE)
