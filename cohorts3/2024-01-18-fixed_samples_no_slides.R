library(tidyverse)


df<-readRDS("ds/2024-01-18 12-21 nf1 cataloged samples.rds")

coh<-readRDS("ds/cohorts-2024-01-17.rds")%>%
  mutate(has_ffpe=has_nec)%>%
  relocate(has_ffpe, .after=has_nec)

df2<-df%>%
  filter(sample_type!="Slide")%>%
  semi_join(coh, by="mouse_num")




in_etoh<-df2%>%
  group_by(mouse_num)%>%
  slice(1)%>%
  select(mouse_num)%>%
  mutate(in_etoh=1)


coh2<-coh%>%
  left_join(in_etoh)%>%
  mutate(has_nec=if_else(is.na(in_etoh),has_nec, 1))%>%
  select(-index, -in_etoh)%>%
  relocate(exclude, mouse_num, resultant_geno, aod, event, found_dead, has_nec, has_ffpe, he_scan)%>%
  rename(dispo=g_comments)


saveRDS(coh2, "ds/cohorts-2024-01-18.rds")

coh2%>%
  #filter(is.na(exclude))%>%
  filter(is.na(has_ffpe))%>%
  filter(!is.na(has_nec))%>%
  view()
