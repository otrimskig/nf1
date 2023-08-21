library(tidyverse)
library(janitor)


nec_perf<-readRDS("ds/colony_necropsy_info.rds")%>%
  select(mouse_num, nec_performed)




slide_locs<-read_csv("raw/nf1 slide boxes - lab_samples.csv")%>%
  row_to_names(1)%>%
  clean_names()%>%
  rename(mouse_num = mouse_id)%>%
  relocate(loc, slot)%>%
  filter(!is.na(loc))%>%
  mutate(organ = toupper(organ))%>%
  mutate(slot = as.numeric(slot))


df1.obs<-readRDS("ds/nf1_cohorts.1.observations.rds")
df1.main<-readRDS("ds/nf1_cohorts.2.dates.rds")


df1.slides<-df1.main%>%
  select(mouse_num, src, exclude, metadata)%>%
  left_join(nec_perf)%>%
  left_join(df1.obs, by ="mouse_num")%>%
  
  select(mouse_num,
         src,
         exclude,
         metadata,
         notes_he_ihc_brain,
         sheri_has_seen_h_e,
         nec_performed)%>%
  
  filter(sheri_has_seen_h_e!="yes"|is.na(sheri_has_seen_h_e))%>%
  filter(nec_performed == 1)



slide_locs2<-slide_locs%>%
  semi_join(df1.slides, by = "mouse_num")%>%
  filter(organ=="B", stain=="HE")%>%
  group_by(mouse_num)%>%
  slice(1)%>%
  ungroup()


df2.slides<-df1.slides%>%
  left_join(slide_locs2, by = "mouse_num")%>%
  filter(!is.na(loc))%>%
  filter(is.na(exclude))%>%
  
  relocate(loc, slot, mouse_num)%>%
  arrange(loc,slot,mouse_num)%>%
  
  
  write_csv("ds/no_observation_cataloged_slides.csv")
  
  