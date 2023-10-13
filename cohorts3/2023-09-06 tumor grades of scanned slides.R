library(tidyverse)
library(janitor)


nec_perf<-readRDS("ds/colony_necropsy_info.rds")%>%
  select(mouse_num, nec_performed)

df1.main<-readRDS("ds/nf1_cohorts.2.dates.rds")%>%
  select(mouse_num, src, exclude, metadata, resultant_geno)

df1.obs<-df1.main%>%
  left_join(readRDS("ds/nf1_cohorts.1.observations.rds"), by = "mouse_num")


#looking for mice within incomplete tumor characterization. 
#that are able to be characterized 
#and/or should be characterized. 
mice_char<-df1.obs%>%
  
  
  #filtering join for mice that had necropsies performed. 
  semi_join(nec_perf, by = "mouse_num")%>%
  
  
  #selecting only necessary var. 
  select(mouse_num,
         notes_he_ihc_brain,
         sheri_has_seen_h_e,
         tumor_grade_post_h_e)%>%
  rename(notes_bhe = notes_he_ihc_brain)





df1.obs%>%
  
  
  #filtering join for mice that had necropsies performed. 
  semi_join(nec_perf, by = "mouse_num")%>%
  

  #selecting only necessary var. 
  select(mouse_num,
         notes_he_ihc_brain,
         sheri_has_seen_h_e,
         tumor_grade_post_h_e)%>%
  rename(notes_bhe = notes_he_ihc_brain)













#need to cross-reference with all slide/sample locations. 

#first step,
#import and cleaning slides dataset. 

#cleaned version of slides dataset (physical) 
readRDS("ds/sample_locations.rds")



scanned_slides<-readRDS("ds/scanned_slides_info.rds")

bhe_scans<-scanned_slides%>%
  filter(staining == "H&E")%>%
  filter(sample=="B")%>%
  select(mouse_num, base2)%>%
  left_join(mice_char)%>%
  left_join(df1.main, by = "mouse_num")%>%
  relocate(mouse_num, base2, resultant_geno)

write_csv(bhe_scans, "out/scanned_slides_characterized.csv", na="")









df1.obs2<-df1.obs%>%
  
  
  #filtering join for mice that had necropsies performed. 
  left_join(nec_perf, by = "mouse_num")%>%
  
  
  #selecting only necessary var. 
  select(mouse_num,
         notes_he_ihc_brain,
         sheri_has_seen_h_e,
         tumor_grade_post_h_e,
         nec_performed)%>%
  rename(notes_bhe = notes_he_ihc_brain)

bhe_all_scans<-scanned_slides%>%
  filter(staining == "H&E")%>%
  filter(sample=="B")%>%
  select(mouse_num, base2)





phys_slides<-readRDS("ds/sample_locations.rds")%>%
  filter(organ=="B")%>%
  filter(stain=="HE")%>%
  semi_join(df1.main, by="mouse_num")%>%
  mutate(slot = sprintf("%03d", slot))%>%
  mutate(sample_loc = paste0(loc,"-", slot, "-", toupper(box_label)))%>%
  select(mouse_num, sample_loc)%>%
  rename(sample1 = sample_loc)%>%
  
  group_by(mouse_num)%>%
  arrange(sample1)%>%
  mutate(sample2 = sample1[2])%>%
  slice(-2)%>%
  mutate(sample3 = sample1[2])%>%
  slice(-2)
  
  

df1.obs3<-df1.obs2%>%
  left_join(bhe_all_scans, by = "mouse_num")%>%
  rename(scan1 = base2)%>%
  relocate(mouse_num, scan1)%>%
  group_by(mouse_num)%>%
  arrange(scan1)%>%
  mutate(scan2 = scan1[2])%>%
  relocate(scan2, .after=scan1)%>%
  slice(-2)%>%
  mutate(scan3 = scan1[2])%>%
  slice(-2)%>%
  distinct()%>%
  relocate(scan3, .after=scan2)%>%
  left_join(df1.main, by = "mouse_num")%>%
 
  left_join(phys_slides, by = "mouse_num")%>%
  relocate(sample1, sample2, sample3, .after=scan3)%>%
  
  
  mutate(notes_bhe = coalesce(notes_bhe, tumor_grade_post_h_e))%>%
  select(-tumor_grade_post_h_e)%>%
  relocate(mouse_num, resultant_geno, notes_bhe)%>%
  
  mutate(needs_sample = if_else(is.na(scan1)&is.na(sample1), 1,0))



saveRDS(df1.obs3,"ds/all_mice_brain_he_sample_locs.rds")


write_csv(df1.obs3,"out/brain_he_samples_plus_tumor_and_geno.csv",na="")






samples_needed<-df1.obs3%>%
  filter(needs_sample==1)%>%
  left_join(readRDS("ds/sample_locations.rds"), by="mouse_num")
  
