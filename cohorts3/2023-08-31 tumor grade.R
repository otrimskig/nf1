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
mice_without_char<-df1.obs%>%
  
  #removing excluded mice. 
  filter(exclude!="1"|is.na(exclude))%>%
  
  #filtering join for mice that had necropsies performed. 
  semi_join(nec_perf, by = "mouse_num")%>%
  
  #looking for mice without verified tumor classifications. 
  filter(is.na(sheri_has_seen_h_e)|sheri_has_seen_h_e!="yes")%>%
  
  #selecting only necessary var. 
  select(mouse_num,
         notes_he_ihc_brain,
         sheri_has_seen_h_e,
         tumor_grade_post_h_e)
  



#need to cross-reference with all slide/sample locations. 

#first step,
#import and cleaning slides dataset. 
slide_locs<-read_csv("raw/nf1 slide boxes - lab_samples.csv")%>%
  row_to_names(1)%>%
  clean_names()%>%
  rename(mouse_num = mouse_id)%>%
  relocate(loc, slot)%>%
  filter(!is.na(loc))%>%
  mutate(organ = toupper(organ))%>%
  mutate(stain = toupper(stain))%>%
  mutate(slot = as.numeric(slot))

#save cleaned version for later use. 
saveRDS(slide_locs, "ds/sample_locations.rds")




#now can use to cross-reference.
#separating into 2 groups: 
#    1.mice that have known locs of brain he slides (physical), in lab.
#
#    2.mice that don't, but still need to be characterized (if possible)


#looking for uncharacterized mice that have brain HE slides. 
mice_bhes<-mice_without_char%>%
  #joining with slide locations. 
  left_join(slide_locs)%>%
  filter(organ=="B"&stain=="HE")%>%
  
  #getting only single slide for each, no need to have more than one. 
  group_by(mouse_num)%>%
  slice(1)%>%
  left_join(df1.main)



moved_slides<-read_csv("ds/no_observation_cataloged_slides.csv")%>%
  mutate(index=row_number())%>%
  select(index, mouse_num)%>%
  rename(temp_box = index)%>%
  mutate(mouse_num = as.character(mouse_num))



mice_bhes%>%
  left_join(moved_slides)%>%

write_csv("ds/brain_he_slides_without_tumor_characterization.csv",
          na="")





#mice that don't have a known location of at least one brain HE slide. 
no_bhe_slides<-mice_without_char%>%
  #filtering join against mice that DO have brain HE slides. 
  anti_join(mice_bhes, by = "mouse_num")%>%
  
  #bringing in a few other vars just to get some more context.
  #metadata, src, exclude, resultant_geno all pulled in from df1.main
  left_join(df1.main)%>%
  
  #adding information for any locations of samples that we do have.
  left_join(slide_locs)
  #no need to slice for only 1 sample for these. 




cohort_counts_missing_brain_he<-no_bhe_slides%>%
  group_by(mouse_num)%>%
  slice(1)%>%
  ungroup()%>%
  count(resultant_geno)
