#standard libraries. 
#pulling dataset for all physical slides in lab. 
#imported data-entry to RDS file in previous R script. 

library(tidyverse)
library(janitor)


#cohort version used: cohorts4.0.1


slides0.1<-readRDS("slides/nf1_slides.rds")

cohorts<-readRDS("cohorts2/ds/cohorts4.0.1.rds")

#getting scanned slides dataset.
scanned_slides<-read_rds("slides/nf1_scanned_slides.rds")




#####################################################################################

#checking slides already scanned. 
#getting list of all scanned slides, and cleaning a bit. 

read_csv("slides/slides-prev/slide_info2.csv")%>%
  select(mouse_num, tumor, staining, path)%>%
  rename(slidescan_path = path)%>%
  mutate(across(1:last_col(), function(x){
    as.character(x)%>%
      tolower()
  }))%>%
  rename(organ = tumor, stain = staining)%>%
  mutate(stain = if_else(grepl("h&e", stain), "he", 
                         if_else(grepl("synaptophysin", stain), "synap", 
                                 stain)))->scanned_slides


#saving for later, if want to use in another script. 
write_rds(scanned_slides, "slides/nf1_scanned_slides.rds")
############################################################################






cohorts%>%
  
  filter(is.na(exclude))%>%
  
  select(mouse_num, brain_ihc_obs, sheri_seen)->ihc_notes





#df of all slides with physical locations. 
slides0.1%>%
  
  #filtering out non-cohort mice.
  semi_join(ihc_notes, by = "mouse_num")%>%

  #joining dataset to include ihc_obs. 
  left_join(ihc_notes)%>%
  arrange(loc, slot, mouse_num)%>%
  relocate(loc, slot, mouse_num)%>%
  
  #saving df of all previously viewed slides. 
  filter(sample_type=="slide")->slides_seen





#adding scanned slide locations to slides df above. 
left_join(slides_seen, scanned_slides)%>%
  
  
  #cleanup
  mutate(slot = as.numeric(slot))%>%
  arrange(loc, slot, mouse_num)%>%
  relocate(brain_ihc_obs)%>%
  rename(sheri_notes = brain_ihc_obs)->slides_seen


#view(slides_seen)




library(googlesheets4)
gs4_auth(email = "gotrimski@gmail.com")


range_write("1bdya6WH1KA-w3ziKprTpbxd-S1zJh1zPef754b8eDKY",
            slides_seen,
            sheet = "slides_all",
            col_names = TRUE,
            range = "b2")
