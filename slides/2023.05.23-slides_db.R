#standard libraries. 
#pulling dataset for all physical slides in lab. 
#imported data-entry to RDS file in previous R script. 

library(tidyverse)
library(janitor)




#cohort version used: cohorts4.0.1



slides0.1<-readRDS("slides/nf1_slides.rds")

cohorts<-readRDS("cohorts2/ds/cohorts4.0.1.rds")


#getting mice nums for non-excluded mice,
#HAVE necropsy
#sheri has NOT seen

cohorts%>%
  

  filter(is.na(sheri_seen)&
           !is.na(necropsy_file_path)&
                is.na(exclude))%>%
  select(mouse_num)->unseen_mice



slides0.1%>%
  group_by(mouse_num)%>%
  
  semi_join(unseen_mice)%>%
  arrange(mouse_num, loc, slot)%>%
  relocate(mouse_num, loc, slot)%>%
  
  
  filter(sample_type=="slide")->slides_unseen



#checking slides already scanned. 


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



str(scanned_slides)
str(slides_unseen)


left_join(slides_unseen, scanned_slides)->slides_unseen0.2







library(googlesheets4)
gs4_auth(email = "gotrimski@gmail.com")


range_write("1bdya6WH1KA-w3ziKprTpbxd-S1zJh1zPef754b8eDKY", 
            slides_unseen0.2, 
            sheet = "slides_latest", 
            col_names = TRUE, 
            range = "b3")
