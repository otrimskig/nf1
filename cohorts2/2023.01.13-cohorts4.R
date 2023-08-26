####################################
#2023.01.13
#more changes to cohorts data.
#checking for wrongly-sacced mice (d147-149-ish)

library(tidyverse)
library(lubridate)
library(googlesheets4)


readRDS("ds/cohorts3.rds")->cohorts3


cohorts3%>%
  
mutate(aod = as.numeric(death_date-injection_date))%>%
  
#new column indicating whether mouse was sacced at endpoint or died/sacced prior. 
  mutate(exp_endpt = NA)%>%
  relocate(mouse_num, aod, exp_endpt)%>%
  arrange(desc(aod))%>%
  
  
  
  
  mutate(exp_endpt = if_else(is.na(exp_endpt)&
                            (grepl("experimental endpoint", comments, ignore.case = TRUE)|
                              grepl("end of experiment", comments, ignore.case = TRUE)|
                               grepl("Sac'd for end date", comments, ignore.case = TRUE)|
                                grepl("sacked for end date", comments, ignore.case = TRUE)|
                            grepl("experimental endpoint", n_comments, ignore.case = TRUE)|
                             grepl("end of experiment", n_comments, ignore.case = TRUE)|
                              grepl("Sac'd for end date", n_comments, ignore.case = TRUE)|
                               grepl("sacked for end date", n_comments, ignore.case = TRUE)),
                            
                        "1", as.character(exp_endpt)))->cohorts3.1
  
  



  cohorts3.1%>%
  filter(exp_endpt == "1"&aod<147)%>%
  
  mutate(exclude = "1")%>%
  mutate(metadata = paste(metadata, "...", "exluded for conflicting information in comments and sac dates. Exp_endpt changed to \"2\" to denote uncertain status."))%>%
  mutate(exp_endpt = "2")%>%
  mutate(metadata = gsub("^NA ... ", "", metadata))->update1
    
  
  cohorts3.1%>%
    anti_join(update1, by = c("mouse_num"))%>%
    full_join(update1)->cohorts3.2

  
  
  
  view(cohorts3.2)
  
  #gleaning additional information from comments. 
  cohorts3.2%>%
   select(mouse_num, 
          sex,
          dob, 
          injection_date,
          src,
          cells_injected,
          vol_injected,
          virus,
          ntva,
          pten,
          ink4a,
          atrx,
          h11cas9,
          tyr_cre,
          c(tis_brain:tum_eye),
          exclude,
          metadata,
          cohort)->cohorts3.2.vars1
  
    
  saveRDS(cohorts3.2.vars1, "ds/cohorts3.2.vars1.rds")  
    
    
  # cohorts3.2%>%
  # summarise_all(class) %>% 
  #   gather(col_name, col_type) %>% 
  #   mutate(desc = NA)%>%
  #   arrange(col_name)%>%
  #   write_csv("meta.csv")
    
  
cohorts3.2%>%
  select(-c(src,
                cells_injected,
                vol_injected,
                virus,
                ntva,
                pten,
                ink4a,
                atrx,
                h11cas9,
                tyr_cre,
                c(tis_brain:tum_eye)))->cohorts3.3


cohorts3.3%>%
  filter(is.na(exclude))%>%
  relocate(found_dead, .after = "exp_endpt")%>%
  filter(is.na(exp_endpt))%>%
  mutate(exp_endpt = ifelse(grepl("Sac for end date", n_comments, ignore.case = TRUE)|
                               grepl("End Date", n_comments, ignore.case = TRUE),
                             1, exp_endpt))%>%
  
  mutate(found_dead = ifelse(grepl("found dead", n_comments, ignore.case = TRUE)|
                                grepl("found dead", comments, ignore.case = TRUE),
                             1, found_dead))->update2
  
  
cohorts3.3%>%
  anti_join(update2, by = c("mouse_num"))%>%
  full_join(update2)->cohorts3.4
  
  
  
cohorts3.4%>%
  relocate(found_dead, .after = "exp_endpt")%>%
  mutate(exp_endpt = as.numeric(exp_endpt))%>%
  mutate(exp_endpt = ifelse(!is.na(found_dead)&found_dead == 1, 0, exp_endpt))%>%
  mutate(found_dead = ifelse(!is.na(exp_endpt)&exp_endpt == 1, 0, found_dead))->cohorts3.4.1
  
  

cohorts3.4.1%>%
  filter(is.na(exp_endpt)|is.na(found_dead))%>%
  filter(is.na(exclude))%>%

  mutate(found_dead = 0)%>%
  mutate(exp_endpt = 0)->update3


cohorts3.4.1%>%
  anti_join(update3, by = c("mouse_num"))%>%
  full_join(update3)->cohorts3.5


cohorts3.5%>%
  filter(is.na(exclude))%>%
  
  mutate(pa = ifelse(grepl("paraly", comments, ignore.case = TRUE)|
                       grepl("paraly", n_comments, ignore.case = TRUE),
                     1, NA))%>%
  relocate(pa, .after = "paralysis")%>%
  
  mutate(paralysis = ifelse(is.na(paralysis)&
                              !is.na(pa)&
                              pa==1, 
                            1, paralysis))%>%
  
  select(-pa)%>%
  
  mutate(paralysis = ifelse(is.na(paralysis)&
                       (grepl("mobility", comments, ignore.case = TRUE)|grepl("mobility", n_comments, ignore.case = TRUE)),
                     1, paralysis))->update5.6
  
  
  
  
cohorts3.5%>%
  anti_join(update5.6, by = c("mouse_num"))%>%
  full_join(update5.6)->cohorts3.6



cohorts3.6%>%
  filter(is.na(exclude))%>%
  
  filter(!is.na(necropsy_file_path))%>%
  
  
  view()




read_csv("../mouse cohorts/in_vivo_rick_clean.csv")->rick

rick%>%
  select(mouse_num, exclude, brain_h_e_and_ihc_notes, sheri_has_seen_h_e)%>%
  filter(is.na(exclude))%>%
  select(-exclude)%>%
  rename(brain_ihc_obs = 2, sheri_seen = 3)->rick_ihc

cohorts3.6%>%
left_join(rick_ihc%>%
            mutate(mouse_num = as.character(mouse_num)), by = "mouse_num"
          )%>%
  mutate(sheri_seen = ifelse(sheri_seen == "yes", "yes", NA))->cohorts3.6.1
  
  
  

cohorts3.6.1%>%
  filter(is.na(exclude))%>%
  filter(!is.na(necropsy_file_path))%>%
  count(sheri_seen)%>%
view()
 


read_csv("../necropsy files/colony_nec_ihc1.csv")->ihc_files


ihc_files%>%
  select(mouse_num, img_sample, staining, img_path)%>%
  filter(staining == "H&E")%>%
  group_by(mouse_num)%>%
  slice(1)%>%
  ungroup()%>%
  select(mouse_num)%>%
  mutate(mouse_num = as.character(mouse_num))%>%
  mutate(has_slidescan = 1)->ihc_mice


cohorts3.6.1%>%
  left_join(ihc_mice, by = "mouse_num")->cohorts3.6.2


cohorts3.6.2%>%
  select(exclude)%>%
  view()



 
cohorts3.6.2%>%
saveRDS("ds/cohorts4.0.rds")


#exporting filtered version for printing. 
cohorts3.6.2%>%
  filter(is.na(exclude))%>%
  filter(!is.na(necropsy_file_path))%>%
  filter(is.na(has_slidescan)&is.na(sheri_seen))%>%
  
gs4_create("cohorts", sheets = .)

###


