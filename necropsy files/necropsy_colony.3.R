##################
library(tidyverse)
library(janitor)
library(stringr)
library(lubridate)

#columns that are organized satisfactorily and/or unnecessary. 
#for easy exclusion for convenient editing.
finished_cols<-c("initials", 
                 "strain",
                 "nec_date",
                 "sex",
                 "sac_date",
                 "injection_date",
                 "virus",
                 "dox_tx",
                 "genotype",
                 "slide_number_s",
                 "gross_visualization_of_lung_nodules",
                 "rcas_injection",
                 "injection_route",
                 "dob",
                 "zinc_fixed",
                 "h_e")



########################################


#starting from most recent colony dataset.
read_csv("colony_necropsy3.csv")%>%
  
  #want this var at the beginning always.
  relocate(mouse_num)%>%
  
  
  #cleaning up column to be more consistent for easier searching.
  mutate(h_e = case_when(grepl("yes", ignore.case = TRUE, h_e)~"yes",
                         grepl("no", ignore.case = TRUE, h_e)~"no",
                         TRUE~as.character(h_e)))%>%

  #using for easier check of which vars still need work.
  #select(-finished_cols)%>%
  
  #same.
  #select(mouse_num, tumor_qual, disposition)%>%
  
  
  #removing unnecessary beginning of string in tumor_qualitative column.
  #yes/no info was already extracted into tumor_bool column.
  #using nested gsub function here for expediency (as opposed to mutate x 100).
  #note that since function is nested, function performs sequential subs in reverse order.
  #this can be important for certain sequential subbing. 

  #"^x" using carrot to denote "at the start of" in gsub string search.
  mutate(tumor_qual = trimws(gsub("^yes\\, ", "", ignore.case = TRUE, 
                          gsub("^Y - ", "", 
                           gsub("^no", "", ignore.case = TRUE,
                                gsub("^maybe", "", ignore.case = TRUE,
                                tumor_qual))))))%>%
 
  #uniting 2 qualitative/descriptive columns. Keeping "..." between to
  #denote separation of information if necessary for future. 
  unite(tumor_obs, c(tumor_qual, disposition), na.rm = TRUE, sep = ". ... ")%>%
  
  #using "x$" using $ to denote "at the end of" in gsub string search.
  mutate(comments = gsub("_NA$", "", comments))%>%
  
  #again united qualitative columns as above. 
  unite(comments, c(tumor_obs, comments), na.rm = TRUE, sep = ". ... ")%>%
  #cleaning up the united column a bit.
  #this way works but is a bit tedious. You can use mutate with paste/paste0
  #next time and then deselect starting columns, allows for cleaner join. 
  mutate(comments = trimws(gsub("^\\,", "",
                            gsub("^:", "l",
                              gsub("^\\.", "",
                                gsub("^. ... ", "",
                                  gsub("... NA$", "",
                                    comments))))))
         
         #assigning to colony_necrospy4 for easier manipulation. 
         #can assign at end of process for easier assign.
         
         )->colony_necropsy4
  
########

#exporting progress for manual annotation.  
colony_necropsy4%>%
  select(1:3)%>%
  write_csv("tumor1.csv")
  
##########################

#joining back to full dataset. starting with last created df.
colony_necropsy4%>%
  #removing columns from df that were manually updated outside R.
  select(-tumor_bool, -comments)%>%

  #joining back, updated columns now in df.
  #file was named with ".m" to denote manual update outside R.
  left_join(read_csv("tumor1.m.csv"), by="mouse_num")%>%

  #cleaning up column where 2 obs were reported in single column.
  #using v1 as temporary holder of extracted obs. 
  #finds when string starts with 5 numbers in a row and extracts them.
  #these are improperly formatted dates. 
  mutate(v1 = as.character(str_extract_all(date_tumor_reported2, "^[0-9]{5}")))%>%
  
  #removing non-hits from column.
  mutate(v1 = case_when(nchar(v1)==5~v1,
                        TRUE~as.character(NA_character_)))%>%
 
   #changing them to properly formatted dates. 
  #dates exported from excel have an origin of the below date.
  #transforming them into date observations.
  mutate(v1 = as.Date(as.numeric(v1), origin = "1899-12-30")%>%as.character())%>%
  
  
  #removing the same search from original column.
  mutate(date_tumor_reported2 = gsub("^[0-9]{5}", "", date_tumor_reported2))%>%
  
  
  unite(v1, c(v1, date_tumor_reported2), na.rm = TRUE, sep = " ")%>%
  mutate(v1 =trimws(v1))%>%
  
  unite(comments, c(comments, v1), na.rm = TRUE, sep = ". ... ")%>%
  
  mutate(comments = trimws(gsub("\\. ... $", "",
                              gsub("^\\,", "",
                                gsub("^:", "l",
                                     gsub("^\\.", "",
                                          gsub("^. ... ", "",
                                               gsub("... NA$", "",
                                                    comments))))))))%>%
  
  #after all that... this variable seems unecessary. 
  #conflicts with observations of if tumor was obs. 
  select(-date_tumor_reported)%>%

  write_csv("colony_necropsy4.csv")
  
  
  
##############################################################################

finished_cols<-c("initials", 
                 "strain",
                 "nec_date",
                 "sex",
                 "sac_date",
                 "injection_date",
                 "virus",
                 "dox_tx",
                 "genotype",
                 "slide_number_s",
                 "gross_visualization_of_lung_nodules",
                 "rcas_injection",
                 "injection_route",
                 "dob",
                 "zinc_fixed",
                 "h_e",
                 "tissue_br",
                 "tumor_bool",
                 "parafin_block_number_s")


##


read_csv("colony_necropsy4.csv")%>%
  
  select(-finished_cols)%>%
  select(-formalin_fixed)%>%
  
  relocate(t, .after = mouse_num)%>%
  
  rename(tis_optic_n = tissue_optic_n, 
         tis_lung = tissue_lung,
         tis_liver = tissue_liver)%>%
  mutate(tis_spinal_c=NA)%>%
  
  
  
  mutate(tum_neck = NA)%>%
  mutate(tum_cran_n = NA)%>%
  mutate(tum_spinal_c = NA)%>%
  mutate(tum_head = NA)%>%
  mutate(tum_low_back = NA)%>%
  mutate(tum_skin = NA)%>%
  
  relocate(comments, .after = mouse_num
           
           )->colony_necropsy5
  
#######

colony_necropsy5%>%
write.csv(file="tis.tum.csv", na="", row.names = FALSE)

############################################################

read_csv("tis.tum.csv")%>%
  #remove columns updated outside R.
  select(-(5:last_col()))%>%

      #join back updated dataset.           
              left_join(read_csv("tis.tum.m.csv")%>%select(-comments,-t, -tissue_collected), 
                        by="mouse_num")%>%
 
  #ensure only 1 instance of each mouse num. 
  group_by(mouse_num)%>%
  slice(1)%>%
  ungroup()%>%
  
  #cleaning up t var. 
  mutate(t = if_else(t == "B", "Brain", t))%>%
  #prepping for merge into comment column. 
  mutate(t = if_else(is.na(t)==FALSE, 
                     paste0("Collected: ", t), 
                     NA_character_))%>%
  #merge into comment column. 
  mutate(comments = paste0(comments, " ... ", t))%>%
  #remove redundant columns. 
  select(-t, -tissue_collected)%>%
  clean_names()%>%
  select(-x22, -t1)%>%
  #re-make column for brain tissue and re-locate vars.
  mutate(tis_brain = 1)%>%
  relocate(c(1:3), tis_brain)%>%
  relocate(unknown_t1, .after = tis_eye
           
           )->nec4

################

#joining above back to main dataset with all vars. 
read_csv("colony_necropsy4.csv")%>%
  #removing updated columns from full dataset. 
  select(-t, -tissue_collected, -tissue_br, -tissue_optic_n, -tissue_lung, -tissue_liver, -comments)%>%
  
  left_join(nec4)%>%
  #removing unnecessary columns. This information is already present in other, 
  #cleaner dataset of mouse colony. 
  select(-strain, -genotype, -virus, -sex, -rcas_injection, -injection_route)%>%
  
  #clarifying variable to avoid confusion for future joins. 
  rename(nec_by = initials)%>%
  
  #all qualitative data is now united into one variable. 
  mutate(comments = paste0(comments, " ... ", "Formalin Fixed: ", formalin_fixed))%>%
  select(-formalin_fixed)%>%
  
  #some organization. 
  relocate(1, dob, injection_date, sac_date, nec_date)%>%
  
  #prepping variable for join. 
  mutate(mouse_num = as.character(mouse_num))%>%
  #adding file paths to necropsy dataset. 
  left_join(read_csv("necropsy_file_paths.csv"), by = "mouse_num")%>%
  #ensuring no duplicates. 
  group_by(mouse_num)%>%
  slice(1)%>%
  ungroup()%>%
  
  
  write_csv("colony_necropsy5.csv")
  
##########################################################################################

#edited most recent colony dataset manually, most updated version
#is colony_necropsy5.m.csv. ".m" denotes manual edit. 


read_csv("colony_necropsy5.m.csv")%>%
  select(-necropsy_file_path)%>%
  
  view()


##############

#use "../" to move up one directory in file path. Still can use relative file paths. 
read_csv("../slides/slide_info.csv")%>%
  select(-hci_number, -folder, -filename)%>%
  rename(img_by = prep_by)%>%
  right_join(read_csv("colony_necropsy5.m.csv")%>%
               select(-necropsy_file_path)%>%
               mutate(mouse_num = as.character(mouse_num)), 
             by = "mouse_num")%>%
  relocate(tumor:slide_number, .after = last_col())%>%
  rename(img_sample = tumor)%>%
  rename(nec_comments = comments)%>%
  select(-unknown_t1)%>%
  rename(img_path = path)%>%

  write.csv("colony_nec_ihc1.csv", na="", row.names = FALSE)
  

#################################

#filling in the blanks.

read_csv("colony_nec_ihc1.csv")%>%
  filter(!is.na(img_path))%>%
  write.csv("ihc-plus-nec.csv", na="", row.names = FALSE)


###############################

read_csv("../mouse cohorts/compiled_cohorts3.csv")%>%
  clean_names()%>%
  select(-x2
         
         )->colony_full1

#######
read_csv("ihc-plus-nec.csv"
         
         )->ihc_nec1

#######
ihc_nec1%>%
    view()

##############################
  colony_full1%>%
  
  select(mouse_num,
         tumor_id,
         patho_est_location,
         patho_tumor_grade,
         patho_notes,
         requests,
         tumor,
         mouse_tumor,
         necropsy_file_comments,
         tumor_locations,
         tumor_grade_post_h_e,
         notes,
         other_organs_harvested,
         necropsy_comments,
         brain_h_e_and_ihc_notes
         
         )->colony_full_desc
  
##########################  
  
#testing transformations
colony_full_desc%>%
  select(mouse_num, tumor_id, tumor, mouse_tumor)%>%
  
  mutate(tumor_id = substr(tumor_id, 7, nchar(tumor_id)))%>%
  mutate(mouse_tumor = substr(mouse_tumor, 7, nchar(mouse_tumor)))%>%
  mutate(tumor_id = if_else(is.na(tumor_id), tumor, tumor_id))%>%
  mutate(tumor_id = if_else(is.na(tumor_id), mouse_tumor, tumor_id))%>%
  select(-tumor, -mouse_tumor)%>%

  
  view()


######
#using same transformations as tested above. 
colony_full1%>%
  mutate(tumor_id = substr(tumor_id, 7, nchar(tumor_id)))%>%
  mutate(mouse_tumor = substr(mouse_tumor, 7, nchar(mouse_tumor)))%>%
  mutate(tumor_id = if_else(is.na(tumor_id), tumor, tumor_id))%>%
  mutate(tumor_id = if_else(is.na(tumor_id), mouse_tumor, tumor_id))%>%
  select(-tumor, -mouse_tumor
         
         )->colony_full2

##################

colony_full2%>%
  select(-necropsy_file_comments)%>%
  rename(brain_ihc_notes = brain_h_e_and_ihc_notes)%>%
  # select(mouse_num,
  #        tumor_id,
  #         patho_est_location,
  #         patho_tumor_grade,
  #         patho_notes,
  #         requests,
  #     
  #         tumor_locations,
  #         tumor_grade_post_h_e,
  #         notes,
  #         other_organs_harvested,
  #         necropsy_comments,
  #         brain_ihc_notes)%>%
  # 
  # select(mouse_num, tumor_id,
  #        
  #        notes,
  #        necropsy_comments,
  #        brain_ihc_notes)%>%
  
  mutate(col_notes = paste0(notes, " ... ", necropsy_comments, " ... ", necropsy_behavior_comments))%>%
  mutate(col_notes = gsub("NA \\.\\.\\. NA", "", col_notes))%>%
  mutate(col_notes = gsub("\\.\\.\\. NA$", "", col_notes))%>%
  mutate(col_notes = trimws(gsub ("\\.\\.\\. $", "", col_notes)))%>%
  mutate(col_notes = trimws(gsub ("^\\.\\.\\.", "", col_notes)))%>%
  select(-notes, -necropsy_comments, -necropsy_behavior_comments
         
         )->colony_full3
  
####################
  
colony_full3%>%
    colnames()%>%
  as_tibble()%>%
  mutate(char = nchar(value))%>%
  rename(name1 = value)%>%
  mutate(name2 = "")%>%
  relocate(name1, name2)%>%
  
  write_csv("colnames.csv")

####

read_csv("colnames.m.csv")->colony_vars

#rename columns using dataset of from/to names. 
colony_full3%>%
  rename_at(vars(colony_vars$name1), function(x) colony_vars$name2)->colony_full3


#####
colony_full3%>%
  left_join(read_csv("ihc-plus-nec.csv"))%>%
  
  relocate(mouse_num)->colony_full4
  
  
#########################################################



colony_full4%>%
 
  rename(tum_bool1 = tumor_bool, tum_bool2 = had_tumor, tum_bool3 = evidence_of_tumor1)%>%
  relocate(tum_bool1, tum_bool2, tum_bool3, .after = last_col())%>%
  
  mutate(exclude = ifelse(is.na(exclude), xclude, exclude))%>%
  select(-xclude)%>%
  relocate(exclude, .after = last_col())%>%
  
  
 
  #comparing all date columns, sac_date is incomplete and/or doesn't agree with death date.
  #using death_date as the correct term.
  select(-sac_date)%>%
  rename(mri_date = mri_imaging_date)%>%
  rename(sex = gender)%>%
  rename(date_tum_obs = tumor_noticed, 
         date_beh_obs = behavior_noticed, 
         date_he_submit = he_submitted,
         patho_requests = requests)%>%
  relocate(mouse_num, 
           sex,
           dob, 
           injection_date, 
           date_beh_obs,
           date_tum_obs,
           mri_date, 
           death_date, 
           end_date, 
           nec_date,
           date_he_submit,
           slide_date,
           img_date)%>%
  
 select(-age_death_capped, -age_death_cap)%>%
  #temp selection
  select(-contains("date"), -dob, -sex, -exclude, -c(tum_bool1:tum_bool3))%>%

  #found missing/confliced info in tumor_id vs img_sample.
  #checked for mismatches and confirmed tumor_id is more accurate.
  mutate(img_sample = tumor_id)%>%
  select(-tumor_id)%>%
  
  
  mutate(tyr_cre = ifelse(is.na(tyr_cre_status), tyr_cre_conf, tyr_cre_status))%>%
  select(-tyr_cre_status, -tyr_cre_conf)%>%
  
  write_csv("colony_necropsy5.csv")
  
 
  
  ###################################
