#going from raw data, to compiled_cohorts3. 
#sourced only based on rick and riley's data, plus
#strain_info<-read_csv("ds/nf1-cohort genos and names.csv").
#no other external sources. 

library(tidyverse)
library(janitor)
library(openxlsx)
library(lubridate)
library(stringr)

RE.df1.0 <- read.xlsx("raw/Riley NF1 Glioma Data (Mouse Cohorts).xlsx", 
                       sheet = "Mouse Information",
                       detectDates = TRUE)


RE.df1.1<-RE.df1.0%>%
  
  clean_names()%>%
  
  #new clean column mouse_num from mouse_number.
  #remove tag information. 
  mutate(mouse_num = substr(mouse_number, 1,5))%>%
  relocate(mouse_num)%>%
  #filter out any rows that aren't mouse numbers.
  filter(grepl("[0-9]", mouse_num))%>%

  #parsing out the injected by, 
  #from the improperly formatted injection_date column. 
  mutate(injected_by =  str_match(injection_date, "\\((.*?)\\)")[, 2])%>%
  
  #now that injected_by info is preserved
  #in separated column, fixing injection_date formatting
  #removes extraneous characters.
  mutate(injection_date = str_replace_all(injection_date, "[^0-9/-]", ""))%>%
  #gets multiple formatted dates into 1 date format. Uses lubridate. 
  mutate(injection_date = case_when(
    str_detect(injection_date, "/") ~ mdy(injection_date),
    str_detect(injection_date, "-") ~ ymd(injection_date),
    TRUE ~ NA_Date_))%>%
 
   #adding src for source document, if conflicts need to 
  #be resolved later. 
  mutate(src = "RE")


#now first bit of rick's data. 
RH.df1.0  <- read.xlsx("raw/NF1 Glioma Mouse Information (Rick).xlsx",
                      sheet = "In Vivo Tumor Study",
                      detectDates = TRUE)%>%
   clean_names()


RH.df1.1<-RH.df1.0%>%
  clean_names()%>%
  #remove tag information. 
  
  mutate(mouse_num = substr(mouse_id_greyed_out_if_it_will_be_excluded_from_results, 1,5))%>%
  relocate(mouse_num)%>%
  
  #filter out any rows that aren't mouse numbers.
  filter(grepl("[0-9]", mouse_num))




#master list of any mouse_nums listed in either dataset, without dupes. 
#these should be all the mice in our cohorts. 
mouse_nums<-full_join(RH.df1.1, RE.df1.1)%>%
  group_by(mouse_num)%>%
  slice(1)%>%
  select(mouse_num)

#saving this for later reference/use, if necessary. 
saveRDS(mouse_nums, "ds/cohort_mouse_nums.rds")




#using mouse_nums to join data, so there are no duplicates. 
#conflicting columns will be labeled based on their source. 
df1.0<-mouse_nums%>%
  left_join(RH.df1.1)%>%
  left_join(RE.df1.1, by = "mouse_num")

#labeling source columns that appear in both datasets. 
colnames(df1.0) <- sub("\\.x$", ".RH", colnames(df1.0))
colnames(df1.0) <- sub("\\.y$", ".RE", colnames(df1.0))

#arranging columns by names to make rick/riley comparisons easier. 
df1.1<-df1.0%>%
  select(sort(names(.)))%>%
  relocate(mouse_num)%>%
  
  #use grouping variables to avoid having to name them in selections. 
  group_by(mouse_num, metadata, exclude)%>%
  
  #select(mouse_num, contains("notes"))%>%
  

  rename("notes_behavior_and_nec" = behavior_and_necropsy_notes,
         "notes_he_ihc_brain" = brain_h_e_and_ihc_notes)%>%
  select(sort(names(.)))%>%
  relocate(mouse_num)%>%

  # select(mouse_num, contains("date"), dob)%>%
  # select(birth_date, dob)%>%
  
  #dob column
  rename(dob.1 = dob)%>%
  mutate(dob = coalesce(dob.1, birth_date))%>%
  select(-birth_date, -dob.1)%>%
  
  
  #select(metadata, contains("injection"))%>%
  
  
  
  mutate(injection_date = coalesce(injection_date.RH, injection_date.RE))%>%
  mutate(metadata = if_else(!is.na(injection_date.RE)&
                              !is.na(injection_date.RH)&
                              injection_date.RH!=injection_date.RE,
                            paste(metadata, "...", "conflicting injection dates. Used Rick's data.", injection_date.RE, "vs", injection_date.RH),
                            metadata))%>%
  
  
  mutate(metadata = sub("^NA ... ", "", metadata))%>%
  
  select(-injection_date.RE, -injection_date.RH)%>%
  
  #temp selection. 
  # select(contains("behavior"))%>%
  #removing columns
  select(-behavior_onset_age, - age_of_behavior_onset)%>%
  
  rename(behavior_date.1 = behavior_date)%>%
  mutate(behavior_date = coalesce(behavior_date.1, behavior_noticed))%>%
  select(-behavior_date.1, -behavior_noticed)%>%
  
  
  select(-contains("age_"))%>%
  select(-contains("_age"))
  
  
df1.2<-df1.1%>%
  
  rename(brain_embedding_submission_date = 
           brain_submission_date_for_embedding_and_h_e_highlighted_blue_if_block_is_useful_as_a_control_or_otherwise_includes_a_large_tumor)%>%
  rename(cranial_cavity_tumor = 
           cranial_nerve_tumor_anything_in_the_cranial_cavity_but_not_necessarily_infiltrating_or_inside_the_brain_most_likely_located_under_the_brain)%>%
  rename(proof_of_abnormal_mass = 
           any_hard_proof_of_any_kind_of_abnormal_mass_or_enlargement_after_necropsy_or_brain_h_e)%>%
  rename(auditory_nerve_abnormal_connection = 
           auditory_nerve_not_connected_to_skull_like_head_tumors_but_on_head_and_connected_to_skin_sort_of)%>%
  
  
  
  
  
  
  #fixing gender column. 
  mutate(gender = coalesce(gender.RE, gender.RH))%>%
  mutate(gender = substr(gender, 1,1))%>%
  select(-gender.RE, -gender.RH)%>%
  relocate(gender, .after = last_col())%>%
  relocate(proof_of_abnormal_mass, auditory_nerve_abnormal_connection, .after = last_col())
  
 

#getting conflicting dates. 
 df1.2%>%
  
  filter(!is.na(death_date)&!is.na(euthanasia_or_found_dead_date)&death_date!=euthanasia_or_found_dead_date)%>%
  mutate(metadata = paste("conflicting death dates.", death_date, "vs", euthanasia_or_found_dead_date))%>%
  mutate(exclude = 2)->dd_conflict_notes
 
  
#adding conflicting dates metadata to main dataset.   
 df1.3<-df1.2%>%
   anti_join(dd_conflict_notes, by = "mouse_num")%>%
   full_join(dd_conflict_notes)%>%
   mutate(death_date = coalesce(euthanasia_or_found_dead_date, death_date))%>%
   
   
   #filter(is.na(end_date))
    relocate(brain_embedding_submission_date, .after = last_col())%>%
   
   relocate(exclude, mouse_num, dob, injection_date, death_date, end_date)%>%
  
   #removing unecessary end dates where death date is filled in, to avoid confusion. 
   #now every mouse either has a death date, OR an end date, not both.
    mutate(end_date = if_else(!is.na(death_date), NA, end_date))%>%
   select(-euthanasia_or_found_dead_date)%>%
   
  # filter(!is.na(found_dead))%>%
   
   select(-mouse_number)%>%
   rename(mouse_id = mouse_id_greyed_out_if_it_will_be_excluded_from_results)%>%
   
   relocate(cranial_cavity_tumor, .after = last_col())%>%
   select(-euthanized)%>%
   select(-found_dead)%>%
   rename(found_dead = found_dead_2)%>%
   relocate(found_dead, .after = end_date)

#moving all data to be used for tumor characterization/not easily graphable for survival curves,
 #to end of dataset. Will use later, but for now separating easy clean dates
 #from observations and notes
df1.4<-df1.3%>%
  relocate(cutaneous_skin_somewhere_on_body, .after = last_col())%>%
  rename(observable_tumor_date = domed_head_or_observable_tumor_date)%>%
  relocate(observable_tumor_date, .after = injection_date)%>%
  relocate(eye_tumor_where_it_was_clear_it_was_the_optical_nerve_and_or_involving_the_eye, .after = last_col())%>%
  relocate(fixed_tissue_still_in_ethanol, .after = last_col())%>%
  relocate(contains("full_slide"), .after = last_col())%>%
  rename(glioma_in_brain_verified_ki67 = glioma_free_survival_1_if_a_glioma_inside_brain_or_infiltrating_the_brain_verified_by_ki67_0_if_no_observable_glioma_is_present_exclude_if_h_e_wasnt_done_or_mouse_was_sacked_before_150_days_for_whatever_reason)



df1.5<-df1.4%>%
  relocate(glioma_in_brain_verified_ki67, .after = last_col())%>%
  
  #select(injected_by, injected_by_highlighted_green_for_variable_im_testing_for)%>%
  #cleaning injected_by column.
  mutate(injected_by = coalesce(injected_by, injected_by_highlighted_green_for_variable_im_testing_for))%>%
  select(-injected_by_highlighted_green_for_variable_im_testing_for)%>%
  mutate(injected_by = gsub("Rick", "RH", injected_by))%>%
  mutate(injected_by = gsub("Zia", "ZL", injected_by))%>%
  
  relocate(injected_by, injected_with, .after = found_dead)%>%
  relocate(mri_imaging_date, .after = injection_date)%>%
  relocate(number_of_cells_injected, .after = injected_by)%>%
  
  relocate(harvested_brain, harvested_other_organs, head_tumor_outside_of_and_connected_to_skull, .after = last_col())


df1.6<-df1.5%>%
  relocate(hydrocephalus, 
           intracranial_glioma_or_outside_of_brain_and_still_in_skull_combo_of_previous_2,
           ki67_result,
           lethargic_difficulty_breathing_looked_sick,
           lower_spinal_cord_tumor_unfortunatly_didnt_start_looking_for_this_until_later,
           lymph_node_tumor_wasnt_always_looking_for_but_noticed_when_they_were_big,
           mouse_id,
           .after = last_col())%>%
  #cleaning src column
  unite("src", src.RH, src.RE, sep = " + ", na.rm = TRUE)%>%
  #cleaning strain column
  mutate(strain = coalesce(strain.RH, strain.RE))%>%
  select(-strain.RE, -strain.RH)%>%
  relocate(strain, .after = exclude)%>%
  relocate(behavior_date, .after = injection_date)
  

df1.7<-df1.6%>%
relocate(src, shorthand_strain_name, tumor_noticed, tva_status, tyr_cre_status, 
           tyr_cre_status_confirmed, virus, volume_injected_highlighted_green_for_variable_im_testing_for,
           gender)%>%
  # select(tumor_noticed, observable_tumor_date)%>%
  #cleaning tumor_noticed column by combining 2 columns with same data. 
  mutate(observable_tumor_date = coalesce(observable_tumor_date, tumor_noticed))%>%
  select(-tumor_noticed)%>%
  
  #same thing here
  mutate(tyr_cre = coalesce(tyr_cre_status, tyr_cre_status_confirmed))%>%
  select(-tyr_cre_status, -tyr_cre_status_confirmed)%>%
  relocate(tyr_cre, .after = injection_date)%>%
 
   #not necessary.
  select(-shorthand_strain_name)%>%
  relocate(tva_status, .after = tyr_cre)%>%
  
  #2 differently named columns with same meaning.
  mutate(injected_with = coalesce(injected_with, virus))%>%
  select(-virus)%>%
  #same
  rename(vol_injected = volume_injected_highlighted_green_for_variable_im_testing_for)%>%
  relocate(vol_injected, .after = number_of_cells_injected)%>%
  
  relocate(mouse_num, exclude, src, strain, tva_status, tyr_cre)%>%
  ungroup()




df1.8<-df1.7%>%
  #cleaning strain and injected_with columns,
  #so that the groups will be grouped properly.
  #clearing up the formatting on the text of strains and viruses. 
  #want all genes to be in the same order for easier viewing. 
  
  #viruses:
  mutate(injected_with = sub("RCAS Cre-U6sgRNA-ex2-NF1",	
                             "RCAS-Cre-pENTR-U6-sgRNA-NF1-ex2", injected_with))%>%
  mutate(injected_with = sub("Rcas Cre", "RCAS-Cre", injected_with))%>%
  
  #strain names:
  mutate(strain = sub("N-TVA::H11LSL-Cas9;Ink4a/Arf f/f", 
                      "N::TVA; H11LSL-Cas9 f/f; Pten +/+; Ink4a/Arf f/f; ATRX +/+",
                      strain))%>%
  
  mutate(strain = sub("N-TVA; Pten f/f; H11LSL-Cas9 f/f; Ink4a/Arf f/f; ATRX f/f",
                      "N::TVA; H11LSL-Cas9 f/f; Pten f/f; Ink4a/Arf f/f; ATRX f/f",
                      strain))%>%

  mutate(strain = sub("N-TVA::H11LSL-Cas9", 
                      "N::TVA; H11LSL-Cas9 f/f; Pten +/+; Ink4a/Arf +/+; ATRX +/+", strain))%>%
  
  #no idea why I had to use if_else here instead of sub as above. Code wasn't working with "sub"
  mutate(strain = if_else(strain =="N-TVA; Pten f/f; H11LSL-Cas9 f/f; Ink4a/Arf f/f; ATRX +/+", 
                          "N::TVA; H11LSL-Cas9 f/f; Pten f/f; Ink4a/Arf f/f; ATRX +/+", strain))



#adding in external strain info with resultant (effective) genotypes for each strain x virus combo
#external sheet info here connecting strain and virus name to resultant geno column.
strain_info<-read_csv("ds/nf1-cohort genos and names.csv")



#joining that info to the main dataset.
df1.9<-df1.8%>%
 left_join(strain_info)%>%
  #a little clean-up to make the later division of the datasets easier. (dates vs. observations)
  relocate(injected_with, resultant_geno, n, .after = strain)%>%
  #realizing that because of different tva status, the
  #n column will not be correct. Furthermore it can very easily be re-tallied
  #once final group numbers can be known. Removing to avoid confusion.
  select(-n)%>%
  
  #clean-up this column; noticed there were some empty values that should have been "NA"
  mutate(tyr_cre = if_else(tyr_cre ==" ", NA, tyr_cre))%>%
  #make sure that if TVA status is negative, that resultant_geno shows that. 
  #want to avoid data analysis that accidentally includes those mice in the same 
  #group as the tva-pos injected mice.
  mutate(resultant_geno = if_else(!is.na(tva_status)&tva_status=="TVA-", "tva negative", resultant_geno))
  #there remain plenty of mice that have NAs for tva_status. That will
#have to be remedied prior to analysis. 
#however, to avoid confusion, since I will have to use another external source,
#I will do that in a subsequent script. 


#dividing the dataset in 2, and saving as rds files for downstream use.
df1.9%>%
  select(mouse_num:metadata, -tyr_cre)%>%
  saveRDS("ds/nf1_cohorts.1.dates.rds")

df1.9%>%
  select(mouse_num, tyr_cre, non_tumor_related_reason_for_death:last_col())%>%
  saveRDS("ds/nf1_cohorts.1.observations.rds")





