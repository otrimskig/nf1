# #going from raw data, to compiled_cohorts3. 
# 
# library(tidyverse)
# library(janitor)
# library(openxlsx)
# library(lubridate)
# library(stringr)
# 
# nf1_riley <- read.xlsx("raw/Riley NF1 Glioma Data (Mouse Cohorts).xlsx", 
#                        sheet = "Mouse Information",
#                        detectDates = TRUE)
# 



# 
# #import csv file, which was saved from xcel file on 11/8/22, with cohort data complete. 
# read_csv("raw/Riley NF1 Glioma Data (Mouse Cohorts).csv")%>%
 
  
  #  #housekeeping
  # clean_names()%>%
  # select(-x19)%>%
  
  # #remove all columns that don't have mouse numbers.
  # arrange(desc(mouse_number))%>%
  # slice(-(1:14))%>%
  # arrange(mouse_number)%>%
  # 
  # #clean up injection_date column, where some of the dates also have initals of person who injected. 
  # #backup column data
  # mutate(injection_date2 = injection_date)%>%
  # #in new column, delete everything before the space. 
  # mutate(injection_date = gsub(" .*", "", injection_date))%>%
  # #convert all date columns to date, (uses lubridate function)
  # mutate(injection_date = mdy(injection_date))%>%
  # mutate(dob = mdy(dob))%>%
  # mutate(death_date = mdy(death_date))%>%
  # mutate(tumor_noticed = mdy(tumor_noticed))%>%
  # mutate(behavior_noticed = mdy(behavior_noticed))%>%
  # mutate(end_date = mdy(end_date))%>%
  # mutate(mri_imaging_date = mdy(mri_imaging_date))%>%
  
#####################################################moved  
  
  # #extract info from mouse number column, which contains both mouse number and tag number. separate out into new columns. 
  # mutate(mouse_num_tag = mouse_number)%>%
  # mutate(mouse_num = substr(mouse_num_tag, 1, 5))%>%
  # mutate(mouse_tag = substr(mouse_num_tag, 7,8))%>%
  # select(-mouse_number)%>%
  # relocate(mouse_num)%>%
  
  # #extract injected_by info from injection date + initials combo. 
  # rename(injected_by = injection_date2)%>%
  # #removes all characters before the space
  # mutate(injected_by = gsub(".* ", "", injected_by))%>%
  # #removes parentheses
  # mutate(injected_by = gsub("\\(", "", injected_by))%>%
  # mutate(injected_by = gsub("\\)", "", injected_by))%>%
  # #all dates that didn't have initials, were left unchanged by the above parsing.
  # #change all strings longer than 3 (ie non initials) to arbitrary 
  # mutate(injected_by = if_else(nchar(injected_by) > 3, "NA", injected_by))%>%
  # mutate(injected_by = na_if(injected_by, "NA"))%>%
  # 
  # mutate(death_date = coalesce(death_date, end_date))%>%
  # mutate(end_date = coalesce(end_date, death_date))%>%
  # mutate(age_death_cap = as.numeric(death_date - injection_date))%>%
  # mutate(age_death_cap = if_else(is.na(tumor_locations)==TRUE, 151, age_death_cap))%>%
  
  # 
  # write.csv("glioma_cohorts_clean.csv", row.names = FALSE)
############done

#checking to see if Riley's survival data has anything not included in above csv.
# #make counts table of all instances of age of death, in both datasets. 
# count1<-
#   read_csv("glioma_cohorts_clean.csv")%>%
#   as_tibble()%>%
#   count(age_at_death)


# #count2<-
# read_csv("redundant/Riley Glioma Results Survival All Cohorts.csv")%>%
#   as_tibble()%>%
#   clean_names()%>%
#   select(1,2)%>%
#   count(age_days)
# 
# #join by number of instances. 
# full_join(count1, count2, by = c("age_at_death" = "age_days"))%>%
#   view()
# #all age of death counts are larger or equal in my dataset. 
# #for a more robust check, join 2 datasets by mouse number.
# read_csv("redundant/Riley Glioma Results Survival All Cohorts.csv")%>%
#   as_tibble()%>%
#   clean_names()%>%
#   select(1,2)%>%
#   full_join(read_csv("glioma_cohorts_clean.csv"), by = c("x1" = "mouse_num_tag"))%>%
# #   select(1,2, age_at_death)%>%
#   view()
#confirmed all matches, with glioma_cohorts_clean having additional data. Riley Glioma Results Survival All is redundant.
#moving to redundant file folder to avoid confusion. 


##################################################done
####################################################################

nf1_rick <- read.xlsx("raw/NF1 Glioma Mouse Information (Rick).xlsx",
                      sheet = "In Vivo Tumor Study",
                      detectDates = TRUE)

#rick data
read_csv("raw/in vivo rick.csv", col_names = FALSE)%>%
  as_tibble()%>%
  slice(-1)%>%
  row_to_names(1)%>%clean_names()%>%
  rename(mouse_id = 2)%>%
  filter(is.na(mouse_id)==FALSE)%>%
  mutate(mouse_num = substr(mouse_id, 1,5))%>%
  relocate(mouse_num)%>%
  mutate(gender = if_else(gender == "Male", "M",
                          if_else(gender == "Female", "F", gender)))%>%
  rename("is_useful_as_a_control_or_otherwise_includes_a_large_tumor" = "highlighted_blue",
         injected_by = "injected_by_highlighted_green_for_variable_im_testing_for",
         vol_injected = "volume_injected_highlighted_green_for_variable_im_testing_for",
         h_e_submission_date = "brain_submission_date_for_embedding_and_h_e_highlighted_blue_if_block_is_useful_as_a_control_or_otherwise_includes_a_large_tumor", 
         tissues_submitted = "tumor_or_other_tissue_submitted_for_embedding_and_h_e_either_with_the_brain_or_separate",
         proof_of_mass = "any_hard_proof_of_any_kind_of_abnormal_mass_or_enlargement_after_necropsy_or_brain_h_e"
  )%>%
  #reformatting all date columns to dates.
  mutate_at(vars(13:17,23), mdy)%>%
  mutate(src = "RH")%>%
  mutate(injected_by = if_else(injected_by == "Rick", "RH", injected_by))%>%
  
  
  
  
  #colnames()
  #view()
  
  write.csv("in_vivo_rick_clean.csv", row.names = FALSE)




###############note: haven't added this to new. Adding rick's cohorts first. 

#adding necropsy file   
read_excel("raw/Riley NF1 Glioma Data (Mouse Cohorts).xlsx", 
           sheet = "Pathologist Comments")%>%
  as_tibble()%>%
  clean_names()%>%
  rename(tumor_id = mouse_number)%>%
  mutate(mouse_num = as.numeric(substr(tumor_id, 1,5)))%>%
  relocate(mouse_num)%>%
  mutate(tumor = gsub(".* ", "", tumor_id))%>%
  relocate(tumor, .after = mouse_num)%>%
  rename(mouse_tumor = tumor_id)%>%
  
  
  full_join(read_csv("glioma_cohorts_clean.csv"), by = c("mouse_num"))%>%
  filter(is.na(mouse_num)==FALSE)%>%
  arrange(mouse_num)%>%
  #clean up data labeled as "see above"
  mutate(necropsy_file_comments = if_else(mouse_num == 28208, "extracranial tumor, sacked for tumor burden. Little movement, squinted eyes. Possible blood clot on the liver. 2 extra possible tumors: one close to the kindeys, the other close to the lower intestines (bladder?)", necropsy_file_comments))%>%
  write.csv("glioma_cohorts_clean2.csv", row.names = FALSE)





read_excel("raw/NF1 Glioma Pathology Reports.xlsx")%>%
  as_tibble()%>%
  clean_names()%>%
  mutate(mouse_num = as.numeric(substr(mouse_number, 1,5)))%>%
  relocate(mouse_num)%>%
  
  #relocate(tumor, .after = mouse_num)%>%
  rename(tumor_id = mouse_number)%>%
  separate_rows(c(tumor_id, est_location), sep = ",")%>%
  mutate(tumor = gsub(".* ", "", tumor_id))%>%
  mutate(tumor_id = paste0(mouse_num, " ", tumor))%>%
  #remove leading spaces in column with trimws function. 
  mutate(est_location = trimws(est_location, "left"))%>%
  rename(patho_est_location = est_location, patho_tumor_grade = tumor_grade, patho_notes = notes)%>%
  full_join(read_csv("glioma_cohorts_clean2.csv"), by = c("mouse_num", "tumor"))%>%
  mutate(src = "RE")%>%
  mutate(necropsy_comments = necropsy_file_comments)%>%
  
  write.csv("glioma_cohorts.clean3.csv", row.names = FALSE)












#################################
#attempting to unify all data. 

full_join(read_csv("glioma_cohorts.clean3.csv"),
          read_csv("in_vivo_rick_clean.csv"))%>%
  write_csv("data_all.csv")
################################

#rm all mice w/o death dates. can look for those later if need be. 



full_join(read_csv("glioma_cohorts.clean3.csv"),
          read_csv("in_vivo_rick_clean.csv")%>%
            
            
            
            
            filter(is.na(death_date)==FALSE)%>%
            select(-shorthand_strain_name)%>%
            relocate(exclude, .after = last_col())%>%
            rename(necropsy_behavior_comments = behavior_and_necropsy_notes)%>%
            rename(tumor_noticed = domed_head_or_observable_tumor_date,
                   behavior_noticed = behavior_date,
                   dob = birth_date,
                   mouse_num_tag = mouse_id)%>%
            select(-death_age_or_current_age_for_survival_curve)%>%
            mutate(src = "RH"), 
          by = c("mouse_num", "gender", 
                 "dob", "injection_date", "tumor_noticed", 
                 "behavior_noticed", "death_date", "injected_by",
                 "mouse_num_tag", "src",
                 "virus" = "injected_with")
)%>%
  filter(is.na(dob)==FALSE)%>%
  select_at(vars(-age_at_death,
                 -age_of_tumor_onset,
                 -age_of_behavior_onset))%>%
  write_csv("compiled_cohorts.csv")

#########
#########




read_csv("compiled_cohorts.csv")%>%
  separate(strain.y, sep = ";",
           into = c("tva","pten", "h11", "ink", "atrx"))%>%
  mutate(tva = "N-TVA::")%>%
  mutate(strain.x = gsub(".*::", "", strain.x)%>%trimws())%>%
  mutate(pten = if_else(grepl("Pten f/f", strain.x), "Pten f/f", pten))%>%
  mutate(h11  = if_else(grepl("H11LSL-Cas9", strain.x), "H11LSL-Cas9 f/f", trimws(h11)))%>%
  mutate(ink  = if_else(grepl("Ink4a/Arf f/f", strain.x), "Ink4a/Arf f/f", ink))%>%
  mutate(atrx  = if_else(grepl("Atrxf/f", strain.x), "Atrxf/f", atrx))%>%
  select(-strain.x)%>%
  mutate(virus = if_else(virus =="Rcas Cre", "RCAS-Cre",
                         if_else(virus=="RCAS Cre-U6sgRNA-ex2-NF1", "RCAS-Cre-pENTR-U6-sgRNA-NF1-ex2", virus)))%>%
  relocate(h11, .after = tva)%>%
  mutate(pten = if_else(is.na(pten), "Pten +/+", trimws(pten)))%>%
  mutate(ink = if_else(is.na(ink), "Ink4a/Arf +/+", trimws(ink)))%>%
  mutate(atrx = if_else(is.na(atrx), "ATRX +/+", atrx))%>%
  mutate(atrx = gsub("atrx", "ATRX", atrx, ignore.case = TRUE)%>%trimws())%>%
  mutate(atrx = if_else(atrx == "ATRXf/f", "ATRX f/f", atrx))%>%
  
  mutate(strain = paste(sep = ";", paste0(tva, h11), pten, ink, atrx))%>%
  mutate(nf1_ko = if_else(grepl("NF1", virus), "nf1 KO", "nf1 wt"))%>%
  mutate(pten_ko = if_else(grepl("f/f", pten), "pten KO", "pten wt"))%>%
  mutate(ink_ko = if_else(grepl("f/f", ink), "ink KO", "ink wt"))%>%
  mutate(atrx_ko = if_else(grepl("f/f", atrx), "atrx KO", "atrx wt"))%>%
  mutate(genes_ko = paste(nf1_ko, pten_ko, ink_ko, atrx_ko, sep = ";"))%>%
  
  relocate(virus, .after = strain)%>%
  mutate(age_death_capped = as.numeric(death_date - injection_date))%>%
  mutate(elapsed_from_tumor = as.numeric(death_date - tumor_noticed))%>%
  mutate(elapsed_from_behavior = as.numeric(death_date - behavior_noticed))%>%
  relocate(age_death_capped, .after = death_date)%>%
  mutate(age_death_capped = if_else(age_death_capped > 148, 300, age_death_capped))%>%
  mutate(age_death_capped = if_else(grepl("end of experiment, looked normal", necropsy_behavior_comments), 300,
                                    if_else(grepl("No tumor. Sac'd for end date.", notes), 300,
                                            if_else(grepl("Experiment endpoint", necropsy_behavior_comments), 300, age_death_capped))))%>%
  write.csv("compiled_cohorts2.csv")



############################################################
############################################################
#creating compiled_cohorts_3.csv
##############################################################
#############################################################
library(tidyverse)
library(janitor)

#creating compiled_cohorts_3
#trying to create a yes/no of had tumor or not. 
#select.csv is a list of any variables that could help tell me if there was a tumor present. 

#first gather information.
#extract list of variable names from select.csv, to use for selection from main dataset. 
cols<-
  read_csv("select.csv", show_col_types = FALSE)%>%
  clean_names()%>%
  filter(x1 == "1")%>%
  select(x3)%>%
  pull()



#use variable names for below transposition of dataframe. 

tt<-
  read_csv("compiled_cohorts2.csv", show_col_types = FALSE)%>%
  select((cols))%>%
  mutate(had_tumor = 0)%>%
  relocate(had_tumor, .before = tumor_id)%>%
  as.data.frame()

#transpose and keep columns as first row. Will use to check how to use each variable.
setNames(data.frame(t(tt[,-1])), tt[,1])%>%
  
  write.csv("tumor_test.csv")


#used output of above to look through data variables. 
#made notes on conditions to look for for tumor determination. 


#looking to create a variable for whether mice had any tumors, at any time. 
#reading in cleaned cohorts data, selecting for any variables that can be informative. 

read_csv("compiled_cohorts2.csv", show_col_types = FALSE)%>%
  
  #create had_tumor col.
  mutate(had_tumor = NA)%>%
  relocate(had_tumor)%>%
  
  #some low-hanging fruit. 
  mutate(had_tumor = case_when(is.na(tumor_id)==FALSE|
                                 is.na(tumor)==FALSE|
                                 is.na(tumor_noticed)==FALSE ~ "1"))%>%
  
  #case_when is having issues with complex self-referential when nested in one
  #mutate command. Separate out if there are multiple complex and/or statements. 
  
  #first "complex" conditional. Entry can exist but may also say that there is no tumor. 
  #sorts for any entries that contain values, but NOT specific phrases denoting no tumors. 
  mutate(had_tumor = case_when(is.na(tumor_locations)==FALSE&
                                 grepl("Pre-tumorigenic", tumor_locations)==FALSE&
                                 grepl("no obs", tumor_locations)==FALSE ~ "1",
                               TRUE ~ as.character(had_tumor)))%>%
  
  #make new column that indicates if mouse was pretumorigenic. 
  #may be useful later, if I want to combine both tumor+pretumorigenic groups. 
  mutate(pretumorigenic = if_else(grepl("Pre-tumorigenic", tumor_locations), 1, NA_real_))%>%
  
  
  #####have to use as.character(x) in order for the false condition to ignore NAs
  #mark 0 for tumor if was pretumorigenic. 
  #leave remaining alone. 
  mutate(had_tumor = case_when(pretumorigenic=="1" ~ "0", 
                               TRUE ~ as.character(had_tumor)))%>%
  
  mutate(had_tumor = case_when(notes == "No tumor. Sac'd for end date." ~ "0",
                               TRUE ~ as.character(had_tumor)))%>%
  
  
  mutate(had_tumor = case_when(tumor_locations == "no obs" ~ "0",
                               TRUE ~ as.character(had_tumor)))%>%
  
  mutate(had_tumor = case_when(notes == "no evidence of tumor" ~ "0",
                               TRUE ~ as.character(had_tumor)))%>%
  
  mutate(had_tumor = case_when(notes == "no evidence of tumor"|
                                 notes == "slightly hunched back but no evidence of tumor"|
                                 notes == "ruffle fur noticed, but no obs. Tumor. found dead"|
                                 notes == "Lost weight, extreme hunchback, head twitching, slow moving. Soft skull and brain. No obs. Tumor, no other CNS tumor found"|
                                 notes == "Lost weight, mild hunchback, slow moving. Soft skull and brain. No obs. Tumor, no other CNS tumor found. Enlarged optic nerve" ~ "0",
                               
                               TRUE ~ as.character(had_tumor)))%>%
  
  mutate(had_tumor = case_when(necropsy_behavior_comments == "end of experiment, looked normal" ~ "0",
                               TRUE ~ as.character(had_tumor)))%>%
  
  mutate(had_tumor = case_when(proof_of_mass == "yes" ~ "1",
                               proof_of_mass == "no" ~"0",
                               TRUE ~ as.character(had_tumor)))%>%
  
  
  mutate(had_tumor = case_when(is.na(tumor_noticed) == FALSE ~ "1",
                               TRUE ~ as.character(had_tumor)))%>%
  
  
  #final product:
  
  write_csv("compiled_cohorts3.csv")