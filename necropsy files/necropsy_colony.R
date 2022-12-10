##################
library(tidyverse)
library(janitor)
library(stringr)
library(lubridate)





read_csv("colony_necropsy.csv")%>%
  #change todays_date to proper format.
  mutate(todays_date = as.Date(as.numeric(todays_date), origin = "1899-12-30"))%>%
  
  #get mouse_num in proper format and move it to front.
  mutate(mouse_num = substr(mouse_id, 1, 5))%>%
  relocate(mouse_num)%>%
  select(-mouse_id)%>%
  
  #looks for columns containing only NAs and removes them.
  select(where(~!all(is.na(.x))))%>%
  
  #combine disposition columns.
  unite(disposition, c(disposition, disposition_2, disposition_3, disposition_4), na.rm = TRUE, sep=",")%>%
  
  #clean up sex column.
  mutate(sex = case_when(grepl("f", sex, ignore.case = TRUE)~"F",
                         TRUE~"M"))%>%
  
  #birth date to proper format.
  mutate(bd = substr(bd, 1,5))%>%
  mutate(dob = as.Date(as.numeric(bd), origin = "1899-12-30"))%>%
  select(-bd)%>%
  
  #unnecessary, can be calced from sac date and birth date.
  select(-age_at_sac, -age_at_tumor_onset)%>%
  
  #sac date to proper format.
  mutate(sac_date = as.Date(as.numeric(sac_date), origin = "1899-12-30"))%>%
  
  #injection_date to proper format
  mutate(injection_date = as.Date(as.numeric(injection_date), origin = "1899-12-30"))%>%
  unite(additonal_comments, c(additional_comments, x_2, x_3), na.rm = TRUE, sep=",")%>%
  
  mutate(virus_1 = trimws(gsub("\\(.*", "", virus_1)))%>%
  rename(virus = virus_1)%>%

rename(additional_comments = additonal_comments)%>%

  #make backup of tissue collected column.
  mutate(t=tissue_collected)%>%
  #making tissue sample columns.
  
mutate(tissue_br = "1")%>%
  mutate(tissue_collected = gsub("^Brain", "", tissue_collected, ignore.case = TRUE))%>%
  mutate(tissue_collected = gsub("Brain", "", tissue_collected, ignore.case = FALSE))%>%
  mutate(tissue_collected = gsub("^B", "", tissue_collected))%>%

  mutate(tissue_optic_n = case_when(grepl("optic", tissue_collected, ignore.case = TRUE)~"1",
                                    TRUE~as.character(NA_real_)))%>%
  
  mutate(tissue_collected = gsub("optic nerve", "", tissue_collected, ignore.case = TRUE))%>%

 
   mutate(tissue_lung = case_when(grepl("lung", tissue_collected, ignore.case = TRUE)~"1",
                                 TRUE~as.character(NA_real_)))%>%
  mutate(tissue_collected = gsub("lung", "", tissue_collected, ignore.case = TRUE))%>%
 
   mutate(tissue_liver = case_when(grepl("liver", tissue_collected, ignore.case = TRUE)~"1",
                                 TRUE~as.character(NA_real_)))%>%
  mutate(tissue_collected = gsub("liver", "", tissue_collected, ignore.case = TRUE))%>%
  mutate(tissue_collected = gsub("^\\,\\ ", "", tissue_collected, ignore.case = TRUE))%>%
  mutate(tissue_collected = trimws(gsub("^\\,\\ ", "", tissue_collected, ignore.case = TRUE)))%>%
  mutate(tissue_collected = gsub("^s\\,", "", tissue_collected, ignore.case = TRUE))%>%
  mutate(tissue_collected = trimws(gsub("^\\,\\ ", "", tissue_collected, ignore.case = TRUE)))%>%
  mutate(tissue_collected = trimws(gsub("^\\,\\ ", "", tissue_collected, ignore.case = TRUE)))%>%
  mutate(tissue_collected = gsub("^s$", "", tissue_collected, ignore.case = TRUE))%>%
  mutate(tissue_collected = gsub("\\, \\, s\\,", "", tissue_collected, ignore.case = TRUE))%>%
  
  
  #select(disposition, evidence_of_tumor, mouse_num)%>%
  
  mutate(tumor_evidence = NA)%>%
  mutate(tumor_evidence = case_when(evidence_of_tumor %in% c("No","no", "NO", "N")~"0",
                                    evidence_of_tumor %in% c("Y", "Yes", "yes")~"1",
                                    TRUE~as.character(tumor_evidence)))%>%

  
  relocate(mouse_num)%>%
  rename(nec_date = todays_date)%>%
  write_csv("colony_necropsy2.csv")



  read_csv("colony_necropsy2.csv")%>%
#working only on tumor bool column.
  select(mouse_num,
         disposition,
         evidence_of_tumor,
         date_tumor_reported,
         tissue_collected,
         formalin_fixed,
         parafin_block_number_s,
         additional_comments,
         x_4,
         tumor_evidence)%>%
    
    unite(comments, c(additional_comments, x_4))%>%
    rename(tumor_bool = tumor_evidence)%>%
    relocate(tumor_bool)%>%
    rename(tumor_qual = evidence_of_tumor)%>%
    relocate(tumor_qual, .after = tumor_bool)%>%
    
    mutate(tumor_qual = gsub("^no$", NA,ignore.case = TRUE,
                        gsub("^yes$", NA, ignore.case = TRUE,
                        gsub("^y$", NA, ignore.case = TRUE,
                        gsub("^n$", NA, ignore.case = TRUE,
                             tumor_qual, )))))%>%
    mutate(tumor_bool = case_when(grepl("^maybe", ignore.case = TRUE, tumor_qual)~"2",
                                  grepl("^potential", ignore.case = TRUE, tumor_qual)~"2",
                                  grepl("^Y ", ignore.case = TRUE, tumor_qual)~"1",
                                  grepl("^glioma", ignore.case = TRUE, tumor_qual)~"1",
                                  grepl("^yes", ignore.case = TRUE, tumor_qual)~"1",
                                  grepl("^tumor", ignore.case = TRUE, tumor_qual)~"1",
                                  grepl("large head tumor", ignore.case = TRUE, tumor_qual)~"1",
                                  grepl("large tumor", ignore.case = TRUE, tumor_qual)~"1",
                                  grepl("tumor", ignore.case = TRUE, tumor_qual)~"3",
                                  TRUE~as.character(tumor_bool)))%>%
  filter(is.na(tumor_qual)==FALSE)%>%
  #filter(is.na(tumor_bool))%>%
  write_csv("man.csv")  
  
  
  #manual edit of cases where "tumor" was included in description,
  #but unclear whether that was a yes or a no.
  #rejoining to dataset above.
  left_join(read_csv("man2.csv"), read_csv("man.csv"))%>%
    select(-tumor_bool)%>%
    rename(tumor_bool = tumor_bool_man)%>%
     write_csv("man3a.csv")
  
  #manual edit of disposition and date_tumor_reported, joining back to dataset.
  left_join(read_csv("man3b.csv"), read_csv("man3a.csv"))%>%
    select(-disposition,-date_tumor_reported)%>%
    rename(disposition = disposition_man, date_tumor_reported = date_tumor_reported_man)%>%
    
    
  #fixing non-tumor glioma that were mistakenly attributed as tumors in previous transformations.
    mutate(tumor_bool = case_when(grepl("^glioma$", ignore.case = TRUE, tumor_qual)~NA_character_,
                           grepl("^glioma\\, no tumor", ignore.case = TRUE, tumor_qual)~NA_character_,
                           TRUE~as.character(tumor_bool)))%>%
    
    
    
    
  write_csv("man4.csv")  
    
   
  
  
   #getting everything back together again. 
    
    read_csv("colony_necropsy2.csv")%>%
   
    unite(comments, c(additional_comments, x_4))%>%
    rename(tumor_bool = tumor_evidence)%>%
    relocate(tumor_bool)%>%
    rename(tumor_qual = evidence_of_tumor)%>%
    relocate(tumor_qual, .after = tumor_bool)%>%
    
    mutate(tumor_qual = gsub("^no$", NA,ignore.case = TRUE,
                             gsub("^yes$", NA, ignore.case = TRUE,
                                  gsub("^y$", NA, ignore.case = TRUE,
                                       gsub("^n$", NA, ignore.case = TRUE,
                                            tumor_qual, )))))%>%
    mutate(tumor_bool = case_when(grepl("^maybe", ignore.case = TRUE, tumor_qual)~"2",
                                  grepl("^potential", ignore.case = TRUE, tumor_qual)~"2",
                                  grepl("^Y ", ignore.case = TRUE, tumor_qual)~"1",
                                  grepl("^glioma", ignore.case = TRUE, tumor_qual)~"1",
                                  grepl("^yes", ignore.case = TRUE, tumor_qual)~"1",
                                  grepl("^tumor", ignore.case = TRUE, tumor_qual)~"1",
                                  grepl("large head tumor", ignore.case = TRUE, tumor_qual)~"1",
                                  grepl("large tumor", ignore.case = TRUE, tumor_qual)~"1",
                                  grepl("tumor", ignore.case = TRUE, tumor_qual)~"3",
                                  TRUE~as.character(tumor_bool)))%>%
    
    
    
    mutate(date_tumor_reported2 = date_tumor_reported)%>%
    mutate(date_tumor_reported = case_when(nchar(date_tumor_reported)>5~NA_character_,
                                           grepl("N/A", date_tumor_reported, ignore.case = TRUE)~NA_character_,
                                           TRUE~as.character(date_tumor_reported)))%>%
    mutate(date_tumor_reported = as.Date(as.numeric(date_tumor_reported), origin = "1899-12-30"))%>%
    mutate(date_tumor_reported2 = case_when(nchar(date_tumor_reported2)<=5~NA_character_,
                                            TRUE~as.character(date_tumor_reported2)))%>%
   
      
    write_csv("colony_necropsy3.csv")  
      
      

# 
# 
#       view()
# 
#        mutate(date_tumor_reported2 = mdy(date_tumor_reported2))%>%
# 
# 
#     mutate(date_tumor_reported = if_else(is.na(date_tumor_reported),
#                                                 date_tumor_reported2,
#                                                 date_tumor_reported))%>%
#     select(-date_tumor_reported2)
# 
# 
# write.csv(file="colony_necropsy2_man.csv", na="", row.names = FALSE)
# 
# 
# view()
# 
#
#
#
# #if date_tumor_reported is 5 char, ie easily cleanable, move to tumor_date.
# mutate(tumor_date = ifelse(nchar(date_tumor_reported)==5, date_tumor_reported, NA))%>%
#   mutate(tumor_date =as.Date(as.numeric(tumor_date), origin = "1899-12-30"))%>%
#
#   #separate other entries into tumor_date2 column.
#   mutate(tumor_date2 = ifelse(nchar(date_tumor_reported)>5, date_tumor_reported, NA))%>%
#   select(-date_tumor_reported)%>%
#
#   
#   #get "dates" prior to comments into date2 column.
#   mutate(date2 = gsub("\\ .*", "", tumor_date2))%>%
#   
#   #get "comments" after first space (date) into a new comment column.
#   mutate(comment2 = gsub("^\\S+\\s+", '', tumor_date2))%>%
#   
#   #separate easily cleanable dates into date3 column. 
#   #keep only other formats in date2 column.
#   #clean date3 into dates and merge into tumor_date column.
#   mutate(date3 = ifelse(nchar(date2)==5, date2, NA))%>%
#   mutate(date3 = as.Date(as.numeric(date3), origin = "1899-12-30"))%>%
#   
#   mutate(date2 = ifelse(nchar(date2)==5,NA, date2))%>%
#   mutate(date2 = mdy(date2))%>%
#   #unite all date columns into tumor_date.
#   unite(tumor_date, c(tumor_date, date2, date3), na.rm = TRUE)%>%
#   select(-tumor_date2)%>%
#   unite(evidence_of_tumor, c(evidence_of_tumor, comment2), na.rm = TRUE, sep=",")%>%
#   
#   
#   view()
# 
# 
# 
# 
# 
       
       
