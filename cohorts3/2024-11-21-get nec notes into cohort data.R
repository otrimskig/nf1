library(tidyverse)
library(dtplyr)

coh<-readRDS("ds/cohorts-2024-01-18.rds")


mouse_nums<-coh%>%select(mouse_num)



nec_full<-readxl::read_excel("C:/Users/u1413890/OneDrive - University of Utah/garrett hl-onedrive/R/rchpc/nec_files/necropsy_files-plus-outside.xlsx")

necf<-nec_full%>%

  mutate(mouse_num=substr(mouse_id, 1,5))%>%

  semi_join(mouse_nums)%>%
  
  select(where(~ !all(is.na(.))))%>%
  
  relocate(mouse_num)%>%
  
  select(-a, -index, -filename, -c("initials":"mouse_id"), -sex, -strain, -virus_1,
         -genotype, -rcas_injection, -zinc_fixed, -injection_route, -age_at_sac, -h_e,
         -injection_date_1, -slide_number_s, -age_at_tumor_onset,
         -gross_visualization_of_lung_nodules)%>%


  mutate(formalin_fixed = if_else(tissue_collected==formalin_fixed, NA, formalin_fixed))%>%
  mutate(formalin_fixed = if_else(tolower(formalin_fixed)=="yes", NA, formalin_fixed))%>%
  mutate(formalin_fixed = if_else(tolower(formalin_fixed)=="full brain", NA, formalin_fixed))%>%
  mutate(formalin_fixed = if_else(tolower(formalin_fixed)=="brain", NA, formalin_fixed))%>%
  
  mutate(tissue_collected = if_else(tolower(tissue_collected)=="b", "Brain", tissue_collected))%>%
  mutate(tissue_collected = if_else(tolower(tissue_collected)=="brain"&!is.na(formalin_fixed), formalin_fixed, tissue_collected))%>%
  
  select(-formalin_fixed)%>%
  
  
  relocate(mouse_num, date_tumor_reported)


necf2<-necf%>%
  
  rename_with(~ paste0("nec_", .), .cols = -c(mouse_num))



coh1<-coh%>%
  left_join(necf2)%>%
  
  mutate(earliest_noted_tumor_date = pmin(observable_tumor_date, nec_date_tumor_reported, na.rm = TRUE) # Handles NA in one column
  )%>%
  
  select(-observable_tumor_date, -nec_date_tumor_reported)%>%
  
  relocate(earliest_noted_tumor_date, .after="exp_end_date")
  



































library(stringr)

coh2 <- coh1 %>%
  mutate(paralysis = NA) %>%
  relocate(paralysis) %>%
  rowwise() %>%
  mutate(paralysis = if_else(any(str_detect(across(everything(), as.character), regex("paral", ignore_case = TRUE))), 1, NA)) %>%
  ungroup()%>%
  
  
  rowwise() %>%
  mutate(breathing_issues = if_else(any(str_detect(across(everything(), as.character), regex("breathing", ignore_case = TRUE))), 1, NA)) %>%
  ungroup()



saveRDS(coh2, "ds/cohorts-2024-11-21.rds")


writexl::write_xlsx(coh2, "ds/cohorts-2024-11-21.xlsx")



