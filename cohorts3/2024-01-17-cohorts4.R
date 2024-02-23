library(tidyverse)
library(survminer)
library(survival)
library(ggpubr)
library(gridExtra)
library(googlesheets4)
library(janitor)
library(openxlsx)


#reading in current dataset. 
readRDS("ds/nf1_cohorts.3.dates.rds")->df1



#authorize user
gs4_auth(email = "gotrimski@gmail.com")

#read input sheet
all_slides<-read_sheet("1Vr8u_WcXR-VIqRJc1MUXVQobB4HBGgnhg8LTLnsYwOQ", 
                                  sheet = "updated_slides")%>%
  
  mutate(across(1:last_col(), as.character))%>%
  
  #then replace all "NULL" with NA. 
  mutate(across(1:last_col(), function(x){na_if(x, "NULL")}))%>%
  
  clean_names()%>%
  relocate(resultant_geno, has_scan, .after="stain")



has_nec<-all_slides%>%
  group_by(mouse_num)%>%
  slice(1)%>%
  select(mouse_num)%>%
  mutate(has_nec=1)



he_scan<-all_slides%>%
  filter(has_scan=="Y")%>%
  #filter(organ=="B")%>%
  filter(stain=="HE")%>%
  group_by(mouse_num)%>%
  slice(1)%>%
  select(mouse_num)%>%
  mutate(he_scan=1)





df2<-df1%>%
  left_join(has_nec)%>%
  left_join(he_scan)%>%
  mutate(event=if_else(is.na(exp_end_date),1,0))%>%
  
  relocate(event, has_nec, he_scan,.after="mouse_num")


saveRDS(df2, "ds/cohorts-2024-01-17.rds")


  

df3<-df2%>%filter(is.na(exclude))




genos<-df3%>%
  group_by(resultant_geno)%>%
  slice(1)%>%
  ungroup()%>%
  select(resultant_geno)%>%
  
  
  
  
  pull(resultant_geno)

  
  
  
  
geno_short<-df3%>%
  group_by(resultant_geno)%>%
  slice(1)%>%
  ungroup()%>%
  select(resultant_geno)%>%
  mutate(resultant_geno=gsub(" ", "", resultant_geno))%>%
  
  
  
 
  pull(resultant_geno)

genos[1]



excel_output<-list("full_cohort"=df2, 
                   "remove excluded"=df3,
                   "only_necropsied"=df3%>%filter(has_nec==1))


for (i in 1:length(genos)){
  fdf<-df3%>%
    filter(resultant_geno==genos[i])
  
  excel_output[[paste0(geno_short[i])]] <- fdf 
  
}


write.xlsx(excel_output, file = "nf1_cohort_data.xlsx", showNA=FALSE)



df3%>%
  filter(has_nec==1)%>%
  view()

