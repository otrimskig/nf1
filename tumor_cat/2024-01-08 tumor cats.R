#R input using Data to R sheet
library(googlesheets4)
library(tidyverse)
library(janitor)
library(lubridate)

#most updated cohort ds. 
cohortdf<-readRDS("../cohorts3/ds/nf1_cohorts.3.dates.rds")




#authorize user
gs4_auth(email = "gotrimski@gmail.com")

#read input sheet
scans_info<-read_sheet("1O-YCDJh4WhjggWK5nRXTyJtB7gfmM7xnxBHDv1IEBEY", 
               sheet = "matches")%>%
  
  mutate(across(1:last_col(), as.character))%>%
  
  #then replace all "NULL" with NA. 
  mutate(across(1:last_col(), function(x){na_if(x, "NULL")}))%>%
  
  clean_names()




df3<-scans_info%>%
  select(mouse_num, tumor, size)







#authorize user
gs4_auth(email = "gotrimski@gmail.com")

#read input sheet
unscanned_slide_notes<-read_sheet("1Vr8u_WcXR-VIqRJc1MUXVQobB4HBGgnhg8LTLnsYwOQ", 
                       sheet = "updated_slides")%>%
  
  mutate(across(1:last_col(), as.character))%>%
  
  #then replace all "NULL" with NA. 
  mutate(across(1:last_col(), function(x){na_if(x, "NULL")}))%>%
  
  clean_names()%>%
  filter(is.na(tumor1))%>%
  filter(is.na(has_scan))%>%
  select(-c(annotated_mrxs:send_for_rna))








df1<-unscanned_slide_notes%>%
  filter(organ=="B")%>%
  filter(stain=="HE")%>%
  group_by(mouse_num)%>%
  slice(1)%>%
  
  mutate(tumor="no cat; not scanned")%>%
  
  select(mouse_num, gnotes_2, tumor)




df2<-cohortdf%>%
  
  filter(is.na(exclude))%>%
  
  select(mouse_num, strain_injection, resultant_geno, aod, 
         found_dead)%>%
  left_join(df1)%>%
  left_join(df3, by="mouse_num")%>%
  
  mutate(tumor_cat=if_else(!is.na(tumor.y), tumor.y, tumor.x, NA))%>%
  mutate(tumor_cat=if_else(is.na(tumor_cat), "no necropsy", tumor_cat))%>%
  
  
  select(-c(tumor.x, tumor.y))%>%
  
  rename(tumor_size=size)%>%
  mutate(tumor_size=as.numeric(tumor_size))%>%
  
  group_by(mouse_num)%>%
  
  arrange(desc(tumor_size))%>%
  slice(1)%>%
  ungroup()%>%

  
  mutate(tumor_cat=if_else(is.na(found_dead), tumor_cat, 
                    if_else(found_dead=="1", "fd", tumor_cat)))%>%
  mutate(tumor_cat=if_else(tumor_cat=="ts/dif", "ts/diff", tumor_cat))




#####################
#begin plots




library(ggplot2)

#stacked bar plot by tumor category. 
df2%>%
  mutate(tumor_cat=sub("^\\d\\.", "", tumor_cat))%>%
  
  group_by(resultant_geno)%>%
  
  count(tumor_cat)%>%
  
  
  ggplot(., aes(x=resultant_geno, y=n, fill=tumor_cat))+
  geom_col(position = "fill", color="black")+
  scale_y_continuous(labels = scales::percent)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  



#aod vs. tumor_size
df2%>%
  ggplot(., aes(x=aod, y=tumor_size))+
  geom_point(aes(color=resultant_geno), size=5)+
  theme_classic()



library(ggbeeswarm)
df2%>%
  mutate(tumor_size=if_else(is.na(tumor_size), 0, tumor_size))%>%
  filter(tumor_size!=0)%>%
  
  ggplot(., aes(x=resultant_geno, y=tumor_size))+
    geom_beeswarm(size=3)+
  theme_classic()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  



df2%>%
  mutate(measurable_tumor = if_else(is.na(tumor_size), "0", "1"))%>%
  group_by(resultant_geno)%>%
  
  
  summarize(n=n(),
            meas=sum(measurable_tumor=="1"),
            perc=meas/n)%>%

  ggplot(., aes(x=resultant_geno, y=perc, fill=resultant_geno))+
  geom_col()+
  scale_y_continuous(labels = scales::percent)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

