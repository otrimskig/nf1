library(tidyverse)
library(janitor)
library(openxlsx)
library(lubridate)
library(stringr)

#previously cleaned data from rick and riley's cohort datasets. 
df1.0<-readRDS("ds/nf1_cohorts.1.dates.rds")

#getting info on cohorts from genotyping file
#this was a dataset produced previously flattening and cleaning
#all genotyping file info regarding mice in nf1 cohorts. 

geno_df<-readRDS("ds/cohort_genotypes_geno_file.rds")%>%
  semi_join(df1.0, by = "mouse_num")



#getting dates from external source in necropsy files. 
#this dataset is a cleaned and flattened version of all necropsy file
#information regarding mice in the nf1 cohorts. 
#saving changes made to rds file for later use.
read_csv("ds/colony_necropsy4.csv")%>%
  mutate(mouse_num = as.character(mouse_num))%>%
  semi_join(df1.0, by = "mouse_num")%>%
  
  #only interested in using this for dates, for now. 
  #other info can wait for other analyses.
  
  mutate(nec_performed = "1")%>%
  
  #getting into standard formats
  mutate(nec_date = mdy(nec_date))%>%
  mutate(sac_date = mdy(sac_date))%>%
  rename(death_date.N = sac_date)%>%
  mutate(nec_date = if_else(death_date.N+1<=nec_date, NA, 
                            if_else(death_date.N>nec_date, NA, nec_date)))%>%
  saveRDS("ds/colony_necropsy_info.rds")
 

nec_dates<-readRDS("ds/colony_necropsy_info.rds")%>%
  #for now, only interested in these columns.
  select(mouse_num, death_date.N)

df1.1<-df1.0%>%
  #select(mouse_num, dob)%>%
  left_join(geno_df, by = "mouse_num")%>%
  left_join(nec_dates, by = "mouse_num")


colnames(df1.1) <- sub("\\.x$", ".C", colnames(df1.1))
colnames(df1.1) <- sub("\\.y$", ".G", colnames(df1.1))

df1.1b.1<-df1.1%>%
  select(mouse_num, src, exclude, metadata, contains(".C"), contains(".G"), contains(".N"))%>%
  select(sort(names(.)))%>%
  relocate(mouse_num)


df1.1b.2<-df1.1b.1%>%
  mutate(death_date_true = NA_Date_)%>%
  mutate(death_date_true = if_else(death_date.C==death_date.G&death_date.G==death_date.N, death_date.C, NA_Date_))%>%
  mutate(death_date_true = if_else(is.na(death_date.C)&death_date.G==death_date.N, death_date.G,
                                   if_else(is.na(death_date.G)&death_date.N==death_date.C, death_date.N,
                                           if_else(is.na(death_date.N)&death_date.C==death_date.G, death_date.C, death_date_true))))%>%
  mutate(death_date_true = if_else(is.na(death_date_true), death_date.N, death_date_true))%>%
  relocate(mouse_num, death_date_true)%>%
  
  
  mutate(exclude = if_else(is.na(death_date_true)&abs(death_date.G-death_date.C)>1, 2, exclude))%>%
  mutate(metadata = if_else(is.na(death_date_true)&abs(death_date.G-death_date.C)>1, 
                            paste(metadata, "...", "conflicting death dates in cohort vs. genotyping files.", 
                                   death_date.C, "vs.", death_date.G, "used cohort date but can exclude from analysis if desired."), metadata))%>%
  mutate(metadata = sub("^NA ... ", "", metadata))%>%
  
  
  mutate(death_date_true = if_else(is.na(death_date_true), death_date.C, death_date_true))




#bringing data back into main dataset. 
df1.1%>%
  select(-c(death_date, exclude, metadata))%>%
  left_join(df1b.2%>%select(mouse_num, death_date_true, exclude, metadata), by = "mouse_num")


  
