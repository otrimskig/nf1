library(tidyverse)

dates<-readRDS("ds/nf1_cohorts.2.dates.rds")
obs<-readRDS("ds/nf1_cohorts.1.observations.rds")



#dataset of easily classifiable tumor grades, 
#from previously done observations. 
#can sub this out for better classifications later if nec. 

df1<-dates%>%
  #unifying all variables. 
  left_join(obs, by = "mouse_num")%>%
  #keeping only what I need for this instance. 
  select(exclude, mouse_num, resultant_geno, notes_he_ihc_brain)%>%
  #renaming for convenience. 
  rename(he_obs = notes_he_ihc_brain)%>%
  
  #check number of unique observations. 
  #~55. These can be gone through manually and classified
  #for now doing simple text extraction. 
  #count(he_obs)%>%
  
  #creating a variable for each classificiation, 
  #ned, lgg, and hgg, based on text components of he_obs. 
  mutate(ned = if_else(grepl("no glioma", he_obs), 1,0))%>%
  mutate(lgg = if_else(grepl("low grade", he_obs), 1,0))%>%
  mutate(hgg = if_else(grepl("high grade", he_obs), 1,0))%>%
  
  #checking for conflicts - ie where obs were classfied twice. 
  mutate(conflicts = ned+lgg+hgg)%>%
 
  #filtering for obs that only had one unique classification. 
  #others can be looked at later. 
  filter(conflicts =="1")%>%
  
  
  #pivoting variables into 1 column, to make plotting easier. 
  pivot_longer(cols = ned:hgg, names_to = "grade")%>%
  filter(value==1)%>%
  select(-value, -conflicts)%>%
  
  #removing excluded mice. 
  filter(is.na(exclude))



plot_df<-df1%>%
  group_by(resultant_geno)%>%
  count(grade)%>%
  left_join(df1%>%
              count(resultant_geno)%>%
              rename(total_n=n)
            )%>%
  mutate(perc=n/total_n*100)



library(ggplot2)

ggplot(plot_df, aes(fill=grade, x=resultant_geno, y=perc))+
  geom_bar(position="fill", stat="identity")+
  coord_flip()+
  theme_classic()



