#R input using Data to R sheet
library(googlesheets4)
library(tidyverse)
library(janitor)
library(lubridate)

#most updated cohort ds. 
cohortdf<-readRDS("../cohorts3/ds/nf1_cohorts.3.dates.rds")

colors<-readRDS("cohort_colors.rds")


df1<-cohortdf%>%
  
  filter(is.na(exclude))%>%
  mutate(event=if_else(!is.na(exp_end_date), 0,1))
  

df1%>%
  filter(event==0)%>%
  view()



df2<-df1%>%  
  group_by(resultant_geno, event)%>%
  
  summarise(count=n())%>%
  
  left_join(df1%>%
              group_by(resultant_geno)%>%count())%>%
  
  mutate(perc=count*100/n)


df2%>%
  rename(tumor_penetrance=perc)%>%
  filter(event==1)%>%
  select(-event)%>%
  write_csv("tumor_penetrance.csv")


  
df2%>%
  filter(event==1)%>%
  
  ggplot(aes(x=resultant_geno, y=perc, fill=resultant_geno))+
  geom_col()+
  #scale_y_continuous(labels = scales::percent)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  
  
  labs(y="Tumor Penetrance",
       x="",
      fill="")+
  theme(axis.text.x=element_blank(),
  plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), 
                     "inches"))+
  scale_fill_manual(values =c((colors%>%arrange(resultant_geno))$hex))+
  scale_fill_discrete(labels=c((colors%>%arrange(resultant_geno))$strain_injection))



colors





#authorize user
library(googlesheets4)
gs4_auth(email = "gotrimski@gmail.com")

#read input sheet
scans_info<-read_sheet("1O-YCDJh4WhjggWK5nRXTyJtB7gfmM7xnxBHDv1IEBEY", 
                       sheet = "matches")%>%
  
  mutate(across(1:last_col(), as.character))%>%
  
  #then replace all "NULL" with NA. 
  mutate(across(1:last_col(), function(x){na_if(x, "NULL")}))%>%
  
  clean_names()%>%
  rename(tumor_type=tumor)%>%
  select(1:4)




gs4_auth(email = "gotrimski@gmail.com")

#read input sheet
grade_conv<-read_sheet("1wjy54DfXNYUAsLe5SzpECdlN-PN67GqWvBWkU_hCCc8", 
                       sheet = "Sheet1")%>%
  
  mutate(across(1:last_col(), as.character))%>%
  
  #then replace all "NULL" with NA. 
  mutate(across(1:last_col(), function(x){na_if(x, "NULL")}))%>%
  
  clean_names()




tumors<-scans_info%>%
  left_join(grade_conv)%>%
  left_join(df1%>%
              select(mouse_num, strain_injection, resultant_geno, aod, event), by="mouse_num")%>%
  filter(!is.na(strain_injection))%>%
  relocate(event,.after = "stain")%>%
  select(-stain)%>%
  
  filter(event==1)%>%
  
  mutate(grade=if_else(is.na(all_tumor_type), "low grade", grade))%>%
mutate(loc=if_else(is.na(all_tumor_type), "in brain", loc))


all<-tumors%>%
  ungroup()%>%
  expand(resultant_geno, grade, loc)


tumor_cats<-tumors%>%  
  
  
  
  
  group_by(resultant_geno, grade, loc)%>%
  
  summarise(count=n())%>%
  
  left_join(tumors%>%
              group_by(resultant_geno)%>%count())%>%
  mutate(perc=count*100/n)%>%
  right_join(all)%>%
  mutate(perc=if_else(is.na(perc), 0, perc))



tumor_cats%>%
  group_by(resultant_geno)%>%
  ggplot(aes(x=resultant_geno, y=perc, fill=factor(grade, levels=c("low grade", "mid grade", "high grade"))))+
    geom_col(aes(color=factor(grade, levels=c("low grade", "mid grade", "high grade"))),width=.6, position = position_dodge(.7))+
  
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), 
                           "inches"))+
  labs(fill="", color="", x="", y="%")




tumor_cats%>%
  group_by(resultant_geno)%>%
  ggplot(aes(x=resultant_geno, y=perc, fill=loc))+
  geom_col(aes(color=loc),width=.6, position = position_dodge(.7))+
  
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin = unit(c(1, 1, 1, 1), 
                           "inches"),
        plot.title = element_text(hjust = 0.5))+
  labs(fill="", color="", x="", y="%",
       title="Tumor Locations")






tumors%>%
  group_by(resultant_geno)%>%
  count(all_tumor_type)%>%
  filter(!is.na(all_tumor_type))%>%
  ggplot(., aes(x=resultant_geno, y=n, fill=all_tumor_type))+
  geom_col(position = "fill", color="black")+
  scale_y_continuous(labels = scales::percent)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), 
                           "inches"),
        legend.position = "none")+
  labs(y="",
       x="",
       title="Diversity of Tumor Types")








`tumors


tumor_cats%>%
  write_csv("tumor_cats_counts.csv")

tumors%>%
  write_csv("all_tumors.csv")
