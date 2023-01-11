####################################
#2023.01.01
#getting information from updated cohort dataset
#into survival curve and analysis. 

library(tidyverse)
library(survminer)
library(survival)



#reading in previously set color data. 
read_csv("../mouse cohorts/cohort_legend.csv")->cohort_legend

#reading in current dataset. 
readRDS("cohort3_survival.rds")->cohort_survival


######################################################
#color key
#updating color key to match to "genes_ko" to "cohort" notation. 
######################################################
#getting list of cohorts in dataset, with n for each. 
cohort_survival%>%
  count(cohort)%>%
  #removing NA cohorts 
  filter(!is.na(cohort))->cohort_stats

#getting match key for cohort <-> gene_ko. 
read_csv("ref/cohort_key.csv")->join_key

#adding cohort names to legend key. 
cohort_stats%>%
  

  
  left_join(join_key)%>%
  
  
  left_join(cohort_legend, by = "genes_ko")%>%
  
  
#creating cohort_info rds file, to which I can later add summary data,
#and use for color-matching consistency.
  saveRDS("cohort_info.rds")


#cleaning up
rm("cohort_legend", "cohort_stats", "join_key")

##################################################################

#continuing with analysis. 


readRDS("cohort_info.rds")->cohort_info
#read in already above, just a reminder:
#readRDS("cohort3_survival.rds")->cohort_survival
read_csv("../mouse cohorts/survival_compiled.csv")->cohort_prev


##prior to actually plotting, need to ensure data matches previously corrected data-especially for age of mice. 
cohort_prev%>%
  select(mouse_num, age_death_capped)%>%
  mutate(mouse_num = as.character(mouse_num))%>%
  full_join(cohort_survival, by = "mouse_num")%>%
  filter(exclude != "1"| is.na(exclude))%>%
  
  mutate(aod = death_date-dob)%>%
  mutate(aod2 = as.numeric(aod))%>%
  select(mouse_num, aod2, age_death_capped)%>%
  
  filter(aod2 != age_death_capped)%>%
  
  view()
#overall not super-informative...continuing with analysis. 




#############################################################################

#taking dataset into form friendly for survival curve. 
cohort_survival%>%
  #getting age_death
  mutate(age_death = as.numeric(death_date-injection_date))%>%
  #removing excluded mice from data. 
  filter(exclude != "1" | is.na(exclude))%>%
  
  mutate(event = case_when(age_death >149~0,
                           age_death <=149~1))%>%
  
  #removing all mice without a cohort. 
  filter(!is.na(cohort))%>%
  left_join(cohort_info%>%select(genes_ko, cohort), by = "cohort")%>%
  arrange(genes_ko)->cohort_survival2




cohort_survival2%>%
  select(cohort, mouse_num, age_death, genes_ko)%>%
  mutate(age_death2 = if_else(age_death>=147, 200, age_death))%>%
  
  
  
  mutate(event = case_when(age_death2 >149~0,
                           age_death2 <=149~1))%>%
  arrange(genes_ko)->cohort_survival3
  

cohort_survival3%>%
  view()










cohort_info%>%
  arrange(genes_ko)%>%
  view()






  #palette info
cohort_info%>%
  
  arrange(genes_ko)%>%
  pull(hex
       
       )->cohort_palette
  
#legend order
cohort_info%>%
  arrange(genes_ko)%>%
  pull(cohort
       
       )->leg_order



leg_order
cohort_palette
#overall survival

cohort_palette%>%
  view()

cohort_info%>%
  view()

cohort_survival2%>%
  view()


survfit(Surv(time = age_death2, event = event)~genes_ko, data = cohort_survival3)->cohort_surv


cohort_surv%>%
  ggsurvplot(xlim = c(0, 150),
             ylim = c(0, 1.02),
             size =3,
             alpha = .9,
             break.x.by = 25,
             break.y.by = .25,
             axes.offset = FALSE,
             palette= cohort_palette,
             legend = "right",
             ggtheme = theme_classic(),
             xlab = "Time Post Injection (Days)",
             legend.title = "cohort",
             legend.lab = leg_order
  )

cohort_surv%>%
  summary()





#statistics:

survdiff(Surv(time = age_death2, event = event) ~ genes_ko, 
         data = cohort_survival3)%>%
  as_tibble()




pairwise_survdiff(Surv(time = age_death2, event = event) ~ genes_ko, 
         data = cohort_survival3, p.adjust.method = "BH")

pairwise_survdiff(Surv(time = age_death2, event = event) ~ genes_ko, 
                  data = cohort_survival3)%>%
  view()

