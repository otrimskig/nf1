####################################
#2023.01.01
#getting information from updated cohort dataset
#into survival curve and analysis. 

library(tidyverse)
library(survminer)
library(survival)



#reading in previously set color data. 
read_csv("../mouse cohorts/cohort_legend.csv")->cohort_legend1

#reading in current dataset. 
readRDS("ds/cohort3_survival.rds")->cohort_survival






######################################################
#color key
#updating color key to match to "genes_ko" to "cohort" notation. 
######################################################

#getting list of cohorts from main dataset, with n's for each. 
cohort_survival%>%
  count(cohort)%>%
  
  #removing NA cohorts 
  filter(!is.na(cohort)
         
         #saving cohort names and n's. 
          )->cohort_stats



#getting match key for cohort <-> gene_ko. 
#need this to match "genes_ko" to "cohorts". Color key data is linked to genes_ko. 
#groups are also ORDERED alphabetically by genes_ko, which will be important later for graph. 
read_csv("ref/cohort_key.csv")->join_key


#joining cohort names with previously created color scheme using join_key. 
cohort_stats%>%
  left_join(join_key)%>%
  left_join(cohort_legend1, by = "genes_ko")%>%
  
#creating cohort_legend rds file, to which I can later add summary data,
#and use for color-matching consistency.
#dataset contains: genes_ko, cohorts, n's, color, hex for that color.   
  saveRDS("ds/cohort_legend.rds")


#cleaning up
#removing unnecessary objects from environment that won't be needed any longer. 
rm("cohort_legend1", "cohort_stats", "join_key")








##################################################################



#reading in necessary data. #legend
readRDS("ds/cohort_legend.rds")->cohort_legend

#read in already above, just a reminder:
#main dataset:
#       cohort_survival

#previously plotted data:
read_csv("../mouse cohorts/survival_compiled.csv")->cohort_prev


#checking for mismatches in current vs. old data. 
#since there were many corrections, I know there will be many mismatches.
#just checking for any obvious ones. 
# 
cohort_prev%>%
  #selecting and quick clean-up of imported data
  select(mouse_num, age_death_capped)%>%
  mutate(mouse_num = as.character(mouse_num))%>%

  #joining dfs and removing excluded data.
  full_join(cohort_survival, by = "mouse_num")%>%
  filter(exclude != "1"| is.na(exclude))%>%

  #calculating age of death as "aod"
  mutate(aod = death_date-dob)%>%
  mutate(aod2 = as.numeric(aod))%>%

  #selecting only necessary columns...
  select(mouse_num, aod2, age_death_capped)%>%

  #filtering for where they're not equal.
  filter(aod2 != age_death_capped)%>%

  view()

#overall not super-informative...
#there are many differences because there were many corrections to original df. 
#however, there are mice #s included in the updated data that were not present in original df. 
#so there will still be many mice that will need to be age-corrected
#which were sacked at d147-148 accidentally that should have been end-date. 

rm(cohort_prev)


#continuing with analysis. 


#############################################################################
#############################################################################




#taking main dataset into form friendly for survival curve. 


cohort_survival%>%
  #getting age_death
  mutate(age_death = as.numeric(death_date-injection_date))%>%
  
  #removing excluded mice from data. 
  filter(exclude != "1" | is.na(exclude))%>%
  #and mice without a cohort. 
  filter(!is.na(cohort))%>%
  
  

  #adding in genes_ko info and connecting to cohort. 
  left_join(cohort_legend%>%select(genes_ko, cohort), by = "cohort")%>%
  
  #selecting info necessary for plotting. 
  select(cohort, mouse_num, age_death, genes_ko)%>%
  
  
  #some very basic data edits to account for mice sacked on the wrong day.
  #will go through in more detail to see if can have a better idea of which 
  #this actually happened for. 
  
  mutate(age_death2 = if_else(age_death>=147, 200, age_death))%>%
  
  #adding "event" column. 
  mutate(event = case_when(age_death2 >150~0,
                           age_death2 <=150~1))%>%
  
  
  #sorting by genes_ko and saving object. 
  arrange(genes_ko
          
          )->cohort_survival2
  


saveRDS(cohort_survival2, "ds/cohort3_survival2.rds")


#getting palette from legend. 

  #palette info
cohort_legend%>%
  arrange(genes_ko)%>%
  pull(hex
       
       )->cohort_palette
  
#getting legend order from legend. 
cohort_legend%>%
  arrange(genes_ko)%>%
  pull(cohort
       
       )->leg_order



#creating survfit object. 
survfit(Surv(time = age_death2, event = event)~genes_ko, data = cohort_survival2)->cohort_surv



#finally, plotting:
#overall survival

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
  )->plot

#export plot - add export to code. 
plot

##########################################################################
##########################################################################

#statistics:
sink("out/output.txt")
cohort_surv%>%
  summary()


survdiff(Surv(time = age_death2, event = event) ~ genes_ko, 
         data = cohort_survival2)

pairwise_survdiff(Surv(time = age_death2, event = event) ~ genes_ko, 
         data = cohort_survival2, p.adjust.method = "BH")->pairwise

pairwise_survdiff(Surv(time = age_death2, event = event) ~ genes_ko, 
        data = cohort_survival2)



sink(NULL)


pairwise_survdiff(Surv(time = age_death2, event = event) ~ genes_ko, 
                  data = cohort_survival2, p.adjust.method = "BH")->pairwise


class(pairwise)

list(pairwise)%>%
  as_tibble(
    validate = NULL,
    .rows = NULL,
    .name_repair = c("check_unique", "unique", "universal", "minimal")
  )

list(pairwise)%>%
  as_tibble(.name_repair= c("universal"))


lapply(list(pairwise), list)


read.delim("out/output.txt")%>%
  as_tibble()%>%
  view()


pairwise

library(fmtr)
library(libr)



pairwise[["p.value"]]%>%
  as_tibble()%>%
  view()


pairwise[["method"]]
