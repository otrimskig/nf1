library(tidyverse)
library(survminer)
library(survival)


cohort_survival<-readRDS("ds/nf1_cohorts.2.dates.rds")%>%
  select(exclude, metadata, mouse_num, resultant_geno, dob, death_date_true, end_date, injection_date)%>%
  filter(is.na(exclude))

cohort_survival2<-cohort_survival%>%
  mutate(age_at_death = as.numeric(death_date_true - injection_date))%>%
  mutate(age_at_death_corrected = if_else(age_at_death>=147, 150, age_at_death))%>%
  mutate(event = if_else(age_at_death_corrected>=148|!is.na(end_date), 0, 1))



#creating survfit object. 
survfit(Surv(time = age_at_death_corrected, event = event)~resultant_geno, data = cohort_survival2)->cohort_surv



#finally, plotting:
#overall survival

cohort_surv%>%
  ggsurvplot(xlim = c(0, 150),
             ylim = c(0, 1.02),
             size =1.5,
             alpha = .9,
             break.x.by = 25,
             break.y.by = .25,
             axes.offset = FALSE,
             #palette= cohort_palette,
             legend = "right",
             ggtheme = theme_classic(),
             xlab = "Time Post Injection (Days)",
             legend.title = "cohort"
             #legend.lab = leg_order
  )->plot

#export plot - add export to code. 
plot
