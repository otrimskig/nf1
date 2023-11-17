library(tidyverse)
library(survminer)
library(survival)
library(ggpubr)
library(gridExtra)

#reading in current dataset. 
readRDS("ds/nf1_cohorts.3.dates.rds")->cohort_survival


df1<-cohort_survival%>%
  
  #keep only necessary vars
  select(exclude, resultant_geno, aod, exp_end_date)%>%
  
  mutate(event = if_else(is.na(exp_end_date),1,0))%>%
  select(-exp_end_date)%>%
  
  #making a convenient variable to easily compare nf1 vs. non-nf1s. 
  mutate(nf1=substr(resultant_geno, 1, 6))%>%
  mutate(geno_bg=substr(resultant_geno, 8, nchar(resultant_geno)))%>%
  
  mutate(aod = ifelse(aod>148,150,aod))%>%
  
  
  #most conservative exclusion criteria
  filter(is.na(exclude))


#make empty list
plot_list <- list() 






#creating survfit object for all cohorts. 
survfit(Surv(time = aod, 
             #did die?
             event = event)~
              #grouped by
              resultant_geno, 
              data = df1)->cohort_surv



#finally, plotting all cohorts.

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
  )->pl

plot_list[[1]]<-pl$plot



#export plot - add export to code. 




#####################################
####################################
#plotting all groups vs. nf1 for each bg
#use df1.

#vector of all bg_genos to be analysed. 
bg_genotypes<-df1%>%
  group_by(geno_bg)%>%
  slice(1)%>%
  pull(geno_bg)



df1%>%
  filter(geno_bg==bg_genotypes[[1]])%>%
  group_by(resultant_geno)%>%
  slice(1)%>%
  pull(resultant_geno)



for(i in 1:length(bg_genotypes)){
  
  dft<-df1%>%
    filter(geno_bg==bg_genotypes[[i]])
 
  
  dfleg<-dft%>%group_by(resultant_geno)%>%
    slice(1)%>%
    pull(resultant_geno)
  
  
  sf<-survfit(Surv(time = aod, 
               #did die?
               event = event)~
            #grouped by
            nf1, 
          data = dft)

  ggs<- ggsurvplot(sf, xlim = c(0, 150),
               ylim = c(0, 1.02),
               size =1.5,
               alpha = .9,
               break.x.by = 25,
               break.y.by = .25,
               axes.offset = FALSE,
               palette= c("black", "grey"),
               legend = c("bottom", nrow=2),
               ggtheme = theme_classic(),
               xlab = "Time Post Injection (Days)",
               legend.title = "cohort",
               legend.lab = dfleg
    )
                           
  
  #have to specify $plot within ggsurvplot object to be understood
  #as a ggplot object (aka "grob")
  plot_list[[1+i]]<-ggs$plot
}


lay <- rbind(c(1,1,1,2,3),
             c(1,1,1,4,5))

grid.arrange(grobs=plot_list[1:3],layout_matrix=lay)











