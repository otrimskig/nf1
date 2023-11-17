#written in context of cohorts3.Rproj

library(tidyverse)
library(ggpubr)
library(gridExtra)
library(ggkm)



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
  
  #most conservative exclusion criteria
  filter(is.na(exclude))%>%
  
  mutate(aod=if_else(event==0, 150,aod))





#make empty list
plot_list <- list() 




#1st element, main plot with all curves. 
plot_list[[1]]<-ggplot(df1)+
  geom_km(aes(time = aod, 
              color=resultant_geno,
              status = event),
          
          
          linewidth=2)+
  
  
  #change alpha of lines. geom_km does not recocnize alpha changes,
  #(even though documentation says it does). 
  
  #note that the hue_pal call refers to the default r palette. 
  scale_color_manual(values = alpha(c(hue_pal()(6)),
                                    0.7))+
  #geom_kmticks(shape = "|")+
  #geom_kmband(alpha=.2, conf.int=.95)+
  
  xlim(0,150)+
  
  theme_classic()+
  theme(legend.position = c(0.2, 0.2))


plot_list[[1]]




#plotting all groups vs. nf1 for each bg
#use df1.

#vector of all bg_genos to be analysed. 
bg_genotypes<-df1%>%
  group_by(geno_bg)%>%
  slice(1)%>%
  pull(geno_bg)



for(i in 1:length(bg_genotypes)){
  
  dft<-df1%>%
    filter(geno_bg==bg_genotypes[[i]])
  

  plot_list[[1+i]]<-ggplot(dft, aes(time = aod, status = event, color=resultant_geno)) + 
    geom_km()+
    scale_color_manual(values=c("red", "grey"))+
    #geom_kmticks(shape = "|")+
    #geom_kmband(alpha=.1, conf.int=.95)+
    xlim(0,150)+
    theme_classic()+
    
    theme(legend.position="bottom", legend.title= element_blank())+
    guides(color = guide_legend(nrow = 2))  
  

}


lay <- rbind(c(1,1,1,NA),
             c(1,1,1,2),
             c(1,1,1,3))

grid.arrange(grobs=plot_list[1:3],layout_matrix=lay)

