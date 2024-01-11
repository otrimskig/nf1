#written in context of cohorts3.Rproj

library(tidyverse)
library(ggpubr)
library(gridExtra)
library(ggkm)



#reading in current dataset. 
readRDS("ds/nf1_cohorts.3.dates.rds")->cohort_survival


#final form for survival analysis. 
df1<-cohort_survival%>%
  
  #keep only necessary vars
  select(exclude, strain_injection, resultant_geno, aod, exp_end_date)%>%
  
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



colors<-bind_cols(tibble(df1%>%
  select(resultant_geno, strain_injection)%>%
  arrange(resultant_geno)%>%
  group_by(resultant_geno)%>%
  slice(1)),
  
  tibble(hex=hue_pal()(6)))%>%
  arrange(strain_injection)


colors%>%
  saveRDS("cohort_colors.rds")

nf1_palette<-colors$hex






#overall survival: all cohorts. 



#1st element, main plot with all curves. 
plot_list[[1]]<-ggplot(df1)+
  geom_km(aes(time = aod, 
              color=strain_injection,
              status = event),
          
          
          linewidth=2)+
  
  
  #change alpha of lines. geom_km does not recocnize alpha changes,
  #(even though documentation says it does). 
  
  #note that the hue_pal call refers to the default r palette. 
  scale_color_manual(values = alpha(c(colors$hex), 0.8))+
  
  #geom_kmticks(shape = "|")+
  #geom_kmband(alpha=.2, conf.int=.95)+
  
  xlim(0,150)+
  
  theme_classic()+
  #theme(legend.position = c(0.2, 0.2))

labs(title="Overall Survival",
     x = "Days Post Injection",
     y = "% Survival",
     color=""
     
)+
  theme(plot.title = element_text(hjust = 0.5))




plot_list[1]



#next, all plots separated. 



#separate plots for each curve. 

#make vector of strain injections to use in for loop.  
strains<-df1%>%
  group_by(strain_injection)%>%
  slice(1)%>%
  pull(strain_injection)




#for-loop to create all plots. 
for(i in 1:length(strains)){
  
  dft<-df1%>%
    filter(strain_injection==strains[[i]])
  
  
  plot_list[[1+i]]<-dft%>%
    
    ggplot(.)+
    geom_km(aes(time = aod, 
                color=strain_injection,
                status = event),
            
            
            linewidth=2)+
    
    
    #change alpha of lines. geom_km does not recocnize alpha changes,
    #(even though documentation says it does). 
    
    #note that the hue_pal call refers to the default r palette. 
    scale_color_manual(values = alpha(c(nf1_palette[i]),    0.8))+
    
    
    #geom_kmticks(shape = "|")+
    #geom_kmband(alpha=.2, conf.int=.95)+
    
    xlim(0,150)+
    ylim(0,1)+
    
    labs(title="Overall Survival",
         x = "Days Post Injection",
         y = "% Survival"
      
    )+
    
    
    
    theme_classic()+
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5))
  
  
  
  
   
}


lay <- rbind(c(2,3,4,1,1),
             c(5,6,7,1,1))

grid.arrange(grobs=plot_list[1:7],layout_matrix=lay)




plot_list[[1]]

plot_list[[2]]

plot_list[[3]]

plot_list[[4]]

plot_list[[5]]

plot_list[[6]]

plot_list[[7]]



#finally print out all into pdfs. 

for (i in 2:length(plot_list)) {
  pdf(paste0("plot_", i, ".pdf"),
      width=10 + 2, 
      height=10 + 2)
  print(plot_list[[i]]+
          theme(plot.margin = margin(2,2,2,2, "in")))
  dev.off()
}




{
pdf("plot_all.pdf",
      width=20, 
      height=10)
print(plot_list[[1]]+
          theme(plot.margin = margin(2,2,2,2, "in")))
dev.off()

}

for (i in seq_along(plot_list)) {
  filename <- paste0("plot_", i, ".jpg")
  ggsave(filename, plot_list[[i]], width = 8, height = 8, units = "in")
}


ggsave("plot_1.jpg",
       plot_list[[1]], 
       width=16, height=8, units="in")
# 

# 
# 

