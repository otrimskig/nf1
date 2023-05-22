
#######################################
library(tidyverse)
library(janitor)
library(survminer)
library(survival)


#creating common color palette and legend order (if needed). 
#palette taken from locuszoom of ggsci package using commented code. 
#Order modified and stuck to each specific group. 
# library(paletteer)
# library(ggsci)
# paletteer_d("ggsci::default_locuszoom", n=6)



#obj will be used in survival plots.
custom_lz<-
  c("#9632B8FF",  "#357EBDFF", "#5CB85CFF",  "#EEA236FF", "#D43F3AFF", "#46B8DAFF")



#make csv file with colors and order of gene groups.
read_csv("survival_compiled.csv")%>%
  count(genes_ko)%>%
  select(1)%>%
  #add color hexes and names. 
  mutate(hex = custom_lz)%>%
  mutate(color_name = c("purple", "dk blue", "green", "yellow", "red", "light blue"))%>%
  #write legend
  write_csv("cohort_legend.csv")




#objects for convenient use in graphs below.
cohorts_color<-
  read_csv("cohort_legend.csv")%>%
  pull(hex)

#objects for convenient use in graphs below.
leg_order<-
  read_csv("cohort_legend.csv")%>%
  pull(genes_ko)




##################################################################################
#PLOTS
##################################################################################
#"heatmap" of genes ko's for easy visualization of groups. 

read_csv("compiled_cohorts3.csv")%>%
  group_by(genes_ko)%>%
  slice(1)%>%
  select(genes_ko, nf1_ko, pten_ko, ink_ko, atrx_ko)


#data
read_csv("kos.csv", show_col_types = FALSE)%>%
  drop_na()%>%
  row_to_names(1)%>%clean_names()%>%
  bind_cols(read_csv("compiled_cohorts3.csv", show_col_types = FALSE)%>%
              group_by(genes_ko)%>%
              slice(1)%>%
              select(genes_ko, nf1_ko, pten_ko, ink_ko, atrx_ko))%>%
  select(1:5)%>%
  relocate(genes_ko)%>%
  mutate_at(vars(2:5), as_factor)%>%
  pivot_longer(2:5, names_to = "gene", values_to = "ko")%>%
  
  
 
#plotting
ggplot(aes(x = factor(gene, levels = c("nf1", "pten", "ink", "atrx")), 
             y=factor(genes_ko, levels = rev(read_csv("leg_order.csv")%>%pull())), 
             fill = ko))+
  geom_point(shape = 22, size = 8)+
  facet_grid(.~factor(gene, levels = c("nf1", "pten", "ink", "atrx")), scales = "free")+
  theme_minimal()+
  theme(axis.title.x=element_blank(),
        axis.text.x.bottom = element_blank(),
        legend.position = "none",
        panel.spacing = unit(-2, "lines"),
        axis.line = element_blank(),
        panel.grid = element_blank())+
  scale_fill_manual(values = c("#00FF81", "#ffffff"))






#######################################################################
#overall survival

  #get data, add event column with 150 cap. 
d1<-
  read_csv("survival_compiled.csv")%>%
  select(age_death_capped, genes_ko)%>%
  mutate(event = case_when(age_death_capped >149~0,
                           age_death_capped <=149~1))




survfit(Surv(time = age_death_capped, event = event)~genes_ko, data = d1)%>%
  ggsurvplot(xlim = c(0, 150),
             ylim = c(0, 1.02),
             size =3,
             alpha = .9,
             break.x.by = 25,
             break.y.by = .25,
             
             axes.offset = FALSE,
             legend = "right",
             ggtheme = theme_classic(),
             palette = custom_lz,
             xlab = "Time Post Injection (Days)",
             legend.title = "Genes KO",
             legend.lab = leg_order
  )

survfit(Surv(time = age_death_capped, event = event)~genes_ko, data = d1)%>%
  summary()



#######################################################################
#tumor survival


#get data, add event column with 150 cap. 
d3<-
  read_csv("survival_compiled.csv")%>%
  select(age_death_capped, genes_ko, had_tumor)%>%
  filter(had_tumor == TRUE)%>%
  select(-had_tumor)%>%
  mutate(event = case_when(age_death_capped >149~0,
                           age_death_capped <=149~1))


d3_labs<-
  d3%>%distinct(genes_ko)%>%arrange_all()%>%pull()


survfit(Surv(time = age_death_capped, event = event)~genes_ko, data = d3)%>%
  ggsurvplot(xlim = c(0, 150),
             ylim = c(0, 1.02),
             size =3,
             alpha = .9,
             break.x.by = 25,
             break.y.by = .25,
             
             axes.offset = FALSE,
             legend = "right",
             ggtheme = theme_classic(),
             palette = custom_lz,
             xlab = "Time Post Injection (Days)",
             legend.title = "Genes KO",
             legend.lab = d3_labs
  )




#############################################################
#percentage of mice in each cohort that presented with tumors 


#ensures that graph is ordered in terms of percentage of tumors. 
#genes_order_tumor_perc<-
 
#d4<-
  read_csv("survival_compiled.csv")%>%
  group_by(genes_ko)%>%
  count(had_tumor)%>%
  drop_na()%>%
  mutate(per = n/sum(n)*100)%>%
  ungroup()%>%
  group_by(had_tumor)%>%
  arrange(per)%>%
  filter(had_tumor == FALSE)%>%
  arrange(per)%>%
  ungroup()%>%
  select(-had_tumor)

#%>%
  #pull(genes_ko)



#plotting 
#filter from dataset, perform percentage calc. 
read_csv("survival_compiled.csv")%>%
  group_by(genes_ko)%>%
  count(had_tumor)%>%
  drop_na()%>%
  mutate(per = n/sum(n)*100)%>%
  filter(had_tumor == FALSE)%>%
  
  #plot
  ggplot(aes((100-per), factor(genes_ko, level = rev(genes_order_tumor_perc)), color = genes_ko))+
  geom_segment(aes(x = -0, 
                   xend = 100-per, 
                   y = factor(genes_ko, level = rev(genes_order_tumor_perc)), 
                   yend = factor(genes_ko, level = rev(genes_order_tumor_perc))), 
               linewidth = 13)+
  scale_color_manual(values = cohorts_color)+
  geom_segment(aes(x = 100-per,
                   xend = 100,
                   y = factor(genes_ko, level = rev(genes_order_tumor_perc)),
                   yend = factor(genes_ko, level = rev(genes_order_tumor_perc))),
               color = "grey", linewidth = 1, alpha = 1)+
 
  theme_classic()+
  theme(panel.grid.major.x = element_blank(),
     panel.border = element_blank(),
    axis.ticks.x = element_blank(),
     legend.position = "none")+
  scale_x_continuous(expand=(c(0,1)))



#######################end##########################################

