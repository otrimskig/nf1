#written in context of cohorts3.Rproj

library(tidyverse)
library(ggpubr)
library(gridExtra)
library(ggkm)
library(scales)


#reading in current dataset. 
coh1<-readRDS("ds/cohorts-2025-01-07.rds")%>%
  mutate(aod = as.numeric(aod))

#final form for survival analysis. 
df1<-coh1%>%
  
  filter(is.na(exclude_from_hist_reason))%>%
  
  
  #keep only necessary vars
  select(exclude, strain_injection, resultant_geno, 
         
          hist,
         
         
         aod, exp_end_date)%>%
  
  mutate(event = if_else(is.na(exp_end_date),1,0))%>%
  select(-exp_end_date)%>%
  
  #making a convenient variable to easily compare nf1 vs. non-nf1s. 
  mutate(nf1=substr(resultant_geno, 1, 6))%>%
  mutate(geno_bg=substr(resultant_geno, 8, nchar(resultant_geno)))%>%
  
  #most conservative exclusion criteria
  filter(is.na(exclude)|exclude>3)%>%
  
  mutate(aod=if_else(event==0, 150,aod))%>%
  
  mutate(hist_cat=substr(hist, 1,1))%>%
  mutate(hist_grade=substr(hist, 3,4))%>%
  mutate(hist_grade=gsub("\\.", "", hist_grade))%>%
  mutate(hist_grade=gsub("15", "1.5", hist_grade))%>%
  mutate(hist_grade=gsub("^d$", "ned", hist_grade))%>%
  
  
  
  mutate(hist_catgrade=paste0(hist_cat, ".", hist_grade))%>%
  mutate(hist_catgrade=if_else(hist_catgrade=="n.ned", "ned", hist_catgrade))


aspectratio<-.6


#make empty list
plot_list <- list() 
master_plot_list <- list() 
#overall survival: all cohorts. 


###################################################
#1st element, main plot with all curves. 
master_plot_list[[1]]<-ggplot(df1)+
  geom_km(aes(time = aod, 
              color=hist_cat,
              status = event),
          
          
          linewidth=2)+
  
  facet_wrap(vars(hist_grade), ncol = 1)+
  
  
  xlim(0,150)+
  
  theme_classic()+
  #theme(legend.position = c(0.2, 0.2))

labs(title="Overall Survival",
     x = "Days Post Injection",
     y = "% Survival",
     color=""
     
)+
  theme(plot.title = element_text(hjust = 0.5),
        aspect.ratio=aspectratio)




###################################################

master_plot_list[[2]]<-ggplot(df1)+
  geom_km(aes(time = aod, 
              color=hist_grade,
              status = event),
          
          
          linewidth=2)+
  
  facet_wrap(vars(hist_cat), ncol = 1)+
  
  
  xlim(0,150)+
  
  theme_classic()+
  #theme(legend.position = c(0.2, 0.2))
  
  labs(title="Overall Survival",
       x = "Days Post Injection",
       y = "% Survival",
       color=""
       
  )+
  theme(plot.title = element_text(hjust = 0.5),
        aspect.ratio = aspectratio)




master_plot_list[[2]]

###################################################


# Split data into subsets based on 'hist_cat'
data_list <- split(df1, df1$hist_cat)

# Create a list of ggplot objects, one for each subset
plot_list <- lapply(data_list, function(subset_data) {
  ggplot(subset_data) +
    geom_km(aes(time = aod, color = hist, status = event), linewidth = 2) +
    xlim(0, 150) +
    scale_y_continuous(limits = c(0,1))+
    theme_classic() +
    labs(
      title = paste("Overall Survival- Hist Category ", unique(subset_data$hist_cat)),
      x = "Days Post Injection",
      y = "% Survival",
      color = ""
    ) +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "right",
      aspect.ratio = aspectratio
    )
})

# Combine the plots into a grid
combined_plot <- grid.arrange(grobs = plot_list, ncol = 1)

master_plot_list[[3]]<-combined_plot



###################################################


# Split data into subsets based on 'hist_cat'
data_list <- split(df1, df1$hist_grade)

# Create a list of ggplot objects, one for each subset
plot_list <- lapply(data_list, function(subset_data) {
  ggplot(subset_data) +
    geom_km(aes(time = aod, color = hist, status = event), linewidth = 2) +
    xlim(0, 150) +
    scale_y_continuous(limits = c(0,1))+
    theme_classic() +
    labs(
      title = paste("Overall Survival - Hist Grade ", unique(subset_data$hist_grade)),
      x = "Days Post Injection",
      y = "% Survival",
      color = ""
    ) +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "right",
      aspect.ratio = aspectratio
    )
})

# Combine the plots into a grid
combined_plot <- grid.arrange(grobs = plot_list, ncol = 1)

master_plot_list[[4]]<-combined_plot


###################################################


# Split data into subsets based on 'hist_cat'
data_list <- split(df1, df1$hist_grade)

# Create a list of ggplot objects, one for each subset
plot_list <- lapply(data_list, function(subset_data) {
  ggplot(subset_data) +
    geom_km(aes(time = aod, color = hist_cat, status = event), linewidth = 2) +
    xlim(0, 150) +
    scale_y_continuous(limits = c(0,1))+
    theme_classic() +
    labs(
      title = paste("Overall Survival - Hist Grade ", unique(subset_data$hist_grade)),
      x = "Days Post Injection",
      y = "% Survival",
      color = ""
    ) +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "right",
      aspect.ratio = aspectratio
    )
})

# Combine the plots into a grid
combined_plot <- grid.arrange(grobs = plot_list, ncol = 1)

master_plot_list[[5]]<-combined_plot


###################################################
colors<-readRDS("ds/cohort_colors.rds")

# nf1_palette<-colors$hex
#note that the hue_pal call refers to the default r palette. 

color_mapping <- setNames(alpha(colors$hex, 0.65), colors$resultant_geno)
  
  
  
  
# Split data into subsets based on 'hist_cat'
data_list <- split(df1, df1$hist)

# Create a list of ggplot objects, one for each subset
plot_list <- lapply(data_list, function(subset_data) {
  ggplot(subset_data) +
    geom_km(aes(time = aod, color = resultant_geno, status = event), linewidth = 2) +
    xlim(0, 150) +
    scale_color_manual(values = color_mapping) +
    scale_y_continuous(limits = c(0,1))+
    theme_classic() +
    labs(
      title = paste("Overall Survival - Histology Description ", unique(subset_data$hist)),
      x = "Days Post Injection",
      y = "% Survival",
      color = ""
    ) +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "right",
      aspect.ratio = aspectratio
    )
})

# Combine the plots into a grid
names(plot_list) <- NULL

combined_plot <- grid.arrange(grobs = plot_list)


master_plot_list[[6]]<-combined_plot



###################################################
colors<-readRDS("ds/cohort_colors.rds")

# nf1_palette<-colors$hex
#note that the hue_pal call refers to the default r palette. 

color_mapping <- setNames(alpha(colors$hex, 0.65), colors$resultant_geno)




# Split data into subsets based on 'hist_cat'
data_list <- split(df1, df1$hist_catgrade)

# Create a list of ggplot objects, one for each subset
plot_list <- lapply(data_list, function(subset_data) {
  ggplot(subset_data) +
    geom_km(aes(time = aod, color = resultant_geno, status = event), linewidth = 2) +
    xlim(0, 150) +
    scale_color_manual(values = color_mapping) +
    scale_y_continuous(limits = c(0,1))+
    theme_classic() +
    labs(
      title = paste("Overall Survival - Histology Cat and Grade ", unique(subset_data$hist_catgrade)),
      x = "Days Post Injection",
      y = "% Survival",
      color = ""
    ) +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "right",
      aspect.ratio = aspectratio
    )
})
plot_list <- lapply(plot_list, function(p) {
  p + theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))
})
# Combine the plots into a grid
combined_plot <- grid.arrange(grobs = plot_list, ncol = 4,
                              padding = unit(3, "cm"))



master_plot_list[[7]]<-combined_plot


# 
# ggarrange(plotlist=plot_list, common.legend = TRUE)+
#   theme(plot.margin = margin(2,2,2,5, "cm"))


###################################################
colors<-readRDS("ds/cohort_colors.rds")

# nf1_palette<-colors$hex
#note that the hue_pal call refers to the default r palette. 

color_mapping <- setNames(alpha(colors$hex, 0.65), colors$resultant_geno)




# Split data into subsets based on 'hist_cat'
data_list <- split(df1, df1$resultant_geno)

# Create a list of ggplot objects, one for each subset
plot_list <- lapply(data_list, function(subset_data) {
  ggplot(subset_data) +
    geom_km(aes(time = aod, color = hist_catgrade, status = event), linewidth = 2) +
    xlim(0, 150) +
   
    scale_y_continuous(limits = c(0,1))+
    theme_classic() +
    labs(
      title = paste("Overall Survival - Within Genotype ", unique(subset_data$resultant_geno)),
      x = "Days Post Injection",
      y = "% Survival",
      color = ""
    ) +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "right",
      aspect.ratio = aspectratio
    )
})

# Combine the plots into a grid
combined_plot <- grid.arrange(grobs = plot_list)



master_plot_list[[8]]<-combined_plot

###################################################
colors<-readRDS("ds/cohort_colors.rds")

# nf1_palette<-colors$hex
#note that the hue_pal call refers to the default r palette. 

color_mapping <- setNames(alpha(colors$hex, 0.65), colors$resultant_geno)




# Split data into subsets based on 'hist_cat'
data_list <- split(df1, df1$hist_catgrade)

# Create a list of ggplot objects, one for each subset
plot_list <- lapply(data_list, function(subset_data) {
  ggplot(subset_data) +
    geom_km(aes(time = aod, color = resultant_geno, status = event), linewidth = 2) +
    xlim(0, 150) +
    scale_color_manual(values = color_mapping) +
    scale_y_continuous(limits = c(0,1))+
    theme_classic() +
    labs(
      title = paste("Overall Survival - For each grade-cat combo ", unique(subset_data$hist_catgrade)),
      x = "Days Post Injection",
      y = "% Survival",
      color = ""
    ) +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "right",
      aspect.ratio = aspectratio
    )
})

# Combine the plots into a grid
combined_plot <- grid.arrange(grobs = plot_list)



master_plot_list[[9]]<-combined_plot




###################################################
# nf1_palette<-colors$hex
#note that the hue_pal call refers to the default r palette. 

library(RColorBrewer)

# Unique categories
categories <- unique(df1$hist_catgrade)



if (length(categories) > 9) {
  color_palette <- colorRampPalette(brewer.pal(9, "Set3"))(length(categories))
}

# Create a named color vector
color_mapping <- setNames(color_palette, categories)





# Split data into subsets based on 'hist_cat'
data_list <- split(df1, df1$resultant_geno)

# Create a list of ggplot objects, one for each subset
plot_list <- lapply(data_list, function(subset_data) {
  ggplot(subset_data) +
    geom_km(aes(time = aod, color = hist_catgrade, status = event), linewidth = 2) +
    xlim(0, 150) +
    
    
    scale_color_manual(values = color_mapping) +
    
    scale_y_continuous(limits = c(0,1))+
    theme_classic() +
    labs(
      title = paste("Overall Survival - Within Genotype ", unique(subset_data$resultant_geno)),
      x = "Days Post Injection",
      y = "% Survival",
      color = ""
    ) +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "right",
      aspect.ratio = aspectratio
    )
})

# Combine the plots into a grid
combined_plot <- grid.arrange(grobs = plot_list)


master_plot_list[[10]]<-combined_plot




###################################################

split1 <- unique(df1$resultant_geno)
plot_list<-list()



for (sp in 1:length(split1)){

split_name<-split1[sp]  
df2<-df1%>%filter(resultant_geno==split_name)


# Unique categories
split2 <- "hist_cat"


data_list <- split(df2, df2[[split2]])



# Create a list of ggplot objects, one for each subset
plot_list[[split_name]] <- lapply(data_list, function(subset_data) {
  ggplot(subset_data) +
    geom_km(aes(time = aod, color = hist_catgrade, status = event), linewidth = 2) +
    xlim(0, 150) +
    
    # scale_color_manual(values = color_mapping) +
    
    scale_y_continuous(limits = c(0,1))+
    theme_classic() +
    labs(
      title = paste("Overall Survival - Within Genotype \n", unique(subset_data$resultant_geno)),
      x = "Days Post Injection",
      y = "% Survival",
      color = ""
    ) +
    theme(
      plot.title = element_text(hjust = 0.5, size=10),
      legend.position = "right",
      aspect.ratio = aspectratio
    )
})

}
# Combine the plots into a grid
combined_plot <- grid.arrange(grobs = flatten(plot_list), ncol=6)


master_plot_list[[11]]<-combined_plot




###################################################

split1 <- unique(df1$resultant_geno)
plot_list<-list()



for (sp in 1:length(split1)){
  
  split_name<-split1[sp]  
  df2<-df1%>%filter(resultant_geno==split_name)
  
  
  # Unique categories
  split2 <- "hist_cat"
  
  
  data_list <- split(df2, df2[[split2]])
  
  
  
  # Create a list of ggplot objects, one for each subset
  plot_list[[split_name]] <- lapply(data_list, function(subset_data) {
    ggplot(subset_data) +
      geom_km(aes(time = aod, color = hist, status = event), linewidth = 2) +
      xlim(0, 150) +
      
      # scale_color_manual(values = color_mapping) +
      
      scale_y_continuous(limits = c(0,1))+
      theme_classic() +
      labs(
        title = paste("Overall Survival - Within Genotype \n", unique(subset_data$resultant_geno)),
        x = "Days Post Injection",
        y = "% Survival",
        color = ""
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, size=10),
        legend.position = "right",
        aspect.ratio = aspectratio
      )
  })
  
}
# Combine the plots into a grid
combined_plot <- grid.arrange(grobs = flatten(plot_list), ncol=6)



master_plot_list[[12]]<-combined_plot









###################################################

split1 <- unique(df1$resultant_geno)
plot_list<-list()



for (sp in 1:length(split1)){
  
  split_name<-split1[sp]  
  df2<-df1%>%filter(resultant_geno==split_name)
  
  
  # Unique categories
  split2 <- "hist_grade"
  
  
  data_list <- split(df2, df2[[split2]])
  
  
  
  # Create a list of ggplot objects, one for each subset
  plot_list[[split_name]] <- lapply(data_list, function(subset_data) {
    ggplot(subset_data) +
      geom_km(aes(time = aod, color = hist_cat, status = event), linewidth = 2) +
      xlim(0, 150) +
      
      # scale_color_manual(values = color_mapping) +
      
      scale_y_continuous(limits = c(0,1))+
      theme_classic() +
      labs(
        title = paste("Overall Survival - Within Genotype \n", unique(subset_data$resultant_geno)),
        x = "Days Post Injection",
        y = "% Survival",
        color = ""
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, size=10),
        legend.position = "right",
        aspect.ratio = aspectratio
      )
  })
  
}
# Combine the plots into a grid
combined_plot <- grid.arrange(grobs = flatten(plot_list), ncol=6)


master_plot_list[[13]]<-combined_plot





for (i in seq_along(master_plot_list)) {
  plot_to_pdf <- master_plot_list[[i]]
  

  ggsave(paste0("surv/plots/", "combined_surv_plot", i, ".pdf"),
         
         title=paste0("src: ",
                      
                      rstudioapi::getSourceEditorContext()$path%>%
                        sub("/uufs/chpc.utah.edu/common/home/holmen-group1/otrimskig/rchpc/","",.),
                      
                      " at ", 
                      
                      lubridate::round_date(Sys.time(), "second")
         ),
         
         
         
         
         
         plot=plot_to_pdf,
         
         
         
         
         limitsize = FALSE,
         
         
         height=20,
         width=50,
         scale = .5,
         dpi=600,
         
         
         
  )
  
  
  
  
  
  
}













