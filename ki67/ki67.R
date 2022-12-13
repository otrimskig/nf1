library(tidyverse)
library(janitor)
library(ggsci)

k<-
read.csv("ki67.csv")%>%
  as_tibble()


mn<-
  read.csv("a.csv")%>%
  as_tibble()%>%
  clean_names()


k1<-
k%>%
  slice(3:9)%>%
  row_to_names(1)%>%
  clean_names()%>%
  pivot_longer(cols = 1:4, names_to = "tumor_type")%>%
  select(-x)%>%
  rename(percent = value)

k2<-
k%>%
  slice(14:24)%>%
  row_to_names(1)%>%
  clean_names()%>%
  pivot_longer(cols = 1:5, names_to = "tumor_location")%>%
  rename(percent = value)

k2%>%
  #join the two experiments by their unique ki67 noted percentages. 
  full_join(k1, by = "percent")%>%
  arrange(desc(percent))%>%
  slice(1:14)%>%
  slice(-4)%>%
  #manually add cell value missing in table. 
  mutate(tumor_location = ifelse(row_number() == 3, "head", tumor_location))%>%
  mutate(percent = ifelse(row_number() == 11, "10.04", percent))%>%
  #change character percent to numeric for join. 
  mutate(percent = as.numeric(percent))%>%
  left_join(mn, by = c("percent" = "x2"))%>%
  rename(mouse_num = x1)%>%
  relocate(mouse_num)%>%
  #manually added mouse numbers to data. 
  mutate(mouse_num = ifelse(row_number() == 12, "28204 B", mouse_num))%>%
  mutate(mouse_num = ifelse(row_number() == 13, "28205", mouse_num))%>%
  arrange(mouse_num)%>%
  mutate(mouse_num2 = mouse_num)%>%
  #extract only mouse numbers from combined mouse number-tumor column. 
  mutate(mouse_num = substr(mouse_num, 1,5))%>%
  #extract tumor information from combined mouse number-tumor column. 
  mutate(tumor = substr(mouse_num2, 7,8))%>%
  #clean it to all uppercase. 
  mutate(tumor = toupper(tumor))%>%
  #remove "dirty" mousenumber-tumor combined column
  select(-mouse_num2)%>%
  #manually add tumor type to specific mouse. 
  mutate(tumor_type = ifelse(row_number() == 5, "neuroepithelial", tumor_type))%>%
  write.csv("ki67_joined.csv")



#####plotting
#vector with desired order of x-axis
x_order <- c("pre_tumorigenic", "glioma","spinal_cord", "head", "nerve")


read.csv("ki67_joined.csv")%>%
  as_tibble()%>%
  select(-1)%>%
  #use code code below to ensure order of x-axis, by setting "level" with above vector. 
  ggplot(aes(x = factor(tumor_location, level = x_order), percent, 
             color = tumor_type))+
  geom_point(size = 8, 
             alpha = .5)+
  #uses ggsci package, 
  scale_color_d3()+
  theme_classic()+
  labs(x = "Tumor Location",
       y = "% Ki67-positive cells")+
  #change legend title
 guides(color = guide_legend(title = "Tumor Type"))


last_plot()
ggsave("plot.png", 
       width = 7, 
       height = 5)



####summary data of counts
read.csv("ki67_joined.csv")%>%
  as_tibble()%>%
  count(tumor_location)

read.csv("ki67_joined.csv")%>%
  as_tibble()%>%
  count(tumor_type)


############plot 2
read.csv("ki67_joined.csv")%>%
  as_tibble()%>%
  select(-1)%>%
  #use code code below to ensure order of x-axis, by setting "level" with above vector. 
  ggplot(aes(x = tumor_type, percent, 
             color = tumor_location))+
  geom_point(size = 8, 
             alpha = .5)+
  #uses ggsci package, 
  scale_color_d3()+
  theme_classic()+
  labs(x = "Tumor Location",
       y = "% Ki67-positive cells")+
  #change legend title
  guides(color = guide_legend(title = "Tumor Type"))


last_plot()
ggsave("plot2.png", 
       width = 7, 
       height = 5)
