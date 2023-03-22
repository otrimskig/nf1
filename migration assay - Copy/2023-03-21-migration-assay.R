library(googlesheets4)
library(tidyverse)
library(janitor)
library(lubridate)


readRDS("2023-03-22_10-35-drive_import.rds")->df


#make vector of 1a, 1b, etc.
list((1:5), LETTERS[1:2])%>%
  expand.grid()%>%
  arrange(Var1)%>%
  unite(x, sep="")%>%
  pull()->nums

df%>%
  slice(-(1:11))%>%
  select(1:2)%>%
  row_to_names(1)%>%
  mutate(well = nums[1:9])->df1


df%>%
  slice(1:6)%>%
  select(1:2)%>%
  row_to_names(1)%>%
  clean_names()->df2



df1%>%
  mutate(number = substr(well, 1,1))%>%

  left_join(df2%>%
              mutate(number = as.character(number)))%>%
  
  
  relocate(number, line, well)->df3


df3$line%>%unlist()

df3

df3%>%
  mutate_at(c(2, 4), unlist)->df3.1
  



df3.1%>%
  group_by(line)%>%
  summarise(sd = sd(abs590),
            mean = mean(abs590))%>%right_join(df3.1)->df3.2


df3.2%>%
 
  range_write("1DYwkZEVK39qMZl1vYrg3R7hi41nye1yikUNYN4tzWbs", sheet = "out",
              ., range = "A1")



library(ggplot2)

df3.2%>%
  
  
  ggplot(aes(x= reorder(line, mean), abs590))+
  geom_point(size = 3, alpha = .3)+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  geom_point(aes(line, mean), shape = 18)






