

#attempt at reading in genotyping file....
library(tidyverse)
library(data.table)
library(janitor)

fread("../copy_genotyping.csv", select=c(1:20))->all_genotypes



all_genotypes%>%
  as_tibble()%>%
  row_to_names(1)%>%
  clean_names()%>%
  mutate(index = 1:NROW(.))%>%
  relocate(index)%>%
 
  select(-c(tail_snip:paternal_id))%>%
  rename(mouse_num = hci)%>%
  select(-c(mc_setup:last_col()))%>%
  
  
  mutate(type = if_else(grepl("^[0-9]{5}", mouse_num), "num",
                        if_else(nchar(mouse_num)<2, "empty",
                                "info")))%>%
  

  #removing extraneous data outside range of colony to 
  #reduce compulational load. 
  filter(index>6427 & index<16340)->all_genotypes1.2
  
  #some reorganizing.
all_genotypes1.2%>%
  rename(dob = bd)%>%
  relocate(c(death_date, disposition, type), .after = dob)%>%
 
relocate(index, type
           )->all_genotypes1.3


all_genotypes1.3%>%
  write_csv("genotypes_cages_colony.csv")



##################################################