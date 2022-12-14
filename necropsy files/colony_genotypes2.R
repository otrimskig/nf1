#cleaned information from genotyping file. 

##################
library(tidyverse)
library(janitor)
library(stringr)
library(lubridate)


#get all relevant mouse numbers.
#since some in colony are not actually IN in the colony dataset,
#join that mouse number dataset to the necropsy dataset.
#filter distinct, then you have all the mice in our colony.
  full_join(read_csv("../mouse cohorts/compiled_cohorts3.csv")%>%select(mouse_num),
            read_csv("colony_necropsy4.csv")%>%select(mouse_num))%>%
  
  distinct()->colony_mice
  #315 mice.
  
#save for later use in csv. 
write_csv(colony_mice, "mouse_numbers.csv")
  
#####
#now import "uncleaned" but fully collated genotyping file.
read_csv("genotypes_joined.csv")%>%
  #extract mouse_num from mouse number information. 
  mutate(mouse_num2 = mouse_num)%>%
  mutate(mouse_num = substring(mouse_num, 1,5))%>%
  #filter for mice in our colony.
  filter(mouse_num %in% colony_mice$mouse_num)->colony_genos
#316 mice. 

colony_genos%>%
  count(mouse_num)%>%
  arrange(desc(n))%>%
  view()
#2 mice are repeats. Means those mice are repeated, AND there is at least one "missing"
#mouse in the genotyping file itself. 
#checked source file and updated. 
#re-run code above with edited source file ("genotypes_joined.csv)...
  
#now no duplicates, and all mice were found.
#315 total mice in colony.
#52 columns. 


#one cage was not converted properly due to formatting issues.
#creating the correct version...
read_csv("mice24801cagemanual.csv")%>%
  left_join(colony_genos%>%
              select(index, info, mouse_num2), 
            by = c("mouse_num" = "mouse_num2"))%>%
  select(-dispos)%>%
  rename(mouse_num2 = mouse_num)%>%
  mutate(mouse_num = substring(mouse_num2, 1,5))->cage24




  
  #removing erroneous data in the original file that will be replaced.
  anti_join(cage24, by = "index")%>%
  
  full_join(cage24)%>%
  
  
  #removing columns that have no data. 
  select(where(function(x) any(!is.na(x))))%>%
  #now just 23 columns. 
  
  #unnecessary column now.
  select(-info)%>%
  
  
  
  
  
  view()
