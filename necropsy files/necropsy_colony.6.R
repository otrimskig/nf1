##################
library(tidyverse)
library(janitor)
library(stringr)
library(lubridate)



read_csv("colony_necropsy4.csv")->colony_necropsy4
colony_necropsy4%>%
  view()
#261 


colony_necropsy4%>%
  group_by(mouse_num)%>%
  slice(1)%>%
  view()
#261 entries.

#finding the repeat (unnecessary once source file was fixed.)
colony_necropsy4%>%
  count(mouse_num)%>%
  arrange(desc(n))%>%
  view()

#mouse 28204 is repeat.
#from observations, appears to be mis-entry of data. Checking for correct number...
#checked data from compiled_cohorts3.csv. 
#one of the "28204"s from colony_necropsy is actually 28205. 
#changed in colony_necropsy3.csv outside R, 
#so is correct going forward in any further transformations. 

#Also was instance in colony_necropsy4.csv of 28206 being an issue.. also fixed. 
#there was no 28206 necropsy. 

#run this again to be sure.
colony_necropsy4%>%
  count(mouse_num)%>%
  arrange(desc(n))%>%
  view()
#now shows correct mouse number (261)


#now sorting for mice in this dataset that are in my colony of interest.
#first, getting a list of unique mouse numbers
#from most current cohort dataset.
read_csv("../mouse cohorts/compiled_cohorts3.csv")%>%
  select(mouse_num)%>%
  group_by(mouse_num)%>%
  slice(1)%>%
  ungroup->mouse_numbers

str(mouse_numbers)
#313 mice. 

#now a filtering join of the necropsy dataset for mice in my colony.
semi_join(colony_necropsy4, mouse_numbers)%>%
  view()
  
#259 mice. only 2 in the dataset not also in colony. 


#checking for the numbers that were not included in the colony dataset...
anti_join(colony_necropsy4, mouse_numbers)%>%
  view()


#appears based on genotypes and treatments that these mice ARE in the colony...
#though not listed in the compiled dataset. 
#mouse nums 258225, 29338.
#will have to check when re-joining data that mice weren't excluded for some reason. 

#seems best to continue with the 2 mice included in this dataset. 
#will check when re-joining to full cohort dataset why they weren't in there. 
#for now move forward with colony_necropsy4. 

colony_necropsy4%>%
  view()


#getting manually entered information from this file. 
read_csv("colony_necropsy5.m.csv")%>%
  
  select(mouse_num, tumor_bool, c(tis_brain:xclude)
         )->tis_and_tumors


colony_necropsy4%>%
  left_join(tis_and_tumors, by = "mouse_num")%>%
  
 
  #checked for conflicts in tumor_bool columns. 
  # filter(tumor_bool.x != tumor_bool.y)%>%
  #confirmed no conflicts; deleting and renaming for 1 column.
  rename(tumor_bool = tumor_bool.y)%>%
  select(-tumor_bool.x)%>%
  
  #remove outdated columns.
  select(-c(tissue_br:tissue_liver))%>%
  
  select(where(function(x) any(!is.na(x))))%>%
  
  
  
  
  view()
