#####################################################
#adding in additional cage information to colony genotypes. 


library(tidyverse)
library(janitor)
library(beepr)
library(stringr)
library(lubridate)

#cleaned colony genotypes data. 
readRDS("colony_genos_clean.rds")->colony_genos



#read in file with cage info for colony number range.
#information in dataset is still in cage format.
#however, non-genotype information is consistent and 
#retained in proper columns for extraction.
read_csv("genotypes_cages_colony.csv")->cages

cages%>%
  
  #prep index for filter join.
  mutate(index = as.character(index))%>%
  #filter for indexes found in colony_genos. 
  semi_join(colony_genos, by = "index")%>%
  #remove extraneous variables. 
  select(-type, -strain, -c(genotyping_results:last_col()))%>%
  #rename to avoid confusion when joining. 
  rename(mouse_id = mouse_num)%>%
  
  #cleaning death_date variable. Some observations contain
  #both death date, and disposition information in the same column.
  
  #use sub instead of gsub here.
  #sub only replaced first instance...
  #copy non-date observations to another column.
  mutate(disp1 = sub(".*? ", "",death_date))%>%
  
  #remove non-date obs from column,
  #and transform into date format. 
  mutate(death_date = mdy(sub(" .*", "", death_date)))%>%
  
  #clean up another date column into standard date format.
  #uses library(lubridate)
  mutate(dob = dmy(dob))%>%
  
  #get copied non-date observations into their proper column. 
  #remove dates from disp1 column so they aren't accidentally
  #copied to the disposition column...
  mutate(disp1 = ifelse(grepl("^[0-9]", disp1), NA, disp1))%>%
  #now copy the observations to their proper column. 
  mutate(disposition = ifelse(is.na(disposition), disp1, disposition))%>%
  #remove unneeded var.
  select(-disp1)%>%
  
  #renaming variable to avoid confusion when joining.
  rename(g_comments = disposition)->cages1.1
  
  
#####
#now joining to genotypes previously prepared.

colony_genos%>%
  full_join(cages1.1
            
            )->colony_genos2


  #full_join gives same number of obs as original dataset.
  #looks good. 
  
#checking no repeats..
colony_genos2%>%
  count(index)%>%
  filter(n>1)
#looks good. no repeats.
#saving data to external object. 
saveRDS(colony_genos2, "colony_genos2.rds")

##################################################
##############################################