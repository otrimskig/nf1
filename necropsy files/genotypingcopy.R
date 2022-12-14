

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



all_genotypes1.3%>%
  select(-c(cage_number:disposition))->all_genotypes2







all_genotypes2%>%
  filter(type=="info")%>%
  pull(index)->info_index


#successful for loop. 
#assigning each separate cage to its own object. 
for(i in 1:length(info_index)){
  assign(paste0("gtp", i),
         if (i==1){ all_genotypes2%>%
             filter(index < info_index[(i+1)])
           
         } else if (i==(length(info_index))){
           all_genotypes2%>%
             filter(index>=info_index[(length(info_index))])
           
         }else {
          all_genotypes2%>%
             filter(index <info_index[i+2] & index>=(info_index[(i+1)]))
         }
  )
}


#get list of objects made in the above for-loop.
#filter out objects with no rows. 

df_list = Filter(function(x) nrow(x)>0, mget(ls(pattern = "gtp[0-9]")))



#use list to perform transformation on list. 
for (i in 1:length(df_list)){
  #for every df on this list...
  df_list[[i]]%<>%
    
    #use the cell coordinates to add the value to the strain column.
    #this coordinate matches to the cage information. 
    mutate(strain = ifelse(type == "num", .[1,3], NA))%>%
    
    row_to_names(1)%>%
    clean_names()%>%
    
    #consistent naming structure for all dfs. 
    rename(mouse_num = 3)%>%
    rename(strain = na)%>%
    rename(index = 1)%>%
    
    #change "" to NAs and then subsequently remove any 
    #column that is all NAs. 
    mutate_all(~ case_when(.==""~NA_character_, TRUE~as.character(.)))%>%
    select(where(function(x) any(!is.na(x))))
  
}


#removing all df where "mouse_num" isn't the 3rd column. 
#this prevents wrongly parsed cage info from stopping the join in the next step.
df_list2 = Filter(function(x) (colnames(x)[3] == "mouse_num"), df_list)



#flatten/join all objects in list to one dataset. 
genos_flattened<-
  reduce(df_list2, full_join)

#finally, save dataset to a csv.
genos_flattened%>%
  write_csv("genotypes_joined2.csv")


###########################################################################################
###############################################################################################





