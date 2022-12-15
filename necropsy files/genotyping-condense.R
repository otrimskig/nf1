#making dataset from condensed genotyping file 
#subsetted to include only the range within which is contained
#mice from my colony.

library(tidyverse)
library(janitor)
library(data.table)
library(beepr)

read_csv("genotypes_cages_colony.csv")->all_genotypes1.3

all_genotypes1.3%>%
  view()

#removing extraneous information that can be re-assembled later.
#keeping only genotypes information, and data needed to parse. 
all_genotypes1.3%>%
  select(-c(cage_number:disposition))%>%
  rename(gen1 = genotyping_results,
         gen2 = x,
         gen3 = x_2,
         gen4 = x_3,
         gen5 = x_4,
         gen6 = x_5)%>%
  slice(-1)->all_genotypes2


all_genotypes2%>%
  view()




all_genotypes2%>%
  filter(type=="info")%>%
  pull(index)->info_index

info_index

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

###############################################################################
#testing for-loop.

# gtp1%>%
#   mutate(strain = ifelse(type == "num", .[1,3], NA))%>%
# 
#   row_to_names(1)%>%
#   clean_names()%>%
# 
#   #consistent naming structure for all dfs.
#   rename(mouse_num = 3)%>%
#   rename(strain = last_col())%>%
#   rename(index = 1)%>%
#   select(-2)%>%
# 
#   #change "" to NAs and then subsequently remove any
#   #column that is all NAs.
#   mutate_all(~ case_when(.==""~NA_character_, TRUE~as.character(.)))%>%
#   select(where(function(x) any(!is.na(x))))


################################################################################

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
    rename(strain = last_col())%>%
    rename(index = 1)%>%
    select(-2)%>%
    
    #change "" to NAs and then subsequently remove any 
    #column that is all NAs. 
    mutate_all(~ case_when(.==""~NA_character_, TRUE~as.character(.)))%>%
    select(where(function(x) any(!is.na(x))))
  
}




##############################################################################3
#removing all df where "mouse_num" isn't the 3rd column. 
#this prevents wrongly parsed cage info from stopping the join in the next step.
df_list2 = Filter(function(x) (colnames(x)[2] == "mouse_num"), df_list)

df_list2

#flatten/join all objects in list to one dataset. 
genos_flattened<-
  reduce(df_list2, full_join)






#finally, save dataset to a csv.
genos_flattened%>%
  write_csv("genotypes_joined2.csv")

#and RDS.
saveRDS(genos_flattened, "genos_flattened.rds")

genos_flattened%>%
  view()


beep(sound = 4)


###########################################################################################
###############################################################################################




