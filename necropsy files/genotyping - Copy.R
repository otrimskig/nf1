
#attempt at reading in genotyping file....
library(tidyverse)
library(data.table)
library(janitor)

fread("../copy_genotyping.csv", select=c(1:20))->all_genotypes


all_genotypes%>%
  as_tibble()%>%
  slice(1:200
        
  )->geno_sample


geno_sample%>%
  row_to_names(1)%>%
  clean_names()%>%
  mutate(index = 1:NROW(.))%>%
  relocate(index)%>%
  select(-c(cage_number:paternal_id))%>%
  rename(mouse_num = hci)%>%
  select(-c(death_date:last_col()))%>%
  mutate(type = if_else(grepl("^[0-9]{5}", mouse_num), "num",
                        if_else(nchar(mouse_num)<2, "empty",
                                "info")))%>%
  relocate(index, type)->geno_sample2


geno_sample2%>%
  filter(type=="info")%>%
  pull(index)->info_index

#vector of indexes in dataset where row data is info.
info_index

#split dataframe starting with every info_index number. 

#df1 = index< info_index[2]

#gtp[i] <- if [i]==1, df[geno_sample2$index<info_index[i]]
#else_if[i]>1, df[geno_sample$index<info_index[i] AND geno_sample$index>info_index[i-1]]

# for(i in 1:length(files2)){
#   assign(paste0("necropsy_file", i),  
#          read_csv(files2[i])%>%
#            mutate_all(as.character))
# }
# df_list = mget(ls(pattern = "necropsy_file[0-9]"))
# 
# all_necropsy<-
#   reduce(df_list, full_join)
geno_sample2

#testing conditions for for loop.
geno_sample2%>%
  filter(index<info_index[1])


geno_sample2%>%
  filter(index<info_index[2])



geno_sample2%>%
  filter(index<info_index[3]&index>=info_index[2])



#successful for loop. 
for(i in 1:length(info_index)){
  assign(paste0("gtp", i),
         if (i==1){ geno_sample2%>%
             filter(index < info_index[(i+1)])
           
         } else if (i==(length(info_index))){
           geno_sample2%>%
             filter(index>=info_index[(length(info_index))])
           
         }else {
           geno_sample2%>%
             filter(index <info_index[i+2] & index>=(info_index[(i+1)]))
         }
  )
}


#get list of objects made.

df_list = Filter(function(x) nrow(x)>0, mget(ls(pattern = "gtp[0-9]")))




#use list to perform transformation on list. 
for (i in 1:length(df_list)){
  
  df_list[[i]]%<>%
    mutate(strain = ifelse(type == "num", .[1,3], NA))%>%
    row_to_names(1)%>%
    clean_names()%>%
    rename(mouse_num = 3)%>%
    rename(strain = na)%>%
    rename(index = 1)%>%
    mutate_all(~ case_when(.==""~NA_character_, TRUE~as.character(.)))%>%
    select(where(function(x) any(!is.na(x))))
  
}

#flatten/join all objects in list to one dataset. 
genos_flattened<-
  reduce(df_list, full_join)

#save dataset. 
genos_flattened%>%
  view()























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
  select(-c(cage_number:paternal_id))%>%
  rename(mouse_num = hci)%>%
  select(-c(death_date:last_col()))%>%
  
  
  mutate(type = if_else(grepl("^[0-9]{5}", mouse_num), "num",
                        if_else(nchar(mouse_num)<2, "empty",
                                "info")))%>%
  relocate(index, type)->all_genotypes2


all_genotypes2%>%
  filter(type=="info")%>%
  pull(index)->info_index


#successful for loop. 
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


#get list of objects made.

df_list = Filter(function(x) nrow(x)>0, mget(ls(pattern = "gtp[0-9]")))


# df_list[[length(df_list)]]
# 
# df_list2<-Filter(function(x) (colnames(x)[3] == "mouse_num"), df_list)
# 
# 
# df_list[[1]]
# length(df_list)
# length(df_list2)

#use list to perform transformation on list. 
for (i in 1:length(df_list)){
  
  df_list[[i]]%<>%
    mutate(strain = ifelse(type == "num", .[1,3], NA))%>%
    row_to_names(1)%>%
    clean_names()%>%
    rename(mouse_num = 3)%>%
    rename(strain = na)%>%
    rename(index = 1)%>%
    mutate_all(~ case_when(.==""~NA_character_, TRUE~as.character(.)))%>%
    select(where(function(x) any(!is.na(x))))
  
}

df_list2 = Filter(function(x) (colnames(x)[3] == "mouse_num"), df_list)



#flatten/join all objects in list to one dataset. 
genos_flattened<-
  reduce(df_list2, full_join)

#save dataset. 
genos_flattened%>%
  write_csv("genotypes_joined.csv")


###########################################################################################
###############################################################################################





