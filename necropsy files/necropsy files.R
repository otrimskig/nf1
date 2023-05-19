library(tidyverse)
library(janitor)
library(stringr)
library(lubridate)

#get list of all necropsy files in holmen X drive, and corresponding mouse numbers. 
#write that into a csv.

  list.files(path = "X:/Holmen Lab/Necropsy files", 
             full.names = TRUE, 
             recursive = TRUE, 
             ignore.case = TRUE)%>%
  as_tibble()%>%
  mutate(mouse_num = as.character(str_extract_all(value, "[0-9]{5}")))%>%
  filter(nchar(mouse_num)==5)%>%
  rename(necropsy_file_path = value)%>%
  mutate(necropsy_file_path = gsub("\\~\\$", "", necropsy_file_path))%>%
  
  write_csv("necropsy_file_paths.csv")


  
  nec_files<-list.files(path = "X:/Holmen Lab/", pattern = "necropsy",
           full.names = TRUE, 
           recursive = TRUE, 
           ignore.case = TRUE)

saveRDS(nec_files, "nec_files.rds")
  
  
necropsy_files<-  
nec_files%>%
  as_tibble()%>%
  rename(file_path = value)%>%
  filter(!grepl("NVCI", file_path))%>%
  filter(grepl(".xls", file_path))%>%
  mutate(filename = gsub(".*\\/", "", file_path))%>%
  mutate(mouse_num = as.character(str_extract_all(filename, "[0-9]{5}")))%>%
  filter(nchar(mouse_num)==5)%>%
  mutate(necropsy_folder = ifelse(grepl("X:/Holmen Lab//Necropsy files/", file_path), 1, 0))%>%
  group_by(mouse_num)%>%
  arrange(desc(necropsy_folder))%>%
  slice(1)%>%
  ungroup()

necropsy_files%>%
  saveRDS("necropsy_files.rds")
  

# nec_file_copy<-
# read_csv("X:/Holmen Lab/Lab Personnel/Garrett/R/nf1/mouse cohorts/compiled_cohorts3.csv")%>%
#   select(mouse_num)%>%
#   mutate(mouse_num = as.character(mouse_num))%>%
#   left_join(necropsy_files)%>%
#   drop_na()

# nec_file_copy

#get all files into dir nec1.
file.copy(from=necropsy_files%>%pull(file_path), 
          to="nec1/", 
          overwrite = TRUE, 
          recursive = TRUE, 
          copy.mode = TRUE)

######
#changing all files to csvs. 


library(rio)

xlsx<- paste0("nec1/", dir("nec1/", pattern = "xlsx"))
created <- mapply(convert, xlsx, gsub("xlsx", "csv", xlsx))



xls<- 
  paste0("nec1/", dir("nec1/", pattern = "xls"))%>%
  as_tibble()%>%
  filter(!grepl("xlsx", value))%>%
  pull()

created <- mapply(convert, xls, gsub("xls", "csv", xls))

#remove all xls or xlsx files from dir.
list.files("./nec1/")%>%
  as_tibble()%>%
  filter((grepl(".xls", value)))%>%
  mutate(value = paste0("nec1/", value))%>%
  pull()%>%
  file.remove()

#get all csv files into proper form.
files<-list.files("./nec1/")
length(files)


##################################################################
for(i in 1:length(files)){
    read.csv(paste0("nec1/",files[i]))%>%
      as_tibble()%>%
      clean_names()%>%
      unite(x2, c(2:last_col()))%>%
      select(1,2)%>%
      data.frame()%>%
      t()%>%
      as_tibble()%>%
      row_to_names(1)%>%
      clean_names()%>%
      mutate_all(as.character)%>%
      write_csv(paste("nec2/",files[i]))
}





#get all csvs into environment.
files2<-paste0("nec2/",list.files("./nec2/"))
length(files2)


for(i in 1:length(files2)){
  assign(paste0("necropsy_file", i),  
         read_csv(files2[i])%>%
           mutate_all(as.character))
}
df_list = mget(ls(pattern = "necropsy_file[0-9]"))

all_necropsy<-
  reduce(df_list, full_join)

all_necropsy%>%
  mutate(across(everything(), gsub, pattern = "_NA", replacement = ""))%>%
  mutate(across(everything(), gsub, pattern = "_", replacement = ""))%>%
  write_csv("colony_necropsy.csv")


#############################################