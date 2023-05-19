library(tidyverse)
library(janitor)
library(stringr)
library(lubridate)
library(rio)
library(fs)


readRDS("nec_files.rds")->nec_files


readRDS("necropsy_files.rds")->necropsy_files






files<-list.files("./nec1/")
length(files)

files<- files[1:2]

files

for(i in 1:length(files)){suppressWarnings(
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
    write_csv(paste("nec2/",files[i])))
}



nec1<-list.files("./nec1/")
nec2<-list.files("./nec2/")


nec1%>%
  as_tibble()%>%
  
  rename(nec1 = value)%>%
 
  mutate(original_file = nec1)->nec1.1
  
  #mutate(nec1 = gsub("\\.[^.]*$", "", nec1))

nec2%>%
  as_tibble()%>%
  rename(nec2 = value)->nec2.1
  
  #mutate(nec2 = gsub("\\.[^.]*$", "", nec2))

full_join(nec1.1, nec2.1, by = c("nec1"="nec2"))%>%
  filter(is.na(original_file))%>%
  pull(nec1)->non_converted



non_converted%>%
  as_tibble()%>%
  mutate(value=trimws(value))%>%
  
  pull(value)->non_converted2

non_converted2





for(i in 1:length(non_converted2)){suppressWarnings(
  read.csv(paste0("nec1/",non_converted2[i]))%>%
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
    write_csv(paste("nec2/",non_converted2[i])))
}