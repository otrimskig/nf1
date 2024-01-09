library(tidyverse)
library(fs)

projdir<-getwd()


path<-"X:/Holmen Lab/Lab Personnel/Garrett/CZI scope files/"
setwd(path)
file_list<-dir_ls(recurse = TRUE, type = "file")%>%
  as_tibble()%>%
  rename(path=value)%>%
  filter(!grepl("meta", path))%>%
  mutate(fn=basename(path))%>%
  mutate(mouse_num=str_extract(fn, "^\\d+"))



setwd(projdir)
saveRDS(file_list, "slides/slide_snap_files.rds")