library(tidyverse)
library(fs)

#path<-"X:/Holmen Lab/Lab Personnel/Garrett/CZI scope files/cohort1-nf1KO-ptenKO-inkKO-atrxKO"

path<-"X:/Holmen Lab/Lab Personnel/Garrett/CZI scope files/"
setwd(path)
file_list<-dir_ls(recurse = TRUE, type = "file")%>%
  as_tibble()%>%
  rename(path=value)%>%
  filter(!grepl("meta", path))


#renaming based on folder. 
# pathA<-"X:/Holmen Lab/Lab Personnel/Garrett/CZI scope files/cohort1-nf1KO-ptenKO-inkKO-atrxKO/23534/"
# rn1<-dir_ls(pathA, type = "file")%>%
#   as_tibble()%>%
#   rename(path1=value)%>%
#   mutate(name1=basename(path1))%>%
#   mutate(name2=gsub("^\\d+", "23524-r",name1))
#   
# setwd(pathA)
# file_move(rn1$name1, rn1$name2)



# pathB<-"X:/Holmen Lab/Lab Personnel/Garrett/CZI scope files/cohort1-nf1KO-ptenKO-inkKO-atrxKO/28531/"
# 
# setwd(pathB)
# 
# dir_ls(recurse=T, type="file")%>%
#   as_tibble()%>%
#   rename(path1=value)%>%
#   mutate(path2=basename(path1))->rn
# 
# file_move(rn$path1, rn$path2)
# 
# dir_delete(dir_ls(type="dir"))





##renaming based on folder.
# pathA<-"X:/Holmen Lab/Lab Personnel/Garrett/CZI scope files/cohort1-nf1KO-ptenKO-inkKO-atrxKO/28531/"
# rn1<-dir_ls(pathA, type = "file")%>%
#   as_tibble()%>%
#   rename(path1=value)%>%
#   mutate(name1=basename(path1))%>%
#   mutate(name2=gsub("^\\d+", "28531-r",name1))
# 
# setwd(pathA)
# file_move(rn1$name1, rn1$name2)


# 
# pathA<-"X:/Holmen Lab/Lab Personnel/Garrett/CZI scope files/unsorted cohort/25242/"
# rn1<-dir_ls(pathA, type = "file")%>%
#   as_tibble()%>%
#   rename(path1=value)%>%
#   mutate(name1=basename(path1))%>%
#   mutate(name2=gsub("^\\d+", "25242-r",name1))
# 
# setwd(pathA)
# file_move(rn1$name1, rn1$name2)



# pathA<-"X:/Holmen Lab/Lab Personnel/Garrett/CZI scope files/"
# setwd(pathA)
# rn<-dir_ls(type = "file", recurse=T)%>%
#   as_tibble()%>%
#   rename(path1=value)%>%
#   mutate(name1=basename(path1))%>%
#   filter(grepl("metadata", path1))%>%
#   mutate(path2=paste0("meta/", name1))
# 
# 
# 
# file_move(rn$path1, rn$path2)




# pathA<-"X:/Holmen Lab/Lab Personnel/Garrett/CZI scope files/cohort2-nf1KO-ptenKO-inkKO-atrxWT/28202"
# rn<-dir_ls(pathA, type = "file")%>%
#   as_tibble()%>%
#   rename(path1=value)%>%
#   mutate(name1=basename(path1))%>%
#   mutate(name2=gsub("^\\d+", "28202-r",name1))
# 
# setwd(pathA)
# file_move(rn$name1, rn$name2)




pathA<-"X:/Holmen Lab/Lab Personnel/Garrett/CZI scope files/cohort3-nf1KO-ptenWT-inkKO-atrxWT/29821"
rn<-dir_ls(pathA, type = "file")%>%
  as_tibble()%>%
  rename(path1=value)%>%
  mutate(name1=basename(path1))%>%
  mutate(name2=gsub("^\\d+", "29821-r",name1))

setwd(pathA)
file_move(rn$name1, rn$name2)




pathA<-"X:/Holmen Lab/Lab Personnel/Garrett/CZI scope files/cohort4-nf1KO-ptenWT-inkWT-atrxWT/28782"
rn<-dir_ls(pathA, type = "file")%>%
  as_tibble()%>%
  rename(path1=value)%>%
  mutate(name1=basename(path1))%>%
  mutate(name2=gsub("^\\d+", "28782-r",name1))

setwd(pathA)
file_move(rn$name1, rn$name2)

