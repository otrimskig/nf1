read.csv("rick.csv", header = FALSE)%>%
  as_tibble()%>%
  mutate(desc = substr(V1, 7, nchar(V1)))%>%
  mutate(exp = substr(V1, 0, 5))%>%
  rename(description = desc)%>%
  select(-V1)%>%
  relocate(exp)%>%
  write.csv("rick2.csv")
  


"X:/Holmen Lab/Lab Personnel/Garrett/NF1 data compiled/data/Experiments (rick)/"




#get table of folders/exp to keep. 
read.csv("rick3.csv")%>%
  as_tibble()%>%
  clean_names()%>%
  filter(need == TRUE)%>%
  select(exp)


#make into tocopy R object. 
tocopy<-read.csv("rick3.csv")%>%
  as_tibble()%>%
  clean_names()%>%
  filter(need == TRUE)%>%
  select(exp)



#get table of folder names.

#delete<-
table<-
read.csv("rick3.csv")%>%
  as_tibble()%>%
  clean_names()%>%
  select(need, exp)%>%
  mutate(path = paste0("X:/Holmen Lab/Lab Personnel/Garrett/NF1 data compiled/data/Experiments (rick)/", exp))%>%
  filter(need == FALSE)%>%
  select(exp)%>%
  slice(-(1:4))


file.remove(delete)


setwd("X:/Holmen Lab/Lab Personnel/Garrett/R/nf1/")
setwd("X:/Holmen Lab/Lab Personnel/Garrett/NF1 data compiled/data/Experiments (rick)/")



delete<-pull(table, exp)

delete

unlink(delete, recursive = TRUE)


