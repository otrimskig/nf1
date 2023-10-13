setwd("C:/Users/u1413890/OneDrive - University of Utah/garrett hl-onedrive/R/nf1/cohorts3")


library(tidyverse)
library(fs)

scanned<-read_csv("ds/scanned_slides_info.csv")

df<-readRDS("ds/nf1_cohorts.2.dates.rds")%>%
  select(exclude, mouse_num, src, metadata, resultant_geno)


df2<-df%>%
  semi_join(scanned, by = "mouse_num")%>%
  left_join(scanned, by = "mouse_num")

#save for later
saveRDS(df2, "ds/cohort_scanned_slides.rds")

new_path_base<-"E:/nf1 glioma scanned/"


df_copy<-df2%>%
  select(mouse_num, path, filename)%>%
  rename(path1=path)%>%
  mutate(path2=paste0(new_path_base, filename, ".mrxs"))


file_copy(df_copy$path1, df_copy$path2)


df_dir_copy<-df2%>%
  select(mouse_num, folder,filename)%>%
  mutate(dir1=paste0(folder,filename,"/"))%>%
  mutate(dir2=paste0(new_path_base, filename, "/"))%>%
  arrange(mouse_num)%>%
  slice(1:50)


dir_copy(df_dir_copy$dir1, df_dir_copy$dir2)


