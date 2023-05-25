library(googlesheets4)
library(tidyverse)
library(janitor)
library(lubridate)

#pull in data from google sheet. 

#authorize user
gs4_auth(email = "gotrimski@gmail.com")


df<-read_sheet("1-iPqqUo4gSoGbWMlbuqFMtitNGLux98Z3a_jHuYRmtM",
               sheet="boxes",
               col_names = FALSE,
               col_types = "c")%>%
  clean_names()%>%
  mutate(across(1:last_col(), tolower))%>%
  mutate(index = 1:nrow(.))%>%
  relocate(index)


df%>%
  filter(!is.na(x1))%>%
  select(index)%>%pull()->box_ns


box_ns


for(i in 1:length(box_ns)){

  assign(paste0("nf1_box", sprintf("%02d", i)),
         if (i==1){ 
           df%>%
             filter(index < box_ns[(i+1)])
           
         } else if (i==(length(box_ns))){
           df%>%
             filter(index>=box_ns[(length(box_ns))])
           
         }else {
           df%>%
             filter(index <box_ns[i+2] & index>=(box_ns[(i+1)]))
         }
  )
}



df_list = Filter(function(x) nrow(x)>0, mget(ls(pattern = "nf1_box[0-9]")))

#############################
#transform all datasets. 


for(i in 1:length(df_list)){

  
  #get box name
  box_name<-(df_list[[i]]%>%
               pull(x2))[1]

  
  
  assign(paste0("nf1_2_box", sprintf("%02d", i)),    
  #change format from box to 2-column df. 
df_list[[i]]%>%
  
  slice(2:nrow(.))%>%
  select(-c(index, x1))%>%
  
  
  row_to_names(1)%>%
  clean_names()%>%
  
  
  pivot_longer(2:last_col(),
               names_to = "xcol", 
               values_to = "sample_name")%>%
  
  mutate(box_name = box_name)%>%
  
  mutate(across(1:last_col(), function(x){na_if(x,"na")}))%>%
  
  filter(!is.na(sample_name))
  
  
)
}     
       
       
       

df_list2 = Filter(function(x) nrow(x)>0, mget(ls(pattern = "nf1_2_box[0-9]")))


map_dfr(df_list2, bind_rows)%>%
  
  
  mutate(xcol = sub("x", "", xcol))%>%
  mutate(coord = paste0(xrow, xcol))%>%
  select(-c(xrow, xcol))%>%
  relocate(coord)%>%
  mutate(temp = "-80")%>%
  
write_rds("sample_inv/sample_inv_80.rds")



  
range_write("1-iPqqUo4gSoGbWMlbuqFMtitNGLux98Z3a_jHuYRmtM",
             readRDS("sample_inv/sample_inv_80.rds"),
             sheet="tidy",
             range="b2")



