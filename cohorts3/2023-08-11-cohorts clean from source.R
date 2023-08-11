#going from raw data, to compiled_cohorts3. 

library(tidyverse)
library(janitor)
library(openxlsx)
library(lubridate)
library(stringr)

RE.df1.0 <- read.xlsx("raw/Riley NF1 Glioma Data (Mouse Cohorts).xlsx", 
                       sheet = "Mouse Information",
                       detectDates = TRUE)


RE.df1.1<-RE.df1.0%>%
  
  clean_names()%>%
  
  #new clean column mouse_num from mouse_number.
  #remove tag information. 
  mutate(mouse_num = substr(mouse_number, 1,5))%>%
  relocate(mouse_num)%>%
  #filter out any rows that aren't mouse numbers.
  filter(grepl("[0-9]", mouse_num))%>%

  #parsing out the injected by, 
  #from the improperly formatted injection_date column. 
  mutate(injected_by =  str_match(injection_date, "\\((.*?)\\)")[, 2])%>%
  
  #now that injected_by info is preserved
  #in separated column, fixing injection_date formatting
  #removes extraneous characters.
  mutate(injection_date = str_replace_all(injection_date, "[^0-9/-]", ""))%>%
  #gets multiple formatted dates into 1 date format. Uses lubridate. 
  mutate(injection_date = case_when(
    str_detect(injection_date, "/") ~ mdy(injection_date),
    str_detect(injection_date, "-") ~ ymd(injection_date),
    TRUE ~ NA_Date_))%>%
 
   #adding src for source document, if conflicts need to 
  #be resolved later. 
  mutate(src = "RE")


#now first bit of rick's data. 
RH.df1.0  <- read.xlsx("raw/NF1 Glioma Mouse Information (Rick).xlsx",
                      sheet = "In Vivo Tumor Study",
                      detectDates = TRUE)%>%
   clean_names()


RH.df1.1<-RH.df1.0%>%
  clean_names()%>%
  #remove tag information. 
  
  mutate(mouse_num = substr(mouse_id_greyed_out_if_it_will_be_excluded_from_results, 1,5))%>%
  relocate(mouse_num)%>%
  #filter out any rows that aren't mouse numbers.
  filter(grepl("[0-9]", mouse_num))




#first join of data.
df1.0<-full_join(RH.df1.1, RE.df1.1)


#master list of any mouse_nums listed in either dataset, without dupes. 
#these should be all the mice in our cohorts. 
mouse_nums<-df1.0%>%
  group_by(mouse_num)%>%
  slice(1)%>%
  select(mouse_num)

#saving this for later reference/use, if necessary. 
saveRDS(mouse_nums, "ds/cohort_mouse_nums.rds")



#list of nums that appear more than once
mouse_num_duplicates<-df1.0%>%
  count(mouse_num)%>%
  filter(n!=1)%>%
  select(mouse_num)




#removing duplicates from master, until they are resolved. 
#adding back the nums, without any data attached. 
df1.1<-df1.0%>%
  anti_join(mouse_num_duplicates)%>%
  right_join(mouse_nums)
#now dataset is "complete", but not cleaned, for all non-duplicated mice.
#mice that appeared in both rick and riley's files, have only their
#mouse num listed in the df1.1, with no information attached.
#this info will be checked now, below. 

# 
# df1.0b.duplicates<-df1.0%>%
#   semi_join(mouse_num_duplicates)%>%
#   group_by(mouse_num)%>%
#   fill(everything(), .direction = "down")
# 
# df1.0b.duplicates%>%
#   select(-src)%>%
# 
#   distinct()%>%
#   count(mouse_num)%>%
#   view()




