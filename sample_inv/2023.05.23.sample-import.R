library(googlesheets4)
library(tidyverse)
library(janitor)
library(lubridate)

#pull in data from google sheet. 

#authorize user
gs4_auth(email = "gotrimski@gmail.com")

#read input sheet
boxes<-read_sheet("1iK1inudWFhjNRAjim7ygs1fJROHg-EjM-qXTQOz3uos", 
                      sheet = "input")

###############################

#read all experimental setup inputs. 
#name of plate
plate_name<-(boxes%>%pull(2))[1]

#name of layout descriptor
labels<-(boxes%>%pull(2))[3]

#name of measured value. 
measured<-(boxes%>%pull(2))[15]


###################################
#read all plate info. 
boxes%>%
  select(2:last_col())%>%
  #keep only data. basic cleaning. 
  slice(-c(1:3))%>%
  row_to_names(1)%>%
  clean_names()%>%
  
  #some data is imported as lists or as NULL. 
  #this fixes the issue. Convert all to character. 
  mutate(across(1:last_col(), as.character))%>%
  
  #then replace all "NULL" with NA. 
  mutate(across(1:last_col(), function(x){na_if(x, "NULL")}))->w96
 

############################
 
#get w96 label layout. 
w96%>%
  #gets labels entered in sheet. 
  slice(1:8)%>%
  
  #pivots data into tidy columns. 
  pivot_longer(2:last_col(), 
               names_to = "xcol",
               values_to = labels)->w96.layout


#############################

#get w96 measured values. 
w96%>%
  #takes only plates measurements entered in correct area. 
  slice(13:20)%>%
  
  #pivots all data into tidy columns. 
  pivot_longer(2:last_col(), 
               names_to = "xcol", 
               values_to = measured)%>%
  
  #changes measured values back to numeric from character. 
  mutate(across(3, function(x){as.numeric(x)}))->w96.measured

###########################

#join by plate location. 

#joins above datasets and removes any NA rows. 
left_join(w96.layout, w96.measured)%>%
  na.omit()->w96_all

#usable, tidy dataset now created. 
################################
################################
################################

#save to RDS file.
w96_all%>%
  saveRDS(file = paste0(as.character(today()),"-", plate_name, ".", "rds"))


#move to google sheet "output". 
#clear old data exported from R. 

range_clear("12NUnpmY9GzYT31EfhmvQFXkvBaSehO7420PTCk_eyTs",
            sheet = "output")

range_clear("12NUnpmY9GzYT31EfhmvQFXkvBaSehO7420PTCk_eyTs",
            sheet = "summary_stats")

#write new to output and summary sheets. 
w96_all%>%
  
  #arrange by column 3 and 4. 
  arrange(.[3], .[4])%>%
  range_write("12NUnpmY9GzYT31EfhmvQFXkvBaSehO7420PTCk_eyTs",
                          sheet = "output",
                          .,
                          range = "A1")


#creating summary stats df. 
#**only works if labels are repeated. 
w96_all%>%
  group_by_at(3)%>%
  summarise_at(3, .funs = funs(mean, sd))%>%
  rename_with(~paste0(measured, "_", "mean"), .cols = "mean")%>%
  rename_with(~paste0(measured, "_", "sd"), .cols = "sd")->w96_summary




#writing summary stats to sheets. 
w96_summary%>%
  range_write("12NUnpmY9GzYT31EfhmvQFXkvBaSehO7420PTCk_eyTs",
              sheet = "summary_stats",
              .,
              range = "A1")




