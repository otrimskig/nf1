library(tidyverse)
library(janitor)
library(stringr)
library(lubridate)

#make slide names dataset from 

files <- list.files(path = "E:/Slides", 
                    full.names = TRUE, 
                    recursive = TRUE, 
                    pattern = "*\\.mrxs$", ignore.case = TRUE)

files2<- list.files(path = "E:/CaseViewer Files", 
                    full.names = TRUE, 
                    recursive = TRUE, 
                    pattern = "*\\.mrxs$", ignore.case = TRUE)

files3<- c(files, files2)

files3%>%
  as_tibble()%>%
  clean_names()%>%
  rename(path = value)%>%
  mutate(filename = gsub(".*\\/", "", path))%>%
  relocate(filename)%>%
  mutate(filename = gsub(".mrxs", "", filename))%>%
  mutate(sample_name = "")%>%
  relocate(sample_name, .after = filename)%>%
  write_csv("slide_names_raw.csv")

#used dataset to go through all slide files, using caseviewer.
#annotated file based on slide label/names, added manually to slide_names dataset.

slide_names<-
read_csv("slide_names.csv")%>%
  mutate(folder = str_replace(path, filename, ""))%>%
  mutate(folder = str_replace(folder, ".mrxs", ""))
  
folder_index<-
slide_names%>%
  distinct(folder)%>%
  mutate(index = 1:20)

slide_names<-
slide_names%>%
  inner_join(folder_index)

slide_names%>%
  clean_names()%>%
  mutate(staining = NA)%>%
  mutate(staining = case_when(grepl("GFAP", sample_info, ignore.case = TRUE)~ "GFAP",
                              grepl("PTEN", sample_info, ignore.case = TRUE)~ "PTEN",
                              grepl("Ki67", sample_info, ignore.case = TRUE)~ "Ki67",
                              grepl("Ki 67", sample_info, ignore.case = TRUE)~ "Ki67",
                              grepl("Ki 67", sample_info, ignore.case = TRUE)~ "Ki67",
                              grepl("Olig2", sample_info, ignore.case = TRUE)~ "Olig2",
                              grepl("synaptophysin", sample_info, ignore.case = TRUE)~ "Synaptophysin",
                              grepl("PERK", sample_info, ignore.case = TRUE)~ "PERK"))%>%
  
#use anything containing H&E to mark that case as an HE stain, unless otherwise noted. 
  mutate(staining = case_when(is.na(staining)& grepl("H&E", folder, ignore.case = TRUE)~"H&E",
                              TRUE~as.character(staining)))%>%
  mutate(staining = case_when(is.na(staining)& folder == "E:/CaseViewer Files/NF1 Glioma IHC Figure Files/"~"H&E",
                              TRUE~as.character(staining)))%>%
#checked slide images manually. anything not already labeled is an HE stain.  
  mutate(staining = case_when(is.na(staining)~"H&E",
                              TRUE~as.character(staining)))%>%
  
#extracting img date from file name. 
  mutate(img_date = as.Date(substr(filename, 7,16)))%>%
#renaming to make variable clearer.
  rename(slide_date = date)%>%

#creating new variable with who prepped the slides. 
#information is contained in a combination of the slide labels,
#or inferred from the date they were taken (ie if others done in the same session were labeled)
#first create column  
  mutate(prep_by = NA)%>%
#take any information in sample info relating to prep person, get it into the prep_by column.
  mutate(prep_by = case_when(grepl("TE", sample_info)~"TE",
                        #anything in a specific person's folder
                             grepl("rick", folder, ignore.case = TRUE)~"RH",
                             grepl("riley", folder, ignore.case = TRUE)~"RE",
                             grepl("tori", folder, ignore.case = TRUE)~"TE",
                        #imgs taken in same session as others that were labeled.     
                             img_date == "2022-04-06"~"RE",
                             img_date == "2022-04-22"~"RE",
                             img_date >= "2022-05-20"~ "TE",
                             TRUE~as.character(prep_by)))%>%

#renaming to avoid confusion.
  rename(slide_label = sample_info)%>%

#dates:
#cleaning up dates. Adding them from where some were contained in the slide label,
#or inproperly formatted in the slide_date column.
  mutate(slide_date2 = case_when(grepl("5/10", slide_label)~"2022-05-10",
                                 grepl("5/12", slide_label)~"2022-05-12",
                                 grepl("612", slide_label)~"2022-06-12",
                                 grepl("6/22", slide_label)~"2022-06-22",
                                 slide_date =="7/27"~"2022-07-27"))%>%
  
#Extracting all data from s_d to s_d2 without overwriting any new info. 
  mutate(slide_date2 = case_when(is.na(slide_date2)~slide_date,
                                 TRUE~as.character(slide_date2)))%>%
  relocate(slide_date2, .after = slide_date)%>%
  
#manually removing data that wasn't a date.  
  mutate(slide_date2 = case_when(slide_date2 == "Rb 8/"~ NA_character_,
                                 TRUE~as.character(slide_date2)))%>%
#removing TE from any moved data/dates, trimming whitespace thereafter. 
  mutate(slide_date2 = trimws(gsub("TE", "", slide_date2)))%>%


#manually adding years to dates, and re-formatting them.  
  mutate(slide_date2 = case_when(grepl("5/2", slide_date2)~"2022-05-02",
                                 grepl("5/3", slide_date2)~"2022-05-03",
                                 grepl("5/10", slide_date2)~"2022-05-10",
                                 grepl("5/12", slide_date2)~"2022-05-12", 
                                 grepl("6/22", slide_date2)~"2022-06-22",
                                 grepl("7/27/22", slide_date)~"2022-07-27",
                                 slide_date2 == "-"~NA_character_,
                                 TRUE~as.character(slide_date2)))%>%
#creating helper column, moving date values and turning them into properly formatted dates.
  mutate(slide_date3 = as.Date(case_when(img_date>"2022-05-20"~slide_date2,
                                         TRUE~NA_character_)))%>%
#same
  mutate(var1 = ifelse(img_date<"2022-05-20",slide_date2,
                         NA))%>%
#moving those values to dates. 
  mutate(var1 = as.Date(mdy(var1)))%>%
#uniting the date columns. 
  mutate(slide_date3 = case_when(is.na(slide_date3)~var1,
                                 TRUE~slide_date3))%>%
#cleaning up. Deleting temp columns and renaming the new, cleaned column as slide_date.   
  select(-slide_date, -slide_date2, -var1)%>%
  rename(slide_date = slide_date3)%>% 
###
  
  
#getting consistent nomenclature for tumor type. 
  mutate(tumor = case_when(tumor=="BR"~"B",
                           TRUE~as.character(tumor)))%>%
#removing mikaela's initials from tumor type. 
  mutate(tumor = trimws(gsub("MF", "", tumor)))%>%
#manual change of mistakenly entered 6-digit mouse number.   
  mutate(mouse_num = case_when(path == "E:/CaseViewer Files/ihc nf1 glioma/slide-2022-06-07T13-07-50-R1-S2.mrxs"~"26280",
                                TRUE~as.character(mouse_num)))%>%
#housekeeping and data export.
  select(-index)%>%
  relocate(filename, .after=folder)%>%
  relocate(staining, .after = tumor)%>%
  relocate(slide_label, slide_number, .after = slide_date)%>%

  write_csv("slide_info.csv")
  


############################################################################
#############################################################################
#now looking for necropsy files matching mice in the slide info. 


#create df with necropsy files and their respective mouse numbers.
#get list (df) of necropsy files with pathnames. 

necropsy<-
list.files(path = "X:/Holmen Lab/Necropsy files", 
           full.names = TRUE, 
           recursive = TRUE, 
           ignore.case = TRUE)%>%
  as_tibble()

necropsy%>%
  mutate(mouse_num = as.character(str_extract_all(value, "[0-9]{5}")))%>%
  filter(nchar(mouse_num)==5)%>%
  rename(necropsy_file_path = value)%>%
  mutate(necropsy_file_path = gsub("\\~\\$", "", necropsy_file_path))%>%
 
   write_csv("necropsy_file_paths.csv")

#joining created df to slide_info dataset. 
left_join(read_csv("slide_info.csv"), read_csv("necropsy_file_paths.csv"))%>%
  
  write_csv("slide_info2.csv")

############################
#trying to extract info from necropsy files. 
#1st step copying relevant files to convenient directory, so originals are unchanged.

nec_file_copy<-
read_csv("slide_info2.csv")%>%
  select(necropsy_file_path)%>%
  mutate(nec_filename = gsub(".*\\/", "", necropsy_file_path))%>%
 
     #rest of this ends up being unnecessary - at least for now for moving the files. 
    #only the destination directory is needed to copy files. 
  mutate(new_file_path = paste0("necropsy/", nec_filename))%>%
  select(1, 3)%>%
  rename(nec_old_path = necropsy_file_path, nec_new_path = new_file_path)


#Copying files - 66 files copied. 
file.copy(from=nec_file_copy%>%select(nec_old_path)%>%drop_na()%>%pull(), 
          to="necropsy/", 
          overwrite = TRUE, 
          recursive = TRUE, 
          copy.mode = TRUE)


######
#changing files to csvs. 


library(rio)

xlsx<- paste0("necropsy/", dir("necropsy/", pattern = "xlsx"))
created <- mapply(convert, xlsx, gsub("xlsx", "csv", xlsx))



xls<- 
paste0("necropsy/", dir("necropsy/", pattern = "xls"))%>%
  as_tibble()%>%
  filter(!grepl("xlsx", value))%>%
  pull()

created <- mapply(convert, xls, gsub("xls", "csv", xls))


#############################

#getting all csvs into proper form.



#single-use conversion. Then converted into for loop.
read.csv("necropsy/Mouse Necropsy HCI -20-23967.csv")%>%
  as_tibble()%>%
  clean_names()%>%
  mutate(x2=paste(x2, x3))%>%
  select(1,2)%>%
  data.frame()%>%
  t()%>%
  as_tibble()%>%
  row_to_names(1)%>%
  clean_names()%>%
  mutate_all(as.character)%>%
  str()
  view()


files2<-list.files("./necropsy/")
#tryCatch used to silence errors and continue the loop.
for(i in 1:length(files2)){
  tryCatch({
  read.csv(paste0("necropsy/",files2[i]))%>%
    as_tibble()%>%
    clean_names()%>%
    mutate(x2=paste(x2, x3))%>%
    select(1,2)%>%
    data.frame()%>%
    t()%>%
    as_tibble()%>%
    row_to_names(1)%>%
    clean_names()%>%
    mutate_all(as.character)%>%
    write_csv(paste("nec2/",files2[i]))
    }, error=function(e){})
  }


#checking file numbers are the same.
list.files("./nec2/")
list.files("./necropsy/")



library(tidyverse)
csvs<-paste0("nec2/", list.files("./nec2/"))
csvs



for(i in 1:length(csvs)){
  assign(paste0("necropsy_file", i),  
    read_csv(csvs[i])%>%
      mutate_all(as.character))
  }


#getting list of necropsy datasets in environment. 
df_list = mget(ls(pattern = "necropsy_file[0-9]"))
df_list

all_necropsy<-
reduce(df_list, full_join)

library(lubridate)



all_necropsy%>%
  mutate(todays_date = as.Date(as.numeric(todays_date), origin = "1899-12-30"))%>%
  mutate(mouse_num = substr(mouse_id, 1, 5))%>%
  relocate(mouse_num)%>%
  #looks for columns containing only NAs and removes them.
  select(where(~!all(is.na(.x))))%>%
  select(-mouse_id)%>%
  unite(disposition, c(disposition, disposition_2, disposition_3, disposition_4), na.rm = TRUE, sep=",")%>%
  mutate(sex = case_when(grepl("f", sex, ignore.case = TRUE)~"F",
                         TRUE~"M"))%>%
  #birth date
  mutate(bd = substr(bd, 1,5))%>%
  mutate(dob = as.Date(as.numeric(bd), origin = "1899-12-30"))%>%
  select(-bd)%>%
  
  #sac date
  mutate(sac_date = as.Date(as.numeric(sac_date), origin = "1899-12-30"))%>%
 
  mutate(injection_date1 = injection_date)%>%
  mutate(injection_date2 = NA)%>%
  select(-injection_date)%>%
  mutate(injection_date1b = ifelse(nchar(injection_date1)==5,injection_date1, 
                                 NA))%>%
  mutate(injection_date1b =  as.Date(as.numeric(injection_date1b), origin = "1899-12-30"))%>%
  
  mutate(injection_date2 = gsub(".*\\,", "", injection_date1))%>%
  mutate(injection_date2 = ifelse(nchar(injection_date2)==5,NA, 
                                  injection_date2))%>%
  mutate(injection_date2 = mdy(injection_date2))%>%
  
  mutate(injection_date1c = gsub("\\,.*", "", injection_date1))%>%
  mutate(injection_date1c = ifelse(nchar(injection_date1c)==5,NA, 
                                  injection_date1c))%>%
  mutate(injection_date1c = mdy(injection_date1c))%>%
  
  
  mutate(injection_date1 = NA)%>%
  mutate(injection_date1 = case_when(is.na(injection_date1b)==FALSE~injection_date1b,
                                     TRUE~injection_date1c))%>%
  select(-injection_date1b, -injection_date1c)%>%

  
  unite(additonal_comments, c(additional_comments, x_2, x_3), na.rm = TRUE, sep=",")%>%
  
  
  #if date_tumor_reported is 5 char, ie easily cleanable, move to tumor_date.
  mutate(tumor_date = ifelse(nchar(date_tumor_reported)==5, date_tumor_reported, NA))%>%
  mutate(tumor_date =as.Date(as.numeric(tumor_date), origin = "1899-12-30"))%>%
  
  #separate other entries into tumor_date2 column.
  mutate(tumor_date2 = ifelse(nchar(date_tumor_reported)>5, date_tumor_reported, NA))%>%
  select(-date_tumor_reported)%>%

 
  #get "dates" prior to comments into date2 column.
  mutate(date2 = gsub("\\ .*", "", tumor_date2))%>%
  
  #get "comments" after first space (date) into a new comment column.
  mutate(comment2 = gsub("^\\S+\\s+", '', tumor_date2))%>%
  
  #separate easily cleanable dates into date3 column. 
  #keep only other formats in date2 column.
  #clean date3 into dates and merge into tumor_date column.
  mutate(date3 = ifelse(nchar(date2)==5, date2, NA))%>%
  mutate(date3 = as.Date(as.numeric(date3), origin = "1899-12-30"))%>%
  
  mutate(date2 = ifelse(nchar(date2)==5,NA, date2))%>%
  mutate(date2 = mdy(date2))%>%
  #unite all date columns into tumor_date.
  unite(tumor_date, c(tumor_date, date2, date3), na.rm = TRUE)%>%
  select(-tumor_date2)%>%
  unite(evidence_of_tumor, c(evidence_of_tumor, comment2), na.rm = TRUE, sep=",")%>%
  write_csv("slides_necropsy.csv")
  
  
  view()
  
 









write_csv(all_necropsy, "all_necropsy.csv")
