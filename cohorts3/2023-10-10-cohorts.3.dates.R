library(tidyverse)


#from nf1_cohorts.2.dates to .3.dates RDS


#some additional dataset cleaning. 

nf1.2<-readRDS("ds/nf1_cohorts.2.dates.rds")

#finding instances with missing death_date_true,
#correcting data to ensure all mice have death_date_true. 
#adding metadata and exclusion note. 
nf1.2.a<-nf1.2%>%
  filter(is.na(death_date_true))%>%
  mutate(exclude = 4)%>%
  mutate(metadata = "missing death date. Used end date, even though tumor was noted.")%>%
  mutate(death_date_true = end_date)


#re-joining corrected data to dataset. 
nf1.3<-nf1.2%>%
  anti_join(nf1.2.a, by="mouse_num")%>%
  full_join(nf1.2.a)%>%
  
  #adding age-of-death (aod) column. 
  mutate(aod = as.numeric(death_date_true - injection_date))%>%
  relocate(injection_date, .after="dob")%>%
  relocate(aod, .after="death_date_true")%>%
  
  #correcting issues with experimental end date. 
  #any mouse survived >=150 days, had and experimental end date. exp_end_date. 
  #looked for clues in comments for mice younger than 150 days, 
  #that were sacced for end date time, and not for other reasons. 
  #mouse 25784 was 149 days post injection according to 1 obs, but 
  #another said it was 150. So very likely it was sacced for end date. 
  #only mice that were sacced for end dates should now have a date in
  #the exp_end_date column. The exception to this are mice that
  #will have to be excluded regardless, 3 mice that were
  #sacced for end date at 120 days, and/or have vastly conflicting data on 
#their respective death dates.

mutate(exp_end_date = if_else(aod>=150|
                                grepl("sacked for end date", g_comments, ignore.case=TRUE)|
                                grepl("sacked for endpoint", g_comments, ignore.case=TRUE)|
                                grepl("experimental end date", g_comments, ignore.case=TRUE)|
                                grepl("experimental endpoint", g_comments, ignore.case=TRUE)|
                                mouse_num=="25784", 
                              
                              death_date_true, NA))%>%
  
  relocate(exp_end_date, .after=aod)%>%
  
  #adding some additional found_dead data to 
  #column, where it was noted in the comments that mice
  #were found dead but not already noted in most current dataset. 
  mutate(found_dead = if_else(is.na(found_dead)&grepl("found dead", g_comments, ignore.case=TRUE), 1, found_dead))



#saving most current version with updates to ds/ folder. 
saveRDS(nf1.3, "ds/nf1_cohorts.3.dates.rds")




