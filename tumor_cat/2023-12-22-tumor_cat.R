#R input using Data to R sheet
library(googlesheets4)
library(tidyverse)
library(janitor)
library(lubridate)



#authorize user
gs4_auth(email = "gotrimski@gmail.com")

#read input sheet
df<-read_sheet("1Vr8u_WcXR-VIqRJc1MUXVQobB4HBGgnhg8LTLnsYwOQ", 
                      sheet = "updated_slides")%>%
  
  mutate(across(1:last_col(), as.character))%>%
  
  #then replace all "NULL" with NA. 
  mutate(across(1:last_col(), function(x){na_if(x, "NULL")}))%>%
  
  clean_names()



df2<-df%>%
  filter(!is.na(tumor1))%>%
  relocate(tumor1,tumor2,t1_size_mm2,t2_size_mm2)%>%
  rename(tumor_1 = tumor1, tumor_2 = tumor2, size_1=t1_size_mm2, size_2=t2_size_mm2)%>%



  pivot_longer(cols=tumor_1:size_2, names_to=c(".value","num"), names_sep="_", values_to =  c("tumor", "size"))%>%
  
  
  select(-num)%>%
  
  filter(!is.na(tumor))%>%
  
  relocate(tumor, size, .after = "stain")%>%
  
  
  mutate(size=as.numeric(size))%>%
  
  
  relocate(send_for_rna, annotated_mrxs, aod, resultant_geno, x, .after="size")%>%
  

  
  select(-c("blue":last_col()))



saveRDS(df2, "categorized_and_sized_tumors_on_scanned_slides.rds")












gs4_auth(email = "gotrimski@gmail.com")

#read input sheet
slide_locs<-read_sheet("1_Kk3q0OkVNOFQ64izfFjL-YFCHMwSEo-pESf5rICrek", 
               sheet = "Sheet1")%>%
  mutate(across(1:last_col(), as.character))%>%
  
  #then replace all "NULL" with NA. 
  mutate(across(1:last_col(), function(x){na_if(x, "NULL")}))%>%
  
  clean_names()



slide_locs2<-slide_locs%>%
  select(mouse_num, organ, stain, has_scan, scan_box, scan_box_slot_2)



df3<-left_join(df2, slide_locs2, by=c("mouse_num", "organ", "stain"))%>%
  
  mutate(slot2=scan_box_slot_2)%>%
  select(-scan_box_slot_2)%>%

  mutate(current_loc = if_else(is.na(scan_box), box, scan_box))%>%
  
  mutate(current_slot = if_else(is.na(slot2), slot, slot2))%>%
  
  
  select(-c("box", "slot", "has_scan", "scan_box", "slot2"))%>%
  
  
  arrange(tumor, desc(size))






types<-df3%>%
  group_by(tumor)%>%
  add_count()%>%
  
  summarize(total_sa=sum(size),
            mean=mean(size),
            n=mean(n))


write_csv(types,"types.csv")





  