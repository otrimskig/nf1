library(tidyverse)


scanned<-readRDS("slides/nf1_scanned_slides.rds")

snaps<-readRDS("slides/slide_snap_files.rds")

snaps2<-snaps%>%
  mutate(organ=if_else(!grepl("^.*-(?!\\d)", fn, perl=TRUE),"b", 
                       if_else(grepl("^.*-(?:t)", fn, perl=TRUE), "t",
                               if_else(grepl("^.*-(?:r)", fn, perl=TRUE), "b",
                       
                       
                       NA))))%>%
  mutate(stain=if_else(organ=="b","he",NA))%>%
  mutate(stain=if_else(grepl("pten", fn), "pten",
                if_else(grepl("ki67", fn), "ki67",
                if_else(grepl("s100", fn), "s100",
                if_else(grepl("olig2", fn), "olig2",
                if_else(grepl("gfap", fn), "gfap",
                if_else(grepl("perk", fn), "perk", stain)))))))%>%
  
  
  mutate(organ=if_else(is.na(organ),"b",organ))%>%
  mutate(stain=if_else(is.na(stain), "he", stain))%>%
  
  filter(fn!="Thumbs.db")%>%
  filter(!is.na(mouse_num))



scanned2<-scanned%>%
  mutate(organ=if_else(is.na(organ), "b", organ))%>%
  group_by(mouse_num, organ, stain)%>%
  slice(1)


scanned2%>%
  group_by(mouse_num, organ, stain)%>%
  count()%>%
  filter(n!=1)




checks<-left_join(snaps2, scanned2, by=c("mouse_num", "organ","stain"))%>%
  mutate(has_scan=if_else(!is.na(slidescan_path), "y", NA))%>%
  select(-slidescan_path)


saveRDS(checks, "slides/snap_scan_match.rds")

write_csv(checks, "slides/snap_scan_match.csv")
