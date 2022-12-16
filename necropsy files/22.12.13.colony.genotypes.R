#########################
#cleaning genotyping information for colony from genotyping file.

library(tidyverse)
library(janitor)
library(beepr)
library(stringr)

read_csv("mouse_numbers.csv")->mouse_numbers
#315 mice in colony.

#reading in all genotypes. saving as RDS for future ease.


# read_csv("genotypes_joined2.csv")%>%
# saveRDS("genos_flattened.rds")
#commenting out that portion. 
#now can open from the RDS file. 


readRDS("genos_flattened.rds")->genos_flattened1 


#get column with only mouse numbers. 
#retain column with other id information. 

genos_flattened1%>%
  mutate(mouse_id = mouse_num)%>%
  mutate(mouse_num = as.numeric(substr(mouse_id, 1,5)))->genos_flattened1.1


#use mouse_numbers to filter genotypes. Retain only those existing in colony.
genos_flattened1.1%>%
semi_join(mouse_numbers, by = "mouse_num")%>%
  select(where(function(x) any(!is.na(x)))

       )->colony_genotypes

#check that there are no repeats and all mice in my colony show up here.
#previous issues were found with some repeats in the original genotyping file. 
#fixed some in previous iterations; fixed remaining issues manually in genotypes_joined2.csv.
#accurate changes reflected now in rds genos_flattened file. 

colony_genotypes%>%
  count(mouse_num)%>%
  view()


#cage info in dataset that was incorrectly parsed.
read_csv("mice24801cagemanual.csv")%>%
  mutate(mouse_id = mouse_num)%>%
  mutate(mouse_num = as.numeric(substr(mouse_id, 1,5)))->manual_edits



colony_genotypes%>%
  #removing incorrect/missing data to be replaced.
  anti_join(manual_edits, by = "index")%>%
  #adding replacement data.
  full_join(manual_edits)%>%
  
  select(where(function(x) any(!is.na(x))))%>%
  select(-c(sex, death_date, dispos, dob))%>%
  clean_names()%>%
  relocate(mouse_id, strain, .after = mouse_num)%>%
  unite("pten", contains("pten"), na.rm = TRUE)%>%
  unite("ink4a", contains("ink"), na.rm = TRUE)%>%
  unite("atrx", contains("atrx"), na.rm = TRUE)%>%
  unite("h11cas9", contains("cas9"), na.rm = TRUE)%>%
  unite("h11cas9", contains("h11"), na.rm = TRUE)%>%
  unite("ntva", contains("n_tva"), na.rm = TRUE)%>%
  
  
  
  #change all blank cells and NAs to "pwt" - presumed wild types.
  mutate_all(function(x)gsub("^$", "pwt", x))%>%
  mutate_all(function(x)ifelse(is.na(x), "pwt", x))%>%
  
  
  #checking for internal consistencies in genotype formatting.
  
  #pten checked.
  #ntva edits.
  mutate(ntva = gsub("prboably", "likely", ntva))%>%
  mutate(ntva = gsub("probably", "likely", ntva))%>%
  #ink4a checked.
  #atrx checked.
  #h11cas9 checked.
  #cic checked.
  #tyr_cre checked.
  #dtva rename.
  rename(dtva = d_tva)%>%
  #dtva checked.
  
  
  mutate_all(function(x)gsub("^Flox/Flox$", "f/f", x))%>%
  
  #everything to lowercase; looks a bit cleaner.
  mutate_all(.fun= tolower)%>%
  
  
  #strain column doesn't always match up with genotype. 
  #is actually unnecessary and at this point, confusing.
  #keep in the saved version, but will delete for downstream re-integrations.
  #will delete and re-make a better version when combining with all cohort data. 
  relocate(ntva, pten, ink4a, atrx, h11cas9, tyr_cre, cic, .after = "strain")%>%
  
  #renaming the strain variable to avoid confusion.
  #cage_strain is the name of the strain of the cage, not necessarily the 
  #genotype of the actual mouse, per se. This makes the info
  #a bit clearer I think. 
  rename(cage_strain = strain)%>%
  
  saveRDS("colony_genos_clean.rds")


##############################################################
  

