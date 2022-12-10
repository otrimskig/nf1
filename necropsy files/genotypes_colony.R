library(tidyverse)
library(janitor)
library(stringr)

read_csv("genotypes_joined.csv")->genotypes_joined

genotypes_joined%>%
  mutate(mouse_num2 = mouse_num)%>%
  mutate(mouse_num = substring(mouse_num, 1,5))->genotypes_joined2



read_csv("colony_necropsy5.csv")%>%
  group_by(mouse_num)%>%
  slice(1)%>%
  ungroup()%>%
  mutate(mouse_num = as.character(mouse_num))%>%
  select(mouse_num)%>%
  left_join(genotypes_joined2, by = "mouse_num")%>%
  select(where(function(x) any(!is.na(x))))%>%
  write_csv("colony_genotypes.csv")



####################################################################

read_csv("colony_genotypes.csv")%>%
  select(mouse_num,
         index,
         contains("atrx"),
         contains("cre"),
         contains("pten"),
         contains("tva"),
         contains("cas9"),
         strain)%>%
  mutate(h11_cas9 = ifelse(is.na(h11lsl_cas9), cas9, h11lsl_cas9))%>%
  select(-h11lsl_cas9, -cas9)%>%
  
  mutate(pten = ifelse(is.na(pten), pten_flox, pten))%>%
  select(-pten_flox)->col_gen
  
  paste0("g_", colnames(col_gen))->col_gen_names
  
  
  col_gen%>%
    rename_all(~col_gen_names)->col_gen2
  
  
#############################################################

read_csv("colony_necropsy5.csv")%>%
  #tva, h12, pten, ink, atrx, tva_status, strain, tyr_cre
  left_join(col_gen2, by = c("mouse_num"="g_mouse_num"))%>%
  
  
    
  #uniting tyr_cre. 
  mutate(tyr_cre = ifelse(is.na(tyr_cre), g_tyr_cre, 
                    ifelse(tyr_cre != g_tyr_cre, g_tyr_cre,  
                          tyr_cre)))%>%
  #removing now-unecessary columns and updating tva column.
  select(-tyr_cre)%>%
  select(-tva_status)%>%
  rename(n_tva = g_n_tva)%>%
  select(-tva)->colony_necropsy5.1
    
    
  ##############################
  colony_necropsy5.1%>%
  select(mouse_num, 
         atrx, 
         g_atrx, 
         strain,
         g_strain)%>%
 
    
    group_by(mouse_num)%>%
    slice(1)%>%
    mutate(g_atrx = gsub("^\\+/\\+", "ATRX \\+/\\+", g_atrx))%>%
 
    mutate(g_atrx = gsub ("^Flox/Flox", "ATRX f/f", ignore.case = TRUE, g_atrx))%>%
    
    filter((atrx != g_atrx)|is.na(g_atrx))%>%
  
    write_csv("atrx_checks.csv")
    
  
  #############################################
  
read_csv("atrx_checks.m.csv")%>%
  
  rename(atrx.m = atrx)%>%
  right_join(colony_necropsy5.1)%>%
 
 mutate(atrx.m = ifelse(is.na(atrx.m), atrx, atrx.m))%>%
select(-atrx, -g_atrx)%>%
 
 rename(atrx = atrx.m)%>%
  rename(h11 = h12)%>%
 
   select(-g_h11_cas9)->colony_necropsy5.2
  
  
  
#################
  colony_necropsy5.2%>%
  select(mouse_num, g_tyr_cre, n_tva, g_strain)%>%
  group_by(mouse_num)%>%
  slice(1)%>%
  ungroup()%>%
  write_csv("check_geno_multi1.csv")

#######################


read_csv("check_geno_multi1.m.csv")%>%
  
  right_join(colony_necropsy5.2, by = "mouse_num")%>%
  select(!ends_with(".y"))%>%
  select(-g_pten)%>%
  select(-strain)%>%
  
  rename(g_tyr_cre = g_tyr_cre.x, ntva = n_tva.x, g_strain = g_strain.x)%>%
  relocate(ntva, .after = g_strain)%>%
  select(-g_index)%>%
  
  mutate(ntva = ifelse(grepl("prboably Tg/+", ntva), "likely Tg/+", ntva))%>%
  mutate(ntva = ifelse(grepl("probably Tg/+", ntva), "likely Tg/+", ntva))->colony_necropsy5.3

##################################################################
  
colony_necropsy5.3%>%
  group_by(mouse_num)%>%
    
 #using local cells to fill missing information.
  #since df is grouped, fill will only apply to each mouse num separately.
    fill(c(patho_location, 2:5, 10,12:22, 26, 28, 35:64, 68:92, injected_by, mouse_tag, cells_injected,
           tis_fix_etoh,
           tis_submitted,
           sheri_saw_he), .direction = "down")%>%
    
    fill(c(patho_location, 2:5, 10,12:22, 26, 28, 35:64, 68:92, injected_by, mouse_tag, cells_injected,
           tis_fix_etoh,
           tis_submitted,
           sheri_saw_he), .direction = "up")->colony_necropsy5.4
  
   



##############################################################################

read_csv("colony_necropsy5.csv")%>%
  view()
  
  
  
