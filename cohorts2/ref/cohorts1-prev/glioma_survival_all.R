mouse_cohorts2 <- mouse_cohorts %>%
  mutate(injection_date = gsub('[(RE)]', '', mouse_cohorts$`Injection Date`))%>%
  mutate(injection_date = gsub('H', '', mouse_cohorts2$injection_date))%>%
  mutate(injection_date = as.Date.numeric(mouse_cohorts2$injection_date))



mouse_cohorts2 <- mouse_cohorts2 %>%
  as_tibble()%>%
  rename_all(., .funs = tolower)



mouse_cohorts2 <- mouse_cohorts2 %>%
  mutate(`injection_date` = as.Date(`injection_date`, format = "%m/%d/%Y"))
  


mouse_cohorts2 <- left_join(mouse_cohorts2, mouse_cohorts[,c("Mouse Number", "End Date")], by = c("mouse number" = "Mouse Number"))


mouse_cohorts3 <- mouse_cohorts2 %>% select(-'injection date', -"...19", -"End Date")

mouse_cohorts3 <- mouse_cohorts3 %>% filter(mouse_number != "Median")
 

mouse_cohorts4 <- mouse_cohorts3 %>%
  filter(!grepl("taken", mouse_number))


mouse_cohorts4 <- mouse_cohorts4 %>%
  relocate("injection_date", .after = "dob")

write.csv(mouse_cohorts4, "mouse_cohorts_clean1.csv", sep = ",")

mouse_cohorts <- mouse_cohorts4


mouse_cohorts <- mouse_cohorts %>%
  mutate(aod = death_date - dob)


mouse_cohorts <-
  mouse_cohorts %>%
  mutate(aod = coalesce(mouse_cohorts$aod, mouse_cohorts$end_date - mouse_cohorts$dob))
 

mouse_cohorts2 <- mouse_cohorts2 %>% mutate(strain_virus = paste(strain, virus, sep= "_"))
 


mouse_cohorts2 <- mouse_cohorts %>%
  mutate(aod_max = ifelse(aod > 150, 150, aod))

survival_mice_slice <- mouse_cohorts2 %>%
  select(mouse_number, strain_virus, aod_max)


view(survival_mice_slice)

view(mouse_cohorts2)
head(mouse_cohorts) 




survival_mice_slice <- survival_mice_slice %>% 
  mutate(aod_max_2 = ifelse(aod_max == 150, 160, aod_max))





Surv(survival_mice_slice$aod_max)




install.packages("survminer")
library(survminer)

fit <- survfit(Surv(time = aod_max_2)~strain_virus, data = survival_mice_slice)


df <- stepfun(fit$time, c(1, fit$surv))

df2 <- df(0:154)








ggsurvplot(fit,
  xlim = c(0, 150),
  break.x.by = 25,
  axes.offset = FALSE,
  legend = "right",
  ggtheme = theme_classic(),
  palette = c("locuszoom")
)


view(survival_mice_slice)

