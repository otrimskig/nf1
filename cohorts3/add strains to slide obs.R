library(tidyverse)
library(googlesheets4)
#authorize user
gs4_auth(email = "gotrimski@gmail.com")

#define sheet input/outputs here. 
#add the google docs id here (../d/<found here>/...)
docs_id<-"1Vr8u_WcXR-VIqRJc1MUXVQobB4HBGgnhg8LTLnsYwOQ"

#set name of sheet to use
name_of_sheet<-"all_slides"


#read input sheet
df1<-read_sheet(docs_id, 
                sheet = name_of_sheet)




cohort<-readRDS("ds/nf1_cohorts.3.dates.rds")%>%
  select(mouse_num, aod, resultant_geno, exclude)




df2<-df1%>%
  select(mouse_num)%>%
  left_join(cohort)






#save output at df2




range_write(docs_id,
            data=df2,
            sheet = name_of_sheet,
            range = "N1",
            
            #use or drop column names from output.
            col_names=TRUE,
            
            #does not overwrite existing formatting.
            reformat = FALSE)
