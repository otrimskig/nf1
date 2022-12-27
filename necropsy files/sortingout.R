library(tidyverse)


list.files(pattern = ".R")%>%
  as_tibble()%>%
  write_csv("sort.csv")
