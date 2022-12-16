pathPrep <- function(path = "clipboard") {
  y <- if (path == "clipboard") {
    readClipboard()
  } else {
    cat("Please enter the path:\n\n")
    readline()
  }
  x <- chartr("\\", "/", y)
  writeClipboard(x)
  return(x)
}




###########################################2###
#main

library(tidyverse)
library(janitor)
library(stringr)

library(fuzzyjoin)

library(tools)
library(readr)
library(fs)

library(googlesheets4)

library(readxl)
library(rio)

##############################################


##colors and other packages for plots
library(ggsci)

library(viridis)

library(RColorBrewer)

library(survival)


#############################################






























home<-"X:/Holmen Lab/Lab Personnel/Garrett/R/"
setwd(home)
###########################################

install.packages("clipr")
install.packages("googlesheets4")

install.packages("tidyverse")
library(tidyverse)

install.packages("ggplot2")
library(ggplot2)

install.packages("janitor")
library(janitor)

install.packages("survival")
library(survival)

install.packages("RColorBrewer")
library(RColorBrewer)

install.packages("viridis")
library(viridis)

install.packages("ggsci")
library(ggsci)

install.packages("fuzzyjoin")
library(fuzzyjoin)

install.packages("fs")
library(fs)

install.packages("rio")

install.packages("installr")

library(installr)

updateR()

