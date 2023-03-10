rm(list = ls())
setwd("~/RWorkspace/")

#load libraries 
library(tidyverse)
library(lubridate)

# Open a connection to the file
diri <- "/Users/tomokokoyama/Data/LDM/Soil_Moisture/"
location <- read.csv(paste0(diri,"SM_SoAf_location.csv"), header = TRUE)

fili <- list.files(path = diri, pattern = "^SM_SoAf_tp_.*.csv$")


for (j in 1:18303) {
  print(paste('File #: ',j))
  for (i in 1:length(fili)) {
    fili <- list.files(path = diri, pattern = "^SM_SoAf_tp_.*.csv$")
    tp <- read.csv(paste0(diri,fili[i]), header = TRUE)
    fili <- list.files(path = diri, pattern = "^SM_SoAf_sm1_.*.csv$")
    sm1 <- read.csv(paste0(diri,fili[i]), header = TRUE)
    fili <- list.files(path = diri, pattern = "^SM_SoAf_sm2_.*.csv$")
    sm2 <- read.csv(paste0(diri,fili[i]), header = TRUE)
    fili <- list.files(path = diri, pattern = "^SM_SoAf_sm3_.*.csv$")
    sm3 <- read.csv(paste0(diri,fili[i]), header = TRUE)
    fili <- list.files(path = diri, pattern = "^SM_SoAf_sm4_.*.csv$")
    sm4 <- read.csv(paste0(diri,fili[i]), header = TRUE)
    
    date <- as_tibble(make_date(year=substring(fili[i],13,16), month = c(1:12), day = 1)) %>%
      rename(ymd = value)
    
    bf <- t(bind_rows(tp[j,], sm1[j,], sm2[j,], sm3[j,], sm4[j,]))  %>%
      as_tibble() %>%
      bind_cols(date) %>%
      select(ymd, tp=V1, sm1=V2, sm2=V3, sm3=V4, sm4=V5)
    
    if (i==1) {
      bf_tot <- bf
    } else {
      bf_tot <- suppressMessages(full_join(bf_tot, bf))
    }
  }
  
  firo <- paste0("SM_SoAf_sample_",sprintf("%05s",j),".csv")
  write.csv(bf_tot, paste0(diri,firo), quote=FALSE, row.names=FALSE)
}
