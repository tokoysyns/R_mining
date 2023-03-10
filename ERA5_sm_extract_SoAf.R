rm(list = ls())
setwd("~/RWorkspace/")

#load libraries 
library(tidyverse)
library(lubridate)
library(ncdf4)

# Open a connection to the file
diri <- "/Users/tomokokoyama/Data/LDM/Soil_Moisture/"
fili <- list.files(path = diri, pattern = "^.*[0-9]\\.nc$")


i <- 1
nc <- nc_open(paste0(diri, fili[i]))

nc_lon  <- ncvar_get(nc, attributes(nc$dim)$names[1])
nc_lat  <- ncvar_get(nc, attributes(nc$dim)$names[2])

tp    <- ncvar_get(nc, attributes(nc$var)$names[1])
swvl1 <- ncvar_get(nc, attributes(nc$var)$names[2])
swvl2 <- ncvar_get(nc, attributes(nc$var)$names[3])
swvl3 <- ncvar_get(nc, attributes(nc$var)$names[4])
swvl4 <- ncvar_get(nc, attributes(nc$var)$names[5])

nc_close(nc)


# South Africa 
# Latitude -21.8 ~ -35.2, Longitude 16 ~ 33.2 
id_lon <- data.frame(lon=seq(0, 359.9, by = 0.1)) %>%
  mutate(id=row_number()) %>%
  filter(lon>= 16 & lon<=33.2) 

id_lat <- data.frame(lat=seq(90, -90, by = -0.1)) %>%
  mutate(id=row_number()) %>%
  filter(lat>= -35.2 & lat<=-21.8) 

along_lon <- as_tibble(seq(1:3600)) %>%
  mutate(tf_lon=ifelse(value %in% id_lon$id, TRUE, FALSE)) %>%
  select(tf_lon) 

along_lat <- as_tibble(seq(1:1801)) %>%
  mutate(tf_lat=ifelse(value %in% id_lat$id, TRUE, FALSE)) %>%
  select(tf_lat)


bf <- array(dim=c(3600,1801))    # 1 - target point
for (iy in 1:1801) {
  for (ix in 1:3600) {
    bf[ix, iy] <- along_lon$tf_lon[ix] && along_lat$tf_lat[iy]
  }
}

SoAf_mask <- bf * !is.na(tp[,,1])
num <- sum(SoAf_mask)

lon <- data.frame(seq(0, 359.9, by = 0.1))
lat <- data.frame(seq(90, -90, by = -0.1))  
loc <- NULL
k = 1
for (iy in 1:1801) {
  for (ix in 1:3600) {
    if (SoAf_mask[ix, iy]==1) {
      loc <- rbind(loc, cbind(lon[ix,],lat[iy,]))
      k = k+1
      }
  }
}

colnames(loc) = c("lon","lat")
write.csv(loc, paste0(diri,"SM_SoAf_location.csv"), quote=FALSE, 
          row.names=FALSE)

#-------------------------------------------------

bf_tp  <- array(0, dim = c(num, 12))
bf_sm1 <- array(0, dim = c(num, 12))
bf_sm2 <- array(0, dim = c(num, 12))
bf_sm3 <- array(0, dim = c(num, 12))
bf_sm4 <- array(0, dim = c(num, 12))

num_file = length(fili)
# The file 2022 does not cover 12 months

for (i in 1:num_file) {    
  nc <- nc_open(paste0(diri, fili[i]))
  
  nc_lon  <- ncvar_get(nc, attributes(nc$dim)$names[1])
  nc_lat  <- ncvar_get(nc, attributes(nc$dim)$names[2])
  nc_time <- ncvar_get(nc, attributes(nc$dim)$names[3])
  
  # ncatt_get(nc,'time')
  timestamp <- data.frame(time=as_datetime(c(nc_time*60*60),origin="1900-01-01")) %>%
    mutate(nd=as.numeric(days_in_month(time)))
  
  # Read data
  tp    <- ncvar_get(nc, attributes(nc$var)$names[1])
  swvl1 <- ncvar_get(nc, attributes(nc$var)$names[2])
  swvl2 <- ncvar_get(nc, attributes(nc$var)$names[3])
  swvl3 <- ncvar_get(nc, attributes(nc$var)$names[4])
  swvl4 <- ncvar_get(nc, attributes(nc$var)$names[5])
  
  nt <- length(nc_time)
  nc_close(nc)
  
  k <- 1
  for (iy in 1:nrow(id_lat)) {
    for (ix in 1:nrow(id_lon)) {
      if (SoAf_mask[id_lon$id[ix], id_lat$id[iy]] ==1) {
        bf_tp[k,] = tp[id_lon$id[ix], id_lat$id[iy],]
        bf_sm1[k,] = swvl1[id_lon$id[ix], id_lat$id[iy],]
        bf_sm2[k,] = swvl2[id_lon$id[ix], id_lat$id[iy],]
        bf_sm3[k,] = swvl3[id_lon$id[ix], id_lat$id[iy],]
        bf_sm4[k,] = swvl4[id_lon$id[ix], id_lat$id[iy],]
        k <- k+1
      }
    }
  }
  
  firo <- paste0("SM_SoAf_tp_",substr(fili[i], 25, 28),".csv")
  write.csv(bf_tp, paste0(diri,firo), quote=FALSE, 
            row.names=FALSE, col.names = FALSE)
  
  firo <- paste0("SM_SoAf_sm1_",substr(fili[i], 25, 28),".csv")
  write.csv(bf_sm1, paste0(diri,firo), quote=FALSE, 
            row.names=FALSE, col.names = FALSE)
  
  firo <- paste0("SM_SoAf_sm2_",substr(fili[i], 25, 28),".csv")
  write.csv(bf_sm2, paste0(diri,firo), quote=FALSE, 
            row.names=FALSE, col.names = FALSE)
  
  firo <- paste0("SM_SoAf_sm3_",substr(fili[i], 25, 28),".csv")
  write.csv(bf_sm3, paste0(diri,firo), quote=FALSE, 
            row.names=FALSE, col.names = FALSE)
  
  firo <- paste0("SM_SoAf_sm4_",substr(fili[i], 25, 28),".csv")
  write.csv(bf_sm4, paste0(diri,firo), quote=FALSE, 
            row.names=FALSE, col.names = FALSE)
  
  rm(nc, tp, swvl1, swvl2, swvl3, swvl4)
}
