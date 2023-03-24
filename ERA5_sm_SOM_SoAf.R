rm(list = ls())
setwd("~/RWorkspace/")

#load libraries 
library(tidyverse)
library(lubridate)

# Open a connection to the file
diri <- "/Users/tomokokoyama/Data/LDM/Soil_Moisture/"
fili <- list.files(path = diri, pattern = "^SM_SoAf_sample_")

all <- NULL

for (i in 1:length(fili)) {
  if (i%%100 == 0) {
    print(paste(i,"/18303"))
  }
  tbl <- as_tibble(read_csv(paste0(diri,fili[i]),
                            show_col_types = FALSE))　
  
  tmp1 <- tbl %>%
    summarise(mntp=mean(tp),
              mn4=mean(sm4), 
              ts4=mean(sm4)+2*sd(sm4), 
              cv4=sd(sm4)/mean(sm4)) %>%
    mutate(fq4=sum(tbl$sm4>ts4))
  all <- bind_rows(all, tmp1)
  
  tmp2 <- tbl %>%
    mutate(month=month(ymd)) %>%
    group_by(month) %>%
    summarise(mn_sm1=mean(sm1), sd_sm1=sd(sm1),
              mn_sm2=mean(sm2), sd_sm2=sd(sm2),
              mn_sm3=mean(sm3), sd_sm3=sd(sm3),
              mn_sm4=mean(sm4), sd_sm4=sd(sm4)) %>%
    pivot_longer(cols = c("mn_sm1", "mn_sm2", "mn_sm3", "mn_sm4",
                          "sd_sm1", "sd_sm2", "sd_sm3", "sd_sm4"),
                 names_to = "index",
                 values_to = paste0("file",sprintf("%02s",i)))
  
  if (i==1) {
    mon <- tmp2
  } else {
    mon <- full_join(mon, tmp2, by=c("month", "index"))
  }
  rm(tmp1, tmp2)
}


write.csv(all, paste0(diri,"South_Africa_SM_all.csv"), quote=FALSE, row.names=FALSE)

# data from 339 locations
#----------------
#　https://rpubs.com/AlgoritmaAcademy/som
library(kohonen)

# select "mean total precipitation", "mean soil moisture 4",
# "cv of sm4", and counts larger than 2 sigma.
all.s <- all %>%
  select(mntp, mn4,cv4, fq4)

# make a train data sets that scaled and convert them to be a matrix 
# cause kohonen function accept numeric matrix
all.train <- as.matrix(scale(all.s))

# make a hexagonal SOM grid
# hexagonal grids are preferred since each node then has 6 immediate neighbours.
# 339/(5x5) = 13.56 <- counts/node
# 173/(4x4) = 10.81
set.seed(100)
all.grid <- somgrid(xdim = 6, ydim = 6, topo = "hexagonal")

# make a SOM model
set.seed(100)
all.model <- som(all.train, 
                 grid = all.grid, 
                 rlen = 3000,               # times to present data
                 alpha=c(0.05,0.01),       # learning rate
                 keep.data = TRUE)

#Training progress for SOM
png("plot1.png", width = 500, height = 500)
plot(all.model, type="changes")
dev.off() 

#Node count plot
plot(all.model, type="count", main="Node Counts")

# U-matrix visualisation
plot(all.model, type="dist.neighbours", main = "SOM neighbour distances")

# Weight Vector View
plot(all.model, type="codes", main = "Codes Plot", palette.name = rainbow)


# Kohonen Heatmap creation
i <- 4
plot(all.model, type = "property", property = getCodes(all.model)[,i], 
     main = colnames(getCodes(all.model))[i]) 

#heatmap.som <- function(model){
#  for (i in 1:4) {
#    plot(all.model, type = "property", property = getCodes(all.model)[,i], 
#         main = colnames(getCodes(all.model))[i]) 
#  }
#}
#heatmap.som(all.model)


# Clustering
library(factoextra)

set.seed(100)
fviz_nbclust(all.model$codes[[1]], kmeans, method = "wss") 

set.seed(100)
clust <- kmeans(all.model$codes[[1]], 6)

plot(all.model, type = "codes", bgcol = rainbow(9)[clust$cluster], main = "Cluster Map")
add.cluster.boundaries(all.model, clust$cluster)

# know cluster each data (this goes to the table)
all.cluster <- data.frame(all.s, cluster = clust$cluster[all.model$unit.classif]) %>%
  group_by(cluster) %>%
  summarise(n=n(), mntp=mean(mntp), mn4=mean(mn4), cv4=mean(cv4), fq4=mean(fq4))

#plot(all.model, "quality", whatmap = 1)

#plot(all.model, "property",
#     property = layer.distances(all.model, whatmap = 1))

loc <- as_tibble(read_csv(paste0(diri,"SM_SoAf_location.csv"),
                          show_col_types = FALSE)) %>%
  mutate(file_id = row_number()) %>%
  relocate(file_id)

out <- data.frame(all, cluster = clust$cluster[all.model$unit.classif],
                  node = all.model$unit.classif) %>%
  mutate(file_id = row_number()) %>%
  relocate(file_id, cluster) %>%
  inner_join(loc, by="file_id") 
  #arrange(cluster, mn4)


write.csv(out, paste0(diri,"ERA5_SM_SoAf.csv"), quote=FALSE, row.names=FALSE)


