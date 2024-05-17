# clean Hornsoe data

# subset to region and save for further analysis

rm(list=ls())


library(sf)
library(gtsummary)
library(mapview)
library(tidyverse)

setwd("C:/Users/DaSchulz/European Forest Institute/Environmental Services Team - wild-E/CS1_Sveaskog_Sweden/Data and docs from Sveaskog/shapefiles")

d <- st_read("./240513_Shapefiles Sveaskog/240513_Avdelning.shp") %>%
  filter(st_is_valid(.)) %>%
  st_transform(., 4326)

# Calculate centroids
centroids <- st_centroid(d)

# Extract coordinates
centroid_coords <- st_coordinates(centroids)

# Add latitude and longitude as columns
d$lat <- centroid_coords[, "Y"]
d$lon <- centroid_coords[, "X"]


# create new treatment variable
d$treatment <- case_when(d$NVMALKL %in% c("PF", "PG") ~ "Production",
                         d$NVMALKL %in% c("NO", "NO_NS", "NS") ~ "Conservation", 
                         TRUE ~ NA)

d$region <- ifelse(d$TSUM < 1200, "north", "south")

d$park <- ifelse(d$lon > 15.9 & d$lat < 60, "hornso", NA)

hornsoe<- d[!is.na(d$park),]


# export 
#st_write(hornsoe %>% select(all_of(c("OBJECTID", "treatment"))), "hornsoe.shp", append=TRUE)
save(hornsoe, file = "C:/Users/DaSchulz/OneDrive - European Forest Institute/Dokumente/research/wildE/data/hornsoe.RData")
