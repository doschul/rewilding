# Title: Sveastkog data collection
# Author: Dario Schulz (dario.schulz@efi.int)
# Purpose: Extract secondary data for rewilding sites in Sweden

##### 1. Setup #####

# working directory
setwd("C:/Users/DaSchulz/Documents/research/rewilding")

rm(list=ls())

#load the libraries
library(sf)
library(rgee)
library(stringr)
library(googledrive)
library(osmdata)
library(tidyverse)


# load shapefiles
hornsu <- st_read("C:/Users/DaSchulz/European Forest Institute/Environmental Services Team - wild-E/CS1_Sveaskog_Sweden/Data and docs from Sveaskog/shapefiles/hornsu.shp")


#ee_Initialize()
ee_Initialize(drive = TRUE)

# area of interest
aoi <- ee$FeatureCollection("FAO/GAUL/2015/level0")$
  filter(ee$Filter$eq('ADM0_NAME', 'Sweden'))

# load shapefile
ee_sites <- ee$FeatureCollection("projects/ee-darioschulz/assets/wilde/hornso")



# Define the target projection (e.g., WGS 84)
targetProjection <- ee$Projection('EPSG:4326')

##### 2. LOAD, PREPARE & EXTRACT DATA #####

# road network

# water bodies



#### 2 a) ISRIC SOILGRIDS ####

# Bulk density of the fine earth fraction	cg/cm³
isric_bdod = ee$Image("projects/soilgrids-isric/bdod_mean")

# Cation Exchange Capacity of the soil	mmol©/kg
isric_cec = ee$Image("projects/soilgrids-isric/cec_mean")

# Volumetric fraction of coarse fragments (> 2 mm)	cm3/dm3 (vol‰)
isric_cfvo = ee$Image("projects/soilgrids-isric/cfvo_mean")

# Proportion of clay (< 0.002 mm) in the fine earth fraction	g/kg
isric_clay = ee$Image("projects/soilgrids-isric/clay_mean") 

# Proportion of sand (> 0.05 mm) in the fine earth fraction	g/kg
isric_sand = ee$Image("projects/soilgrids-isric/sand_mean")

# Proportion of silt (≥ 0.002 mm and ≤ 0.05 mm) in the fine earth fraction	g/kg
isric_silt = ee$Image("projects/soilgrids-isric/silt_mean") 

# Total nitrogen (N)	cg/kg
isric_nitr = ee$Image("projects/soilgrids-isric/nitrogen_mean")

# Soil pH	pHx10
isric_ph = ee$Image("projects/soilgrids-isric/phh2o_mean")  

# Soil organic carbon content in the fine earth fraction	dg/kg
isric_soc = ee$Image("projects/soilgrids-isric/soc_mean")     

# Organic carbon density	hg/dm³
isric_ocd = ee$Image("projects/soilgrids-isric/ocd_mean")

# Organic carbon stocks	t/ha
isric_ocs = ee$Image("projects/soilgrids-isric/ocs_mean")     


# merge bands
isric <- isric_bdod$
  addBands(isric_cec)$
  addBands(isric_cfvo)$
  addBands(isric_clay)$
  addBands(isric_sand)$
  addBands(isric_silt)$
  addBands(isric_nitr)$
  addBands(isric_ph)$
  addBands(isric_soc)$
  # select upper 5 cm only
  select(".*0-5cm_mean")$
  clip(aoi)$
  reproject(targetProjection)


# extract
isricFeat <- isric$reduceRegions(
  collection = ee_sites,
  reducer = ee$Reducer$mean(),
  scale = 250
)



isric_names <- c("OBJECTID", paste0(isric$bandNames()$getInfo()))

# write to drive
task_vector <- ee_table_to_drive(
  collection = isricFeat,
  description = 'isricFeat',
  folder = "gee_wilde",
  timePrefix = FALSE,
  fileFormat = 'CSV', 
  selectors = isric_names
)

task_vector$start()
#ee_monitoring(task_vector) # optional




#### 2 b) TOPOGRAPHY ####

# import digital elevation model
fabdem <- ee$ImageCollection("projects/sat-io/open-datasets/FABDEM")

# Explanation on setting default Projection here 
# https://twitter.com/jstnbraaten/status/1494038930643042309
elev <- fabdem$mosaic()$setDefaultProjection('EPSG:4326',NULL, 30)$rename('elev')

# calculate slopes
slope <- ee$Terrain$slope(elev)$rename('slope')
# calculate aspect
aspect <- ee$Terrain$aspect(elev)$rename('aspect')

topo <- elev$
  addBands(slope)$
  addBands(aspect)$
  clip(aoi)

# extract
topoFeat <- topo$reduceRegions(
  collection = ee_sites,
  reducer = ee$Reducer$mean(),
  scale = 30
)

topo_names <- c("OBJECTID", paste0(topo$bandNames()$getInfo()))

# write to drive
task_vector <- ee_table_to_drive(
  collection = topoFeat,
  description = 'topoFeat',
  folder = "gee_wilde",
  timePrefix = FALSE,
  fileFormat = 'CSV',
  selectors = topo_names
)

task_vector$start()
#ee_monitoring(task_vector) # optional

#### 2 c) Travel time ####

access_dat <- ee$Image("Oxford/MAP/friction_surface_2019")$
  clip(aoi)

# extract
accessFeat <- access_dat$reduceRegions(
  collection = ee_sites,
  reducer = ee$Reducer$mean(),
  scale = 1000
)

access_names <- c("OBJECTID", paste0(access_dat$bandNames()$getInfo()))

# write to drive
task_vector <- ee_table_to_drive(
  collection = accessFeat,
  description = 'accessFeat',
  folder = "gee_wilde",
  timePrefix = FALSE,
  fileFormat = 'CSV',
  selectors = access_names
)

task_vector$start()
#ee_monitoring(task_vector) # optional


#### 2 d) Forest height ####

# P. Potapov, X. Li, A. Hernandez-Serna, A. Tyukavina, M.C. Hansen, 
# A. Kommareddy, A. Pickens, S. Turubanova, H. Tang, C.E. Silva, J. Armston, 
# R. Dubayah, J. B. Blair, M. Hofton (2020) Mapping and monitoring global 
# forest canopy height through integration of GEDI and Landsat data. Remote 
# Sensing of Environment, 112165. https://doi.org/10.1016/j.rse.2020.112165

# Dataset Access in Google Earth Engine
# Image Collection id: users/potapovpeter/GEDI_V27

# tree height
tree_height = ee$ImageCollection("users/potapovpeter/GEDI_V27")  

# extract
tree_heightFeat <- tree_height$reduceRegions(
  collection = ee_sites,
  reducer = ee$Reducer$mean(),
  scale = 1000
)

treeheight_names <- c("OBJECTID", paste0(tree_height$bandNames()$getInfo()))

# write to drive
task_vector <- ee_table_to_drive(
  collection = treeheightFeat,
  description = 'treeheightFeat',
  folder = "gee_wilde",
  timePrefix = FALSE,
  fileFormat = 'CSV',
  selectors = tree_height_names
)

task_vector$start()
#ee_monitoring(task_vector) # optional


#### 2 e) Hansen Forest change ####

# Load the Hansen Global Forest Change data
hansen_collection <- ee$Image('UMD/hansen/global_forest_change_2023_v1_11')

# Clip the collection to the AOI
hansen_clipped <- hansen_collection$clip(aoi)

# Create a new image with each band representing one year
years <- 2000:2022
forest_cover_bands <- lapply(years, function(year) {
  hansen_clipped$select(ee$String('lossyear'))$eq(year - 2000)$rename(paste0('loss_', year))
})

# Combine all the bands into a single image
forest_cover_image <- do.call(ee$Image$cat, forest_cover_bands)

# extract
forest_loss <- forest_cover_image$reduceRegions(
  collection = ee_sites,
  reducer = ee$Reducer$mean(),
  scale = 1000
)

fl_names <- c("OBJECTID", paste0(forest_cover_image$bandNames()$getInfo()))

# write to drive
task_vector <- ee_table_to_drive(
  collection = forest_loss,
  description = 'forest_loss',
  folder = "gee_wilde",
  timePrefix = FALSE,
  fileFormat = 'CSV',
  selectors = fl_names
)

task_vector$start()
#ee_monitoring(task_vector) # optional

#### 2 f) Annual forest carbon fluxes ####

# Harris, N.L., Gibbs, D.A., Baccini, A. et al. Global maps of twenty-first century forest carbon fluxes. Nat. Clim. Chang. 11, 234–240 (2021).
# https://doi.org/10.1038/s41558-020-00976-6

#emissions <- ee$Image("projects/sat-io/open-datasets/forest_carbon_fluxes/gross_emissions")
#removals <- ee$Image("projects/sat-io/open-datasets/forest_carbon_fluxes/gross_removals")
#net_flux <- ee$Image("projects/sat-io/open-datasets/forest_carbon_fluxes/net_flux")




##### 3 Local data analysis #####

location <- opq(bbox = st_bbox(hornsu))
# slightly larger bbox (for Hornso)
location <- opq(bbox = c(15.9, 56.9, 16.3, 57.1))


# Make query to get streets and roads
osm_hwy <- location %>%
  add_osm_feature(key = 'highway') %>%
  osmdata_sf()

# Extract and combine different types of streets and roads
streets <- osm_hwy$osm_lines[osm_hwy$osm_lines$highway %in% c('motorway', 'trunk', 'primary', 'secondary', 'tertiary', 'unclassified', 'residential', 'service', 'living_street', 'pedestrian', 'track', 'road'),]

# Extract gravel roads (these are often tagged as "track" with "tracktype")
gravel_roads <- osm_hwy$osm_lines[osm_hwy$osm_lines$highway == 'track',]

# open water bodies
water <- location %>%
  add_osm_feature(key = "natural", 
                  value = c("water")) %>%
  osmdata_sf()

ponds <- water[["osm_polygons"]]
lakes <- water[["osm_multipolygons"]]


# power lines
pwr_dat <- location %>%
  add_osm_feature(key = "power", 
                  value = c("line")) %>%
  osmdata_sf()
pwr_lines <- pwr_dat[["osm_lines"]]


# waterways
stream_dat <- location %>%
  add_osm_feature(key = "waterway", 
                  value = c("river", "stream")) %>%
  osmdata_sf()
streams <- stream_dat[["osm_lines"]]


### Calculate distances


feat_list <- list("dist_street" = streets, 
                  #"dist_road"   = gravel_roads, 
                  "dist_stream" = streams, 
                  "dist_prwlne" = pwr_lines, 
                  "dist_pond"   = ponds, 
                  "dist_lake"   = lakes)

# prepare empty df
dist_df = data.frame(OBJECTID = hornsu$OBJECTID)

for (i in 1:length(feat_list)) {
  cat(i)
  varname <- names(feat_list[i])
  dat <- feat_list[[i]] %>% st_make_valid()
  
  # calculate minimum distances
  dist_tmp <- st_distance(hornsu, dat) %>%
    as.data.frame() %>%
    do.call(pmin, .)
  
  # merge to df
  dist_df[varname] <- dist_tmp
}

# save distance data
#save(dist_df, file = "C:/Users/DaSchulz/OneDrive - European Forest Institute/Dokumente/research/wildE/data/dist_df.RData")

##### 3 DOWNLOAD & CLEAN DATA #####



# GEE download folder
gee_dl_dir <- "C:/Users/DaSchulz/OneDrive - European Forest Institute/Dokumente/research/wildE/data"

#folder link to id
jp_folder = "https://drive.google.com/drive/folders/1yC7fhAO_9PT3miZPXznh_a6K3wD7BAip"
folder_id = drive_get(as_id(jp_folder))

#find files in folder
files = drive_ls(folder_id)

#download files
for (file_i in seq_along(files$name)) {
  #fails if already exists
  try({
    drive_download(
      as_id(files$id[file_i]),
      path = str_c(gee_dl_dir, files$name[file_i])
    )
  })
}

# load files
file_list <- list.files(path = gee_dl_dir, pattern = ".csv", full.names = T) %>%
  lapply(., read_csv)

# Initialize the result dataframe with the first dataframe in the list
gee_dat <- file_list[[1]]

# Loop through the remaining dataframes in the list and left join each of them
for (i in 2:length(file_list)) {
  gee_dat <- left_join(gee_dat, file_list[[i]], by = "OBJECTID")
}


# save results locally
#save(gee_dat, file = "C:/Users/DaSchulz/OneDrive - European Forest Institute/Dokumente/research/wildE/data/gee_df.RData")


sec_dat <- left_join(dist_df, gee_dat, by = "OBJECTID")


# save combined secondary data locally
save(sec_dat, file = "C:/Users/DaSchulz/OneDrive - European Forest Institute/Dokumente/research/wildE/data/sec_dat.RData")

