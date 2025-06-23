library(tidyverse)
library(sf)
library(terra)


# Load Census Data --------------------------------------------------------

census_data <- read_sf("/Users/mattwilliamson/Downloads/cb_2018_us_ua10_500k/cb_2018_us_ua10_500k.shp")

urban_area <- census_data %>% filter(., UATYP10 == "U")%>% 
  st_transform(., 2163)%>% 
  as(., "SpatVector")

urban_cluster <- census_data %>% filter(., UATYP10 == "C")%>% 
  st_transform(., 2163)%>% 
  as(., "SpatVector")

#get CONUS boundary for raster template
not_conus <- c("AK", "HI", "VI", "MP", "GU", "AS", "PR")
conus_states <- tigris::states() %>% 
  filter(., !STUSPS %in% not_conus ) %>% 
  st_transform(., 2163)%>% 
  as(., "SpatVector")

#create template raster with 10km resolution and estimate distance to nearest urban_
template_rast <- rast(conus_states, resolution = 10000, extent=ext(conus_states))
template_rast[] <- NA
dist_urban_area <- terra::distance(template_rast, urban_area) #estimate distance from each 10km cell to urban_area
dist_urban_cluster <- terra::distance(template_rast, urban_cluster) #estimate distance from each 10km cell to urban_cluster
dist_urban <- c(dist_urban_area, dist_urban_cluster) #combine together to make extraction faster
names(dist_urban) <- c("dist_ua", "dist_uc")#assign names to each layer
#load parcel values
mn_sale_shp <- read_sf("/Users/mattwilliamson/Downloads/Copy of MN_sale_shp/MN_sale.shp")%>% 
  st_transform(., 2163)%>% 
  as(., "SpatVector")

mn_urban_area_dist <- extract(dist_urban, mn_sale_shp, fun = mean) #take the mean value
mn_urban_km <- mn_urban_area_dist %>% 
  mutate(., dist_ua_km = dist_ua/1000,
         dist_uc_km = dist_uc/1000)

mn_ids <- as.data.frame(mn_sale_shp) #get original ids so that they align with the order of extraction
mn_urban <- cbind(mn_ids, mn_urban_km[,2:5])

write_csv(mn_urban, "/Users/mattwilliamson/Downloads/mn_urban_dist.csv")
