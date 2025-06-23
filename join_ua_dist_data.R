library(readxl)


setwd("~/Post-doc/Covid/cleaned_data")


ua_dat <- read_excel("ua_list_all.xls")



mn_dist_urb <- read.csv("mn_urban_dist.csv")
mn_dist_urb$dist_ua_km <- NULL
mn_dist_urb$dist_uc_km <- NULL

d <- merge(mn_dist_urb, ua_dat, by = 

##### Arc GIS Steps for getting the sales that occured on non-urban/developed land #####

# the table mn_sale_urban (in cleaned_data folder) was processed in arc gis pro using the MN_sale shapefile,
# the aft land cover raster, and a shapefile of the u.s. states. THe processing 
# setps are as follows:
# 1. definition query on the state shapefile to select the desired state
# 2. clip hte land cover raster to the selected state
# 3. use the raster calculator, conditional statement: "con(landcover == 6, 1, 0)
#     - this results in a binary raster where 1 indicates urban, 0 everything else  
# 4. zonal stats as table: calculate the mean of the binary raster values within
#    each sales parcel
# 5. export table to cleaned data folder