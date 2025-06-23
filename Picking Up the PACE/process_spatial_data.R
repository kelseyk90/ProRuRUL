library(terra)
library(tidyverse)

#load and plot PVR layer
r <- terra::rast("productivity_versatility_resiliency_2016_OR_30m.tif")
terra::plot(r)

c <- terra::vect("cb_2018_us_county_20m.shp")
terra::plot(c)

c <- terra::subset(c, c$STATEFP == "41") # get only counties in oregon
c <- terra::subset(c, c$COUNTYFP %in% c("003", "039", "041", "043", "047", "053")) # get only counties in benton, lane, lincoln, linn, marion, polk counties

#plot counties over the pvr layer
terra::plot(r)
terra::lines(c)

# get average pvr score per county - takes about 3 minuts for the 6 counties
pvr <- terra::extract(r, c) # extract cells for each county
pvr <- pvr[pvr$remapped != "NaN",] # get rid of NaN values
pvr %>% group_by(ID) %>% summarise(mean_pvr = mean(remapped)) # average over county


#load ztrax shapefile and filter to ag parcels 
p <- terra::vect("OR_sale.shp")

terra::plot(p)
