library(ggplot2)
library(tigris)
library(sf)
library(terra)
library(landscapemetrics)
library(rgdal)
library(sp)
library(raster)
library(fasterize)
library(tidyverse)
library(exactextractr)
library(dplyr)
library(fastDummies)
library(fixest)
library(broom)
library(openxlsx)
library(fredr)
library(zoo)
library(modelsummary)
library(marginaleffects)
options(scipen=999)

rm(list = ls())

states_a <- c("CA", "CO", "CT", "DE", "MD", "MA", "MI", "NJ", "NY", "NC", "OH", "PA", "VA", "VT")
states <- state.abb
states <- states[!(states %in% c("AK", "HI"))]
states <- c("MT", "ID")


#### FUNCTIONS ####
lu_change_fn <- function(states){
  state <- states
  s <- states_shape[states_shape$STUSPS == state,]
  extract_poly <- pald_buf[pald_buf$STUSPS == state,]
  extract_poly <- st_transform(extract_poly, crs(farmloss))
  
  extract_lu_change <- exact_extract(farmloss, extract_poly, default_value = 999, fun = "frac", append_cols = "unique_id")
  
  extract_lu_change <- extract_lu_change[,c("unique_id", "frac_1")]
}

spatial_fn <- function(states){
  state <- states
  
  #### 1. Load all data needed ####
  
  # load pvr dataset
  setwd("D:/Users.old/johnske5/03_ProRuRUL")
  # file_name <- paste0(state, ".vrt")
  # raster <- sprintf(file_name)
  # r <- terra::rast(raster)
  r <- terra::rast("productivity_versatility_resiliency_2016_conus_30m.tif")
  
  setwd("D:/Users.old/johnske5/03_ProRuRUL/easements")
  
  # get state and counties polygons
  s <- states_shape[states_shape$STUSPS == state,]
  c <- tigris::counties(state = state, cb = TRUE) %>%
    st_as_sf() %>%
    st_transform(terra::crs(r))
  
  # subset pald = dataset/polygons of easements subset to state[i]
  p <- pald[pald$STUSPS %in% state,]
  
  # subset easement buffer polygons and transform crs to rasters that will be extracted
  extract_poly <- pald_buf[pald_buf$STUSPS == state,]
  extract_poly_lc <- st_transform(extract_poly, crs(lc))
  extract_poly_dev <- st_transform(extract_poly, crs(dev))
  
  
  
  #### 2. Get state and county level averages  and state std dev.for indep. variables ####
  
  # get average county pvr
  # pvr_co <- exact_extract(r, c, fun = "mean", append_cols = c("STUSPS", "GEOID")) %>%
  #   rename(mean_co_pvr = mean)
  
  # get average state pvr
  pvr_st <- exact_extract(r, s, fun = c("mean", "stdev"), append_cols = c("STUSPS", "GEOID"))%>%
    rename(mean_st_pvr = mean,
           sd_st_pvr = stdev)
 
  
  # get average county land value
  # val_co <- exact_extract(v, c, fun = "mean", append_cols = c("STUSPS", "GEOID"))%>%
  #   rename(mean_co_val = mean)
  
  # get average state land value
  val_st <- exact_extract(v, s, fun = c("mean", "stdev"), append_cols = c("STUSPS", "GEOID"))%>%
    rename(mean_st_val = mean,
           sd_st_val = stdev)
  
  # merge these together
  
  st_data <- list(pvr_st, val_st)%>%
   reduce(full_join, by = c("STUSPS", "GEOID"))
  
  # co_data <- list(pvr_co, val_co)%>%
  #   reduce(full_join, by = c("STUSPS", "GEOID"))
  
  # co_st_data <- list(co_data, val_st, pvr_st)%>%
  #   reduce(full_join, by = "STUSPS")
  
  rm(pvr_st, val_st, c, s)
  
  
  #### 3. get easement level values for pvr, value, ... ####
  # extract
  # note that any easements with a NaN pvr value means they are not on ag land at all

  pvr <- exact_extract(r, p, fun = "mean", append_cols = c("GEOID", "id")) %>%
    rename(mean_pvr = mean)
  
  val <- exact_extract(v, p, fun = "mean", append_cols = c("GEOID", "id")) %>%
    rename(mean_val = mean)
  
  
  
  
  #### 4. get buffer level stats for Dev2040 and Landcover #####
  
  ### dev 2040
  extract_dev <- exact_extract(dev, extract_poly_dev, fun="frac", append_cols = c("GEOID", "id")) %>%
    mutate(buf_frac_dev2040 = frac_1+frac_2)
  mean(extract_dev$frac_0 + extract_dev$buf_frac_dev2040) # check that dev frac and frac0 add up to one
  extract_dev <- extract_dev[,c("GEOID", "id", "buf_frac_dev2040")] # subset columns to only id and frac_dev
  
  #### land cover
  extract_lc <- exact_extract(lc, extract_poly_lc, "frac", append_cols = c("GEOID", "id")) # get fraction of cell values that cover each buffer
  

  ## get all land cover class columns in the extract_lc dataset
  # list of all classes that should be in extrac_lc
  x <- c("frac_1", "frac_2", "frac_3", "frac_4", "frac_5", "frac_6", "frac_7", "frac_8", "frac_9",
         "frac_10", "frac_12", "frac_1101", "frac_1102", "frac_1103", "frac_1104", "frac_1105", "frac_1106",
         "frac_1107", "frac_1108", "frac_1109", "frac_1110", "frac_1112")
  
  # vector of zeros to add in for any colunmn that doesn't exist
  x0 <- rep(0, nrow(extract_lc))
  
  # do the checking and adding of missing columns
  extract_lc[x[!(x %in% colnames(extract_lc))]] = x0
  
  # check that all rows add up to 1
  mean(rowSums(extract_lc[,3:24]))
  
  # new land cover classes: 
  #  1 = agriculture (original values: 1, 2, 3)
  #  2 = forest (original values: 4,5)
  #  3 = developed (original values: 6, 1106)
  #  4 = 1101, 1102, 1103 = ag w.in LDR
  #  5 = 1104, 1105, 1107, 1108, 1109, 1110, 1112 = LDR
  #  6 = 7, 8, 9, 10, 12, = other
  
  extract_lc <- extract_lc %>%
    mutate(buf_frac_ag = frac_1 + frac_2 + frac_3, 
           buf_frac_forest = frac_4 + frac_5,
           buf_frac_dev = frac_6 + frac_1106,
           buf_frac_LDRag = frac_1101 + frac_1102 + frac_1103,
           buf_frac_LDR =  frac_1104 + frac_1105 + frac_1107 + frac_1108 + frac_1109 + frac_1110 + frac_1112,
           buf_frac_other = frac_7 + frac_8 + frac_9 + frac_10 + frac_12)
  
  # the new lc classes you've made
  categories <- c("buf_frac_ag", "buf_frac_forest", "buf_frac_dev", "buf_frac_LDRag", "buf_frac_LDR", "buf_frac_other")
  
  # check that all rows add up to 1
  mean(rowSums(extract_lc[,categories]))
  
  # keep only new category coverages
  extract_lc <- extract_lc[,c("GEOID", "id", categories)]
  
  
  #### 5. MERGE DATA ####
  # list of dataframes we've created
  # st_data (or co_st_data if doing county data also) 
  # val, pvr
  # extract_dev, extract_lc
  
  # first merge easement and buffer level stuff together
  m1 <- list(pvr, val, extract_dev, extract_lc) %>%
    reduce(full_join, by=c("GEOID", "id")) %>%
    mutate(state = substr(GEOID, 1, 2))
  
  # merge those with the county and state data and DONE
  m2 <- merge(m1, st_data, by.x = "state", by.y ="GEOID", all.x = T)
  
  return(m2)
  
  rm(x, x0, dev, extract_poly, extract_poly_dev, extract_poly_lc, p, pvr, raster, val, categories, file_name) 
}# function to make all dependent variables (spatial stats on the easements and buffers)
popn_fn <- function(x) {
  sid <- paste0(x, "POP")
  
  pg <-
    fredr(
      series_id = sid,
      units = "pch",
      observation_start = as.Date("1905-01-01"),
      observation_end = as.Date("2021-12-31")
    )[,c(1:3)]
  pg$state <- substr(pg$series_id, 1, 2)
  pg$pct_popch <- pg$value
  pg$value <- NULL
  pg$series_id <- NULL
  pg$year <- substr(pg$date, 1, 4)
  pg$date <- NULL
  
  # calculate average popn change over the whole time period
  pg$avg_pct_popch <- mean(pg$pct_popch)
  
  
  # calculate 5- and 10-year rolling averages
  pg$pct_popch_5avg <- rollmean(pg$pct_popch, k=5, align = "right", fill = NA)   # align right means it will calculate avg for previous k years
  pg$pct_popch_10avg <- rollmean(pg$pct_popch, k=10, align = "right", fill = NA)
  
  return(pg)
  
}# funxtion to get population (growth) --> just specify units = "pch"


#### Set FREDR api  key ####
# fredr documentation: https://cran.r-project.org/web/packages/fredr/fredr.pdf
fredr_set_key("cdf35c39d13d093ab4da9bd1db666e2d")


#### LOAD DATA ####
# get state shapefiles
states_shape <- tigris::states(cb=T)
#counties <- tigris::counties(state = "NJ")

#create a name, abbrev. table for states
st_lookup <- st_drop_geometry(states_shape)[, c("STUSPS", "NAME")]

states_noCA <- states[!states == "CA"]

# load the PALD and subset and fix state variables
setwd("D:/Users.old/johnske5/03_ProRuRUL/easements")
pald <- sf::st_read("PALD.shp")
pald <- pald[pald$STUSPS %in% states,]
pald$id <- seq.int(nrow(pald))
pald <- merge(pald, st_lookup, by = "STUSPS", all.x = T) # merge st_lookup with pald to get correct state names
pald$GEOID_OG <- pald$GEOID
pald$GEOID <- paste0(pald$STATEFP, pald$COUNTYFP) # redo the GEOIDs since some observations have the wrong first two digits

# create buffers around each easement
st_crs(pald)$units # check units of the pald: METERS
pald_buf <- st_buffer(pald, dist =2000)
pald_buf$buf_area <- st_area(pald_buf)

pald_buf1km <- st_buffer(pald, dist = 1000)
pald_buf1km$buf_area <- st_area(pald_buf1km)

## load the aft landcover and dev2040 data ##
lc <- rast("land_cover_and_use_2016_CONUS_30m.tif")

setwd("D:/Users.old/johnske5/03_ProRuRUL")
dev <- terra::rast("dev_2040_rs.tif")


# load noltes land value data
# the data that are downloaded from driad are at 480m resolution and measured as ln($/ha)
setwd("D:/Users.old/johnske5/03_ProRuRUL/easements/nolte_landval_estim")
v <- terra::rast("places_fmv_all.tif")

## check how many duplicates of easement #23
#	8f2a1214e78b772-01

#### PROCESS DATA ####

setwd("D:/Users.old/johnske5/03_ProRuRUL/easements")

#### 1. Dependent variables ####

# create easement/buffer statistics/dependent variables
#Y <- spatial_fn(states)
Y <- lapply(states, spatial_fn)
Y <- bind_rows(Y)
# eb_data <- lapply(states_noCA, spatial_fn)
# ca_eb_data <- spatial_fn("CA")
# Y <- bind_rows(eb_data, ca_eb_data)
#write.csv(Y, "data_incomplete_112723.csv")

#### 2. Get/process population data ####

POP <- lapply(states, popn_fn)
POP <- bind_rows(POP)
POP2 <- unique(POP[,c("state", "avg_pct_popch")])
POP$avg_pct_popch <- NULL


#### 3. Get policy scores - can find at aft's website: https://csp-fut.appspot.com/ ####

setwd("D:/Users.old/johnske5/03_ProRuRUL/easements")
scorecard <- read.csv("scorecard.csv")

scorecard <- merge(scorecard, st_lookup, by.x = "State", by.y = "NAME", all.x = T)

names(scorecard) <- c("state","PACE_score", "LU_planning_score", "prop_tax_score", "ag_dist_score", "farmlink_score", "leasing_score", "totals", "policy_score", "quartile", "STUSPS")

SCORE <- scorecard


#### MERGE AND SAVE ####

merge1 <- merge(pald[ ,!(names(pald) == "state")], Y, by = c("id", "GEOID", "STUSPS"), all.x = T)

# turn year to numeric 
merge1$Year_Est_num <- as.numeric(merge1$Year_Est)


# now add the population change data
merge2 <- merge(merge1, POP, by.x = c("STUSPS", "Year_Est_num"), by.y = c("state", "year"), all.x = T)
merge3 <- merge(merge2, POP2, by.x = "STUSPS", by.y = "state", all.x = T)

# now add scorecards
merge4 <- merge(merge3, SCORE, by = "STUSPS", all.x = T)



setwd("D:/Users.old/johnske5/03_ProRuRUL/easements")
write.csv(st_drop_geometry(merge4), "PACE_data_MT_ID.csv")
saveRDS(st_drop_geometry(merge4), "PACE_data_MT_ID.rds")


#### PROCESS AND MERGE LAND USE CHANGE DATA ####
setwd("D:/Users.old/johnske5/03_ProRuRUL/easements")
states2 <- c("CA", "CO", "CT", "DE", "MD", "MA", "NJ", "NY", "OH", "PA", "VT")

farmloss <- terra::rast("ag_land_conversion_2001_2016_conus_30m.tif")
fl <- lapply(states2, lu_change_fn)
fl <- bind_rows(fl) 
fl <- fl %>% rename(prop_farm_loss = frac_1)

p <- readRDS("PACE_data_112923.rds") %>%
  filter(STUSPS %in% states2)

d <- merge(p, fl, by = "unique_id", all.x = T)

write.csv(st_drop_geometry(d), "PACE_data_032224.csv")

#### START HERE: ESTIMATE REGRESSION MODELS ####
rm(list = ls())
setwd("D:/Users.old/johnske5/03_ProRuRUL/easements")

p <- read.csv("PACE_data_032224.csv")
states <- c("CA", "CO", "CT", "DE", "MD", "MA", "NJ", "NY", "OH", "PA", "VT")
p <- p[p$STUSPS %in% states,]

p$GEOID <- str_pad(as.character(p$GEOID), width = 5, side = "left", pad = "0")
p$STATEFP <- str_pad(as.character(p$STATEFP), width = 2, side = "left", pad = "0")
p$COUNTYFP <- str_pad(as.character(p$COUNTYFP), width = 3, side = "left", pad = "0")


# create binary variables indicating pace programs
p$PACE_state_admin   <- ifelse(p$STUSPS %in% c("DE", "MA", "CT"), 1, 0)
p$PACE_local_partner <- ifelse(p$STUSPS %in% c("MD", "PA", "NJ", "MI", 'OH', "VT"),1,0)
p$PACE_grants        <- ifelse(p$STUSPS %in% c("NY", "CA", "CO", "VA", "NC"), 1, 0)
p$PACE               <- ifelse(p$PACE == "Y", 1, 0)

p$FED <- ifelse(p$NRCS %in% c("ACEP-ALE", "RCPP-ALE", "FRPP") & p$PACE == 0, 1, 0)
p$GRP <- ifelse(p$NRCS == "GRP" & p$PACE == 0, 1, 0)
p$ALE <- ifelse(p$NRCS %in% c("ACEP-ALE", "RCPP-ALE") & p$PACE == 0, 1, 0)
p$FRP <- ifelse(p$NRCS == "FRPP" & p$PACE == 0, 1, 0)


# create dependent variables
p$pvr_stdz <- (p$mean_pvr - p$mean_st_pvr)/p$sd_st_pvr
p$val_stdz <- (p$mean_val - p$mean_st_val)/p$sd_st_val

p$buf_frac_ag2 <- p$buf_frac_ag + p$buf_frac_LDRag

# create new variable to categorize easements by PACE state and PACE acquisition

p$PACE_state_ease <- ifelse(p$PACE == 1 & p$PACE_state_admin == 1, 1, 0) 
p$PACE_local_ease <- ifelse(p$PACE == 1 & p$PACE_local_partner == 1, 1, 0)
p$PACE_grant_ease <- ifelse(p$PACE == 1 & p$PACE_grants == 1, 1, 0)
p$noPACE_fed <-      ifelse(p$PACE == 0 & p$FED == 1, 1, 0)
p$noPACE <- ifelse(p$PACE == 0, 1, 0)

# create a dummy variable for western states
p$west <- ifelse(p$STUSPS %in% c("CA", "CO"), 1, 0)

# create variable for first year of PACE implementation/acquisition
# list of states
                  #"CA"  "CO"  "CT"  "DE"  "MD"  "MA"  "NJ"  "NY"  "OH"  "PA"  "VT"
PACE_incep_yr <- c(1995, 1992, 1978, 1991, 1977, 1977, 1983, 1996, 1999, 1988, 1987)
first_ac_yr   <- c(1997, 1995, 1979, 1996, 1980, 1980, 1985, 1998, 1999, 1989, 1987)
PACE_incep <- data.frame(states, PACE_incep_yr, first_ac_yr)
p <- merge(p,PACE_incep, by.x = "STUSPS", by.y = "states", all.x= T)



#### SUMMARY STATS ####
p$easement_type <- ifelse(p$PACE_state_ease == 1, "Through state Admin. PACE",
                         ifelse(p$PACE_local_ease == 1, "Through local partnership PACE", 
                                ifelse(p$PACE_grant_ease == 1, "Through grant PACE",
                                       ifelse(p$noPACE_fed == 1, "Through federal program", "Other"))))

p$PACE_structure <- ifelse(p$STUSPS %in% c("DE", "MA", "CT"), "state", 
                           ifelse(p$STUSPS %in% c("MD", "PA", "NJ", "MI", 'OH', "VT"), "local", "grant"))

p$easement_size_ha <- p$gis_acres/2.471


sum_stats <- p %>%
  #filter(!STUSPS %in% c("MI", "VA", "NC")) %>%
  select(PACE, STUSPS, PACE_structure, easement_size_ha, mean_pvr, mean_val, buf_frac_LDR, prop_farm_loss)%>%
  mutate(PACE = ifelse(PACE == 1, "PACE", "NON_PACE"))%>%
  group_by(STUSPS, PACE_structure, PACE) %>%
  summarize(avg_easement_size_ha = mean(easement_size_ha),
            sd_easement_size_ha = sd(easement_size_ha),
            num_easements = n(),
            avg_pvr = mean(mean_pvr, na.rm = TRUE),
            sd_pvr = sd(mean_pvr, na.rm = TRUE),
            avg_value = mean(mean_val),
            sd_value = sd(mean_val),
            LDR_occur = mean(buf_frac_LDR),
            sd_LDR_occur = sd(buf_frac_LDR),
            farmloss_prop = mean(prop_farm_loss),
            sd_farmloss = sd(prop_farm_loss)
            )%>%
  group_by(PACE_structure)%>%
  arrange(STUSPS, .by_group =T) %>%
  pivot_wider(names_from = PACE, values_from = avg_easement_size_ha:sd_farmloss)

sum_stats_state <- p %>%
  group_by(STUSPS, PACE_structure) %>%
  summarize(avg_easement_size_ha = mean(easement_size_ha),
            sd_easement_size_ha = sd(easement_size_ha),
            num_easements = n(),
            avg_pvr = mean(mean_pvr, na.rm = T),
            sd_pvr = sd(mean_pvr, na.rm = T),
            avg_value = mean(mean_val),
            sd_value = sd(mean_val),
            LDR_occur = mean(buf_frac_LDR),
            sd_LDR_occur = sd(buf_frac_LDR)
  )

sum_stats <- merge(sum_stats, sum_stats_state, by = c("STUSPS", "PACE_structure"), all.x = T)%>%
  group_by(PACE_structure)%>%
  arrange(STUSPS, .by_group =T) %>%
  select("STUSPS","PACE_structure","avg_easement_size_ha_PACE", "sd_easement_size_ha_PACE",
         "avg_easement_size_ha_NON_PACE", "sd_easement_size_ha_NON_PACE",
         "avg_easement_size_ha", "sd_easement_size_ha",
         "num_easements_PACE", "num_easements_NON_PACE", "num_easements",  
         "avg_pvr_PACE", "sd_pvr_PACE", "avg_pvr_NON_PACE", "sd_pvr_NON_PACE", "avg_pvr", "sd_pvr", 
         "avg_value_PACE",  "sd_value_PACE", "avg_value_NON_PACE", "sd_value_NON_PACE", "avg_value", "sd_value",
         "LDR_occur_PACE", "sd_LDR_occur_PACE", "LDR_occur_NON_PACE",  "sd_LDR_occur_NON_PACE", 
         "LDR_occur", "sd_LDR_occur")
         
  
  
write.csv(sum_stats, "sum_stats_table.csv")


# set which variables to use for y, and for population.

# y options: 1. avgpvr,              5. avgval,      
#            3. pvr_minus_stpvr,     6. val_minus_stval
#            5. dev                  6. frac_ag land
#            7. buf_frac_LDR         8.
# pop options: 1. avg_pct_popch, 
#              2. pct_popch, 
#              3. pct_popch_5avg, 
#              4. pct_popch_10avg

# drop MI, NC, and VA
p <- p[!(p$STUSPS %in% c("MI", "NC", "VA")),]



# # run models
# models1 <- list(
#   "PVR" = feols(pvr_stdz ~ PACE + PACE_state_admin + PACE_local_partner + FED + PACE*PACE_state_admin + PACE*PACE_local_partner + 
#                   PACE_score + FED*PACE_state_admin + FED*PACE_local_partner + west + PACE_score + first_ac_yr, 
#                 data=p),
#   "Land Value" = feols(val_stdz ~ PACE + PACE_state_admin + PACE_local_partner + FED + PACE*PACE_state_admin + PACE*PACE_local_partner + 
#                          PACE_score + FED*PACE_state_admin + FED*PACE_local_partner + west + PACE_score + first_ac_yr, 
#                        data=p),
#   "Buffer LDR" = feols(buf_frac_LDRag ~ PACE + PACE_state_admin + PACE_local_partner + FED + PACE*PACE_state_admin + PACE*PACE_local_partner + 
#                          PACE_score + FED*PACE_state_admin + FED*PACE_local_partner + west + PACE_score + first_ac_yr, 
#                        data=p)
# )

models2 <- list(
  "PVR" = feols(pvr_stdz ~ PACE_state_ease + PACE_local_ease + PACE_grant_ease +
                  west + PACE_score + first_ac_yr,# + avg_pct_popch, 
                data=p),
  "Land Value" = feols(val_stdz ~ PACE_state_ease + PACE_local_ease + PACE_grant_ease +
                           west + PACE_score + first_ac_yr,# + avg_pct_popch, 
                       data=p),
  "Buffer LDR" = feols(buf_frac_LDRag~ PACE_state_ease + PACE_local_ease + PACE_grant_ease +
                         west + PACE_score + first_ac_yr,# + avg_pct_popch, 
                       data=p)
)


models2fe <- list(
  "PVR" = feols(pvr_stdz ~ PACE_state_ease + PACE_local_ease + PACE_grant_ease +
                  noPACE_fed  + west + PACE_score + first_ac_yr
                | GEOID, 
                data=p),
  "Land Value" = feols(val_stdz ~ PACE_state_ease + PACE_local_ease + PACE_grant_ease +
                         noPACE_fed  + west + PACE_score + first_ac_yr
                       | GEOID, 
                       data=p),
  "Buffer LDR" = feols(buf_frac_LDRag~ PACE_state_ease + PACE_local_ease + PACE_grant_ease +
                         noPACE_fed  + west + PACE_score + first_ac_yr
                       | GEOID, 
                       data=p)
)


modelsummary(models2, stars = T, output = "D:/Users.old/johnske5/03_ProRuRUL/easements/results/reg_results_nomivanc_nopopch.docx")

modelsummary(models2, stars = T, output = "D:/Users.old/johnske5/03_ProRuRUL/easements/results/reg_results2_ldrag.docx")
modelsummary(models2fe, stars = T, output = "D:/Users.old/johnske5/03_ProRuRUL/easements/results/reg_results2_fe.docx")

#### Get marginal effects of pace programs:

# first calculate some by hand
# pver model
#                            Int   Easetype  PACEprog PACEscore               Acqyr                     Easety*PACEprog
pace_easement_state_state <- 10.54 + 0.834 + 0.299 - 0.005*mean(p$PACE_score)-0.005*mean(p$first_ac_yr) - 0.712
pace_easement_local_state <- 10.54 + 0.834 + 0.717 - 0.005*mean(p$PACE_score)-0.005*mean(p$first_ac_yr) - 1.112
pace_easement_grant_state <- 10.54 + 0.834 + 0     - 0.005*mean(p$PACE_score)-0.005*mean(p$first_ac_yr) - 0
fed_easement_state_state <- 10.54  + 0.419 + 0.299 - 0.005*mean(p$PACE_score)-0.005*mean(p$first_ac_yr) - 0.331
fed_easement_local_state <- 10.54  + 0.419 + 0.717 - 0.005*mean(p$PACE_score)-0.005*mean(p$first_ac_yr) - 0.255 
fed_easement_grant_state <- 10.54  + 0.419 + 0     - 0.005*mean(p$PACE_score)-0.005*mean(p$first_ac_yr) - 0
othr_easement_state_state <- 10.54 + 0     + 0.299 - 0.005*mean(p$PACE_score)-0.005*mean(p$first_ac_yr) - 0
othr_easement_local_state <- 10.54 + 0     + 0.717 - 0.005*mean(p$PACE_score)-0.005*mean(p$first_ac_yr) - 0
othr_easement_grant_state <- 10.54 + 0     + 0     - 0.005*61.68169-0.005*1988.348 - 0

# how does PACE state compare to grant state
pace_easement_state_state - pace_easement_grant_state
fed_easement_state_state - fed_easement_grant_state
othr_easement_state_state - othr_easement_grant_state
# how do federal easements in a state_state compare to PACE easements?
# this outcome is not in  the marginal effects table below
pace_easement_state_state - fed_easement_state_state

pace_easement_state_state - pace_easement_grant_state
pace_easement_local_state - pace_easement_grant_state
# must do it model-by-model as the slopes() function does not support models of class "list"


#### MODEL ESTIMATES AND PLOTS ####

#### Predictions for pvr ####
### Estimate MOdel
pvr2 <- lm(pvr_stdz ~ PACE_state_ease + PACE_local_ease + PACE_grant_ease +
                + west + PACE_score + first_ac_yr + avg_pct_popch, 
              data=p)

pvrfe <- feols(pvr_stdz ~ PACE_state_ease + PACE_local_ease + PACE_grant_ease|
                 GEOID, 
               data=p)


#### Predicted values
pred_pvr2 <- predictions(pvr2, 
                         newdata = datagrid(
                           PACE_state_ease = c(0,1), 
                           PACE_local_ease = c(0,1),
                           PACE_grant_ease = c(0,1)
                         )
)

pred_pvrfe <- predictions(pvrfe,
                          newdata = datagrid(
                            PACE_state_ease = c(0,1), 
                            PACE_local_ease = c(0,1),
                            PACE_grant_ease = c(0,1)
                          )
)

# filter pred_pvr2 to scenarios that exist
pred_pvr2 <- pred_pvr2[!(pred_pvr2$PACE_state_ease == 1 & pred_pvr2$PACE_local_ease == 1) &
                         !(pred_pvr2$PACE_state_ease == 1 & pred_pvr2$PACE_grant_ease == 1) &
                         !(pred_pvr2$PACE_local_ease == 1 & pred_pvr2$PACE_grant_ease == 1),]

pred_pvr2$easement_type <- ifelse(pred_pvr2$PACE_state_ease == 1, "Centralized",
                                 ifelse(pred_pvr2$PACE_local_ease == 1, "Collaborative", 
                                        ifelse(pred_pvr2$PACE_grant_ease == 1, "Decentralized", "Non-PACE")))
pred_pvr2$category <- ifelse(pred_pvr2$easement_type == "Non-PACE", "Non-PACE Easements", "PACE Easements")


pred_pvrfe <- pred_pvrfe[!(pred_pvrfe$PACE_state_ease == 1 & pred_pvrfe$PACE_local_ease == 1) &
                         !(pred_pvrfe$PACE_state_ease == 1 & pred_pvrfe$PACE_grant_ease == 1) &
                         !(pred_pvrfe$PACE_local_ease == 1 & pred_pvrfe$PACE_grant_ease == 1),]

pred_pvrfe$easement_type <- ifelse(pred_pvrfe$PACE_state_ease == 1, "Centralized",
                                  ifelse(pred_pvrfe$PACE_local_ease == 1, "Collaborative", 
                                         ifelse(pred_pvrfe$PACE_grant_ease == 1, "Decentralized", "Non-PACE")))
pred_pvrfe$category <- ifelse(pred_pvrfe$easement_type == "Non-PACE", "Non-PACE Easements", "PACE Easements")

ggplot(pred_pvrfe) +
  geom_point(aes(x = easement_type, y = estimate))+
  geom_errorbar(aes(x = easement_type, ymin = conf.low, ymax = conf.high))+
  geom_hline(yintercept = 0, linetype = "dotted", color = "black")+
  geom_hline(aes(yintercept = 0.3394339, linetype = "Quality of non- PACE easements"), color = "red")+
  labs(
    title = "Predicted Land Quality across PACE Programs and Easement Types",
    subtitle = "Measured as deviations from state average",
    x = "PACE Program Structure",
    y = "Predicted Land Quality + 95% CI"
    ) + 
  theme_bw()+
  facet_grid(~category,
             scales = "free_x",
             space = "free")+
  theme(axis.text.x = element_text(vjust = 1, 
                                   # hjust = 1,
                                   size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12,
                                    margin=margin(t = 10)),
        axis.title.y = element_text(size = 12,
                                    margin=margin(r=10)),
        strip.text = element_text(size = 12))+
  scale_linetype_manual(name = NULL, 
                        values = 2, 
                        guide = guide_legend(override.aes = list(color = "red")),
                        labels = function(x) str_wrap(x, width = 15))
  
  

#### Predictions for land value ####
#### estimate model
val2 <- lm(mean_val ~ PACE_state_ease + PACE_local_ease + PACE_grant_ease +
                + west + PACE_score + first_ac_yr + avg_pct_popch, 
              data=p)

valfe <- feols(mean_val ~ PACE_state_ease + PACE_local_ease + PACE_grant_ease|
                 GEOID, 
               data=p)

#### Predicted values
pred_val2 <- predictions(val2, 
                         newdata = datagrid(
                           PACE_state_ease = c(0,1), 
                           PACE_local_ease = c(0,1),
                           PACE_grant_ease = c(0,1)
                         )
)

pred_valfe <- predictions(valfe, 
                         newdata = datagrid(
                           PACE_state_ease = c(0,1), 
                           PACE_local_ease = c(0,1),
                           PACE_grant_ease = c(0,1)
                         )
)
# filter pred_val2 to scenarios that exist
pred_val2 <- pred_val2[!(pred_val2$PACE_state_ease == 1 & pred_val2$PACE_local_ease == 1) &
                         !(pred_val2$PACE_state_ease == 1 & pred_val2$PACE_grant_ease == 1) &
                         !(pred_val2$PACE_local_ease == 1 & pred_val2$PACE_grant_ease == 1),]

pred_val2$easement_type <- ifelse(pred_val2$PACE_state_ease == 1, "Centralized",
                                   ifelse(pred_val2$PACE_local_ease == 1, "Collaborative", 
                                          ifelse(pred_val2$PACE_grant_ease == 1, "Decentralized", "Non-PACE")))
pred_val2$category <- ifelse(pred_pvr2$easement_type == "Non-PACE", "Non-PACE Easements", "PACE Easements")

pred_valfe <- pred_valfe[!(pred_valfe$PACE_state_ease == 1 & pred_valfe$PACE_local_ease == 1) &
                         !(pred_valfe$PACE_state_ease == 1 & pred_valfe$PACE_grant_ease == 1) &
                         !(pred_valfe$PACE_local_ease == 1 & pred_valfe$PACE_grant_ease == 1),]

pred_valfe$easement_type <- ifelse(pred_valfe$PACE_state_ease == 1, "Centralized",
                                  ifelse(pred_valfe$PACE_local_ease == 1, "Collaborative", 
                                         ifelse(pred_valfe$PACE_grant_ease == 1, "Decentralized", "Non-PACE")))
pred_valfe$category <- ifelse(pred_pvrfe$easement_type == "Non-PACE", "Non-PACE Easements", "PACE Easements")


ggplot(pred_valfe) +
  geom_point(aes(x = easement_type, y = estimate))+
  geom_errorbar(aes(x = easement_type, ymin = conf.low, ymax = conf.high))+
  geom_hline(aes(yintercept = 10.71186, linetype = "Value of non- PACE easements"), color = "red")+
  labs(
    title = "Predicted Land Values across PACE Programs and Easement Types",
    x = "PACE Program Structure",
    y = "Predicted Land Value (log($/ha)) + 95% CI"
  ) +
  theme_bw()+
  facet_grid(~category,
             scales = "free_x",
             space = "free")+
  theme(axis.text.x = element_text(vjust = 1, 
                                   # hjust = 1,
                                   size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12,
                                    margin=margin(t = 10)),
        axis.title.y = element_text(size = 12,
                                    margin=margin(r=10)),
        strip.text = element_text(size = 12))+
  scale_linetype_manual(name = NULL, 
                        values = 2, 
                        guide = guide_legend(override.aes = list(color = "red")),
                        labels = function(x) str_wrap(x, width = 15))



#### ME and predictions for LDR occurence ####
#### estimate model
ldr2 <- lm(buf_frac_LDRag ~ PACE_state_ease + PACE_local_ease + PACE_grant_ease +
             + west + PACE_score + first_ac_yr + avg_pct_popch, 
              data=p)

ldrfe <-feols(buf_frac_LDRag ~ PACE_state_ease + PACE_local_ease + PACE_grant_ease
              | GEOID,
              data=p)


#### Predicted values
pred_ldr2 <- predictions(ldrfe, 
                         newdata = datagrid(
                           PACE_state_ease = c(0,1), 
                           PACE_local_ease = c(0,1),
                           PACE_grant_ease = c(0,1)
                         )
)

# filter pred_ldr2 to scenarios that exist
pred_ldr2 <- pred_ldr2[!(pred_ldr2$PACE_state_ease == 1 & pred_ldr2$PACE_local_ease == 1) &
                         !(pred_ldr2$PACE_state_ease == 1 & pred_ldr2$PACE_grant_ease == 1) &
                         !(pred_ldr2$PACE_local_ease == 1 & pred_ldr2$PACE_grant_ease == 1),]

pred_ldr2$easement_type <- ifelse(pred_ldr2$PACE_state_ease == 1, "Centralized",
                                  ifelse(pred_ldr2$PACE_local_ease == 1, "Collaborative", 
                                         ifelse(pred_ldr2$PACE_grant_ease == 1, "Decentralized", "Non-PACE")))
pred_ldr2$category <- ifelse(pred_ldr2$easement_type == "Non-PACE", "Non-PACE Easements", "PACE Easements")


ggplot(pred_ldr2) +
  geom_point(aes(x = easement_type, y = estimate))+
  geom_errorbar(aes(x = easement_type, ymin = conf.low, ymax = conf.high))+
  geom_hline(aes(yintercept = 0.06573577, linetype = "Development risk of non-PACE easements"), color = "red")+
  labs(
    title = "Predicted Development Risk across PACE Programs and Easement Types",
    x = "PACE Program Structure",
    y = "Proportion of LDR land w.in 2 km + 95% CI"
  ) +
  theme_bw()+
  facet_grid(~category,
             scales = "free_x",
             space = "free")+
  theme(axis.text.x = element_text(vjust = 1, 
                                   # hjust = 1,
                                   size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 12,
                                    margin=margin(t = 10)),
        axis.title.y = element_text(size = 12,
                                    margin=margin(r=10)),
        strip.text = element_text(size = 12))+
  scale_linetype_manual(name = NULL, 
                        values = 2, 
                        guide = guide_legend(override.aes = list(color = "red")),
                        labels = function(x) str_wrap(x, width = 17))

#### ME and predictions for farmloss occurence ####
#### estimate model
flmod <- lm(prop_farm_loss ~ PACE_state_ease + PACE_local_ease + PACE_grant_ease +
             + west + PACE_score + first_ac_yr + avg_pct_popch, 
           data=p)

modelsummary(flmod, stars = T, output = "D:/Users.old/johnske5/03_ProRuRUL/easements/results/reg_results_farmloss.docx")

flmodfe <- feols(prop_farm_loss ~ PACE_state_ease + PACE_local_ease + PACE_grant_ease|
                GEOID, 
            data=p)
#### Predicted values
pred_fl <- predictions(flmodfe, 
                         newdata = datagrid(
                           PACE_state_ease = c(0,1), 
                           PACE_local_ease = c(0,1),
                           PACE_grant_ease = c(0,1)
                         )
)

# filter pred_ldr2 to scenarios that exist
pred_fl <- pred_fl[!(pred_fl$PACE_state_ease == 1 & pred_fl$PACE_local_ease == 1) &
                         !(pred_fl$PACE_state_ease == 1 & pred_fl$PACE_grant_ease == 1) &
                         !(pred_fl$PACE_local_ease == 1 & pred_fl$PACE_grant_ease == 1),]

pred_fl$easement_type <- ifelse(pred_fl$PACE_state_ease == 1, "Through state Admin. PACE",
                                  ifelse(pred_fl$PACE_local_ease == 1, "Through local partnership PACE", 
                                         ifelse(pred_fl$PACE_grant_ease == 1, "Through grant PACE", "Other")))

ggplot(pred_fl) +
  geom_point(aes(x = easement_type, y = estimate))+
  geom_errorbar(aes(x = easement_type, ymin = conf.low, ymax = conf.high))+
  geom_hline(yintercept = 0, linetype = "dotted")+
  labs(
    title = "Predicted Development Risk across PACE Programs and Easement Types",
    subtitle = "Development risk measured as proportion of land that experienced farm loss w/in 2km of easements",
    x = "Easement Acquisition",
    y = "Predicted Proportion of land that lost farms within 2 km + 95% CI"
  ) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))








##### OTHER STUFF #####
# plot the average easement size by state
easement_size <- p %>% 
  group_by(STUSPS) %>%
  summarize(avg_size = mean(gis_acres))

easement_size$PACE_structure <- ifelse(easement_size$STUSPS %in% c("DE", "MA", "CT"), "state", 
                           ifelse(easement_size$STUSPS %in% c("MD", "PA", "NJ", "MI", 'OH', "VT"), "local", "grant"))

# bar chart of landscape metrics
# metrics to choose from: "area_mn"  "clumpy"   "cohesion" "enn_mn"   "lpi"     
#                         "ndca"     "np"       "pd"       "pland"    "tca"   

gtitle <- "Average Easement Size (acres)"
save_name <- paste0("D:/Users.old/johnske5/03_ProRuRUL/easements/ldscp_metrics/barplot_easements", "_", "easement_acres", ".png")
x_order <- c("CT", "DE", 'MA', "MD", "MI", "NJ", "OH", "PA", "VT", "CO", "NC", "NY", "VA")


ggplot(easement_size, aes(x = factor(STUSPS, level = x_order), y  = avg_size, color = PACE_structure, fill = PACE_structure)) +
  geom_bar(stat = "identity") +
  labs(title = gtitle,
       x = "state",
       y = gtitle,
       fill = "PACE structure",
       color = "PACE structure")

ggsave(save_name)


# model for subsample of one type of state only
model <- feols(y ~ PACE +  pop + policy_score            # LU score, prop tax score, farmlink score, and policy score are significant and lead to higher R2
               | GEOID+ Year_Est_num,
               data=p[p$PACE_local_partner == 1,], vcov = "twoway")

summary(model)
#### CATEGORIZE PACE EASEMENTS
multi <- c("WA", "OR", "CO", "MI", "PA", "NJ", "SC", "NC", "VA", "MD", "ME", 'NY', "VT", "NH", "RI")
pace_easement_holders <- unique(st_drop_geometry(pald[,c("esmthldr", "eholdtype", "STUSPS", "PACE")]))
pace_easement_holders <- pace_easement_holders[pace_easement_holders$STUSPS %in% multi & pace_easement_holders$PACE == "Y",]
pace_easement_holders$pace_cat <- ifelse(pace_easement_holders$STUSPS %in% c("CO", "ME", "NY", 
                                                                             "NC", "OR", "SC",
                                                                             "VT", "VA", "WA"), "grant+local",
                                         ifelse(pace_easement_holders$STUSPS %in% c("MD", "NH", "NJ",
                                                                                    "PA", "RI"), "all3", "state+local"))
#### CALCULATE STATE LEVEL LANDSCAPE METRICS  ####




lscp_met_list <- list()

# convert pald to spatial
pald_sp <- as(pald, "Spatial")


# conveft state_shape to an sp object
state_shape_sp <- as(state_shape, "Spatial")

# reproject state shape to crs of pald
state_shape_sp <- spTransform(state_shape_sp, CRS(proj4string(pald_sp)))


for (i in 1:length(states)){
  s  <- states[i]
  s_shp <- state_shape_sp[state_shape$STUSPS == s,]
  lc_crop <- terra::crop(lc, s_shp)
  
  #agg metrics
  like_adj <- lsm_c_pladj(lc_crop) %>%
    mutate(pladj = value) %>%
    select(class, pladj)
  intersp  <- lsm_c_iji(lc_crop)%>%
    mutate(iji = value)%>%
    select(class, iji)
  
  
  d <- merge(like_adj, intersp, by = "class")%>%
    mutate(state = s)
  
  
  lscp_met_list[[i]] <- d
}


df <- bind_rows(lscp_met_list)

#### mutate data for graphing ####

df$grouping_var <- df$group2 # choice of: 4 = "name", 5 = "group", 6 = "group2" 
save_name <- "composition_groups_6.png" # choice of "all" "6" or "4"

df2 <- df %>%
  group_by(id, grouping_var,STUSPS, PACE, Year_Est, GEOID)%>%            # group by easement and lc class however you've grouped 
  summarize(percentage = sum(percentage)) %>%  # get percentage cover for your new groups (if you specify name as grouping variable, it returns the value of each obs)
  pivot_wider(names_from = grouping_var, values_from = percentage) %>%             # get rid of nas
  mutate_at(vars(-("id")), ~replace_na(.,0))%>% # reshape long so theres a lc group for each easement  
  pivot_longer(!c(id, STUSPS, PACE, Year_Est, GEOID), names_to = "landcover", values_to = "percentage") %>%  # reshape wide so there's only one observation per easement
  mutate(State = ifelse(STUSPS == "KY", "KY (PACE)", "TN"))

d <- df2#[df2$STUSPS == "KY",]
# plot with lc groups
ggplot(d, aes(x = landcover, y = percentage, fill = State)) +
  geom_boxplot()+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  theme_bw() +
  theme(axis.text = element_text(size = 11)) +
  labs(y = "land cover composition within easement",
       x = "land cover class (from AFT)",
       title = "Landcover Composition w/in Easements")
  

ggsave(save_name, width = 14, height = 9)



#### calculate landscape metrics ####

# for next pass, consider creating landscape metrics just for the PACE acquired easements

# create binary variable to indicate PACE program
df$PACE <- ifelse(df$state =="KY","PACE","NO PACE")

# join the lc class names
df <- merge(df, lc_names, by.x = "class", by.y = "value")

# reshape wide so there is a column for 
dfw <- df %>% 
  select(pladj, iji, state, group2) %>%
  pivot_wider(names_from = group2, values_from = value)

# create plots of a few of the different metrics across states

# agg metrics 
ggplot(data=df, aes(x = name, y = pladj, fill = state)) +
  geom_bar(position=position_dodge(),stat = "identity", width = 0.5)+
  #geom_text(aes(label = round(pladj, digits = 2)), vjust = -0.3)+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  ylab("Percentage of Like Adjacencies")+
  ggtitle("Percentage of Like Adjacencies within 1km of Easements")

ggsave("pladj_1k.png")

ggplot(data=df, aes(x = name, y = iji, fill = state)) +
  geom_bar(position=position_dodge(), stat = "identity", width = 0.5)+
  #geom_text(aes(label = round(cohesion, digits = 2)), vjust = -0.3)+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))+
  ylab("Interspersion and Juxtaposition")+
  ggtitle("Interspersion and Juxtaposition within 1km of Easements")

ggsave("iji_1k.png", width = 17.5, height = 6)


summary_of_values <- df %>%
  group_by(value)%>%
  summarize(avg_percent = mean(percentage))

ggplot(data = summary_of_values)+
  geom_bar(aes(x = value, y = avg_percent))

pald_lc <- merge(pald_lc, easement_size, by = "id", all.x = T)
# check units of vector
st_crs(pald)

# rasterize to pald as template with a defined pixel size
# if units are meters, resolution = 100 signifies 100m pixels
template <- raster(rast(pald, resolution = 100))

# convert pald to an sp object
# pald_sp <- as(pald, "Spatial")

r <- fasterize(pald, template, background = 0)




# get state sahpe files
state_shape <- tigris::states(cb = TRUE)
state_shape <-  filter_state(state_shape, state_names)

# conveft state_shape to an sp object
state_shape_sp <- as(state_shape, "Spatial")

# reproject state shape to crs of pald
state_shape_sp <- spTransform(state_shape_sp, CRS(proj4string(pald_sp)))


#### calculate landscape metrics ####

# for next pass, consider creating landscape metrics just for the PACE acquired easements
lscp_met_list <- list()

states <- c("KY", "TN")

for (i in 1:length(states)){
  s  <- state_east[i]
  s_shp <- state_shape_sp[state_shape$STUSPS == s,]
  ps2 <- terra::crop(r_us, s_shp)
  
  #agg metrics
  clump <- lsm_c_clumpy(ps2)
  cohes_q <- lsm_c_cohesion(ps2, directions = 8)
  # cohes_r <- lsm_c_cohesion(ps2, directions = 4)
  
  #area and edge metrics
  pch_area_q <- lsm_c_area_mn(ps2, directions = 8)
  # pch_area_r <- lsm_c_area_mn(ps2, directions = 4)
  lgst_pch_q <- lsm_c_lpi(ps2, directions = 8)
  # lgst_pch_r <- lsm_c_lpi(ps2, directions = 4)
  pland_q <- lsm_c_pland(ps2, directions = 8)
  # pland_r <- lsm_c_pland(ps2, directions = 4)
  
  #core area
  cai_q <- lsm_c_cai_mn(ps2, directions = 8, edge_depth = 1)
  # cai_r <- lsm_c_cai_mn(ps2, directions = 4, edge_depth = 1)
  
  #shape metric
  contig_q <- lsm_c_contig_mn(ps2, directions = 8)
  # contig_r <- lsm_c_contig_mn(ps2, directions = 4)
  mn_shp_q <- lsm_c_shape_mn(ps2, directions = 8)
  # mn_shp_r <- lsm_c_shape_mn(ps2, directions = 4)
  
  d <- bind_rows(cai_q, clump, cohes_q, contig_q,lgst_pch_q,
                 mn_shp_q, pch_area_q, pland_q)
  
  d$state <- s
  lscp_met_list[[i]] <- d
}

# bind rows of list to get df

df <- bind_rows(lscp_met_list)

# create binary variable to indicate PACE program
df$PACE <- ifelse(df$state %in% c("DE", "NJ", "VT", "MD", "PA", "MA", "RI", "CT", "NH", "OH", "ME", "NY", 
                                 "FL", "KY", "WV", "MI", "VA", "NC", "WI", "TX", "SC", "AL"),
                  "PACE","NO PACE")

# get rid of rows that calculate metric for non-easement class
df <- df[df$class == 1,]

# reshape wide so there is a column for 
dfw <- df %>% 
  pivot_wider(names_from = metric, values_from = value)

# create plots of a few of the different metrics across states

# agg metrics 
ggplot(data=dfw, aes(x = state, y = clumpy, fill = PACE)) +
  geom_bar(stat = "identity", width = 0.5)+
  geom_text(aes(label = round(clumpy, digits = 2)), vjust = -0.3)+
  ylab("Clumpiness Index")+
  ggtitle("Plot of Clumpiness Index by State (Eastern US)")

ggplot(data=dfw, aes(x = state, y = cohesion, fill = PACE)) +
  geom_bar(stat = "identity", width = 0.5)+
  geom_text(aes(label = round(cohesion, digits = 2)), vjust = -0.3)+
  ylab("Cohesion Index")+
  ggtitle("Plot of Patch Cohesion Index by State (Eastern US)")

# core area metrics
ggplot(data=dfw, aes(x = state, y = cai_mn, fill = PACE)) +
  geom_bar(stat = "identity", width = 0.5)+
  geom_text(aes(label = round(cai_mn, digits = 2)), vjust = -0.3)+
  ylab("Mean Core Area Index")+
  ggtitle("Plot of Mean Core Area Index by State (Eastern US)")

# area and edge metrics
ggplot(data=dfw, aes(x = state, y = area_mn, fill = PACE)) +
  geom_bar(stat = "identity", width = 0.5)+
  geom_text(aes(label = round(area_mn, digits = 2)), vjust = -0.3)+
  ylab("Mean Patch Area")+
  ggtitle("Plot of Mean Patch Area by State (Eastern US)")

ggplot(data=dfw, aes(x = state, y = pland, fill = PACE)) +
  geom_bar(stat = "identity", width = 0.5)+
  geom_text(aes(label = round(pland, digits = 2)), vjust = -0.3)+
  ylab("Core Area Percentage of Landscape")+
  ggtitle("Plot of Core Area Percentage of Landscape (Eastern US)")

# shape metric
ggplot(data=dfw, aes(x = state, y = contig_mn, fill = PACE)) +
  geom_bar(stat = "identity", width = 0.5)+
  geom_text(aes(label = round(contig_mn, digits = 2)), vjust = -0.3)+
  ylab("Mean of Contiguity index")+
  ggtitle("Plot of Mean of Contiguity index (Eastern US)")

ggplot(data=dfw, aes(x = state, y = shape_mn, fill = PACE)) +
  geom_bar(stat = "identity", width = 0.5)+
  geom_text(aes(label = round(shape_mn, digits = 2)), vjust = -0.3)+
  ylab("Mean of Shape index")+
  ggtitle("Plot of Mean of Shape index (Eastern US)")


# map of tennessee and kentucky
# list of "border counties"
border_co <- c("21075", "21105", "21039", "21083", "21157", "21035","21221","21143","21033","21047","21107","21177","21219", 
               "21141","21031","21227","21213","21003","21061","21009","21169","21171","21001","21057","21207","21053","21231","21199",
               "21147","21235","21125","21051","21121","21013",
               "47095","47045","47131","47053","47183","47079","47017","47005","47161","47083","47085","47125","47043","47021","47147",
               "47037","47189","47165","47111","47169","47159","47141","47087","47027","47133","47137","47049","47129","47151","47013",
               "47001","47173","47025")
# get county shapes
KYTN <- c("KY", "TN")
county_shape <- tigris::counties(state = KYTN, cb = TRUE) 
county_shape <- county_shape[county_shape$GEOID %in% border_co,]

# make a county level map data
map_data_co <- merge(county_shape, PACE_incep, by.x = "STUSPS", by.y = "state", all.x = T)

# make a fips code for the pald df
pald$fips <- paste0(pald$STATEFP, pald$COUNTYFP)