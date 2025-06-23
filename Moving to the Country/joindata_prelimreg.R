library(sf)
library(raster)
library(rgdal)
library(ggplot2)
library(sp)
library(stringr)
library(lubridate)
library(tidyverse)
library(zoo)

# import county-level aft risk which was calculated using zonal statistics in ArcGIS pro
setwd("~/Post-doc/Covid")
aft <- read.csv("avg_risk_county.csv")
aft$ZONE_CODE <- NULL
aft$COUNT <- NULL
aft$AREA <- NULL
colnames(aft)[colnames(aft)=="MEAN"] <- "aft_mean_conversion_risk"
colnames(aft)[colnames(aft)=="GEOID"] <- "fips"

# import the cleaned unemployment and bldg permits data
setwd("~/Post-doc/Covid/cleaned_data")
bldg <- read.csv("bldg_permits.csv")
bldg$fips <- str_pad(bldg$fips, 5, pad = "0")
bldg$state <- substr(bldg$fips, 1, 2)
bldg_mn <- bldg[bldg$state == "27",]
bldg$month <- NULL
bldg$year <- NULL
unemp <- read.csv("unemp.csv")

# merge the three data sets

d <- merge(unemp, bldg, by = c("fips", "mmyyyy"), all.x = T)
d$X.x <- NULL
d$X.y <- NULL
d$footnote_codes <- NULL
colnames(d)[colnames(d)=="footnote_text"] <- "unemp_footnote"

test <- merge(d, aft, by = "fips", all.x = T)
test$OID <- NULL

write.csv(test, "county_bldg_unemp_aft.csv")

data <- read.csv("county_bldg_unemp_aft.csv")


data$fips <- str_pad(data$fips, 5, pad = "0")
data$state <- substr(data$fips, 1, 2)


# load aft data on land use and conversion
setwd("~/Post-doc/Covid/cleaned_data")

lcu <- read.csv("land_cover_use.csv")
lcu <- lcu[,c(1:4)]
conv <- read.csv("conversion.csv")

# from conv get fips and projected % ag converted
ag_con <- conv[,c(3,13:21)]
ag_con$fips <- str_pad(ag_con$GEOID, 5, pad = "0")
ag_con$GEOID <- NULL



# estimate preliminary county level model of building permits
# use a negative binomial regression

library(foreign)
library(ggplot2)
library(MASS)
library(jtools)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(webshot)
library(pglm)

mn <- data[data$state == "27",]
mn$unemp_rate <- as.numeric(mn$unemp_rate)
mn$unemployment <-  as.numeric(mn$unemployment)
mn$employment <- as.numeric(mn$employment)
mn$labor_force <- as.numeric(mn$labor_force)

# create a covid dummy variable
mn$covid1 <- ifelse(mn$year == 2020 & mn$month > 3, 1,
                    ifelse(mn$year > 2020, 1, 0))

# remove observations that don't have permit data
mn <- mn[complete.cases(mn[,11]),]


# create county dummy variables and turn them to factors
# mn <- dummy_cols(mn, select_columns = "fips", remove_first_dummy = T)
# for(i in 20:ncol(mn)) {
#   mn[,i] <- as.factor(mn[,i])
# }

# specify fips as a factor variable
mn$fips.f <- factor(mn$fips)

mn2 <- merge(mn, ag_con, by = "fips", all.x = T)

# define treatment for dif in dif using the % ag converted to dev - start with 5% as threshold
mn2$treated_dev <- ifelse(mn2$ag_to_dev_bau_pcnt > 5, 1, 0)
mn2$treated_uhd <- ifelse(mn2$ag_to_uhd_bau_pcnt > 3, 1, 0)
mn2$treated_ldr <- ifelse(mn2$ag_to_ldr_bau_pcnt > 3, 1, 0)

# create interaction term
mn2$did_dev <- mn2$covid1 * mn2$treated_dev
mn2$did_uhd <- mn2$covid1 * mn2$treated_uhd
mn2$did_ldr <- mn2$covid1 * mn2$treated_ldr

write.csv(mn2, "mn_data.csv")

mn <- read.csv("mn_data.csv")

# test the parallel trends assumption
mn_precovid <- mn2[mn2$covid1 == 0,]
mn_precovid$trend <- ifelse(mn_precovid$year == 2015, mn_precovid$month,
                            ifelse(mn_precovid$year == 2016, mn_precovid$month + 12, 
                                   ifelse(mn_precovid$year == 2017, mn_precovid$month + 24,
                                          ifelse(mn_precovid$year == 2018, mn_precovid$month + 36,
                                                 ifelse(mn_precovid$year == 2019, mn_precovid$month + 48,
                                                        ifelse(mn_precovid$year == 2020, mn_precovid$month + 60, 0))))))

mn_precovid$treat_x_trend <- mn_precovid$treated_dev * mn_precovid$trend
mn_precovid$treatuhd_x_trend <- mn_precovid$treated_uhd * mn_precovid$trend

parallel <- pglm(all_permits ~ trend + treated_uhd + treatuhd_x_trend, data = mn_precovid, 
                 effect = "individual",
                 model  = "within",
                 family = "negbin", 
                 index  = "fips")

parallel <- pglm(all_permits ~ treat_x_trend, data = mn_precovid, 
                 effect = "individual",
                 model  = "within",
                 family = "negbin", 
                 index  = "fips")
summary(parallel)

# estimate dif in dif
did_reg <- lm(all_permits ~ covid1 + treated + did + unemp_rate + fips.f , data = mn2)
summary(did_reg)

did_pglm <- pglm(all_permits ~ covid1 + treated_uhd + did_uhd + unemp_rate, data = mn2, 
     effect = "individual",
     model  = "within",
     family = "negbin", 
     index  = "fips")

summary(did_pglm)
# estimate negative binomial regression

summary(did_reg2 <- glm.nb(all_permits ~ covid1 + treated_dev + did_dev + unemp_rate + fips.f, data = mn2))

tab <- tab_model(m1, file = "reg1.html")
tab_model(m1, file = "reg1.html")
webshot("reg1.html", "reg1.png")

# plot data

mn$mmyyyy <- str_pad(mn$mmyyyy, 6, pad = "0")
mn$monthyear <- parse_date_time(mn$mmyyy, "my")
mn$monthyear2 <- format(as.Date(mn$monthyear, "%y/%m/%Y"), "%m/%Y")
mn$treated <- as.factor(mn$treated)

lines <- mn %>%
  group_by(treated, monthyear) %>%
  summarise(mean_permits = mean(all_permits))%>%
  ggplot( aes(x=monthyear, y=mean_permits, group=treated,)) +
  geom_line(aes(color = treated)) +
  geom_vline(xintercept = as.numeric(mn$monthyear[14]), linetype = "dotted", color = "black", size = 1.5) +
  ggtitle("Building Permits Issued") +
  ylab("Number of permits issued")
