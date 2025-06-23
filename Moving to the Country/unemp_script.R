library(readr)
library(tidyverse)

url <- "https://download.bls.gov/pub/time.series/la/la.data.64.County"
d <- data.frame(read.delim(url))
footnotes <- read.delim("https://download.bls.gov/pub/time.series/la/la.footnote")

unemp <- d

# pare down to 2015-2022
unemp <- unemp[unemp$year %in% c(2015:2022),]

# separate the measure code from the series id
unemp$measure_code <- substr(unemp$series_id, 19, 20)
unemp$series_id <- substr(unemp$series_id, 1, 18)

# pivot wider so there is a variable for each unempl measure
unemp <- unemp %>% pivot_wider(names_from = measure_code, values_from = value, names_prefix = "meas_code_") 

# clean
unemp$fips <- substr(unemp$series_id, 6, 10)
unemp$month <- substr(unemp$period, 2,3)
unemp$mmyyyy <- paste0(unemp$month, unemp$year)
unemp$period <- NULL
unemp$series_id <- NULL

# get rid of the annual average observations (month == 13)
unemp <- unemp[unemp$month != "13",]

# rename measure codes
unemp <- unemp %>% rename(unemp_rate = meas_code_03, 
                          unemployment = meas_code_04,
                          employment = meas_code_05,
                          labor_force = meas_code_06)

unemp <- merge(unemp, footnotes, by.x = "footnote_codes", by.y = "footnote_code", all.x = T)


unemp <- unemp[,c(7,8,2,9,3,4,5,6,1,10)]

setwd("~/Boise State")
write.csv(unemp, "unemp.csv")






# add the footnote info
test$footnote_text <- ifelse(test$footnote_codes == C)