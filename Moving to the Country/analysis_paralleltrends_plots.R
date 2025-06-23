library(googledrive)
library(fixest)
#library(modelsummary)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(datasets)
library(zoo)

# bring in HPI for inflation adjusting prices
setwd("D:/Users.old/johnske5/forest_project/covid")

# open HPI dataset from FHFA (https://www.fhfa.gov/DataTools/Downloads/Pages/House-Price-Index-Datasets.aspx#qpo)
# using the quarterly states purchase only indices

hpi <- read_xls("HPI_PO_state.xls")

# create index base 2020  
hpi <-  hpi %>%
  group_by(state)%>%
  mutate(
    index_base2020 = index_sa/index_sa[yr == 2020 & qtr == 1]*index_sa[yr == 1991 & qtr == 1]
  )

# link to google drive
#cleaned_files <- drive_ls(path = "Cleaned data")

setwd("D:/Users.old/johnske5/forest_project/covid/cleaned data")

# get vector of state abbrev. for those states that have data and do not have
# multiple files 

states <- state.abb
states <- states[!(states %in% c("ID","KS", "LA", "MI", "MO", "MT", "NM", "ND", "UT", 
                                 "WY", "AK", "AZ", "FL", "HI", "TX", "IN"))]

# IN doesn't have # baths, only # beds and #rooms. run separately
# DC is missing variables, has NAs in variable lc_majority

state_results <- list()

# process state by state
for (i in 1:length(states)) {
  
  state <- states[i]
  print(state)

### get sale file
name <- paste0(state, "_cleaned.rds")
# temp <- tempfile(fileext = ".rds")
# dl   <- drive_download(cleaned_files[cleaned_files$name == name,], path = temp, overwrite = T)
# d    <- readRDS(temp)
d <- readRDS(name)


###mn_clean_hedonic.R cleans data and creates hedonic data set

d <- subset(d,
  !(is.na(bld_n_beds) |
      is.na(bld_n_baths) |
      is.na(m2_bld_fp)  |
      is.na(fips) |
      is.na (treat_bau_1)|
      is.na(hi_dev_2016))
)

d <- d[d$hi_dev_2016 == 0,]

# make different treatment variables
d$sale_month.f    <- as.factor(d$sale_ym.num)
d$ua_binary       <- ifelse(d$ua_UATYP10 == "U", 1, 0)

d$treat_uc        <- ifelse(d$ua_binary == 0 & d$treat_bau_1 == 1, 1, 0)
d$treat_ua        <- ifelse(d$ua_binary == 1 & d$treat_bau_1 == 1, 1, 0)

# different future scenarios
d$treat_uc_rs        <- ifelse(d$ua_binary == 0 & d$treat_rs_1 == 1, 1, 0)
d$treat_ua_rs        <- ifelse(d$ua_binary == 1 & d$treat_rs_1 == 1, 1, 0)

d$treat_uc_bbc        <- ifelse(d$ua_binary == 0 & d$treat_bbc_1 == 1, 1, 0)
d$treat_ua_bbc        <- ifelse(d$ua_binary == 1 & d$treat_bbc_1 == 1, 1, 0)


  # d$treat_dist_1    <- ifelse(d$ua_dist_0_1 == 1 & d$treat_bau_1 == 1, 1, 0)
  # d$treat_dist_5    <- ifelse(d$ua_dist_1_5 == 1 & d$treat_bau_1 == 1, 1, 0)
  # d$treat_dist_10   <- ifelse(d$ua_dist_5_10 == 1 & d$treat_bau_1 == 1, 1, 0)
  # d$treat_dist_more <- ifelse(d$ua_dist_10p == 1 & d$treat_bau_1 == 1, 1, 0)
  # 
  # d$treat_dist_1_ua    <- ifelse(d$ua_binary == 1 & d$ua_dist_0_1  == 1 & d$treat_bau_1 == 1, 1, 0)
  # d$treat_dist_1_uc    <- ifelse(d$ua_binary == 0 & d$ua_dist_0_1  == 1 & d$treat_bau_1 == 1, 1, 0)
  # d$treat_dist_5_ua    <- ifelse(d$ua_binary == 1 & d$ua_dist_1_5  == 1 & d$treat_bau_1 == 1, 1, 0)
  # d$treat_dist_5_uc    <- ifelse(d$ua_binary == 0 & d$ua_dist_1_5  == 1 & d$treat_bau_1 == 1, 1, 0)
  # d$treat_dist_10_ua   <- ifelse(d$ua_binary == 1 & d$ua_dist_5_10 == 1 & d$treat_bau_1 == 1, 1, 0)
  # d$treat_dist_10_uc   <- ifelse(d$ua_binary == 0 & d$ua_dist_5_10 == 1 & d$treat_bau_1 == 1, 1, 0)
  # d$treat_dist_more_ua <- ifelse(d$ua_binary == 1 & d$ua_dist_10p  == 1 & d$treat_bau_1 == 1, 1, 0)
  # d$treat_dist_more_uc <- ifelse(d$ua_binary == 0 & d$ua_dist_10p  == 1 & d$treat_bau_1 == 1, 1, 0)
  # 
  # d$treat_popden_0_20 <- ifelse(d$popden_0_15 == 1 & d$popden_15_20 == 1 & d$treat_bau_1 == 1, 1, 0)
  # d$treat_popden_20p  <- ifelse(d$popden_20_25 == 1 & d$popden_25p == 1 & d$treat_bau_1 == 1, 1, 0)


# inflation adjust prices
d$state <- state
d$sale_month.num <- as.numeric(substr(d$sale_ym.char, 5, 6))
d$qtr <- ifelse(d$sale_month.num %in% c(1:3), 1,
                ifelse(d$sale_month.num %in% c(4:6), 2,
                       ifelse(d$sale_month.num %in% c(7:9), 3, 4)))
d <- merge(d, hpi, by.x = c("state", "sale_year.num", "qtr"), by.y = c("state", "yr", "qtr"), all.x = T)
d$price_2020 <- d$price/(d$index_base2020/100)
d$lnprice_2020 <- log(d$price_2020)

# square ha for a quadratic
d$ha_sq <- d$ha*d$ha

models <- list()
###residential model
d$treated <- d$treat_bau_1
models[['sfr1']] <- feols(lnprice_2020 ~   bld_n_beds + bld_n_baths + m2_bld_fp +  ha  + 
                                        ua_dist_mile + I(treated * post) +  treated 
                                       | fips + sale_month.f 
                                        ,data=subset(d, sfr==1), vcov = "twoway")

d$treated_ua <- d$treat_ua
d$treated_uc <- d$treat_uc
models[['sfr2']]  <- feols(lnprice_2020 ~   bld_n_beds + bld_n_baths + m2_bld_fp +  ha  +
                  ua_dist_mile  + I(treated_ua*post)  + I(treated_uc * post) +  treated_ua + treated_uc  
                | fips + sale_month.f 
                ,data=subset(d, sfr==1), vcov = "twoway") 

d$treated <- d$treat_rs_1
models[['rs1']] <- feols(lnprice_2020 ~   bld_n_beds + bld_n_baths + m2_bld_fp +  ha  + 
                           ua_dist_mile + I(treated * post) +  treated 
                         | fips + sale_month.f 
                         ,data=subset(d, sfr==1), vcov = "twoway")

d$treated_ua <- d$treat_ua_rs
d$treated_uc <- d$treat_uc_rs
models[['rs2']] <- feols(lnprice_2020 ~   bld_n_beds + bld_n_baths + m2_bld_fp +  ha  + 
                           ua_dist_mile  + I(treated_ua*post) +
                           I(treated_uc * post) +  treated_ua + treated_uc
                         | fips + sale_month.f 
                         ,data=subset(d, sfr==1), vcov = "twoway") 

d$treated <- d$treat_bbc_1
models[['bbc1']] <- feols(lnprice_2020 ~   bld_n_beds + bld_n_baths + m2_bld_fp +  ha  + 
                            ua_dist_mile + I(treated * post) +  treated 
                          | fips + sale_month.f 
                          ,data=subset(d, sfr==1), vcov = "twoway")

d$treated_ua <- d$treat_ua_bbc
d$treated_uc <- d$treat_uc_bbc
models[['bbc2']] <- feols(lnprice_2020 ~   bld_n_beds + bld_n_baths + m2_bld_fp +  ha  + 
                            ua_dist_mile  + I(treated_ua*post) +
                            I(treated_uc * post) +  treated_ua + treated_uc 
                          | fips + sale_month.f 
                          ,data=subset(d, sfr==1), vcov = "twoway") 



state_results[[i]] <- models
names(state_results)[i] <- state

rm(models, d)

}


# setwd("Z:/KelseyJohnson/covid")
saveRDS(state_results, "results_092922.rds")


################## INDIANA ################
state <- "IN"

### get sale file
name <- paste0(state, "_cleaned.rds")
d <- readRDS(name)


###mn_clean_hedonic.R cleans data and creates hedonic data set

d <- subset(d,
            !(is.na(bld_n_rooms) |           # use number of rooms instead of beds and baths
                is.na(m2_bld_fp)  |
                is.na(fips) |
                is.na (treat_bau_1)|
                is.na(hi_dev_2016))
)

d <- d[d$hi_dev_2016 == 0,]

d$sale_month.f <- as.factor(d$sale_ym.num)

d$ua_binary <- ifelse(d$ua_UATYP10 == "U", 1, 0)
d$treat_uc  <- ifelse(d$ua_binary == 0 & d$treat_bau_1, 1, 0)
d$treat_ua  <- ifelse(d$ua_binary == 1 & d$treat_bau_1, 1, 0)
# different future scenarios
d$treat_uc_rs  <- ifelse(d$ua_binary == 0 & d$treat_rs_1 == 1, 1, 0)
d$treat_ua_rs  <- ifelse(d$ua_binary == 1 & d$treat_rs_1 == 1, 1, 0)
d$treat_uc_bbc <- ifelse(d$ua_binary == 0 & d$treat_bbc_1 == 1, 1, 0)
d$treat_ua_bbc <- ifelse(d$ua_binary == 1 & d$treat_bbc_1 == 1, 1, 0)

d$state <- state
d$sale_month.num <- as.numeric(substr(d$sale_ym.char, 5, 6))
d$qtr <- ifelse(d$sale_month.num %in% c(1:3), 1,
                ifelse(d$sale_month.num %in% c(4:6), 2,
                       ifelse(d$sale_month.num %in% c(7:9), 3, 4)))
d <- merge(d, hpi, by.x = c("state", "sale_year.num", "qtr"), by.y = c("state", "yr", "qtr"), all.x = T)
d$price_2020 <- d$price/(d$index_base2020/100)
d$lnprice_2020 <- log(d$price_2020)


models <- list()

#### DID Models
d$treated <- d$treat_bau_1
models[['sfr1']] <- feols(lnprice_2020 ~   bld_n_rooms + m2_bld_fp +  ha  + 
                             ua_dist_mile + I(treated * post) +  treated 
                           | fips + sale_month.f 
                           ,data=subset(d, sfr==1), vcov = "twoway")

d$treated_ua <- d$treat_ua
d$treated_uc <- d$treat_uc
models[['sfr2']] <- feols(lnprice_2020 ~   bld_n_rooms + m2_bld_fp +  ha  + 
                             ua_dist_mile  + I(treated_ua*post)  + I(treated_uc * post) +  treated_ua + treated_uc 
                           | fips + sale_month.f 
                           ,data=subset(d, sfr==1), vcov = "twoway") 

d$treated <- d$treat_rs_1
models[['rs1']] <- feols(lnprice_2020 ~   bld_n_rooms + m2_bld_fp +  ha  + 
                           ua_dist_mile + I(treated * post) +  treated 
                         | fips + sale_month.f 
                         ,data=subset(d, sfr==1), vcov = "twoway")

d$treated_ua <- d$treat_ua_rs
d$treated_uc <- d$treat_uc_rs
models[['rs2']] <- feols(lnprice_2020 ~   bld_n_rooms + m2_bld_fp +  ha  + 
                           ua_dist_mile  + I(treated_ua*post) +
                           I(treated_uc * post) +  treated_ua + treated_uc
                         | fips + sale_month.f 
                         ,data=subset(d, sfr==1), vcov = "twoway") 

d$treated <- d$treat_bbc_1
models[['bbc1']] <- feols(lnprice_2020 ~   bld_n_rooms + m2_bld_fp +  ha  + 
                            ua_dist_mile + I(treated * post) +  treated 
                          | fips + sale_month.f 
                          ,data=subset(d, sfr==1), vcov = "twoway")

d$treated_ua <- d$treat_ua_bbc
d$treated_uc <- d$treat_uc_bbc
models[['bbc2']] <- feols(lnprice_2020 ~   bld_n_rooms + m2_bld_fp +  ha  + 
                            ua_dist_mile  + I(treated_ua*post) +
                            I(treated_uc * post) +  treated_ua + treated_uc 
                          | fips + sale_month.f 
                          ,data=subset(d, sfr==1), vcov = "twoway") 



state_results[[35]] <- models
names(state_results)[35] <- state


##### create and save results tables ####
setwd("D:/Users.old/johnske5/forest_project/covid/results")
results_table_list <- list()
for(i in 1:35){
  state <- states[i]
  state <- ifelse(is.na(state), "IN", state)
  print(state)
    t <- etable(state_results[[i]]$sfr2,
         state_results[[i]]$rs2,
         state_results[[i]]$bbc2,
         state_results[[i]]$sfr1,
         state_results[[i]]$rs1,
         state_results[[i]]$bbc1, 
         se = "twoway", 
         digits = 3, 
         se.below = T, 
         order = c("I", "treated"),
         tex = F)
   
    names(t) <- c("Model 1 (BAU)", 'Model 1 (RS)', "Model 1 (BB)", "Model 2 (BAU)", "Model 2 (RS)", "Model 2 (BB)")
    
    state_row <- c(state, "", "", "", "", "")
    t <- rbind(state_row, t)
    rownames(t)[1] <- "state"
    results_table_list[[i]] <- t
    names(results_table_list)[i] <- names(state_results)[i]
    file_name <- paste0(state, "_results.csv")
    write.csv(t, file_name)
}

results_one_table <- bind_rows(results_table_list)


saveRDS(results_table_list, "results_tables.rds")

########## plot results #########

present_results <- list()

for (i in c(1,3:length(state_results))){

  # DID model 1
  state_abb <- names(state_results)[[i]]
  
  results<- state_results[[i]]$sfr.1
  
  DID <- results$coefficients["I(treat_bau_1 * post)"]
  CI     <- confint(results, level = 0.95, vcov = "twoway")["I(treat_bau_1 * post)",]
  CIlo <- CI[[1]]
  CIhi <- CI[[2]]
  pval  <- pvalue(results)["I(treat_bau_1 * post)"]
  
  # DID model 2
  results<- state_results[[i]]$sfr.2
  
  DID_uc  <- results$coefficients["I(treat_uc * post)"]
  CI     <- confint(results, level = 0.95, vcov = "twoway")["I(treat_uc * post)",]
  CIlo_uc <- CI[[1]]
  CIhi_uc <- CI[[2]]
  pval_uc  <- pvalue(results)["I(treat_uc * post)"]
  
  DID_ua  <- results$coefficients["I(treat_ua * post)"]
  CI     <- confint(results, level = 0.95, vcov = "twoway")["I(treat_ua * post)",]
  CIlo_ua <- CI[[1]]
  CIhi_ua <- CI[[2]]
  pval_ua <- pvalue(results)["I(treat_ua * post)"]
  
  # DID model 3
  results <- state_results[[i]]$sfr.3
  coeffs <- results$coefficients
  pvals <- pvalue(results)
  
  DID_d1  <- ifelse(length(coeffs["I(treat_dist_1 * post)"]) == 0, NA, coeffs["I(treat_dist_1 * post)"])
  DID_d5  <- ifelse(length(coeffs["I(treat_dist_5 * post)"]) == 0, NA, coeffs["I(treat_dist_5 * post)"])
  DID_d10 <- ifelse(length(coeffs["I(treat_dist_10 * post)"]) == 0, NA, coeffs["I(treat_dist_10 * post)"])
  DID_d10p<- ifelse(length(coeffs["I(treat_dist_more * post)"]) == 0, NA, coeffs["I(treat_dist_more * post)"])
  
  pval_d1  <- ifelse(length(pvals["I(treat_dist_1 * post)"]) == 0, NA, pvals["I(treat_dist_1 * post)"]) 
  pval_d5  <- ifelse(length(pvals["I(treat_dist_5 * post)"]) == 0, NA, pvals["I(treat_dist_5 * post)"])
  pval_d10 <- ifelse(length(pvals["I(treat_dist_10 * post)"]) == 0, NA, pvals["I(treat_dist_10 * post)"]) 
  pval_d10p<- ifelse(length(pvals["I(treat_dist_more * post)"]) == 0, NA, pvals["I(treat_dist_more * post)"]) 
  
  df <- data.frame(state_abb, 
                   DID, CIlo, CIhi, pval, 
                   DID_uc, CIlo_uc, CIhi_uc, pval_uc,DID_ua, CIlo_ua, CIhi_ua, pval_ua,
                   DID_d1, pval_d1, DID_d5, pval_d5, DID_d10, pval_d10, DID_d10p, pval_d10p)
  
  present_results[[i]] <- df
  rm(results)
}

results <- bind_rows(present_results)
saveRDS(results, "DID_coeffs.rds")

results$sig1 <- ifelse(results$pval <=0.05, "p <= 0.05", "p > 0.05")
results$sig1 <- as.factor(results$sig1)


ggplot(results, aes(x = DID, y = state_abb, color = sig1))+
  geom_point()+
  geom_errorbar(aes(xmin = CIlo, xmax = CIhi)) + 
  scale_colour_manual(values=c("#000000", "#CCCCCC"))+
  ggtitle("DID effect w/ single treatment group")+
  xlab("DID coefficient estimate")+
  ylab("State")
  
# graph results of the second did model
DID2_results <- results[,c(1,6:13)]
test <- DID2_results %>% pivot_longer(cols = !state_abb, names_to = "statistic", values_to = "estimate")
test <- test %>% separate(statistic, c("statistic", "urban_code"))

test <- test %>% pivot_wider(id_cols = c(state_abb, urban_code), names_from = statistic, values_from = estimate)

test$significance <- ifelse(test$pval <= 0.05, "p <= 0.05", "p > 0.05")
test$treatment_significance <- ifelse(test$urban_code == "ua" & test$pval <= 0.05, "near big city, p <= 0.05",
                                      ifelse(test$urban_code == "ua" & test$pval > 0.05, "near big city, p > 0.05",
                                             ifelse(test$urban_code == "uc" & test$pval <= 0.05, "near small city, p <= 0.05",
                                                    "near small city, p > 0.05")))

# test_ua <- test[test$urban_code == "ua",]
# test_uc <- test[test$urban_code == "uc",]

p2 <- ggplot(test, aes(x = DID, y = state_abb, color = treatment_significance))+
  geom_point()


p2 + geom_errorbar(aes(xmin = CIlo, xmax = CIhi)) +
  scale_colour_manual(values=c("#56B4E9", "#999999", "#D55E00", "#CCCCCC"))+
  ggtitle("DID effect w/ 2 treatment arms")+
  xlab("DID coefficient estimate w/ confidence interval")+
  ylab("State")
 

# graph 3rd DID model
DID3_results <- results[,c(1,14:21)]

# reshape long so there is a column called DID coeff
d3 <- DID3_results %>% pivot_longer(!state_abb, names_to = "stat", values_to = "estimate") %>%
  separate(stat, c("name", "coefficient")) %>%
  pivot_wider(id_cols = c("state_abb", "coefficient"), names_from = "name", values_from = "estimate")

d3$distance_bin <- ifelse(d3$coefficient == "d1", "0-1 mi from city", 
                          ifelse(d3$coefficient == "d5", "1-5 mi from city",
                                 ifelse(d3$coefficient == "d10", "5-10 mi from city",
                                        ifelse(d3$coefficient == "d10p", "10+ mi from city", "error"))))
d3 <- d3[!is.na(d3$DID) & d3$pval <= 0.05,]
p3 <- ggplot(d3, aes(x=distance_bin, y=DID, col=state_abb)) + 
  geom_point() + 
  geom_hline(yintercept = 0, linetype = "dashed")+
  ggtitle("DID effect w/ 4 treatment arms")+
  xlab("Distance from city")+
  ylab("DID estimate (p <= 0.05)")


for (i in 1:length(positive_states)){
  print(positive_states[i])
  print(state_results[[positive_states[i]]]$sfr.1)
}

# map
# load states shapefile
setwd("Z:/KelseyJohnson/covid")
state_shp <- 
  
# join results to shapefile
  
# plot and color based on DID1
  

# sfr and ag model

etable(fxs_ar1, fxs_ar2, se=("cluster"),
       signif.code = c(" ***"=0.01, " **"=0.05, " *"=0.1)  , tex=TRUE
       )

######## Parallel trends ####

# show.plot = function(recent_sale,label="", show.means=TRUE) {
#   library(ggplot2)
#   gdat = recent_sale %>%
#     group_by(treat_bau_1, sale_month.f,exp,treat) %>%
#     summarize(y = mean(lnprice))

# unconditional price trends: regress lnprice_2020 on FE only Gibson & Mullens (2020)

  # create new dataset that has dummies for fips and sale_month.f

for (i in 1:2){
  setwd("D:/Users.old/johnske5/forest_project/covid/cleaned data")
  state <- states[i]
  
  ### get sale file
  name <- paste0(state, "_cleaned.rds")
  d <- readRDS(name)
  d_backup <-d
  
  ###mn_clean_hedonic.R cleans data and creates hedonic data set
  vars <- c("sid", "fips", "price", "lnprice", "date", "sfr", "post", "bld_n_beds", "bld_n_baths", "m2_bld_fp", "ha", 
            "hi_dev_2016", "p_aft_bau_2", "p_aft_bau_1","p_aft_rs_1", "p_aft_rs_2", "p_aft_bcc_1", "p_aft_bcc_2", 
            "bld_code", "sale_ym.num", "sale_ym.char", "sale_year.num", "treat_bau_1", "treat_bau_2", "treat_rs_1", 
            "treat_rs_2", "treat_bbc_1", "treat_bbc_2", "ua_dist", "ua_dist_mile", "ua_UATYP10")
  d <- d[,vars]
  
  d <- subset(d,
              !(is.na(bld_n_beds) |
                  is.na(bld_n_baths) |
                  is.na(m2_bld_fp)  |
                  is.na(fips) |
                  is.na (treat_bau_1)|
                  is.na(hi_dev_2016))
  )
  
  d <- d[d$hi_dev_2016 == 0,]
  
  # make different treatment variables
  d$sale_month.f    <- as.factor(d$sale_ym.num)
  d$ua_binary       <- ifelse(d$ua_UATYP10 == "U", 1, 0)
  
  d$treat_uc        <- ifelse(d$ua_binary == 0 & d$treat_bau_1 == 1, 1, 0)
  d$treat_ua        <- ifelse(d$ua_binary == 1 & d$treat_bau_1 == 1, 1, 0)
  
  # different future scenarios
  d$treat_uc_rs        <- ifelse(d$ua_binary == 0 & d$treat_rs_1 == 1, 1, 0)
  d$treat_ua_rs        <- ifelse(d$ua_binary == 1 & d$treat_rs_1 == 1, 1, 0)
  
  d$treat_uc_bbc        <- ifelse(d$ua_binary == 0 & d$treat_bbc_1 == 1, 1, 0)
  d$treat_ua_bbc        <- ifelse(d$ua_binary == 1 & d$treat_bbc_1 == 1, 1, 0)
  
  
  
  # inflation adjust prices
  d$state <- state
  d$sale_month.num <- as.numeric(substr(d$sale_ym.char, 5, 6))
  d$qtr <- ifelse(d$sale_month.num %in% c(1:3), 1,
                  ifelse(d$sale_month.num %in% c(4:6), 2,
                         ifelse(d$sale_month.num %in% c(7:9), 3, 4)))
  d <- merge(d, hpi, by.x = c("state", "sale_year.num", "qtr"), by.y = c("state", "yr", "qtr"), all.x = T)
  d$price_2020 <- d$price/(d$index_base2020/100)
  d$lnprice_2020 <- log(d$price_2020)
  
  # subset d so only has sfr transactions
  d <- d[d$sfr == 1,]
  
# estimate model for unconditional price trends: regress lnprice_2020 on FE only Gibson & Mullens (2020)
trend_unc <- feols(lnprice_2020 ~ 1 | fips + sale_month.f 
                ,data = d, vcov = "twoway")

# estimate model for conditional price trends: regress lnprice_2020 on structural, location, and fe
# similar to Bakkensen et al (2019)

trend_cond <- feols(lnprice_2020 ~  bld_n_beds + bld_n_baths + m2_bld_fp +  ha  + 
                      ua_dist_mile | fips + sale_month.f 
                    ,data = d, vcov = "twoway")

# get residuals and save as variable in d
d$resid_unc <- resid(trend_unc)
d$resid_con <- resid(trend_cond)
d$year_mon <- as.yearmon(d$date)

# aggregate to treated and month
d_plot <- d %>% 
  group_by(sale_month.f, sale_year.num, treat_bau_1, post) %>%
  summarise(resid_unc = mean(resid_unc),
            resid_con = mean(resid_con)) %>%
  mutate(treated = ifelse(treat_bau_1 == 1, "treated", "control"),
         sale_month.num = as.numeric(sale_month.f),
         year_month.char = as.character(sale_month.f),
         year_month.num = as.numeric(year_month.char))

d$two_treat <- ifelse(d$treat_ua == 1, "treated near large cities",
                      ifelse(d$treat_uc == 1, "treated near small cities", "control"))

# aggregate to treated_ua treated_uc and month
d_plot2 <- d %>% 
  group_by(sale_month.f, sale_year.num, two_treat, post) %>%
  summarise(resid_unc = mean(resid_unc),
            resid_con = mean(resid_con)) %>%
  mutate(sale_month.num = as.numeric(sale_month.f),
         year_month.char = as.character(sale_month.f),
         year_month.num = as.numeric(year_month.char))
  
setwd("D:/Users.old/johnske5/forest_project/covid/plots")
  
  ggplot(d_plot, aes(y = resid_unc, x=sale_month.num, shape = treated)) +
    geom_point() + 
    geom_smooth(method = lm, formula = y~x, se = FALSE, aes(color = treated), 
                data = d_plot[d_plot$post == 0,], fullrange = F)+
    geom_smooth(method = lm, formula = y~x, se = FALSE, aes(color = treated), 
                data = d_plot[d_plot$post == 1,], fullrange = F)+
    geom_vline(xintercept=100) +
    #scale_x_discrete(breaks = waiver(), labels = d$sale_year.num)
    theme_bw() +
    labs(x="Sale Month (1 = Jan 2012)", y= "residual log(price)", title="Unconditional Price Trends" ) 
  
  name <- paste0(state, "_unc_trend.png")
  ggsave(name)
  
  ggplot(d_plot, aes(y = resid_con, x=sale_month.num, shape = treated)) +
    geom_point() + 
    geom_smooth(method = lm, formula = y~x, se = FALSE, aes(color = treated), 
                data = d_plot[d_plot$post == 0,], fullrange = F)+
    geom_smooth(method = lm, formula = y~x, se = FALSE, aes(color = treated), 
                data = d_plot[d_plot$post == 1,], fullrange = F)+
    geom_vline(xintercept=100) +
    #scale_x_discrete(breaks = waiver(), labels = d$sale_year.num)
    theme_bw() +
    labs(x="Sale Month (1 = Jan 2012)", y= "residual log(price)", title="Conditional Price Trends")
  
  name <- paste0(state, "_con_trend.png")
  ggsave(name)
  
  ggplot(d_plot2, aes(y = resid_unc, x=sale_month.num, shape = two_treat)) +
    geom_point() + 
    geom_smooth(method = lm, formula = y~x, se = FALSE, aes(color = two_treat), 
                data = d_plot2[d_plot2$post == 0,], fullrange = F)+
    geom_smooth(method = lm, formula = y~x, se = FALSE, aes(color = two_treat), 
                data = d_plot2[d_plot2$post == 1,], fullrange = F)+
    geom_vline(xintercept=100) +
    #scale_x_discrete(breaks = waiver(), labels = d$sale_year.num)
    theme_bw() +
    labs(x="Sale Month (1 = Jan 2012)", y= "residual log(price)", title="Unconditional Price Trends" ) 
  
  name <- paste0(state, "_unc_trend2.png")
  ggsave(name)
  
  ggplot(d_plot2, aes(y = resid_con, x=sale_month.num, shape = two_treat)) +
    geom_point() + 
    geom_smooth(method = lm, formula = y~x, se = FALSE, aes(color = two_treat), 
                data = d_plot2[d_plot2$post == 0,], fullrange = F)+
    geom_smooth(method = lm, formula = y~x, se = FALSE, aes(color = two_treat), 
                data = d_plot2[d_plot2$post == 1,], fullrange = F)+
    geom_vline(xintercept=100) +
    #scale_x_discrete(breaks = waiver(), labels = d$sale_year.num)
    theme_bw() +
    labs(x="Sale Month (1 = Jan 2012)", y= "residual log(price)", title="Conditional Price Trends" ) 
  
  name <- paste0(state, "_con_trend2.png")
  ggsave(name)
print(paste0("saved ", state, " graphs"))
}





