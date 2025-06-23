library(googledrive)
library(fixest)
#library(modelsummary)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(datasets)
library(readxl)

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


setwd("D:/Users.old/johnske5/forest_project/covid/cleaned data")

# get vector of state abbrev. 
# IN needs to be run separately

states <- state.abb
states <- states[!(states %in% c("ID","KS", "LA", "MI", "MO", "MT", "NM", "ND", "UT", 
                                 "WY", "AK", "AZ", "FL", "HI", "TX", "IN"))]

robust_results <- list()

# process state by state
for (i in 1:length(states)) {
  
  state <- states[i]
  print(state)

  ### get sale file
  name <- paste0(state, "_cleaned.rds")
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

  # create treatment date dummies for falsification tests: 1 year before and after, 6 mo before and after
  d$post_1yr_pre  <- ifelse(d$sale_ym.num > 201904, 1, 0)
  d$post_1yr_post <- ifelse(d$sale_ym.num > 202104, 1, 0)
  d$post_6mo_pre  <- ifelse(d$sale_ym.num > 201910, 1, 0)
  d$post_6mo_post <- ifelse(d$sale_ym.num > 202010, 1, 0)
  d$post_4yr_pre <- ifelse(d$sale_ym.num > 201604, 1, 0)
  
  # randomly assign treated and control
  d$treat_random <- sample(c(0,1), size = nrow(d), replace = T)
  d$treat_ua_random <- ifelse(d$treat_random == 0, 0, sample(c(0,1)))
  d$treat_uc_random <- ifelse(d$treat_random == 0, 0, 
                              ifelse(d$treat_ua_random == 1, 0, 1))
  
  # randomly assign pre and post
  d$post_random <- sample(c(0,1), size = nrow(d), replace = T)

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

robust <- list()
robust[['4yr.pre1']] <- feols(lnprice_2020 ~   bld_n_beds + bld_n_baths + m2_bld_fp +  ha  + 
                                ua_dist_mile  + I(treat_bau_1*post_4yr_pre) + treat_bau_1   
                              | fips + sale_month.f 
                              ,data=subset(d, sfr==1 & post == 0), vcov = "twoway")


robust[['1yr.pre1']] <- feols(lnprice_2020 ~   bld_n_beds + bld_n_baths + m2_bld_fp +  ha  + 
                             ua_dist_mile + I(treat_bau_1 * post_1yr_pre) +  treat_bau_1  
                           | fips + sale_month.f 
                           ,data=subset(d, sfr==1 & post == 0), vcov = "twoway")

# robust[['1yr.post1']] <- feols(lnprice_2020 ~   bld_n_beds + bld_n_baths + m2_bld_fp +  ha  + 
#                                        ua_dist_mile + I(treat_bau_1 * post_1yr_post) +  treat_bau_1  
#                                      | fips + sale_month.f 
#                                      ,data=subset(d, sfr==1 & post == 1), vcov = "twoway")

robust[['6mo.pre1']] <- feols(lnprice_2020 ~   bld_n_beds + bld_n_baths + m2_bld_fp +  ha  + 
                                       ua_dist_mile + I(treat_bau_1 * post_6mo_pre) +  treat_bau_1  
                                     | fips + sale_month.f 
                                     ,data=subset(d, sfr==1 & post == 0), vcov = "twoway")

# robust[['6mo.post1']] <- feols(lnprice_2020 ~   bld_n_beds + bld_n_baths + m2_bld_fp +  ha  + 
#                                         ua_dist_mile + I(treat_bau_1 * post_6mo_post) +  treat_bau_1  
#                                       | fips + sale_month.f 
#                                       ,data=subset(d, sfr==1 & post == 1), vcov = "twoway")

robust[['4yr.pre2']] <- feols(lnprice_2020 ~   bld_n_beds + bld_n_baths + m2_bld_fp +  ha  + 
                            ua_dist_mile  + I(treat_ua*post_4yr_pre) + I(treat_uc * post_4yr_pre) + 
                            treat_ua + treat_uc  
                            | fips + sale_month.f 
                            ,data=subset(d, sfr==1 & post == 0), vcov = "twoway")

robust[['1yr.pre2']] <- feols(lnprice_2020 ~   bld_n_beds + bld_n_baths + m2_bld_fp +  ha  + 
                             ua_dist_mile  + I(treat_ua*post_1yr_pre)  + I(treat_uc * post_1yr_pre) +  
                               treat_ua + treat_uc  
                           | fips + sale_month.f 
                           ,data=subset(d, sfr==1 & post == 0), vcov = "twoway") 

# robust[['1yr.post2']] <- feols(lnprice_2020 ~   bld_n_beds + bld_n_baths + m2_bld_fp +  ha  + 
#                                        ua_dist_mile  + I(treat_ua*post_1yr_post)  + I(treat_uc * post_1yr_post) +  
#                                         treat_ua + treat_uc  
#                                      | fips + sale_month.f 
#                                      ,data=subset(d, sfr==1 & post == 1), vcov = "twoway")

robust[['6mo.pre2']] <- feols(lnprice_2020 ~   bld_n_beds + bld_n_baths + m2_bld_fp +  ha  + 
                                       ua_dist_mile + I(treat_ua*post_6mo_pre) + I(treat_uc * post_6mo_pre) +  
                                       treat_ua + treat_uc  
                                     | fips + sale_month.f 
                                     ,data=subset(d, sfr==1 & post == 0), vcov = "twoway")

# robust[['6mo.post2']] <- feols(lnprice_2020 ~   bld_n_beds + bld_n_baths + m2_bld_fp +  ha  + 
#                                        ua_dist_mile  + I(treat_ua*post_6mo_post) + I(treat_uc * post_6mo_post) + 
#                                         treat_ua + treat_uc  
#                                      | fips + sale_month.f 
#                                      ,data=subset(d, sfr==1 & post == 1), vcov = "twoway")

robust[['rand.treat1']] <- feols(lnprice_2020 ~   bld_n_beds + bld_n_baths + m2_bld_fp +  ha  + 
                                       ua_dist_mile + I(treat_random * post) +  treat_random 
                                     | fips + sale_month.f 
                                     ,data=subset(d, sfr==1), vcov = "twoway")

robust[['rand.treat2']] <- feols(lnprice_2020 ~   bld_n_beds + bld_n_baths + m2_bld_fp +  ha  + 
                                       ua_dist_mile  + I(treat_ua_random*post) + 
                                         I(treat_uc_random * post) +  treat_ua_random + treat_uc_random  
                                     | fips + sale_month.f 
                                     ,data=subset(d, sfr==1), vcov = "twoway") 

robust[['rand.post1']] <- feols(lnprice_2020 ~   bld_n_beds + bld_n_baths + m2_bld_fp +  ha  + 
                                          ua_dist_mile + I(treat_bau_1 * post_random) +  treat_bau_1 + post_random 
                                        | fips + sale_month.f 
                                        ,data=subset(d, sfr==1), vcov = "twoway")

robust[['rand.post2']] <- feols(lnprice_2020 ~   bld_n_beds + bld_n_baths + m2_bld_fp +  ha  + 
                                          ua_dist_mile  + I(treat_ua*post_random) +
                                          I(treat_uc * post_random) +  treat_ua + treat_uc + post_random
                                        | fips + sale_month.f 
                                        ,data=subset(d, sfr==1), vcov = "twoway") 




robust_results[[i]] <- robust
names(robust_results)[i] <- state

rm(models, d)

}


# setwd("Z:/KelseyJohnson/covid")
saveRDS(state_results, "results_092922.rds")


################## INDIANA ################
i <- 9
state <- states[i]

### get sale file
name <- paste0(state, "_cleaned.rds")
temp <- tempfile(fileext = ".rds")
dl   <- drive_download(cleaned_files[cleaned_files$name == name,], path = temp, overwrite = T)
d    <- readRDS(temp)


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
d$treat_uc <- ifelse(d$ua_binary == 0 & d$treat_bau_1, 1, 0)
d$treat_ua <- ifelse(d$ua_binary == 1 & d$treat_bau_1, 1, 0)

d$treat_dist_1    <- ifelse(d$ua_dist_0_1 == 1 & d$treat_bau_1, 1, 0)
d$treat_dist_5    <- ifelse(d$ua_dist_1_5 == 1 & d$treat_bau_1, 1, 0)
d$treat_dist_10   <- ifelse(d$ua_dist_5_10 == 1 & d$treat_bau_1, 1, 0)
d$treat_dist_more <- ifelse(d$ua_dist_10p == 1 & d$treat_bau_1, 1, 0)

# d$post2 <- ifelse(d$sale_ym.num > 202005, 1, 0)
# d$treat_popden_0_20 <- ifelse(d$popden_0_15 == 1 & d$popden_15_20 == 1 & d$treat_bau_1, 1, 0)
# d$treat_popden_20p  <- ifelse(d$popden_20_25 == 1 & d$popden_25p == 1 & d$treat_bau_1, 1, 0)


models <- list()
###residential model
models[['sfr.1']] <- feols(lnprice_2020 ~   bld_n_rooms + m2_bld_fp +  ha  + 
                             ua_dist_mile + I(treat_bau_1 * post) +  treat_bau_1 + post 
                           | fips + sale_month.f 
                           ,data=subset(d, sfr==1), vcov = "twoway")


models[['sfr.2']] <- feols(lnprice_2020 ~   bld_n_rooms + m2_bld_fp +  ha  + 
                             ua_dist_mile  + I(treat_ua*post)  + I(treat_uc * post) +  treat_ua + treat_uc + post 
                           | fips + sale_month.f 
                           ,data=subset(d, sfr==1), vcov = "twoway") 

models[['sfr.3']] <- feols(lnprice_2020 ~   bld_n_rooms + m2_bld_fp +  ha  + I(treat_dist_1*post) + 
                                I(treat_dist_5 * post) + I(treat_dist_10 * post) + I(treat_dist_more * post) + 
                                treat_dist_1 + treat_dist_5 + treat_dist_10 + treat_dist_more  
                              | fips + sale_month.f 
                              ,data=subset(d, sfr==1), vcov = "twoway")



###ag model
models[['ag-sfr.1']] <- feols(lnprice_2020 ~   bld_n_rooms + m2_bld_fp +  ha  + 
                                ua_dist_mile + I(treat_bau_1 * post) +  treat_bau_1 + post 
                              | fips + sale_month.f 
                              ,data=subset(d, sfr==1 | AGtran == 1), vcov = "twoway")


models[['ag-sfr.2']] <- feols(lnprice_2020 ~   bld_n_rooms + m2_bld_fp +  ha  + 
                                ua_dist_mile  + I(treat_ua*post)  + I(treat_uc * post) +  treat_ua + treat_uc + post 
                              | fips + sale_month.f 
                              ,data=subset(d, sfr==1 | AGtran == 1), vcov = "twoway")

models[['ag-sfr.3']] <- feols(lnprice_2020 ~   bld_n_rooms + m2_bld_fp +  ha  + I(treat_dist_1*post) + 
                                I(treat_dist_5 * post) + I(treat_dist_10 * post) + I(treat_dist_more * post) + 
                                treat_dist_1 + treat_dist_5 + treat_dist_10 + treat_dist_more  
                              | fips + sale_month.f 
                              ,data=subset(d, sfr==1 | AGtran), vcov = "twoway")



state_results[[i]] <- models
names(state_results)[i] <- state


???###################### end #############################

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

###################Parallel trends
# show.plot = function(recent_sale,label="", show.means=TRUE) {
#   library(ggplot2)
#   gdat = recent_sale %>%
#     group_by(treat_bau_1, sale_month.f,exp,treat) %>%
#     summarize(y = mean(lnprice))

trenddat <- subset(sfrtran.df,sfr==TRUE & uhd2016==0 )
trendm1 <- feols(lnprice ~   bld_n_beds + bld_n_baths + m2_bld_fp +  ha  + 
                  travel_weiss  + treat_bau_1 * post   
                | fips + sale_month.f + sale_year.f
                ,data=trenddat)


  trenddat$y = predict(trendm1,trenddat) + resid(trendm1)
  show.plot(trenddat, trenddat$sale_month.f, trenddat$post, 
            trenddat$treat_bau_1, trenddat$treat_bau_1,
            show.means = FALSE, label="parallel pretrends")
  
trenddat$group <- ifelse(trenddat$treat_bau_1==1,"treated", "control")

  gdat = trenddat %>%
    group_by( group, sale_month.f,post,treat_bau_1) %>%
    summarize(y = mean(y))
  
  gg = ggplot(gdat, aes(y=y,x=sale_month.f, color= group)) +
    geom_line() + 
    geom_vline(xintercept=604) +
    theme_bw() +
    #annotate("text",x=579, y = 0.9*max(gdat$y), label="") +
    labs(x="Sale Month", y= "ln(price)", title="Single Family Sales - Parallel Trends" ) #+
  
plot(gg)

