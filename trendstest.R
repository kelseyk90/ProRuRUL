library(lmtest)
library(sandwich)
library(arrow)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(readr)
library(fixest)
library(did)
library(tsibble)
library(foreign)
library(car)
library(DRDID)
library(magick)
library(broom)
library(ParallelTrendsPlot)
library(arrow)
library(datasets)
library(googledrive)
library(tidyverse)


name <- "MN_cleaned.rds"
temp <- tempfile(fileext = ".rds")
dl   <- drive_download(cleaned_files[cleaned_files$name == name,], path = temp, overwrite = T)
d    <- readRDS(temp)


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


       )

###################Parallel trends
# show.plot = function(recent_sale,label="", show.means=TRUE) {
#   library(ggplot2)
#   gdat = recent_sale %>%
#     group_by(treat_bau_1, sale_month.f,exp,treat) %>%
#     summarize(y = mean(lnprice))

trenddat <- subset(d,sfr==TRUE & uhd2016==0 )


trendtestm1 <-feols(lnprice ~   bld_n_beds + bld_n_baths + m2_bld_fp +  ha  + 
                      ua_dist_mile + I(treat_bau_1 * post) +  treat_bau_1  
                    | fips + sale_month.f 
                    ,data=subset(d, sfr==1), vcov = "twoway")

d2 <- subset(d, obs(trendtestm1))

trendtestm2 <-feols(lnprice ~   bld_n_beds + bld_n_baths + m2_bld_fp +  ha  + 
                      ua_dist_mile + I(treat_bau_1 * post) +  treat_bau_1  
                    | fips + sale_month.f 
                    ,d , subset=obs(trendtestm1), vcov = "twoway")
test1 <- subset(d, data[-trendtestm1$fixef_removed$id])
test2 <- head(model.matrix(trendtestm1, data = d[, "sid", drop = FALSE], subset = TRUE))

d0 <- d %>%
  mutate(bld_n_beds <-coalesce(bld_n_beds, 0), 
           bld_n_baths <-coalesce(bld_n_baths, 0), 
           m2_bld_fp <-coalesce(m2_bld_fp, 0),  
           ha  <-coalesce(ha, 0),
           ua_dist_mile <-coalesce(ua_dist_mile, 0))

#trendm1 <- feols(lnprice ~   bld_n_beds + bld_n_baths + m2_bld_fp +  ha  + 
#                | fips + sale_month.f + sale_year.f
#                ,data=d)
d0sub<- subset(d0,
               !(is.na(lnprice)))

  d0$y = predict(trendtestm1,d0) + resid(trendtestm1)

  
  show.plot(d0, show.means=FALSE, label="Parallel Trends")
    
  plot(d0, sale_month.f,  
            treat_bau_1, 
            show.means = FALSE, label="parallel pretrends")
    
  show.plot(trenddat, trenddat$sale_month.f, trenddat$post, 
            trenddat$treat_bau_1, trenddat$treat_bau_1,
            show.means = FALSE, label="parallel pretrends")
  
d0$group <- ifelse(d0$treat_bau_1==1,"treated", "control")

  gdat = d0 %>%
    group_by( group, sale_month.f, post) %>%
    summarize(y = mean(y))
  
  gg = ggplot(gdat, aes(y=y,x=sale_month.f, color= group)) +
    geom_line() + 
    geom_vline(xintercept=604) +
    theme_bw() +
    #annotate("text",x=579, y = 0.9*max(gdat$y), label="") +
    labs(x="Sale Month", y= "ln(price)", title="Single Family Sales - Parallel Trends" ) #+
  
plot(gg)


library(ParallelTrendsPlot)
dat$y=dat$y.org
pt.dat = parallel.trends.data(dat,cvars="x")
parallel.trends.plot(pt.dat) + theme_bw()

show.plot = function(dat,label="", show.means=TRUE) {
  library(ggplot2)
  gdat = dat %>%
    group_by(group, sale_month.f, post , treat_bau_1) %>%
    summarize(y = mean(y))
  
  gg = ggplot(gdat, aes(y=y,x=t, color= group)) +
    geom_line() + 
    geom_vline(xintercept=T/2) +
    theme_bw() +
    annotate("text",x=T/4, y = 0.9*max(gdat$y), label=label)
  
  if (show.means) {
    y.pre.tr <<- mean(filter(gdat,treat==1, exp==0)$y) %>% round(1)
    y.exp.tr <<- mean(filter(gdat,treat==1, exp==1)$y) %>% round(1)
    y.pre.co <<- mean(filter(gdat,treat==0, exp==0)$y) %>% round(1)
    y.exp.co <<- mean(filter(gdat,treat==0, exp==1)$y) %>% round(1)
    gg = gg + 
      annotate("label", x=T/4, y=y.pre.tr+15,label=y.pre.tr) +
      annotate("label", x=T/4, y=y.pre.co-15,label=y.pre.co) +
      annotate("label", x=T*0.75, y=y.exp.tr+15,label=y.exp.tr) +
      annotate("label", x=T*0.75, y=y.exp.co-15,label=y.exp.co)
  }
  gg
}  
