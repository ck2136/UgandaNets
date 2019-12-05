##############################################
# Data Analysis
# Created by: CK
# Created on: 2/10/2018
# Modified by: CK
# Modified on: 5/2/2018
##############################################

# - - - - - - - - - - - - - - - - - - - - - #
## 1. Loading Libraries and Data ----
# - - - - - - - - - - - - - - - - - - - - - #

rm(list=ls())
setwd("/home/ck1/Documents/Projects/DrColborn/UgandaNets/data/process")
df <- readRDS("final_hh_df.RDS")
# df <- readRDS("final_hh_df.RDS") # this is for when we have enough info on the survey 4 timepoint


# - - - - - - - - - - - - - - - - - - - - - #
## 2.GLM analysis: effect of netCov on rdtPos rate  ----
# - - - - - - - - - - - - - - - - - - - - - #

### 2.1 Simple Poisson Regression

#install.packages("sjPlot")
library(sjPlot)
library(sjmisc)
library(sjlabelled)
# simple poisson regression 

# recode data frame for pretty sjPlot output
df$time <- rec(
  df$stime,
  rec = c("1=1 [First]; 2=2 [Second]; 3=3 [Third]; 4=4 [Fourth]; else=NA"),
  var.label = "Survey Time",
  as.num = FALSE # we want a factor
)
attr(df$rdtPos_Sum, "label") <- "Number of RDT+ in household"

# first glm model accounting for time
glm1 <- glm(rdtPos_Sum ~ time + offset(log(num_hh_finals)), subset = (county == 2),
            data = df, family = poisson())
# try negative binomial to account for overdispersion
library(MASS)
library(ggplot2)
theme_set(theme_sjplot())
glm2 <- glm.nb(rdtPos_Sum ~ time + offset(log(num_hh_finals)), subset = (county == 2), data = df)
summary(glm1)

sjt.glm(glm1, depvar.labels = "RDT+", show.aic = T, show.family = T, show.dev = T, show.r2 = T)

# plot indicates survey time 3 compared to 1 has lower risk of Malaria incidence

AIC(glm2, glm1) # the fit for the negative binomial is worse here...

# now fit a model with rdtPos ~ netCov
glm3 <- glm(rdtPos_Sum ~ netCov + offset(log(num_hh_finals)), 
            subset = (county == 2), 
            data = df, family = poisson())
sjt.glm(glm3, depvar.labels = "RDT+", show.aic = T, show.family = T, show.dev = T, show.r2 = T)
# higher net coverage indicates lower RDT rate
# fit nb model
glm4 <- glm(micPos_Sum ~ netCov + offset(log(num_hh_finals)), 
            subset = (county == 2), 
            data = df)
sjt.glm(glm4, depvar.labels = "RDT+", show.aic = T, show.family = T, show.dev = T, show.r2 = T)

AIC(glm3, glm4) # again nb has a higher AIC... interseting also R^2 smaller

# add altitude
glm5 <- glm(rdtPos_Sum ~ as.factor(Netptwo) + offset(log(num_hh_finals)), 
               subset = (county == 2), 
               data = df,
            family = poisson("log"))
sjt.glm(glm5, depvar.labels = "RDT+", show.aic = T, show.family = T, show.dev = T, show.r2 = T)
AIC(glm3, glm4, glm5) # adding longitude and latitude doesn't improve the fit (makes it so much worse)

glm6 <- glm(rdtPos_Sum ~ num_net + offset(log(num_hh_finals)), 
            subset = (county == 2), 
            data = df,
            family = poisson("log"))
sjt.glm(glm6, depvar.labels = "RDT+", show.aic = T, show.family = T, show.dev = T, show.r2 = T)
# num_net not significant...

# now fit a model with netCov in addition to survey time
glm7 <- glm(rdtPos_Sum ~ netCov + time + netCov:time + offset(log(num_hh_finals)), 
            subset = (county == 2), 
            data = df,
            family = poisson(link="log"))
sjt.glm(glm7, depvar.labels = "RDT+", show.aic = T, show.family = T, show.dev = T, show.r2 = T)
# there is no significance between netCov or time or any interaction between the two terms
AIC(glm7, glm3) # the AIC for the interaction model is better compared to the simple model.

# now fit a model with slept_net in addition to survey time
glm8 <- glm(micPos_Sum ~ netCov + time + offset(log(num_hh_finals)), 
            subset = (county == 2), 
            data = df,
            family = poisson(link="log"))
sjt.glm(glm8, depvar.labels = "RDT+", show.aic = T, show.family = T, show.dev = T, show.r2 = T)
# there is no significance between slept_net or time or any interaction between the two terms
AIC(glm8, glm7) # the AIC for the interaction model is better compared to the simple model.

# now fit a model with altitude, evi, and temperature in addition to survey time
library(dplyr)
glm9 <- glm(rdtPos_Sum ~ netCov + time + netCov:time + alt_srtm + tempC  + EVI + offset(log(num_hh_finals)), 
            subset = (county == 2), 
            data = df %>%
              mutate(
                     EVI = base::scale(EVI_96),
                     #EVI = base::scale(EVI_112),
                     alt_srtm = base::scale(alt_srtm),
                     tempC = tempK_48 - 273.15
                     #tempC = tempK_64 - 273.15
              ),
            family = poisson(link="log"))
summary(glm9)
sjt.glm(glm9, depvar.labels = "RDT+", show.aic = T, show.family = T, show.dev = T, show.r2 = T)

#- using Microscopy with interaction
glm11 <- glm(micPos_Sum ~ slept_net_p + time + slept_net_p:time + tempC  + EVI + alt_srtm + offset(log(num_hh_finals)), 
            subset = (county == 2), 
            data = df %>%
              mutate(
                     EVI = base::scale(EVI_96),
                     #EVI = base::scale(EVI_112),
                     alt_srtm = base::scale(alt_srtm),
                     tempC = tempK_48 - 273.15
                     #tempC = tempK_64 - 273.15
              ),
            family = poisson(link="log"))
summary(glm11)

AIC(glm3, glm9, glm7, glm8, glm10)


#- using Microscopy with interaction
glm10 <- glm(micPos_Sum ~ oneNet + time + netCov:time + tempC  + EVI + offset(log(num_hh_finals)), 
            subset = (county == 2), 
            data = df %>%
              mutate(
                     EVI = base::scale(EVI_96),
                     #EVI = base::scale(EVI_112),
                     alt_srtm = base::scale(alt_srtm),
                     tempC = tempK_48 - 273.15
                     #tempC = tempK_64 - 273.15
              ),
            family = poisson(link="log"))
summary(glm10)

AIC(glm3, glm9, glm7, glm8, glm10)

# check each time points and see where netCov significant
tmp <- lapply(1:3, function(x) {
  glm(rdtPos_Sum ~ netCov + offset(log(num_hh_finals)), 
      subset = (county == 2 & stime == x), 
      data = df, family = poisson())
})

sjt.glm(tmp[[1]], tmp[[2]], tmp[[3]], depvar.labels = rep(c("RDT+"),3))
# only the 3rd time point is significant 

### 2.2 Zero inflated Poisson Regression
#install.packages("pscl")
library(pscl)
summary(zip1 <- zeroinfl(rdtPos_Sum ~ netCov + offset(log(num_hh_finals)), subset = county == 2,  data = df ))
AIC(glm3, zip1) # it seems that the ZIP model doesn't provide a benefit i.e. the AIC is higher

summary(zip2 <- zeroinfl(rdtPos_Sum ~ netCov:as.factor(stime) + offset(log(num_hh_finals)), subset = county == 2,  data = df ))
AIC(glm3, zip1, zip2) # it seems that the ZIP model doesn't provide a benefit i.e. the AIC is higher



# - - - - - - - - - - - - - - - - - - - - - #
## 3.Bayensian GLGM MCMC  ----
# - - - - - - - - - - - - - - - - - - - - - #

# Now it's time to account for spatial correlation (i.e. trend surface) 
# we are going to go straight into bayesian mode because it's more convenient

# first we need to load library
library(geoRglm)
library(geoR)

# we need to set up our data into geodata
geo_df <- as.geodata(df %>%
                     filter(!duplicated(latitude) & county == 2) %>%
                     mutate(stime = as.factor(stime), 
                            oneNet = as.factor(oneNet),
                            Netptwo = as.factor(Netptwo),
                            county = as.factor(county),
                            EVI_112 = as.vector(base::scale(EVI_112)),
                            alt_srtm = as.vector(base::scale(alt_srtm)),
                            tempC_64 = tempK_64 - 273.15
                            ) %>%
                     ungroup(),
                   coords.col = 7:6, data.col = c(3,4), # data.col c(15,16)/c(3,4) for rdtRate,micRate/rdtmicCout 
                   covar.col = c("stime","netCov","oneNet","Netptwo","slept_net_p","county","alt_srtm","tempC_64","EVI_112"),  
                   units.m.col = c("rdtPos_tested"),
                   na.action = "none") # columns 15 and 16 are rate of rdt+ and mic+ for survey 1
geo_df_slept <- as.geodata(df %>%
                     filter(!duplicated(latitude) & county == 2 & !is.na(slept_net_p)) %>%
                     mutate(stime = as.factor(stime), 
                            oneNet = as.factor(oneNet),
                            Netptwo = as.factor(Netptwo),
                            county = as.factor(county),
                            EVI_112 = as.vector(base::scale(EVI_112)),
                            alt_srtm = as.vector(base::scale(alt_srtm)),
                            tempC_64 = tempK_64 - 273.15
                            ) %>%
                     ungroup(),
                   coords.col = 7:6, data.col = c(3,4), # data.col c(15,16)/c(3,4) for rdtRate,micRate/rdtmicCout 
                   covar.col = c("stime","netCov","oneNet","Netptwo","slept_net_p","county","alt_srtm","tempC_64","EVI_112"),  
                   units.m.col = c("rdtPos_tested"),
                   na.action = "none") # columns 15 and 16 are rate of rdt+ and mic+ for survey 1
geo_dfm <- as.geodata(df %>%
                     filter(!duplicated(latitude) & county == 2 & micPos_tested > 0) %>%
                     mutate(stime= as.factor(stime), 
                            oneNet = as.factor(oneNet),
                            Netptwo = as.factor(Netptwo),
                            county = as.factor(county),
                            EVI_112 = as.vector(base::scale(EVI_112)),
                            alt_srtm = as.vector(base::scale(alt_srtm)),
                            tempC_64 = tempK_64 - 273.15
                            ) %>%
                     ungroup(),
                   coords.col = 7:6, data.col = c(3,4), # data.col c(15,16)/c(3,4) for rdtRate,micRate/rdtmicCout 
                   covar.col = c("stime","netCov","oneNet","Netptwo","slept_net_p","county","alt_srtm","tempC_64","EVI_112"),  
                   units.m.col = c("micPos_tested"),
                   na.action = "none") # columns 15 and 16 are rate of rdt+ and mic+ for survey 1
geo_dfm_slept <- as.geodata(df %>%
                     filter(!duplicated(latitude) & county == 2 & micPos_tested > 0 & !is.na(slept_net_p)) %>%
                     mutate(stime= as.factor(stime), 
                            oneNet = as.factor(oneNet),
                            Netptwo = as.factor(Netptwo),
                            county = as.factor(county),
                            EVI_112 = as.vector(base::scale(EVI_112)),
                            alt_srtm = as.vector(base::scale(alt_srtm)),
                            tempC_64 = tempK_64 - 273.15
                            ) %>%
                     ungroup(),
                   coords.col = 7:6, data.col = c(3,4), # data.col c(15,16)/c(3,4) for rdtRate,micRate/rdtmicCout 
                   covar.col = c("stime","netCov","oneNet","Netptwo","slept_net_p","county","alt_srtm","tempC_64","EVI_112"),  
                   units.m.col = c("micPos_tested"),
                   na.action = "none") # columns 15 and 16 are rate of rdt+ and mic+ for survey 1
names(geo_df) # check to makesure all components are set up well
names(geo_dfm) # check to makesure all components are set up well



# - - - - - - - - - - - - - - - - - - - - - #
## Model 1: Net Coverage and Stime only
# - - - - - - - - - - - - - - - - - - - - - #

# First specify the external trend and trend surface that we want to add into the model
t.all <- trend.spatial(geo_df, trend = ~netCov +  stime,  add.to.trend = "1st")
t.allm <- trend.spatial(geo_dfm, trend = ~netCov +  stime,  add.to.trend = "1st")
#t.all_n <- trend.spatial(geo_df, trend = ~netCov +  stime)
# Next we need to tune the S.scale and phi.scale (i.e. our variances) to an acceptance rate of 25~30% and 60% respectively once we go through this we cacn run the full length of MCMC
MCc <- mcmc.control(S.scale = 0.004, n.iter = 5000, thin = 100, phi.scale = 0.006)
MGc <- model.glm.control(trend.d = t.all, trend.l = t.all, kappa = 1)
MGcm <- model.glm.control(trend.d = t.allm, trend.l = t.allm, kappa = 1)
#MGc_n <- model.glm.control(trend.d = t.all_n, trend.l = t.all_n, kappa = 1)
OC <- output.glm.control(sim.pred = T, quantile = T)
# specify the priors here: phi ~ u(0,2) discretized as described in Diggle (2007). We are going to assume a relatively high measurement error(i.e. tau^2 = 2)
PGC <- prior.glm.control(phi.prior = "uniform" , phi = 0.014,
                         phi.discrete = seq(0, 2, by =0.02), tausq.rel = 1)
#PGC1 <- prior.glm.control(phi.prior = "squared.reciprocal" , phi = 1.5,
                         #phi.discrete = seq(0, 2, by =0.02), tausq.rel = 0.5)
# set seed for reproducibility
set.seed(371)
# run mcmc
pkb <- pois.krige.bayes(geo_df, prior = PGC, mcmc = MCc, model = MGc, data = geo_df$data[,2], output = OC)
pkbm <- pois.krige.bayes(geo_dfm, prior = PGC, mcmc = MCc, model = MGcm, data = geo_dfm$data[,2], output = OC)
#pkb1 <- pois.krige.bayes(geo_df, prior = PGC1, mcmc = MCc, model = MGc_n, data = geo_df$data[,1], output = OC)
# once we found the right acceptance rates for the parameters run full length 

# Now run the full length

# specify longer length
MCc <- mcmc.control(S.scale = 0.004, n.iter = 110000, thin = 100, burn.in = 10000, phi.scale = 0.006)
#MGc_n <- model.glm.control(trend.d = t.all_n, trend.l = t.all_n, kappa = 1)
# run full length
pkb <- pois.krige.bayes(geo_df, prior = PGC, mcmc = MCc, 
                        model = MGc, data = geo_df$data[,1], 
                        out = OC 
                        #locations = t.all # this is commented for now but need to work on this for prediction
)
pkbm <- pois.krige.bayes(geo_dfm, prior = PGC, mcmc = MCc, 
                        model = MGcm, data = geo_dfm$data[,2], 
                        out = OC 
                        #locations = t.all # this is commented for now but need to work on this for prediction
)
#pkb1 <- pois.krige.bayes(geo_df, prior = PGC, mcmc = MCc, 
                        #model = MGc_n, data = geo_df$data[,1], 
                        #out = OC 
                        ##locations = t.all # this is commented for now but need to work on this for prediction
#)
# create mcmc object then get the hpd 
pkb.mcmc.tune <- mcmc.control(S.scale = 0.005, thin = 1)
pkb.coda.b0 <- create.mcmc.coda(pkb$posterior$beta$sample[1,], mcmc.input = pkb.mcmc.tune) # check trace of b0
pkb.coda.b1 <- create.mcmc.coda(pkb$posterior$beta$sample[2,], mcmc.input = pkb.mcmc.tune) # check trace of b1 (i.e. netCov)
pkb1.coda.b0 <- create.mcmc.coda(pkb1$posterior$beta$sample[1,], mcmc.input = pkb.mcmc.tune) # check trace of b0
pkb1.coda.b1 <- create.mcmc.coda(pkb1$posterior$beta$sample[2,], mcmc.input = pkb.mcmc.tune) # check trace of b1 (i.e. netCov)

# plots of simulated parameter estimates
plot(pkb.coda.b0)
plot(pkb.coda.b1)
plot(pkb1.coda.b0)
plot(pkb1.coda.b1)
library(coda)
HPDinterval(pkb.coda.b1, prob = 0.95) #HPD of b1
HPDinterval(pkb1.coda.b0, prob = 0.95) #HPD of b1
HPDinterval(pkb1.coda.b1, prob = 0.95) #HPD of b1

# plot using just the original krig.bayes class
par(mfrow=c(1,2))
plot(pkb$posterior$phi$sample,type="l") 
acf(pkb$posterior$phi$sample) 
plot(pkb$posterior$sim[1,],type="l") 
acf(pkb$posterior$phi$sample) 
#for(i in 1:nrow(pkb$posterior$beta$sample)) {
  #plot(pkb$posterior$beta$sample[i,], type = "l")
  #acf(pkb$posterior$beta$sample[i,]) 
#}

saveRDS(pkb, "~/Documents/Projects/DrColborn/UgandaNets/data/results/pkb.RDS") # save for RMD usage
saveRDS(pkbm, "~/Documents/Projects/DrColborn/UgandaNets/data/results/pkbm.RDS") # save for RMD usage
#saveRDS(pkb1, "~/Documents/Projects/DrColborn/UgandaNets/data/results/pkb1.RDS") # save for RMD usage
#-- rdt as outcome
pkb<- readRDS("~/Documents/Projects/DrColborn/UgandaNets/data/results/pkb.RDS")
#-- mic as outcome
pkb<- readRDS("~/Documents/Projects/DrColborn/UgandaNets/data/results/pkbm.RDS")
#pkb<- readRDS("~/Documents/Projects/DrColborn/Uganda/data/pkb1.rds")
# use pkb1.RDS for model without surface trend

# create dataframe containing HPD and median for betas
rt <- sapply(1:nrow(pkb$posterior$beta$sample), function(x) {
  c(HPDinterval(create.mcmc.coda(pkb$posterior$beta$sample[x,],mcmc.input = pkb.mcmc.tune), prob = 0.95), median(create.mcmc.coda(pkb$posterior$beta$sample[x,],mcmc.input = pkb.mcmc.tune)))
})

# merge HPD of sigma^2 and phi
rt <- cbind(rt, c(HPDinterval(create.mcmc.coda(pkb$posterior$sigmasq$sample ,mcmc.input = pkb.mcmc.tune)), median(create.mcmc.coda(pkb$posterior$sigmasq$sample ,mcmc.input = pkb.mcmc.tune))))
rt <- cbind(rt, c(HPDinterval(create.mcmc.coda(pkb$posterior$phi$sample ,mcmc.input = pkb.mcmc.tune)), median(create.mcmc.coda(pkb$posteriore$phi$sample ,mcmc.input = pkb.mcmc.tune))))
# merge the means as row and exponentiate
rt <- exp(rbind(rt, c(pkb$posterior$beta$mean, pkb$posterior$sigmasq$mean, pkb$posterior$phi$mean)))
rownames(rt) <- c("95% LCI","95% UCI","Median","Mean")
colnames(rt) <- c("intercep","netCov","stime2","stime3","stime4","longitude","latitude", "sig^2","phi")
library(pander)
pander(t(rt), split.tbl = Inf)

# estimate and confidence interval for 50% increase in netCov
1 - exp(log(rt[,2])*0.5)
# IRR difference between stime 1 and 3 after accounting for other covariates
1 - exp(log(rt[,4]))


# - - - - - - - - - - - - - - - - - - - - - #
## Model 2: netCov stime and interaction
# - - - - - - - - - - - - - - - - - - - - - #

# First specify the external trend and trend surface that we want to add into the model
t.all <- trend.spatial(geo_df, trend = ~netCov +  stime + netCov*stime,  add.to.trend = "1st")
# Next we need to tune the S.scale and phi.scale (i.e. our variances) to an acceptance rate of 25~30% and 60% respectively once we go through this we cacn run the full length of MCMC
MCc <- mcmc.control(S.scale = 0.004, n.iter = 5000, thin = 100, phi.scale = 0.006)
MGc <- model.glm.control(trend.d = t.all, trend.l = t.all, kappa = 1)
OC <- output.glm.control(sim.pred = T, quantile = T)
# specify the priors here: phi ~ u(0,2) discretized as described in Diggle (2007). We are going to assume a relatively high measurement error(i.e. tau^2 = 2)
PGC <- prior.glm.control(phi.prior = "uniform" , phi = 0.004,
                         phi.discrete = seq(0, 2, by =0.02), tausq.rel = 2)
# set seed for reproducibility
set.seed(371)
# run mcmc
pkb <- pois.krige.bayes(geo_df, prior = PGC, mcmc = MCc, model = MGc, data = geo_df$data[,1], output = OC)

# once we found the right acceptance rates for the parameters run full length 

# Now run the full length

# specify longer length
MCc <- mcmc.control(S.scale = 0.004, n.iter = 110000, thin = 100, burn.in = 10000, phi.scale = 0.006)
# to specify the predictions on the locations specify spatial trend
MGc <- model.glm.control(trend.d = t.all, trend.l = t.all, kappa = 1)
# run full length
pkb <- pois.krige.bayes(geo_df, prior = PGC, mcmc = MCc, 
                        model = MGc, data = geo_df$data[,1], 
                        out = OC 
                        #locations = t.all # this is commented for now but need to work on this for prediction
)

# create mcmc object then get the hpd 
pkb.mcmc.tune <- mcmc.control(S.scale = 0.005, thin = 1)
pkb.coda.b0 <- create.mcmc.coda(pkb$posterior$beta$sample[1,], mcmc.input = pkb.mcmc.tune) # check trace of b0
pkb.coda.b1 <- create.mcmc.coda(pkb$posterior$beta$sample[2,], mcmc.input = pkb.mcmc.tune) # check trace of b1 (i.e. netCov)

# plots of simulated parameter estimates
plot(pkb.coda.b0)
plot(pkb.coda.b1)
library(coda)
HPDinterval(pkb.coda.b1, prob = 0.95) #HPD of b1

# plot using just the original krig.bayes class
par(mfrow=c(1,2))
plot(pkb$posterior$phi$sample,type="l") 
acf(pkb$posterior$phi$sample) 
plot(pkb$posterior$sim[1,],type="l") 
acf(pkb$posterior$phi$sample) 
#for(i in 1:nrow(pkb$posterior$beta$sample)) {
  #plot(pkb$posterior$beta$sample[i,], type = "l")
  #acf(pkb$posterior$beta$sample[i,]) 
#}

saveRDS(pkb, "~/Documents/Projects/DrColborn/UgandaNets/data/results/pkb_int.RDS") # save for RMD usage
pkb<- readRDS("~/Documents/Projects/DrColborn/UgandaNets/data/results/pkb_int.RDS")
#pkb<- readRDS("~/Documents/Projects/DrColborn/Uganda/data/pkb1.rds")
# use pkb1.RDS for model without surface trend

# create dataframe containing HPD and median for betas
rt <- sapply(1:nrow(pkb$posterior$beta$sample), function(x) {
  c(HPDinterval(create.mcmc.coda(pkb$posterior$beta$sample[x,],mcmc.input = pkb.mcmc.tune), prob = 0.95), median(create.mcmc.coda(pkb$posterior$beta$sample[x,],mcmc.input = pkb.mcmc.tune)))
})

# merge HPD of sigma^2 and phi
rt <- cbind(rt, c(HPDinterval(create.mcmc.coda(pkb$posterior$sigmasq$sample ,mcmc.input = pkb.mcmc.tune)), median(create.mcmc.coda(pkb$posterior$sigmasq$sample ,mcmc.input = pkb.mcmc.tune))))
rt <- cbind(rt, c(HPDinterval(create.mcmc.coda(pkb$posterior$phi$sample ,mcmc.input = pkb.mcmc.tune)), median(create.mcmc.coda(pkb$posteriore$phi$sample ,mcmc.input = pkb.mcmc.tune))))
# merge the means as row and exponentiate
rt <- exp(rbind(rt, c(pkb$posterior$beta$mean, pkb$posterior$sigmasq$mean, pkb$posterior$phi$mean)))
rownames(rt) <- c("95% LCI","95% UCI","Median","Mean")
colnames(rt) <- c("intercep","netCov","stime2","stime3", "stime4", "netCov*s2", "netCov*s3", "netCov*s4","longitude","latitude", "sig^2","phi")
pander(t(rt), split.tbl = Inf)

# estimate and confidence interval for 50% increase in netCov
1 - exp(log(rt[,2])*0.5)
# IRR difference between stime 1 and 3 after accounting for other covariates
1 - exp(log(rt[,4]))


# - - - - - - - - - - - - - - - - - - - - - #
## Model 3: netCov stime with Geospatial information
# - - - - - - - - - - - - - - - - - - - - - #
# run model with altitude and EVI_112, tempC_64

# First specify the external trend and trend surface that we want to add into the model
#t.all <- trend.spatial(geo_df, trend = ~netCov +  stime + netCov*stime + alt_srtm + EVI_112 + tempC_64,  add.to.trend = "1st")
t.all <- trend.spatial(geo_df_slept, trend = ~slept_net_p +  stime + slept_net_p*stime + alt_srtm + EVI_112 + tempC_64,  add.to.trend = "1st")
#t.allm <- trend.spatial(geo_dfm, trend = ~netCov +  stime + netCov*stime + alt_srtm + EVI_112 + tempC_64,  add.to.trend = "1st")
t.allm <- trend.spatial(geo_dfm_slept, trend = ~slept_net_p +  stime + slept_net_p*stime + alt_srtm + EVI_112 + tempC_64,  add.to.trend = "1st")
# Next we need to tune the S.scale and phi.scale (i.e. our variances) to an acceptance rate of 25~30% and 60% respectively once we go through this we cacn run the full length of MCMC
MCc <- mcmc.control(S.scale = 0.003, n.iter = 5000, thin = 100, phi.scale = 0.004)
MGc <- model.glm.control(trend.d = t.all, trend.l = t.all, kappa = 1)
MGcm <- model.glm.control(trend.d = t.allm, trend.l = t.allm, kappa = 1)
OC <- output.glm.control(sim.pred = T, quantile = T)
# specify the priors here: phi ~ u(0,2) discretized as described in Diggle (2007). We are going to assume a relatively high measurement error(i.e. tau^2 = 2)
PGC <- prior.glm.control(phi.prior = "uniform" , phi = 0.500,
                         phi.discrete = seq(0, 2, by =0.02), tausq.rel = 1.5)
# set seed for reproducibility
set.seed(371)
# run mcmc
pkb <- pois.krige.bayes(geo_df_slept, prior = PGC, mcmc = MCc, model = MGc, data = geo_df_slept$data[,1], output = OC)
pkbm <- pois.krige.bayes(geo_dfm_slept, prior = PGC, mcmc = MCc, model = MGcm, data = geo_dfm_slept$data[,2], output = OC)

# once we found the right acceptance rates for the parameters run full length 

# Now run the full length

# specify longer length
MCc <- mcmc.control(S.scale = 0.003, n.iter = 110000, thin = 100, burn.in = 10000, phi.scale = 0.006)
# run full length
pkb <- pois.krige.bayes(geo_df_slept, prior = PGC, mcmc = MCc, model = MGc, data = geo_df_slept$data[,1], output = OC)
pkbm <- pois.krige.bayes(geo_dfm_slept, prior = PGC, mcmc = MCc, model = MGcm, data = geo_dfm_slept$data[,2], output = OC)

# save the data
#saveRDS(pkb, "~/Documents/Projects/DrColborn/UgandaNets/data/results/pkb_geo_slept.RDS") # save for RMD usage
#saveRDS(pkbm, "~/Documents/Projects/DrColborn/UgandaNets/data/results/pkb_geom_slept.RDS") # save for RMD usage
# save the data
saveRDS(pkb, "~/Documents/Projects/DrColborn/UgandaNets/data/results/pkb_geo.RDS") # save for RMD usage
saveRDS(pkbm, "~/Documents/Projects/DrColborn/UgandaNets/data/results/pkb_geom.RDS") # save for RMD usage
#pkb<- readRDS("~/Documents/Projects/DrColborn/UgandaNets/data/results/pkb_geo.RDS")
#pkb<- readRDS("~/Documents/Projects/DrColborn/UgandaNets/data/results/pkb_geom.RDS")
pkb<- readRDS("~/Documents/Projects/DrColborn/UgandaNets/data/results/pkb_geo_slept.RDS")
#pkb<- readRDS("~/Documents/Projects/DrColborn/UgandaNets/data/results/pkb_geom_slept.RDS")

# get parameter estimates
# create dataframe containing HPD and median for betas
rt <- sapply(1:nrow(pkb$posterior$beta$sample), function(x) {
  c(HPDinterval(create.mcmc.coda(pkb$posterior$beta$sample[x,],mcmc.input = pkb.mcmc.tune), prob = 0.95), median(create.mcmc.coda(pkb$posterior$beta$sample[x,],mcmc.input = pkb.mcmc.tune)))
})

# merge HPD of sigma^2 and phi
rt <- cbind(rt, c(HPDinterval(create.mcmc.coda(pkb$posterior$sigmasq$sample ,mcmc.input = pkb.mcmc.tune)), median(create.mcmc.coda(pkb$posterior$sigmasq$sample ,mcmc.input = pkb.mcmc.tune))))
rt <- cbind(rt, c(HPDinterval(create.mcmc.coda(pkb$posterior$phi$sample ,mcmc.input = pkb.mcmc.tune)), median(create.mcmc.coda(pkb$posteriore$phi$sample ,mcmc.input = pkb.mcmc.tune))))
# merge the means as row and exponentiate
rt <- exp(rbind(rt, c(pkb$posterior$beta$mean, pkb$posterior$sigmasq$mean, pkb$posterior$phi$mean)))
rownames(rt) <- c("95% LCI","95% UCI","Median","Mean")
colnames(rt) <- c("intercep","slept_net_p", "stime2","stime3", "stime4", "slept_net_p:stime2" ,"slept_net_p:stime3" ,"slept_net_p:stime4" , "Altitude","EVI_112", "tempC_64","longitude","latitude", "sig^2","phi")
#colnames(rt) <- c("intercep","netCov", "stime2","stime3", "stime4", "netCov:stime2" ,"netCov:stime3" ,"netCov:stime4" , "Altitude","EVI_112", "tempC_64","longitude","latitude", "sig^2","phi")
library(pander)
pander(t(rt), split.tbl = Inf)

# The above modeling indicated that we should stick with either taking into account linear surface trend or no surface trend at all. Either way the below code will run through the estimation using various covariates

# - - - - - - - - - - - - - - - - - - - - - #
## Model 4: Everything from netCov, oneNet, Netptwo 
# - - - - - - - - - - - - - - - - - - - - - #

# Now do the parameter estimation using mutliple explanatory variables and for both rdtPos_Sum and micPos_Sum
temp <- geo_df # this is from the temporary.R file. 
tempm <- geo_dfm # this is from the temporary.R file. 
tmp1 <- lapply(c("netCov","oneNet","Netptwo"), function(x) {
  list(pois.krige.bayes(temp, 
                        prior = PGC, 
                        mcmc = MCc, 
                        model = model.glm.control(trend.d = trend.spatial(temp, trend = ~get(x) +  stime + get(x)*stime + alt_srtm + tempC_64 + EVI_112), trend.l = trend.spatial(temp, trend = ~get(x) +  stime + get(x)*stime + alt_srtm + tempC_64 + EVI_112), kappa = 1), 
                        data = temp$data[,1], 
                        output = OC),
       pois.krige.bayes(temp, 
                        prior = PGC, 
                        mcmc = MCc, 
                        model = model.glm.control(trend.d = trend.spatial(temp, trend = ~get(x) +  stime + get(x)*stime + alt_srtm + tempC_64 + EVI_112, add.to.trend = "1st"), trend.l = trend.spatial(temp, trend = ~get(x) +  stime + get(x)*stime + alt_srtm + tempC_64 + EVI_112, add.to.trend = "1st"), kappa = 1), 
                        data = temp$data[,1], 
                        output = OC),
       
       # For microscopy data
       pois.krige.bayes(tempm, 
                        prior = PGC, 
                        mcmc = MCc, 
                        model = model.glm.control(trend.d = trend.spatial(tempm, trend = ~get(x) +  stime + get(x)*stime + alt_srtm + tempC_64 + EVI_112), trend.l = trend.spatial(temp, trend = ~get(x) +  stime + get(x)*stime + alt_srtm + tempC_64 + EVI_112), kappa = 1), 
                        data = tempm$data[,2], 
                        output = OC),
       pois.krige.bayes(tempm, 
                        prior = PGC, 
                        mcmc = MCc, 
                        model = model.glm.control(trend.d = trend.spatial(tempm, trend = ~get(x) +  stime + get(x)*stime + alt_srtm + tempC_64 + EVI_112, add.to.trend = "1st"), trend.l = trend.spatial(temp, trend = ~get(x) +  stime + get(x)*stime + alt_srtm + tempC_64 + EVI_112, add.to.trend = "1st"), kappa = 1), 
                        data = tempm$data[,2], 
                        output = OC)
       )
})

# save the data
saveRDS(tmp1, "~/Documents/Projects/DrColborn/UgandaNets/data/results/pkb_allvarsint.RDS") # save for RMD usage
pkb_all <- readRDS("~/Documents/Projects/DrColborn/UgandaNets/data/results/pkb_allvars.RDS")
# load in the microscopy model (This object contains all of the other combination of covariates also)

# create dataframe containing HPD and median for betas
rt6 <- sapply(1:nrow(pkb_all[[1]][[4]]$posterior$beta$sample), function(x) {
  c(HPDinterval(create.mcmc.coda(pkb_all[[1]][[4]]$posterior$beta$sample[x,],mcmc.input = pkb.mcmc.tune), prob = 0.95), median(create.mcmc.coda(pkb_all[[1]][[4]]$posterior$beta$sample[x,],mcmc.input = pkb.mcmc.tune)))
})

# merge HPD of sigma^2 and phi
rt6 <- cbind(rt6, c(HPDinterval(create.mcmc.coda(pkb_all[[1]][[4]]$posterior$sigmasq$sample ,mcmc.input = pkb.mcmc.tune)), median(create.mcmc.coda(pkb_all[[1]][[4]]$posterior$sigmasq$sample ,mcmc.input = pkb.mcmc.tune))))
rt6 <- cbind(rt6, c(HPDinterval(create.mcmc.coda(pkb_all[[1]][[4]]$posterior$phi$sample ,mcmc.input = pkb.mcmc.tune)), median(create.mcmc.coda(pkb_all[[1]][[4]]$posteriore$phi$sample ,mcmc.input = pkb.mcmc.tune))))
# merge the means as row and exponentiate
rt6 <- exp(rbind(rt6, c(pkb_all[[1]][[4]]$posterior$beta$mean, pkb_all[[1]][[4]]$posterior$sigmasq$mean, pkb_all[[1]][[4]]$posterior$phi$mean)))
rownames(rt6) <- c("95% LCI","95% UCI","Median","Mean")
colnames(rt6) <- c("intercep","netCov","stime2","stime3", "stime4","longitude","latitude", "sig^2","phi")

# exponentiate to get IRR
pander(t(rt6), split.tbl = Inf)
# est and 95% CI for 50% inc in NetCov
1 - exp(log(rt6[,2])*0.5)
# IRR difference between stime 1 and 3 after accounting for other covariates (outcome = mic+)
1 - exp(log(rt6[,4]))
