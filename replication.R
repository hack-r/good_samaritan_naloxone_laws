
# Libraries and Options ---------------------------------------------------
# set working directory
setwd("jle_gs_nal/")

# libaries
require(foreign)
require(plm)
require(Matching)

# Set random seed
#   Note: A seed was not set in the original author's Stata code
#         by default, Stata uses a random seed of 123456789 
set.seed(123456789)

# Import Data -------------------------------------------------------------
# Original Data (DTA format)
download.file("https://www.journals.uchicago.edu/doi/suppl/10.1086/700703/suppl_file/9065Data.zip","9065Data.zip")

# Convert format
print("The data was converted from Stata 15 DTA format using Pandas in Python, i.e. pandas.io.stata.read_stata('Data.dta')")
data <- read.csv("original/Data_from_dta.csv")

# Replicate original data shape for plots
data.year <- aggregate(cbind(population, newopioiddeath, newheroindeaths, new_nonheroinopioiddeath) ~ year, 
                       sum,data=data)

data.year$opioid_rate    <- (data.year$newopioiddeath/data.year$population)*100000
data.year$heroin_rate    <- (data.year$newheroindeaths/data.year$population)*100000
data.year$nonheroin_rate <- (data.year$new_nonheroinopioiddeath/data.year$population)*100000

saveRDS(data.year, "data.year.RDS")

# Notes -------------------------------------------------------------------
# Table 1: List of NAL laws
# Table 2: List of Good Samaritan laws

# Replication of Original Table 3 -----------------------------------------
# Simple OLS
ols1.t3 <- glm("lnnewopioiddeath_rate ~ nal + gsl + lnpop + I(as.factor(fips)) + I(as.factor(year))", data = data, weights=data$population)
summary(ols1.t3) # matches original
saveRDS(ols1.t3,"data/ols1.t3.RDS")

# Kitchen sink OLS
ols2.t3 <- glm("lnnewopioiddeath_rate ~ nal + gsl + lnpop + pdmp + lnpolice_percapita + mml + lnbeer + lncig + lncollege + lnpci + lnunemp + lnmw + I(as.factor(fips)) + I(as.factor(year))", data=data, weights=data$population)  
summary(ols2.t3) # matches original
saveRDS(ols2.t3,"data/ols2.t3.RDS")

# Simple Poisson
poi1.t3 <- glm("newopioiddeath ~ nal + gsl + lnpop + I(as.factor(fips)) + I(as.factor(year))", data=data, family= "poisson", weights=data$population)
summary(poi1.t3) # matches original
saveRDS(poi1.t3,"data/poi1.t3.RDS")

# Kitchen sink Poisson
poi2.t3 <- glm("newopioiddeath ~ nal + gsl + lnpop + pdmp + lnpolice_percapita + mml + lnbeer + lncig + lncollege + lnpci + lnunemp + lnmw + I(as.factor(fips)) + I(as.factor(year))", data=data, family= "poisson", weights=data$population)
summary(poi2.t3) # matches original
saveRDS(poi2.t3, "data/poi2.t3.RDS")

# Replication of Original Table 4 -----------------------------------------
ols1.t4 <- glm("lnnewopioiddeath_rate ~ Lead4year + Lead3year + Lead2year + Lead1year + YrStateLawChange + 
            Lag1year + Lag2year + Lag3plusyear + gsl + mml + pdmp + lncollege + lnmw + lnpci + lnunemp + lnpolice_percapita + lnbeer + 
            lncig + lnpop + I(as.factor(fips)) + I(as.factor(year))", data = data, weights=data$population)
summary(ols1.t4) # matches original estimates
saveRDS(ols1.t4, "data/ols1.t4.RDS")

poi1.t4 <- glm("newopioiddeath ~ Lead4year + Lead3year + Lead2year + Lead1year + YrStateLawChange + 
            Lag1year + Lag2year + Lag3plusyear + gsl + mml + pdmp + lncollege + lnmw + lnpci + lnunemp + lnpolice_percapita + lnbeer + 
            lncig + lnpop + I(as.factor(fips)) + I(as.factor(year))", data = data, 
               weights=as.numeric(data$population), # change class to avoid integer overflow 
               family="poisson")
summary(poi1.t4) # matches original estimates
saveRDS(poi1.t4, "data/poi1.t4.RDS")

# Replication of Original Table 5 -----------------------------------------
#reg ln_newhdr nal gsl pdmp lnpolice_percapita mml lnbeer lncig lncollege lnpci lnunemp lnmw lnpop i.fips i.year [pw=population], cl(fips) //column 1 (heroin)
ols1.t5 <- glm("ln_newhdr ~ nal + gsl + pdmp + lnpolice_percapita + mml + lnbeer + lncig +
               lncollege + lnpci + lnunemp + lnmw + lnpop + I(as.factor(fips)) + I(as.factor(year))",data=data,weights=as.numeric(data$population)) #//column 1 (heroin)
summary(ols1.t5) # does not match - mistake in original?
saveRDS(ols1.t5, "data/ols1.t5.RDS")

#poisson newheroindeaths nal gsl pdmp lnpolice_percapita mml lnbeer lncig lncollege lnpci lnunemp lnmw lnpop i.year i.fips [pw=population], cl(fips) //column 2 (heroin)
poi1.t5 <- glm("newheroindeaths ~ nal + gsl + pdmp + lnpolice_percapita + mml + lnbeer + lncig + 
               lncollege + lnpci + lnunemp + lnmw + lnpop + I(as.factor(fips)) + 
               I(as.factor(year))",data=data,weights=as.numeric(data$population),family="poisson")
summary(poi1.t5) # matches original estimates
saveRDS(poi1.t5, "data/poi1.t5.RDS")

# reg ln_newnhdr nal gsl pdmp lnpolice_percapita mml lnbeer lncig lncollege lnpci lnunemp lnmw lnpop i.fips i.year [pw=population], cl(fips) //column 3 (other opioids (non-heroin))
ols2.t5 <- glm("ln_newnhdr ~ nal + gsl + pdmp + lnpolice_percapita + mml + lnbeer + lncig + lncollege + lnpci + lnunemp + lnmw + lnpop + I(as.factor(fips)) + I(as.factor(year))",data=data,weights=as.numeric(data$population)) #//column 1 (heroin)
summary(ols2.t5) # matches original estimates
saveRDS(ols2.t5, "data/ols2.t5.RDS")

# poisson new_nonheroinopioiddeath nal gsl pdmp lnpolice_percapita mml lnbeer lncig lncollege lnpci lnunemp lnmw lnpop i.fips i.year [pw=population], cl(fips) //column 4 (other opioids (non-heroin))
poi2.t5 <- glm("new_nonheroinopioiddeath ~ nal + gsl + pdmp + lnpolice_percapita + mml + lnbeer + lncig + lncollege + lnpci + lnunemp + lnmw + lnpop + I(as.factor(fips)) + I(as.factor(year))",data=data,weights=as.numeric(data$population),family="poisson") #//column 1 (heroin)
summary(poi2.t5) # matches original estimates
saveRDS(poi2.t5, "data/poi2.t5.RDS")

# Replication of Original Table 6 -----------------------------------------
# reg lnnewopioiddeath_rate nal_crim_poss nal_doc_immunity nal_standing_order nal_third_party gsl pdmp lnpolice_percapita mml lnbeer lncig lncollege lnpci lnunemp lnmw lnpop i.fips i.year [pw=population], cl(fips) //column 1 (all opioids)
ols1.t6 <- glm("lnnewopioiddeath_rate ~ nal_crim_poss + nal_doc_immunity + nal_standing_order + nal_third_party + gsl + pdmp + lnpolice_percapita + mml + lnbeer + lncig +
               lncollege + lnpci + lnunemp + lnmw + lnpop + I(as.factor(fips)) + I(as.factor(year))",data=data,weights=as.numeric(data$population)) #//column 1 (heroin)
summary(ols1.t6) # matches original estimates
saveRDS(ols1.t6, "data/ols1.t6.RDS")

# poisson newopioiddeath nal_doc_immunity nal_crim_poss nal_standing_order nal_third_party gsl pdmp lnpolice_percapita mml lnbeer lncig lncollege lnpci lnunemp lnmw lnpop i.fips i.year [pw=population], cl(fips) //column 2 (all opioids)
poi1.t6 <- glm("newopioiddeath ~ nal_crim_poss + nal_doc_immunity + nal_standing_order + nal_third_party + gsl + pdmp + lnpolice_percapita + mml + lnbeer + lncig +
               lncollege + lnpci + lnunemp + lnmw + lnpop + I(as.factor(fips)) + I(as.factor(year))",data=data,weights=as.numeric(data$population),family="poisson") #//column 1 (heroin)
summary(poi1.t6) # matches original estimates
saveRDS(poi1.t6, "data/poi1.t6.RDS")

# reg ln_newhdr nal_doc_immunity nal_crim_poss nal_standing_order nal_third_party gsl pdmp lnpolice_percapita mml lnbeer lncig lncollege lnpci lnunemp lnmw lnpop i.fips i.year [pw=population], cl(fips) //column 3 (heroin)
ols2.t6 <- glm("ln_newhdr ~ nal_crim_poss + nal_doc_immunity + nal_standing_order + nal_third_party + gsl + pdmp + lnpolice_percapita + mml + lnbeer + lncig +
               lncollege + lnpci + lnunemp + lnmw + lnpop + I(as.factor(fips)) + I(as.factor(year))",data=data,weights=as.numeric(data$population)) #//column 1 (heroin)
summary(ols2.t6) # close but different
saveRDS(ols2.t6, "data/ols2.t6.RDS")

# poisson newheroindeaths nal_doc_immunity nal_crim_poss nal_standing_order nal_third_party gsl pdmp lnpolice_percapita mml lnbeer lncig lncollege lnpci lnunemp lnmw lnpop i.year i.fips [pw=population], cl(fips) //column 4 (heroin)
poi2.t6 <- glm("newheroindeaths ~ nal_doc_immunity + nal_crim_poss + nal_standing_order + nal_third_party + gsl + pdmp + lnpolice_percapita + mml + lnbeer + lncig +
               lncollege + lnpci + lnunemp + lnmw + lnpop + I(as.factor(fips)) + I(as.factor(year))",data=data,weights=as.numeric(data$population),family="poisson") #//column 1 (heroin)
summary(poi2.t6) # matches original estimates
saveRDS(poi2.t6, "data/poi2.t6.RDS")

# reg ln_newnhdr nal_doc_immunity nal_crim_poss nal_standing_order nal_third_party gsl pdmp lnpolice_percapita mml lnbeer lncig lncollege lnpci lnunemp lnmw lnpop i.fips i.year [pw=population], cl(fips) //column 5 (non-heroin)
ols3.t6 <- glm("ln_newnhdr ~ nal_crim_poss + nal_doc_immunity + nal_standing_order + nal_third_party + gsl + pdmp + lnpolice_percapita + mml + lnbeer + lncig +
               lncollege + lnpci + lnunemp + lnmw + lnpop + I(as.factor(fips)) + I(as.factor(year))",data=data,weights=as.numeric(data$population)) #//column 1 (heroin)
summary(ols3.t6) # matches original estimates
saveRDS(ols3.t6, "data/ols3.t6.RDS")

# poisson new_nonheroinopioiddeath nal_doc_immunity nal_crim_poss nal_standing_order nal_third_party gsl pdmp lnpolice_percapita mml lnbeer lncig lncollege lnpci lnunemp lnmw lnpop i.fips i.year [pw=population], cl(fips) //column 6 (non-heroin)
poi3.t6 <- glm("new_nonheroinopioiddeath ~ nal_doc_immunity + nal_crim_poss + nal_standing_order + nal_third_party + gsl + pdmp + lnpolice_percapita + mml + lnbeer + lncig +
               lncollege + lnpci + lnunemp + lnmw + lnpop + I(as.factor(fips)) + I(as.factor(year))",data=data,weights=as.numeric(data$population),family="poisson") #//column 1 (heroin)
summary(poi3.t6) # matches original estimates
saveRDS(poi3.t6, "data/poi3.t6.RDS")

# Replication of Original Table 7 -----------------------------------------
# //early-mid-late
# gen nal_early=nal if inlist(fips,6,9,17,35,53) //early-adopting states
# replace nal_early=0 if nal_early==.
data$nal_early <- ifelse(data$fips %in% c(6,9,17,35,53),data$nal,0)
# gen nal_mid=nal if inlist(fips,25,44) //mid-adopting states
# replace nal_mid=0 if nal_mid==.
data$nal_mid <- ifelse(data$fips %in% c(25,44),data$nal,0)
# gen nal_late=nal if inlist(fips,8,10,11,13,21,23,24,26,27,34,36,37,39,40,41,42,47,49,50,51,55) //late-adopting states
# replace nal_late=0 if nal_late==.
data$nal_late <- ifelse(data$fips %in% c(8,10,11,13,21,23,24,26,27,34,36,37,39,40,41,42,47,49,50,51,55),data$nal,0)
saveRDS(data, "data/data.RDS")

# reg lnnewopioiddeath_rate nal_early nal_mid nal_late gsl lnpop pdmp lnpolice_percapita mml lnbeer lncig lncollege lnpci lnunemp lnmw i.fips i.year [pw=population], cl(fips)
ols1.t7 <- glm("lnnewopioiddeath_rate ~ nal_early + nal_mid + nal_late + gsl + pdmp + lnpolice_percapita + mml + lnbeer + lncig +
               lncollege + lnpci + lnunemp + lnmw + lnpop + I(as.factor(fips)) + I(as.factor(year))",data=data,weights=as.numeric(data$population)) #//column 1 (heroin)
summary(ols1.t7) # matches original estimates
saveRDS(ols1.t7, "data/ols1.t7.RDS")

# poisson newopioiddeath nal_early nal_mid nal_late gsl pdmp lnpolice_percapita mml lnbeer lncig lncollege lnpci lnunemp lnmw lnpop i.fips i.year [pw=population], cl(fips)
poi1.t7 <- glm("newopioiddeath ~ nal_early + nal_mid + nal_late + gsl + pdmp + lnpolice_percapita + mml + lnbeer + lncig +
               lncollege + lnpci + lnunemp + lnmw + lnpop + I(as.factor(fips)) + I(as.factor(year))",data=data,weights=as.numeric(data$population),family="poisson") #//column 1 (heroin)
summary(poi1.t7) # matches original estimates
saveRDS(poi1.t7, "data/poi1.t7.RDS")


# Temp New Code Area ------------------------------------------------------
tr.nal <- ifelse(data$nal==1,1,0)
X.nal  <- data[,!colnames(data) %in% c("nal_late","nal_early","nal_mid", "gsl","newopioiddeath",
                                       "newopioiddeath_rate", "newheroindeaths", "newheroindeaths_rate",
                                       "ln_newhdr", "ln_newnhdr", "new_nonheroinopioiddeath_rate",
                                       )]
ps.nal <- glm(tr.nal ~ X.nal[,1] + X.nal[,2] + X.nal[,3],family="binomial")
summary(ps.nal)

match.nal <- Match(X             = X.nal,
                   Y             = data$lnnewopioiddeath_rate,
                   Tr            = tr.nal,
                   CommonSupport = T,
                   estimand      = "ATT",
                   M             = 2,
                   exact         = F,
                   replace       = T,
                   ties          = T
                   )
summary(match.nal)

# Simple OLS
new.ols1.t3 <- glm("lnnewopioiddeath_rate ~ nal + gsl + lnpop + I(as.factor(fips)) + I(as.factor(year))", data = data, weights=data$population)
summary(new.ols1.t3) # matches original


# Kitchen sink OLS
new.ols2.t3 <- glm("lnnewopioiddeath_rate ~ nal + gsl + lnpop + pdmp + lnpolice_percapita + mml + lnbeer + lncig + lncollege + lnpci + lnunemp + lnmw + I(as.factor(fips)) + I(as.factor(year))", data=data, weights=data$population)  
summary(new.ols2.t3) # matches original
saveRDS(new.ols2.t3,"data/new.ols2.t3.RDS")

# Simple Poisson
poi1.t3 <- glm("newopioiddeath ~ nal + gsl + lnpop + I(as.factor(fips)) + I(as.factor(year))", data=data, family= "poisson", weights=data$population)
summary(poi1.t3) # matches original
saveRDS(poi1.t3,"data/poi1.t3.RDS")

# Kitchen sink Poisson
poi2.t3 <- glm("newopioiddeath ~ nal + gsl + lnpop + pdmp + lnpolice_percapita + mml + lnbeer + lncig + lncollege + lnpci + lnunemp + lnmw + I(as.factor(fips)) + I(as.factor(year))", data=data, family= "poisson", weights=data$population)
summary(poi2.t3) # matches original
saveRDS(poi2.t3, "data/poi2.t3.RDS")



# Code to recover cluster standard errors:

# # compute Stata like df-adjustment
# G <- length(unique(p.df$year))
# N <- length(p.df$year)
# dfa <- (G/(G - 1)) * (N - 1)/pm1$df.residual
#
# # display with cluster VCE and df-adjustment
# time_c_vcov <- dfa * vcovHC(pm1, type = "HC0", cluster = "time", adjust = T)
# coeftest(pm1, vcov = time_c_vcov)
# ##

