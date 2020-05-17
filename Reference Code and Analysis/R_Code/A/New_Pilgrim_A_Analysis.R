library(car)
source('./mylib2.r')


###########
###Case A #
###########

#Read-in data
pilg <- read.csv("pilgrim_bank_data_1.csv", header=TRUE)

names(pilg)
dim(pilg)
head(pilg)
tail(pilg)

# look at your data

# first take a look at a histogram
hist(pilg$Profit9)

# plot the empirical cumulative distribution function
plot(ecdf(pilg$Profit9))

# examine some basic pairwise plots, Co-variance matrix (but remember multivariate problem)
pairs(~Age9+Inc9+Tenure9+Profit9,data=pilg[sample(nrow(pilg),100),],main="Plots")

# District9 should be considered a factor
pilg$fDistrict9 <- as.factor(pilg$District9)

attach(pilg)

# runs sum basic summary statistics

# note that Age9 and Inc9 both have a significant percentage of missing values
basic.stats(Profit9)
basic.stats(Online9)
basic.stats(Age9)
basic.stats(Inc9)
basic.stats(Tenure9)

# Preliminary Analysis

#Define and analyse an initial model
onl_prof	<- lm(Profit9 ~ Online9)
summary(onl_prof)

#Define and analyse an initial model + age
onl_prof	<- lm(Profit9 ~ Online9 + Age9)
summary(onl_prof)

#Define and analyse an initial model + age + income
onl_prof	<- lm(Profit9 ~ Online9 + Age9 + Inc9)
summary(onl_prof)

# Missing data in this case may lead to biased estimates!
# Need to properly Handle Missing Data

# Code up for missing values of Age
MissingAge <- rep(0,length(Age9))
MissingAge[is.na(Age9)] <- 1

# Is distribution of missing age values related to profitability
quantile(Profit9, prob=c(0.10, 0.20, 0.30, 0.40, 0.50, 0.60, 0.70, 0.80, 0.90, 1.00))

# using library dplyr
# library(dplyr)
# profitdeciles <- ntile(Profit9, 10)
# aggregate(MissingAge, by=list(profitdeciles), sum)

# using mylib2.r (already sourced)
profitdeciles <- groups(Profit9, 10)
groupfunc(MissingAge,profitdeciles,sum)
groupfunc(Online9,profitdeciles,sum)
groupfunc(Tenure9, profitdeciles, mean)

# so more low profit customers are missing data that high profit customers
# why is this happening?  does this seem logical

# alternatively run simple test to see if missing obs are differnt
agetest <- lm(Profit9 ~ MissingAge)
summary(agetest)

# one simplistic approach, replace with mean value
NAge <- Age9
NAge[is.na(Age9)] <- mean(Age9, na.rm=TRUE)

# A better approach is to build a simple imputation model
# which assumes missing will depend on other X values
# 1. build a prediction model of the missing ages
fitAge <- lm(Age9 ~ Online9 + Tenure9 + fDistrict9 + Billpay9)
# 2. predict missing values using the rpedict function
AgeImpt <- Age9
ndata <- pilg[is.na(Age9),c("Online9", "Tenure9", "fDistrict9", "Billpay9")]
AgeImpt[is.na(Age9)] <- predict.lm(fitAge, ndata)
plot(AgeImpt,  NAge)
 
 
# first use the mean value and dummy
# Code up for missing value of Income
MissingInc <- rep(0, length(Inc9))
MissingInc[is.na(Inc9)] <- 1
NInc <- Inc9
NInc[is.na(Inc9)] <- mean(Inc9, na.rm=TRUE)

# build  simple imputation model
fitInc <- lm(Inc9 ~ Online9 + Tenure9 + fDistrict9 + Billpay9)
IncImpt <- Inc9
ndata <- pilg[is.na(Inc9),c("Online9", "Tenure9", "fDistrict9", "Billpay9")]
IncImpt[is.na(Inc9)] <- predict.lm(fitInc, ndata)
plot(IncImpt, NInc)

# run test to see if missing obs are different
inctest <- lm(Profit9 ~ MissingInc)
summary(inctest)

# Need to add full controls (all factors that may have an impact on profitability
# Code up Dummy Variables for District
levels(fDistrict9)
D1100 <- rep(0,length(District9))
D1100[fDistrict9 == "1100"] <- 1
basic.stats(D1100)
D1200 <- rep(0,length(District9))
D1200[fDistrict9 == "1200"] <- 1
basic.stats(D1200)

detach(pilg)

# Now add all of the new varaibles to the data frame
# and remove the copies that remain in the workspace

pilg <- data.frame(pilg, NAge, AgeImpt, MissingAge, NInc, IncImpt, MissingInc, D1100, D1200)

# now write out the extended dataset for later use
write.csv(pilg,file="extended_data.csv")

# clean up leftovers
rm(NAge)
rm(AgeImpt)
rm(MissingAge)
rm(NInc)
rm(IncImpt)
rm(MissingInc)
rm(D1100)
rm(D1200)
rm(profitdeciles)

attach(pilg)

sum(is.na(Profit9))
sum(is.na(Online9))
sum(is.na(D1100))
sum(is.na(D1200))
sum(is.na(AgeImpt))
sum(is.na(MissingAge))
sum(is.na(IncImpt))
sum(is.na(MissingInc))
sum(is.na(Tenure9))

#Run the same analysis for all controls and dummies for missing data
onl_prof <- lm(Profit9 ~ Online9 + D1100 + D1200 + AgeImpt + MissingAge + IncImpt + MissingInc + Tenure9)
summary(onl_prof)

# examine residuals
cat("\n\nExamine Statistics of Residuals")
univariate.stats(resid(onl_prof))

# check for normality
cat("\n\nGenerate a Histogram of Standardized Residuals\n")
stdres <- rstandard(onl_prof)
hist(stdres, plot=TRUE, main="Main Model")

# Use Normality Plot
cat("\n\nGenerate a Normality Plot of Residuals\n")
qqnorm(stdres, ylab="Standardized Residuals", xlab="Normal Scores", main="Main Model")
qqline(stdres)

# make residual plots
cat("\n\nGenerate Residual Plots\n")
plot(AgeImpt, rstandard(onl_prof), main="Main Model", ylab="Std. Residuals", xlab="AgeImpt")
abline(0,0)

plot(IncImpt, rstandard(onl_prof), main="Main Model", ylab="Std. Residuals", xlab="NewInc")
abline(0,0)

plot(Tenure9, rstandard(onl_prof), main="Main Model", ylab="Std. Residuals", xlab="Tenure9")
abline(0,0)

# Check for Heteroskedasticity
cat("\n\nUse Plot of Residuals vs Fitted to check for Heteroskedasticity\n")
plot(fitted(onl_prof), resid(onl_prof), main="Main Model", ylab="Residuals", xlab="Fitted Values")
abline(0,0)

# check for outliers
cat("\n\nCooks Distance - Unusual Observations\n")
trm <- terms(onl_prof)
degfree <- length(attributes(trm)$term.labels) + attributes(trm)$intercept
cd <- cooks.distance(onl_prof)
pcd <- pf(cd,degfree,length(cd))
if (length(pcd[pcd>0.15]) > 0) {
    ds <-which(pcd > 0.15)
    print(ID[ds])
}

# check for multi-collinearity
cat("\n\nMain Model: Check for Multi-Collinearity Issues")
vlst <- cbind(Profit9, Online9, AgeImpt, MissingAge, IncImpt, MissingInc, Tenure9, D1100, D1200)
cor(vlst)
vif(onl_prof)


# Residuals truly are not normally distributed due to long right tail of Profit
# Try simple Rank Regression to see if it shows anything 
# Use factors (dummy variables) for level of age and level of income

cat("\n\nAlt Model: Replace Profit9 with its Rank")
PRank <- rank(Profit9)
fNAge <- factor(NAge)
fNInc <- factor(NInc)
altmdl <- lm(PRank ~ Online9 + fNAge + fNInc + Tenure9 + D1100 + D1200)
summary(altmdl)
univariate.stats(resid(altmdl))
stdres <- rstandard(altmdl)
hist(stdres, plot=TRUE, main="Alt Model")
qqnorm(stdres, ylab="Standardized Residuals", xlab="Normal Scores", main="Alt Model")
qqline(stdres)


# Result is driven by long right tail of Profits, anything that 
# helps to pick out the big winners will show up as significant

cat("\n\nAlt Model 2 - Using categorical factors for Age and Income")

altmdl2 <- lm(Profit9 ~ Online9 + D1100 + D1200 + fNAge + fNInc + Tenure9)
summary(altmdl2)

# examine residuals
cat("\n\nAlt Model 2: Examine Statistics of Residuals")
univariate.stats(resid(altmdl2))

# check for normality
cat("\n\nAlt Model 2: Generate a Histogram of Standardized Residuals\n")
stdres <- rstandard(altmdl2)
hist(stdres, plot=TRUE, main="Alt Model2")

# Use Normality Plot
cat("\n\nGenerate a Normality Plot of Residuals for Alt Model 2\n")
qqnorm(stdres, ylab="Standardized Residuals", xlab="Normal Scores", main="Alt Model 2 Norm Plot")
qqline(stdres)
