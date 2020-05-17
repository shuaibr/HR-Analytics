library(car)
source('./mylib2.r')


###########
###Case B #
###########

#Read-in data
pilg <- read.csv("extended_data.csv", header=TRUE)

names(pilg)
dim(pilg)
head(pilg)
tail(pilg)

attach(pilg)

univariate.stats(Profit9)
univariate.stats(Profit0)

# Missing data in this case may lead to biased estimates!
# Need to properly Handle Missing Data
# see NInc, IncImpt, MissingInc, NAge, AgeImpt, MissingAge
# already create last time

# Categorical versions for NAge and NInc
# note this can not be done with IncImpt and AgeImpt
# since these are a mix of levels and continuous
fNAge = factor(NAge)
fNInc = factor(NInc)

# Customer was retained
retain <- rep(1, length(pilg$Profit9))
retain[is.na(pilg$Profit0)] <- 0
head(retain)
length(retain)

detach(pilg)

# Now add all of the new varaibles to the data frame
# and remove the copies that remain in the workspace
pilg <- data.frame(pilg, retain, fNAge, fNInc)
rm(retain);  rm(fNAge); rm(fNInc);

attach(pilg)

# Run logistic logit regression on our most robust model to find
# out more about the drivers of cutomer retention.
lfit <- glm(retain ~ Profit9 + Online9 + Tenure9 + D1100 + D1200 + IncImpt + MissingInc + AgeImpt + MissingAge, family=binomial(link=logit))
#Interpretation of the results/coefficients
summary(lfit)
logLik(lfit)

cat("\nOverall Model Error Rate\n")
# overall error rate
predret <- rep(0,length(retain))
predret[lfit$fitted > 0.5] <- 1
err = abs(predret - retain)
sum(err)/length(err)

cat("\nTable of Predicted vs Actual\n")
table(predret, retain)

cat("\nTest of Overall Fit of the Model\n")
cat("\nDifference in Deviance: ")
lfit$null.deviance - lfit$deviance
cat("\nDegrees of Freedom of the Difference: ")
lfit$df.null - lfit$df.residual
cat("\nChiSquare test for the Overall Model\n")
dchisq(lfit$null.deviance-lfit$deviance, lfit$df.null-lfit$df.residual)
cat("\n\n")

cat("\nBootstrapped Estimate of Error Rate using Cross Validation\n")
library(boot)
val10fold <- cv.glm(pilg, glmfit = lfit, K = 10)
val10fold$delta


detach(pilg)

cat("\n\nTest if Online9 Associated with Higher Profit0 with controls including Profit9, etc\n")

# remove all non-retained values from the data set
npilg <- pilg
ds <- which(is.na(npilg$Profit0))
npilg <- npilg[-ds,]

attach(npilg)
univariate.stats(Profit0)
univariate.stats(Profit9)

# Run the base analysis for all controls and dummies for missing data
bfit <- lm(Profit0 ~ Online9 + D1100 + D1200 + AgeImpt + MissingAge + IncImpt + MissingInc + Tenure9 + Profit9)
summary(bfit)

# examine residuals
cat("\n\nExamine Statistics of Residuals")
univariate.stats(resid(bfit))

# check for normality
cat("\n\nGenerate a Histogram of Standardized Residuals\n")
stdres <- rstandard(bfit)
hist(stdres, plot=TRUE)

# Use Normality Plot
cat("\n\nGenerate a Normality Plot of Residuals\n")
qqnorm(stdres, ylab="Standardized Residuals", xlab="Normal Scores", main="Online Profitability Model")
qqline(stdres)

# make residual plots
cat("\n\nGenerate Residual Plots\n")
plot(AgeImpt, stdres, ylab="Std. Residuals", xlab="AgeImpt")
abline(0,0)

plot(IncImpt, stdres, ylab="Std. Residuals", xlab="IncImpt")
abline(0,0)

plot(Tenure9, stdres, ylab="Std. Residuals", xlab="Tenure9")
abline(0,0)

plot(Profit9, stdres, ylab="Std. Residuals", xlab="Profit9")
abline(0,0)

# Check for Heteroskedasticity
cat("\n\nUse Plot of Residuals vs Fitted to check for Heteroskedasticity\n")
plot(fitted(bfit), resid(bfit), ylab="Residuals", xlab="Fitted Values")
abline(0,0)

# check for outliers
cat("\n\nCooks Distance - Unusual Observations\n")
trm <- terms(bfit)
degfree <- length(attributes(trm)$term.labels) + attributes(trm)$intercept
cd <- cooks.distance(bfit)
pcd <- pf(cd,degfree,length(cd))
if (length(pcd[pcd>0.15]) > 0) {
    ds <-which(pcd > 0.15)
    print(ID[ds])
}

# check for multi-collinearity
cat("\n\nCheck for Multi-Collinearity Issues")
vlst <- cbind(Profit0, Online9, AgeImpt, MissingAge, IncImpt, MissingInc, Tenure9, D1100, D1200, Profit9)
cor(vlst)
vif(bfit)

detach(npilg)

cat("Use Trimming and repeat Test if Online9 Associated with Higher Profit0 with controls including Profit9, etc\n")
# Run the same analysis for all controls and dummies for missing data after winsorizing
wsdata <- npilg
ds <- NULL
tails = quantile(wsdata$Profit0, c(0.025, 0.975))
ds <- which(wsdata$Profit0 < tails[[1]])
wsdata <- wsdata[-ds,]
ds <- which(wsdata$Profit0 > tails[[2]])
wsdata <- wsdata[-ds,]

bfit2 <- lm(Profit0 ~ Online9 + D1100 + D1200 + fNAge + fNInc + Tenure9 + Profit9, data=wsdata)
summary(bfit2)

# examine residuals
cat("\n\nB - After Trimming: Examine Statistics of Residuals")
univariate.stats(resid(bfit2))

# check for normality
cat("\n\nB - After Trimming: Generate a Histogram of Standardized Residuals\n")
stdres <- rstandard(bfit2)
hist(stdres, plot=TRUE, main="After Trimming")

# Use Normality Plot
cat("\n\nB - After Trimming: Generate a Normality Plot of Residuals\n")
qqnorm(stdres, ylab="Standardized Residuals", xlab="Normal Scores", main="Trimmed Model")
qqline(stdres)

# make residual plots
cat("\n\nB - After Trimming: Generate Residual Plots\n")
plot(wsdata$NAge, stdres, ylab="Std. Residuals", xlab="NAge", main="After Trimming")
abline(0,0)

plot(wsdata$NInc, stdres, ylab="Std. Residuals", xlab="NInc", main="After Trimming")
abline(0,0)

plot(wsdata$Tenure9, stdres, ylab="Std. Residuals", xlab="Tenure9", main="After Trimming")
abline(0,0)

plot(wsdata$Profit9, stdres, ylab="Std. Residuals", xlab="Profit9", main="After Trimming")
abline(0,0)
