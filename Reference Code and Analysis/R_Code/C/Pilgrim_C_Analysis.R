library(car)
source('./mylib2.r')


###########
###Case C #
###########

#Read-in data
pilg <- read.csv("extended_data.csv", header=TRUE)

names(pilg)
dim(pilg)
head(pilg)
tail(pilg)

attach(pilg)

# note that Age9 and Inc9 both have a significant percentage of missing values
basic.stats(Billpay9)
basic.stats(Billpay0)
basic.stats(Online9)
basic.stats(Online0)

# Need to properly Handle Missing Data
# Missing data already imputed 

# Customer Retention
retain <- rep(1, length(pilg$Profit9))
retain[is.na(pilg$Profit0)] <- 0
head(retain)
length(retain)
basic.stats(retain)

# Categorical versions for NAge and NInc
# note this can not be done with IncImpt and AgeImpt
# since these are a mix of levels and continuous
fNAge = factor(NAge)
fNInc = factor(NInc)

# Add New Variables for State Transition Analysis
# State is Tuple: (Online, BillPay) or Out

B00 = rep(0, length(Profit9))
B10 = rep(0, length(Profit9))
B11 = rep(0, length(Profit9))

B00[((Online9 == 0) & (Billpay9 == 0))] <- 1
B10[((Online9 == 1) & (Billpay9 == 0))] <- 1
B11[((Online9 == 1) & (Billpay9 == 1))] <- 1
sum(B00)
100.0 * sum(B00)/length(Profit9)
sum(B10)
100.0 * sum(B10)/length(Profit9)
sum(B11)
100.0 * sum(B11)/length(Profit9)


E00 = rep(0, length(Profit9))
E10 = rep(0, length(Profit9))
E11 = rep(0, length(Profit9))
EOut = rep(0, length(Profit9))

E00[(retain == 1) & (Online0 == 0) & (Billpay0 == 0)] <- 1
E10[(retain == 1) & (Online0 == 1) & (Billpay0 == 0)] <- 1
E11[(retain == 1) & (Online0 == 1) & (Billpay0 == 1)] <- 1
EOut[(retain == 0)] <- 1
sum(E00)
100.0 * sum(E00)/length(Profit9)
sum(E10)
100.0 * sum(E10)/length(Profit9)
sum(E11)
100.0 * sum(E11)/length(Profit9)
sum(EOut)
100.0 * sum(EOut)/length(Profit9)

#Calculate Conditional State Transition Probabilities and  Number of Transitions
sum(B00 == 1 & E00 == 1)
100.0 * sum(B00 == 1 & E00 == 1) / sum(B00)

sum(B00 == 1 & E10 == 1)
100.0 * sum(B00 == 1 & E10 == 1) / sum(B00)

sum(B00 == 1 & E11 == 1)
100.0 * sum(B00 == 1 & E11 == 1) / sum(B00)

sum(B00 == 1 & EOut == 1)
100.0 * sum(B00 == 1 & EOut == 1) / sum(B00)

sum(B10 == 1 & E00 == 1)
100.0 * sum(B10 == 1 & E00 == 1) / sum(B10)

sum(B10 == 1 & E10 == 1)
100.0 * sum(B10 == 1 & E10 == 1) / sum(B10)

sum(B10 == 1 & E11 == 1)
100.0 * sum(B10 == 1 & E11 == 1) / sum(B10)

sum(B10 == 1 & EOut == 1)
100.0 * sum(B10 == 1 & EOut == 1) / sum(B10)


sum(B11 == 1 & E00 == 1)
100.0 * sum(B11 == 1 & E00 == 1) / sum(B11)

sum(B11 == 1 & E10 == 1)
100.0 * sum(B11 == 1 & E10 == 1) / sum(B11)

sum(B11 == 1 & E11 == 1)
100.0 * sum(B11 == 1 & E11 == 1) / sum(B11)

sum(B11 == 1 & EOut == 1)
100.0 * sum(B11 == 1 & EOut == 1) / sum(B11)

detach(pilg)

# Now add all of the new varaibles to the data frame
# and remove the copies that remain in the workspace

pilg <- data.frame(pilg, retain, fNAge, fNInc, B00, B10, B11, E00, E10, E11, EOut)
rm(retain); rm(fNAge); rm(fNInc)
rm(B00); rm(B10); rm(B11)
rm(E00); rm(E10); rm(E11); rm(EOut)

attach(pilg)

# Run logistic logit regression on our most robust model to find
# out more about the drivers of customer retention.

lfit <- glm(retain ~ Profit9 + Online9 + Billpay9 + Tenure9 + D1100 + D1200 + IncImpt + MissingInc + AgeImpt + MissingAge, family=binomial(link=logit))
#Interpretation of the results/coefficients
summary(lfit)
logLik(lfit)
cat("\nTest of Overall Fit of the Model\n")
cat("\nDifference in Deviance: ")
lfit$null.deviance - lfit$deviance
cat("\nDegrees of Freedom of the Difference: ")
lfit$df.null - lfit$df.residual
cat("\nChiSquare test for the Overall Model\n")
dchisq(lfit$null.deviance-lfit$deviance, lfit$df.null-lfit$df.residual)
cat("\n\n")

cat("\nOverall Model Error Rate\n")
# overall error rate
predret <- rep(0,length(retain))
predret[lfit$fitted > 0.5] <- 1
err = abs(predret - retain)
sum(err)/length(err)

cat("\nBootstrapped Estimate of Error Rate\n")
library(boot)
val10fold <- cv.glm(pilg, glmfit = lfit, K = 10)
val10fold$delta

lfit2 <- glm(retain ~ Profit9 + Online9 + Billpay9 + Tenure9 + D1100 + D1200 + fNInc + fNAge, family=binomial(link=logit))
#Interpretation of the results/coefficients
summary(lfit2)
logLik(lfit2)
cat("\nTest of Overall Fit of the Model\n")
cat("\nDifference in Deviance: ")
lfit2$null.deviance - lfit2$deviance
cat("\nDegrees of Freedom of the Difference: ")
lfit2$df.null - lfit2$df.residual
cat("\nChiSquare test for the Overall Model\n")
dchisq(lfit2$null.deviance-lfit2$deviance, lfit2$df.null-lfit2$df.residual)
cat("\n\n")

cat("\nOverall Model Error Rate\n")
# overall error rate
predret <- rep(0,length(retain))
predret[lfit2$fitted > 0.5] <- 1
err = abs(predret - retain)
sum(err)/length(err)

cat("\nBootstrapped Estimate of Error Rate\n")
val10fold <- cv.glm(pilg, glmfit = lfit2, K = 10)
val10fold$delta

