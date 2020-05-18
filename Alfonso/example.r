# load a packaged library to help with regressions
library(car)

# load my own .r code to help with simple stats
source('./mylib.r')

# read in the data from the csv file
adata <- read.csv('./alf_data_clean.csv', header=TRUE)

# adata is called a "data frame" and you can think of it like a matrix or worksheet
adata

# attaching to the data frame makes it easy to work with its columns/variables
attach(adata)

# you can create new variables
Other_Sales <- All_Sales - Alf_Sales

# you should ALWAYS look at your data BEFORE doing any analyses

# You can do fancy plots for presentations
# you can use c() to create your own vectors/columns
# here we are setting the limits for the y-axis for the upcoming graph
ylim <- c(0, 2500000)
plot(Week, Alf_Sales, xlab="Time", ylab="Sales", type="l", col="green", ylim=ylim)
lines(Week,Other_Sales, type="l", col="red")
legend(20,700000,c("Alf","Other"),lty=c(1,1),lwd=c(2.5,2.5),col=c("Green","Red"))
title("Time Series Plot")

Avg_Other_Sales <- mean(Other_Sales)
Avg_Other_Sales

Sales_Cntrl <- (Other_Sales - Avg_Other_Sales) / Avg_Other_Sales

# The Sales Control
Sales_Cntrl

# or you can do quick and dirty plots
plot(Alf_Sales, Sales_Cntrl)

# detaching puts you back outside the matrix/worksheet
detach(adata)

# you can add columns to the 
adata <- data.frame(adata, Other_Sales, Sales_Cntrl)
adata
rm(Other_Sales)
rm(Sales_Cntrl)

attach(adata)

# examine you data with simple descriptive stats
basic.stats(Alf_Sales)
basic.stats(Other_Sales)

# Regression is handled by specifying your linear model
# add - 1 to the end of the model to remove the intercept if so desired
rgrfit <- lm(Alf_Sales ~ Alf_Adv + Other_Adv + Sales_Cntrl)

# run summary on the regression model to get the standard output
summary(rgrfit)

# can easily check for multi-collinearity issues using vifs (variance inflation factors)
# vif = max of (1 / (1 - Rsquared)) of regressing each X on the other X's
# should be under 5, over 15 unstable
vif(rgrfit)

# more descriptive stats on regression residuals
univariate.stats(residuals(rgrfit))

# generate a histogram of the standard residuals
hist(rstandard(rgrfit))

# get the beta coefficients
coefficients(rgrfit)

# get the standardized residuals
rstandard(rgrfit)

# generate some quick residual plots
plot(residuals(rgrfit),Alf_Sales)
plot(residuals(rgrfit),Alf_Adv)
plot(residuals(rgrfit),Sales_Cntrl)

# examine the fitted values
fitted(rgrfit)

# plot residuals versus fillted values to check for heteroskedasticity issues
yhat <- fitted(rgrfit)
plot(rstandard(rgrfit), fitted(rgrfit))

# you can use cbind() to join columns of data (variables) into a matrix
vlst <- cbind(Alf_Sales, Other_Sales, Alf_Adv, Other_Adv, Sales_Cntrl)

# generate a simple correlation matrix
cor(vlst)

# Test second hypothesis about Advertising This Week
# Hurting Sales Next week (buying forward)

adata

# generate Next Weeks Alf_Sales and Sales_Cntrl
AlfSales_Next <- Alf_Sales[2:26]
SalesCntrl_Next <- Sales_Cntrl[2:26]

# grab the same amunt of this week's data for Alf_Adv and Other_Adv
AlfAdv_This <- Alf_Adv[1:25]
OtherAdv_This <- Other_Adv[1:25]

# build a new data frame for this set of columns
nadata <- data.frame(AlfSales_Next, AlfAdv_This, OtherAdv_This, SalesCntrl_Next)
nadata

# now remove the local copies of these variables from 
# your workspace so as to not confuse/mask things
rm(AlfSales_Next)
rm(AlfAdv_This)
rm(OtherAdv_This)
rm(SalesCntrl_Next)

# detach from adata and attach to nadata 
detach(adata)
attach(nadata)

# run the second regression and summarize the output
rgrfit2 <- lm(AlfSales_Next ~ AlfAdv_This + OtherAdv_This + SalesCntrl_Next)
summary(rgrfit2)

detach(nadata)
rm(nadata)

attach(adata)
# some other things you can do

# removing cols from a data frame
# the following will remove columns 5 and 6 (Other_Adv and Other_Sales)
# to create a new data frame
new_adata <- adata[,-c(5,6)]
new_adata

# you could merge two data frames by specifying a unique key
# now merge the records based on Week and Alf_Sales
mkey <- c("Week","Alf_Sales")
junk <- merge(adata,new_adata,by.x=mkey, by.y=mkey)
junk

# you can delete uneeded data frames
rm(new_adata)
rm(junk)

# you can delete rows from a data frame where a condition is true
huh <- adata
huh

# which selects either rows or columns or both from a matrix/data frame
# logical operators are ! for not, | for or, and & for and
ds <- which(adata[,"Other_Adv"] == 0.0, arr.ind=T)
ds
 
huh <- huh[-ds,]
huh

# create a dummy variable indicating when Other_Sales declined
Recess <- rep(0,length(Sales_Cntrl))
Recess[Sales_Cntrl < 0.0] <- 1
Recess

# you can do the same with character data as well
# Note: WH is a string variable indicating where inventory is being held: IN -> internal, CU -> customer
# WHCU <- rep(0,length(WH))
# WHCU[WH == "CU"] <- 1
# WHIN <- rep(0,length(WH))
# WHIN[WH == "IN"] <- 1

# you can also specify how to deal with NA missing data

# recode specific values to missing data
OA <- Other_Adv
OA[Other_Adv == 0] <- NA
OA

# you can specify how you want NAs dealt with in most R routines
# the default is to delete them (and their associated rows)
# basic.stats will remove them but tell you how many were missing
basic.stats(OA)

# can test if missing using is.na()
is.na(OA)
