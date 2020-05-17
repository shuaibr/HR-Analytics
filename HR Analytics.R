source('./mylib2.r')
library(dplyr)
library(car)


#Read-in data
rdata <- read.csv("ExamData_CSV.csv", header=TRUE)

summary(rdata)
names(rdata)
dim(rdata)
head(rdata)
tail(rdata)

attach(rdata)

#irrelevant columns with no information to understand the case 
drops <- c("X", "X.1")
rdata<- rdata[,!(names(rdata) %in% drops)]

View(rdata)

#check for missing values 
apply(is.na(rdata), 2, which)

#Let's clean our data 
#Duration to accept offer is missign 2700 values, a few are negative 
#no significant change in mean value 
rdata$Duration.to.accept.offer <- abs(Duration.to.accept.offer)
meanD <- mean(Duration.to.accept.offer[!is.na(Duration.to.accept.offer)])
rdata$meanDAO <- Duration.to.accept.offer
detach(rdata)
attach(rdata)
rdata$meanDAO[is.na(meanDAO)] <- meanD
basic.stats(meanDAO)
rdata$meanDAO <- abs(meanDAO)
#% hike in CTC 
rdata$Percent.difference.CTC[is.na(Percent.difference.CTC)] <- 0
rdata$Percent.hike.offered.in.CTC[is.na(Percent.hike.offered.in.CTC)] <- 0
rdata$Pecent.hike.expected.in.CTC[is.na(Pecent.hike.expected.in.CTC)] <- 0
rdata$Offered.band <- as.factor(Offered.band)
detach(rdata)
attach(rdata)
View(rdata)

basic.stats(rdata$meanDAO)
basic.stats(Notice.period)
basic.stats(Rex.in.Yrs)
basic.stats(rdata$Percent.difference.CTC)
basic.stats(Percent.hike.offered.in.CTC)
basic.stats(Pecent.hike.expected.in.CTC)


#within 50 days, about 80% of the candidates have accepted 
plot(ecdf(Duration.to.accept.offer))
#obvious check, higher age means more relevant experience can be expected  
plot(Age, Rex.in.Yrs)
#majority of the candidates accept within 10 days 
hist(Duration.to.accept.offer)

#heteroskedasticity check is true, they are also linearly correlated 
not_acc <- lm(Notice.period ~ meanDAO)
plot(fitted(not_acc), resid(not_acc), main="Main Model", ylab="Residuals", xlab="Fitted Values")
plot(Notice.period ~ meanDAO)

#let's start modelling 
set.seed(123)
train_in <- sample(seq_len(nrow(rdata)), size = floor(0.8*nrow(rdata)))

train <- rdata[train_in, ]
test<- rdata[-train_in, ]

logit1 <- glm(Status ~ DOJ.Extended + Notice.period + Offered.band + Percent.hike.offered.in.CTC + Pecent.hike.expected.in.CTC + Percent.difference.CTC + Joining.Bonus + Candidate.relocate.actual + Gender + Candidate.Source  + LOB + Location + Age, family = binomial())
summary(logit1)

#Referenced from Pilgrim Case C 
cat("\nTest of Overall Fit of the Model\n")
cat("\nDifference in Deviance: ")
logit1$null.deviance - logit1$deviance

val10fold <- cv.glm(rdata, glmfit = logit1, K = 5)
val10fold$delta

logit2 <- glm(Status ~ DOJ.Extended + Notice.period + Offered.band + Percent.hike.offered.in.CTC + Pecent.hike.expected.in.CTC + Percent.difference.CTC + Joining.Bonus + Candidate.relocate.actual + Gender+ Candidate.Source + Rex.in.Yrs+ LOB  + Age, family = binomial())
summary(logit2)

logit2err <- cv.glm(rdata, glmfit = logit2, K = 5)
logit2err$delta

predict <- ifelse(predict(logit2, type = "response")>0.5, 1, 0)
actual <- rdata$Status
conf <- table(predict, actual)
conf

sensitivity <- conf[2, "Not Joined"]/(conf[2, "Joined"]+conf[2, "Not Joined"])
specificity <- conf[1, "Joined"]/(conf[1, "Joined"]+conf[1, "Not Joined"])
accuracy <- (conf[1, "Joined"]+conf[2, "Not Joined"])/(conf[1, "Not Joined"]+conf[2, "Not Joined"]+conf[1, "Joined"]+conf[2, "Joined"])
cat("Specificity: ",specificity)
cat("Sensitivity: ",sensitivity)
cat("Accuracy: ",accuracy)

