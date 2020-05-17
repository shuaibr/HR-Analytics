# read in the dataset
rdata <- read.csv("portfolio_selection_data2.csv", header=TRUE)
View(rdata)

# convert percentages to actual returns
rdata$GM <- rdata$GM / 100.0
rdata$GE <- rdata$GE / 100.0
rdata$Oracle <- rdata$Oracle / 100.0
rdata$Microsoft <- rdata$Microsoft / 100.0

# stocks
stocks <- c("GM", "GE", "Oracle", "Microsoft")

# there are many builting functions to handle sampling
# or resampling, we will use the most basic to 
# select a random set of 12 rows (to create a potential annual return)
ranrows <- sample(1:nrow(rdata), 12, replace=TRUE)
ranrows

# here you can create a new dataset by extract a random set of rows
ndata <- rdata[sample(1:nrow(rdata),12,replace=TRUE),stocks]
ndata

# use the prod function to calculate a true compound return
gmret <- prod(ndata$GM + 1.0) - 1
gmret

# now to make things easier, create our own compound return
# function, cmpret that calculates a compound return for any column/vector
cmpret <- function( vname )
+ { cmpret <- prod(vname + 1.0) - 1.0 }

# applies the function "cmpret" to every column (2) in ndata
apply(ndata, 2, cmpret)

# generates the first row of the bootstrapped dataset
bdata <- apply(ndata, 2, cmpret)

# now generate the remaining 999 rows
for (i in 1:999) { 
    ndata <- rdata[sample(1:nrow(rdata),12, replace=TRUE), stocks]
    bdata <- rbind(bdata, apply(ndata, 2, cmpret))
}

str(bdata)
summary(bdata)

# so we now have a bootstrapped up dataset

# we want to choose the "best" set of portfolio weights
# to meet our investment criteria
# potential choices are:
#    max portfolio return
#    max portfolio return subject to a limit on risk
#    minimize risk
#    max ret / risk

# make sure it is a data frame
bdata <- as.data.frame(bdata)

# use matrix algebra to multiply vector of weights by matrix
# to create portfolio returns vector 1000x1 and calculate sd
prisk <- function( vname ) {
  v <- vname
  dim(v) <- c(4,1)
  prisk <- sd(as.matrix(bdata) %*% v)
}

# use matrix algebra to multiply vector of weights by matrix
# to create portfolio returns vector 1000x1 and calculate mean
# and have it return -1 times that value for default minimization

pret <- function( vname ) {
  v <- vname
  dim(v) <- c(4,1)
  pret <- -1.0 * mean(as.matrix(bdata) %*% v)
}

# all portfolio weights must have lower bound of 0
# and upper bounds of 1

wlb <- c(0,0,0,0)
dim(wlb) <- c(4,1)

wub <- c(1,1,1,1)
dim(wub) <- c(4,1)

# starting  point for solution
pw <- c(0.25,0.25,0.25,0.25)
dim(pw) <- c(4,1)

# and all portfolio weights must sum to 1
# LHS (transposed)
Ae <- c(1,1,1,1)
dim(Ae) <- c(1,4)

# RHS of equality
B <- 1.0

# install.packages("NlcOptim")
library(NlcOptim)
help(solnl)

h <- solnl(pw, pret, Aeq=Ae, Beq=B, lb=wlb, ub=wub)
print(h)

