# personal library of useful routines

univariate.stats <- function( vname, vlabel=deparse(substitute(vname)), ...) 
{
     # univariate statistics on vname
     
     cat("\n\n")
     cat("Summary of ",vlabel,"\n\n")

     # need number of non exact zero values for proper wilcoxon and sign tests
     tmp <- vname[vname != 0.0]
     nz <- length(tmp)

     n <- length(vname)
     no <- length(vname[is.na(vname)==F])
     cat("Observations   : ",n,"\n")
     cat("Non-Missing    : ",no,"\n")
     cat("Missing        : ",(n - no),"\n")
     
     mn <- mean(vname, na.rm=T)
     mdn <- median(vname, na.rm=T)
     mxm <- max(vname,na.rm=T)
     mnm <- min(vname,na.rm=T)
     msm <- sum(vname,na.rm=T)
     rng <- mxm - mnm
     cat("Mean           : ",mn,"\n")
     cat("Median         : ",mdn,"\n")
     cat("Sum            : ",msm,"\n")
     cat("Maximum        : ",mxm,"\n")
     cat("Minimum        : ",mnm,"\n")
     cat("Range          : ",rng,"\n")

     # standard deviation, variance, skewness, kurtosis, uncss and conss
     mm <- vname ^ 2
     uncss <- sum(mm,na.rm=T)
     mm <- vname - mn
     cm <- mm ^ 2
     conss <- sum(cm, na.rm=T)
     vr <- conss / (no - 1.0)
     s <- sqrt(vr)
     cv <- (100.0 * s) / mn
     cat("Std. Dev.      : ",s,"\n")
     cat("Variance       : ",vr,"\n")
     cat("Coeff. of Var. : ",cv,"\n")
     cat("Sum of Squares : ",uncss,"\n")
     cat("Constrained SS : ",conss,"\n")

     cm <- cm * mm
     skew <- sum(cm, na.rm=T) / (no - 2.0)
     skew <- skew / (s ^ 3)
     skew <- skew * ( no / (no - 1.0))
     cat("Skewness       : ",skew,"\n")

     cm <- cm * mm
     kurt <- sum(cm, na.rm=T)/(no - 3.0)
     kurt <- kurt / (vr ^ 2)
     kurt <- kurt * (no / (no - 2.0)) * ((no + 1.0)/(no - 1.0))
     kurt <- kurt - (3.0 * ((no - 1.0)/(no - 2.0)) * ((no - 1.0) / (no - 3.0)))
     cat("Kurtosis       : ",kurt,"\n")
     cat("\n")

     # Other Percentile Points
     u.ppoints <- quantile( vname , c(0.0,0.01,0.025,0.05,0.10,0.25,0.50,0.75,0.90,0.95,0.99,1.0), na.rm=T)
     cat("Percentiles\n")
     print(u.ppoints)
     cat("\n")

     # Tests for difference from 0
     u.t <- t.test( vname , mu=0.0 , na.rm=T)
     cat("t-test mu = 0.0\n")
     cat("    t : ",u.t$statistic,"\n")
     cat("    df: ",u.t$parameter,"\n")
     cat("    p : ",u.t$p.value,"\n")
     cat("\n")

     u.w <- wilcox.test( vname , mu=0.0 , na.rm=T)
     cat("wilcox-test mu=0.0\n")
     vs <- u.w$statistic
     evs <- (nz * (nz + 1.0)) / 4.0
     vvs <- (nz * (nz + 1.0) * ((2.0 * nz) + 1.0)) / 24.0
     wilcoxZ <- ( vs + 0.5 - evs ) / sqrt(vvs)
     cat("    V : ",u.w$statistic,"\n")
     cat("    Z : ",wilcoxZ,"\n")
     cat("    p : ",u.w$p.value,"\n")
     cat("\n")

     npos <- length(vname[(vname > 0.0) & (is.na(vname) == F)])
     u.s <-binom.test(npos, nz, p=0.5)
     cat("sign-test\n")
     u.spp = (u.s$statistic / u.s$parameter) * 100.0
     cat("    %P: ",u.spp,"\n")
     cat("    p : ",u.s$p.value,"\n")
     cat("\n")

     unv<- c(n, no, mn, mdn, mxm, mnm, msm, uncss, conss, s, vr, cv, skew, kurt,
             u.t$statistic, u.t$parameter, u.t$p.value,
             u.w$statistic, wilcoxZ, u.w$p.value,
             u.s$statistic, u.s$parameter, u.spp, u.s$p.value)

     names(unv) <- c('n', 'obs', 'mean', 'median', 'max', 'min', 'sum', 'uss', 'css', 
                      'stddev', 'variance', 'coef_var','skewness','kurtosis',
                      't.val', 't.df', 't.pv',
                      'wilcox.V', 'wilcoxZ', 'wilcox.pv',
                      's.pos', 's.n', 's.pp', 's.pv')
     univariate.stats <- unv
}


basic.stats <- function( vname, vlabel=deparse(substitute(vname)), ...) 
{
     # basic statistics on vname
     
     cat("\n\n")
     cat("Summary of ",vlabel,"\n\n")

     n <- length(vname)
     no <- length(vname[is.na(vname)==F])
     cat("Observations   : ",n,"\n")
     cat("Non-Missing    : ",no,"\n")
     cat("Missing        : ",(n - no),"\n")

     mn <- mean(vname, na.rm=T)
     mdn <- median(vname, na.rm=T)
     mxm <- max(vname,na.rm=T)
     mnm <- min(vname,na.rm=T)
     msm <- sum(vname,na.rm=T)
     rng <- mxm - mnm
     cat("Mean           : ",mn,"\n")
     cat("Median         : ",mdn,"\n")
     cat("Sum            : ",msm,"\n")
     cat("Maximum        : ",mxm,"\n")
     cat("Minimum        : ",mnm,"\n")
     cat("Range          : ",rng,"\n")

     # standard deviation, variance, skewness, kurtosis, uncss and conss
     mm <- vname ^ 2
     uncss <- sum(mm,na.rm=T)
     mm <- vname - mn
     cm <- mm ^ 2
     conss <- sum(cm, na.rm=T)
     vr <- conss / (no - 1.0)
     s <- sqrt(vr)
     cv <- (100.0 * s) / mn
     cat("Std. Dev.      : ",s,"\n")
     cat("Variance       : ",vr,"\n")
     cat("Coeff. of Var. : ",cv,"\n")
     cat("Sum of Squares : ",uncss,"\n")
     cat("Constrained SS : ",conss,"\n")

     cm <- cm * mm
     skew <- sum(cm, na.rm=T) / (no - 2.0)
     skew <- skew / (s ^ 3)
     skew <- skew * ( no / (no - 1.0))
     cat("Skewness       : ",skew,"\n")

     cm <- cm * mm
     kurt <- sum(cm, na.rm=T)/(no - 3.0)
     kurt <- kurt / (vr ^ 2)
     kurt <- kurt * (no / (no - 2.0)) * ((no + 1.0)/(no - 1.0))
     kurt <- kurt - (3.0 * ((no - 1.0)/(no - 2.0)) * ((no - 1.0) / (no - 3.0)))
     cat("Kurtosis       : ",kurt,"\n")
     cat("\n")

     unv<- c(n, no, mn, mdn, mxm, mnm, msm, uncss, conss, s, vr, cv, skew, kurt)

     names(unv) <- c('n', 'obs', 'mean', 'median', 'max', 'min', 'sum', 'uss', 'css', 
                      'stddev', 'variance', 'coef_var','skewness','kurtosis')
     basic.stats <- unv
}


groups <- function(v,n) {
  bp <- quantile(v,probs=seq(0.0,1.0,1/n))
  bp[[1]] <- bp[[1]] - bp[[n+1]] - 1.0
  groups <- cut(v,breaks=bp,labels=FALSE)
  groups
}


groupfunc <- function(v, p, fcn) {
   z <- levels(as.factor(p))
   r <- NULL
   for (i in 1:length(z)) {
      r <- c(r, fcn(v[p==z[[i]]]))
   }
   groupfunc <- r
   groupfunc
}



# to work properly both v and p must be sorted
# in the same way otherwise r will be out of sequence with v
groups_by_sorted <- function(v, p, n) {
    r <- unlist(lapply(split(v,as.factor(p)),groups,n))
    r
}



# return a new data frame of selected basic stats (dn)
# for each subgroups (ie.. unique value of sort key) of 
# a sorted vector (v) with  sort key (k) 
# note:  key must be the same length as vector v

# allowable basic stats are "N", "SUM", "MEAN", "MIN", "MAX", and "VAR"
# levels of the sort key are included in the output dataset
 
stats_by_sorted <- function(v, k, dn) {

   # determine the vector of unique keys and their length
   RKEY <- levels(as.factor(k))
   ng <- length(RKEY)

   # create output vectors for all stats of proper length
   # to prevent runtime re-allocation costs
   RN <- rep.int(0,ng)
   RSUM <- rep(0,ng)
   RMEAN <- rep(0,ng)
   RMAX <- rep(0,ng)
   RMIN <- rep(0,ng)
   RVAR <- rep(0,ng)

   # intialize scaler variables for the key and for the stats
   # for the very first value of vector v
   key <- k[[1]]
   n <- as.integer(1)
   tv <- v[[1]]
   vsum <- tv
   vmax <- tv
   vmin <- tv
   vvar <- tv * tv

   # keep a pointer into the output vectors to prevent need to concatentate
   # and the associated memory allocation costs 
   ptr <- as.integer(1)

   # for each remaining value in the vector
   for (i in 2:length(k)) {

     if (key != k[[i]]) {
          # if a new key value, for old key - calc stats and store in output vectors
         vmean <- vsum / as.numeric(n)
         vvar <- (vvar - (as.numeric(n) * vmean * vmean)) / as.numeric(n - 1)
         RN[[ptr]] <- n
         RSUM[[ptr]] <- vsum
         RMEAN[[ptr]] <- vmean
         RMAX[[ptr]] <- vmax
         RMIN[[ptr]] <- vmin
         RVAR[[ptr]] <- vvar
         ptr <- ptr + as.integer(1)

         # now restart stats for new key and vector values
         n <- as.integer(1)
         tv <- v[[i]]
         vsum <- tv
         vmax <- tv
         vmin <- tv
         vvar <- tv * tv
         key <- k[[i]]

     } else {
         # part of same key so simply update stats
         tv <- v[[i]]
         n <- n + as.integer(1)
         vsum <- vsum + tv
         if (tv > vmax) vmax <- tv
         if (tv < vmin) vmin <- tv
         vvar <- vvar + (tv * tv)       
     }
   }
   # handle very last key value at the end
   vmean <- vsum / as.numeric(n)
   vvar <- (vvar - (as.numeric(n) * vmean * vmean)) / as.numeric(n - 1)
   RN[[ptr]] <- n
   RSUM[[ptr]] <- vsum
   RMEAN[[ptr]] <- vmean
   RMAX[[ptr]] <- vmax
   RMIN[[ptr]] <- vmin
   RVAR[[ptr]] <- vvar

   # now build the output data frame based on values of dn
   # the first element of list dn is the "basename" for each column
   # of the output data frame
   bn <- dn[[1]]
   # add name for the unique sort Keys
   cn <- paste(bn,"KEY",sep="")
   # make sure it is not forced into a factor
   dd <- data.frame(I(RKEY))

   # for each of the remaining requested basic stats
   for (i in 2:length(dn)) {
        # store its name
        cn <- c(cn, paste(bn,dn[[i]],sep=""))
        # build up the output frame for each requested stat
        if (dn[[i]] == "N") dd <- data.frame(dd,RN)
        if (dn[[i]] == "SUM") dd <- data.frame(dd,RSUM)
        if (dn[[i]] == "MEAN") dd <- data.frame(dd,RMEAN)
        if (dn[[i]] == "VAR") dd <- data.frame(dd,RVAR)
        if (dn[[i]] == "MAX") dd <- data.frame(dd,RMAX)
        if (dn[[i]] == "MIN") dd <- data.frame(dd,RMIN)
   }
   # set the requested column names for the data frame
   colnames(dd) <- cn
   dd
}

