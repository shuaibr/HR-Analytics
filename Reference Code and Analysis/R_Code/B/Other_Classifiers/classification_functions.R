# this function calculates the sensitivity, specificity, and classification rate
# (thanks to stack overflow)
# cut = threshold, mod = classification model, y = vector of properly classified results
# (do not worry about details of the function)
perf = function(cut, mod,dta, y)
{ 
   fit <- predict.glm(mod,dta,type="response")
   yhat = (fit>cut)
   w = which(y==1)
   sensitivity = mean( yhat[w] == 1 ) 
   specificity = mean( yhat[-w] == 0 )
   prob_tp = sum(yhat[w] == 1)/nrow(dta)
   prob_fp = sum(yhat[-w] == 1)/nrow(dta)
   prob_tn = sum(yhat[-w] == 0)/nrow(dta)
   prob_fn = sum(yhat[w] == 0)/nrow(dta)
   c.rate = mean( y==yhat ) 
   out = t(as.matrix(c(cut,sensitivity, specificity,sensitivity,1-specificity,specificity,1-sensitivity,prob_tp,prob_fp,prob_tn,prob_fn, c.rate)))
   colnames(out) = c("threshold","sensitivity", "specificity","tp","fp","tn","fn","prob_tp","prob_fp","prob_tn","prob_fn","accuracy")
   return(as.data.frame(out))
}

# evaluate performance for range of cutoffs
# collect output in OUT dataframe
thresholds <- function(mod,dta,y)
{
	s = seq(.01,.99,length=200)
	OUT = as.data.frame(perf(s[1],mod,dta,y))
	for(i in 2:200) OUT<-rbind(OUT,perf(s[i],mod,dta,y))
	return(OUT)
}
