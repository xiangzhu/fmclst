# return the autoregressive coef for time series in each row

AR.PIC <- function(series){
	p <- dim(series)[1] # number of variates (series) in the multiple set
	K <- NULL
	for (i in 1:p){
		uni.series <- as.ts(as.numeric(series[i,]))
		K[i] <- length(ar(uni.series,aic=FALSE)$ar)
}
	k <- max(K)
	COEF <- matrix(data=0, nrow=p, ncol=k)
	for (i in 1:p){
		uni.series <- as.ts(as.numeric(series[i,]))
		COEF[i,1:K[i]] <- ar(uni.series,aic=FALSE)$ar
}
	COEF
}
