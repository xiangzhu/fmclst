# This function returns the autoregressive coefficients for each
# row of the multivariate time series matrix.

AR.PIC <- function(series){
	p <- dim(series)[1]
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
	return(COEF)
}
