# This function computes the sample auto-covariance matrix of 
# multivariate time series D at time lag k.

SIG <- function(D,k){
	AVG <- apply(t(D),1,mean)
	TEMP <- matrix(data=0, ncol=dim(D)[2],nrow=dim(D)[2])
	for (t in 1: (dim(D)[1]-k)){
		TEMP <- TEMP + (t(Y)[,t+k]-AVG) %*% t(t(Y)[,t]-AVG) 
}
	SIGHAT <- TEMP/(dim(D)[1]-k)
	return(SIGHAT)
}
