CSM <- function(cls1,cls2){
	M <- length(table(cls1))
	G <- NULL; A <- NULL; temp <- NULL; result <- 0
	GA = Sima = matrix(data=0, nrow=M, ncol=M)
	for(k in 1:M){
		G[k] <- length(which(cls1 == k))
		A[k] <- length(which(cls2 == k))
}
	for(i in 1:M){
		for(j in 1:M){
			GA[i,j] <- length( which(cls1==i & cls2==j) )
			Sima[i,j] <- (2*GA[i,j]) / (G[i]+A[j])
}
		temp[i] <- max(Sima[i,])
}
	mean(temp)
}
