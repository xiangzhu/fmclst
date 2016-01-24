NMI <- function(cls1,cls2){
	M <- length(table(cls1))
	G <- NULL; A <- NULL; temp <- 0
	GA <- matrix(data=0,nrow=M,ncol=M)
	SUMG <- 0; SUMA <- 0; SUMGA <- 0
	N <- length(cls1)
	for(k in 1:M){
		G[k] <- length(which(cls1 == k))
		A[k] <- length(which(cls2 == k))
		SUMG <- SUMG + G[k] * log(G[k]/N)
		SUMA <- SUMA + A[k] * log(A[k]/N) 
}
	for(i in 1:M){
		for(j in 1:M){
		GA[i,j] <- length( which(cls1 == i & cls2 == j) )
		if (GA[i,j] > 0) temp <- GA[i,j] * log( (N*GA[i,j])/(G[i]*A[j])) else temp <- 0
		SUMGA <- SUMGA + temp
}
}
	SUMGA / (sqrt(SUMG * SUMA))
}
