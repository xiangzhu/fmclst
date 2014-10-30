source('armodel.r')
library('MASS')
# generate the 6 stationary factor process
factor.GEN <- function(TL,k,sigma){
	ar.data <- matrix(data=0, ncol=TL, nrow=(6*k))
	for (j in 1:k){
		ar.data[(1+(j-1)*6), 1:TL] <- aur(TL,sigma)
		ar.data[(2+(j-1)*6), 1:TL] <- bl(TL,sigma)
		ar.data[(3+(j-1)*6), 1:TL] <- ex(TL,sigma)
		ar.data[(4+(j-1)*6), 1:TL] <- se(TL,sigma)
		ar.data[(5+(j-1)*6), 1:TL] <- nl(TL,sigma)
		ar.data[(6+(j-1)*6), 1:TL] <- st(TL,sigma)
}
	ar.data
}

# generate the loading matrix
loading.GEN <- function(K){
	LM <- matrix(data=0, ncol=6, nrow=(4*K))
	for (j in 1:K){
		LM[(1+(j-1)*4),] <- rep(1,6)
		LM[(2+(j-1)*4),] <- c(1,rep(0,3),1,0)
		LM[(3+(j-1)*4),] <- c(0,rep(1,2),rep(0,3)) 
		LM[(4+(j-1)*4),] <- c(rep(0,3),1,0,1)
}
	LM
}

# generate the time series
ts.GEN <- function(K,TL){
	TS <- matrix(data=0, ncol=(TL+1), nrow=(4*K))
	TS[,1] <- rep(c(1,2,3,4),K)
	MU <- rep(0,(4*K))
	VAR <- diag(1,(4*K)) * 0.01
	for (t in 2:(TL+1)){
		TS[,t] <- loading.GEN(K) %*% factor.GEN(TL,1,1)[,(t-1)] + mvrnorm(n=1,mu=MU,Sigma=VAR)
}
	TS
}
