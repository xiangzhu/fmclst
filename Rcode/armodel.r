aur <- function(L,sigma){
	XD <- NULL; XD[1] = 0
	for (i in 2:100){
		XD[i] = XD[i-1] * 0.6 + rnorm(1,mean=0,sd=sigma)
}	
	XD[1] <- XD[100]
	for (i in 2:L){
		XD[i] = XD[i-1] * 0.6 + rnorm(1,mean=0,sd=sigma)
}		
	XD
}

bl <- function(L,sigma){
	XD <- NULL; XD[1] = 0
	for (i in 2:100){
		XD[i] = (0.3-0.2*rnorm(1,mean=0,sd=sigma)) * XD[i-1] + 1 + rnorm(1,mean=0,sd=sigma)
}
	XD[1] <- XD[100]
	for (i in 2:L){
		XD[i] = (0.3-0.2*rnorm(1,mean=0,sd=sigma)) * XD[i-1] + 1 + rnorm(1,mean=0,sd=sigma)
}
	XD
}

ex <- function(L,sigma){
	XD <- NULL; XD[1] = 0
	for (i in 2:100){
		XD[i] = (0.9*exp(-(XD[i-1])^2) - 0.6) * XD[i-1] + 1 + rnorm(1,mean=0,sd=sigma)
}
	XD[1] <- XD[100]
	for (i in 2:L){
		XD[i] = (0.9*exp(-(XD[i-1])^2) - 0.6) * XD[i-1] + 1 + rnorm(1,mean=0,sd=sigma)
}
	XD
}

se <- function(L,sigma){
	XD <- NULL; XD[1] = 0
	for (i in 2:100){
		if (XD[i-1] >= 0.2) TEMP <- 0.3*XD[i-1]+1 else TEMP <- -(0.3*XD[i-1]-1)
		XD[i] = TEMP + rnorm(1,mean=0,sd=sigma)
}
	XD[1] <- XD[100]
	for (i in 2:L){
                if (XD[i-1] >= 0.2) TEMP <- 0.3*XD[i-1]+1 else TEMP <- -(0.3*XD[i-1]-1)
                XD[i] = TEMP + rnorm(1,mean=0,sd=sigma)
}
	XD
}

nl <- function(L,sigma){
	XD <- NULL; XD[1] = 0
	for (i in 2:100){
		XD[i] = 0.7 * abs(XD[i-1]) / (2+abs(XD[i-1])) + rnorm(1,mean=0,sd=sigma)
}
	XD[1] <- XD[100]
	for (i in 2:L){
                XD[i] = 0.7 * abs(XD[i-1]) / (2+abs(XD[i-1])) + rnorm(1,mean=0,sd=sigma)
}
	XD
}

st <- function(L,sigma){
	XD <- NULL; XD[1] = 0
	for (i in 2:100){
		XD[i] = 0.8*XD[i-1] - 0.8*XD[i-1]/(1+exp(-10*XD[i-1])) + rnorm(1,mean=0,sd=sigma)
}
	XD[1] <- XD[100]
	for (i in 2:L){
                XD[i] = 0.8*XD[i-1] - 0.8*XD[i-1]/(1+exp(-10*XD[i-1])) + rnorm(1,mean=0,sd=sigma)
}
	XD
}

