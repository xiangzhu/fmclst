
#library(MASS)
#library(wmtsa)

#source('sim_data.r')
#source('nmi.r')
#source('jaccard.r')
#source('fmidx.r')
#source('randidx.r')
#source('csm.r')
#source('wavelet.r')
#source('arpic.r')
#source('macf.r')

#set.seed(150)
#data <- ts.GEN(125,100)
Y <- t(data[,-1])
label <- data[,1]

p <- dim(Y)[2]
k.0 <- 10
M <- matrix(data=0,ncol=p,nrow=p)
for (i in 1:k.0){
	M <- M + SIG(Y,i) %*% t(SIG(Y,i))
}

A <- (eigen(M)$vectors)[,1:6]

RAND = JACC = FOMA = EVA = NOMI = rep(0,4)

for(i in 1:100){	
	mlabel <- kmeans(A, 4, nstart=1)$cluster 
	olabel <- kmeans(data[,-1], 4, nstart=1)$cluster
	wlabel <- kmeans(WFE(data[,-1]), 4, nstart=1)$cluster
	alabel <- kmeans(AR.PIC(data[,-1]), 4, nstart=1)$cluster
	RAND[1] = RAND[1] + RI(label,mlabel)
	RAND[2] = RAND[2] + RI(label,olabel)
	RAND[3] = RAND[3] + RI(label,wlabel)
	RAND[4] = RAND[4] + RI(label,alabel)
	JACC[1] = JACC[1] + JC(label,mlabel)
        JACC[2] = JACC[2] + JC(label,olabel)
	JACC[3] = JACC[3] + JC(label,wlabel)
	JACC[4] = JACC[4] + JC(label,alabel)
	FOMA[1] = FOMA[1] + FM(label,mlabel)
        FOMA[2] = FOMA[2] + FM(label,olabel)
	FOMA[3] = FOMA[3] + FM(label,wlabel)
	FOMA[4] = FOMA[4] + FM(label,alabel)
 	EVA[1] = EVA[1] + CSM(label,mlabel)
        EVA[2] = EVA[2] + CSM(label,olabel)
	EVA[3] = EVA[3] + CSM(label,wlabel)
	EVA[4] = EVA[4] + CSM(label,alabel)
        NOMI[1] = NOMI[1] + NMI(label,mlabel)
        NOMI[2] = NOMI[2] + NMI(label,olabel)
	NOMI[3] = NOMI[3] + NMI(label,wlabel)
	NOMI[4] = NOMI[4] + NMI(label,alabel)
}

print(RAND/100)
print(JACC/100)
print(FOMA/100)
print(EVA/100)
print(NOMI/100)
