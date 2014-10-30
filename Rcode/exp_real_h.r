# simulation study on UCR time series dataset
library('TSclust')
source('sim_data.r')
source('nmi.r')
source('jaccard.r')
source('fmidx.r')
source('randidx.r')

data1 <- read.table('uWaveGestureLibrary_Z_TEST', header=F)
data2 <- read.table('uWaveGestureLibrary_Z_TRAIN', header=F)
data <- rbind(data1,data2)

Y <- t(data[,-1])
label <- data[,1]

p <- dim(Y)[2]
k.0 <- 10
source('macf.r')
M <- matrix(data=0,ncol=p,nrow=p)
for (i in 1:k.0){
	M <- M + SIG(Y,i) %*% t(SIG(Y,i))
}

R <- round(p/3)
ratio.stat <- NULL
ratio.stat <- eigen(M)$values[2:(R+1)] / eigen(M)$values[1:R]
r <- which(ratio.stat == min(ratio.stat))
A <- (eigen(M)$vectors)[,1:6]

RAND = JACC = FOMA = EVA = NOMI = c(0,0,0)

FMcls <- hclust(dist(A), method='ward')
ORcls <- hclust(dist(data[,-1]), method='ward') 
DWcls <- hclust(diss.DWT(data[,-1]), method='ward') 
	
	
	mlabel <- cutree(FMcls, k=8) 
	olabel <- cutree(ORcls, k=8)
	wlabel <- cutree(DWcls, k=8)
	RAND[1] = RAND[1] + RI(label,mlabel)
	RAND[2] = RAND[2] + RI(label,olabel)
	RAND[3] = RAND[3] + RI(label,wlabel)
	JACC[1] = JACC[1] + JC(label,mlabel)
        JACC[2] = JACC[2] + JC(label,olabel)
        JACC[3] = JACC[3] + JC(label,wlabel)
	FOMA[1] = FOMA[1] + FM(label,mlabel)
        FOMA[2] = FOMA[2] + FM(label,olabel)
        FOMA[3] = FOMA[3] + FM(label,wlabel)
 	EVA[1] = EVA[1] + cluster.evaluation(label,mlabel)
        EVA[2] = EVA[2] + cluster.evaluation(label,olabel)
        EVA[3] = EVA[3] + cluster.evaluation(label,wlabel)
        NOMI[1] = NOMI[1] + NMI(label,mlabel)
        NOMI[2] = NOMI[2] + NMI(label,olabel)
        NOMI[3] = NOMI[3] + NMI(label,wlabel)

print('UWGLZ')
print(RAND)
print(JACC)
print(FOMA)
print(EVA)
print(NOMI)
