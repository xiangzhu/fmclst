# simulation study on UCR time series dataset
library('TSclust')
source('sim_data.r')
source('nmi.r')
source('jaccard.r')
source('fmidx.r')
source('randidx.r')
source('arpic.r')

data1 <- read.table('uWaveGestureLibrary_Z_TEST', header=F)
data2 <- read.table('uWaveGestureLibrary_Z_TRAIN', header=F)
data <- rbind(data1,data2)
#data <- read.table('Trace.txt', header=F)
label <- data[,1]
#tdata <- matrix(data=0, nrow=dim(data)[1], ncol=dim(data)[2]-1)
#for (i in 1:dim(data)[1]){
#	tdata[i,] <- as.ts(as.numeric(data[i,2:dim(data)[2]]))
#}

RAND = JACC = FOMA = EVA = NOMI = NULL

DIST <- dist(AR.PIC(data[,-1]))
ARcls <- hclust(DIST, method='ward')
alabel <- cutree(ARcls, k=8)

print('uZ')
RAND <- RI(label,alabel)
JACC <- JC(label,alabel)
FOMA <- FM(label,alabel)
EVA <- cluster.evaluation(label,alabel)
NOMI <- NMI(label,alabel)
print(c(RAND,JACC,FOMA,EVA,NOMI))
