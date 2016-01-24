# This script compares four methods on the UCR time series FACEFOUR data.
# The results are averaged over 100 trials of k-means clustering output.

# Load packages.
library(MASS)
library(wmtsa)
library(knitr)

# Load scripts.
source('nmi.r')
source('jaccard.r')
source('fmidx.r')
source('randidx.r')
source('csm.r')
source('wavelet.r')
source('macf.r')
source('arpic.r')

# Load data.
data1 <- read.table('FaceFour_TEST', header=F)
data2 <- read.table('FaceFour_TRAIN', header=F)
data <- rbind(data1, data2)

# Create time series and true labels.
Y <- t(data[,-1])
label <- data[,1]

# Compute the matrix M.
p <- dim(Y)[2]
k.0 <- 10
M <- matrix(data=0,ncol=p,nrow=p)
for (i in 1:k.0){
	M <- M + SIG(Y,i) %*% t(SIG(Y,i))
}

# Perform the Lam-Yao procedure.
R <- round(p/3)
ratio.stat <- NULL
ratio.stat <- eigen(M)$values[2:(R+1)] / eigen(M)$values[1:R]
r <- which(ratio.stat == min(ratio.stat))
A <- (eigen(M)$vectors)[,1:r]

# Compute the clustering evaluation scores.
RAND = JACC = FOMA = EVA = NOMI = rep(0,4)
for (i in 1:100){
  # Apply k-means clustering over four input matrices.
  # FM: the factor loading matrix
	mlabel <- kmeans(A, 4, nstart=1)$cluster
	# OR: the original time series
	olabel <- kmeans(data[,-1], 4, nstart=1)$cluster
	# DW: the Haar wavelet coefficient matrix
	wlabel <- kmeans(WFE(data[,-1]), 4, nstart=1)$cluster
	# AP: the AR(\infty) operator coefficient matrix
	alabel <- kmeans(AR.PIC(data[,-1]), 4, nstart=1)$cluster
	
	# Compute the Rand index.
	RAND[1] <- RAND[1] + RI(label,mlabel)
	RAND[2] <- RAND[2] + RI(label,olabel)
	RAND[3] <- RAND[3] + RI(label,wlabel)
	RAND[4] <- RAND[4] + RI(label,alabel)
	
	# Compute the Jaccard score.
	JACC[1] <- JACC[1] + JC(label,mlabel)
  JACC[2] <- JACC[2] + JC(label,olabel)
  JACC[3] <- JACC[3] + JC(label,wlabel)
	JACC[4] <- JACC[4] + JC(label,alabel)
	
	# Compute the Folkes and Mallow index.
	FOMA[1] <- FOMA[1] + FM(label,mlabel)
  FOMA[2] <- FOMA[2] + FM(label,olabel)
  FOMA[3] <- FOMA[3] + FM(label,wlabel)
	FOMA[4] <- FOMA[4] + FM(label,alabel)
	
	# Compute the Cluster similarity measure.
 	EVA[1] <- EVA[1] + CSM(label,mlabel)
  EVA[2] <- EVA[2] + CSM(label,olabel)
  EVA[3] <- EVA[3] + CSM(label,wlabel)
	EVA[4] <- EVA[4] + CSM(label,alabel)
	
	# Compute the Normalized Mutual Information.
  NOMI[1] <- NOMI[1] + NMI(label,mlabel)
  NOMI[2] <- NOMI[2] + NMI(label,olabel)
  NOMI[3] <- NOMI[3] + NMI(label,wlabel)
	NOMI[4] <- NOMI[4] + NMI(label,alabel)
	
}

# Print the results that are averaged over 100 replications.
print('The UCR Time Series Datasets being used: FACEFOUR')

results.matrix <- rbind(RAND, JACC, FOMA, EVA, NOMI) / 100
colnames(results.matrix) <- c('FM', 'OR', 'DW', 'AP')
rownames(results.matrix) <- c('Rand', 'Jaccard', 'FMI', 'CSM', 'NMI')
knitr::kable(results.matrix, digits=4)


