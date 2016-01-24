# useful packages
library('TSclust') # package for time series clustering (DWT method included) 

# input the Trace data set
data <- read.csv('tlambda.csv',header=F)
Y <- t(data)

# factor modelling for high-dim time series (Lam and Yao, 2012)

# compute matrix M
p <- dim(Y)[2]
k.0 <- 10
source('macf.r') # function SIG: compute the sample covariance matrix of Y at time lag k
M <- matrix(data=0,ncol=p,nrow=p)
for (i in 1:k.0){
	M <- M + SIG(Y,i) %*% t(SIG(Y,i))
}

# determine the number of factors: r
R <- round(p/3)
ratio.stat <- NULL
ratio.stat <- eigen(M)$values[2:(R+1)] / eigen(M)$values[1:R]
r <- which(ratio.stat == min(ratio.stat))
plot(ratio.stat, xlab='', ylab='',t='l', lwd=2)
title(main='How to determine the number of factors?',
xlab='index', ylab='ratio of consecutive estimated eigenvalues')

# estimate the factor loading matrix: A
A <- (eigen(M)$vectors)[,1:r]

# estimate the factor process: X_t
X <- t( t(A) %*% t(Y) ) # each row of X

# check the validity of model: residual plot
Idt <- diag(1,p)
Resid <- t( (Idt-A%*%t(A)) %*% t(Y) ) # each row of Resid
library('mvtsplot') # package for multivariate time series visualization
mvtsplot(Resid, norm='internal', margin=F)

# k-means clustering on the factor loading matrix A
mlabel <- kmeans(A,4)$cluster # use default setting of k-means clustering in R

# compute evaluation criterion for k-means
source('randidx.r')
source('jaccard.r')
source('fmidx')
source('nmi.r')
RI(label,mlabel) # rand index
JC(label,mlabel) # jaccard score
FM(label,mlabel) # folkes and mallow index
cluster.evaluation(label,mlabel) # cluster similarity measure in package 'TSclust'
NMI(label,mlabel) # normalized mutual information

# hierarchical clustering on the factor loading matrix A
mmlabel <- hclust(dist(A), method='single')? # Euclidean distance + single linkage

# compute evaluation criterion for hierarchical clustering
RI(label,mmlabel)
JC(label,mmlabel)
FM(label,mmlabel)
cluster.evaluation(label,mmlabel)
NMI(label,mmlabel)
