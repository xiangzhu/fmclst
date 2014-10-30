library(ape)
data <- read.csv('pt141_494_0819.csv', header=F)
Y <- as.data.frame(t((data)))
Yname <- c(374.26, 379.274, 384.292, 389.313, 394.337, 399.365, 404.397, 409.432, 414.47, 419.512, 424.557, 429.606, 434.658, 439.713, 444.772, 449.834, 454.9, 459.97, 465.042, 470.118, 475.198, 480.281, 485.368, 490.458, 495.551, 500.648, 505.748, 510.852, 515.959, 521.069, 526.183, 531.301, 536.422, 541.546, 546.674, 551.805, 556.94, 562.078, 567.219, 572.365, 577.513, 582.665, 587.82, 592.979, 598.141, 603.307, 608.476, 613.648, 618.824, 624.004, 629.187, 634.373, 639.563, 644.756, 649.953, 655.153, 660.356, 665.563, 670.773, 675.987, 681.205, 686.425, 691.649, 696.877, 702.108, 707.343, 712.581, 717.822, 723.067, 728.315, 733.567, 738.822, 744.08, 749.342, 754.608, 759.877, 765.149, 770.425, 775.704, 780.987, 786.273, 791.563, 796.856, 802.152, 807.452, 812.755, 818.062, 823.372, 828.686, 834.003, 839.324, 844.648, 849.975, 855.306, 860.64, 865.978, 871.319, 876.664, 882.012, 887.363, 892.718, 898.077, 903.439, 908.804, 914.173, 919.545, 924.92, 930.299, 935.682, 941.068, 946.457, 951.85, 957.246, 962.646, 968.049, 973.456, 978.866, 984.279, 989.696, 995.117, 1000.54, 1005.97, 1011.4, 1016.83, 1022.27, 1027.71, 1033.16, 1038.6)
T1 <- length(which(Yname <= 450))
T2 <- length(which(Yname>450 & Yname<=500))
T3 <- length(which(Yname>500 & Yname<=600))
T4 <- length(which(Yname>600 & Yname<=720))
T5 <- length(which(Yname>720))

WL <- as.character(round(Yname, digits=1))

p <- dim(Y)[2]
k.0 <- 10
source('macf.r')
M <- matrix(data=0,ncol=p,nrow=p)
for (i in 1:k.0){
        M <- M + SIG(Y,i) %*% t(SIG(Y,i))
}

R <- round(p/2)
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
Idt <- diag(1,p)
Resid <- t( (Idt-A%*%t(A)) %*% t(Y) ) # each row of Resid

# clustering
rownames(A) <- WL
hc <- hclust(dist(A),method='ward')
clus5 = cutree(hc,5)
mypal = c('darkorange','grey','seagreen','deeppink','navy')
#mywd <- c (1,1.5,2,2.5,3)
Type <- c(rep(1,T1),rep(2,T2),rep(3,T3),rep(4,T4),rep(5,T5))
yuki = c('darkmagenta','deepskyblue','limegreen','red','black')
png('fan_0819.png',width=600,height=600, unit='px')
plot(as.phylo(hc),type='fan',label.offset=0.15,cex=0.75,tip.color=yuki[Type],edge.color=mypal[clus5],edge.width=1.5)
title(main='Hyperspectral profile on 2013-08-19')
dev.off()
