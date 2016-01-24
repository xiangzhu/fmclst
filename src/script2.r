library(MASS)
library(wmtsa)

source('sim_data.r')
source('nmi.r')
source('jaccard.r')
source('fmidx.r')
source('randidx.r')
source('csm.r')
source('wavelet.r')

#set.seed(1200)
#data <- ts.GEN(400,800)
#print('400*4,800')
#source('EXP_sim.r')
#data <- NULL

set.seed(1800)
data <- ts.GEN(1000,800)
print('1000*4,800')
source('EXP_sim.r')
data <- NULL

