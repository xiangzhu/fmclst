library(MASS)
library(wmtsa)

source('sim_data.r')
source('nmi.r')
source('jaccard.r')
source('fmidx.r')
source('randidx.r')
source('csm.r')
source('wavelet.r')
source('arpic.r')
source('macf.r')

for (i in 1:50){
	data <- ts.GEN(20,400)
	print('20*4,400')
	source('EXP_sim.r')
	data <- NULL
	#data <- ts.GEN(5,100)
	#print('5*4,100')
	#source('EXP_sim.r')
	#data <- NULL
}
#set.seed(300)
#data <- ts.GEN(100,200)
#print('100*4,200')
#source('EXP_sim.r')
#data <- NULL

#set.seed(450)
#data <- ts.GEN(250,200)
#print('250*4,200')
#source('EXP_sim.r')
#data <- NULL

#set.seed(600)
#data <- ts.GEN(200,400)
#print('200*4,400')
#source('EXP_sim.r')
#data <- NULL

#set.seed(900)
#data <- ts.GEN(500,400)
#print('500*4,400')
#source('EXP_sim.r')
#data <- NULL

