#Gun point dataset: C=2,T=150,P=100*2
gunx <- read.table('Gun_Point_TEST', header=F)
guny <- read.table('Gun_Point_TRAIN', header=F)
gun <- rbind(gunx,guny)

# Synthetic control dataset: C=6,T=60,P=100*6
scx <- read.table('synthetic_control_TEST', header=F)
scy <- read.table('synthetic_control_TRAIN', header=F)
sc <- rbind(scx,scy)

# CBF dataset: C=3,T=128,P=310*3
CBFx <- read.table('CBF_TEST', header=F)
CBFy <- read.table('CBF_TRAIN', header=F)
CBF <- rbind(CBFx,CBFy)
