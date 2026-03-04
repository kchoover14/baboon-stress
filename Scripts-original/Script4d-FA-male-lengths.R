options(prompt="R>", scipen=100, digits=4)

#This script examines trait side differences via scatterplots and Rosner's ESD tests

#MALE LENGTHS R-L (SD) of trait v trait
library(readxl)
library(dplyr)
library(tidyr)
sdml=read_excel("0-reps-10-postME.xlsx", sheet="male-lengths")
sdml$Side=as.factor(sdml$Side)
sdml$Rep=as.factor(sdml$Rep)
ml1= sdml[sdml$Rep=="1", ]
ml2= sdml[sdml$Rep=="2", ]
ml3= sdml[sdml$Rep=="3", ]
ml4= sdml[sdml$Rep=="4", ]
ml5= sdml[sdml$Rep=="5", ]
ml6= sdml[sdml$Rep=="6", ]
ml7= sdml[sdml$Rep=="7", ]
ml8= sdml[sdml$Rep=="8", ]
ml9= sdml[sdml$Rep=="9", ]
ml10= sdml[sdml$Rep=="10", ]

#average replicates
ml1$mnm1av=(ml1$mnm1+ml2$mnm1+ml3$mnm1+ml4$mnm1+ml5$mnm1+ml6$mnm1+ml7$mnm1+ml8$mnm1+ml9$mnm1+ml10$mnm1)/10
ml1$mnm2av=(ml1$mnm2+ml2$mnm2+ml3$mnm2+ml4$mnm2+ml5$mnm2+ml6$mnm2+ml7$mnm2+ml8$mnm2+ml9$mnm2+ml10$mnm2)/10
ml1$mnm3av=(ml1$mnm3+ml2$mnm3+ml3$mnm3+ml4$mnm3+ml5$mnm3+ml6$mnm3+ml7$mnm3+ml8$mnm3+ml9$mnm3+ml10$mnm3)/10
ml1$mnp4av=(ml1$mnp4+ml2$mnp4+ml3$mnp4+ml4$mnp4+ml5$mnp4+ml6$mnp4+ml7$mnp4+ml8$mnp4+ml9$mnp4+ml10$mnp4)/10
ml1$mxm1av=(ml1$mxm1+ml2$mxm1+ml3$mxm1+ml4$mxm1+ml5$mxm1+ml6$mxm1+ml7$mxm1+ml8$mxm1+ml9$mxm1+ml10$mxm1)/10
ml1$mxm2av=(ml1$mxm2+ml2$mxm2+ml3$mxm2+ml4$mxm2+ml5$mxm2+ml6$mxm2+ml7$mxm2+ml8$mxm2+ml9$mxm2+ml10$mxm2)/10
ml1$mxm3av=(ml1$mxm3+ml2$mxm3+ml3$mxm3+ml4$mxm3+ml5$mxm3+ml6$mxm3+ml7$mxm3+ml8$mxm3+ml9$mxm3+ml10$mxm3)/10
ml1$mxp3av=(ml1$mxp3+ml2$mxp3+ml3$mxp3+ml4$mxp3+ml5$mxp3+ml6$mxp3+ml7$mxp3+ml8$mxp3+ml9$mxp3+ml10$mxp3)/10
ml1$mxp4av=(ml1$mxp4+ml2$mxp4+ml3$mxp4+ml4$mxp4+ml5$mxp4+ml6$mxp4+ml7$mxp4+ml8$mxp4+ml9$mxp4+ml10$mxp4)/10
ml1$mnm1=NULL
ml1$mnm2=NULL
ml1$mnm3=NULL
ml1$mnp4=NULL
ml1$mxm1=NULL
ml1$mxm2=NULL
ml1$mxm3=NULL
ml1$mxp3=NULL
ml1$mxp4=NULL
ml1$Rep=NULL
mlleft= ml1[ml1$Side=="L", ]
mlleft$Side=NULL
mlright= ml1[ml1$Side=="R", ]
mlright$Side=NULL

#convert wide to long
mlllong=gather(mlleft, var, val, mnm1av:mxp4av, factor_key=TRUE)
mlrlong=gather(mlright, var, val, mnm1av:mxp4av, factor_key=TRUE)
sdplots= merge(mlllong, mlrlong, by= c("Ind", "var"))
colnames(sdplots)[colnames(sdplots)=="val.x"] <- "val.left"
colnames(sdplots)[colnames(sdplots)=="val.y"] <- "val.right"
sdplots.noNA=na.omit(sdplots)
sdplots.noNA$diff= sdplots.noNA$val.right - sdplots.noNA$val.left
sdplots.noNA$val.left=NULL
sdplots.noNA$val.right=NULL
sdplots_wide= spread(sdplots.noNA, var, diff)
sdplots_wide= spread(sdplots.noNA, var, diff)
colnames(sdplots_wide)[colnames(sdplots_wide)=="mnm1av"] <- "mnm1_sd"
colnames(sdplots_wide)[colnames(sdplots_wide)=="mnm2av"] <- "mnm2_sd"
colnames(sdplots_wide)[colnames(sdplots_wide)=="mnm3av"] <- "mnm3_sd"
colnames(sdplots_wide)[colnames(sdplots_wide)=="mnp4av"] <- "mnp4_sd"
colnames(sdplots_wide)[colnames(sdplots_wide)=="mxm1av"] <- "mxm1_sd"
colnames(sdplots_wide)[colnames(sdplots_wide)=="mxm2av"] <- "mxm2_sd"
colnames(sdplots_wide)[colnames(sdplots_wide)=="mxm3av"] <- "mxm3_sd"
colnames(sdplots_wide)[colnames(sdplots_wide)=="mxp3av"] <- "mxp3_sd"
colnames(sdplots_wide)[colnames(sdplots_wide)=="mxp4av"] <- "mxp4_sd"

#Scatterplots
library(ggplot2)
library(gridExtra)
ggplot(sdplots_wide, aes(mnm2_sd, mnm3_sd)) + geom_point() + theme_classic()
ggplot(sdplots_wide, aes(mnp4_sd, mnm1_sd)) + geom_point() + theme_classic()
ggplot(sdplots_wide, aes(mxm2_sd, mxm3_sd)) + geom_point() + theme_classic()
ggplot(sdplots_wide, aes(mxp3_sd, mxp4_sd)) + geom_point() + theme_classic()
ggplot(sdplots_wide, aes(mxm1_sd, mxp3_sd)) + geom_point() + theme_classic()

#Rosner's outlier detection
library(EnvStats)
rosnerTest(sdplots_wide$mnm1, k=6, alpha=0.05, warn=TRUE)
rosnerTest(sdplots_wide$mnm2, k=6, alpha=0.05, warn=TRUE)
rosnerTest(sdplots_wide$mnm3, k=6, alpha=0.05, warn=TRUE)
rosnerTest(sdplots_wide$mnp4, k=6, alpha=0.05, warn=TRUE)
rosnerTest(sdplots_wide$mxm1, k=6, alpha=0.05, warn=TRUE)
rosnerTest(sdplots_wide$mxm2, k=6, alpha=0.05, warn=TRUE)
rosnerTest(sdplots_wide$mxm3, k=6, alpha=0.05, warn=TRUE)
rosnerTest(sdplots_wide$mxp3, k=6, alpha=0.05, warn=TRUE)
rosnerTest(sdplots_wide$mxp4, k=6, alpha=0.05, warn=TRUE)

