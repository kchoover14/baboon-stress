options(prompt="R>", scipen=100, digits=4)

#This script examines trait side differences via scatterplots and Rosner's ESD tests

#FEMALE BREADTHS R-L (SD) of trait v trait
library(readxl)
library(dplyr)
library(tidyr)
sdfb=read_excel("0-reps-10-postME.xlsx", sheet="female-breadths")
sdfb$Side=as.factor(sdfb$Side)
sdfb$Rep=as.factor(sdfb$Rep)
fb1= sdfb[sdfb$Rep=="1", ]
fb2= sdfb[sdfb$Rep=="2", ]
fb3= sdfb[sdfb$Rep=="3", ]
fb4= sdfb[sdfb$Rep=="4", ]
fb5= sdfb[sdfb$Rep=="5", ]
fb6= sdfb[sdfb$Rep=="6", ]
fb7= sdfb[sdfb$Rep=="7", ]
fb8= sdfb[sdfb$Rep=="8", ]
fb9= sdfb[sdfb$Rep=="9", ]
fb10= sdfb[sdfb$Rep=="10", ]

#average replicates
fb1$mnm1av=(fb1$mnm1+fb2$mnm1+fb3$mnm1+fb4$mnm1+fb5$mnm1+fb6$mnm1+fb7$mnm1+fb8$mnm1+fb9$mnm1+fb10$mnm1)/10
fb1$mnm2av=(fb1$mnm2+fb2$mnm2+fb3$mnm2+fb4$mnm2+fb5$mnm2+fb6$mnm2+fb7$mnm2+fb8$mnm2+fb9$mnm2+fb10$mnm2)/10
fb1$mnm3av=(fb1$mnm3+fb2$mnm3+fb3$mnm3+fb4$mnm3+fb5$mnm3+fb6$mnm3+fb7$mnm3+fb8$mnm3+fb9$mnm3+fb10$mnm3)/10
fb1$mnp4av=(fb1$mnp4+fb2$mnp4+fb3$mnp4+fb4$mnp4+fb5$mnp4+fb6$mnp4+fb7$mnp4+fb8$mnp4+fb9$mnp4+fb10$mnp4)/10
fb1$mxm1av=(fb1$mxm1+fb2$mxm1+fb3$mxm1+fb4$mxm1+fb5$mxm1+fb6$mxm1+fb7$mxm1+fb8$mxm1+fb9$mxm1+fb10$mxm1)/10
fb1$mxm2av=(fb1$mxm2+fb2$mxm2+fb3$mxm2+fb4$mxm2+fb5$mxm2+fb6$mxm2+fb7$mxm2+fb8$mxm2+fb9$mxm2+fb10$mxm2)/10
fb1$mxm3av=(fb1$mxm3+fb2$mxm3+fb3$mxm3+fb4$mxm3+fb5$mxm3+fb6$mxm3+fb7$mxm3+fb8$mxm3+fb9$mxm3+fb10$mxm3)/10
fb1$mxp3av=(fb1$mxp3+fb2$mxp3+fb3$mxp3+fb4$mxp3+fb5$mxp3+fb6$mxp3+fb7$mxp3+fb8$mxp3+fb9$mxp3+fb10$mxp3)/10
fb1$mxp4av=(fb1$mxp4+fb2$mxp4+fb3$mxp4+fb4$mxp4+fb5$mxp4+fb6$mxp4+fb7$mxp4+fb8$mxp4+fb9$mxp4+fb10$mxp4)/10
fb1$mnm1=NULL
fb1$mnm2=NULL
fb1$mnm3=NULL
fb1$mnp4=NULL
fb1$mxm1=NULL
fb1$mxm2=NULL
fb1$mxm3=NULL
fb1$mxp3=NULL
fb1$mxp4=NULL
fb1$Rep=NULL
fbleft= fb1[fb1$Side=="L", ]
fbleft$Side=NULL
fbright= fb1[fb1$Side=="R", ]
fbright$Side=NULL

#convert wide to long
fbllong=gather(fbleft, var, val, mnm1av:mxp4av, factor_key=TRUE)
fbrlong=gather(fbright, var, val, mnm1av:mxp4av, factor_key=TRUE)
sdplots= merge(fbllong, fbrlong, by= c("Ind", "var"))
colnames(sdplots)[colnames(sdplots)=="val.x"] <- "val.left"
colnames(sdplots)[colnames(sdplots)=="val.y"] <- "val.right"
sdplots.noNA=na.omit(sdplots)
sdplots.noNA$diff= sdplots.noNA$val.right - sdplots.noNA$val.left
sdplots.noNA$val.left=NULL
sdplots.noNA$val.right=NULL
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


