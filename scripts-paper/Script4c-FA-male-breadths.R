options(prompt="R>", scipen=100, digits=4)

#This script examines trait side differences via scatterplots and Rosner's ESD tests

#MALE BREADTHS R-L (SD) of trait v trait
library(readxl)
library(dplyr)
library(tidyr)
sdmb=read_excel("0-reps-10-postME.xlsx", sheet="male-breadths")
sdmb$Side=as.factor(sdmb$Side)
sdmb$Rep=as.factor(sdmb$Rep)
mb1= sdmb[sdmb$Rep=="1", ]
mb2= sdmb[sdmb$Rep=="2", ]
mb3= sdmb[sdmb$Rep=="3", ]
mb4= sdmb[sdmb$Rep=="4", ]
mb5= sdmb[sdmb$Rep=="5", ]
mb6= sdmb[sdmb$Rep=="6", ]
mb7= sdmb[sdmb$Rep=="7", ]
mb8= sdmb[sdmb$Rep=="8", ]
mb9= sdmb[sdmb$Rep=="9", ]
mb10= sdmb[sdmb$Rep=="10", ]

#average replicates
mb1$mnm1av=(mb1$mnm1+mb2$mnm1+mb3$mnm1+mb4$mnm1+mb5$mnm1+mb6$mnm1+mb7$mnm1+mb8$mnm1+mb9$mnm1+mb10$mnm1)/10
mb1$mnm2av=(mb1$mnm2+mb2$mnm2+mb3$mnm2+mb4$mnm2+mb5$mnm2+mb6$mnm2+mb7$mnm2+mb8$mnm2+mb9$mnm2+mb10$mnm2)/10
mb1$mnm3av=(mb1$mnm3+mb2$mnm3+mb3$mnm3+mb4$mnm3+mb5$mnm3+mb6$mnm3+mb7$mnm3+mb8$mnm3+mb9$mnm3+mb10$mnm3)/10
mb1$mnp4av=(mb1$mnp4+mb2$mnp4+mb3$mnp4+mb4$mnp4+mb5$mnp4+mb6$mnp4+mb7$mnp4+mb8$mnp4+mb9$mnp4+mb10$mnp4)/10
mb1$mxm1av=(mb1$mxm1+mb2$mxm1+mb3$mxm1+mb4$mxm1+mb5$mxm1+mb6$mxm1+mb7$mxm1+mb8$mxm1+mb9$mxm1+mb10$mxm1)/10
mb1$mxm2av=(mb1$mxm2+mb2$mxm2+mb3$mxm2+mb4$mxm2+mb5$mxm2+mb6$mxm2+mb7$mxm2+mb8$mxm2+mb9$mxm2+mb10$mxm2)/10
mb1$mxm3av=(mb1$mxm3+mb2$mxm3+mb3$mxm3+mb4$mxm3+mb5$mxm3+mb6$mxm3+mb7$mxm3+mb8$mxm3+mb9$mxm3+mb10$mxm3)/10
mb1$mxp3av=(mb1$mxp3+mb2$mxp3+mb3$mxp3+mb4$mxp3+mb5$mxp3+mb6$mxp3+mb7$mxp3+mb8$mxp3+mb9$mxp3+mb10$mxp3)/10
mb1$mxp4av=(mb1$mxp4+mb2$mxp4+mb3$mxp4+mb4$mxp4+mb5$mxp4+mb6$mxp4+mb7$mxp4+mb8$mxp4+mb9$mxp4+mb10$mxp4)/10
mb1$mnm1=NULL
mb1$mnm2=NULL
mb1$mnm3=NULL
mb1$mnp4=NULL
mb1$mxm1=NULL
mb1$mxm2=NULL
mb1$mxm3=NULL
mb1$mxp3=NULL
mb1$mxp4=NULL
mb1$Rep=NULL
mbleft= mb1[mb1$Side=="L", ]
mbleft$Side=NULL
mbright= mb1[mb1$Side=="R", ]
mbright$Side=NULL

#convert wide to long
mbllong=gather(mbleft, var, val, mnm1av:mxp4av, factor_key=TRUE)
mbrlong=gather(mbright, var, val, mnm1av:mxp4av, factor_key=TRUE)
sdplots= merge(mbllong, mbrlong, by= c("Ind", "var"))
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


