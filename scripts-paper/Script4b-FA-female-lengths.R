options(prompt="R>", scipen=100, digits=4)

#This script examines trait side differences via scatterplots and Rosner's ESD tests

#FEMALE LENGTHS R-L (SD) of trait v trait
library(readxl)
library(dplyr)
library(tidyr)
sdfl=read_excel("0-reps-10-postME.xlsx", sheet="female-lengths")
sdfl$Side=as.factor(sdfl$Side)
sdfl$Rep=as.factor(sdfl$Rep)
fl1= sdfl[sdfl$Rep=="1", ]
fl2= sdfl[sdfl$Rep=="2", ]
fl3= sdfl[sdfl$Rep=="3", ]
fl4= sdfl[sdfl$Rep=="4", ]
fl5= sdfl[sdfl$Rep=="5", ]
fl6= sdfl[sdfl$Rep=="6", ]
fl7= sdfl[sdfl$Rep=="7", ]
fl8= sdfl[sdfl$Rep=="8", ]
fl9= sdfl[sdfl$Rep=="9", ]
fl10= sdfl[sdfl$Rep=="10", ]

#average replicates
fl1$mnm1av=(fl1$mnm1+fl2$mnm1+fl3$mnm1+fl4$mnm1+fl5$mnm1+fl6$mnm1+fl7$mnm1+fl8$mnm1+fl9$mnm1+fl10$mnm1)/10
fl1$mnm2av=(fl1$mnm2+fl2$mnm2+fl3$mnm2+fl4$mnm2+fl5$mnm2+fl6$mnm2+fl7$mnm2+fl8$mnm2+fl9$mnm2+fl10$mnm2)/10
fl1$mnm3av=(fl1$mnm3+fl2$mnm3+fl3$mnm3+fl4$mnm3+fl5$mnm3+fl6$mnm3+fl7$mnm3+fl8$mnm3+fl9$mnm3+fl10$mnm3)/10
fl1$mnp4av=(fl1$mnp4+fl2$mnp4+fl3$mnp4+fl4$mnp4+fl5$mnp4+fl6$mnp4+fl7$mnp4+fl8$mnp4+fl9$mnp4+fl10$mnp4)/10
fl1$mxm1av=(fl1$mxm1+fl2$mxm1+fl3$mxm1+fl4$mxm1+fl5$mxm1+fl6$mxm1+fl7$mxm1+fl8$mxm1+fl9$mxm1+fl10$mxm1)/10
fl1$mxm2av=(fl1$mxm2+fl2$mxm2+fl3$mxm2+fl4$mxm2+fl5$mxm2+fl6$mxm2+fl7$mxm2+fl8$mxm2+fl9$mxm2+fl10$mxm2)/10
fl1$mxm3av=(fl1$mxm3+fl2$mxm3+fl3$mxm3+fl4$mxm3+fl5$mxm3+fl6$mxm3+fl7$mxm3+fl8$mxm3+fl9$mxm3+fl10$mxm3)/10
fl1$mxp3av=(fl1$mxp3+fl2$mxp3+fl3$mxp3+fl4$mxp3+fl5$mxp3+fl6$mxp3+fl7$mxp3+fl8$mxp3+fl9$mxp3+fl10$mxp3)/10
fl1$mxp4av=(fl1$mxp4+fl2$mxp4+fl3$mxp4+fl4$mxp4+fl5$mxp4+fl6$mxp4+fl7$mxp4+fl8$mxp4+fl9$mxp4+fl10$mxp4)/10
fl1$mnm1=NULL
fl1$mnm2=NULL
fl1$mnm3=NULL
fl1$mnp4=NULL
fl1$mxm1=NULL
fl1$mxm2=NULL
fl1$mxm3=NULL
fl1$mxp3=NULL
fl1$mxp4=NULL
fl1$Rep=NULL
flleft= fl1[fl1$Side=="L", ]
flleft$Side=NULL
flright= fl1[fl1$Side=="R", ]
flright$Side=NULL

#convert wide to long
flllong=gather(flleft, var, val, mnm1av:mxp4av, factor_key=TRUE)
flrlong=gather(flright, var, val, mnm1av:mxp4av, factor_key=TRUE)
sdplots= merge(flllong, flrlong, by= c("Ind", "var"))
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

