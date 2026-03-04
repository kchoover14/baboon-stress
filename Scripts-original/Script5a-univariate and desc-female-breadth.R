options(prompt="R>", scipen=100, digits=4)

#This script generates univariate and descriptive values for female breadths for the PS worksheet

#univariates
library(readxl)
femb=read_excel("0-reps-10-postFA.xlsx", sheet="female-breadths")
cat(paste("\n", "MNM1", "\n"))
summary(aov(mnm1 ~ Side + Side*Ind + Error(Ind/(Side*Ind)), data=femb))
cat(paste("\n", 'MNM2', "\n"))
summary(aov(mnm2 ~ Side + Side*Ind + Error(Ind/(Side*Ind)), data=femb))
cat(paste("\n", 'MNM3', "\n"))
summary(aov(mnm3 ~ Side + Side*Ind + Error(Ind/(Side*Ind)), data=femb))
cat(paste("\n", 'MNP4', "\n"))
summary(aov(mnp4 ~ Side + Side*Ind + Error(Ind/(Side*Ind)), data=femb))
cat(paste("\n", 'MXM1', "\n"))
summary(aov(mxm1 ~ Side + Side*Ind + Error(Ind/(Side*Ind)), data=femb))
cat(paste("\n", 'MXM2', "\n"))
summary(aov(mxm2 ~ Side + Side*Ind + Error(Ind/(Side*Ind)), data=femb))
cat(paste("\n", 'MXM4', "\n"))
summary(aov(mxm3 ~ Side + Side*Ind + Error(Ind/(Side*Ind)), data=femb))
cat(paste("\n", 'MXP3', "\n"))
summary(aov(mxp3 ~ Side + Side*Ind + Error(Ind/(Side*Ind)), data=femb))
cat(paste("\n", 'MXP4', "\n"))
summary(aov(mxp4 ~ Side + Side*Ind + Error(Ind/(Side*Ind)), data=femb))

#Average all replicates for descriptives
library(readxl)
library(dplyr)
library(tidyr)
femb$Ind=as.factor(femb$Ind)
femb$Side=as.factor(femb$Side)
femb$Rep=as.factor(femb$Rep)

#average all reps
fb1= femb[femb$Rep=="1", ]
fb2= femb[femb$Rep=="2", ]
fb3= femb[femb$Rep=="3", ]
fb4= femb[femb$Rep=="4", ]
fb5= femb[femb$Rep=="5", ]
fb6= femb[femb$Rep=="6", ]
fb7= femb[femb$Rep=="7", ]
fb8= femb[femb$Rep=="8", ]
fb9= femb[femb$Rep=="9", ]
fb10= femb[femb$Rep=="10", ]

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

#SKEW and KURTOSIS
library(DescTools)
library(readxl)
cat(paste("\n", "MNM1 SKEW", "\n"))
Skew(fb1$mnm1av, na.rm=TRUE)
cat(paste("\n", "MNM2 SKEW", "\n"))
Skew(fb1$mnm2av, na.rm=TRUE)
cat(paste("\n", "MNM3 SKEW", "\n"))
Skew(fb1$mnm3av, na.rm=TRUE)
cat(paste("\n", "MNp4 SKEW", "\n"))
Skew(fb1$mnp4av, na.rm=TRUE)
cat(paste("\n", "MXM1 SKEW", "\n"))
Skew(fb1$mxm1av, na.rm=TRUE)
cat(paste("\n", "MXM2 SKEW", "\n"))
Skew(fb1$mxm2av, na.rm=TRUE)
cat(paste("\n", "MXM3 SKEW", "\n"))
Skew(fb1$mxm3av, na.rm=TRUE)
cat(paste("\n", "MXP3 SKEW", "\n"))
Skew(fb1$mxp3av, na.rm=TRUE)
cat(paste("\n", "MXP4 SKEW", "\n"))
Skew(fb1$mxp4av, na.rm=TRUE)
cat(paste("\n", "MNM1 KURTOSIS", "\n"))
Kurt(fb1$mnm1av, na.rm = TRUE, method = 1)
cat(paste("\n", "MNM2 KURTOSIS", "\n"))
Kurt(fb1$mnm2av, na.rm = TRUE, method = 1)
cat(paste("\n", "MNM3 KURTOSIS", "\n"))
Kurt(fb1$mnm3av, na.rm = TRUE, method = 1)
cat(paste("\n", "MNP4 KURTOSIS", "\n"))
Kurt(fb1$mnp4av, na.rm = TRUE, method = 1)
cat(paste("\n", "MXM1 KURTOSIS", "\n"))
Kurt(fb1$mxm1av, na.rm = TRUE, method = 1)
cat(paste("\n", "MXM2 KURTOSIS", "\n"))
Kurt(fb1$mxm2av, na.rm = TRUE, method = 1)
cat(paste("\n", "MXM3 KURTOSIS", "\n"))
Kurt(fb1$mxm3av, na.rm = TRUE, method = 1)
cat(paste("\n", "MXP3 KURTOSIS", "\n"))
Kurt(fb1$mxp3av, na.rm = TRUE, method = 1)
cat(paste("\n", "MXP4 KURTOSIS", "\n"))
Kurt(fb1$mxp4av, na.rm = TRUE, method = 1)

#Get sd and average sd
fbleft= fb1[fb1$Side=="L", ]
fbleft$Side=NULL
fbright= fb1[fb1$Side=="R", ]
fbright$Side=NULL

#convert wide to long
fbllong=gather(fbleft, var, val, mnm1av:mxp4av, factor_key=TRUE)
fbrlong=gather(fbright, var, val, mnm1av:mxp4av, factor_key=TRUE)
fbdesc= merge(fbllong, fbrlong, by= c("Ind", "var"))
colnames(fbdesc)[colnames(fbdesc)=="val.x"] <- "left"
colnames(fbdesc)[colnames(fbdesc)=="val.y"] <- "right"
fbdesc.noNA=na.omit(fbdesc)

#make datasets for sd and av calculations
fbdiff=fbdesc.noNA
fbav=fbdesc.noNA

#get side differences,(R-L)
fbdiff$diff= fbdiff$right - fbdiff$left
fbdiff$left=NULL
fbdiff$right=NULL
fbdiff_wide= spread(fbdiff, var, diff)
colnames(fbdiff_wide)[colnames(fbdiff_wide)=="mnm1av"] <- "mnm1_sd"
colnames(fbdiff_wide)[colnames(fbdiff_wide)=="mnm2av"] <- "mnm2_sd"
colnames(fbdiff_wide)[colnames(fbdiff_wide)=="mnm3av"] <- "mnm3_sd"
colnames(fbdiff_wide)[colnames(fbdiff_wide)=="mnp4av"] <- "mnp4_sd"
colnames(fbdiff_wide)[colnames(fbdiff_wide)=="mxm1av"] <- "mxm1_sd"
colnames(fbdiff_wide)[colnames(fbdiff_wide)=="mxm2av"] <- "mxm2_sd"
colnames(fbdiff_wide)[colnames(fbdiff_wide)=="mxm3av"] <- "mxm3_sd"
colnames(fbdiff_wide)[colnames(fbdiff_wide)=="mxp3av"] <- "mxp3_sd"
colnames(fbdiff_wide)[colnames(fbdiff_wide)=="mxp4av"] <- "mxp4_sd"

#get  side averages,(R+L)/2
fbav$average= (fbav$right + fbav$left)/2
fbav$left=NULL
fbav$right=NULL
fbav_wide= spread(fbav, var, average)

library(Hmisc)
"MNM1-sd"
describe(fbdiff_wide$mnm1_sd, type=2)
"MNM2-sd"
describe(fbdiff_wide$mnm2_sd, type=2)
"MNM3-sd"
describe(fbdiff_wide$mnm3_sd, type=2)
"MNP4-sd"
describe(fbdiff_wide$mnp4_sd, type=2)
"MXM1-sd"
describe(fbdiff_wide$mxm1_sd, type=2)
"MXM2-sd"
describe(fbdiff_wide$mxm2_sd, type=2)
"MXM3-sd"
describe(fbdiff_wide$mxm3_sd, type=2)
"MXP3-sd"
describe(fbdiff_wide$mxp3_sd, type=2)
"MXP4-sd"
describe(fbdiff_wide$mxp4_sd, type=2)
"MNM1-av"
describe(fbav_wide$mnm1, type=2)
"MNM2-av"
describe(fbav_wide$mnm2, type=2)
"MNM3-av"
describe(fbav_wide$mnm3, type=2)
"MNP4-av"
describe(fbav_wide$mnp4, type=2)
"MXM1-av"
describe(fbav_wide$mxm1, type=2)
"MXM2-av"
describe(fbav_wide$mxm2, type=2)
"MXM3-av"
describe(fbav_wide$mxm3, type=2)
"MXP3-av"
describe(fbav_wide$mxp3, type=2)
"MXP4-av"
describe(fbav_wide$mxp4, type=2)
