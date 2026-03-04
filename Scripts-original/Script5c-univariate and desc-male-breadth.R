options(prompt="R>", scipen=100, digits=4)

#This script generates univariate and descriptive values for male breadth for the PS worksheet

#univariates
library(readxl)
maleb=read_excel("0-reps-10-postFA.xlsx", sheet="male-breadths")
cat(paste("\n", "MNM1", "\n"))
summary(aov(mnm1 ~ Side + Side*Ind + Error(Ind/(Side*Ind)), data=maleb))
cat(paste("\n", 'MNM2', "\n"))
summary(aov(mnm2 ~ Side + Side*Ind + Error(Ind/(Side*Ind)), data=maleb))
cat(paste("\n", 'MNM3', "\n"))
summary(aov(mnm3 ~ Side + Side*Ind + Error(Ind/(Side*Ind)), data=maleb))
cat(paste("\n", 'MNP4', "\n"))
summary(aov(mnp4 ~ Side + Side*Ind + Error(Ind/(Side*Ind)), data=maleb))
cat(paste("\n", 'MXM1', "\n"))
summary(aov(mxm1 ~ Side + Side*Ind + Error(Ind/(Side*Ind)), data=maleb))
cat(paste("\n", 'MXM2', "\n"))
summary(aov(mxm2 ~ Side + Side*Ind + Error(Ind/(Side*Ind)), data=maleb))
cat(paste("\n", 'MXM4', "\n"))
summary(aov(mxm3 ~ Side + Side*Ind + Error(Ind/(Side*Ind)), data=maleb))
cat(paste("\n", 'MXP3', "\n"))
summary(aov(mxp3 ~ Side + Side*Ind + Error(Ind/(Side*Ind)), data=maleb))
cat(paste("\n", 'MXP4', "\n"))
summary(aov(mxp4 ~ Side + Side*Ind + Error(Ind/(Side*Ind)), data=maleb))

#Average all replicates for descriptives
library(readxl)
library(dplyr)
library(tidyr)
maleb=read_excel("0-reps-10-postFA.xlsx", sheet="male-breadths")
maleb$Ind=as.factor(maleb$Ind)
maleb$Side=as.factor(maleb$Side)
maleb$Rep=as.factor(maleb$Rep)

#average all reps
mb1= maleb[maleb$Rep=="1", ]
mb2= maleb[maleb$Rep=="2", ]
mb3= maleb[maleb$Rep=="3", ]
mb4= maleb[maleb$Rep=="4", ]
mb5= maleb[maleb$Rep=="5", ]
mb6= maleb[maleb$Rep=="6", ]
mb7= maleb[maleb$Rep=="7", ]
mb8= maleb[maleb$Rep=="8", ]
mb9= maleb[maleb$Rep=="9", ]
mb10= maleb[maleb$Rep=="10", ]

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

#SKEW and KURTOSIS
library(DescTools)
library(readxl)
cat(paste("\n", "MNM1 SKEW", "\n"))
Skew(mb1$mnm1av, na.rm=TRUE)
cat(paste("\n", "MNM2 SKEW", "\n"))
Skew(mb1$mnm2av, na.rm=TRUE)
cat(paste("\n", "MNM3 SKEW", "\n"))
Skew(mb1$mnm3av, na.rm=TRUE)
cat(paste("\n", "MNp4 SKEW", "\n"))
Skew(mb1$mnp4av, na.rm=TRUE)
cat(paste("\n", "MXM1 SKEW", "\n"))
Skew(mb1$mxm1av, na.rm=TRUE)
cat(paste("\n", "MXM2 SKEW", "\n"))
Skew(mb1$mxm2av, na.rm=TRUE)
cat(paste("\n", "MXM3 SKEW", "\n"))
Skew(mb1$mxm3av, na.rm=TRUE)
cat(paste("\n", "MXP3 SKEW", "\n"))
Skew(mb1$mxp3av, na.rm=TRUE)
cat(paste("\n", "MXP4 SKEW", "\n"))
Skew(mb1$mxp4av, na.rm=TRUE)
cat(paste("\n", "MNM1 KURTOSIS", "\n"))
Kurt(mb1$mnm1av, na.rm = TRUE, method = 1)
cat(paste("\n", "MNM2 KURTOSIS", "\n"))
Kurt(mb1$mnm2av, na.rm = TRUE, method = 1)
cat(paste("\n", "MNM3 KURTOSIS", "\n"))
Kurt(mb1$mnm3av, na.rm = TRUE, method = 1)
cat(paste("\n", "MNP4 KURTOSIS", "\n"))
Kurt(mb1$mnp4av, na.rm = TRUE, method = 1)
cat(paste("\n", "MXM1 KURTOSIS", "\n"))
Kurt(mb1$mxm1av, na.rm = TRUE, method = 1)
cat(paste("\n", "MXM2 KURTOSIS", "\n"))
Kurt(mb1$mxm2av, na.rm = TRUE, method = 1)
cat(paste("\n", "MXM3 KURTOSIS", "\n"))
Kurt(mb1$mxm3av, na.rm = TRUE, method = 1)
cat(paste("\n", "MXP3 KURTOSIS", "\n"))
Kurt(mb1$mxp3av, na.rm = TRUE, method = 1)
cat(paste("\n", "MXP4 KURTOSIS", "\n"))
Kurt(mb1$mxp4av, na.rm = TRUE, method = 1)

#Get sd and average sd
mbleft= mb1[mb1$Side=="L", ]
mbleft$Side=NULL
mbright= mb1[mb1$Side=="R", ]
mbright$Side=NULL

#wide to long
mbllong=gather(mbleft, var, val, mnm1av:mxp4av, factor_key=TRUE)
mbrlong=gather(mbright, var, val, mnm1av:mxp4av, factor_key=TRUE)
mbdesc= merge(mbllong, mbrlong, by= c("Ind", "var"))
colnames(mbdesc)[colnames(mbdesc)=="val.x"] <- "left"
colnames(mbdesc)[colnames(mbdesc)=="val.y"] <- "right"
mbdesc.noNA=na.omit(mbdesc)

#make datasets for sd and av calculations
mbdiff=mbdesc.noNA
mbav=mbdesc.noNA

#get side differences,(R-L)
mbdiff$diff= mbdiff$right - mbdiff$left
mbdiff$left=NULL
mbdiff$right=NULL
mbdiff_wide= spread(mbdiff, var, diff)
colnames(mbdiff_wide)[colnames(mbdiff_wide)=="mnm1av"] <- "mnm1_sd"
colnames(mbdiff_wide)[colnames(mbdiff_wide)=="mnm2av"] <- "mnm2_sd"
colnames(mbdiff_wide)[colnames(mbdiff_wide)=="mnm3av"] <- "mnm3_sd"
colnames(mbdiff_wide)[colnames(mbdiff_wide)=="mnp4av"] <- "mnp4_sd"
colnames(mbdiff_wide)[colnames(mbdiff_wide)=="mxm1av"] <- "mxm1_sd"
colnames(mbdiff_wide)[colnames(mbdiff_wide)=="mxm2av"] <- "mxm2_sd"
colnames(mbdiff_wide)[colnames(mbdiff_wide)=="mxm3av"] <- "mxm3_sd"
colnames(mbdiff_wide)[colnames(mbdiff_wide)=="mxp3av"] <- "mxp3_sd"
colnames(mbdiff_wide)[colnames(mbdiff_wide)=="mxp4av"] <- "mxp4_sd"

#get  side averages,(R+L)/2
mbav$average= (mbav$right + mbav$left)/2
mbav$left=NULL
mbav$right=NULL
mbav_wide= spread(mbav, var, average)

library(Hmisc)
"MNM1-sd"
describe(mbdiff_wide$mnm1_sd, type=2)
"MNM2-sd"
describe(mbdiff_wide$mnm2_sd, type=2)
"MNM3-sd"
describe(mbdiff_wide$mnm3_sd, type=2)
"MNP4-sd"
describe(mbdiff_wide$mnp4_sd, type=2)
"MXM1-sd"
describe(mbdiff_wide$mxm1_sd, type=2)
"MXM2-sd"
describe(mbdiff_wide$mxm2_sd, type=2)
"MXM3-sd"
describe(mbdiff_wide$mxm3_sd, type=2)
"MXP3-sd"
describe(mbdiff_wide$mxp3_sd, type=2)
"MXP4-sd"
describe(mbdiff_wide$mxp4_sd, type=2)
"MNM1-av"
describe(mbav_wide$mnm1, type=2)
"MNM2-av"
describe(mbav_wide$mnm2, type=2)
"MNM3-av"
describe(mbav_wide$mnm3, type=2)
"MNP4-av"
describe(mbav_wide$mnp4, type=2)
"MXM1-av"
describe(mbav_wide$mxm1, type=2)
"MXM2-av"
describe(mbav_wide$mxm2, type=2)
"MXM3-av"
describe(mbav_wide$mxm3, type=2)
"MXP3-av"
describe(mbav_wide$mxp3, type=2)
"MXP4-av"
describe(mbav_wide$mxp4, type=2)
