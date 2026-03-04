options(prompt="R>", scipen=100, digits=4)

#This script generates univariate and descriptive values for female lengths for the PS worksheet

#univariates
library(readxl)
feml=read_excel("0-reps-10-postFA.xlsx", sheet="female-lengths")
cat(paste("\n", "MNM1", "\n"))
summary(aov(mnm1 ~ Side + Side*Ind + Error(Ind/(Side*Ind)), data=feml))
cat(paste("\n", 'MNM2', "\n"))
summary(aov(mnm2 ~ Side + Side*Ind + Error(Ind/(Side*Ind)), data=feml))
cat(paste("\n", 'MNM3', "\n"))
summary(aov(mnm3 ~ Side + Side*Ind + Error(Ind/(Side*Ind)), data=feml))
cat(paste("\n", 'MNP4', "\n"))
summary(aov(mnp4 ~ Side + Side*Ind + Error(Ind/(Side*Ind)), data=feml))
cat(paste("\n", 'MXM1', "\n"))
summary(aov(mxm1 ~ Side + Side*Ind + Error(Ind/(Side*Ind)), data=feml))
cat(paste("\n", 'MXM2', "\n"))
summary(aov(mxm2 ~ Side + Side*Ind + Error(Ind/(Side*Ind)), data=feml))
cat(paste("\n", 'MXM4', "\n"))
summary(aov(mxm3 ~ Side + Side*Ind + Error(Ind/(Side*Ind)), data=feml))
cat(paste("\n", 'MXP3', "\n"))
summary(aov(mxp3 ~ Side + Side*Ind + Error(Ind/(Side*Ind)), data=feml))
cat(paste("\n", 'MXP4', "\n"))
summary(aov(mxp4 ~ Side + Side*Ind + Error(Ind/(Side*Ind)), data=feml))

#Average all replicates for descriptives
library(readxl)
library(dplyr)
library(tidyr)
feml=read_excel("0-reps-10-postFA.xlsx", sheet="female-lengths")
feml$Ind=as.factor(feml$Ind)
feml$Side=as.factor(feml$Side)
feml$Rep=as.factor(feml$Rep)

#average all reps
fl1= feml[feml$Rep=="1", ]
fl2= feml[feml$Rep=="2", ]
fl3= feml[feml$Rep=="3", ]
fl4= feml[feml$Rep=="4", ]
fl5= feml[feml$Rep=="5", ]
fl6= feml[feml$Rep=="6", ]
fl7= feml[feml$Rep=="7", ]
fl8= feml[feml$Rep=="8", ]
fl9= feml[feml$Rep=="9", ]
fl10= feml[feml$Rep=="10", ]

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

#SKEW and KURTOSIS
library(DescTools)
library(readxl)
cat(paste("\n", "MNM1 SKEW", "\n"))
Skew(fl1$mnm1av, na.rm=TRUE)
cat(paste("\n", "MNM2 SKEW", "\n"))
Skew(fl1$mnm2av, na.rm=TRUE)
cat(paste("\n", "MNM3 SKEW", "\n"))
Skew(fl1$mnm3av, na.rm=TRUE)
cat(paste("\n", "MNp4 SKEW", "\n"))
Skew(fl1$mnp4av, na.rm=TRUE)
cat(paste("\n", "MXM1 SKEW", "\n"))
Skew(fl1$mxm1av, na.rm=TRUE)
cat(paste("\n", "MXM2 SKEW", "\n"))
Skew(fl1$mxm2av, na.rm=TRUE)
cat(paste("\n", "MXM3 SKEW", "\n"))
Skew(fl1$mxm3av, na.rm=TRUE)
cat(paste("\n", "MXP3 SKEW", "\n"))
Skew(fl1$mxp3av, na.rm=TRUE)
cat(paste("\n", "MXP4 SKEW", "\n"))
Skew(fl1$mxp4av, na.rm=TRUE)
cat(paste("\n", "MNM1 KURTOSIS", "\n"))
Kurt(fl1$mnm1av, na.rm = TRUE, method = 1)
cat(paste("\n", "MNM2 KURTOSIS", "\n"))
Kurt(fl1$mnm2av, na.rm = TRUE, method = 1)
cat(paste("\n", "MNM3 KURTOSIS", "\n"))
Kurt(fl1$mnm3av, na.rm = TRUE, method = 1)
cat(paste("\n", "MNP4 KURTOSIS", "\n"))
Kurt(fl1$mnp4av, na.rm = TRUE, method = 1)
cat(paste("\n", "MXM1 KURTOSIS", "\n"))
Kurt(fl1$mxm1av, na.rm = TRUE, method = 1)
cat(paste("\n", "MXM2 KURTOSIS", "\n"))
Kurt(fl1$mxm2av, na.rm = TRUE, method = 1)
cat(paste("\n", "MXM3 KURTOSIS", "\n"))
Kurt(fl1$mxm3av, na.rm = TRUE, method = 1)
cat(paste("\n", "MXP3 KURTOSIS", "\n"))
Kurt(fl1$mxp3av, na.rm = TRUE, method = 1)
cat(paste("\n", "MXP4 KURTOSIS", "\n"))
Kurt(fl1$mxp4av, na.rm = TRUE, method = 1)

#Get sd and average sd
flleft= fl1[fl1$Side=="L", ]
flleft$Side=NULL
flright= fl1[fl1$Side=="R", ]
flright$Side=NULL

#wide to long
flllong=gather(flleft, var, val, mnm1av:mxp4av, factor_key=TRUE)
flrlong=gather(flright, var, val, mnm1av:mxp4av, factor_key=TRUE)
fldesc= merge(flllong, flrlong, by= c("Ind", "var"))
colnames(fldesc)[colnames(fldesc)=="val.x"] <- "left"
colnames(fldesc)[colnames(fldesc)=="val.y"] <- "right"
fldesc.noNA=na.omit(fldesc)

#make datasets for sd and av calculations
fldiff=fldesc.noNA
flav=fldesc.noNA

#get side differences,(R-L)
fldiff$diff= fldiff$right - fldiff$left
fldiff$left=NULL
fldiff$right=NULL
fldiff_wide= spread(fldiff, var, diff)
colnames(fldiff_wide)[colnames(fldiff_wide)=="mnm1av"] <- "mnm1_sd"
colnames(fldiff_wide)[colnames(fldiff_wide)=="mnm2av"] <- "mnm2_sd"
colnames(fldiff_wide)[colnames(fldiff_wide)=="mnm3av"] <- "mnm3_sd"
colnames(fldiff_wide)[colnames(fldiff_wide)=="mnp4av"] <- "mnp4_sd"
colnames(fldiff_wide)[colnames(fldiff_wide)=="mxm1av"] <- "mxm1_sd"
colnames(fldiff_wide)[colnames(fldiff_wide)=="mxm2av"] <- "mxm2_sd"
colnames(fldiff_wide)[colnames(fldiff_wide)=="mxm3av"] <- "mxm3_sd"
colnames(fldiff_wide)[colnames(fldiff_wide)=="mxp3av"] <- "mxp3_sd"
colnames(fldiff_wide)[colnames(fldiff_wide)=="mxp4av"] <- "mxp4_sd"

#get  side averages,(R+L)/2
flav$average= (flav$right + flav$left)/2
flav$left=NULL
flav$right=NULL
flav_wide= spread(flav, var, average)

library(Hmisc)
"MNM1-sd"
describe(fldiff_wide$mnm1_sd, type=2)
"MNM2-sd"
describe(fldiff_wide$mnm2_sd, type=2)
"MNM3-sd"
describe(fldiff_wide$mnm3_sd, type=2)
"MNP4-sd"
describe(fldiff_wide$mnp4_sd, type=2)
"MXM1-sd"
describe(fldiff_wide$mxm1_sd, type=2)
"MXM2-sd"
describe(fldiff_wide$mxm2_sd, type=2)
"MXM3-sd"
describe(fldiff_wide$mxm3_sd, type=2)
"MXP3-sd"
describe(fldiff_wide$mxp3_sd, type=2)
"MXP4-sd"
describe(fldiff_wide$mxp4_sd, type=2)
"MNM1-av"
describe(flav_wide$mnm1, type=2)
"MNM2-av"
describe(flav_wide$mnm2, type=2)
"MNM3-av"
describe(flav_wide$mnm3, type=2)
"MNP4-av"
describe(flav_wide$mnp4, type=2)
"MXM1-av"
describe(flav_wide$mxm1, type=2)
"MXM2-av"
describe(flav_wide$mxm2, type=2)
"MXM3-av"
describe(flav_wide$mxm3, type=2)
"MXP3-av"
describe(flav_wide$mxp3, type=2)
"MXP4-av"
describe(flav_wide$mxp4, type=2)

