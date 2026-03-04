options(prompt="R>", scipen=100, digits=4)

#This script generates univariate and descriptive values for male lengths for the PS worksheet

#univariates
library(readxl)
malel=read_excel("0-reps-10-postFA.xlsx", sheet="male-lengths")
cat(paste("\n", "MNM1", "\n"))
summary(aov(mnm1 ~ Side + Side*Ind + Error(Ind/(Side*Ind)), data=malel))
cat(paste("\n", 'MNM2', "\n"))
summary(aov(mnm2 ~ Side + Side*Ind + Error(Ind/(Side*Ind)), data=malel))
cat(paste("\n", 'MNM3', "\n"))
summary(aov(mnm3 ~ Side + Side*Ind + Error(Ind/(Side*Ind)), data=malel))
cat(paste("\n", 'MNP4', "\n"))
summary(aov(mnp4 ~ Side + Side*Ind + Error(Ind/(Side*Ind)), data=malel))
cat(paste("\n", 'MXM1', "\n"))
summary(aov(mxm1 ~ Side + Side*Ind + Error(Ind/(Side*Ind)), data=malel))
cat(paste("\n", 'MXM2', "\n"))
summary(aov(mxm2 ~ Side + Side*Ind + Error(Ind/(Side*Ind)), data=malel))
cat(paste("\n", 'MXM4', "\n"))
summary(aov(mxm3 ~ Side + Side*Ind + Error(Ind/(Side*Ind)), data=malel))
cat(paste("\n", 'MXP3', "\n"))
summary(aov(mxp3 ~ Side + Side*Ind + Error(Ind/(Side*Ind)), data=malel))
cat(paste("\n", 'MXP4', "\n"))
summary(aov(mxp4 ~ Side + Side*Ind + Error(Ind/(Side*Ind)), data=malel))

#Average all replicates for descriptives
library(readxl)
library(dplyr)
library(tidyr)
malel=read_excel("0-reps-10-postFA.xlsx", sheet="male-lengths")
malel$Ind=as.factor(malel$Ind)
malel$Side=as.factor(malel$Side)
malel$Rep=as.factor(malel$Rep)

#average all reps
ml1= malel[malel$Rep=="1", ]
ml2= malel[malel$Rep=="2", ]
ml3= malel[malel$Rep=="3", ]
ml4= malel[malel$Rep=="4", ]
ml5= malel[malel$Rep=="5", ]
ml6= malel[malel$Rep=="6", ]
ml7= malel[malel$Rep=="7", ]
ml8= malel[malel$Rep=="8", ]
ml9= malel[malel$Rep=="9", ]
ml10= malel[malel$Rep=="10", ]

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

#SKEW and KURTOSIS
library(DescTools)
library(readxl)
cat(paste("\n", "MNM1 SKEW", "\n"))
Skew(ml1$mnm1av, na.rm=TRUE)
cat(paste("\n", "MNM2 SKEW", "\n"))
Skew(ml1$mnm2av, na.rm=TRUE)
cat(paste("\n", "MNM3 SKEW", "\n"))
Skew(ml1$mnm3av, na.rm=TRUE)
cat(paste("\n", "MNp4 SKEW", "\n"))
Skew(ml1$mnp4av, na.rm=TRUE)
cat(paste("\n", "MXM1 SKEW", "\n"))
Skew(ml1$mxm1av, na.rm=TRUE)
cat(paste("\n", "MXM2 SKEW", "\n"))
Skew(ml1$mxm2av, na.rm=TRUE)
cat(paste("\n", "MXM3 SKEW", "\n"))
Skew(ml1$mxm3av, na.rm=TRUE)
cat(paste("\n", "MXP3 SKEW", "\n"))
Skew(ml1$mxp3av, na.rm=TRUE)
cat(paste("\n", "MXP4 SKEW", "\n"))
Skew(ml1$mxp4av, na.rm=TRUE)
cat(paste("\n", "MNM1 KURTOSIS", "\n"))
Kurt(ml1$mnm1av, na.rm = TRUE, method = 1)
cat(paste("\n", "MNM2 KURTOSIS", "\n"))
Kurt(ml1$mnm2av, na.rm = TRUE, method = 1)
cat(paste("\n", "MNM3 KURTOSIS", "\n"))
Kurt(ml1$mnm3av, na.rm = TRUE, method = 1)
cat(paste("\n", "MNP4 KURTOSIS", "\n"))
Kurt(ml1$mnp4av, na.rm = TRUE, method = 1)
cat(paste("\n", "MXM1 KURTOSIS", "\n"))
Kurt(ml1$mxm1av, na.rm = TRUE, method = 1)
cat(paste("\n", "MXM2 KURTOSIS", "\n"))
Kurt(ml1$mxm2av, na.rm = TRUE, method = 1)
cat(paste("\n", "MXM3 KURTOSIS", "\n"))
Kurt(ml1$mxm3av, na.rm = TRUE, method = 1)
cat(paste("\n", "MXP3 KURTOSIS", "\n"))
Kurt(ml1$mxp3av, na.rm = TRUE, method = 1)
cat(paste("\n", "MXP4 KURTOSIS", "\n"))
Kurt(ml1$mxp4av, na.rm = TRUE, method = 1)

#Get sd and average sd
mlleft= ml1[ml1$Side=="L", ]
mlleft$Side=NULL
mlright= ml1[ml1$Side=="R", ]
mlright$Side=NULL

#wide to long
mlllong=gather(mlleft, var, val, mnm1av:mxp4av, factor_key=TRUE)
mlrlong=gather(mlright, var, val, mnm1av:mxp4av, factor_key=TRUE)
mldesc= merge(mlllong, mlrlong, by= c("Ind", "var"))
colnames(mldesc)[colnames(mldesc)=="val.x"] <- "left"
colnames(mldesc)[colnames(mldesc)=="val.y"] <- "right"
mldesc.noNA=na.omit(mldesc)

#make datasets for sd and av calculations
mldiff=mldesc.noNA
mlav=mldesc.noNA

#get side differences,(R-L)
mldiff$diff= mldiff$right - mldiff$left
mldiff$left=NULL
mldiff$right=NULL
mldiff_wide= spread(mldiff, var, diff)
colnames(mldiff_wide)[colnames(mldiff_wide)=="mnm1av"] <- "mnm1_sd"
colnames(mldiff_wide)[colnames(mldiff_wide)=="mnm2av"] <- "mnm2_sd"
colnames(mldiff_wide)[colnames(mldiff_wide)=="mnm3av"] <- "mnm3_sd"
colnames(mldiff_wide)[colnames(mldiff_wide)=="mnp4av"] <- "mnp4_sd"
colnames(mldiff_wide)[colnames(mldiff_wide)=="mxm1av"] <- "mxm1_sd"
colnames(mldiff_wide)[colnames(mldiff_wide)=="mxm2av"] <- "mxm2_sd"
colnames(mldiff_wide)[colnames(mldiff_wide)=="mxm3av"] <- "mxm3_sd"
colnames(mldiff_wide)[colnames(mldiff_wide)=="mxp3av"] <- "mxp3_sd"
colnames(mldiff_wide)[colnames(mldiff_wide)=="mxp4av"] <- "mxp4_sd"

#get  side averages,(R+L)/2
mlav$average= (mlav$right + mlav$left)/2
mlav$left=NULL
mlav$right=NULL
mlav_wide= spread(mlav, var, average)

library(Hmisc)
"MNM1-sd"
describe(mldiff_wide$mnm1_sd, type=2)
"MNM2-sd"
describe(mldiff_wide$mnm2_sd, type=2)
"MNM3-sd"
describe(mldiff_wide$mnm3_sd, type=2)
"MNP4-sd"
describe(mldiff_wide$mnp4_sd, type=2)
"MXM1-sd"
describe(mldiff_wide$mxm1_sd, type=2)
"MXM2-sd"
describe(mldiff_wide$mxm2_sd, type=2)
"MXM3-sd"
describe(mldiff_wide$mxm3_sd, type=2)
"MXP3-sd"
describe(mldiff_wide$mxp3_sd, type=2)
"MXP4-sd"
describe(mldiff_wide$mxp4_sd, type=2)
"MNM1-av"
describe(mlav_wide$mnm1, type=2)
"MNM2-av"
describe(mlav_wide$mnm2, type=2)
"MNM3-av"
describe(mlav_wide$mnm3, type=2)
"MNP4-av"
describe(mlav_wide$mnp4, type=2)
"MXM1-av"
describe(mlav_wide$mxm1, type=2)
"MXM2-av"
describe(mlav_wide$mxm2, type=2)
"MXM3-av"
describe(mlav_wide$mxm3, type=2)
"MXP3-av"
describe(mlav_wide$mxp3, type=2)
"MXP4-av"
describe(mlav_wide$mxp4, type=2)


