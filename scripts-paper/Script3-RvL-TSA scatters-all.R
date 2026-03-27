options(prompt="R>", scipen=100, digits=4)

#This script examines trait size asymmetry via scatterplots and Rosner's ESD tests

#FEMALE BREADTHS
library(tidyr)
library(readxl)
spfbLvR=read_excel("0-reps-10-postME.xlsx", sheet="female-breadths")
spfbLvR$Ind=as.factor(spfbLvR$Ind)
spfbLvR$Side=as.factor(spfbLvR$Side)
spfbLvR$Sex=NULL
spfbLvR$Rep=as.factor(spfbLvR$Rep)
fembleft= spfbLvR[spfbLvR$Side=="L", ]
fembright= spfbLvR[spfbLvR$Side=="R", ]

#convert data from wide to long
femblongl= gather(fembleft, var, val, mnm1:mxp4, factor_key=TRUE)
femblongr=gather(fembright, var, val, mnm1:mxp4, factor_key=TRUE)
rlplots=merge(femblongl,femblongr,
      by.x=c("Ind","Rep", "var"),
      by.y =c("Ind","Rep", "var"))
colnames(rlplots)[colnames(rlplots)=="val.x"] <- "val.left"
colnames(rlplots)[colnames(rlplots)=="val.y"] <- "val.right"
rlplots$Side.x=NULL
rlplots$Side.y=NULL

#scatterplots for LxR (LvR) per trait
library(ggplot2)
rlplots$var= as.factor(rlplots$var)
ggplot(rlplots, aes(val.left, val.right)) +
  geom_point()+
  facet_wrap(rlplots$var) + theme_classic()

#FEMALE LENGTHS
library(tidyr)
library(readxl)
spflLvR=read_excel("0-reps-10-postME.xlsx", sheet="female-lengths")
spflLvR$Ind=as.factor(spflLvR$Ind)
spflLvR$Side=as.factor(spflLvR$Side)
spflLvR$Sex=NULL
spflLvR$Rep=as.factor(spflLvR$Rep)
femlleft= spflLvR[spflLvR$Side=="L", ]
femlright= spflLvR[spflLvR$Side=="R", ]

#convert data from wide to long
femllongl= gather(femlleft, var, val, mnm1:mxp4, factor_key=TRUE)
femllongr=gather(femlright, var, val, mnm1:mxp4, factor_key=TRUE)
rlplots=merge(femllongl,femllongr,
              by.x=c("Ind","Rep", "var"),
              by.y =c("Ind","Rep", "var"))
colnames(rlplots)[colnames(rlplots)=="val.x"] <- "val.left"
colnames(rlplots)[colnames(rlplots)=="val.y"] <- "val.right"
rlplots$Side.x=NULL
rlplots$Side.y=NULL

#scatterplots for LxR (LvR) per trait
library(ggplot2)
rlplots$var= as.factor(rlplots$var)
ggplot(rlplots, aes(val.left, val.right)) +
  geom_point()+
  facet_wrap(rlplots$var) + theme_classic()

#MALE BREADTHS
library(tidyr)
library(readxl)
spmbLvR=read_excel("0-reps-10-postME.xlsx", sheet="male-breadths")
spmbLvR$Ind=as.factor(spmbLvR$Ind)
spmbLvR$Side=as.factor(spmbLvR$Side)
spmbLvR$Sex=NULL
spmbLvR$Rep=as.factor(spmbLvR$Rep)
malebleft= spmbLvR[spmbLvR$Side=="L", ]
malebright= spmbLvR[spmbLvR$Side=="R", ]

#convert data from wide to long
maleblongl= gather(malebleft, var, val, mnm1:mxp4, factor_key=TRUE)
maleblongr=gather(malebright, var, val, mnm1:mxp4, factor_key=TRUE)
rlplots=merge(maleblongl,maleblongr,
              by.x=c("Ind","Rep", "var"),
              by.y =c("Ind","Rep", "var"))
colnames(rlplots)[colnames(rlplots)=="val.x"] <- "val.left"
colnames(rlplots)[colnames(rlplots)=="val.y"] <- "val.right"
rlplots$Side.x=NULL
rlplots$Side.y=NULL

#scatterplots for LxR (LvR) per trait
library(ggplot2)
rlplots$var= as.factor(rlplots$var)
ggplot(rlplots, aes(val.left, val.right)) +
  geom_point()+
  facet_wrap(rlplots$var) + theme_classic()

#MALE lengths
library(tidyr)
library(readxl)
spmlLvR=read_excel("0-reps-10-postME.xlsx", sheet="male-lengths")
spmlLvR$Ind=as.factor(spmlLvR$Ind)
spmlLvR$Side=as.factor(spmlLvR$Side)
spmlLvR$Sex=NULL
spmlLvR$Rep=as.factor(spmlLvR$Rep)
malelleft= spmlLvR[spmlLvR$Side=="L", ]
malelright= spmlLvR[spmlLvR$Side=="R", ]

#convert data from wide to long
malellongl= gather(malelleft, var, val, mnm1:mxp4, factor_key=TRUE)
malellongr=gather(malelright, var, val, mnm1:mxp4, factor_key=TRUE)
rlplots=merge(malellongl,malellongr,
              by.x=c("Ind","Rep", "var"),
              by.y =c("Ind","Rep", "var"))
colnames(rlplots)[colnames(rlplots)=="val.x"] <- "val.left"
colnames(rlplots)[colnames(rlplots)=="val.y"] <- "val.right"
rlplots$Side.x=NULL
rlplots$Side.y=NULL

#scatterplots for LxR (LvR) per trait
library(ggplot2)
rlplots$var= as.factor(rlplots$var)
ggplot(rlplots, aes(val.left, val.right)) +
  geom_point()+
  facet_wrap(rlplots$var) + theme_classic()

#ROSNER'S ESD test for outliers
library(EnvStats)
outfb=read_excel("0-reps-10-postME.xlsx", sheet="female-breadths")
rosnerTest(outfb$mnm1, k=6, alpha=0.05, warn=TRUE)
rosnerTest(outfb$mnm2, k=6, alpha=0.05, warn=TRUE)
rosnerTest(outfb$mnm3, k=6, alpha=0.05, warn=TRUE)
rosnerTest(outfb$mnp4, k=6, alpha=0.05, warn=TRUE)
rosnerTest(outfb$mxm1, k=6, alpha=0.05, warn=TRUE)
rosnerTest(outfb$mxm2, k=6, alpha=0.05, warn=TRUE)
rosnerTest(outfb$mxm3, k=6, alpha=0.05, warn=TRUE)
rosnerTest(outfb$mxp3, k=6, alpha=0.05, warn=TRUE)
rosnerTest(outfb$mxp4, k=6, alpha=0.05, warn=TRUE)

outfl=read_excel("0-reps-10-postME.xlsx", sheet="female-lengths")
rosnerTest(outfl$mnm1, k=6, alpha=0.05, warn=TRUE)
rosnerTest(outfl$mnm2, k=6, alpha=0.05, warn=TRUE)
rosnerTest(outfl$mnm3, k=6, alpha=0.05, warn=TRUE)
rosnerTest(outfl$mnp4, k=6, alpha=0.05, warn=TRUE)
rosnerTest(outfl$mxm1, k=6, alpha=0.05, warn=TRUE)
rosnerTest(outfl$mxm2, k=6, alpha=0.05, warn=TRUE)
rosnerTest(outfl$mxm3, k=6, alpha=0.05, warn=TRUE)
rosnerTest(outfl$mxp3, k=6, alpha=0.05, warn=TRUE)
rosnerTest(outfl$mxp4, k=6, alpha=0.05, warn=TRUE)

outmb=read_excel("0-reps-10-postME.xlsx", sheet="male-breadths")
rosnerTest(outmb$mnm1, k=6, alpha=0.05, warn=TRUE)
rosnerTest(outmb$mnm2, k=6, alpha=0.05, warn=TRUE)
rosnerTest(outmb$mnm3, k=6, alpha=0.05, warn=TRUE)
rosnerTest(outmb$mnp4, k=6, alpha=0.05, warn=TRUE)
rosnerTest(outmb$mxm1, k=6, alpha=0.05, warn=TRUE)
rosnerTest(outmb$mxm2, k=6, alpha=0.05, warn=TRUE)
rosnerTest(outmb$mxm3, k=6, alpha=0.05, warn=TRUE)
rosnerTest(outmb$mxp3, k=6, alpha=0.05, warn=TRUE)
rosnerTest(outmb$mxp4, k=6, alpha=0.05, warn=TRUE)

outml=read_excel("0-reps-10-postME.xlsx", sheet="female-breadths")
rosnerTest(outml$mnm1, k=6, alpha=0.05, warn=TRUE)
rosnerTest(outml$mnm2, k=6, alpha=0.05, warn=TRUE)
rosnerTest(outml$mnm3, k=6, alpha=0.05, warn=TRUE)
rosnerTest(outml$mnp4, k=6, alpha=0.05, warn=TRUE)
rosnerTest(outml$mxm1, k=6, alpha=0.05, warn=TRUE)
rosnerTest(outml$mxm2, k=6, alpha=0.05, warn=TRUE)
rosnerTest(outml$mxm3, k=6, alpha=0.05, warn=TRUE)
rosnerTest(outml$mxp3, k=6, alpha=0.05, warn=TRUE)
rosnerTest(outml$mxp4, k=6, alpha=0.05, warn=TRUE)

