options(prompt="R>", scipen=100, digits=4)

#This script generates replicate differences for assessment of ME on the cleaned dataset without outliers

library(readxl)
library(dplyr)
library(tidyr)
me=read_excel("1a-reps-10-MEvardiff.xlsx", sheet="male-breadths")
me$Rep=as.factor(me$Rep)
rep1= me[me$Rep=="1", ]
rep2= me[me$Rep=="2", ]
rep3= me[me$Rep=="3", ]
rep4= me[me$Rep=="4", ]
rep5= me[me$Rep=="5", ]
rep6= me[me$Rep=="6", ]
rep7= me[me$Rep=="7", ]
rep8= me[me$Rep=="8", ]
rep9= me[me$Rep=="9", ]
rep10= me[me$Rep=="10", ]

#wide to long
long1= gather(rep1, var, val, mxm1:mxp4, factor_key=TRUE)
long2= gather(rep2, var, val, mxm1:mxp4, factor_key=TRUE)
long3= gather(rep3, var, val, mxm1:mxp4, factor_key=TRUE)
long4= gather(rep4, var, val, mxm1:mxp4, factor_key=TRUE)
long5= gather(rep5, var, val, mxm1:mxp4, factor_key=TRUE)
long6= gather(rep6, var, val, mxm1:mxp4, factor_key=TRUE)
long7= gather(rep7, var, val, mxm1:mxp4, factor_key=TRUE)
long8= gather(rep8, var, val, mxm1:mxp4, factor_key=TRUE)
long9= gather(rep9, var, val, mxm1:mxp4, factor_key=TRUE)
long10= gather(rep10, var, val, mxm1:mxp4, factor_key=TRUE)

#merge each of the 10 replicates in long format
#into one dataframe for ME analysis
d1=merge(long1,long2,by.x=c("Ind","Side", "var"),
               by.y=c("Ind","Side", "var"))
colnames(d1)[colnames(d1)=="val.x"] <- "rep1"
colnames(d1)[colnames(d1)=="val.y"] <- "rep2"
d1$Rep.x=NULL
d1$Rep.y=NULL
d2=merge(d1, long3,by.x=c("Ind","Side", "var"),
         by.y=c("Ind","Side", "var"))
colnames(d2)[colnames(d2)=="val"] <- "rep3"
d2$Rep=NULL
d3=merge(d2, long4,by.x=c("Ind","Side", "var"),
         by.y=c("Ind","Side", "var"))
colnames(d3)[colnames(d3)=="val"] <- "rep4"
d3$Rep=NULL
d4=merge(d3, long5,by.x=c("Ind","Side", "var"),
         by.y=c("Ind","Side", "var"))
colnames(d4)[colnames(d4)=="val"] <- "rep5"
d4$Rep=NULL
d5=merge(d4, long6,by.x=c("Ind","Side", "var"),
         by.y=c("Ind","Side", "var"))
colnames(d5)[colnames(d5)=="val"] <- "rep6"
d5$Rep=NULL
d6=merge(d5, long7,by.x=c("Ind","Side", "var"),
         by.y=c("Ind","Side", "var"))
colnames(d6)[colnames(d6)=="val"] <- "rep7"
d6$Rep=NULL
d7=merge(d6, long8,by.x=c("Ind","Side", "var"),
         by.y=c("Ind","Side", "var"))
colnames(d7)[colnames(d7)=="val"] <- "rep8"
d7$Rep=NULL
d8=merge(d7, long9,by.x=c("Ind","Side", "var"),
         by.y=c("Ind","Side", "var"))
colnames(d8)[colnames(d8)=="val"] <- "rep9"
d8$Rep=NULL
d9=merge(d8, long10,by.x=c("Ind","Side", "var"),
         by.y=c("Ind","Side", "var"))
colnames(d9)[colnames(d9)=="val"] <- "rep10"
d9$Rep=NULL

#rep diffs
d9$diff12= d9$rep1 - d9$rep2
d9$diff13= d9$rep1 - d9$rep3
d9$diff14= d9$rep1 - d9$rep4
d9$diff15= d9$rep1 - d9$rep5
d9$diff16= d9$rep1 - d9$rep6
d9$diff17= d9$rep1 - d9$rep7
d9$diff18= d9$rep1 - d9$rep8
d9$diff19= d9$rep1 - d9$rep9
d9$diff110= d9$rep1 - d9$rep10
d9$diff23=d9$rep2 - d9$rep3
d9$diff24=d9$rep2 - d9$rep4
d9$diff25=d9$rep2 - d9$rep5
d9$diff26=d9$rep2 - d9$rep6
d9$diff27=d9$rep2 - d9$rep7
d9$diff28=d9$rep2 - d9$rep8
d9$diff29=d9$rep2 - d9$rep9
d9$diff210=d9$rep2 - d9$rep10
d9$diff34=d9$rep3 - d9$rep4
d9$diff35=d9$rep3 - d9$rep5
d9$diff36=d9$rep3 - d9$rep6
d9$diff37=d9$rep3 - d9$rep7
d9$diff38=d9$rep3 - d9$rep8
d9$diff39=d9$rep3 - d9$rep9
d9$diff310=d9$rep3 - d9$rep10
d9$diff45=d9$rep4 - d9$rep5
d9$diff46=d9$rep4 - d9$rep6
d9$diff47=d9$rep4 - d9$rep7
d9$diff48=d9$rep4 - d9$rep8
d9$diff49=d9$rep4 - d9$rep9
d9$diff410=d9$rep4 - d9$rep10
d9$diff56=d9$rep5 - d9$rep6
d9$diff57=d9$rep5 - d9$rep7
d9$diff58=d9$rep5 - d9$rep8
d9$diff59=d9$rep5 - d9$rep9
d9$diff510=d9$rep5 - d9$rep10
d9$diff67=d9$rep6 - d9$rep7
d9$diff68=d9$rep6 - d9$rep8
d9$diff69=d9$rep6 - d9$rep9
d9$diff610=d9$rep6 - d9$rep10
d9$diff78=d9$rep7 - d9$rep8
d9$diff79=d9$rep7 - d9$rep9
d9$diff710=d9$rep7 - d9$rep10
d9$diff89=d9$rep8 - d9$rep9
d9$diff810=d9$rep8 - d9$rep10
d9$diff910=d9$rep9 - d9$rep10

d9$rep1=NULL
d9$rep2=NULL
d9$rep3=NULL
d9$rep4=NULL
d9$rep5=NULL
d9$rep6=NULL
d9$rep7=NULL
d9$rep8=NULL
d9$rep9=NULL
d9$rep10=NULL
d9$Sex=NULL
