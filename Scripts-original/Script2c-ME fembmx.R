options(prompt="R>", scipen=100, digits=4)

#This script examines measurement error in female maxillary breadths

library(readxl)
library(dplyr)
library(tidyr)
me=read_excel("0-reps-10.xlsx", sheet="fembmx")

#make each replicate a factor rather than as numeric
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

#reformat each replicate from wide to long
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

#create new final merged dataset for next phase of analysis
me2 <- d9

#create new variable which is the difference between each replicate
#would be simpler with function
me2$diff12= me2$rep1 - me2$rep2
me2$diff13= me2$rep1 - me2$rep3
me2$diff14= me2$rep1 - me2$rep4
me2$diff15= me2$rep1 - me2$rep5
me2$diff16= me2$rep1 - me2$rep6
me2$diff17= me2$rep1 - me2$rep7
me2$diff18= me2$rep1 - me2$rep8
me2$diff19= me2$rep1 - me2$rep9
me2$diff110= me2$rep1 - me2$rep10
me2$diff23=me2$rep2 - me2$rep3
me2$diff24=me2$rep2 - me2$rep4
me2$diff25=me2$rep2 - me2$rep5
me2$diff26=me2$rep2 - me2$rep6
me2$diff27=me2$rep2 - me2$rep7
me2$diff28=me2$rep2 - me2$rep8
me2$diff29=me2$rep2 - me2$rep9
me2$diff210=me2$rep2 - me2$rep10
me2$diff34=me2$rep3 - me2$rep4
me2$diff35=me2$rep3 - me2$rep5
me2$diff36=me2$rep3 - me2$rep6
me2$diff37=me2$rep3 - me2$rep7
me2$diff38=me2$rep3 - me2$rep8
me2$diff39=me2$rep3 - me2$rep9
me2$diff310=me2$rep3 - me2$rep10
me2$diff45=me2$rep4 - me2$rep5
me2$diff46=me2$rep4 - me2$rep6
me2$diff47=me2$rep4 - me2$rep7
me2$diff48=me2$rep4 - me2$rep8
me2$diff49=me2$rep4 - me2$rep9
me2$diff410=me2$rep4 - me2$rep10
me2$diff56=me2$rep5 - me2$rep6
me2$diff57=me2$rep5 - me2$rep7
me2$diff58=me2$rep5 - me2$rep8
me2$diff59=me2$rep5 - me2$rep9
me2$diff510=me2$rep5 - me2$rep10
me2$diff67=me2$rep6 - me2$rep7
me2$diff68=me2$rep6 - me2$rep8
me2$diff69=me2$rep6 - me2$rep9
me2$diff610=me2$rep6 - me2$rep10
me2$diff78=me2$rep7 - me2$rep8
me2$diff79=me2$rep7 - me2$rep9
me2$diff710=me2$rep7 - me2$rep10
me2$diff89=me2$rep8 - me2$rep9
me2$diff810=me2$rep8 - me2$rep10
me2$diff910=me2$rep9 - me2$rep10

me2$rep1=NULL
me2$rep2=NULL
me2$rep3=NULL
me2$rep4=NULL
me2$rep5=NULL
me2$rep6=NULL
me2$rep7=NULL
me2$rep8=NULL
me2$rep9=NULL
me2$rep10=NULL
me2$Sex=NULL

library(reshape2)
#create new dataframes for each tooth with all differences in long format
#these dataframes will be tested for outliers.
mxm1= me2[me2$var=="mxm1", ]
mxm1$Ind=paste0(as.character(mxm1$Ind),"_", as.character(mxm1$Side))
mxm1$var=NULL
mxm1$Side=NULL
x5=colnames(mxm1[,-1])
mxm1=melt(mxm1,id.vars="Ind",measure.vars=x5, variable.name="Diff",
          value.name="mxm1",na.rm = TRUE)
write.table(mxm1,"mxm1-femb.txt",sep="\t")

mxm2= me2[me2$var=="mxm2", ]
mxm2$Ind=paste0(as.character(mxm2$Ind),"_", as.character(mxm2$Side))
mxm2$var=NULL
mxm2$Side=NULL
x6=colnames(mxm2[,-1])
mxm2=melt(mxm2,id.vars="Ind",measure.vars=x6, variable.name="Diff",
          value.name="mxm2",na.rm = TRUE)
write.table(mxm2,"mxm2-femb.txt",sep="\t")

mxm3= me2[me2$var=="mxm3", ]
mxm3$Ind=paste0(as.character(mxm3$Ind),"_", as.character(mxm3$Side))
mxm3$var=NULL
mxm3$Side=NULL
x7=colnames(mxm3[,-1])
mxm3=melt(mxm3,id.vars="Ind",measure.vars=x7, variable.name="Diff",
          value.name="mxm3",na.rm = TRUE)
write.table(mxm3,"mxm3-femb.txt",sep="\t")

mxp3= me2[me2$var=="mxp3", ]
mxp3$Ind=paste0(as.character(mxp3$Ind),"_", as.character(mxp3$Side))
mxp3$var=NULL
mxp3$Side=NULL
x8=colnames(mxp3[,-1])
mxp3=melt(mxp3,id.vars="Ind",measure.vars=x8, variable.name="Diff",
          value.name="mxp3",na.rm = TRUE)
write.table(mxp3,"mxp3-femb.txt",sep="\t")

mxp4= me2[me2$var=="mxp4", ]
mxp4$Ind=paste0(as.character(mxp4$Ind),"_", as.character(mxp4$Side))
mxp4$var=NULL
mxp4$Side=NULL
x9=colnames(mxp4[,-1])
mxp4=melt(mxp4,id.vars="Ind",measure.vars=x9, variable.name="Diff",
          value.name="mxp4",na.rm = TRUE)
write.table(mxp4,"mxp4-femb.txt",sep="\t")

#Rosner's outlier detection
library(EnvStats)
rosnerTest(mxm1$mxm1, k=12, alpha=0.05, warn=TRUE)
rosnerTest(mxm2$mxm2, k=30, alpha=0.05, warn=TRUE)
rosnerTest(mxm3$mxm3, k=12, alpha=0.05, warn=TRUE)
rosnerTest(mxp3$mxp3, k=12, alpha=0.05, warn=TRUE)
rosnerTest(mxp4$mxp4, k=12, alpha=0.05, warn=TRUE)


