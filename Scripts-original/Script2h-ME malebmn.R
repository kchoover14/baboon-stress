options(prompt="R>", scipen=100, digits=4)

#This script examines measurement error in male mandibular breadths

library(readxl)
library(dplyr)
library(tidyr)
me=read_excel("0-reps-10.xlsx", sheet="malebmn")

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
long1= gather(rep1, var, val, mnm1:mnp4, factor_key=TRUE)
long2= gather(rep2, var, val, mnm1:mnp4, factor_key=TRUE)
long3= gather(rep3, var, val, mnm1:mnp4, factor_key=TRUE)
long4= gather(rep4, var, val, mnm1:mnp4, factor_key=TRUE)
long5= gather(rep5, var, val, mnm1:mnp4, factor_key=TRUE)
long6= gather(rep6, var, val, mnm1:mnp4, factor_key=TRUE)
long7= gather(rep7, var, val, mnm1:mnp4, factor_key=TRUE)
long8= gather(rep8, var, val, mnm1:mnp4, factor_key=TRUE)
long9= gather(rep9, var, val, mnm1:mnp4, factor_key=TRUE)
long10= gather(rep10, var, val, mnm1:mnp4, factor_key=TRUE)

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
mnm1= me2[me2$var=="mnm1", ]
mnm1$Ind=paste0(as.character(mnm1$Ind),"_", as.character(mnm1$Side))
mnm1$var=NULL
mnm1$Side=NULL
x1=colnames(mnm1[,-1])
mnm1=melt(mnm1,id.vars="Ind",measure.vars=x1, variable.name="Diff",
          value.name="mnm1",na.rm = TRUE)
write.table(mnm1,"mnm1-maleb.txt", sep = "\t")

mnm2= me2[me2$var=="mnm2", ]
mnm2$Ind=paste0(as.character(mnm2$Ind),"_", as.character(mnm2$Side))
mnm2$var=NULL
mnm2$Side=NULL
x2=colnames(mnm2[,-1])
mnm2=melt(mnm2,id.vars="Ind",measure.vars=x2, variable.name="Diff",
          value.name="mnm2",na.rm = TRUE)

mnm3= me2[me2$var=="mnm3", ]
mnm3$Ind=paste0(as.character(mnm3$Ind),"_", as.character(mnm3$Side))
mnm3$var=NULL
mnm3$Side=NULL
x3=colnames(mnm3[,-1])
mnm3=melt(mnm3,id.vars="Ind",measure.vars=x3, variable.name="Diff",
          value.name="mnm3",na.rm = TRUE)

mnp4= me2[me2$var=="mnp4", ]
mnp4$Ind=paste0(as.character(mnp4$Ind),"_", as.character(mnp4$Side))
mnp4$var=NULL
mnp4$Side=NULL
x4=colnames(mnp4[,-1])
mnp4=melt(mnp4,id.vars="Ind",measure.vars=x4, variable.name="Diff",
          value.name="mnp4",na.rm = TRUE)
write.table(mnp4,"mnp4-maleb.txt", sep = "\t")

#Rosner's outlier detection
library(EnvStats)
rosnerTest(mnm1$mnm1, k=10, alpha=0.05, warn=TRUE)
rosnerTest(mnm2$mnm2, k=10, alpha=0.05, warn=TRUE)
rosnerTest(mnm3$mnm3, k=10, alpha=0.05, warn=TRUE)
rosnerTest(mnp4$mnp4, k=10, alpha=0.05, warn=TRUE)
