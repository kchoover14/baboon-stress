options(prompt="R>", scipen=100, digits=4)

#This script test ME variance across length v breadth and toothclass.

library(readxl)
library(dplyr)
library(tidyr)
me=read_excel("MEdiff-all.xlsx")
me2= gather(me, var, val,diff12:diff910, factor_key=TRUE)

library(car)
as.factor(me2$Sex)
as.factor(me2$Metric)
as.factor(me2$Tooth)
leveneTest(Diff~Sex*Metric*Tooth, data=me2, center=mean)
leveneTest(Diff~Sex, data=me2, center=mean)
leveneTest(Diff~Metric, data=me2, center=mean)
leveneTest(Diff~Tooth, data=me2, center=mean)

