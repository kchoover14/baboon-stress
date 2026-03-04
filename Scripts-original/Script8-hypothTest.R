options(prompt="R>", scipen=100, digits=4)

#This script explores baboon data trends in FA with those reported in humans and tests hypotheses using the Levene test.
#The file used for analysis derives from the PS worksheet where the FA 10 values are calculated and have ME removed.

library(readxl)
library(car)
baboonfaindex=read_excel("1b-reps-10-hypothtest.xlsx", sheet="FA10")
baboonfaindex$Sex=as.factor(baboonfaindex$Sex)
baboonfaindex$Metric=as.factor(baboonfaindex$Metric)
baboonfaindex$Tooth=as.factor(baboonfaindex$Tooth)
baboonfaindex$Class=as.factor(baboonfaindex$Class)
baboonfaindex$M1=as.factor(baboonfaindex$M1)
baboonfaindex$M3=as.factor(baboonfaindex$M3)
baboonfaindex$Arcade=as.factor(baboonfaindex$Arcade)

#EXPLORES TRENDS IN BABOON FA DATA TO COMPARE TO THOSE REPORTED IN HUMAN FA DATA
#Bailit-certain have greater FA
"FA10*Tooth"
leveneTest(FA10~Tooth, data=baboonfaindex, center=mean)
cat("\n","\n")
"FA10*Tooth*Sex"
#leveneTest(FA10~Tooth*Sex, data=baboonfaindex, center=mean)
#warning about perfect fit and unreliable result
cat("\n","\n")
"FA10*Class"
leveneTest(FA10~Class, data=baboonfaindex, center=mean)
cat("\n","\n")
leveneTest(FA10~Class*Sex, data=baboonfaindex, center=mean)
cat("\n","\n")

#Harris max and lengths worse except mand lengths bad too
"FA10*Arcade"
leveneTest(FA10~Arcade, data=baboonfaindex, center=mean)
cat("\n","\n")
leveneTest(FA10~Arcade*Sex, data=baboonfaindex, center=mean)
cat("\n","\n")

"FA10*Metric"
"FA10*Metric*Sex"
leveneTest(FA10~Metric, data=baboonfaindex, center=mean)
cat("\n","\n")

"FA10*Metric*Sex"
leveneTest(FA10~Metric*Sex, data=baboonfaindex, center=mean)
cat("\n","\n")

#HYPOTHESES TESTING
"Hypothesis 1: Sex Differences"
leveneTest(FA10~Sex, data=baboonfaindex, center=mean)
cat("\n","\n")

#hypothesis 2: first molar and weaning
"Hypothesis 2: M1 Different"
leveneTest(FA10~M1, data=baboonfaindex, center=mean)
cat("\n","\n")
"Hypothesis 2: M1 Different by Sex"
leveneTest(FA10~M1*Sex, data=baboonfaindex, center=mean)
cat("\n","\n")

#hypothesis 3: third molar and reproduction
"Hypothesis 3: M3 Different"
leveneTest(FA10~M3, data=baboonfaindex, center=mean)
cat("\n","\n")
"Hypothesis 3: M3 Differenb by Sex"
leveneTest(FA10~M3*Sex, data=baboonfaindex, center=mean)


