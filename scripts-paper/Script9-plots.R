options(prompt="R>", scipen=100, digits=4)

#This script generates plots used in the paper to illustrate findings.

library(readxl)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(dplyr)
fa=read_excel("1b-reps-10-hypothtest.xlsx", sheet="FA10")
fa$Sex=as.factor(fa$Sex)
fa$Metric=as.factor(fa$Metric)
fa$Tooth=as.factor(fa$Tooth)
fa$Measurement=as.factor(fa$Measurement)
fa$M1=as.factor(fa$M1)
fa$M3=as.factor(fa$M3)

fa.sorted <- fa[order(fa$FA10) , ]

#Barplot of Sex Differences
p1=ggplot(fa.sorted, aes(reorder(Measurement, -FA10, sum), FA10, fill=Sex))+
    geom_bar(stat="identity")+
    xlab("")+
    scale_fill_viridis_d()+
    theme_classic2()+
    theme(axis.text.x=element_text(angle=45, hjust=1))
p2=ggplot(fa, aes(Sex, FA10, fill=Sex))+
    geom_boxplot(color="black")+
    scale_fill_viridis_d()+
    theme_classic2()
png("F2-SexDiff.png", width = 10, height = 5, units = "in",
    res= 300, pointsize = 12, family= "sans", type = "cairo")
grid.arrange(p1,p2, nrow=1)
dev.off()

#Plot for M1
p3=ggplot(fa, aes(M1, FA10, fill=M1))+
    geom_boxplot(color="black")+
    scale_fill_viridis_d()+
    theme_classic2()
p4=ggplot(fa, aes(M1, FA10, fill=Sex))+
    geom_boxplot(color="black")+
    facet_wrap(fa$Sex)+
    scale_fill_viridis_d()+
    theme_classic2()
png("F3-M1 and SexDiff.png", width = 8, height = 3, units = "in",
    res= 300, pointsize = 12, family= "sans", type = "cairo")
grid.arrange(p3,p4, nrow=1)
dev.off()

#Plot for M3
p5=ggplot(fa, aes(M3, FA10, fill=M3))+
    geom_boxplot(color="black")+
    scale_fill_viridis_d()+
    theme_classic2()
p6=ggplot(fa, aes(M3, FA10, fill=Sex))+
    geom_boxplot(color="black")+
    facet_wrap(fa$Sex)+
    scale_fill_viridis_d()+
    theme_classic2()
png("F4-M3 and SexDiff.png", width = 8, height = 3, units = "in",
    res= 300, pointsize = 12, family= "sans", type = "cairo")
grid.arrange(p5,p6, nrow=1)
dev.off()

