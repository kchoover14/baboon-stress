########################
#This script uses the original paper data for hypothesis testing to reproduce paper figures and provide R environment control.

# NOTE: This pipeline had two errors:
    # incorrect skew and kurtosis values: only mxp4 female breadth was sig
    # failure to remove variables with significant DA

######################## SETUP

library(readxl) # read Excel files
library(ggplot2) # data visualization
library(gridExtra) # arrange multiple plots
library(dplyr) # data manipulation
library(car) # Levene's test

fa = read_excel("data-paper/1b-reps-10-hypothtest.xlsx", sheet = "FA10")

fa$Sex = as.factor(fa$Sex)
fa$Metric = as.factor(fa$Metric)
fa$Tooth = as.factor(fa$Tooth)
fa$Measurement = as.factor(fa$Measurement)
fa$Class = as.factor(fa$Class)
fa$M1 = as.factor(fa$M1)
fa$M3 = as.factor(fa$M3)
fa$Arcade = as.factor(fa$Arcade)

######################## DATA EXPLORATION -- FA TRENDS

# Explore FA trends across tooth, class, arcade, and metric
# to compare baboon patterns to those reported in humans

leveneTest(FA10 ~ Tooth, data = fa, center = mean)
leveneTest(FA10 ~ Class, data = fa, center = mean)
leveneTest(FA10 ~ Arcade, data = fa, center = mean)
leveneTest(FA10 ~ Metric, data = fa, center = mean)

leveneTest(FA10 ~ Class * Sex, data = fa, center = mean)
leveneTest(FA10 ~ Arcade * Sex, data = fa, center = mean)
leveneTest(FA10 ~ Metric * Sex, data = fa, center = mean)


######################## HYPOTHESIS TESTING

# Hypothesis 1: sex differences in overall developmental instability
leveneTest(FA10 ~ Sex, data = fa, center = mean) #sig

# Hypothesis 2: weaning stress -- is M1 FA higher than other teeth?
leveneTest(FA10 ~ M1, data = fa, center = mean) #n.s.
leveneTest(FA10 ~ M1 * Sex, data = fa, center = mean) #sig

# Hypothesis 3: reproductive stress -- is M3 FA higher than other teeth?
leveneTest(FA10 ~ M3, data = fa, center = mean) #ns.s
leveneTest(FA10 ~ M3 * Sex, data = fa, center = mean)#sig

######################## LIFE HISTORY COMPARISON -- M1 VS M3

# Compare FA between weaning (M1) and reproductive (M3) periods
# Subset to M1 and M3 only; exclude "Other" teeth
fasub = filter(fa, Teeth != "Other")
fasubf = filter(fa, Sex == "Female")
fasubm = filter(fa, Sex == "Male")

with(fasub, t.test(FA10[Teeth == "M1"], FA10[Teeth == "M3"])) #n.s.
with(fasubf, t.test(FA10[Teeth == "M1"], FA10[Teeth == "M3"])) #n.s.
with(fasubm, t.test(FA10[Teeth == "M1"], FA10[Teeth == "M3"])) #n.s.

######################## FIGURES

fa.sorted = fa[order(fa$FA10), ]

# Figure 1: FA10a values by variable (stacked bar) and by sex (boxplot)
p1 = ggplot(fa.sorted, aes(reorder(Measurement, -FA10, sum), FA10, fill = Sex)) +
    geom_bar(stat = "identity") +
    xlab("") +
    scale_fill_viridis_d() +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

p2 = ggplot(fa, aes(Sex, FA10, fill = Sex)) +
    geom_boxplot(color = "black") +
    labs(title="H1: original paper data") +
    scale_fill_viridis_d() +
    theme_classic()

png("paper--fig1.png", width = 10, height = 5, units = "in",
    res = 300, pointsize = 12, family = "sans")
grid.arrange(p1, p2, nrow = 1)
dev.off()

# Figure 2: M1 vs other teeth (pooled and by sex)
p3 = ggplot(fa, aes(M1, FA10, fill = M1)) +
    geom_boxplot(color = "black") +
    scale_fill_viridis_d() +
    theme_classic()

p4 = ggplot(fa, aes(M1, FA10, fill = Sex)) +
    geom_boxplot(color = "black") +
    labs(title="H2: original paper data") +
    facet_wrap(~ Sex) +
    scale_fill_viridis_d() +
    theme_classic()

png("paper-fig2.png", width = 8, height = 3, units = "in",
    res = 300, pointsize = 12, family = "sans")
grid.arrange(p3, p4, nrow = 1)
dev.off()

# Figure 3: M3 vs other teeth (pooled and by sex)
p5 = ggplot(fa, aes(M3, FA10, fill = M3)) +
    geom_boxplot(color = "black") +
    scale_fill_viridis_d() +
    theme_classic()

p6 = ggplot(fa, aes(M3, FA10, fill = Sex)) +
    geom_boxplot(color = "black") +
    labs(title="H3: original paper data") +
    facet_wrap(~ Sex) +
    scale_fill_viridis_d() +
    theme_classic()

png("paper--fig3.png", width = 8, height = 3, units = "in",
    res = 300, pointsize = 12, family = "sans")
grid.arrange(p5, p6, nrow = 1)
dev.off()

######################## TIDY
rm(list = ls())
gc()