########################
#This script uses the revised data for hypothesis testing to reproduce paper figures and provide R environment control.

# NOTE: This pipeline corrects outlier and skew/kurtosis.

######################## SETUP

library(readxl) # read Excel files
library(ggplot2) # data visualization
library(gridExtra) # arrange multiple plots
library(dplyr) # data manipulation
library(car) # Levene's test

# for hypothesis testing
wrangle_fa = function(path) {
    fa = read_csv(path, show_col_types = FALSE)
    names(fa) = tolower(names(fa))
    if ("trait" %in% names(fa)) fa = fa |> rename(trait = trait)
    fa = fa |> select(sex, dimension, trait, pct_me, fa10_nome)
    fa = fa |>
        mutate(
            position = case_when(
                grepl("m1", trait) ~ "first",
                TRUE ~ "notFirst"
            ),
            class = case_when(
                grepl("p", trait) ~ "premolar",
                TRUE ~ "molar"
            ),
            arcade = case_when(
                grepl("mx", trait) ~ "maxillary",
                grepl("mn", trait) ~ "mandibular"
            ),
            measurement = paste(trait, dimension, sep = "_"),
            lifeHistory = case_when(
                grepl("m1", trait) ~ "m1",
                grepl("m3", trait) ~ "m3",
                TRUE ~ "other"
            ),
            h2 = ifelse(grepl("m1", trait), "m1", "notm1"),
            h3 = ifelse(grepl("m3", trait), "m3", "notm3")
        ) |>
        rename(fa10 = fa10_nome)
    fa$sex         = as.factor(fa$sex)
    fa$dimension   = as.factor(fa$dimension)
    fa$trait       = as.factor(fa$trait)
    fa$measurement = as.factor(fa$measurement)
    fa$class       = as.factor(fa$class)
    fa$position    = as.factor(fa$position)
    fa$arcade      = as.factor(fa$arcade)
    fa$lifeHistory = as.factor(fa$lifeHistory)
    fa$h2          = as.factor(fa$h2)
    fa$h3          = as.factor(fa$h3)
    droplevels(fa)
}

fa = wrangle_fa("data-revised/data-revised-hypothesis.csv")


######################## DATA EXPLORATION -- FA TRENDS

# Explore FA trends across trait, class, arcade, and dimension
# to compare baboon patterns to those reported in humans

leveneTest(fa10 ~ trait, data = fa, center = mean) #sig
leveneTest(fa10 ~ class, data = fa, center = mean) #sig
leveneTest(fa10 ~ arcade, data = fa, center = mean) #ns
leveneTest(fa10 ~ dimension, data = fa, center = mean) #sig

leveneTest(fa10 ~ class * sex, data = fa, center = mean) #sig
leveneTest(fa10 ~ arcade * sex, data = fa, center = mean) #sig
leveneTest(fa10 ~ dimension * sex, data = fa, center = mean) #sig


######################## HYPOTHESIS TESTING

# Hypothesis 1: sex differences in overall developmental instability
leveneTest(fa10 ~ sex, data = fa, center = mean) #ns

# Hypothesis 2: weaning stress -- is m1 FA higher than other teeth?
leveneTest(fa10 ~ h2, data = fa, center = mean) #n.s.
leveneTest(fa10 ~ h2 * sex, data = fa, center = mean) #ns

# Hypothesis 3: reproductive stress -- is m3 FA higher than other teeth?
leveneTest(fa10 ~ h3, data = fa, center = mean) #ns
leveneTest(fa10 ~ h3 * sex, data = fa, center = mean) #ns

######################## LIFE HISTORY COMPARISON -- m1 VS m3

# Compare FA between weaning (m1) and reproductive (m3) periods
# Subset to m1 and m3 only; exclude "Other" teeth
fasub = fa |> filter(lifeHistory != "other") |> droplevels()
t.test(fa10 ~ lifeHistory, data=fasub) #n.s.

fasubf = filter(fa, sex == "female")
fasubf = fasubf |> filter(lifeHistory != "other") |> droplevels()
t.test(fa10 ~ lifeHistory, data=fasubf) #n.s.

fasubm = filter(fa, sex == "male")
fasubm = fasubm |> filter(lifeHistory != "other") |> droplevels()
t.test(fa10 ~ lifeHistory, data=fasubm) #n.s.



######################## FIGURES

fa.sorted = fa[order(fa$fa10), ]

# Figure 1: fa10a values by variable (stacked bar) and by sex (boxplot)
p1 = ggplot(fa.sorted, aes(reorder(measurement, -fa10, sum), fa10, fill = sex)) +
    geom_bar(stat = "identity") +
    xlab("") +
    scale_fill_viridis_d() +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

p2 = ggplot(fa, aes(sex, fa10, fill = sex)) +
    geom_boxplot(color = "black") +
    labs(title="H1: original paper data") +
    scale_fill_viridis_d() +
    theme_classic()

png("paper-revised-data-fig1.png", width = 10, height = 5, units = "in",
    res = 300, pointsize = 12, family = "sans")
grid.arrange(p1, p2, nrow = 1)
dev.off()

# Figure 2: m1 vs other teeth (pooled and by sex)
p3 = ggplot(fa, aes(h2, fa10, fill = h2)) +
    geom_boxplot(color = "black") +
    scale_fill_viridis_d() +
    theme_classic()

p4 = ggplot(fa, aes(h2, fa10, fill = sex)) +
    geom_boxplot(color = "black") +
    labs(title="H2: original paper data") +
    facet_wrap(~ sex) +
    scale_fill_viridis_d() +
    theme_classic()

png("paper-revised-data-fig2.png", width = 8, height = 3, units = "in",
    res = 300, pointsize = 12, family = "sans")
grid.arrange(p3, p4, nrow = 1)
dev.off()

# Figure 3: m3 vs other teeth (pooled and by sex)
p5 = ggplot(fa, aes(h3, fa10, fill = h3)) +
    geom_boxplot(color = "black") +
    scale_fill_viridis_d() +
    theme_classic()

p6 = ggplot(fa, aes(h3, fa10, fill = sex)) +
    geom_boxplot(color = "black") +
    labs(title="H3: original paper data") +
    facet_wrap(~ sex) +
    scale_fill_viridis_d() +
    theme_classic()

png("paper-revised-data-fig3.png", width = 8, height = 3, units = "in",
    res = 300, pointsize = 12, family = "sans")
grid.arrange(p5, p6, nrow = 1)
dev.off()

######################## TIDY
rm(list = ls())
gc()
