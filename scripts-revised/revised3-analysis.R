######################## LIBRARIES
library(dplyr)        # data manipulation
library(tidyr)        # data reshaping
library(readr)        # read CSV files
library(car)          # Levene's test
library(ggplot2)      # data visualization
library(plotly)       # interative plots with tooltip
library(htmlwidgets)  # save plotly interactive plots
library(gridExtra)    # arrange multiple plots
library(cowplot)      # arrange multiple plots
library(ggridges)     # ridge/density plots





######################## DATA DISTRIBUTION
#### for distribution visualization
build_sd_long = function(f_data) {
    dat = read_csv(f_data, show_col_types = FALSE) |>
        filter(!is.na(Value)) |>
        group_by(Ind, Sex, Dimension, Trait, Side) |>
        summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop")

    left_df  = dat |> filter(Side == "L") |> rename(val_l = Value) |> select(-Side)
    right_df = dat |> filter(Side == "R") |> rename(val_r = Value) |> select(-Side)

    inner_join(left_df, right_df, by = c("Ind", "Sex", "Dimension", "Trait")) |>
        mutate(diff = val_r - val_l) |>
        select(Ind, Sex, Dimension, Trait, diff)
}

### load data
fa_distribution = build_sd_long("data-revised/data-revised-fromStep9.csv")


#### plot
ggplot(fa_distribution, aes(x = diff, y = interaction(Sex, Dimension), fill = interaction(Sex, Dimension))) +
    geom_density_ridges(alpha = 0.7) +
    scale_fill_viridis_d(option = "mako", begin = 0.2, end = 0.8,
                         breaks = c("female.breadth", "male.breadth", "female.length", "male.length")) +
    labs(x = "R-L", y = NULL, fill = NULL) +
    theme_classic()
ggsave("revised-fig-ridgeline.png", width = 9, height = 3, dpi = 300)

rm(fa_distribution, build_sd_long)





######################## LOAD DATA
wrangle_fa = function(path) {
    fa = read_csv(path, show_col_types = FALSE)
    names(fa) = tolower(names(fa))
    if ("trait" %in% names(fa)) fa = fa |> rename(tooth = trait)
    fa = fa |> select(sex, dimension, tooth, pct_me, fa10_nome)
    fa = fa |>
        mutate(
            position = case_when(
                grepl("m1", tooth) ~ "first",
                TRUE ~ "notFirst"
            ),
            class = case_when(
                grepl("p", tooth) ~ "premolar",
                TRUE ~ "molar"
            ),
            arcade = case_when(
                grepl("mx", tooth) ~ "maxillary",
                grepl("mn", tooth) ~ "mandibular"
            )
        ) |>
        rename(fa10 = fa10_nome)
    fa$sex         = as.factor(fa$sex)
    fa$dimension   = as.factor(fa$dimension)
    fa$tooth       = as.factor(fa$tooth)
    fa$class       = as.factor(fa$class)
    fa$position    = as.factor(fa$position)
    fa$arcade      = as.factor(fa$arcade)
    droplevels(fa)
}

#load fa10b (ln to account for trait size differences)
fa = wrangle_fa("data-revised/data-revised-hypothesis.csv")
rm(wrangle_fa)





######################## DATASETS FOR EXPLORING RESEARCH QUESTIONS AND PLOTS

# ln back to mm for figures to show actual sizes
fa_mm = fa |>
    mutate(fa10_mm = exp(fa10)) |>
    ungroup()

# lengths only data
fa_l = droplevels(filter(fa, dimension == "length"))

# ln back to mm for figures to show actual sizes
fa_l_mm = fa_l |>
    mutate(fa10_mm = exp(fa10)) |>
    ungroup()




######################## FA10b V PCT_ME
# FA10b excludes ME
# shows which teeth were most impacted
fa_mm |>
    ggplot(aes(x = pct_me, y = fa10)) +
    geom_point(aes(color = tooth), size = 4) +
    scale_color_viridis_d() +
    scale_shape_manual(values = c(77, 88)) +
    theme_minimal() +
    labs(title = "", x = "% ME", y = "FA10b")
ggsave("revised-fig-percentME.png", width = 8, height = 4, dpi = 300)



######################## HUMAN TRENDS

# arcade
fa_mm |>
    ggplot(aes(x = tooth, y = fa10)) +
    geom_point(aes(shape = arcade, color = sex), size = 4) +
    scale_color_viridis_d(option = "rocket", begin = 0.2, end = 0.5) +
    scale_shape_manual(values = c(77, 88)) +
    theme_minimal() +
    labs(title = "", x = "% ME", y = "FA10b")
ggsave("revised-fig-arcade.png", width = 8, height = 4, dpi = 300)

#class
fa_mm |>
    ggplot(aes(x = tooth, y = fa10, shape = class, color = sex)) +
    geom_jitter(aes(shape = class, color = sex), size = 4, width = 0.5, height = 0) +
    scale_color_viridis_d(option = "rocket", begin = 0.2, end = 0.5) +
    scale_shape_manual(values = c(77, 80)) +
    theme_minimal() +
    labs(title = "", x = "% ME", y = "FA10b")
ggsave("revised-fig-class.png", width = 8, height = 4, dpi = 300)

#dimension
fa_mm |>
    ggplot(aes(x = tooth, y = fa10)) +
    geom_jitter(aes(shape = dimension, color=sex), size = 4, width = 0.5, height = 0) +
    scale_color_viridis_d(option = "rocket", begin = 0.2, end = 0.5) +
    scale_shape_manual(values = c(66, 76)) +
    theme_minimal() +
    labs(title = "", x = "% ME", y = "FA10b")
ggsave("revised-fig-dimension.png", width = 8, height = 4, dpi = 300)

# position, not useful b/c we only have m1 for polar so this is
fa_mm |>
    ggplot(aes(x = tooth, y = fa10)) +
    geom_jitter(aes(shape = position, color=sex), size = 4, width = 0.5, height = 0) +
    scale_color_viridis_d(option = "rocket", begin = 0.2, end = 0.5) +
    scale_shape_manual(values = c(80, 111)) +
    theme_minimal() +
    labs(title = "", x = "% ME", y = "FA10b")
ggsave("revised-fig-position.png", width = 8, height = 4, dpi = 300)





######################## LEVENE FUNCTION
run_levene = function(formula, data, hypothesis) {
    raw = leveneTest(formula, data = data, center = mean)
    data.frame(
        hypothesis = hypothesis,
        df1        = raw[1, "Df"],
        df2        = raw[2, "Df"],
        f_value    = raw[1, "F value"],
        p_value    = raw[1, "Pr(>F)"],
        sig        = ifelse(raw[1, "Pr(>F)"] < 0.05, "*", "")
    )
}



######################## HUMAN TRENDS EXPLORATION

# Tooth, class, arcade, dimension have established FA patterns in humans, not yet explored in baboons.

run_exploration = function(fa) {
    results = list()

    run_levene_mean = function(label, formula, data) {
        tryCatch({
            raw = leveneTest(formula, data = data, center = mean)
            data.frame(
                test      = label,
                statistic = raw[1, "F value"],
                df1       = raw[1, "Df"],
                df2       = if (nrow(raw) >= 2) raw[2, "Df"] else NA,
                p_value   = raw[1, "Pr(>F)"],
                sig       = ifelse(raw[1, "Pr(>F)"] < 0.05, "*", "")
            )
        }, error = function(e) {
            cat("Skipping", label, ":", conditionMessage(e), "\n")
            data.frame(
                test      = label,
                statistic = NA, df1 = NA, df2 = NA,
                p_value   = NA,
                sig       = "insufficient data"
            )
        })
    }

    fa_f = droplevels(filter(fa, sex == "female"))
    fa_m = droplevels(filter(fa, sex == "male"))

    for (var in c("class", "arcade", "dimension")) {
        f_pooled      = as.formula(paste("fa10 ~", var))
        f_interaction = as.formula(paste("fa10 ~", var, "* sex"))

        results[[paste0(var, "_pooled")]]      = run_levene_mean(paste0(var, " (pooled)"),      f_pooled,      fa)
        results[[paste0(var, "_interaction")]] = run_levene_mean(paste0(var, " * sex"),          f_interaction, fa)
        results[[paste0(var, "_female")]]      = run_levene_mean(paste0(var, " (female only)"),  f_pooled,      fa_f)
        results[[paste0(var, "_male")]]        = run_levene_mean(paste0(var, " (male only)"),    f_pooled,      fa_m)
    }

    bind_rows(results)
}

results_exp = run_exploration(fa)

write.csv(
    results_exp,
    "data-revised/data-revised-analysis-exploration.csv",
    row.names = FALSE
)
cat("Written: data-revised/data-revised-analysis-exploration.csv\n")


# premolars are clearly different than other teeth, suggesting the stage between weaning and reproduction is more stressful.


######################## TESTS

results_hypothesis = bind_rows(
    # all dimensions
    run_levene(fa10 ~ class, fa, "Premolar > FA"),
    run_levene(fa10 ~ position, filter(fa, class != "premolar"), "Weaning"),
    run_levene(fa10 ~ repro, filter(fa, class != "premolar") |>
                   mutate(repro = as.factor(ifelse(grepl("m3", tooth), "m3", "notM3"))), "Reproduction"),

    # lengths
    run_levene(fa10 ~ class, fa_l, "Premolar > FA-lengths"),
    run_levene(fa10 ~ position, filter(fa_l, class != "premolar"), "Weaning-lengths"),
    run_levene(fa10 ~ repro, filter(fa_l, class != "premolar") |>
                   mutate(repro = as.factor(ifelse(grepl("m3", tooth), "m3", "notM3"))), "Reproduction-lengths")
)

write.csv(results_hypothesis,
          "data-revised/data-revised-analysis-hypothesis.csv",
          row.names = FALSE)
cat("Written: data-revised/data-revised-analysis-hypothesis.csv\n")


######################## FIGURES
## See revised-analysis-revised-data.R and revised-analysis-paper-data.R for testing original hypotheses.

# Sex differences in lengths
fa_mm |>
    ggplot(aes(x = tooth, y = fa10)) +
    geom_jitter(aes(color = dimension, shape = sex), size = 4, width = 0.5, height = 0) +
    scale_color_viridis_d(option = "rocket", begin = 0.2, end = 0.5) +
    scale_shape_manual(values = c(70, 77)) +
    theme_minimal() +
    labs(title = "", x = "", y = "deviation from group mean (mm)") +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1))
ggsave("revised-fig-sexDifferences.png", width = 8, height = 4, dpi = 300)

# Premolar lengths
fa_l_mm |>
    group_by(class) |>
    mutate(dev_class = (fa10_mm - mean(fa10_mm, na.rm = TRUE)) * 1000) |>
    ungroup() |>
    ggplot(aes(x = tooth, y = dev_class, shape = class, color = sex)) +
    geom_jitter(size = 4, width = 0.5, height = 0) +
    scale_color_viridis_d(option = "rocket", begin = 0.2, end = 0.5) +
    scale_shape_manual(values = c(77, 80)) +
    theme_minimal() +
    labs(title = "", x = "", y = "deviation from group mean (x10???³ mm)")
ggsave("revised-fig-premolarL.png", width = 8, height = 4, dpi = 300)

# Weaning
fa_molars_l = filter(fa_l_mm, class != "premolar") |>
    group_by(position) |>
    mutate(dev_position = (fa10_mm - mean(fa10_mm, na.rm = TRUE)) * 1000) |>
    ungroup()

ggplot(fa_molars_l, aes(x = position, y = dev_position)) +
    geom_jitter(aes(color = sex, shape = sex), size = 4, width = 0.5, height = 0) +
    scale_color_viridis_d(option = "rocket", begin = 0.2, end = 0.5) +
    scale_shape_manual(values = c(70, 77)) +
    theme_minimal() +
    labs(x = NULL, y = "deviation from group mean (x10???³ mm)")
ggsave("revised-fig-weaning.png", width = 8, height = 4, dpi = 300)


# Reproduction
fa_molars_l_repro = filter(fa_l_mm, class != "premolar") |>
    mutate(repro = ifelse(grepl("m3", tooth), "m3", "notM3")) |>
    mutate(repro = as.factor(repro)) |>
    group_by(repro) |>
    mutate(dev_repro = (fa10_mm - mean(fa10_mm, na.rm = TRUE)) * 1000) |>
    ungroup()

ggplot(fa_molars_l_repro, aes(x = repro, y = dev_repro)) +
    geom_jitter(aes(color = sex, shape = sex), size = 4, width = 0.5, height = 0) +
    scale_color_viridis_d(option = "rocket", begin = 0.2, end = 0.5) +
    scale_shape_manual(values = c(70, 77)) +
    theme_minimal() +
    theme(legend.position = "none") +
    labs(x = NULL, y = "deviation from group mean (x10???³ mm)")
ggsave("revised-fig-reproduction.png", width = 6, height = 4, dpi = 300)



######################## TIDY
rm(list = ls())
gc()
