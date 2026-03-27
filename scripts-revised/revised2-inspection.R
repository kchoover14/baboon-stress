######################## LIBRARIES
library(readr)       # read CSV files
library(dplyr)       # data manipulation
library(tidyr)       # reshaping (pivot_longer, pivot_wider)
library(ggplot2)     # plotting
library(car)         # leveneTest, scatterplotMatrix
library(outliers)    # dixon.test
library(DescTools)   # Skew, Kurt
library(Hmisc)       # describe
library(coin)        # spearman_test
library(plotly)      # interactive plots
library(htmlwidgets) # save interactive plots as HTML

######################## SOURCE SHARED FILES
source("scripts-revised/fa-functions.R")
source("scripts-revised/fa-constants.R")




######################## NORMALITY SCREENING
# Scatterplot matrix per group (one plot per sex x dimension).
# Produces 4 tiff files in figures-inspection/.
# Diagonal panels show QQ plots; off-diagonal panels show pairwise scatters.
# Used to assess distributional properties of FA10a values before screening.
dir.create("figures-inspection", showWarnings = FALSE)
run_normality_scatters(f_wrangled, "normal-scatters-raw")

# there are tails and a few s-shaped curves which is typical for biological data
# no deviations appear to exceed expectations of normality
# parametric testing is appropriate.


######################## STEPS 1-2: ME OUTLIER DETECTION (RAW DATA)
# For each group (sex x dimension), computes |R-L| values per
# individual per trait, then consecutive pair differences of those values.
# Dixon's two-tailed test is run per individual per trait against the group
# mean of consecutive pair differences AND against zero.
# Bonferroni correction applied within each group separately.
#
# STEP 1 is skipped: the number of replicate-pair plots is prohibitive.
# STEP 2: Dixon's test per trait per group.

# outlier testing
me_dixon_rows = list()
for (grp in names(group_labels)) {
    g = load_group(f_wrangled, grp, group_trait_vars[[grp]])
    me_dixon_rows[[grp]] = run_me_dixon(g$df, g$trait_cols, grp, group_labels[[grp]])
}

me_dixon_all = do.call(rbind, me_dixon_rows)
write_csv(me_dixon_all, f_checkpoint_step12)
cat("Written:", f_checkpoint_step12, "\n")


# Review f_checkpoint_step12.
# sig_mean or sig_zero == "*" indicates a flagged individual x trait.
# Use diagnose_me_flagged() to identify which specific replicate is the offender.
# The extreme_val column and pair labels in the diagnostic output show which
# consecutive pair difference is driving the flag.
# abs_rl_a is the lower numbered replicate
# abs_fl_b is the higher numbered replicate
# select the outlier, extreme high or lower: compare to other values

raw_groups = list()
for (grp in names(group_labels)) {
    g = load_group(f_wrangled, grp, group_trait_vars[[grp]])
    raw_groups[[grp]] = g$df
}
me_diagnostic = diagnose_me_flagged(me_dixon_all, raw_groups)

# populate results in constants



######################## STEPS 3-5
# Each step reads from f_fromStep2 (post-ME CSV) and can be run independently.
#
# Step 3: L vs R scatterplots -- reveals extreme asymmetries and extreme-sized
#         individuals within traits (visual inspection only)
# Step 4: (R-L) vs (R-L) cross-trait scatterplots -- reveals subtler FA
#         outliers among individuals and traits (visual inspection only)
# Step 5: Dixon's test on signed (R-L) side differences -- statistical
#         confirmation of univariate outliers per trait; cross-trait patterns
#         from Step 4 require visual judgment and may not always be confirmed here




######################## STEP 3: L vs R SCATTERPLOTS (ALL GROUPS)
# Visual inspection only. Produces one interactive HTML plot per group.
# Requires: f_fromStep2

for (grp in names(group_labels)) {
    g   = load_group(f_wrangled, grp, group_trait_vars[[grp]])
    rl  = build_rl(g$df, g$trait_cols)

    rl_lims = range(c(rl$val_left, rl$val_right), na.rm = TRUE)
    rl_pad  = diff(rl_lims) * 0.05
    rl_lo   = rl_lims[1] - rl_pad
    rl_hi   = rl_lims[2] + rl_pad

    p = ggplot(rl, aes(val_left, val_right, text = Ind)) +
        geom_point() +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
        coord_equal(xlim = c(rl_lo, rl_hi), ylim = c(rl_lo, rl_hi)) +
        facet_wrap(~var) + theme_classic() +
        labs(x = "Left", y = "Right",
             title = paste("Step 3: L vs R --", group_labels[[grp]]))

    fname = file.path("figures-inspection",
                      paste0("step3-lvr-", grp, ".html"))
    saveWidget(ggplotly(p, tooltip = "text"), file = fname, selfcontained = TRUE)
    cat("Written:", fname, "\n")
}




######################## STEP 4: CROSS-TRAIT (R-L) SCATTERPLOTS (ALL GROUPS)
# Visual inspection only. Produces one HTML plot per trait pair per group.
# Requires: f_fromStep2

for (grp in names(group_labels)) {
    g        = load_group(f_wrangled, grp, group_trait_vars[[grp]])
    sd_wide  = build_sd_wide(g$df, g$trait_cols)
    sd_cols  = grep("_sd$", names(sd_wide), value = TRUE)

    if (length(sd_cols) < 2) next

    trait_pairs = combn(sd_cols, 2)
    for (i in seq_len(ncol(trait_pairs))) {
        xv = trait_pairs[1, i]; yv = trait_pairs[2, i]
        p  = ggplot(sd_wide, aes(.data[[xv]], .data[[yv]], text = Ind)) +
            geom_point() +
            theme_classic() +
            labs(x = xv, y = yv,
                 title = paste("Step 4: Cross-trait (R-L) --", group_labels[[grp]]))
        fname = file.path("figures-inspection",
                          paste0("step4-sd-", grp, "-",
                                 gsub("_sd$", "", xv), "_vs_",
                                 gsub("_sd$", "", yv), ".html"))
        saveWidget(ggplotly(p, tooltip = "text"), file = fname, selfcontained = TRUE)
    }
    cat("Step 4 plots saved:", group_labels[[grp]], "\n")
}





######################## STEP 5: DIXON'S TEST ON (R-L) SIDE DIFFERENCES
# Bonferroni correction applied within each group (sex x dimension) separately
# Fully independent: reads f_fromStep2 directly, no dependency on Steps 3-4.

fa_dixon_rows = list()

for (grp in names(group_labels)) {
    g = load_group(f_wrangled, grp, group_trait_vars[[grp]])
    fa_dixon_rows[[grp]] = run_fa_dixon(g$df, g$trait_cols, grp, group_labels[[grp]])
}

fa_dixon_all = do.call(rbind, fa_dixon_rows)
write_csv(fa_dixon_all, f_checkpoint_step345)
cat("Written:", f_checkpoint_step345, "\n")


# Review f_checkpoint_step345.
# Statistical confirmation of univariate FA outliers per trait.
# sig_mean or sig_zero == "*" indicates a flagged individual x trait.

# Use diagnose_me_flagged() to identify which specific replicate is the offender.

raw_groups = list()
for (grp in names(group_labels)) {
    g = load_group(f_wrangled, grp, group_trait_vars[[grp]])
    raw_groups[[grp]] = g$df
}
fa_diagnostic = diagnose_fa_flagged(fa_dixon_all, raw_groups)

# populate results in constants






######################## OUTLIER REMOVAL PRIOR TO STEP 6
# Applies all ME (Steps 1-2) and FA (Steps 3-5) replicate removals in one pass.
# Removal is at the level of the specific offending replicate per individual
# per trait, not the entire individual x trait combination.
# Reload constants to pick up me_dixon_removals and fa_dixon_removals.

source("scripts-revised/fa-constants.R")

all_removals = c(me_dixon_removals, fa_dixon_removals)
cat("Total replicate removals loaded:", length(all_removals), "\n")

posts5 = read_csv(f_wrangled, show_col_types = FALSE)

for (entry in all_removals) {
    posts5 = null_replicate(posts5, entry$ind, entry$trait, entry$rep)
}

cat("Replicate removals applied:", length(all_removals), "\n")
write_csv(posts5, f_fromStep5)
cat("Written:", f_fromStep5, "\n")





######################## STEP 6: SIDES x INDIVIDUALS MIXED MODEL (FA > ME)
# Runs the Palmer & Strobeck mixed model ANOVA per trait per group.
# Only traits with significant FA > ME pass this checkpoint.
# Bonferroni correction applied within each group (sex x dimension).
# Requires: f_fromStep5

aov_inputs = list()
for (grp in names(group_labels)) {
    g = load_group(f_fromStep5, grp, group_trait_vars[[grp]])
    aov_inputs[[grp]] = run_univariates(g$df, g$trait_cols, grp, group_labels[[grp]])
}

step6_out = list()
for (grp in names(group_labels)) {
    step6_out[[grp]] = run_mixedmodel(grp, aov_inputs)
}

step6_all = do.call(rbind, step6_out)
write_csv(step6_all, f_step6)
cat("\nWritten:", f_step6, "\n")

# >>> CHECKPOINT: STEP 6 <<<
# Review f_step6 (anova_alpha column) per group.
# Remove any trait where FA > ME is n.s. from all downstream steps.
# Populate step6_eliminations in fa-constants.R if any removals needed.
# Format: list(sex="...", dimension="...", trait="var_name")
# Save fa-constants.R before continuing.


######################## STEP 6 REMOVAL
# Removal unit: entire trait column for a group.
# Reload constants to pick up step6_eliminations.

source("scripts-revised/fa-constants.R")
cat("Step 6 eliminations loaded:", length(step6_eliminations), "entries\n")

posts6 = read_csv(f_fromStep5, show_col_types = FALSE)

for (entry in step6_eliminations) {
    # Match rows for this group and set Value to NA for the eliminated trait
    if (identical(entry$sex, "all")) {
        posts6$Value[posts6$Trait == entry$trait] = NA
    } else {
        posts6$Value[
            posts6$Sex       == entry$sex       &
                posts6$Dimension == entry$dimension &
                posts6$Trait     == entry$trait
        ] = NA
    }
}

write_csv(posts6, f_fromStep6)
cat("Written:", f_fromStep6, "\n")







######################## STEP 7: LEVENE'S TEST ON ME VARIANCE
# Tests whether ME variance is homogeneous across sex, dimension, and trait.
# ME measured as |consecutive replicate differences| per individual per trait
# per side, following Palmer & Strobeck (2003).
# Requires: f_fromStep5

me_var = read_csv(f_fromStep5, show_col_types = FALSE) |>
    filter(!is.na(Value)) |>
    arrange(Ind, Sex, Dimension, Trait, Side, Rep) |>
    group_by(Ind, Sex, Dimension, Trait, Side) |>
    summarise(
        me = list(abs(diff(Value))),
        .groups = "drop"
    ) |>
    tidyr::unnest(me) |>
    filter(!is.na(me))

leveneTest(me ~ Sex * Dimension * Trait, data = me_var, center = mean)







######################## STEP 8: TRAIT SIZE DEPENDENCY
# Tests whether FA (|R-L|) depends on trait size [(R+L)/2] per trait per group.
# Bonferroni correction applied within each group (sex x dimension).
# Requires: f_fromStep6


#### STEP 8A: SCATTER PLOTS (run once -- slow)
# Generates interactive |R-L| vs (R+L)/2 scatterplots per trait per group.
# Saved to figures-inspection/. Run once; skip on reruns if plots already exist.

for (grp in names(group_labels)) {
    g = load_group(f_fromStep6, grp, group_trait_vars[[grp]])
    run_step8a_scatters(g$df, g$trait_cols, grp, group_labels[[grp]])
}


#### STEP 8B: SPEARMAN CORRELATION (re-runnable independently)
# Spearman correlation used (rank-based, robust to non-normality).
# Sequential Bonferroni correction applied within each group.
# Elimination only justified if association is significant AND positive
# (larger traits more variable); negative associations are not biologically
# meaningful for size correction (Palmer & Strobeck 2003).

step8_out = list()
for (grp in names(group_labels)) {
    g = load_group(f_fromStep6, grp, group_trait_vars[[grp]])
    step8_out[[grp]] = run_step8b_spearman(g$df, g$trait_cols, grp, group_labels[[grp]])
}

step8_combined = do.call(rbind, step8_out)
write_csv(step8_combined, f_checkpoint_step8)
cat("\nWritten:", f_checkpoint_step8, "\n")

######################## CHECKPOINT: STEP 8
# Review f_checkpoint_step8 and scatterplots in figures-inspection/.
# Eliminate any trait with a significant (after Bonferroni) POSITIVE association.
# Negative associations are not biologically meaningful and do not justify removal.
# Populate step8_eliminations in fa-constants.R with confirmed size-dependent traits.
# Save fa-constants.R before continuing.



######################## STEP 9: IDEAL FA (DA, SKEW, KURTOSIS)
# Compute DA, skew, and kurtosis on post-Step8 data.
# Each test is independently Bonferroni-sorted within each group.
# Eliminate any trait flagged as significant for DA, skew, or kurtosis.
# Requires: f_fromStep6

desc_inputs = list()
for (grp in names(group_labels)) {
    g = load_group(f_fromStep6, grp, group_trait_vars[[grp]])
    desc_inputs[[grp]] = run_descriptives(g$df, g$trait_cols, grp, group_labels[[grp]])
}

step9_out = list()
for (grp in names(group_labels)) {
    step9_out[[grp]] = run_idealfa(grp, desc_inputs)
}

# Combine all three blocks (da, skew, kurt) across all groups into one CSV each
step9_da_all   = do.call(rbind, lapply(step9_out, function(x) x$da))
step9_skew_all = do.call(rbind, lapply(step9_out, function(x) x$skew))
step9_kurt_all = do.call(rbind, lapply(step9_out, function(x) x$kurt))
step9_all      = list(da = step9_da_all, skew = step9_skew_all, kurt = step9_kurt_all)

write_csv(step9_da_all,   sub("\\.csv", "-da.csv",   f_checkpoint_step9))
write_csv(step9_skew_all, sub("\\.csv", "-skew.csv", f_checkpoint_step9))
write_csv(step9_kurt_all, sub("\\.csv", "-kurt.csv", f_checkpoint_step9))
cat("\nWritten: Step 9 checkpoint files\n")


######################## CHECKPOINT: STEP 9
# Review the three checkpoint files (da, skew, kurt).
# Populate step9_eliminations in fa-constants.R with traits to remove.
# Populate step9_eliminations_da with traits for the DA-corrected dataset.
# Save fa-constants.R before continuing.



######################## ASSESSMENT OF DA
# For traits with significant DA, apply Palmer & Strobeck test:
# if |mean(R-L)| (DA) <= FA4a (mean |R-L|), the directional component is
# small relative to random side differences and the trait may be retained.
# Traits where DA > FA4a are excluded; borderline cases excluded conservatively.
# Requires: desc_inputs (already computed above from f_fromStep6)

da_assessment = assess_da_vs_fa4a(f_fromStep6, step9_highDA)
write_csv(da_assessment, sub("\\.csv", "-da-assessment.csv", f_checkpoint_step9_daTest))
cat("Written: DA assessment\n")

# Review da_assessment: traits where exceeds == TRUE require exclusion.
# Borderline cases (da_mean close to fa4a) should be excluded conservatively.
# Populate step9_eliminations in fa-constants.R accordingly.



######################## REMOVALS STEP 9 -- REVISED
# Reads from f_fromStep6. Removes all traits flagged for DA, skew, or kurtosis.
# Populates f_fromStep9revised.

source("scripts-revised/fa-constants.R")

posts9revised = read_csv(f_fromStep6, show_col_types = FALSE)

for (entry in step9_eliminations) {
    for (tr in entry$trait) {
        posts9revised$Value[
            posts9revised$Sex       == entry$sex       &
                posts9revised$Dimension == entry$dimension &
                posts9revised$Trait     == tr
        ] = NA
    }
}

write_csv(posts9revised, f_fromStep9)
cat("Written:", f_fromStep9, "\n")







######################## NORMALITY SCREENING ON FINAL DATASET
run_normality_scatters(f_fromStep9, "normal-scatters-final")
# tails and some s-shaped curves are typical for biological data
# no deviations appear to exceed expectations of normality





######################## GENERATE HYPOTHESIS TESTING DATA
# ln transformation applied to all values per Palmer & Strobeck FA10b
# to account for trait size dependency in mxp4 male length.

aov_inputs_ln = list()
for (grp in names(group_labels)) {
    g = load_group(f_fromStep9, grp, group_trait_vars[[grp]])
    g$df$Value = log(g$df$Value)
    cat(grp, "- first 3 values after log:", head(g$df$Value, 3), "\n")
    aov_inputs_ln[[grp]] = run_univariates(g$df, g$trait_cols, grp, group_labels[[grp]])
}

mm_inputs_ln = list()
for (grp in names(aov_inputs_ln)) {
    mm_inputs_ln[[grp]] = run_mixedmodel(grp, aov_inputs_ln)
}

hypothesis_all = do.call(rbind, mm_inputs_ln)
write_csv(hypothesis_all, f_hypothesis)
cat("Written:", f_hypothesis, "\n")



######################## TIDY
rm(list = ls())
gc()
