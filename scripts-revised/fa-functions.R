######################## fa-functions.R
# Shared analytical functions for the Palmer-Strobeck FA protocol.
# Source this file at the start of each session before running any
# inspection or analysis script:
#   source("scripts-revised/fa-functions.R")
#   source("scripts-revised/fa-constants.R")   # study-specific constants
#
# No study-specific values are defined here. All constants (file paths,
# variable lists, outlier removals) live in fa-constants.R.
# Data I/O uses readr (read_csv / write_csv) throughout.
#
# ME outlier detection (Steps 1-2) uses Dixon's test on consecutive
# pair differences of |R-L| values per individual per trait per group.
# FA outlier detection (Step 5) uses Dixon's test on signed R-L values
# per individual per trait per group.
# Both steps test against the group mean AND against zero (Palmer & Strobeck).
# Bonferroni correction is applied within each group (sex x dimension).

######################## DATA RESHAPING

# pivot_reps_wide: pivots replicates from long to wide.
# Arguments:
#   df         -- long-format dataframe with Ind, Side, Rep, Trait, Value columns
#   trait_cols -- character vector of trait names
# Returns: dataframe with Ind, Side, var, rep1:repN
pivot_reps_wide = function(df, trait_cols) {
    df$Rep = as.factor(df$Rep)
    long = pivot_longer(df |> filter(Trait %in% trait_cols),
                        cols = Value, names_to = "val_col", values_to = "val")
    wide = pivot_wider(long, names_from = Rep, values_from = val,
                       names_prefix = "rep")
    return(wide)
}

# consec_diffs: computes consecutive differences from a numeric vector.
# For ME outlier detection: input is |R-L| values per individual per trait;
# output is consecutive pair differences.
# Arguments:
#   x -- numeric vector of length n (ordered by replicate number)
# Returns: numeric vector of length n-1
consec_diffs = function(x) {
    diff(x)
}

# avg_reps: averages replicates per trait per individual-side row.
# Works on long-format data (Trait/Value columns).
# Arguments:
#   df         -- long-format dataframe with Ind, Side, Rep, Trait, Value columns
#   trait_cols -- character vector of trait names
# Returns: dataframe with one row per Ind-Side, one column per trait (averaged)
avg_reps = function(df, trait_cols) {
    df$Rep = as.integer(as.character(df$Rep))
    df |>
        filter(Trait %in% trait_cols) |>
        group_by(Ind, Side, Trait) |>
        summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop") |>
        pivot_wider(names_from = Trait, values_from = Value)
}

# build_sd_wide: returns wide dataframe of R-L (or |R-L|) side differences
# per trait, averaged across replicates.
# Used by Steps 4 and 5.
# Arguments:
#   df         -- long-format dataframe with Ind, Side, Rep, Trait, Value columns
#   trait_cols -- character vector of trait names
#   abs_val    -- logical; if TRUE returns |R-L|, if FALSE returns signed R-L
# Returns: wide dataframe with Ind and <trait>_sd columns
build_sd_wide = function(df, trait_cols, abs_val = FALSE) {
    df_avg   = avg_reps(df, trait_cols)
    left_df  = df_avg[df_avg$Side == "L", ] |> dplyr::select(-Side)
    right_df = df_avg[df_avg$Side == "R", ] |> dplyr::select(-Side)

    long_l   = pivot_longer(left_df,  cols = all_of(trait_cols),
                            names_to = "var", values_to = "val_left")
    long_r   = pivot_longer(right_df, cols = all_of(trait_cols),
                            names_to = "var", values_to = "val_right")

    sd_df           = merge(long_l, long_r, by = c("Ind", "var"))
    sd_df           = na.omit(sd_df)
    sd_df$diff      = sd_df$val_right - sd_df$val_left
    if (abs_val) sd_df$diff = abs(sd_df$diff)
    sd_df$val_left  = NULL
    sd_df$val_right = NULL

    sd_wide        = pivot_wider(sd_df, names_from = var, values_from = diff)
    names(sd_wide) = ifelse(names(sd_wide) == "Ind", "Ind",
                            paste0(names(sd_wide), "_sd"))
    return(sd_wide)
}

# null_replicate: sets a specific replicate Value to NA for a given
# individual and trait. Used for ME and FA outlier removal where the
# removal unit is the specific offending replicate, not the whole individual.
# Operates on the long-format CSV: matches on Ind, Rep, and Trait columns.
# Arguments:
#   df     -- long-format dataframe with Ind, Rep, Trait, Value columns
#   ind_id -- individual ID string (e.g. "F1")
#   trait  -- trait name (e.g. "mnm1")
#   rep    -- integer replicate number to null
# Returns: dataframe with the specified Value set to NA
null_replicate = function(df, ind_id, trait, rep) {
    df$Value[df$Ind == ind_id & df$Rep == rep & df$Trait == trait] = NA
    return(df)
}

######################## OUTLIER DETECTION

# dixon_q: computes Dixon's Q statistic for the most extreme value in a
# sample relative to a reference value. Two-tailed: tests both the minimum
# and maximum values and returns the larger Q statistic and its p-value.
# Uses Dixon's Q critical value approximation via the outliers package.
# Arguments:
#   vals -- numeric vector (NAs removed before calling)
#   ref  -- reference value to test against (group mean or zero)
# Returns: list with Q statistic, p-value (two-tailed), and direction
#          ("high" or "low") of the extreme value
dixon_q = function(vals, ref) {
    n = length(vals)
    if (n < 3) return(list(Q = NA_real_, p = NA_real_, direction = NA_character_,
                           extreme_val = NA_real_))

    shifted = vals - ref
    rng     = diff(range(shifted))
    if (rng == 0) return(list(Q = NA_real_, p = NA_real_, direction = NA_character_,
                              extreme_val = NA_real_))

    sorted = sort(shifted)
    Q_low  = (sorted[2]   - sorted[1])   / rng
    Q_high = (sorted[n]   - sorted[n-1]) / rng

    if (Q_high >= Q_low) {
        Q         = Q_high
        direction = "high"
        extreme   = vals[which.max(shifted)]
    } else {
        Q         = Q_low
        direction = "low"
        extreme   = vals[which.min(shifted)]
    }

    p_result = tryCatch(
        outliers::dixon.test(vals, type = 0, opposite = (direction == "low"),
                             two.sided = TRUE)$p.value,
        error = function(e) NA_real_
    )

    list(Q = Q, p = p_result, direction = direction, extreme_val = extreme)
}

# run_me_dixon: Step 2 ME outlier detection using Dixon's test.
# For each individual x trait x group:
#   1. Computes |R-L| values (one per replicate)
#   2. Computes consecutive pair differences of those |R-L| values
#   3. Runs Dixon's two-tailed test against the group mean of pair diffs
#      AND against zero
# Bonferroni correction applied within each group separately.
# Arguments:
#   df         -- long-format dataframe for one group (filtered from CSV)
#   trait_cols -- character vector of trait names for this group
#   grp        -- group key string (e.g. "female-breadth")
#   label      -- human-readable group label for console output
# Returns: dataframe with Sex, Dimension, trait columns and test results
run_me_dixon = function(df, trait_cols, grp, label) {
    cat("\n---", label, "---\n")
    parts   = strsplit(grp, "-")[[1]]
    sex_val = parts[1]; dim_val = parts[2]

    rows = list()

    for (v in trait_cols) {
        cat("\n", toupper(v), "\n")

        trait_df = df[df$Trait == v, c("Ind", "Rep", "Side", "Value")]
        trait_df = trait_df[!is.na(trait_df$Value), ]
        trait_df$Rep = as.integer(as.character(trait_df$Rep))

        inds = unique(trait_df$Ind)
        ind_sd_list = list()
        for (ind in inds) {
            sub  = trait_df[trait_df$Ind == ind, ]
            reps = sort(unique(sub$Rep))
            sd_vals = vapply(reps, function(r) {
                r_val = sub$Value[sub$Rep == r & sub$Side == "R"]
                l_val = sub$Value[sub$Rep == r & sub$Side == "L"]
                if (length(r_val) == 1 && length(l_val) == 1 &&
                    !is.na(r_val) && !is.na(l_val)) {
                    abs(r_val - l_val)
                } else {
                    NA_real_
                }
            }, numeric(1))
            names(sd_vals) = reps
            ind_sd_list[[as.character(ind)]] = sd_vals
        }

        all_consec = unlist(lapply(ind_sd_list, function(x) {
            x_clean = x[!is.na(x)]
            if (length(x_clean) >= 2) consec_diffs(x_clean) else numeric(0)
        }))
        group_mean = mean(all_consec, na.rm = TRUE)
        group_sd   = sd(all_consec,   na.rm = TRUE)
        cat(sprintf("  Group mean consec diff: %.6f  SD: %.6f  n: %d\n",
                    group_mean, group_sd, length(all_consec)))

        for (ind in inds) {
            sd_vals  = ind_sd_list[[as.character(ind)]]
            sd_clean = sd_vals[!is.na(sd_vals)]
            if (length(sd_clean) < 2) next
            cd = consec_diffs(sd_clean)
            if (length(cd) < 3) next

            res_mean = dixon_q(cd, ref = group_mean)
            res_zero = dixon_q(cd, ref = 0)

            rows[[length(rows) + 1]] = data.frame(
                Sex       = sex_val,
                Dimension = dim_val,
                trait     = v,
                Ind       = as.character(ind),
                n_consec  = length(cd),
                group_mean  = group_mean,
                group_sd    = group_sd,
                extreme_val = res_mean$extreme_val,
                direction   = res_mean$direction,
                Q_vs_mean   = res_mean$Q,
                p_vs_mean   = res_mean$p,
                Q_vs_zero   = res_zero$Q,
                p_vs_zero   = res_zero$p,
                stringsAsFactors = FALSE
            )
        }
    }

    out = do.call(rbind, rows)
    if (is.null(out) || nrow(out) == 0) return(data.frame())

    out = out[order(out$p_vs_mean), ]
    bonf_mean     = bonferroni_sorted(out$p_vs_mean)
    out$bonf_mean = bonf_mean$bonf
    out$rank_mean = bonf_mean$rank
    out$sig_mean  = bonf_mean$alpha

    out = out[order(out$p_vs_zero), ]
    bonf_zero     = bonferroni_sorted(out$p_vs_zero)
    out$bonf_zero = bonf_zero$bonf
    out$rank_zero = bonf_zero$rank
    out$sig_zero  = bonf_zero$alpha

    out = out[order(out$Sex, out$Dimension, out$trait, out$Ind), ]
    rownames(out) = NULL

    print(out[, c("Sex", "Dimension", "trait", "Ind", "n_consec",
                  "group_mean", "extreme_val",
                  "Q_vs_mean", "p_vs_mean", "sig_mean",
                  "Q_vs_zero", "p_vs_zero", "sig_zero")])
    out
}

# run_fa_dixon: Step 5 FA outlier detection using Dixon's test.
# For each individual x trait x group:
#   Uses signed R-L values (one per replicate, not differenced)
#   Runs Dixon's two-tailed test against group mean R-L AND against zero
# Bonferroni correction applied within each group separately.
# Arguments:
#   df         -- long-format dataframe for one group (filtered from CSV)
#   trait_cols -- character vector of trait names for this group
#   grp        -- group key string (e.g. "female-breadth")
#   label      -- human-readable group label for console output
# Returns: dataframe with Sex, Dimension, trait columns and test results
run_fa_dixon = function(df, trait_cols, grp, label) {
    cat("\n---", label, "---\n")
    parts   = strsplit(grp, "-")[[1]]
    sex_val = parts[1]; dim_val = parts[2]

    rows = list()

    for (v in trait_cols) {
        cat("\n", toupper(v), "\n")

        trait_df = df[df$Trait == v, c("Ind", "Rep", "Side", "Value")]
        trait_df = trait_df[!is.na(trait_df$Value), ]
        trait_df$Rep = as.integer(as.character(trait_df$Rep))

        inds = unique(trait_df$Ind)
        ind_rl_list = list()
        for (ind in inds) {
            sub  = trait_df[trait_df$Ind == ind, ]
            reps = sort(unique(sub$Rep))
            rl_vals = vapply(reps, function(r) {
                r_val = sub$Value[sub$Rep == r & sub$Side == "R"]
                l_val = sub$Value[sub$Rep == r & sub$Side == "L"]
                if (length(r_val) == 1 && length(l_val) == 1 &&
                    !is.na(r_val) && !is.na(l_val)) {
                    r_val - l_val
                } else {
                    NA_real_
                }
            }, numeric(1))
            names(rl_vals) = reps
            ind_rl_list[[as.character(ind)]] = rl_vals
        }

        all_rl     = unlist(lapply(ind_rl_list, function(x) x[!is.na(x)]))
        group_mean = mean(all_rl, na.rm = TRUE)
        group_sd   = sd(all_rl,   na.rm = TRUE)
        cat(sprintf("  Group mean R-L: %.6f  SD: %.6f  n: %d\n",
                    group_mean, group_sd, length(all_rl)))

        for (ind in inds) {
            rl_vals  = ind_rl_list[[as.character(ind)]]
            rl_clean = rl_vals[!is.na(rl_vals)]
            if (length(rl_clean) < 3) next

            res_mean = dixon_q(rl_clean, ref = group_mean)
            res_zero = dixon_q(rl_clean, ref = 0)

            rows[[length(rows) + 1]] = data.frame(
                Sex       = sex_val,
                Dimension = dim_val,
                trait     = v,
                Ind       = as.character(ind),
                n_reps    = length(rl_clean),
                group_mean  = group_mean,
                group_sd    = group_sd,
                extreme_val = res_mean$extreme_val,
                direction   = res_mean$direction,
                Q_vs_mean   = res_mean$Q,
                p_vs_mean   = res_mean$p,
                Q_vs_zero   = res_zero$Q,
                p_vs_zero   = res_zero$p,
                stringsAsFactors = FALSE
            )
        }
    }

    out = do.call(rbind, rows)
    if (is.null(out) || nrow(out) == 0) return(data.frame())

    out = out[order(out$p_vs_mean), ]
    bonf_mean     = bonferroni_sorted(out$p_vs_mean)
    out$bonf_mean = bonf_mean$bonf
    out$rank_mean = bonf_mean$rank
    out$sig_mean  = bonf_mean$alpha

    out = out[order(out$p_vs_zero), ]
    bonf_zero     = bonferroni_sorted(out$p_vs_zero)
    out$bonf_zero = bonf_zero$bonf
    out$rank_zero = bonf_zero$rank
    out$sig_zero  = bonf_zero$alpha

    out = out[order(out$Sex, out$Dimension, out$trait, out$Ind), ]
    rownames(out) = NULL

    print(out[, c("Sex", "Dimension", "trait", "Ind", "n_reps",
                  "group_mean", "extreme_val",
                  "Q_vs_mean", "p_vs_mean", "sig_mean",
                  "Q_vs_zero", "p_vs_zero", "sig_zero")])
    out
}

######################## DIAGNOSTIC

# diagnose_fa_flagged: for each flagged individual x trait from run_fa_dixon,
# computes the signed R-L values per replicate and identifies which replicate
# produced the extreme value.
# Arguments:
#   dixon_out  -- dataframe returned by run_fa_dixon (full output, all rows)
#   raw_groups -- named list of long-format group dataframes, keyed by group key
#                 (e.g. list("female-breadth" = df, ...))
# Returns: dataframe with one row per replicate per flagged individual x trait,
#          including a column flagging which replicate is the likely offender
diagnose_fa_flagged = function(dixon_out, raw_groups) {
    flagged = dixon_out[dixon_out$sig_mean == "*" | dixon_out$sig_zero == "*", ]
    if (nrow(flagged) == 0) {
        cat("No flagged cases to diagnose.\n")
        return(data.frame())
    }

    rows = list()

    for (i in seq_len(nrow(flagged))) {
        grp   = paste(flagged$Sex[i], flagged$Dimension[i], sep = "-")
        trait = flagged$trait[i]
        ind   = flagged$Ind[i]

        df = raw_groups[[grp]]
        if (is.null(df)) {
            cat("Warning: no raw data found for group", grp, "\n")
            next
        }

        sub = df[df$Ind == ind & df$Trait == trait, c("Rep", "Side", "Value")]
        sub = sub[!is.na(sub$Value), ]
        sub$Rep = as.integer(as.character(sub$Rep))
        reps = sort(unique(sub$Rep))

        rl_vals = vapply(reps, function(r) {
            rv = sub$Value[sub$Rep == r & sub$Side == "R"]
            lv = sub$Value[sub$Rep == r & sub$Side == "L"]
            if (length(rv) == 1 && length(lv) == 1) rv - lv else NA_real_
        }, numeric(1))
        names(rl_vals) = reps
        rl_vals = rl_vals[!is.na(rl_vals)]

        gm = flagged$group_mean[i]
        deviations = abs(rl_vals - gm)

        for (j in seq_along(rl_vals)) {
            rows[[length(rows) + 1]] = data.frame(
                Sex       = flagged$Sex[i],
                Dimension = flagged$Dimension[i],
                trait     = trait,
                Ind       = ind,
                rep       = as.integer(names(rl_vals)[j]),
                rl_val    = rl_vals[j],
                group_mean = gm,
                deviation  = deviations[j],
                is_extreme = deviations[j] == max(deviations),
                stringsAsFactors = FALSE
            )
        }
    }

    out = do.call(rbind, rows)
    out = out[order(out$Sex, out$Dimension, out$trait, out$Ind), ]
    out = out[order(!out$is_extreme), ]
    rownames(out) = NULL
    print(out)
    out
}

# diagnose_me_flagged: for each flagged individual x trait from run_me_dixon,
# computes the |R-L| values and consecutive pair differences with replicate
# pair labels, and identifies which pair produced the extreme value.
# Arguments:
#   dixon_out  -- dataframe returned by run_me_dixon (full output, all rows)
#   raw_groups -- named list of long-format group dataframes, keyed by group key
#                 (e.g. list("female-breadth" = df, ...))
# Returns: dataframe with one row per consecutive pair per flagged individual x trait,
#          including a column flagging which pair is the likely offending replicate
diagnose_me_flagged = function(dixon_out, raw_groups) {
    flagged = dixon_out[dixon_out$sig_mean == "*" | dixon_out$sig_zero == "*", ]
    if (nrow(flagged) == 0) {
        cat("No flagged cases to diagnose.\n")
        return(data.frame())
    }

    rows = list()

    for (i in seq_len(nrow(flagged))) {
        grp   = paste(flagged$Sex[i], flagged$Dimension[i], sep = "-")
        trait = flagged$trait[i]
        ind   = flagged$Ind[i]

        df = raw_groups[[grp]]
        if (is.null(df)) {
            cat("Warning: no raw data found for group", grp, "\n")
            next
        }

        sub = df[df$Ind == ind & df$Trait == trait, c("Rep", "Side", "Value")]
        sub = sub[!is.na(sub$Value), ]
        sub$Rep = as.integer(as.character(sub$Rep))
        reps = sort(unique(sub$Rep))

        abs_rl = vapply(reps, function(r) {
            rv = sub$Value[sub$Rep == r & sub$Side == "R"]
            lv = sub$Value[sub$Rep == r & sub$Side == "L"]
            if (length(rv) == 1 && length(lv) == 1) abs(rv - lv) else NA_real_
        }, numeric(1))
        names(abs_rl) = reps
        abs_rl = abs_rl[!is.na(abs_rl)]

        if (length(abs_rl) < 2) next

        cd          = consec_diffs(abs_rl)
        rep_keys    = names(abs_rl)
        pair_labels = paste0("M", rep_keys[-1], "-M", rep_keys[-length(rep_keys)])

        for (j in seq_along(cd)) {
            rows[[length(rows) + 1]] = data.frame(
                Sex       = flagged$Sex[i],
                Dimension = flagged$Dimension[i],
                trait     = trait,
                Ind       = ind,
                pair      = pair_labels[j],
                abs_rl_a  = abs_rl[j],
                abs_rl_b  = abs_rl[j + 1],
                consec_diff = cd[j],
                is_extreme  = abs(cd[j] - flagged$group_mean[i]) ==
                    max(abs(cd - flagged$group_mean[i])),
                stringsAsFactors = FALSE
            )
        }
    }

    out = do.call(rbind, rows)
    out = out[order(out$Sex, out$Dimension, out$trait, out$Ind), ]
    out = out[order(!out$is_extreme), ]
    rownames(out) = NULL
    print(out)
    out
}

######################## PS WORKSHEET HELPERS

# bonferroni_sorted: sequential Bonferroni correction (Rice 1989).
# Step-down procedure: sort p values ascending, threshold at rank i = alpha/(n-i+1).
# Stop at first non-significant result; all subsequent rows also non-significant.
# Data must be sorted by p-value ascending before calling.
bonferroni_sorted = function(p_vals, alpha = 0.05) {
    n    = length(p_vals)
    rank = seq_len(n)
    bonf = alpha / (n - rank + 1)
    sig  = logical(n)
    for (i in seq_len(n)) {
        if (!is.na(p_vals[i]) && p_vals[i] < bonf[i]) {
            sig[i] = TRUE
        } else {
            break
        }
    }
    data.frame(rank  = rank,
               bonf  = bonf,
               alpha = ifelse(sig, "*", "n.s."),
               stringsAsFactors = FALSE)
}

# derive_M: derives M (number of replicates) from ANOVA df values.
# Excel equivalent: =(df_me / ((df_sides + 1) * (df_ind + 1))) + 1
# fa10_m is always derived from df - never hardcoded.
derive_M = function(df_me, df_sides, df_ind) {
    (df_me / ((df_sides + 1) * (df_ind + 1))) + 1
}

# skew_se: exact standard error of skewness.
# Excel: =SQRT((6*n*(n-1)) / ((n-2)*(n+1)*(n+3)))
# Warning: NOT sqrt(6/n) - use this exact formula.
skew_se = function(n) sqrt((6 * n * (n - 1)) / ((n - 2) * (n + 1) * (n + 3)))

# kurt_se: exact standard error of kurtosis.
# Excel: =SQRT((24*n*(n-1)^2) / ((n-3)*(n-2)*(n+3)*(n+5)))
# Warning: NOT sqrt(24/n) - use this exact formula.
kurt_se = function(n) {
    sqrt((24 * n * (n - 1)^2) / ((n - 3) * (n - 2) * (n + 3) * (n + 5)))
}

# moment_p: p-value for skew/kurt tests using df=10000 approximation.
# Worksheet approximation for infinity degrees of freedom.
moment_p = function(ts) pt(-abs(ts), df = 10000) * 2

# lev_to_df: converts leveneTest output to a labelled dataframe for export.
# Arguments:
#   lev  -- output of leveneTest()
#   term -- character label for the tested term (e.g. "Sex * Dimension * Trait")
# Returns: dataframe with term and row columns added
lev_to_df = function(lev, term) {
    df       = as.data.frame(lev)
    df$term  = term
    df$row   = rownames(df)
    df
}

######################## INSPECTION HELPERS

# build_rl: returns merged left/right replicate-averaged dataframe (long).
# Used by Steps 3 and 4 for L vs R and cross-trait scatterplot generation.
# Arguments:
#   df         -- long-format dataframe with Ind, Side, Rep, Trait, Value columns
#   trait_cols -- character vector of trait names
# Returns: long dataframe with val_left and val_right per Ind-var pair
build_rl = function(df, trait_cols) {
    df_avg   = avg_reps(df, trait_cols)
    left_df  = df_avg[df_avg$Side == "L", ]
    right_df = df_avg[df_avg$Side == "R", ]
    long_l   = pivot_longer(left_df,  cols = all_of(trait_cols),
                            names_to = "var", values_to = "val_left")
    long_r   = pivot_longer(right_df, cols = all_of(trait_cols),
                            names_to = "var", values_to = "val_right")
    rl       = merge(long_l, long_r, by = c("Ind", "var"))
    rl$var   = as.factor(rl$var)
    return(rl)
}

# load_group: reads the long-format CSV and filters to one analytical group.
# Returns the filtered dataframe with factor columns coerced, and the
# trait vector for that group intersected with traits actually present.
# Arguments:
#   f_data     -- path to the long-format CSV
#   grp        -- group key string (e.g. "female-breadth")
#   trait_cols -- character vector of expected trait names for this group
#                 (from group_trait_vars in constants)
# Returns: list with df (filtered dataframe) and trait_cols (intersected)
load_group = function(f_data, grp, trait_cols) {
    parts   = strsplit(grp, "-")[[1]]
    sex_val = parts[1]; dim_val = parts[2]

    df = read_csv(f_data, show_col_types = FALSE) |>
        filter(Sex == sex_val, Dimension == dim_val)

    df$Ind   = as.factor(df$Ind)
    df$Side  = as.factor(df$Side)
    df$Rep   = as.factor(df$Rep)
    df$Trait = as.factor(df$Trait)

    trait_cols = intersect(trait_cols, unique(as.character(df$Trait)))
    # Exclude traits where all values are NA (eliminated in prior steps)
    trait_cols = trait_cols[sapply(trait_cols, function(t) {
        any(!is.na(df$Value[df$Trait == t]))
    })]
    list(df = df, trait_cols = trait_cols)
}

# run_univariates: runs Sides x Individuals ANOVA on post-FA data (Step 6).
# Accepts a pre-filtered long-format dataframe for one group.
# Pivots wide per trait internally before running ANOVA.
# Arguments:
#   df         -- long-format dataframe for one group (filtered from CSV)
#   trait_cols -- character vector of trait names for this group
#   grp        -- group key string (e.g. "female-breadth-mn")
#   label      -- human-readable group label for console output
# Returns: dataframe with Sex, Dimension, trait columns and ANOVA MS/df values
run_univariates = function(df, trait_cols, grp, label) {
    cat("\n\n---", label, "---\n")
    parts   = strsplit(grp, "-")[[1]]
    sex_val = parts[1]; dim_val = parts[2]

    df_wide = df |>
        filter(Trait %in% trait_cols) |>
        select(Ind, Side, Rep, Trait, Value) |>
        pivot_wider(names_from = Trait, values_from = Value)

    df_wide$Ind  = as.factor(df_wide$Ind)
    df_wide$Side = as.factor(df_wide$Side)
    df_wide$Rep  = as.factor(df_wide$Rep)

    trait_cols = intersect(trait_cols, names(df_wide))

    cat("\nAOV Univariates:\n")
    aov_rows = list()
    for (v in trait_cols) {
        cat("\n", toupper(v), "\n")
        fit = suppressWarnings(aov(
            as.formula(paste(v, "~ Side + Side*Ind + Error(Ind/(Side*Ind))")),
            data = df_wide
        ))
        s = summary(fit)
        print(s)

        s_ind  = s[["Error: Ind"]][[1]]
        s_side = s[["Error: Ind:Side"]][[1]]
        s_with = s[["Error: Within"]][[1]]
        rownames(s_side) = trimws(rownames(s_side))

        ms_I  = s_ind["Ind",        "Mean Sq"]
        df_I  = s_ind["Ind",        "Df"]
        ms_S  = s_side["Side",      "Mean Sq"]
        df_S  = s_side["Side",      "Df"]
        ms_SI = s_side["Side:Ind",  "Mean Sq"]
        df_SI = s_side["Side:Ind",  "Df"]
        ms_ME = s_with["Residuals", "Mean Sq"]
        df_ME = s_with["Residuals", "Df"]

        aov_rows[[v]] = data.frame(
            Sex       = sex_val,
            Dimension = dim_val,
            trait = v,
            MS_S  = ms_S,  df_S  = df_S,
            MS_I  = ms_I,  df_I  = df_I,
            MS_SI = ms_SI, df_SI = df_SI,
            MS_ME = ms_ME, df_ME = df_ME,
            stringsAsFactors = FALSE
        )
    }
    aov_df = do.call(rbind, aov_rows)
    rownames(aov_df) = NULL
    aov_df
}

# run_descriptives: computes skew, kurtosis, and side diff/avg descriptives
# on post-Step8 data (Step 9).
# Arguments:
#   df         -- long-format dataframe for one group (filtered from CSV)
#   trait_cols -- character vector of trait names for this group
#   grp        -- group key string (e.g. "female-breadth")
#   label      -- human-readable group label for console output
# Returns: dataframe with Sex, Dimension, trait columns and descriptive stats
run_descriptives = function(df, trait_cols, grp, label) {
    cat("\n\n---", label, "---\n")
    parts   = strsplit(grp, "-")[[1]]
    sex_val = parts[1]; dim_val = parts[2]

    df_avg   = avg_reps(df, trait_cols)
    left_df  = df_avg[df_avg$Side == "L", ] |> dplyr::select(-Side)
    right_df = df_avg[df_avg$Side == "R", ] |> dplyr::select(-Side)

    long_l = pivot_longer(left_df,  cols = all_of(trait_cols),
                          names_to = "var", values_to = "left")
    long_r = pivot_longer(right_df, cols = all_of(trait_cols),
                          names_to = "var", values_to = "right")

    desc_df       = merge(long_l, long_r, by = c("Ind", "var"))
    desc_df       = na.omit(desc_df)

    diff_df       = desc_df
    diff_df$diff  = diff_df$right - diff_df$left
    diff_df$left  = NULL
    diff_df$right = NULL
    diff_wide     = pivot_wider(diff_df, names_from = var, values_from = diff)
    names(diff_wide) = ifelse(names(diff_wide) == "Ind", "Ind",
                              paste0(names(diff_wide), "_sd"))

    av_df         = desc_df
    av_df$average = (av_df$right + av_df$left) / 2
    av_df$left    = NULL
    av_df$right   = NULL
    av_wide       = pivot_wider(av_df, names_from = var, values_from = average)

    cat("\nSkew and Kurtosis on R-L differences:\n")
    sd_cols   = grep("_sd$", names(diff_wide), value = TRUE)
    skew_rows = list()
    for (v in sd_cols) {
        trait = sub("_sd$", "", v)
        vals  = diff_wide[[v]]; vals = vals[!is.na(vals)]
        cat("\n", toupper(trait), "\n")
        sk = Skew(vals, na.rm = TRUE)
        ku = Kurt(vals, na.rm = TRUE, method = 1)
        cat("  Skew:    ", sk, "\n")
        cat("  Kurtosis:", ku, "\n")
        skew_rows[[trait]] = data.frame(trait = trait,
                                        skew  = sk,
                                        kurt  = ku,
                                        stringsAsFactors = FALSE)
    }
    skew_df = do.call(rbind, skew_rows)
    rownames(skew_df) = NULL

    cat("\nSide Difference Descriptives (R-L):\n")
    desc_rows = list()
    for (v in sd_cols) {
        trait   = sub("_sd$", "", v)
        vals_sd = diff_wide[[v]];   vals_sd = vals_sd[!is.na(vals_sd)]
        vals_av = av_wide[[trait]]; vals_av = vals_av[!is.na(vals_av)]
        cat("\n", toupper(trait), "\n")
        print(describe(vals_sd, type = 2))
        desc_rows[[trait]] = data.frame(
            trait        = trait,
            n            = length(vals_sd),
            mean_diff    = mean(vals_sd),
            mean_diff_se = sd(vals_sd) / sqrt(length(vals_sd)),
            mean_av      = mean(vals_av),
            mean_av_se   = sd(vals_av) / sqrt(length(vals_av)),
            stringsAsFactors = FALSE
        )
    }

    cat("\nSide Average Descriptives ((R+L)/2):\n")
    for (v in setdiff(names(av_wide), "Ind")) {
        cat("\n", toupper(v), "\n")
        print(describe(av_wide[[v]], type = 2))
    }

    desc_out = do.call(rbind, desc_rows)
    rownames(desc_out) = NULL

    result = merge(skew_df, desc_out, by = "trait", all = TRUE)
    result$Sex       = sex_val
    result$Dimension = dim_val
    result
}

# run_mixedmodel: computes FA > ME ANOVA block with Bonferroni correction (Step 6).
# Bonferroni correction applied within each group (sex x dimension).
# Arguments:
#   grp        -- group key string (key into aov_inputs)
#   aov_inputs -- named list of dataframes from run_univariates
# Returns: dataframe sorted by fa_me_p with Bonferroni columns
run_mixedmodel = function(grp, aov_inputs) {
    ws = aov_inputs[[grp]]
    n  = nrow(ws)

    cat(sprintf("\n=== %s  (n traits = %d) ===\n", grp, n))

    sides_f = numeric(n); sides_p = numeric(n)
    ind_f   = numeric(n); ind_p   = numeric(n)
    fa_me_f = numeric(n); fa_me_p = numeric(n)
    me_pct     = numeric(n)
    fa10_m     = numeric(n)
    fa10_value = numeric(n)
    fa10_df    = numeric(n)

    for (i in seq_len(n)) {
        ms_s  = ws$MS_S[i];  df_s  = ws$df_S[i]
        ms_i  = ws$MS_I[i];  df_i  = ws$df_I[i]
        ms_si = ws$MS_SI[i]; df_si = ws$df_SI[i]
        ms_me = ws$MS_ME[i]; df_me = ws$df_ME[i]

        sides_f[i] = ms_s  / ms_si
        sides_p[i] = pf(sides_f[i], df_s,  df_si, lower.tail = FALSE)

        ind_f[i]   = ms_i  / ms_si
        ind_p[i]   = pf(ind_f[i],   df_i,  df_si, lower.tail = FALSE)

        fa_me_f[i] = ms_si / ms_me
        fa_me_p[i] = pf(fa_me_f[i], df_si, df_me, lower.tail = FALSE)

        me_pct[i]     = 100 * ms_me / ms_si
        fa10_m[i]     = derive_M(df_me, df_s, df_i)
        fa10_value[i] = max(ms_si - ms_me, 0) / fa10_m[i]

        if (ms_si > ms_me) {
            num        = (ms_si - ms_me)^2
            denom      = (ms_si^2 / df_si) + (ms_me^2 / df_me)
            fa10_df[i] = num / denom
        } else {
            fa10_df[i] = NA_real_
        }
    }

    anova_base = data.frame(
        Sex       = ws$Sex,
        Dimension = ws$Dimension,
        trait     = ws$trait,
        sides_ms  = ws$MS_S,   sides_df = ws$df_S,
        ind_ms    = ws$MS_I,   ind_df   = ws$df_I,
        sxi_ms    = ws$MS_SI,  sxi_df   = ws$df_SI,
        me_ms     = ws$MS_ME,  me_df    = ws$df_ME,
        sides_f   = sides_f,   sides_p  = sides_p,
        ind_f     = ind_f,     ind_p    = ind_p,
        fa_me_f   = fa_me_f,   fa_me_p  = fa_me_p,
        stringsAsFactors = FALSE
    )
    anova_sorted = anova_base[order(anova_base$fa_me_p), ]
    bonf_anova   = bonferroni_sorted(anova_sorted$fa_me_p)
    anova_out    = cbind(anova_sorted,
                         anova_bonf  = bonf_anova$bonf,
                         anova_rank  = bonf_anova$rank,
                         anova_alpha = bonf_anova$alpha,
                         pct_me      = me_pct[order(anova_base$fa_me_p)],
                         n_reps      = fa10_m[order(anova_base$fa_me_p)],
                         fa10_noME   = fa10_value[order(anova_base$fa_me_p)],
                         fa10_df     = fa10_df[order(anova_base$fa_me_p)])

    cat("\n  ANOVA sort (by fa_me_p):\n")
    print(anova_out[, c("Sex", "Dimension", "trait",
                        "fa_me_f", "fa_me_p", "anova_bonf",
                        "anova_rank", "anova_alpha", "fa10_noME", "fa10_df", "pct_me")])
    anova_out
}
# run_idealfa: computes DA, skew, and kurtosis blocks with Bonferroni correction
# (Step 9). Input is from run_descriptives on post-Step8 data.
# Bonferroni correction applied within each group (sex x dimension).
# Arguments:
#   grp         -- group key string (key into desc_inputs)
#   desc_inputs -- named list of dataframes from run_descriptives
# Returns: list with da, skew, kurt blocks (each independently Bonferroni-sorted)
run_idealfa = function(grp, desc_inputs) {
    ws = desc_inputs[[grp]]
    n  = nrow(ws)

    cat(sprintf("\n=== %s  (n traits = %d) ===\n", grp, n))

    da_ts  = ws$mean_diff / ws$mean_diff_se
    da_p   = pt(-abs(da_ts), df = ws$n - 1) * 2

    se_skew = skew_se(ws$n)
    skew_ts = ws$skew / se_skew
    skew_p  = moment_p(skew_ts)

    se_kurt = kurt_se(ws$n)
    kurt_ts = ws$kurt / se_kurt
    kurt_p  = moment_p(kurt_ts)

    ######################## DA - sorted by da_p
    da_base = data.frame(
        Sex       = ws$Sex,
        Dimension = ws$Dimension,
        trait        = ws$trait,
        mean_av      = ws$mean_av,   mean_av_se   = ws$mean_av_se,
        mean_diff    = ws$mean_diff, mean_diff_se = ws$mean_diff_se,
        n            = ws$n,
        skew         = ws$skew,
        kurt         = ws$kurt,
        blank_1      = NA,
        da_ts        = da_ts,
        da_p         = da_p,
        stringsAsFactors = FALSE
    )
    da_sorted = da_base[order(da_base$da_p), ]
    bonf_da   = bonferroni_sorted(da_sorted$da_p)
    da_out    = cbind(da_sorted,
                      da_bonf  = bonf_da$bonf,
                      da_rank  = bonf_da$rank,
                      da_alpha = bonf_da$alpha)

    ######################## SKEW - sorted by skew_p
    skew_base = data.frame(
        Sex       = ws$Sex,
        Dimension = ws$Dimension,
        trait         = ws$trait,
        skew_obs      = ws$skew,
        skew_n        = ws$n,
        skew_expected = 0,
        skew_se       = se_skew,
        skew_ts       = skew_ts,
        skew_df       = 10000,
        skew_p        = skew_p,
        stringsAsFactors = FALSE
    )
    skew_sorted = skew_base[order(skew_base$skew_p), ]
    bonf_skew   = bonferroni_sorted(skew_sorted$skew_p)
    skew_out    = cbind(skew_sorted,
                        skew_bonf  = bonf_skew$bonf,
                        skew_rank  = bonf_skew$rank,
                        skew_alpha = bonf_skew$alpha)

    ######################## KURT - sorted by kurt_p
    kurt_base = data.frame(
        Sex       = ws$Sex,
        Dimension = ws$Dimension,
        trait         = ws$trait,
        kurt_obs      = ws$kurt,
        kurt_n        = ws$n,
        kurt_expected = 0,
        kurt_se       = se_kurt,
        kurt_ts       = kurt_ts,
        kurt_df       = 10000,
        kurt_p        = kurt_p,
        stringsAsFactors = FALSE
    )
    kurt_sorted = kurt_base[order(kurt_base$kurt_p), ]
    bonf_kurt   = bonferroni_sorted(kurt_sorted$kurt_p)
    kurt_out    = cbind(kurt_sorted,
                        kurt_bonf  = bonf_kurt$bonf,
                        kurt_rank  = bonf_kurt$rank,
                        kurt_alpha = bonf_kurt$alpha)

    cat("\n  DA sort (by da_p):\n")
    print(da_out[, c("Sex", "Dimension", "trait",
                     "da_ts", "da_p", "da_bonf", "da_rank", "da_alpha")])

    list(da = da_out, skew = skew_out, kurt = kurt_out)
}




# assess_da_vs_fa4a: Step 9 DA assessment using Palmer & Strobeck rule of thumb.
# For each trait in a group, compares |mean(R-L)| (DA) to FA4a (mean |R-L|).
# If DA > FA4a, the directional component dominates and the trait is flagged.
# Traits in step9_highDA (significant DA) are assessed; others pass automatically.
# Arguments:
#   desc_inputs   -- list of descriptives output from run_descriptives(), keyed by grp
#   highDA_list   -- step9_highDA constant: list of sex/dimension/trait entries
# Returns: dataframe with Sex, Dimension, trait, da_mean, fa4a, exceeds (logical),
#          flag ("exclude", "conservative", or "retain")
assess_da_vs_fa4a = function(f_data, highDA_list) {
    dat  = read_csv(f_data, show_col_types = FALSE)
    rows = list()

    for (entry in highDA_list) {
        for (tr in entry$trait) {
            sub = dat |>
                filter(Sex == entry$sex, Dimension == entry$dimension,
                       Trait == tr, !is.na(Value))

            # average replicates per individual per side
            avg = sub |>
                group_by(Ind, Side) |>
                summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop")

            # compute R-L per individual
            left_df  = avg |> filter(Side == "L") |> rename(val_l = Value) |> select(Ind, val_l)
            right_df = avg |> filter(Side == "R") |> rename(val_r = Value) |> select(Ind, val_r)
            merged   = inner_join(left_df, right_df, by = "Ind") |>
                mutate(diff = val_r - val_l)

            if (nrow(merged) == 0) next

            da   = abs(mean(merged$diff))
            fa4a = mean(abs(merged$diff))

            rows[[paste(entry$sex, entry$dimension, tr, sep = "-")]] = data.frame(
                Sex       = entry$sex,
                Dimension = entry$dimension,
                trait     = tr,
                da        = da,
                fa4a      = fa4a,
                exceeds   = da > fa4a,
                stringsAsFactors = FALSE
            )
        }
    }

    out = do.call(rbind, rows)
    out = out[order(out$Sex, out$Dimension, out$trait), ]
    rownames(out) = NULL
    cat("\nDA vs FA4a Assessment:\n")
    print(out)
    out
}

# run_step8a_scatters: Step 8A -- scatter plots of |R-L| vs (R+L)/2 per trait.
# Slow step. Run once; skip on Spearman reruns.
# Saves interactive scatterplots to figures-inspection/.
# Arguments:
#   df         -- long-format dataframe for one group (filtered from CSV)
#   trait_cols -- character vector of trait names for this group
#   grp        -- group key string (e.g. "female-breadth")
#   label      -- human-readable group label for plot titles
run_step8a_scatters = function(df, trait_cols, grp, label) {
    cat("\n---", label, "---\n")

    df_avg   = avg_reps(df, trait_cols)
    left_df  = df_avg[df_avg$Side == "L", ] |> dplyr::select(-Side)
    right_df = df_avg[df_avg$Side == "R", ] |> dplyr::select(-Side)

    long_l = pivot_longer(left_df,  cols = all_of(trait_cols),
                          names_to = "var", values_to = "val_left")
    long_r = pivot_longer(right_df, cols = all_of(trait_cols),
                          names_to = "var", values_to = "val_right")

    rl           = merge(long_l, long_r, by = c("Ind", "var"))
    rl           = na.omit(rl)
    rl$abs_diff  = abs(rl$val_right - rl$val_left)
    rl$trait_avg = (rl$val_right + rl$val_left) / 2

    for (v in trait_cols) {
        sub = rl[rl$var == v, ]
        if (nrow(sub) < 3) next
        p = ggplot(sub, aes(trait_avg, abs_diff, text = Ind)) +
            geom_point() +
            theme_classic() +
            labs(x = "(R+L)/2", y = "|R-L|",
                 title = paste("Step 8: Size dependency --", label, "--", v))
        fname = file.path("figures-inspection",
                          paste0("step8-", grp, "-", v, ".html"))
        saveWidget(ggplotly(p, tooltip = "text"), file = fname, selfcontained = TRUE)
    }
    invisible(NULL)
}

# run_step8b_spearman: Step 8B -- Spearman correlation of |R-L| vs (R+L)/2 per trait.
# Fast; re-runnable independently of scatter plots.
# Bonferroni correction applied within each group (sex x dimension).
# Arguments:
#   df         -- long-format dataframe for one group (filtered from CSV)
#   trait_cols -- character vector of trait names for this group
#   grp        -- group key string (e.g. "female-breadth")
#   label      -- human-readable group label for output
# Returns: dataframe with Sex, Dimension, Spearman z, p, Bonferroni correction
run_step8b_spearman = function(df, trait_cols, grp, label) {
    cat("\n---", label, "---\n")
    parts   = strsplit(grp, "-")[[1]]
    sex_val = parts[1]; dim_val = parts[2]

    df_avg   = avg_reps(df, trait_cols)
    left_df  = df_avg[df_avg$Side == "L", ] |> dplyr::select(-Side)
    right_df = df_avg[df_avg$Side == "R", ] |> dplyr::select(-Side)

    long_l = pivot_longer(left_df,  cols = all_of(trait_cols),
                          names_to = "var", values_to = "val_left")
    long_r = pivot_longer(right_df, cols = all_of(trait_cols),
                          names_to = "var", values_to = "val_right")

    rl           = merge(long_l, long_r, by = c("Ind", "var"))
    rl           = na.omit(rl)
    rl$abs_diff  = abs(rl$val_right - rl$val_left)
    rl$trait_avg = (rl$val_right + rl$val_left) / 2

    rows = list()
    for (v in trait_cols) {
        sub = rl[rl$var == v, ]
        if (nrow(sub) < 3) next
        tst   = spearman_test(abs_diff ~ trait_avg, data = sub)
        zstat = as.numeric(statistic(tst))
        pval  = pvalue(tst)
        rows[[v]] = data.frame(
            Sex       = sex_val,
            Dimension = dim_val,
            trait = v,
            n     = nrow(sub),
            z     = zstat,
            p     = pval,
            stringsAsFactors = FALSE
        )
    }

    out       = do.call(rbind, rows)
    out       = out[order(out$p), ]
    bonf      = bonferroni_sorted(out$p)
    out$bonf  = bonf$bonf
    out$rank  = bonf$rank
    out$alpha = bonf$alpha
    cat("\n")
    print(out)
    out
}

######################## NORMALITY SCREENING
run_normality_scatters = function(f_data, prefix, grp_map = group_labels) {
    for (grp in names(grp_map)) {
        g    = load_group(f_data, grp, group_trait_vars[[grp]])
        df   = g$df
        vars = g$trait_cols

        if (length(vars) == 0) {
            cat("Skipping", grp, "-- no traits retained\n")
            next
        }

        df_wide = df |>
            filter(Trait %in% vars) |>
            select(Ind, Rep, Side, Trait, Value) |>
            pivot_wider(names_from = Trait, values_from = Value) |>
            select(all_of(vars))

        # Skip if all values are NA (group fully eliminated in prior steps)
        if (all(is.na(df_wide))) {
            cat("Skipping", grp, "-- all values NA\n")
            next
        }

        # Drop any trait columns that are entirely NA
        vars = vars[sapply(vars, function(v) any(!is.na(df_wide[[v]])))]
        if (length(vars) == 0) {
            cat("Skipping", grp, "-- no non-NA traits\n")
            next
        }
        df_wide = df_wide[, vars, drop = FALSE]

        fname = file.path("figures-inspection",
                          paste0(prefix, "-", grp, ".tiff"))
        tiff(fname, units = "in", width = 20, height = 20,
             compression = "lzw", res = 300)
        if (length(vars) >= 3) {
            scatterplotMatrix(
                df_wide[, vars, drop = FALSE],
                cex        = 0.5,
                diagonal   = list(method = "qqplot"),
                var.labels = vars,
                main       = paste("Normality --", grp_map[[grp]])
            )
        } else {
            par(mfrow = c(1, length(vars)))
            for (v in vars) {
                qqnorm(df_wide[[v]], main = paste("QQ --", grp_map[[grp]], "--", v))
                qqline(df_wide[[v]], col = "red")
            }
            par(mfrow = c(1, 1))
        }
        dev.off()
        cat("Written:", fname, "\n")
    }
}