#   Convert raw data files into the pipeline-ready format
#   Source: sheets 1-10 in each file, one sheet per replicate trial.
#   Output: single long CSV with factor columns for Sex, Dimension, Arcade,
#           Class, and Position. One row per Ind x Rep x Side x trait group.
#
# DIMENSION KEY:
#   'a' = BL breadth
#   'b' = MD length
#   'c' and 'd' = diagonal measurements, not used in this pipeline
#    Male 2 contains an extra column with a note ("a=BL, b=MD..."), ignored here.
#
# TRAIT NAMING:
#    arcade prefix (mx, mn), class letter (p, m), and position (1,2,3)

############## LIBRARIES

library(readxl)
library(dplyr)
library(tidyr)
library(readr)



############## PATHS

path_females = file.path("data-raw", "Females_EG data.xlsx")
path_males   = file.path("data-raw", "Males_EG data.xlsx")
path_output  = "data-revised/data-revised-wrangled.csv"



############## HELPER: read_rep_sheets()
#   Reads sheets 1-10 from one of the student's files.
#   Each sheet: row 0 = specimen IDs, col 0 = trait variable names,
#               remaining cells = measurements.
#   Returns a data frame with columns: Ind, Rep, and one column per
#   trait variable (78 total: RMaxM3a ... LManI1).
# -----------------------------------------------------------------------------
read_rep_sheets = function(path, id_pattern) {
    all_reps = list()

    for (rep in 1:10) {
        sheet = read_excel(path, sheet = as.character(rep),
                           col_names = TRUE, .name_repair = "minimal")

        # col 1 is the trait variable name; remaining cols are specimens.
        # Filter to only specimen columns matching the ID pattern (e.g. F1, M12)
        # to drop any stray annotation columns the student left in.
        spec_cols = grepl(id_pattern, names(sheet))
        spec_cols[1] = FALSE  # first col is the row-label column, not data

        trait_col = sheet[[1]]          # row labels: RMaxM3a, RMaxM3b, ...
        data_cols = sheet[, spec_cols]  # one col per specimen

        # Transpose: specimens become rows, trait vars become columns
        df = as.data.frame(t(data_cols), stringsAsFactors = FALSE)
        colnames(df) = trait_col
        df$Ind = names(data_cols)
        df$Rep = rep

        # Coerce all trait columns to numeric (read_excel may return character
        # if there are mixed types in a column)
        trait_vars = setdiff(names(df), c("Ind", "Rep"))
        df[trait_vars] = lapply(df[trait_vars], function(x) suppressWarnings(as.numeric(x)))

        all_reps[[rep]] = df
    }

    bind_rows(all_reps)
}

############## HELPER: make_long()
#   Selects the relevant trait columns for one sex x dimension combination,
#   reshapes to one row per Ind x Rep x Side, then adds factor columns:
#   Sex, Dimension, Arcade, Class, Position.
#
#   trait_vars  : named character vector
#                 names  = pipeline trait names (e.g. mnm1, mxp4)
#                 values = student column base  (e.g. ManM1, MaxP4)
#                 dimension suffix ('a' or 'b') is appended per side
#   dim_suffix  : 'b' for length, 'a' for breadth
#   sex_label   : "female" or "male"
#   dim_label   : "breadth" or "length"
# -----------------------------------------------------------------------------
make_long = function(df, trait_vars, dim_suffix, sex_label, dim_label) {
    rows = list()

    for (ind in unique(df$Ind)) {
        ind_data = df |> filter(Ind == ind)

        for (rep in sort(unique(ind_data$Rep))) {
            rep_data = ind_data |> filter(Rep == rep)

            for (side in c("R", "L")) {
                row = list(Ind = ind, Rep = rep, Side = side)

                for (pipe_name in names(trait_vars)) {
                    base = trait_vars[[pipe_name]]        # e.g. "MaxM3"
                    col  = paste0(side, base, dim_suffix) # e.g. "RMaxM3b"
                    row[[pipe_name]] = if (col %in% names(rep_data)) rep_data[[col]][1] else NA_real_
                }

                rows[[length(rows) + 1]] = as.data.frame(row, stringsAsFactors = FALSE)
            }
        }
    }

    result = bind_rows(rows)

    # Add factor columns
    result$Sex       = sex_label
    result$Dimension = dim_label

    # Derive Arcade, Class, Position from trait names and add as long-format
    # columns. These are added per-trait in the pivot below.
    result
}

############## HELPER: add_trait_factors()
#   Pivots the wide per-trait columns to long format and derives
#   Arcade, Class, and Position from the trait name.
#   Arcade:   "mn" (mandibular) or "mx" (maxillary) -- first two chars
#   Class:    "molar" or "premolar"                 -- third char (m/p)
#   Position: integer 1-4                           -- fourth char
# -----------------------------------------------------------------------------
add_trait_factors = function(df, trait_names) {
    long = pivot_longer(df,
                        cols      = all_of(trait_names),
                        names_to  = "Trait",
                        values_to = "Value")

    long$Arcade   = substr(long$Trait, 1, 2)   # "mn" or "mx"
    long$Class    = ifelse(substr(long$Trait, 3, 3) == "m", "molar", "premolar")
    long$Position = as.integer(substr(long$Trait, 4, 4))

    # Reorder columns: identifiers first, then factors, then value
    long |> select(Ind, Rep, Side, Sex, Dimension, Arcade, Class, Position,
                   Trait, Value)
}

############## HELPER: enforce_min_reps()
#   For each Ind x Trait combination, finds replicates where BOTH R and L
#   are non-NA (paired reps). If fewer than 2 paired reps exist, sets all
#   values for that Ind x Trait to NA on both sides.
#   Minimum of 2 paired replicates is required for the sides x individuals
#   ANOVA (Palmer & Strobeck).
# -----------------------------------------------------------------------------
enforce_min_reps = function(df) {
    # For each Ind x Trait, count paired reps (both R and L non-NA)
    paired_counts = df |>
        group_by(Ind, Trait, Rep) |>
        summarise(
            has_R = any(Side == "R" & !is.na(Value)),
            has_L = any(Side == "L" & !is.na(Value)),
            .groups = "drop"
        ) |>
        mutate(paired = has_R & has_L) |>
        group_by(Ind, Trait) |>
        summarise(n_paired = sum(paired), .groups = "drop")

    # Identify Ind x Trait combinations with fewer than 2 paired reps
    insufficient = paired_counts |>
        filter(n_paired < 2) |>
        select(Ind, Trait)

    if (nrow(insufficient) > 0) {
        cat(sprintf("  enforce_min_reps: setting %d Ind x Trait combinations to NA\n",
                    nrow(insufficient)))
        # Set Value to NA for all rows matching insufficient Ind x Trait pairs
        df = df |>
            left_join(insufficient |> mutate(drop = TRUE),
                      by = c("Ind", "Trait")) |>
            mutate(Value = ifelse(!is.na(drop), NA_real_, Value)) |>
            select(-drop)
    }

    df
}

############## TRAIT VARIABLE DEFINITIONS
#   Named character vectors: names = pipeline trait names, values = student
#   column base names. Dimension suffix ('a'/'b') is appended by make_long().

len_vars = c(
    mnm1 = "ManM1",
    mnm2 = "ManM2",
    mnm3 = "ManM3",
    mnp4 = "ManP4",
    mxm1 = "MaxM1",
    mxm2 = "MaxM2",
    mxm3 = "MaxM3",
    mxp4 = "MaxP4"
)

bre_vars = c(
    mnm1 = "ManM1",
    mnm2 = "ManM2",
    mnm3 = "ManM3",
    mnp4 = "ManP4",
    mxm1 = "MaxM1",
    mxm2 = "MaxM2",
    mxm3 = "MaxM3",
    mxp4 = "MaxP4"
)

############## READ SOURCE DATA
cat("Reading raw data...\n")
females_raw = read_rep_sheets(path_females, id_pattern = "^F[0-9]+$")
cat(sprintf("  females: %d rows, %d individuals\n",
            nrow(females_raw), length(unique(females_raw$Ind))))

males_raw = read_rep_sheets(path_males, id_pattern = "^M[0-9]+$")
cat(sprintf("  males:   %d rows, %d individuals\n",
            nrow(males_raw), length(unique(males_raw$Ind))))

############## BUILD WIDE BLOCKS PER SEX x DIMENSION
cat("\nBuilding wide blocks...\n")
female_lengths  = make_long(females_raw, len_vars, "b", "female", "length")
female_breadths = make_long(females_raw, bre_vars, "a", "female", "breadth")
male_lengths    = make_long(males_raw,   len_vars, "b", "male",   "length")
male_breadths   = make_long(males_raw,   bre_vars, "a", "male",   "breadth")

cat(sprintf("  female-length:  %d rows\n", nrow(female_lengths)))
cat(sprintf("  female-breadth: %d rows\n", nrow(female_breadths)))
cat(sprintf("  male-length:    %d rows\n", nrow(male_lengths)))
cat(sprintf("  male-breadth:   %d rows\n", nrow(male_breadths)))

############## PIVOT TO LONG AND ADD TRAIT FACTORS
cat("\nPivoting to long format and deriving trait factors...\n")
long_fl = add_trait_factors(female_lengths,  names(len_vars))
long_fb = add_trait_factors(female_breadths, names(bre_vars))
long_ml = add_trait_factors(male_lengths,    names(len_vars))
long_mb = add_trait_factors(male_breadths,   names(bre_vars))

# Combine all groups into single long CSV
long_all = bind_rows(long_fl, long_fb, long_ml, long_mb)
cat(sprintf("  combined: %d rows\n", nrow(long_all)))

############## ENFORCE MINIMUM PAIRED REPLICATES
cat("\nEnforcing minimum 2 paired replicates per Ind x Trait...\n")
long_all = enforce_min_reps(long_all)

############## WRITE OUTPUT
cat(sprintf("\nWriting %s...\n", path_output))
write_csv(long_all, path_output)
cat("Done.\n")


##############  TIDY
rm(list = ls())
gc()