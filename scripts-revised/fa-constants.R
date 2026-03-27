######################## fa-constants.R
# Study-specific constants for the baboon stress FA analysis.
# Source this file after fa-functions.R at the start of each session:

######################## FILE PATHS
f_wrangled                   = "data-revised/data-revised-wrangled.csv"
f_checkpoint_step12          = "data-revised/data-revised-checkpoint-step1-2-dixon.csv"
f_checkpoint_step345         = "data-revised/data-revised-checkpoint-step3-4-5-dixon.csv"
f_fromStep5                  = "data-revised/data-revised-fromStep5.csv"
f_step6                      = "data-revised/data-revised-checkpoint-step6-mixedmodel.csv"
f_fromStep6                  = "data-revised/data-revised-fromStep6.csv"
f_checkpoint_step7           = "data-revised/data-revised-checkpoint-step7-leveneME.csv"
f_checkpoint_step8           = "data-revised/data-revised-checkpoint-step8-spearman.csv"
f_checkpoint_step9           = "data-revised/data-revised-checkpoint-step9-daSkewKurtosis.csv"
f_checkpoint_step9_daTest    = "data-revised/data-revised-checkpoint-step9-daTest.csv"
f_fromStep9                  = "data-revised/data-revised-fromStep9.csv"
f_hypothesis                 = "data-revised/data-revised-hypothesis.csv"


######################## TRAIT VARIABLES
# All trait variables used in the pipeline.
# mp3 excluded due to canine honing
trait_vars_all = c("mnm1", "mnm2", "mnm3", "mnp4",
                   "mxm1", "mxm2", "mxm3", "mxp4")

######################## TRAIT VARIABLE SETS PER GROUP
# Each of the 4 groups (sex x dimension) has its own trait vector combining
# both arcades. These define the Bonferroni family for Steps 2, 5, 6, 8, and 9.
# Key format: "sex-dimension"

group_trait_vars = list(
    "female-breadth" = c("mnm1", "mnm2", "mnm3", "mnp4", "mxm1", "mxm2", "mxm3", "mxp4"),
    "female-length"  = c("mnm1", "mnm2", "mnm3", "mnp4", "mxm1", "mxm2", "mxm3", "mxp4"),
    "male-breadth"   = c("mnm1", "mnm2", "mnm3", "mnp4", "mxm1", "mxm2", "mxm3", "mxp4"),
    "male-length"    = c("mnm1", "mnm2", "mnm3", "mnp4", "mxm1", "mxm2", "mxm3", "mxp4")
)

######################## GROUP LABELS
# Human-readable labels for the 4 analytical groups.
# Used in plot titles and console output.
group_labels = list(
    "female-breadth" = "Female Breadth",
    "female-length"  = "Female Length",
    "male-breadth"   = "Male Breadth",
    "male-length"    = "Male Length"
)

######################## OUTLIER REMOVALS (STEPS 1-2 and 3-5)
#### ME OUTLIERS (STEP 2)
# Removal unit: specific replicate for one individual x trait.
# Across all traits, 2 individuals were flagged (same for mean and zero).
#   female-length   mxp4  F4   rep 10
#   male-breadth    mxm3  M22  rep 8

me_dixon_removals = list(
    list(sex = "female", dimension = "length",  ind = "F4",  trait = "mxp4", rep = 10),
    list(sex = "male",   dimension = "breadth", ind = "M22", trait = "mxm3", rep = 8)
)

#### FA OUTLIER REMOVALS (STEP 5)
# Removal unit: specific replicate for one individual x trait.
# Across all traits, 6 individuals were flagged (same for mean and zero).
#   female-breadth  mxm3  F41  rep 1
#   female-breadth  mxp4  F23  rep 3
#   female-length   mxm2  F32  rep 1
#   male-breadth    mnm1  M12  rep 8
#   male-breadth    mnm1  M2   rep 6
#   male-length     mxm1  M8   rep 2

fa_dixon_removals = list(
    list(sex = "female", dimension = "breadth", ind = "F41", trait = "mxm3", rep = 1),
    list(sex = "female", dimension = "breadth", ind = "F23", trait = "mxp4", rep = 3),
    list(sex = "female", dimension = "length",  ind = "F32", trait = "mxm2", rep = 1),
    list(sex = "male",   dimension = "breadth", ind = "M12", trait = "mnm1", rep = 8),
    list(sex = "male",   dimension = "breadth", ind = "M2",  trait = "mnm1", rep = 6),
    list(sex = "male",   dimension = "length",  ind = "M8",  trait = "mxm1", rep = 2)
)


######################## FA>ME == n.s. REMOVALS (STEP 6)
# All traits had significant FA>ME in this analysis -- list is empty.
# Structure when needed:
#   list(sex = "...", dimension = "...", trait = "var_name")
# Use sex = "all" to remove a trait from every group.

step6_eliminations = list()


######################## STEP 8 ASSESSMENT OF TRAIT SIZE DEPENDENCY
# Removal unit: entire trait column for a group.
# Traits with a significant positive Spearman correlation between |R-L| and
# (R+L)/2 after sequential Bonferroni correction.
# Elimination only justified for significant AND positive associations.
# One trait had significant trait size dependency:
#   male-length  mxp4
# use ln correction rather than remove the trait.


######################## STEP 9 REMOVALS AND DA CORRECTION
# Removal unit: entire trait column for a group.
# Variables eliminated due to significant DA, skew, or kurtosis after
# sequential Bonferroni correction within each group.

# DA found most variables had high DA
# follow guide lines from Palmer and Strobeck on whether DA is excessive
# Unfortunately, sometimes even very slight DA may become significant statistically in
# studies involving large samples. Under these circumstances, too many data might be lost if these
# traits were excluded, and factoring out DA would seem desirable. The critical question here is: At
# what point is DA so small that it is unlikely to confound interpretations of FA variation? Any rule
# is arbitrary, but a potentially useful rule of thumb may help. If DA, as mean(R - L), is no larger
# than FA4a (Table 1), then the predisposition towards one side is less than the average deviation about
# mean(R - L). Therefore, since the underlying variation in DA would likely be 10 - 20% of the mean
# DA - the CV for many traits is commonly in this range (Lande, 1977) - deviations about the
# mean DA would be due largely to DI.

step9_highDA = list(
    list(sex = "female", dimension = "breadth",
         trait = c("mxp4", "mxm1", "mxm2", "mxm3", "mnm1", "mnm2", "mnm3", "mnp4")),
    list(sex = "female", dimension = "length",
         trait = c("mxp4", "mxm1", "mxm2")),
    list(sex = "male",   dimension = "breadth",
         trait = c("mxp4", "mxm1", "mxm2", "mxm3", "mnm1", "mnm2", "mnm3", "mnp4")),
    list(sex = "male",   dimension = "length",
         trait = c("mxm1", "mxm2", "mxm3", "mnm1", "mnm2", "mnm3"))
)
# none exceeded the threshold so all can be included

# assess skew and kurtosis
# remove sig skew and kurtosis values
    # Skew and Kurtosis
        # female breadth mxp4
    # Skew
        # female breadth mxm2

step9_eliminations = list(
    list(sex = "female", dimension = "breadth",
         trait = c("mxp4", "mxm2"))
)
