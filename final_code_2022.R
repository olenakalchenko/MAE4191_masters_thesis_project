# ==============================================================================
# Packages
# ==============================================================================

library(haven)
library(dplyr)
library(survey)
library(mitools)
library(psych)
library(corrplot)
library(car)
library(DescTools)
library(lsr)
library(naniar)
library(mice)

packages <- c("haven", "dplyr", "survey", "mitools", "psych", "corrplot", "car", "DescTools", "lsr", "naniar")
for (pkg in packages) { cat("\n===", pkg, "===\n"); print(citation(pkg)) }
R.version.string

# ==============================================================================
# Load data 
# ==============================================================================

# Source: PISA 2022 Student Questionnaire Data (OECD)
# Link: https://www.oecd.org/en/data/datasets/pisa-2022-database.html#data

# Set working directory
# Not needed if I upload everything as an R project
setwd("C:/Users/elly2/Desktop/thesis")
# Load PISA 2022 Student Questionnaire data from SPSS file
CY08MSP_STU_QQQ <- read_sav("CY08MSP_STU_QQQ.SAV", user_na = TRUE)
# Save the original dataset as master copy
MDATA_22_STU_QQQ <- CY08MSP_STU_QQQ

# ==============================================================================
# Variable selection
# ==============================================================================

# ------------------------------------------------------------------------------
# Step 1:  Define columns and subset by country
# ------------------------------------------------------------------------------

pv_cols     <- grep("^PV[0-9]+MATH$", names(MDATA_22_STU_QQQ), value = TRUE)
# OR pv_cols <- paste0("PV", 1:10, "MATH") # math plausible values
w_reps      <- grep("^W_FSTURWT[0-9]+$", names(MDATA_22_STU_QQQ), value = TRUE)
# OR w_reps <- paste0("W_FSTURWT", 1:80) # BRR replicate weights
design_vars <- c("CNT", "STRATUM", "CNTSCHID", "W_FSTUWT")

cands <- c("ESCS", "IMMIG", "LANGN",           # student background / demographics
           "MATHEFF", "ANXMAT",                # learning dispositions
           "TEACHSUP", "DISCLIM",              # school climate / learning environment
           "EXPOFA")                           # opportunity to learn

# cont_cands        <- cands[!cands %in% c("IMMIG", "LANGN")]
# predictors_screen <- c("ESCS", "IMMIG", "LANGN", cont_cands[cont_cands != "ESCS"])

keep_cols <- unique(c(design_vars, cands, w_reps, pv_cols))

STU_NOR <- MDATA_22_STU_QQQ %>% filter(CNT == "NOR") %>% select(all_of(keep_cols))
STU_FIN <- MDATA_22_STU_QQQ %>% filter(CNT == "FIN") %>% select(all_of(keep_cols))

# Quick checks
stopifnot(all(keep_cols %in% names(STU_NOR)))
stopifnot(all(keep_cols %in% names(STU_FIN)))
cat("NOR rows:", nrow(STU_NOR), "| FIN rows:", nrow(STU_FIN), "\n")

# ------------------------------------------------------------------------------
# Step 2: Strip SPSS labels and recode PISA missing value codes to NA
# ------------------------------------------------------------------------------

invalid_codes <- c(99, 999, 9999, 99999, 999999, 9999999,   # Missing
                   97, 997, 9997, 99997, 999997, 9999997,   # N/A
                   98, 998, 9998, 99998, 999998, 9999998,   # Not Reached
                   95, 995, 9995, 99995, 999995, 9999995,   # Quest Not Admin
                   96, 996, 9996, 99996, 999996, 9999996)   # Logical Skip

# Note: read_sav(user_na = TRUE) already converts SPSS user-defined missing
# values to tagged NAs at import. The sentinel recoding below is kept as a
# safeguard for any codes not formally declared as user-missing in the SPSS
# file. Verified: NA counts are identical before and after recoding, confirming
# user_na = TRUE handled all conversions at import.

recode_pisa <- function(df) {
  df %>%
    mutate(across(everything(), haven::zap_missing)) %>% # Use this version
    mutate(across(everything(), haven::zap_labels)) %>%
    mutate(across(all_of(c(cands, pv_cols)),
                  ~ { x <- .x; x[!is.na(x) & x %in% invalid_codes] <- NA; x }))
}

STU_NOR <- recode_pisa(STU_NOR)
STU_FIN <- recode_pisa(STU_FIN)

# Verify: no labels remain, no sentinel codes remain as non-NA values
stopifnot(!any(sapply(STU_NOR, haven::is.labelled)))
stopifnot(!any(sapply(STU_FIN, haven::is.labelled)))
stopifnot(!any(sapply(c(cands, pv_cols), function(v) 
  any(STU_NOR[[v]] %in% invalid_codes, na.rm = TRUE))))
stopifnot(!any(sapply(c(cands, pv_cols), function(v) 
  any(STU_FIN[[v]] %in% invalid_codes, na.rm = TRUE))))

# ------------------------------------------------------------------------------
# Step 3: Diagnostic — inspect which sentinel code types were present in raw data
# ------------------------------------------------------------------------------

# Run on MDATA_22_STU_QQQ to document missingness types
# Useful for methods section: distinguishes non-response from logical skips etc.
# Note: returns TRUE where tagged NAs carry sentinel metadata from user_na = TRUE

cat("\n--- Sentinel code presence in raw data (tagged NAs): Norway ---\n")
sapply(cands, function(v) {
  any(MDATA_22_STU_QQQ[[v]][MDATA_22_STU_QQQ$CNT == "NOR"] %in% invalid_codes,
      na.rm = TRUE)
})

cat("\n--- Sentinel code presence in raw data (tagged NAs): Finland ---\n")
sapply(cands, function(v) {
  any(MDATA_22_STU_QQQ[[v]][MDATA_22_STU_QQQ$CNT == "FIN"] %in% invalid_codes,
      na.rm = TRUE)
})


# ------------------------------------------------------------------------------
# Step 4: Recode LANGN and IMMIG
# ------------------------------------------------------------------------------

# LANGN

# Verify LANGN value labels:
attr(MDATA_22_STU_QQQ$LANGN, "labels")

# Check actual LANGN distributions in the raw data before recoding:
# Examine the empirical distribution of language codes within each country sample.
# This confirms which categories are present and ensures that majority-language codes are correctly specified.
table(MDATA_22_STU_QQQ$LANGN[MDATA_22_STU_QQQ$CNT == "NOR"])
table(MDATA_22_STU_QQQ$LANGN[MDATA_22_STU_QQQ$CNT == "FIN"])

# Inspect variable structure after preprocessing (e.g., zap_labels, NA handling):
# Confirm that LANGN is stored as a numeric (double) variable rather than a labelled or factor variable,
# to ensure valid logical comparisons in subsequent recoding.
class(STU_NOR$LANGN)
typeof(STU_NOR$LANGN)
class(STU_FIN$LANGN)
typeof(STU_FIN$LANGN)

# Verify cleaned distribution:
# Check that invalid/missing codes have been removed or set to NA,
# and that the observed frequencies match expectations prior to recoding.
table(STU_NOR$LANGN, useNA = "ifany")
table(STU_FIN$LANGN, useNA = "ifany")

# Norway: Norwegian (523) = Majority
STU_NOR <- STU_NOR %>%
  mutate(
    LANGN = factor(
      case_when(
        LANGN == 523 ~ "Majority",
        is.na(LANGN) ~ NA_character_,
        TRUE ~ "Other"
      ),
      levels = c("Majority", "Other")
    )
  )

# Finland: Finnish (420) and Swedish (494) = Majority
STU_FIN <- STU_FIN %>%
  mutate(
    LANGN = factor(
      case_when(
        LANGN %in% c(420, 494) ~ "Majority",
        is.na(LANGN) ~ NA_character_,
        TRUE ~ "Other"
      ),
      levels = c("Majority", "Other")
    )
  )

# Verify LANGN recoding
cat("\nLANGN distribution — Norway:\n")
print(table(STU_NOR$LANGN, useNA = "ifany"))
cat("\nLANGN distribution — Finland:\n")
print(table(STU_FIN$LANGN, useNA = "ifany"))

# IMMIG

# Inspect IMMIG value label values:
# Identify how numeric codes map to migration status categories.
attr(MDATA_22_STU_QQQ$IMMIG, "labels")

# Check raw IMMIG distributions by country:
# Verify which categories are present and their frequencies in each sample.
table(MDATA_22_STU_QQQ$IMMIG[MDATA_22_STU_QQQ$CNT == "NOR"])
table(MDATA_22_STU_QQQ$IMMIG[MDATA_22_STU_QQQ$CNT == "FIN"])

# Inspect structure after preprocessing:
# Confirm IMMIG is numeric and missing codes have been handled.
class(STU_NOR$IMMIG)
typeof(STU_NOR$IMMIG)
class(STU_FIN$IMMIG)
typeof(STU_FIN$IMMIG)

# Verify cleaned distributions:
# Ensure only valid categories (1–3) and NA remain.
table(STU_NOR$IMMIG, useNA = "ifany")
table(STU_FIN$IMMIG, useNA = "ifany")

STU_NOR <- STU_NOR %>%
  mutate(
    IMMIG = factor(
      case_when(
        is.na(IMMIG) ~ NA_character_,
        TRUE ~ as.character(IMMIG)
      ),
      levels = c("1", "2", "3"),
      labels = c("Native", "Second-generation", "First-generation")
    )
  )

STU_FIN <- STU_FIN %>%
  mutate(
    IMMIG = factor(
      case_when(
        is.na(IMMIG) ~ NA_character_,
        TRUE ~ as.character(IMMIG)
      ),
      levels = c("1", "2", "3"),
      labels = c("Native", "Second-generation", "First-generation")
    )
  )

# Verify IMMIG recoding
cat("\nIMMIG distribution — Norway:\n")
table(STU_NOR$IMMIG, useNA = "ifany")
cat("\nIMMIG distribution — Finland:\n")
table(STU_FIN$IMMIG, useNA = "ifany")

# ------------------------------------------------------------------------------
# Step 5: IMMIG vs LANGN — diagnostic comparison
# ------------------------------------------------------------------------------
# This step compares IMMIG and LANGN as alternative demographic predictors.
# The aim is to assess (1) overlap with ESCS and (2) association with mathematics performance.
#
# Weighted point estimates are computed using the final student weight (W_FSTUWT)
# for descriptive purposes only; no inference is conducted at this stage.
# Full BRR design is applied from Step 8 onward for all analytical results.
#
# For this diagnostic, both variables are converted to binary numeric indicators:
# LANGN: Majority = 1, Other = 0
# IMMIG: Native = 1, first- and second-generation students = 0
# Missing values remain NA.
#
# This binary simplification is used only for this screening step and is not used
# in the final regression specification of IMMIG.
# ------------------------------------------------------------------------------

weighted_cor <- function(x, y, w) {
  complete <- !is.na(x) & !is.na(y) & !is.na(w)
  x <- x[complete]; y <- y[complete]; w <- w[complete]
  if (length(x) < 3) return(NA_real_)
  wx <- w * (x - weighted.mean(x, w))
  wy <- w * (y - weighted.mean(y, w))
  sum(wx * wy) / sqrt(sum(wx^2) * sum(wy^2))
}

# Convert to binary numeric indicators for diagnostic purposes
immig_bin_nor <- as.numeric(STU_NOR$IMMIG == "Native")
immig_bin_fin <- as.numeric(STU_FIN$IMMIG == "Native")
langn_bin_nor <- as.numeric(STU_NOR$LANGN == "Majority")
langn_bin_fin <- as.numeric(STU_FIN$LANGN == "Majority")

# Correlations with ESCS (overlap with socioeconomic status)
cor_immig_escs_nor <- weighted_cor(immig_bin_nor, STU_NOR$ESCS, STU_NOR$W_FSTUWT)
cor_langn_escs_nor <- weighted_cor(langn_bin_nor, STU_NOR$ESCS, STU_NOR$W_FSTUWT)
cor_immig_escs_fin <- weighted_cor(immig_bin_fin, STU_FIN$ESCS, STU_FIN$W_FSTUWT)
cor_langn_escs_fin <- weighted_cor(langn_bin_fin, STU_FIN$ESCS, STU_FIN$W_FSTUWT)

# Correlations with mathematics performance
# For descriptive comparison, correlations are computed for each PV
# and averaged across all 10 plausible values
cor_immig_math_nor <- mean(sapply(pv_cols, function(pv)
  weighted_cor(immig_bin_nor, STU_NOR[[pv]], STU_NOR$W_FSTUWT)), na.rm = TRUE)
cor_langn_math_nor <- mean(sapply(pv_cols, function(pv)
  weighted_cor(langn_bin_nor, STU_NOR[[pv]], STU_NOR$W_FSTUWT)), na.rm = TRUE)
cor_immig_math_fin <- mean(sapply(pv_cols, function(pv)
  weighted_cor(immig_bin_fin, STU_FIN[[pv]], STU_FIN$W_FSTUWT)), na.rm = TRUE)
cor_langn_math_fin <- mean(sapply(pv_cols, function(pv)
  weighted_cor(langn_bin_fin, STU_FIN[[pv]], STU_FIN$W_FSTUWT)), na.rm = TRUE)

# Correlation between IMMIG and LANGN (direct overlap)
cor_immig_langn_nor <- weighted_cor(immig_bin_nor, langn_bin_nor, STU_NOR$W_FSTUWT)
cor_immig_langn_fin <- weighted_cor(immig_bin_fin, langn_bin_fin, STU_FIN$W_FSTUWT)

# Summary table
dem_comparison <- data.frame(
  country       = c("NOR", "NOR", "FIN", "FIN"),
  variable      = c("IMMIG", "LANGN", "IMMIG", "LANGN"),
  cor_with_ESCS = round(abs(c(cor_immig_escs_nor, cor_langn_escs_nor,
                              cor_immig_escs_fin, cor_langn_escs_fin)), 3),
  cor_with_math = round(abs(c(cor_immig_math_nor, cor_langn_math_nor,
                              cor_immig_math_fin, cor_langn_math_fin)), 3)
)

cat("\n--- IMMIG vs LANGN diagnostic ---\n")
print(dem_comparison)

cat("\n--- Correlation between IMMIG and LANGN ---\n")
cat("NOR:", round(cor_immig_langn_nor, 3), "\n")
cat("FIN:", round(cor_immig_langn_fin, 3), "\n")

# Decision rule:
# Prefer the variable that shows less overlap with ESCS while retaining
# a meaningful association with mathematics performance.
# Also consider conceptual relevance, consistency across countries,
# and interpretability in the final model.

# ------------------------------------------------------------------------------
# Step 6: Identify and remove strata represented by fewer than 2 PSUs
# In the student-level file, PSUs correspond to sampled schools (CNTSCHID).
# Strata with fewer than two distinct PSUs are excluded because they do not
# support variance estimation in subsequent survey design specifications.
# ------------------------------------------------------------------------------

# Check variable structure before filtering:
# Confirm that STRATUM and CNTSCHID are available and inspect their storage type.
class(STU_NOR$STRATUM)
typeof(STU_NOR$STRATUM)
class(STU_NOR$CNTSCHID)
typeof(STU_NOR$CNTSCHID)

class(STU_FIN$STRATUM)
typeof(STU_FIN$STRATUM)
class(STU_FIN$CNTSCHID)
typeof(STU_FIN$CNTSCHID)

# Count distinct PSUs within each stratum before filtering:
# This identifies whether any strata are represented by fewer than 2 schools.
psu_check_nor <- STU_NOR %>%
  group_by(STRATUM) %>%
  summarise(
    n_psu = n_distinct(CNTSCHID),
    n_students = n(),
    .groups = "drop"
  ) %>%
  arrange(n_psu, STRATUM)

psu_check_fin <- STU_FIN %>%
  group_by(STRATUM) %>%
  summarise(
    n_psu = n_distinct(CNTSCHID),
    n_students = n(),
    .groups = "drop"
  ) %>%
  arrange(n_psu, STRATUM)

cat("\n--- PSU count by stratum before filtering: Norway ---\n")
print(psu_check_nor)

cat("\n--- PSU count by stratum before filtering: Finland ---\n")
print(psu_check_fin)

# Summarise how many strata fall below the minimum requirement
cat("\nNorway: strata with <2 PSUs =", sum(psu_check_nor$n_psu < 2), "\n")
cat("Finland: strata with <2 PSUs =", sum(psu_check_fin$n_psu < 2), "\n")

# Function to remove strata with fewer than 2 distinct PSUs
remove_single_psu <- function(df) {
  valid <- df %>%
    group_by(STRATUM) %>%
    summarise(n_psu = n_distinct(CNTSCHID), .groups = "drop") %>%
    filter(n_psu >= 2)
  
  out <- df %>% semi_join(valid, by = "STRATUM")
  
  message(
    unique(df$CNT), ": removed ", nrow(df) - nrow(out),
    " rows from strata with fewer than 2 PSUs. Remaining: ", nrow(out)
  )
  
  out
}

STU_NOR <- remove_single_psu(STU_NOR)
STU_FIN <- remove_single_psu(STU_FIN)

# Verify PSU structure after filtering:
# Confirm that all remaining strata now contain at least 2 distinct PSUs.
psu_check_nor_after <- STU_NOR %>%
  group_by(STRATUM) %>%
  summarise(
    n_psu = n_distinct(CNTSCHID),
    n_students = n(),
    .groups = "drop"
  ) %>%
  arrange(n_psu, STRATUM)

psu_check_fin_after <- STU_FIN %>%
  group_by(STRATUM) %>%
  summarise(
    n_psu = n_distinct(CNTSCHID),
    n_students = n(),
    .groups = "drop"
  ) %>%
  arrange(n_psu, STRATUM)

cat("\n--- PSU count by stratum after filtering: Norway ---\n")
print(psu_check_nor_after, n = Inf)

cat("\n--- PSU count by stratum after filtering: Finland ---\n")
print(psu_check_fin_after, n = Inf)

cat("\nNorway: any strata with <2 PSUs after filtering? ",
    any(psu_check_nor_after$n_psu < 2), "\n")
cat("Finland: any strata with <2 PSUs after filtering? ",
    any(psu_check_fin_after$n_psu < 2), "\n")

# ------------------------------------------------------------------------------
# Step 7: Build BRR survey designs
# These survey design objects are reused for all subsequent weighted analyses.
# PISA 2022 uses Fay's BRR replication; the final student weight (W_FSTUWT)
# and the 80 replicate weights (W_FSTURWT1-W_FSTURWT80) are used here.
# ------------------------------------------------------------------------------

# Check structure of the full weight and replicate weights
class(STU_NOR$W_FSTUWT)
typeof(STU_NOR$W_FSTUWT)
class(STU_FIN$W_FSTUWT)
typeof(STU_FIN$W_FSTUWT)

# Check that all replicate-weight columns are present
stopifnot(all(w_reps %in% names(STU_NOR)))
stopifnot(all(w_reps %in% names(STU_FIN)))

# Verify replicate weights are numeric
stopifnot(all(sapply(STU_NOR[, w_reps], is.numeric)))
stopifnot(all(sapply(STU_FIN[, w_reps], is.numeric)))

# Check for missing values in the final weight and replicate weights
cat("\nMissing values in W_FSTUWT — Norway:", sum(is.na(STU_NOR$W_FSTUWT)), "\n")
cat("Missing values in W_FSTUWT — Finland:", sum(is.na(STU_FIN$W_FSTUWT)), "\n")

cat("\nMissing values across BRR replicate weights — Norway:",
    sum(is.na(as.matrix(STU_NOR[, w_reps]))), "\n")
cat("Missing values across BRR replicate weights — Finland:",
    sum(is.na(as.matrix(STU_FIN[, w_reps]))), "\n")

# Function to build BRR survey design
make_design <- function(df, reps) {
  svrepdesign(
    weights          = ~W_FSTUWT,
    repweights       = as.matrix(df[, reps]),
    data             = df,
    type             = "BRR",
    fay.rho          = 0.5,
    combined.weights = TRUE,
    mse              = TRUE
  )
}

# Build survey design objects
des_nor <- make_design(STU_NOR, w_reps)
des_fin <- make_design(STU_FIN, w_reps)

# Verify design objects
cat("\n--- Survey design summary: Norway ---\n")
print(summary(des_nor))

cat("\n--- Survey design summary: Finland ---\n")
print(summary(des_fin))

cat("\nWeighted mean ESCS — Norway:\n")
print(svymean(~ESCS, des_nor, na.rm = TRUE))

cat("\nWeighted mean ESCS — Finland:\n")
print(svymean(~ESCS, des_fin, na.rm = TRUE))

# ------------------------------------------------------------------------------
# Step 8: Weighted missingness per predictor
# Computes the weighted percentage of missing values for each predictor using
# the BRR survey design. Missingness is coded as a 0/1 indicator, so the
# weighted mean corresponds directly to the weighted share of missing values.
# ------------------------------------------------------------------------------

weighted_missing <- function(design, vars) {
  res <- sapply(vars, function(v) {
    f <- as.formula(paste0("~ I(as.numeric(is.na(", v, ")))"))
    m <- svymean(f, design = design, na.rm = TRUE)
    round(as.numeric(m) * 100, 2)
  })
  
  data.frame(
    variable = vars,
    missing_pct = as.numeric(res),
    row.names = NULL
  )
}

miss_nor <- weighted_missing(des_nor, cands)
miss_fin <- weighted_missing(des_fin, cands)

miss_summary <- merge(
  miss_nor, miss_fin,
  by = "variable",
  suffixes = c("_NOR", "_FIN")
)

cat("\n--- Weighted missingness (%) by predictor ---\n")
print(miss_summary)

# Thresholds for interpretation:
# >5% warrants discussion
# >30% suggests exclusion unless there is a strong substantive reason to retain

# Interpretation:
# Several predictors show non-trivial weighted missingness, especially in Norway,
# with the highest levels observed for ANXMAT, EXPOFA, and MATHEFF.
# However, no variable exceeds the exclusion threshold of 30%.
# Variables with missingness above 5% should be discussed explicitly in the
# methods/results sections and considered carefully in subsequent diagnostics.

# ------------------------------------------------------------------------------
# Step 9a: Unweighted complete-case summary (analytic sample size)
# Computes the number and proportion of observations with no missing values
# across all selected predictors under a casewise deletion approach.
# ------------------------------------------------------------------------------

# Check that all candidate predictors are present in both country datasets
stopifnot(all(cands %in% names(STU_NOR)))
stopifnot(all(cands %in% names(STU_FIN)))

# Inspect predictors included in the complete-case diagnostic
cat("\n--- Predictors included in complete-case check ---\n")
print(cands)

# Verify row counts before computing complete cases
cat("\nRows in STU_NOR:", nrow(STU_NOR), "\n")
cat("Rows in STU_FIN:", nrow(STU_FIN), "\n")

# Unweighted missing counts by predictor
cat("\n--- Unweighted missing counts by predictor: Norway ---\n")
print(colSums(is.na(STU_NOR[, cands])))

cat("\n--- Unweighted missing counts by predictor: Finland ---\n")
print(colSums(is.na(STU_FIN[, cands])))

complete_cases_summary <- function(df, vars) {
  cc_flag <- complete.cases(df[, vars])
  
  data.frame(
    country      = dplyr::first(df$CNT),
    n_total      = nrow(df),
    n_complete   = sum(cc_flag),
    pct_complete = round(sum(cc_flag) / nrow(df) * 100, 2),
    n_dropped    = nrow(df) - sum(cc_flag),
    pct_dropped  = round((nrow(df) - sum(cc_flag)) / nrow(df) * 100, 2),
    row.names    = NULL
  )
}

cc_table <- bind_rows(
  complete_cases_summary(STU_NOR, cands),
  complete_cases_summary(STU_FIN, cands)
)

cat("\n--- Complete cases summary (unweighted analytic sample) ---\n")
print(cc_table)

# Internal consistency check
stopifnot(all(cc_table$n_complete + cc_table$n_dropped == cc_table$n_total))

# ------------------------------------------------------------------------------
# Step 9b: Weighted complete-case summary
# Computes the weighted share of complete and incomplete cases using the BRR
# survey design. This shows the population-representative impact of casewise
# deletion.
# ------------------------------------------------------------------------------

add_complete_case_flag <- function(df, vars) {
  df$cc_all <- as.numeric(complete.cases(df[, vars]))
  df
}

STU_NOR <- add_complete_case_flag(STU_NOR, cands)
STU_FIN <- add_complete_case_flag(STU_FIN, cands)

# Rebuild design objects so they include the complete-case flag
des_nor <- make_design(STU_NOR, w_reps)
des_fin <- make_design(STU_FIN, w_reps)

weighted_complete_case_summary <- function(design, country_label) {
  m <- svymean(~cc_all, design = design, na.rm = TRUE)
  
  data.frame(
    country = country_label,
    pct_complete_weighted = round(as.numeric(m) * 100, 2),
    pct_dropped_weighted  = round(100 - as.numeric(m) * 100, 2),
    row.names = NULL
  )
}

cc_weighted <- bind_rows(
  weighted_complete_case_summary(des_nor, "NOR"),
  weighted_complete_case_summary(des_fin, "FIN")
)

cat("\n--- Complete cases summary (weighted) ---\n")
print(cc_weighted)

cc_summary_full <- merge(cc_table, cc_weighted, by = "country")

cat("\n--- Complete cases summary (unweighted + weighted) ---\n")
print(cc_summary_full)

# ------------------------------------------------------------------------------
# Step 9c: Diagnose whether missingness is consistent with MCAR
# Missingness is modeled as a binary outcome (1 = missing, 0 = observed)
# using weighted survey logistic regression with BRR/Fay design.
#
# Interpretation:
# - If missingness is associated with observed covariates, MCAR is unlikely.
# - This does NOT prove MAR, but it provides evidence against MCAR.
# ------------------------------------------------------------------------------

# Variables with notable missingness to examine more closely
miss_vars <- c("MATHEFF", "ANXMAT", "TEACHSUP", "DISCLIM", "EXPOFA", "ESCS")

# Predictors used to explain missingness
# Keep this set limited to observed background variables that are easy to interpret.
# Since IMMIG and LANGN may themselves be missing, they can still be included,
# but observations with missing predictor values will be omitted from each model.
mis_predictors <- c("ESCS", "IMMIG", "LANGN")

# Check that all requested variables exist
stopifnot(all(miss_vars %in% names(STU_NOR)))
stopifnot(all(miss_vars %in% names(STU_FIN)))
stopifnot(all(mis_predictors %in% names(STU_NOR)))
stopifnot(all(mis_predictors %in% names(STU_FIN)))

# Add missingness indicators to each dataset
add_missing_indicators <- function(df, vars) {
  for (v in vars) {
    df[[paste0("mis_", v)]] <- as.numeric(is.na(df[[v]]))
  }
  df
}

STU_NOR <- add_missing_indicators(STU_NOR, miss_vars)
STU_FIN <- add_missing_indicators(STU_FIN, miss_vars)

# Rebuild survey designs so the new missingness indicators are included
des_nor <- make_design(STU_NOR, w_reps)
des_fin <- make_design(STU_FIN, w_reps)

# Verify that missingness indicators were created correctly
cat("\n--- Missingness indicator checks: Norway ---\n")
print(colSums(STU_NOR[, paste0("mis_", miss_vars)]))

cat("\n--- Missingness indicator checks: Finland ---\n")
print(colSums(STU_FIN[, paste0("mis_", miss_vars)]))

# Function to run weighted logistic regressions of missingness
run_missingness_models <- function(design, miss_vars, predictors, country_label) {
  model_list <- list()
  
  for (v in miss_vars) {
    preds_v <- setdiff(predictors, v)
    
    f <- as.formula(
      paste0("mis_", v, " ~ ", paste(preds_v, collapse = " + "))
    )
    
    model_list[[v]] <- svyglm(
      f,
      design = design,
      family = quasibinomial()
    )
  }
  
  names(model_list) <- paste0(country_label, "_", miss_vars)
  model_list
}

mods_mis_nor <- run_missingness_models(des_nor, miss_vars, mis_predictors, "NOR")
mods_mis_fin <- run_missingness_models(des_fin, miss_vars, mis_predictors, "FIN")

# Print full model summaries
cat("\n==============================\n")
cat("Weighted missingness models — Norway\n")
cat("==============================\n")
for (nm in names(mods_mis_nor)) {
  cat("\n---", nm, "---\n")
  print(summary(mods_mis_nor[[nm]]))
}

cat("\n==============================\n")
cat("Weighted missingness models — Finland\n")
cat("==============================\n")
for (nm in names(mods_mis_fin)) {
  cat("\n---", nm, "---\n")
  print(summary(mods_mis_fin[[nm]]))
}

# ------------------------------------------------------------------------------
# Extract a compact summary table of coefficients and p-values
# ------------------------------------------------------------------------------

extract_missingness_results <- function(model_list, country_label) {
  out <- lapply(names(model_list), function(v) {
    sm <- summary(model_list[[v]])$coefficients
    
    data.frame(
      country   = country_label,
      outcome   = sub(paste0("^", country_label, "_"), "", v),
      term      = rownames(sm),
      estimate  = sm[, "Estimate"],
      std_error = sm[, "Std. Error"],
      t_value   = sm[, "t value"],
      p_value   = sm[, "Pr(>|t|)"],
      row.names = NULL
    )
  })
  
  do.call(rbind, out)
}

mis_results_nor <- extract_missingness_results(mods_mis_nor, "NOR")
mis_results_fin <- extract_missingness_results(mods_mis_fin, "FIN")
mis_results_all <- rbind(mis_results_nor, mis_results_fin)

mis_sig <- subset(mis_results_all, term != "(Intercept)" & p_value < 0.05)

cat("\n--- Compact summary of weighted missingness models ---\n")
print(mis_results_all)

cat("\n--- Significant predictors of missingness (p < .05) ---\n")
print(mis_sig)

# ------------------------------------------------------------------------------
# Optional descriptive diagnostics:
# Weighted missingness rates by IMMIG and LANGN
# These are useful as a quick descriptive check alongside the regression models.
# ------------------------------------------------------------------------------

weighted_missing_by_group <- function(design, miss_var, group_var, country_label) {
  f_miss  <- as.formula(paste0("~ mis_", miss_var))
  f_group <- as.formula(paste0("~ ", group_var))
  
  out <- svyby(
    f_miss,
    f_group,
    design,
    svymean,
    na.rm = TRUE,
    vartype = "se"
  )
  
  data.frame(
    country = country_label,
    missing_variable = miss_var,
    group_variable = group_var,
    group = out[[group_var]],
    missing_prop = out[[paste0("mis_", miss_var)]],
    se = out[["se"]],
    row.names = NULL
  )
}

# Run descriptive weighted missingness checks for all target variables by IMMIG and LANGN
group_checks_nor <- do.call(rbind, lapply(miss_vars, function(v) {
  rbind(
    weighted_missing_by_group(des_nor, v, "IMMIG", "NOR"),
    weighted_missing_by_group(des_nor, v, "LANGN", "NOR")
  )
}))

group_checks_fin <- do.call(rbind, lapply(miss_vars, function(v) {
  rbind(
    weighted_missing_by_group(des_fin, v, "IMMIG", "FIN"),
    weighted_missing_by_group(des_fin, v, "LANGN", "FIN")
  )
}))

group_checks_all <- rbind(group_checks_nor, group_checks_fin)

group_checks_all$missing_pct <- round(group_checks_all$missing_prop * 100, 2)
group_checks_all$se_pct <- round(group_checks_all$se * 100, 2)

cat("\n--- Weighted missingness by group (IMMIG / LANGN) ---\n")
print(group_checks_all)

# ------------------------------------------------------------------------------
# Step 9d (summary): Highlight large group differences in missingness
# Computes the maximum gap in weighted missingness percentages within each
# country × missing-variable × grouping-variable combination.
# Large gaps provide descriptive evidence against MCAR.
# ------------------------------------------------------------------------------

group_gaps <- group_checks_all %>%
  group_by(country, missing_variable, group_variable) %>%
  summarise(
    max_gap = max(missing_pct) - min(missing_pct),
    .groups = "drop"
  ) %>%
  arrange(desc(max_gap))

cat("\n--- Largest group differences in missingness (%) ---\n")
print(group_gaps, n = Inf)

# Optional flags for interpretation
group_gaps <- group_gaps %>%
  mutate(
    interpretation = case_when(
      max_gap > 20 ~ "Strong evidence against MCAR",
      max_gap > 10 ~ "Meaningful difference",
      TRUE ~ "Small difference"
    )
  )

cat("\n--- Largest group differences in missingness (%) with interpretation ---\n")
print(group_gaps, n = Inf)

# ------------------------------------------------------------------------------
# Step 9e: Little's MCAR test (supplementary, unweighted)
# Note:
# - This is a supplementary diagnostic only.
# - It does NOT account for PISA's complex survey design or weights.
# - Primary missingness assessment should rely on the weighted diagnostics above.
# ------------------------------------------------------------------------------

# Prepare temporary data for Little's test
# Convert factors to numeric codes so the test runs more smoothly
little_data_nor <- STU_NOR[, cands]
little_data_fin <- STU_FIN[, cands]

little_data_nor[] <- lapply(little_data_nor, function(x) {
  if (is.factor(x)) as.numeric(x) else x
})

little_data_fin[] <- lapply(little_data_fin, function(x) {
  if (is.factor(x)) as.numeric(x) else x
})

cat("\n--- Little's MCAR test: Norway ---\n")
little_nor <- mcar_test(little_data_nor)
print(little_nor)

cat("\n--- Little's MCAR test: Finland ---\n")
little_fin <- mcar_test(little_data_fin)
print(little_fin)

# ------------------------------------------------------------------------------
# Step 10: Weighted correlations with math (averaged across all 10 PVs)
# This is a descriptive screening step. Correlations are computed separately for
# each plausible value and then averaged across PV1MATH-PV10MATH.
# ------------------------------------------------------------------------------
# Notes:
# - Continuous predictors are examined directly.
# - For categorical demographic variables, temporary binary indicators are used
#   for screening only:
#     IMMIG: Native = 1, all immigrant groups = 0
#     LANGN: Majority = 1, Other = 0
# - These binary codings are NOT used for final regression specification.
# ------------------------------------------------------------------------------

# Continuous predictors
cont_cands <- cands[!cands %in% c("IMMIG", "LANGN")]

# Temporary binary versions of demographic variables for screening only
immig_bin_nor <- as.numeric(STU_NOR$IMMIG == "Native")
immig_bin_fin <- as.numeric(STU_FIN$IMMIG == "Native")
langn_bin_nor <- as.numeric(STU_NOR$LANGN == "Majority")
langn_bin_fin <- as.numeric(STU_FIN$LANGN == "Majority")

# Check variables going into Step 11
cat("\n--- Step 11: predictors used for weighted predictor–math correlations ---\n")
print(cont_cands)
cat("\nDemographic screening variables: IMMIG (binary), LANGN (binary)\n")

weighted_pv_cors <- function(df, vars, pvs) {
  sapply(vars, function(pred) {
    cors <- sapply(pvs, function(pv) {
      weighted_cor(as.numeric(df[[pred]]), df[[pv]], df$W_FSTUWT)
    })
    mean(cors, na.rm = TRUE)
  })
}

weighted_pv_cors_bin <- function(x, df, pvs) {
  cors <- sapply(pvs, function(pv) {
    weighted_cor(x, df[[pv]], df$W_FSTUWT)
  })
  mean(cors, na.rm = TRUE)
}

# Continuous predictors
pvcors_nor_cont <- weighted_pv_cors(STU_NOR, cont_cands, pv_cols)
pvcors_fin_cont <- weighted_pv_cors(STU_FIN, cont_cands, pv_cols)

# Binary demographic screening indicators
pvcors_nor_immig <- weighted_pv_cors_bin(immig_bin_nor, STU_NOR, pv_cols)
pvcors_fin_immig <- weighted_pv_cors_bin(immig_bin_fin, STU_FIN, pv_cols)

pvcors_nor_langn <- weighted_pv_cors_bin(langn_bin_nor, STU_NOR, pv_cols)
pvcors_fin_langn <- weighted_pv_cors_bin(langn_bin_fin, STU_FIN, pv_cols)

pvcor_table <- data.frame(
  variable = c(cont_cands, "IMMIG_bin", "LANGN_bin"),
  avg_cor_NOR = round(c(pvcors_nor_cont, pvcors_nor_immig, pvcors_nor_langn), 3),
  avg_cor_FIN = round(c(pvcors_fin_cont, pvcors_fin_immig, pvcors_fin_langn), 3),
  row.names = NULL
)

cat("\n--- Weighted predictor–outcome correlations (avg across PVs) ---\n")
print(pvcor_table)

# Flag predictors with weak associations in either country
pvcor_flags <- subset(
  pvcor_table,
  abs(avg_cor_NOR) < 0.10 | abs(avg_cor_FIN) < 0.10
)

cat("\n--- Predictors with |r| < .10 in at least one country ---\n")
print(pvcor_flags)


# ------------------------------------------------------------------------------
# Step 11: Weighted inter-predictor correlation matrix (no jtools needed)
# Uses survey::svyvar() to compute covariance matrix, then converts to correlations
# ------------------------------------------------------------------------------

weighted_cor_matrix <- function(vars, design) {
  
  # Build formula
  f <- as.formula(paste("~", paste(vars, collapse = " + ")))
  
  # Get covariance matrix
  cov_mat <- as.matrix(svyvar(f, design, na.rm = TRUE))
  
  # Convert to correlation matrix
  sd_vec <- sqrt(diag(cov_mat))
  cor_mat <- cov_mat / (sd_vec %o% sd_vec)
  
  return(cor_mat)
}

# Compute matrices
wcor_nor <- weighted_cor_matrix(cont_cands, des_nor)
wcor_fin <- weighted_cor_matrix(cont_cands, des_fin)

cat("\n--- Weighted predictor correlations: Norway ---\n")
print(round(wcor_nor, 3))

cat("\n--- Weighted predictor correlations: Finland ---\n")
print(round(wcor_fin, 3))

# Optional visualization
corrplot::corrplot(
  wcor_nor,
  method = "color",
  type = "upper",
  tl.col = "black",
  addCoef.col = "black",
  number.cex = 0.7,
  title = "Norway",
  mar = c(0, 0, 1, 0)
)

corrplot::corrplot(
  wcor_fin,
  method = "color",
  type = "upper",
  tl.col = "black",
  addCoef.col = "black",
  number.cex = 0.7,
  title = "Finland",
  mar = c(0, 0, 1, 0)
)

# Flag high correlations
find_high_corr_pairs <- function(cor_mat, cutoff = 0.8) {
  idx <- which(abs(cor_mat) > cutoff & upper.tri(cor_mat), arr.ind = TRUE)
  
  if (nrow(idx) == 0) return(data.frame())
  
  data.frame(
    var1 = rownames(cor_mat)[idx[, 1]],
    var2 = colnames(cor_mat)[idx[, 2]],
    corr = cor_mat[idx],
    row.names = NULL
  )
}

cat("\n--- Predictor pairs with |r| > .80: Norway ---\n")
print(find_high_corr_pairs(wcor_nor))

cat("\n--- Predictor pairs with |r| > .80: Finland ---\n")
print(find_high_corr_pairs(wcor_fin))

# ------------------------------------------------------------------------------
# Step 12: VIF check (unweighted, diagnostic only)
# This is a directional multicollinearity screen using PV1MATH as a proxy outcome.
# ------------------------------------------------------------------------------
# Notes:
# - Unweighted
# - Single plausible value only
# - Includes both IMMIG and LANGN provisionally, because no final demographic
#   variable has been selected yet.
# ------------------------------------------------------------------------------

# Provisional predictor set for VIF screening
predictors_screen <- c("ESCS", "IMMIG", "LANGN", cont_cands[cont_cands != "ESCS"])

cat("\n--- Step 13: provisional predictors for VIF screening ---\n")
print(predictors_screen)

# Check presence
stopifnot(all(predictors_screen %in% names(STU_NOR)))
stopifnot(all(predictors_screen %in% names(STU_FIN)))

vif_formula <- as.formula(
  paste("PV1MATH ~", paste(predictors_screen, collapse = " + "))
)

cat("\n--- VIF: Norway ---\n")
print(round(car::vif(lm(vif_formula, data = STU_NOR)), 2))

cat("\n--- VIF: Finland ---\n")
print(round(car::vif(lm(vif_formula, data = STU_FIN)), 2))

# Interpretation guideline:
# - VIF > 5: warrants attention
# - VIF > 10: problematic

# ------------------------------------------------------------------------------
# Step 13: Country-level entirely-missing check
# Checks whether any predictor is entirely missing within a country sample.
# ------------------------------------------------------------------------------

entirely_missing <- function(df, vars) {
  sapply(vars, function(v) all(is.na(df[[v]])))
}

cat("\n--- Entirely missing: Norway ---\n")
print(entirely_missing(STU_NOR, predictors_screen))

cat("\n--- Entirely missing: Finland ---\n")
print(entirely_missing(STU_FIN, predictors_screen))

# ------------------------------------------------------------------------------
# Provisional variable selection summary
# No variable is removed at this stage.
# This block simply prints the variables currently retained for screening.
# ------------------------------------------------------------------------------

cat("\nProvisional predictor set retained for now:\n")
print(predictors_screen)

# ------------------------------------------------------------------------------
# Step 14: Confirm final predictor set
# This block defines the confirmed predictor set for use in multiple imputation 
# and Oaxaca-Blinder decomposition. No further variables are dropped or added
# beyond this point.
# ------------------------------------------------------------------------------

# Final predictor set: ESCS, LANGN, MATHEFF, ANXMAT, TEACHSUP

final_preds_22 <- c("ESCS", "LANGN", "MATHEFF", "ANXMAT", "TEACHSUP")
final_cont_preds_22 <- final_preds_22[!final_preds_22 %in% "LANGN"]

stopifnot(all(final_preds_22 %in% names(STU_NOR)))
stopifnot(all(final_preds_22 %in% names(STU_FIN)))

cat("\n--- Final predictor set: 2022 ---\n")
print(final_preds_22)

cat("\nFinal continuous predictors:\n")
print(final_cont_preds_22)

cat("\nFinal sample sizes:\n")
cat("Norway:", nrow(STU_NOR), "\n")
cat("Finland:", nrow(STU_FIN), "\n")

# ==============================================================================
# Multiple Imputation — PISA 2022
# ==============================================================================
# Strategy:
# - For each of the 10 plausible values, create a version of the data with
#   that PV included as a single auxiliary variable (renamed PV_MATH)
# - Run mice() on each PV-specific dataset with m = 20 imputations
# - This gives 10 x 20 = 200 imputed datasets per country
# - PV_MATH is included as a predictor but not imputed
# - Survey weight W_FSTUWT is excluded from imputation entirely
# - LANGN (binary factor): logreg
# - Continuous predictors (ESCS, MATHEFF, ANXMAT, TEACHSUP): pmm
# ==============================================================================

# ------------------------------------------------------------------------------
# Step 15: Build predictor matrix and method vector
# ------------------------------------------------------------------------------

build_imp_config <- function(data, cont_preds, factor_preds,
                             pv_col = "PV_MATH",
                             weight_col = "W_FSTUWT") {
  
  # Start from default — all 1s except diagonal
  pred <- mice::make.predictorMatrix(data)
  
  # W_FSTUWT: exclude completely — neither predicts nor is predicted
  if (weight_col %in% colnames(pred)) {
    pred[weight_col, ] <- 0
    pred[, weight_col] <- 0
  }
  
  # PV_MATH: not imputed, but used as predictor
  if (pv_col %in% colnames(pred)) {
    pred[pv_col, ] <- 0   
  }
  
  # Set imputation methods
  meth <- rep("", ncol(data))
  names(meth) <- colnames(data)
  
  meth[cont_preds]   <- "pmm"
  meth[factor_preds] <- "logreg"
  meth[pv_col]       <- ""
  meth[weight_col]   <- ""
  
  list(method = meth, predictorMatrix = pred)
}

# ------------------------------------------------------------------------------
# Step 16: Prepare one dataset per PV
# ------------------------------------------------------------------------------

# Base columns: final predictors + survey weight
base_cols <- c(final_preds_22, "W_FSTUWT")

# For each PV, create a dataset with that PV renamed to PV_MATH
make_pv_datasets <- function(df, pv_cols, base_cols) {
  lapply(pv_cols, function(pv) {
    dat <- df[, c(base_cols, pv)]
    names(dat)[names(dat) == pv] <- "PV_MATH"
    dat
  })
}

pv_data_nor <- make_pv_datasets(STU_NOR, pv_cols, base_cols)
pv_data_fin <- make_pv_datasets(STU_FIN, pv_cols, base_cols)

cat("\n--- PV datasets prepared ---\n")
cat("Norway: ", length(pv_data_nor), "datasets, each with",
    ncol(pv_data_nor[[1]]), "columns and",
    nrow(pv_data_nor[[1]]), "rows\n")
cat("Finland:", length(pv_data_fin), "datasets, each with",
    ncol(pv_data_fin[[1]]), "columns and",
    nrow(pv_data_fin[[1]]), "rows\n")

# Verify structure of first PV dataset
cat("\n--- Column names (PV1 dataset): ---\n")
print(names(pv_data_nor[[1]]))

cat("\n--- Missing values in PV1 dataset: Norway ---\n")
print(colSums(is.na(pv_data_nor[[1]])))

# Build config from first PV dataset (structure identical across all PVs)
config_22 <- build_imp_config(
  data         = pv_data_nor[[1]],
  cont_preds   = final_cont_preds_22,
  factor_preds = "LANGN"
)

cat("\n--- Imputation methods ---\n")
print(config_22$method)

cat("\n--- Predictor matrix ---\n")
print(config_22$predictorMatrix)

# ------------------------------------------------------------------------------
# Step 17: Quick test run before full imputation
# ------------------------------------------------------------------------------

cat("\n--- Test run: PV1, Norway, m = 5, maxit = 5 ---\n")
set.seed(2022)
test_imp_nor <- mice(
  pv_data_nor[[1]],
  m               = 5,
  maxit           = 5,
  method          = config_22$method,
  predictorMatrix = config_22$predictorMatrix,
  printFlag       = TRUE
)
cat("Norway test passed — no errors\n")

cat("\n--- Test run: PV1, Finland, m = 5, maxit = 5 ---\n")
set.seed(2022)
test_imp_fin <- mice(
  pv_data_fin[[1]],
  m               = 5,
  maxit           = 5,
  method          = config_22$method,
  predictorMatrix = config_22$predictorMatrix,
  printFlag       = TRUE
)
cat("Finland test passed — no errors\n")

# ------------------------------------------------------------------------------
# Step 18: Full imputation — Norway
# ------------------------------------------------------------------------------

cat("\n--- Full imputation: Norway (10 PVs x m = 20) ---\n")

imp_nor_list <- vector("list", length(pv_cols))
names(imp_nor_list) <- pv_cols

for (i in seq_along(pv_cols)) {
  cat("\n  Norway — PV", i, "of", length(pv_cols), "\n")
  set.seed(2022 + i)
  imp_nor_list[[i]] <- mice(
    pv_data_nor[[i]],
    m               = 20,
    maxit           = 20,
    method          = config_22$method,
    predictorMatrix = config_22$predictorMatrix,
    printFlag       = FALSE
  )
}

cat("\nNorway imputation complete.\n")
cat("Imputed datasets: 10 PVs x 20 imputations =",
    length(imp_nor_list) * 20, "\n")

# ------------------------------------------------------------------------------
# Step 19: Full imputation — Finland
# ------------------------------------------------------------------------------

cat("\n--- Full imputation: Finland (10 PVs x m = 20) ---\n")

imp_fin_list <- vector("list", length(pv_cols))
names(imp_fin_list) <- pv_cols

for (i in seq_along(pv_cols)) {
  cat("\n  Finland — PV", i, "of", length(pv_cols), "\n")
  set.seed(2022 + i)
  imp_fin_list[[i]] <- mice(
    pv_data_fin[[i]],
    m               = 20,
    maxit           = 20,
    method          = config_22$method,
    predictorMatrix = config_22$predictorMatrix,
    printFlag       = FALSE
  )
}

cat("\nFinland imputation complete.\n")
cat("Imputed datasets: 10 PVs x 20 imputations =",
    length(imp_fin_list) * 20, "\n")

# ------------------------------------------------------------------------------
# Step 20: Validate imputations
# ------------------------------------------------------------------------------

# --- Convergence plots: Norway 2022 ---
plot(imp_nor_list[[1]], main = "Convergence diagnostics — Norway 2022 (PV1)")

# --- Convergence plots: Finland 2022 ---
plot(imp_fin_list[[1]], main = "Convergence diagnostics — Finland 2022 (PV1)")

# --- Density plots for continuous variables ---
densityplot(imp_nor_list[[1]], ~ ESCS + MATHEFF + ANXMAT + TEACHSUP)
densityplot(imp_fin_list[[1]], ~ ESCS + MATHEFF + ANXMAT + TEACHSUP)

# --- LANGN: check imputed proportions across imputations ---
cat("\n--- LANGN imputed proportions: Norway ---\n")
nor_langn_props <- sapply(1:20, function(j) {
  dat <- complete(imp_nor_list[[1]], j)
  prop.table(table(dat$LANGN))
})
print(round(nor_langn_props, 3))

cat("\n--- LANGN observed proportion: Norway ---\n")
print(round(prop.table(table(STU_NOR$LANGN, useNA = "no")), 3))

cat("\n--- LANGN imputed proportions: Finland ---\n")
fin_langn_props <- sapply(1:20, function(j) {
  dat <- complete(imp_fin_list[[1]], j)
  prop.table(table(dat$LANGN))
})
print(round(fin_langn_props, 3))

cat("\n--- LANGN observed proportion: Finland ---\n")
print(round(prop.table(table(STU_FIN$LANGN, useNA = "no")), 3))

# --- Missing values after imputation: all PVs, all imputations ---

cat("\n--- Missing values after imputation: Norway ---\n")
nor_missing_full <- sapply(seq_along(pv_cols), function(i) {
  sapply(1:20, function(j) {
    sum(is.na(complete(imp_nor_list[[i]], j)[, final_preds_22]))
  })
})
colnames(nor_missing_full) <- pv_cols
rownames(nor_missing_full) <- paste0("imp_", 1:20)
print(nor_missing_full)
cat("All zero?", all(nor_missing_full == 0), "\n")

cat("\n--- Missing values after imputation: Finland ---\n")
fin_missing_full <- sapply(seq_along(pv_cols), function(i) {
  sapply(1:20, function(j) {
    sum(is.na(complete(imp_fin_list[[i]], j)[, final_preds_22]))
  })
})
colnames(fin_missing_full) <- pv_cols
rownames(fin_missing_full) <- paste0("imp_", 1:20)
print(fin_missing_full)
cat("All zero?", all(fin_missing_full == 0), "\n")

# ------------------------------------------------------------------------------
# Step 21: Extract imputed datasets
# ------------------------------------------------------------------------------

extract_imputed <- function(imp_list, pv_cols, m) {
  lapply(seq_along(pv_cols), function(i) {
    lapply(1:m, function(j) {
      dat           <- complete(imp_list[[i]], j)
      dat$pv_index  <- i
      dat$imp_index <- j
      dat
    })
  })
}

imp_datasets_nor <- extract_imputed(imp_nor_list, pv_cols, m = 20)
imp_datasets_fin <- extract_imputed(imp_fin_list, pv_cols, m = 20)

cat("\n--- Imputed datasets extracted ---\n")
cat("Norway:  ", length(imp_datasets_nor), "PVs x",
    length(imp_datasets_nor[[1]]), "imputations =",
    length(imp_datasets_nor) * length(imp_datasets_nor[[1]]),
    "total datasets\n")
cat("Finland: ", length(imp_datasets_fin), "PVs x",
    length(imp_datasets_fin[[1]]), "imputations =",
    length(imp_datasets_fin) * length(imp_datasets_fin[[1]]),
    "total datasets\n")

# --- Verify column structure is identical across all datasets ---
cat("\n--- Column names (should be identical across all datasets) ---\n")
print(names(imp_datasets_nor[[1]][[1]]))

nor_cols_consistent <- all(sapply(seq_along(pv_cols), function(i)
  sapply(1:20, function(j)
    identical(names(imp_datasets_nor[[i]][[j]]),
              names(imp_datasets_nor[[1]][[1]])))))

fin_cols_consistent <- all(sapply(seq_along(pv_cols), function(i)
  sapply(1:20, function(j)
    identical(names(imp_datasets_fin[[i]][[j]]),
              names(imp_datasets_nor[[1]][[1]])))))

cat("Column structure consistent — Norway:", nor_cols_consistent, "\n")
cat("Column structure consistent — Finland:", fin_cols_consistent, "\n")

# --- Predictor means across all PVs and imputations ---
# Average means per predictor across all 200 datasets per country
# Should be close to the observed means

cat("\n--- Average predictor means across all 200 datasets: Norway ---\n")
nor_means <- Reduce("+", lapply(seq_along(pv_cols), function(i)
  Reduce("+", lapply(1:20, function(j)
    colMeans(imp_datasets_nor[[i]][[j]][, final_cont_preds_22],
             na.rm = TRUE))))) / 200
print(round(nor_means, 3))

cat("\n--- Average predictor means across all 200 datasets: Finland ---\n")
fin_means <- Reduce("+", lapply(seq_along(pv_cols), function(i)
  Reduce("+", lapply(1:20, function(j)
    colMeans(imp_datasets_fin[[i]][[j]][, final_cont_preds_22],
             na.rm = TRUE))))) / 200
print(round(fin_means, 3))

# --- Compare against observed means ---
cat("\n--- Observed predictor means (complete cases only): Norway ---\n")
print(round(colMeans(STU_NOR[, final_cont_preds_22], na.rm = TRUE), 3))

cat("\n--- Observed predictor means (complete cases only): Finland ---\n")
print(round(colMeans(STU_FIN[, final_cont_preds_22], na.rm = TRUE), 3))

# --- Row counts consistent across all datasets ---
cat("\n--- Row counts consistent: Norway ---\n")
nor_rows <- all(sapply(seq_along(pv_cols), function(i)
  sapply(1:20, function(j)
    nrow(imp_datasets_nor[[i]][[j]]) == nrow(STU_NOR))))
cat("All datasets have", nrow(STU_NOR), "rows:", nor_rows, "\n")

cat("\n--- Row counts consistent: Finland ---\n")
fin_rows <- all(sapply(seq_along(pv_cols), function(i)
  sapply(1:20, function(j)
    nrow(imp_datasets_fin[[i]][[j]]) == nrow(STU_FIN))))
cat("All datasets have", nrow(STU_FIN), "rows:", fin_rows, "\n")

# ==============================================================================
# Analysis script — PISA 2022
# Pooling structure:
#   Stage 1: MIcombine across m imputations within each PV
#   Stage 2: PV combination rules across pooled PV estimates
# ==============================================================================

# ------------------------------------------------------------------------------
# Helper functions — defined explicitly for self-contained 2022 script
# ------------------------------------------------------------------------------

add_replicate_weights <- function(imp_datasets, rw, w_reps) {
  lapply(imp_datasets, function(pv_list) {
    lapply(pv_list, function(dat) {
      stopifnot(nrow(dat) == nrow(rw))
      stopifnot(all(w_reps %in% names(rw)))
      if (any(w_reps %in% names(dat))) return(dat)
      cbind(dat, rw)
    })
  })
}

format_reg_results <- function(reg, country, cycle) {
  coefs  <- reg$coefficients
  se     <- reg$se
  z_vals <- coefs / se
  p_vals <- 2 * pnorm(-abs(z_vals))
  
  data.frame(
    cycle       = cycle,
    country     = country,
    variable    = names(coefs),
    coefficient = round(coefs, 4),
    se          = round(se, 4),
    z_value     = round(z_vals, 3),
    p_value     = round(p_vals, 4),
    significant = ifelse(p_vals < 0.05, "*", ""),
    row.names   = NULL
  )
}

# ------------------------------------------------------------------------------
# Step 1: Add replicate weights back to imputed datasets
# imp_datasets_nor and imp_datasets_fin are the 2022 MI outputs from Step 21
# ------------------------------------------------------------------------------

rw_nor_22 <- STU_NOR[, w_reps]
rw_fin_22 <- STU_FIN[, w_reps]

imp_datasets_nor_22 <- add_replicate_weights(imp_datasets_nor, rw_nor_22, w_reps)
imp_datasets_fin_22 <- add_replicate_weights(imp_datasets_fin, rw_fin_22, w_reps)

cat("\n--- Replicate weights added: 2022 ---\n")
cat("Norway columns:", ncol(imp_datasets_nor_22[[1]][[1]]), "\n")
cat("W_FSTURWT1 present:",
    "W_FSTURWT1" %in% names(imp_datasets_nor_22[[1]][[1]]), "\n")
cat("W_FSTURWT80 present:",
    "W_FSTURWT80" %in% names(imp_datasets_nor_22[[1]][[1]]), "\n")

# ------------------------------------------------------------------------------
# Step 2: Define regression formula and BRR design builder
# ------------------------------------------------------------------------------

reg_formula_22 <- PV_MATH ~ ESCS + LANGN + MATHEFF + ANXMAT + TEACHSUP

make_brr_design_22 <- function(dat) {
  svrepdesign(
    weights          = ~ W_FSTUWT,
    repweights       = as.matrix(dat[, w_reps]),
    data             = dat,
    type             = "BRR",
    fay.rho          = 0.5,
    combined.weights = TRUE,
    mse              = TRUE
  )
}

# ------------------------------------------------------------------------------
# Step 3: Nested two-stage regression pooling
# ------------------------------------------------------------------------------

pool_regression_nested_22 <- function(imp_datasets, formula, label) {
  
  cat("\n--- Running nested regression pipeline:", label, "---\n")
  
  Q <- length(imp_datasets)
  m <- length(imp_datasets[[1]])
  
  cat("PVs:", Q, "| Imputations per PV:", m, "\n")
  
  pv_coefs <- vector("list", Q)
  pv_vcovs <- vector("list", Q)
  
  for (i in seq_len(Q)) {
    cat("  Stage 1 — PV", i, "of", Q, ": fitting", m, "models\n")
    
    pv_models <- lapply(imp_datasets[[i]], function(dat) {
      design <- make_brr_design_22(dat)
      svyglm(formula, design = design, family = gaussian())
    })
    
    pooled_pv     <- MIcombine(pv_models)
    pv_coefs[[i]] <- coef(pooled_pv)
    pv_vcovs[[i]] <- vcov(pooled_pv)
  }
  
  # Verify coefficient names are consistent across all PVs
  stopifnot(all(sapply(pv_coefs, function(x)
    identical(names(x), names(pv_coefs[[1]])))))
  
  # Verify vcov column names are consistent across all PVs
  stopifnot(all(sapply(pv_vcovs, function(x)
    identical(colnames(x), colnames(pv_vcovs[[1]])))))
  
  cat("\n  Stage 2 — combining", Q, "PV estimates\n")
  
  coef_mat <- do.call(rbind, pv_coefs)
  var_mat  <- do.call(rbind, lapply(pv_vcovs, diag))
  
  final_coefs <- colMeans(coef_mat)
  avg_pv_var  <- colMeans(var_mat)
  between_var <- apply(coef_mat, 2, var)
  total_var   <- avg_pv_var + (1 + 1/Q) * between_var
  final_se    <- sqrt(total_var)
  
  cat("Nested pooling complete —", label, "\n")
  
  list(
    coefficients = final_coefs,
    se           = final_se,
    total_var    = total_var,
    avg_pv_var   = avg_pv_var,
    between_var  = between_var,
    n_pv         = Q,
    n_imp        = m
  )
}

reg_nor_22 <- pool_regression_nested_22(
  imp_datasets_nor_22, reg_formula_22, "Norway 2022"
)

reg_fin_22 <- pool_regression_nested_22(
  imp_datasets_fin_22, reg_formula_22, "Finland 2022"
)

# ------------------------------------------------------------------------------
# Step 4: Format regression results
# ------------------------------------------------------------------------------

reg_results_nor_22 <- format_reg_results(reg_nor_22, "NOR", 2022)
reg_results_fin_22 <- format_reg_results(reg_fin_22, "FIN", 2022)
reg_results_22     <- rbind(reg_results_nor_22, reg_results_fin_22)

cat("\n--- Pooled regression results: Norway 2022 ---\n")
print(reg_results_nor_22)

cat("\n--- Pooled regression results: Finland 2022 ---\n")
print(reg_results_fin_22)

# ------------------------------------------------------------------------------
# Step 5: Compute pooled weighted means for OBD endowments
# For OBD input, only pooled point estimates are needed.
# Weighted means are averaged across all PV x imputation datasets.
# No pooled variance or significance testing is computed for these means here.
# ------------------------------------------------------------------------------

compute_pooled_means_22 <- function(imp_datasets, cont_preds, label) {
  
  cat("\n--- Computing weighted means:", label, "---\n")
  
  all_means <- list()
  
  for (i in seq_along(imp_datasets)) {
    for (j in seq_along(imp_datasets[[i]])) {
      
      dat    <- imp_datasets[[i]][[j]]
      design <- make_brr_design_22(dat)
      
      cont_means <- as.numeric(
        svymean(as.formula(paste("~",
                                 paste(cont_preds, collapse = " + "))),
                design, na.rm = TRUE)
      )
      names(cont_means) <- cont_preds
      
      # Use names returned by svymean to avoid mislabelling
      langn_obj  <- svymean(~ LANGN, design, na.rm = TRUE)
      langn_mean <- as.numeric(langn_obj)
      names(langn_mean) <- names(coef(langn_obj))
      
      outcome_mean <- as.numeric(svymean(~ PV_MATH, design, na.rm = TRUE))
      names(outcome_mean) <- "PV_MATH"
      
      all_means[[length(all_means) + 1]] <-
        c(cont_means, langn_mean, outcome_mean)
    }
    cat("  PV", i, "complete\n")
  }
  
  means_mat    <- do.call(rbind, all_means)
  pooled_means <- colMeans(means_mat, na.rm = TRUE)
  
  cat("Means computed —", label, "\n")
  pooled_means
}

pooled_means_nor_22 <- compute_pooled_means_22(
  imp_datasets_nor_22, final_cont_preds_22, "Norway 2022"
)

pooled_means_fin_22 <- compute_pooled_means_22(
  imp_datasets_fin_22, final_cont_preds_22, "Finland 2022"
)

# Verify names are aligned before building table
stopifnot(identical(names(pooled_means_nor_22), names(pooled_means_fin_22)))

cat("\n--- Pooled weighted means: Norway 2022 ---\n")
print(round(pooled_means_nor_22, 4))

cat("\n--- Pooled weighted means: Finland 2022 ---\n")
print(round(pooled_means_fin_22, 4))

means_table_22 <- data.frame(
  variable           = names(pooled_means_nor_22),
  mean_NOR           = round(pooled_means_nor_22, 4),
  mean_FIN           = round(pooled_means_fin_22, 4),
  diff_NOR_minus_FIN = round(pooled_means_nor_22 - pooled_means_fin_22, 4),
  row.names          = NULL
)

cat("\n--- Means comparison table: 2022 ---\n")
print(means_table_22)

# ------------------------------------------------------------------------------
# Step 6: Export to CSV for Excel OBD calculation
# ------------------------------------------------------------------------------

write.csv(reg_results_22, "reg_results_2022.csv", row.names = FALSE)
write.csv(means_table_22, "means_table_2022.csv", row.names = FALSE)

cat("\n--- Files saved ---\n")
cat("reg_results_2022.csv\n")
cat("means_table_2022.csv\n")

# ==============================================================================
# Extract pooled weighted means WITH standard errors — PISA 2022
# Needed to test whether X_22 - X_12 is significant
# Pooling structure mirrors regression:
#   Stage 1: pool m imputations within each PV
#   Stage 2: combine across PVs
# ==============================================================================

compute_pooled_means_with_se <- function(imp_datasets, cont_preds, label) {
  
  cat("\n--- Computing pooled means with SEs:", label, "---\n")
  
  Q <- length(imp_datasets)
  m <- length(imp_datasets[[1]])
  
  # Storage: one row per PV, one column per variable
  pv_means_list <- vector("list", Q)
  pv_vars_list  <- vector("list", Q)
  
  for (i in seq_len(Q)) {
    
    # Within each PV: collect means and variances across m imputations
    imp_means <- matrix(NA, nrow = m, ncol = length(cont_preds) + 3)
    # columns: cont_preds + LANGN_Majority + LANGN_Other + PV_MATH
    
    for (j in seq_len(m)) {
      dat    <- imp_datasets[[i]][[j]]
      design <- make_brr_design_22(dat)
      
      # Continuous means
      cont_obj <- svymean(
        as.formula(paste("~", paste(cont_preds, collapse = " + "))),
        design, na.rm = TRUE
      )
      cont_est <- as.numeric(cont_obj)
      names(cont_est) <- cont_preds
      
      # LANGN
      langn_obj  <- svymean(~ LANGN, design, na.rm = TRUE)
      langn_est  <- as.numeric(langn_obj)
      names(langn_est) <- names(coef(langn_obj))
      
      # Outcome
      pv_est <- as.numeric(svymean(~ PV_MATH, design, na.rm = TRUE))
      names(pv_est) <- "PV_MATH"
      
      imp_means[j, ] <- c(cont_est, langn_est, pv_est)
    }
    
    var_names <- c(cont_preds, names(coef(langn_obj)), "PV_MATH")
    colnames(imp_means) <- var_names
    
    # Stage 1: pool across m imputations using Rubin's rules
    # Point estimate: mean across imputations
    pv_means_list[[i]] <- colMeans(imp_means)
    
    # Within-imputation variance: need SE from BRR design
    # Collect SEs across imputations
    imp_ses <- matrix(NA, nrow = m, ncol = length(var_names))
    colnames(imp_ses) <- var_names
    
    for (j in seq_len(m)) {
      dat    <- imp_datasets[[i]][[j]]
      design <- make_brr_design_22(dat)
      
      cont_obj   <- svymean(as.formula(paste("~", paste(cont_preds, collapse = " + "))), design, na.rm = TRUE)
      langn_obj  <- svymean(~ LANGN, design, na.rm = TRUE)
      pv_obj     <- svymean(~ PV_MATH, design, na.rm = TRUE)
      
      imp_ses[j, cont_preds]              <- SE(cont_obj)
      imp_ses[j, names(coef(langn_obj))]  <- SE(langn_obj)
      imp_ses[j, "PV_MATH"]              <- SE(pv_obj)
    }
    
    # Within-imputation variance = mean of squared SEs
    within_var <- colMeans(imp_ses^2)
    
    # Between-imputation variance
    between_var <- apply(imp_means, 2, var)
    
    # Total variance per Rubin's rules
    total_var <- within_var + (1 + 1/m) * between_var
    
    pv_vars_list[[i]] <- total_var
    
    cat("  PV", i, "complete\n")
  }
  
  # Stage 2: combine across Q PVs
  means_mat <- do.call(rbind, pv_means_list)
  vars_mat  <- do.call(rbind, pv_vars_list)
  
  final_means <- colMeans(means_mat)
  
  avg_pv_var  <- colMeans(vars_mat)
  between_var <- apply(means_mat, 2, var)
  total_var   <- avg_pv_var + (1 + 1/Q) * between_var
  final_se    <- sqrt(total_var)
  
  cat("Done —", label, "\n")
  
  data.frame(
    variable = names(final_means),
    mean     = round(final_means, 4),
    se       = round(final_se, 4),
    row.names = NULL
  )
}

# Run for Norway and Finland 2022
means_se_nor_22 <- compute_pooled_means_with_se(
  imp_datasets_nor_22, final_cont_preds_22, "Norway 2022"
)

means_se_fin_22 <- compute_pooled_means_with_se(
  imp_datasets_fin_22, final_cont_preds_22, "Finland 2022"
)

cat("\n--- Pooled means with SEs: Norway 2022 ---\n")
print(means_se_nor_22)

cat("\n--- Pooled means with SEs: Finland 2022 ---\n")
print(means_se_fin_22)

# Export
write.csv(means_se_nor_22, "means_se_nor_2022.csv", row.names = FALSE)
write.csv(means_se_fin_22, "means_se_fin_2022.csv", row.names = FALSE)
