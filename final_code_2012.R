# ==============================================================================
# MAE4191 MASTER'S THESIS PROJECT. PART 2 - PISA 2012 DATA ANALYSIS
# ==============================================================================

# ==============================================================================
# 0. Packages and session info
# ==============================================================================

library(haven)       # Read SPSS .sav files and handle labelled variables
library(dplyr)       # Data manipulation
library(tidyverse)   # Data manipulation extras (used in ESCS merge)
library(survey)      # Complex-survey designs (BRR/Fay)
library(mitools)     # MIcombine() for pooled estimates with Rubin's rules
library(naniar)      # Little's MCAR test
library(VIM)         # Missing-data visualisation utilities
library(mice)        # Multiple Imputation by Chained Equations
library(corrplot)    # Correlation matrix visualisation
library(car)         # VIF calculations
library(DescTools)   # Misc. descriptive utilities
library(EdSurvey)    # Educational-survey utilities
library(lsr)         # Effect-size helpers
library(gridExtra)   # Multi-panel plotting
library(ggplot2)     # Plotting
library(patchwork)   # Plot composition

packages <- c("survey", "mice", "mitools")
for (pkg in packages) {
  cat("\n===", pkg, "===\n")
  print(citation(pkg))
}
R.version.string # R version 4.5.3

# ==============================================================================
# 1. Load data
# ==============================================================================

# ------------------------------------------------------------------------------
# 1.1  Load PISA 2012 Student Questionnaire
# ------------------------------------------------------------------------------

# Source: PISA 2012 Student Questionnaire Data (OECD)
# Link: https://www.oecd.org/en/data/datasets/pisa-2012-database.html#data
# Original data: TXT format, converted to .sav using OECD's official SPSS
# control file (Syntax to read in student questionnaire data file).
# Data dimensions: 480,174 observations x 634 variables

setwd("C:/Users/elly2/Desktop/thesis") # IMPORTANT! Change to local working directory
INT_STU12_DEC03  <- read_sav("INT_STU12_DEC03.sav", user_na = TRUE)
MDATA_12_STU_QQQ <- INT_STU12_DEC03 # Save the original dataset as master copy

# ------------------------------------------------------------------------------
# 1.2  Merge rescaled ESCS for cross-cycle trend analysis
# ------------------------------------------------------------------------------

# The rescaled ESCS file is downloaded separately from the OECD 2022 database:
# https://www.oecd.org/en/data/datasets/pisa-2022-database.html#indices
# It contains ESCS recomputed using 2022 methodology for 2012, 2015 and 2018.

escs_trend <- read.csv("escs_trend.csv")

cat("--- escs_trend file structure ---\n")
head(escs_trend)
names(escs_trend)

# Filter to 2012 only (cycle == 5 in the trend file's coding)
escs_2012 <- escs_trend %>%
  filter(cycle == 5) %>%
  select(cnt, schoolid, studentid, escs_trend)

cat("Rows in escs_2012:", nrow(escs_2012), "\n")  # Expected: 453,304

# Harmonise ID formats for merging.
# escs_trend stores IDs as numeric (1, 2, 3, ...).
# PISA 2012 file stores IDs as zero-padded character strings:
# SCHOOLID = 7 characters (e.g., "0000001"); StIDStd = 5 characters (e.g., "00001").

escs_2012 <- escs_2012 %>%
  mutate(
    cnt      = toupper(cnt),                          # ensure uppercase to match CNT
    SCHOOLID = sprintf("%07d", as.integer(schoolid)), # pad to 7 characters
    StIDStd  = sprintf("%05d", as.integer(studentid)) # pad to 5 characters
  ) %>%
  select(cnt, SCHOOLID, StIDStd, escs_trend)

# Verify ID formats match before merging
cat("\nSample SCHOOLID from escs_2012:\n"); head(escs_2012$SCHOOLID)
cat("\nSample SCHOOLID from MDATA_12_STU_QQQ:\n"); head(MDATA_12_STU_QQQ$SCHOOLID[MDATA_12_STU_QQQ$CNT == "NOR"])
cat("\nSample StIDStd from escs_2012:\n"); head(escs_2012$StIDStd)
cat("\nSample StIDStd from MDATA_12_STU_QQQ:\n"); head(MDATA_12_STU_QQQ$StIDStd[MDATA_12_STU_QQQ$CNT == "NOR"])

# Merge rescaled ESCS into the master dataset (replaces original ESCS column)
MDATA_12_STU_QQQ <- MDATA_12_STU_QQQ %>%
  left_join(escs_2012, by = c("CNT" = "cnt", "SCHOOLID", "StIDStd")) %>%
  mutate(ESCS = escs_trend) %>%   # overwrite original ESCS with rescaled version
  select(-escs_trend)

# Verify merge quality
cat("\nRows after merge:", nrow(MDATA_12_STU_QQQ), "\n") # Should be unchanged
cat("Missing rescaled ESCS (overall):", sum(is.na(MDATA_12_STU_QQQ$ESCS)), "\n")
cat("Missing ESCS for NOR:", sum(is.na(MDATA_12_STU_QQQ$ESCS[MDATA_12_STU_QQQ$CNT == "NOR"])), "\n")
cat("Missing ESCS for FIN:", sum(is.na(MDATA_12_STU_QQQ$ESCS[MDATA_12_STU_QQQ$CNT == "FIN"])), "\n")

# ==============================================================================
# 2. Variable selection and country subsetting
# ==============================================================================

# ------------------------------------------------------------------------------
# 2.1  Define column groups
# ------------------------------------------------------------------------------

pv_cols     <- paste0("PV", 1:5, "MATH")                              # Math plausible values
w_reps      <- paste0("W_FSTR", 1:80)                                 # BRR replicate weights
design_vars <- c("CNT", "STRATUM", "SCHOOLID", "W_FSTUWT", "QuestID") # Design variables

# SCHOOLID in 2012 (CNTSCHID in 2022).
# QuestID indicates booklet form (rotated design).

# Notes on 2012-specific variable naming:
# DISCLIM   → DISCLIMA in 2012
# EXPOFA    → split into EXAPPLM and EXPUREM in 2012; both retained for diagnostics

cands <- c("ESCS", "IMMIG", "LANGN",         # Student background / demographics
           "MATHEFF", "ANXMAT",              # Learning dispositions (mathematics)
           "TEACHSUP", "DISCLIMA",           # School climate / learning environment
           "EXAPPLM", "EXPUREM")             # Opportunity to learn (2012 equivalents)

keep_cols <- unique(c(design_vars, cands, w_reps, pv_cols))

# ------------------------------------------------------------------------------
# 2.2  Subset by country
# ------------------------------------------------------------------------------

STU_NOR_12 <- MDATA_12_STU_QQQ %>% filter(CNT == "NOR") %>% select(all_of(keep_cols))
STU_FIN_12 <- MDATA_12_STU_QQQ %>% filter(CNT == "FIN") %>% select(all_of(keep_cols))

stopifnot(all(keep_cols %in% names(STU_NOR_12)))
stopifnot(all(keep_cols %in% names(STU_FIN_12)))
cat("NOR rows:", nrow(STU_NOR_12), "| FIN rows:", nrow(STU_FIN_12), "\n")
# Expected: NOR = 4,686 | FIN = 8,829

# ==============================================================================
# 3. Recoding
# ==============================================================================

# ------------------------------------------------------------------------------
# 3.0  Candidate-variable inspection before recoding
# ------------------------------------------------------------------------------

# Raw inspection only; output is not used downstream.

categorical_vars <- c("IMMIG", "LANGN")
continuous_vars  <- setdiff(cands, categorical_vars)

inspect_candidate <- function(data, var, country_label) {
  x <- data[[var]]
  
  cat("\n============================================================\n")
  cat("Variable:", var, "| Country:", country_label, "\n")
  cat("============================================================\n")
  
  cat("\nClass:", paste(class(x), collapse = ", "), "\n")
  cat("Type:", typeof(x), "\n")
  
  cat("\nSPSS value labels:\n")
  print(attr(x, "labels"))
  
  if (var %in% categorical_vars) {
    cat("\nObserved categories with labels and counts:\n")
    
    out <- data.frame(
      code  = as.character(x),
      label = as.character(haven::as_factor(x))
    ) |>
      dplyr::count(code, label, name = "n") |>
      dplyr::arrange(code)
    
    print(out, row.names = FALSE)
    
  } else {
    cat("\nSummary statistics:\n")
    
    x_num <- as.numeric(x)
    
    out <- data.frame(
      n_valid   = sum(!is.na(x_num)),
      n_missing = sum(is.na(x_num)),
      min       = min(x_num, na.rm = TRUE),
      q1        = quantile(x_num, 0.25, na.rm = TRUE),
      median    = median(x_num, na.rm = TRUE),
      mean      = mean(x_num, na.rm = TRUE),
      q3        = quantile(x_num, 0.75, na.rm = TRUE),
      max       = max(x_num, na.rm = TRUE),
      sd        = sd(x_num, na.rm = TRUE)
    )
    
    print(out, row.names = FALSE)
  }
}

for (v in cands) inspect_candidate(STU_NOR_12, v, "Norway")
for (v in cands) inspect_candidate(STU_FIN_12, v, "Finland")

# ------------------------------------------------------------------------------
# 3.1  Strip SPSS labels and recode missing-value sentinel codes to NA
# ------------------------------------------------------------------------------

# NOTE: read_sav(user_na = TRUE) imports SPSS user-defined missing values as
# tagged NAs (or honours na_range / na_values attributes). zap_missing() then
# converts these to standard NA. The additional sentinel recoding below
# safeguards against numeric missing codes that were not formally declared as
# user-missing in the SPSS file.

invalid_codes <- c(9, 99, 999, 9999, 99999, 999999, 9999999,   # Item-level non-response
                   7, 97, 997, 9997, 99997, 999997, 9999997,   # Not administered
                   8, 98, 998, 9998, 99998, 999998, 9999998)   # Multiple / invalid responses

recode_pisa <- function(df) {
  df %>%
    mutate(across(everything(), haven::zap_missing)) %>%
    mutate(across(everything(), haven::zap_labels)) %>%
    mutate(across(all_of(c(cands, pv_cols)),
                  ~ { x <- .x; x[!is.na(x) & x %in% invalid_codes] <- NA; x }))
}

STU_NOR_12 <- recode_pisa(STU_NOR_12)
STU_FIN_12 <- recode_pisa(STU_FIN_12)

# ------------------------------------------------------------------------------
# 3.2  Post-cleaning validation
# ------------------------------------------------------------------------------

# No SPSS-labelled variables remain
stopifnot(!any(sapply(STU_NOR_12, haven::is.labelled)))
stopifnot(!any(sapply(STU_FIN_12, haven::is.labelled)))

# No residual numeric sentinel codes remain in candidate variables or PVs
stopifnot(!any(sapply(c(cands, pv_cols), function(v)
  any(STU_NOR_12[[v]] %in% invalid_codes, na.rm = TRUE))))
stopifnot(!any(sapply(c(cands, pv_cols), function(v)
  any(STU_FIN_12[[v]] %in% invalid_codes, na.rm = TRUE))))

# IMMIG-specific check: only valid codes (1–3) and NA should remain
stopifnot(!any(STU_NOR_12$IMMIG %in% c(5, 7, 8, 9), na.rm = TRUE))
stopifnot(!any(STU_FIN_12$IMMIG %in% c(5, 7, 8, 9), na.rm = TRUE))

cat("\nIMMIG distribution after cleaning — Norway:\n")
print(table(STU_NOR_12$IMMIG, useNA = "ifany"))

cat("\nIMMIG distribution after cleaning — Finland:\n")
print(table(STU_FIN_12$IMMIG, useNA = "ifany"))

# ------------------------------------------------------------------------------
# 3.3  Recode LANGN to a binary factor
# ------------------------------------------------------------------------------

# LANGN distributions for Norway and Finland (labels and counts)
# attr(MDATA_12_STU_QQQ$LANGN, "labels")

langn_counts <- function(data, country) {
  x <- data$LANGN[data$CNT == country]
  
  data.frame(
    code  = as.character(x),
    label = as.character(haven::as_factor(x))
  ) |>
    dplyr::count(code, label, name = "n") |>
    dplyr::arrange(code)
}

langn_counts(MDATA_12_STU_QQQ, "NOR")
langn_counts(MDATA_12_STU_QQQ, "FIN")

# Check LANGN structure before binary recoding
class(STU_NOR_12$LANGN); typeof(STU_NOR_12$LANGN)
class(STU_FIN_12$LANGN); typeof(STU_FIN_12$LANGN)

# Norway: Norwegian (523) = Majority
STU_NOR_12 <- STU_NOR_12 %>%
  mutate(
    LANGN = factor(
      case_when(
        LANGN == "523" ~ "Majority",
        is.na(LANGN)   ~ NA_character_,
        TRUE           ~ "Other"
      ),
      levels = c("Majority", "Other")
    )
  )

# Finland: Finnish (420) and Swedish (494) = Majority
STU_FIN_12 <- STU_FIN_12 %>%
  mutate(
    LANGN = factor(
      case_when(
        LANGN %in% c("420", "494") ~ "Majority",
        is.na(LANGN)               ~ NA_character_,
        TRUE                       ~ "Other"
      ),
      levels = c("Majority", "Other")
    )
  )

# Verify LANGN recoding
cat("\nLANGN distribution — Norway:\n"); print(table(STU_NOR_12$LANGN, useNA = "ifany"))
# Expected: Majority = 4134, Other = 360 (11+9+28+312), NA = 192
cat("\nLANGN distribution — Finland:\n"); print(table(STU_FIN_12$LANGN, useNA = "ifany"))
# Expected: Majority = 7454 (6253+1201), Other = 1210, NA = 165

# ------------------------------------------------------------------------------
# 3.4  Recode IMMIG to a three-level factor
# ------------------------------------------------------------------------------

attr(MDATA_12_STU_QQQ$IMMIG, "labels")

# Raw IMMIG distributions for Norway and Finland
table(MDATA_12_STU_QQQ$IMMIG[MDATA_12_STU_QQQ$CNT == "NOR"])
table(MDATA_12_STU_QQQ$IMMIG[MDATA_12_STU_QQQ$CNT == "FIN"])

# Verify cleaned IMMIG distribution: only categories 1–3 and NA should remain
cat("\nIMMIG distribution — Norway:\n"); print(table(STU_NOR_12$IMMIG, useNA = "ifany"))
cat("\nIMMIG distribution — Finland:\n"); print(table(STU_FIN_12$IMMIG, useNA = "ifany"))

STU_NOR_12 <- STU_NOR_12 %>%
  mutate(
    IMMIG = factor(
      case_when(
        is.na(IMMIG) ~ NA_character_,
        TRUE         ~ as.character(IMMIG)
      ),
      levels = c("1", "2", "3"),
      labels = c("Native", "Second-generation", "First-generation")
    )
  )

STU_FIN_12 <- STU_FIN_12 %>%
  mutate(
    IMMIG = factor(
      case_when(
        is.na(IMMIG) ~ NA_character_,
        TRUE         ~ as.character(IMMIG)
      ),
      levels = c("1", "2", "3"),
      labels = c("Native", "Second-generation", "First-generation")
    )
  )

# Verify IMMIG recoding
cat("\nIMMIG distribution — Norway:\n"); print(table(STU_NOR_12$IMMIG, useNA = "ifany"))
# Expected: Native = 4113, Second-generation = 229, First-generation = 224, NA = 120
cat("\nIMMIG distribution — Finland:\n"); print(table(STU_FIN_12$IMMIG, useNA = "ifany"))
# Expected: Native = 7406, Second-generation = 583, First-generation = 687, NA = 153

# ------------------------------------------------------------------------------
# 3.5  Recode QuestID (booklet-form indicator, 2012-specific)
# ------------------------------------------------------------------------------

# QuestID distribution for Norway and Finland (labels and counts)
questid_counts <- function(data, country) {
  x <- data$QuestID[data$CNT == country]
  
  data.frame(
    code  = as.character(x),
    label = as.character(haven::as_factor(x))
  ) |>
    dplyr::count(code, label, name = "n") |>
    dplyr::arrange(code)
}

questid_counts(MDATA_12_STU_QQQ, "NOR")
questid_counts(MDATA_12_STU_QQQ, "FIN")

STU_NOR_12 <- STU_NOR_12 %>% mutate(QuestID = factor(QuestID))
STU_FIN_12 <- STU_FIN_12 %>% mutate(QuestID = factor(QuestID))

# Verify factor levels
cat("\nQuestID levels — Norway:",  levels(STU_NOR_12$QuestID), "\n")
cat("QuestID levels — Finland:",   levels(STU_FIN_12$QuestID), "\n")

# ==============================================================================
# 4. IMMIG vs LANGN — diagnostic comparison
# ==============================================================================

# Compares IMMIG and LANGN as alternative demographic predictors. Aim is to
# assess (1) overlap with ESCS and (2) association with mathematics performance.
#
# Both variables are converted to binary numeric indicators for this diagnostic:
#   LANGN: Majority = 1, Other = 0
#   IMMIG: Native   = 1, first-/second-generation = 0
#
# Weighted point estimates use the final student weight (W_FSTUWT) for
# descriptive purposes only. No inference is conducted at this stage. Full
# BRR/Fay variance estimation is applied from Section 6 onward.

weighted_cor <- function(x, y, w) {
  complete <- !is.na(x) & !is.na(y) & !is.na(w)
  x <- x[complete]; y <- y[complete]; w <- w[complete]
  if (length(x) < 3) return(NA_real_)
  wx <- w * (x - weighted.mean(x, w))
  wy <- w * (y - weighted.mean(y, w))
  sum(wx * wy) / sqrt(sum(wx^2) * sum(wy^2))
}

# Convert to binary numeric indicators for diagnostic purposes
immig_bin_nor_12 <- as.numeric(STU_NOR_12$IMMIG == "Native")
immig_bin_fin_12 <- as.numeric(STU_FIN_12$IMMIG == "Native")
langn_bin_nor_12 <- as.numeric(STU_NOR_12$LANGN == "Majority")
langn_bin_fin_12 <- as.numeric(STU_FIN_12$LANGN == "Majority")

# Correlations with ESCS (overlap with socioeconomic status)
cor_immig_escs_nor_12 <- weighted_cor(immig_bin_nor_12, STU_NOR_12$ESCS, STU_NOR_12$W_FSTUWT)
cor_langn_escs_nor_12 <- weighted_cor(langn_bin_nor_12, STU_NOR_12$ESCS, STU_NOR_12$W_FSTUWT)
cor_immig_escs_fin_12 <- weighted_cor(immig_bin_fin_12, STU_FIN_12$ESCS, STU_FIN_12$W_FSTUWT)
cor_langn_escs_fin_12 <- weighted_cor(langn_bin_fin_12, STU_FIN_12$ESCS, STU_FIN_12$W_FSTUWT)

# Correlations with mathematics performance (averaged across all 5 PVs)
cor_immig_math_nor_12 <- mean(sapply(pv_cols, function(pv)
  weighted_cor(immig_bin_nor_12, STU_NOR_12[[pv]], STU_NOR_12$W_FSTUWT)), na.rm = TRUE)
cor_langn_math_nor_12 <- mean(sapply(pv_cols, function(pv)
  weighted_cor(langn_bin_nor_12, STU_NOR_12[[pv]], STU_NOR_12$W_FSTUWT)), na.rm = TRUE)
cor_immig_math_fin_12 <- mean(sapply(pv_cols, function(pv)
  weighted_cor(immig_bin_fin_12, STU_FIN_12[[pv]], STU_FIN_12$W_FSTUWT)), na.rm = TRUE)
cor_langn_math_fin_12 <- mean(sapply(pv_cols, function(pv)
  weighted_cor(langn_bin_fin_12, STU_FIN_12[[pv]], STU_FIN_12$W_FSTUWT)), na.rm = TRUE)

# Direct overlap between IMMIG and LANGN
cor_immig_langn_nor_12 <- weighted_cor(immig_bin_nor_12, langn_bin_nor_12, STU_NOR_12$W_FSTUWT)
cor_immig_langn_fin_12 <- weighted_cor(immig_bin_fin_12, langn_bin_fin_12, STU_FIN_12$W_FSTUWT)

# Summary table
dem_comparison_12 <- data.frame(
  country       = c("NOR", "NOR", "FIN", "FIN"),
  variable      = c("IMMIG", "LANGN", "IMMIG", "LANGN"),
  cor_with_ESCS = round(abs(c(cor_immig_escs_nor_12, cor_langn_escs_nor_12,
                              cor_immig_escs_fin_12, cor_langn_escs_fin_12)), 3),
  cor_with_math = round(abs(c(cor_immig_math_nor_12, cor_langn_math_nor_12,
                              cor_immig_math_fin_12, cor_langn_math_fin_12)), 3)
)

cat("\n--- IMMIG vs LANGN diagnostic ---\n")
print(dem_comparison_12)

cat("\n--- Correlation between IMMIG and LANGN ---\n")
cat("NOR:", round(cor_immig_langn_nor_12, 3), "\n")
cat("FIN:", round(cor_immig_langn_fin_12, 3), "\n")

# Decision rule:
# Prefer the demographic variable that shows less overlap with ESCS while
# retaining a meaningful association with mathematics performance. Final
# choice (LANGN) is locked in at Section 9.

# ==============================================================================
# 5. PSU structure and stratum filtering
# ==============================================================================

# In the student-level file, PSUs correspond to sampled schools. In 2012 the
# school identifier is SCHOOLID (CNTSCHID is the 2022 equivalent). Strata
# containing fewer than two distinct PSUs cannot support BRR variance
# estimation and must be removed.

# Storage-type checks before filtering
class(STU_NOR_12$STRATUM);  typeof(STU_NOR_12$STRATUM)
class(STU_NOR_12$SCHOOLID); typeof(STU_NOR_12$SCHOOLID)
class(STU_FIN_12$STRATUM);  typeof(STU_FIN_12$STRATUM)
class(STU_FIN_12$SCHOOLID); typeof(STU_FIN_12$SCHOOLID)

psu_check_12 <- function(df) {
  df %>%
    group_by(STRATUM) %>%
    summarise(n_psu      = n_distinct(SCHOOLID),
              n_students = n(),
              .groups    = "drop") %>%
    arrange(n_psu, STRATUM)
}

psu_check_nor_12 <- psu_check_12(STU_NOR_12)
psu_check_fin_12 <- psu_check_12(STU_FIN_12)

cat("\n--- PSU count by stratum before filtering: Norway ---\n");  print(psu_check_nor_12)
cat("\n--- PSU count by stratum before filtering: Finland ---\n"); print(psu_check_fin_12)

cat("\nNorway: strata with <2 PSUs =",  sum(psu_check_nor_12$n_psu < 2), "\n")
cat("Finland: strata with <2 PSUs =",   sum(psu_check_fin_12$n_psu < 2), "\n")

# Function to remove strata with fewer than 2 distinct PSUs
remove_single_psu_12 <- function(df) {
  valid <- df %>%
    group_by(STRATUM) %>%
    summarise(n_psu = n_distinct(SCHOOLID), .groups = "drop") %>%
    filter(n_psu >= 2)
  
  out <- df %>% semi_join(valid, by = "STRATUM")
  
  message(
    unique(df$CNT), ": removed ", nrow(df) - nrow(out),
    " rows from strata with fewer than 2 PSUs. Remaining: ", nrow(out)
  )
  
  out
}

STU_NOR_12 <- remove_single_psu_12(STU_NOR_12)
STU_FIN_12 <- remove_single_psu_12(STU_FIN_12)

# Verification — every remaining stratum should now contain >= 2 distinct PSUs
psu_check_nor_12_after <- psu_check_12(STU_NOR_12)
psu_check_fin_12_after <- psu_check_12(STU_FIN_12)

cat("\n--- PSU count by stratum after filtering: Norway ---\n")
print(psu_check_nor_12_after, n = Inf)
cat("\n--- PSU count by stratum after filtering: Finland ---\n")
print(psu_check_fin_12_after, n = Inf)

cat("\nNorway: any strata with <2 PSUs after filtering? ",
    any(psu_check_nor_12_after$n_psu < 2), "\n")
cat("Finland: any strata with <2 PSUs after filtering? ",
    any(psu_check_fin_12_after$n_psu < 2), "\n")

# ==============================================================================
# 6. Build BRR survey designs
# ==============================================================================

# PISA 2012 uses Fay's modified BRR (rho = 0.5). Survey design objects are
# constructed using the final student weight (W_FSTUWT) and the 80 replicate
# weights (W_FSTR1–W_FSTR80; named W_FSTURWT1–W_FSTURWT80 in 2022).

# Check structure of the full weight and replicate weights
class(STU_NOR_12$W_FSTUWT); typeof(STU_NOR_12$W_FSTUWT)
class(STU_FIN_12$W_FSTUWT); typeof(STU_FIN_12$W_FSTUWT)

# Check that all replicate-weight columns are present
stopifnot(all(w_reps %in% names(STU_NOR_12)))
stopifnot(all(w_reps %in% names(STU_FIN_12)))

# Verify replicate weights are numeric
stopifnot(all(sapply(STU_NOR_12[, w_reps], is.numeric)))
stopifnot(all(sapply(STU_FIN_12[, w_reps], is.numeric)))

# Check for missing values in the final weight and replicate weights
cat("\nMissing values in W_FSTUWT — Norway:",  sum(is.na(STU_NOR_12$W_FSTUWT)), "\n")
cat("Missing values in W_FSTUWT — Finland:",   sum(is.na(STU_FIN_12$W_FSTUWT)), "\n")
cat("\nMissing values across BRR replicate weights — Norway:",
    sum(is.na(as.matrix(STU_NOR_12[, w_reps]))), "\n")
cat("Missing values across BRR replicate weights — Finland:",
    sum(is.na(as.matrix(STU_FIN_12[, w_reps]))), "\n")

# Function to build BRR survey design
make_design_12 <- function(df, reps) {
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
des_nor_12 <- make_design_12(STU_NOR_12, w_reps)
des_fin_12 <- make_design_12(STU_FIN_12, w_reps)

# Verify design objects
cat("\n--- Survey design summary: Norway ---\n");  print(summary(des_nor_12))
cat("\n--- Survey design summary: Finland ---\n"); print(summary(des_fin_12))

cat("\nWeighted mean ESCS — Norway:\n");  print(svymean(~ESCS, des_nor_12, na.rm = TRUE))
cat("\nWeighted mean ESCS — Finland:\n"); print(svymean(~ESCS, des_fin_12, na.rm = TRUE))

# ==============================================================================
# 7. Missingness analysis
# ==============================================================================

# ------------------------------------------------------------------------------
# 7.1  Weighted missingness per predictor
# ------------------------------------------------------------------------------

# Computes the weighted percentage of missing values for each predictor using
# the BRR survey design. Missingness is coded as a 0/1 indicator, so the
# weighted mean equals the weighted share of missing values.

weighted_missing_12 <- function(design, vars) {
  res <- sapply(vars, function(v) {
    f <- as.formula(paste0("~ I(as.numeric(is.na(", v, ")))"))
    m <- svymean(f, design = design, na.rm = TRUE)
    round(as.numeric(m) * 100, 2)
  })
  data.frame(variable    = vars,
             missing_pct = as.numeric(res),
             row.names   = NULL)
}

miss_nor_12 <- weighted_missing_12(des_nor_12, cands)
miss_fin_12 <- weighted_missing_12(des_fin_12, cands)

miss_summary_12 <- merge(miss_nor_12, miss_fin_12,
                         by = "variable", suffixes = c("_NOR", "_FIN"))

cat("\n--- Weighted missingness (%) by predictor ---\n")
print(miss_summary_12)

# Thresholds for interpretation:
# > 5% warrants discussion
# > 30% suggests exclusion unless strong substantive reason to retain

# ------------------------------------------------------------------------------
# 7.2  Booklet-aware missingness diagnostic (2012-specific)
# ------------------------------------------------------------------------------

# 2012 used a rotated booklet design: each questionnaire form (QuestID)
# carried only a subset of the non-cognitive scales. Cross-tabulating
# QuestID against missingness identifies which forms genuinely lacked which
# variables (missing-by-design) versus genuine item non-response.

# Extract QuestID from the master copy for Norway and Finland
questid_nor <- STU_NOR_12$QuestID
questid_fin <- STU_FIN_12$QuestID

# Booklet assignment vs ANXMAT missingness (illustrative)
cat("\n--- Booklet assignment vs ANXMAT missingness: Norway 2012 ---\n")
print(table(questid_nor, is.na(STU_NOR_12$ANXMAT)))

cat("\n--- Booklet assignment vs ANXMAT missingness: Finland 2012 ---\n")
print(table(questid_fin, is.na(STU_FIN_12$ANXMAT)))

# Document the analytic sample after accounting for booklet design.
# Forms 2 and 3 in Norway received ANXMAT items; in Finland, forms 2, 3, 5.
cat("\nStudents who received ANXMAT items — Norway 2012:",
    sum(questid_nor %in% c(2, 3)), "\n")
cat("Students who received ANXMAT items — Finland 2012:",
    sum(questid_fin %in% c(2, 3, 5)), "\n")

# Effective non-response rate among students who actually received the items
cat("\nGenuine non-response rate among booklet recipients (ANXMAT):\n")
cat("Norway:",
    round(sum(questid_nor == 2 & is.na(STU_NOR_12$ANXMAT)) +
            sum(questid_nor == 3 & is.na(STU_NOR_12$ANXMAT)), 0),
    "out of", sum(questid_nor %in% c(2, 3)), "=",
    round((sum(questid_nor == 2 & is.na(STU_NOR_12$ANXMAT)) +
             sum(questid_nor == 3 & is.na(STU_NOR_12$ANXMAT))) /
            sum(questid_nor %in% c(2, 3)) * 100, 2), "%\n")
cat("Finland:",
    round(sum(questid_fin %in% c(2, 3, 5) & is.na(STU_FIN_12$ANXMAT)), 0),
    "out of", sum(questid_fin %in% c(2, 3, 5)), "=",
    round(sum(questid_fin %in% c(2, 3, 5) & is.na(STU_FIN_12$ANXMAT)) /
            sum(questid_fin %in% c(2, 3, 5)) * 100, 2), "%\n")

# Booklet-vs-missingness summary for all rotated variables
rotated_vars <- c("MATHEFF", "ANXMAT", "TEACHSUP", "DISCLIMA", "EXAPPLM", "EXPUREM")

cat("\n--- Booklet vs missingness summary: Norway 2012 ---\n")
for (v in rotated_vars) {
  tab <- table(questid_nor, is.na(STU_NOR_12[[v]]))
  cat(v, "— missing by form:", paste(tab[, "TRUE"], collapse = " / "), "\n")
}

cat("\n--- Booklet vs missingness summary: Finland 2012 ---\n")
for (v in rotated_vars) {
  tab <- table(questid_fin, is.na(STU_FIN_12[[v]]))
  cat(v, "— missing by form:", paste(tab[, "TRUE"], collapse = " / "), "\n")
}

# ------------------------------------------------------------------------------
# 7.3  Complete-case summary (unweighted and weighted)
# ------------------------------------------------------------------------------

# Documents how casewise deletion would affect the analytic sample. In 2012,
# the booklet-rotation design means complete-case analysis would discard the
# vast majority of observations (no student saw all questionnaire constructs),
# providing strong justification for multiple imputation.

stopifnot(all(cands %in% names(STU_NOR_12)))
stopifnot(all(cands %in% names(STU_FIN_12)))

cat("\n--- Predictors included in complete-case check: 2012 ---\n")
print(cands)
cat("\nRows in STU_NOR_12:", nrow(STU_NOR_12), "\n")
cat("Rows in STU_FIN_12:", nrow(STU_FIN_12), "\n")

cat("\n--- Unweighted missing counts by predictor: Norway 2012 ---\n")
print(colSums(is.na(STU_NOR_12[, cands])))
cat("\n--- Unweighted missing counts by predictor: Finland 2012 ---\n")
print(colSums(is.na(STU_FIN_12[, cands])))

complete_cases_summary_12 <- function(df, vars) {
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

cc_table_12 <- bind_rows(
  complete_cases_summary_12(STU_NOR_12, cands),
  complete_cases_summary_12(STU_FIN_12, cands)
)

cat("\n--- Complete cases summary (unweighted analytic sample): 2012 ---\n")
print(cc_table_12)
stopifnot(all(cc_table_12$n_complete + cc_table_12$n_dropped == cc_table_12$n_total))

# Add complete-case flag and rebuild design objects for weighted summary
add_complete_case_flag_12 <- function(df, vars) {
  df$cc_all <- as.numeric(complete.cases(df[, vars]))
  df
}

STU_NOR_12 <- add_complete_case_flag_12(STU_NOR_12, cands)
STU_FIN_12 <- add_complete_case_flag_12(STU_FIN_12, cands)

des_nor_12 <- make_design_12(STU_NOR_12, w_reps)
des_fin_12 <- make_design_12(STU_FIN_12, w_reps)

weighted_complete_case_summary_12 <- function(design, country_label) {
  m <- svymean(~cc_all, design = design, na.rm = TRUE)
  data.frame(
    country               = country_label,
    pct_complete_weighted = round(as.numeric(m) * 100, 2),
    pct_dropped_weighted  = round(100 - as.numeric(m) * 100, 2),
    row.names             = NULL
  )
}

cc_weighted_12 <- bind_rows(
  weighted_complete_case_summary_12(des_nor_12, "NOR"),
  weighted_complete_case_summary_12(des_fin_12, "FIN")
)

cc_summary_full_12 <- merge(cc_table_12, cc_weighted_12, by = "country")
cat("\n--- Complete cases summary (unweighted + weighted): 2012 ---\n")
print(cc_summary_full_12)

# ------------------------------------------------------------------------------
# 7.4  MAR plausibility — survey-weighted logistic regressions of missingness
# ------------------------------------------------------------------------------

# Models a binary missingness indicator (1 = missing, 0 = observed) for each
# variable with notable missingness, regressed on observed covariates
# (ESCS, IMMIG, LANGN, QuestID). For the rotated variables in 2012, QuestID
# captures most of the missingness pattern by design.

miss_vars_12     <- cands[!cands %in% c("ESCS", "IMMIG", "LANGN")]
mis_predictors_12 <- c("ESCS", "IMMIG", "LANGN", "QuestID")

stopifnot(all(miss_vars_12 %in% names(STU_NOR_12)))
stopifnot(all(miss_vars_12 %in% names(STU_FIN_12)))
stopifnot(all(mis_predictors_12 %in% names(STU_NOR_12)))
stopifnot(all(mis_predictors_12 %in% names(STU_FIN_12)))

# Add missingness indicators to each dataset
add_missing_indicators_12 <- function(df, vars) {
  for (v in vars) df[[paste0("mis_", v)]] <- as.numeric(is.na(df[[v]]))
  df
}

STU_NOR_12 <- add_missing_indicators_12(STU_NOR_12, miss_vars_12)
STU_FIN_12 <- add_missing_indicators_12(STU_FIN_12, miss_vars_12)

# Rebuild survey designs so the new indicators are visible
des_nor_12 <- make_design_12(STU_NOR_12, w_reps)
des_fin_12 <- make_design_12(STU_FIN_12, w_reps)

cat("\n--- Missingness indicator checks: Norway 2012 ---\n")
print(colSums(STU_NOR_12[, paste0("mis_", miss_vars_12)]))
cat("\n--- Missingness indicator checks: Finland 2012 ---\n")
print(colSums(STU_FIN_12[, paste0("mis_", miss_vars_12)]))

# Function to run weighted logistic regressions of missingness
run_missingness_models_12 <- function(design, miss_vars, predictors, country_label) {
  lapply(setNames(miss_vars, paste0(country_label, "_", miss_vars)), function(v) {
    preds_v <- setdiff(predictors, v)
    f <- as.formula(paste0("mis_", v, " ~ ", paste(preds_v, collapse = " + ")))
    svyglm(f, design = design, family = quasibinomial())
  })
}

mods_mis_nor_12 <- run_missingness_models_12(des_nor_12, miss_vars_12, mis_predictors_12, "NOR")
mods_mis_fin_12 <- run_missingness_models_12(des_fin_12, miss_vars_12, mis_predictors_12, "FIN")

cat("\n==============================\n")
cat("Weighted missingness models — Norway 2012\n")
cat("==============================\n")
for (nm in names(mods_mis_nor_12)) {
  cat("\n---", nm, "---\n")
  print(summary(mods_mis_nor_12[[nm]]))
}

cat("\n==============================\n")
cat("Weighted missingness models — Finland 2012\n")
cat("==============================\n")
for (nm in names(mods_mis_fin_12)) {
  cat("\n---", nm, "---\n")
  print(summary(mods_mis_fin_12[[nm]]))
}

# ------------------------------------------------------------------------------
# 7.5  Little's MCAR test (supplementary, unweighted)
# ------------------------------------------------------------------------------

# Reported only as a supplementary diagnostic. Does not account for the
# complex survey design or weights. In 2012 specifically, a significant
# Little's test result reflects the systematic booklet-driven missing pattern
# rather than student-characteristic-driven non-response. Little's test cannot
# distinguish missing-by-design (MCAR by random booklet assignment) from
# missing-by-student-characteristics (MAR). The booklet diagnostics in
# Section 7.2 provide stronger and more direct evidence that missingness in
# the rotated variables is MCAR by design.

little_data_nor_12 <- STU_NOR_12[, cands]
little_data_fin_12 <- STU_FIN_12[, cands]

little_data_nor_12[] <- lapply(little_data_nor_12,
                               function(x) if (is.factor(x)) as.numeric(x) else x)
little_data_fin_12[] <- lapply(little_data_fin_12,
                               function(x) if (is.factor(x)) as.numeric(x) else x)

cat("\n--- Little's MCAR test: Norway 2012 ---\n");  print(mcar_test(little_data_nor_12))
cat("\n--- Little's MCAR test: Finland 2012 ---\n"); print(mcar_test(little_data_fin_12))

# ==============================================================================
# 8. Pre-imputation predictor screening
# ==============================================================================

# ------------------------------------------------------------------------------
# 8.1  Weighted inter-predictor correlation matrix
# ------------------------------------------------------------------------------

# Identifies highly correlated predictor pairs (|r| > .80) as a multicollinearity
# screen.
#
# 2012-specific note: svyvar() fails on the rotated questionnaire constructs
# because no student has complete data on all of them simultaneously
# (~35% missing-by-design on each rotated variable). Pairwise weighted_cor()
# is used instead — each pair is computed on its own complete-case set.

cont_cands_12 <- cands[!cands %in% c("IMMIG", "LANGN")]

cat("\n--- Continuous predictors included in correlation matrix: 2012 ---\n")
print(cont_cands_12)

# Build pairwise weighted correlation matrix
pairwise_wcor_matrix <- function(df, vars, weight_var) {
  n   <- length(vars)
  mat <- matrix(NA, n, n, dimnames = list(vars, vars))
  diag(mat) <- 1
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      r <- weighted_cor(as.numeric(df[[vars[i]]]),
                        as.numeric(df[[vars[j]]]),
                        df[[weight_var]])
      mat[i, j] <- r
      mat[j, i] <- r
    }
  }
  mat
}

wcor_nor_12 <- pairwise_wcor_matrix(STU_NOR_12, cont_cands_12, "W_FSTUWT")
wcor_fin_12 <- pairwise_wcor_matrix(STU_FIN_12, cont_cands_12, "W_FSTUWT")

cat("\n--- Weighted predictor correlations: Norway 2012 ---\n");  print(round(wcor_nor_12, 3))
cat("\n--- Weighted predictor correlations: Finland 2012 ---\n"); print(round(wcor_fin_12, 3))

# Visualise correlation matrices
plot_weighted_corr_12 <- function(cor_mat, country_label) {
  corrplot::corrplot(
    cor_mat,
    method      = "color",
    type        = "upper",
    addCoef.col = "black",
    number.cex  = 0.75,
    tl.col      = "black",
    tl.cex      = 0.9,
    tl.srt      = 45,
    col         = corrplot::COL2("RdBu", 200),
    cl.cex      = 0.8,
    diag        = TRUE,
    mar         = c(0, 0, 3, 0),
    title       = paste0(country_label,
                         "\nWeighted inter-predictor correlations")
  )
}

plot_weighted_corr_12(wcor_nor_12, "Norway, PISA 2012")
plot_weighted_corr_12(wcor_fin_12, "Finland, PISA 2012")

# Flag highly correlated predictor pairs
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

cat("\n--- Predictor pairs with |r| > .80: Norway 2012 ---\n");  print(find_high_corr_pairs(wcor_nor_12))
cat("\n--- Predictor pairs with |r| > .80: Finland 2012 ---\n"); print(find_high_corr_pairs(wcor_fin_12))

# ------------------------------------------------------------------------------
# 8.2  VIF (unweighted, diagnostic only) — booklet-aware
# ------------------------------------------------------------------------------

# Directional multicollinearity screen using PV1MATH as a proxy outcome.
#
# 2012-specific note: due to the rotated booklet design, no student has
# complete data on all candidate predictors simultaneously. VIF is therefore
# computed separately for each booklet form's available variable subset using
# the relevant student subsample.
#
# Interpretation:
#   VIF > 5  warrants attention
#   VIF > 10 problematic

predictors_screen_12 <- c("ESCS", "IMMIG", "LANGN",
                          cont_cands_12[cont_cands_12 != "ESCS"])

cat("\n--- Provisional predictors for VIF screening: 2012 ---\n")
print(predictors_screen_12)

stopifnot(all(predictors_screen_12 %in% names(STU_NOR_12)))
stopifnot(all(predictors_screen_12 %in% names(STU_FIN_12)))

# Form 2 subsample: MATHEFF, ANXMAT, TEACHSUP, DISCLIMA available
# (Form 1 students do not receive these — exclude Form 1)
form2_vars_nor <- !is.na(STU_NOR_12$MATHEFF) & !is.na(STU_NOR_12$ANXMAT)
form2_vars_fin <- !is.na(STU_FIN_12$MATHEFF) & !is.na(STU_FIN_12$ANXMAT)

vif_formula_form2 <- as.formula(
  paste("PV1MATH ~ ESCS + IMMIG + LANGN + MATHEFF + ANXMAT + TEACHSUP + DISCLIMA")
)

cat("\n--- VIF (Form 2 subsample — MATHEFF/ANXMAT/TEACHSUP/DISCLIMA): Norway 2012 ---\n")
print(round(car::vif(lm(vif_formula_form2, data = STU_NOR_12[form2_vars_nor, ])), 2))

cat("\n--- VIF (Form 2 subsample — MATHEFF/ANXMAT/TEACHSUP/DISCLIMA): Finland 2012 ---\n")
print(round(car::vif(lm(vif_formula_form2, data = STU_FIN_12[form2_vars_fin, ])), 2))

# Form 1 subsample: EXAPPLM, EXPUREM available
# (Form 2 and Form 3 students do not receive these — exclude them)
form1_vars_nor <- !is.na(STU_NOR_12$EXAPPLM)
form1_vars_fin <- !is.na(STU_FIN_12$EXAPPLM)

vif_formula_form1 <- as.formula(
  paste("PV1MATH ~ ESCS + IMMIG + LANGN + EXAPPLM + EXPUREM")
)

cat("\n--- VIF (Form 1 subsample — EXAPPLM/EXPUREM): Norway 2012 ---\n")
print(round(car::vif(lm(vif_formula_form1, data = STU_NOR_12[form1_vars_nor, ])), 2))

cat("\n--- VIF (Form 1 subsample — EXAPPLM/EXPUREM): Finland 2012 ---\n")
print(round(car::vif(lm(vif_formula_form1, data = STU_FIN_12[form1_vars_fin, ])), 2))

# ------------------------------------------------------------------------------
# 8.3  Country-level entirely-missing check
# ------------------------------------------------------------------------------

# Defensive check: ensures no candidate predictor is entirely missing within
# either country sample (would block imputation).

entirely_missing <- function(df, vars) {
  sapply(vars, function(v) all(is.na(df[[v]])))
}

cat("\n--- Entirely missing: Norway 2012 ---\n");  print(entirely_missing(STU_NOR_12, predictors_screen_12))
cat("\n--- Entirely missing: Finland 2012 ---\n"); print(entirely_missing(STU_FIN_12, predictors_screen_12))

cat("\nProvisional predictor set retained for now: 2012\n")
print(predictors_screen_12)

# ==============================================================================
# 9. Final predictor set
# ==============================================================================

# Locks in the predictors used for multiple imputation and the Oaxaca–Blinder
# decomposition. Justifications for each choice are documented in the methods
# chapter:
#   - ESCS (rescaled to 2022 metric), MATHEFF, ANXMAT, TEACHSUP retained as core predictors.
#   - LANGN preferred over IMMIG: lower overlap with ESCS while retaining a
#     meaningful association with mathematics performance, and consistent
#     across both countries (cf. Section 4).
#   - DISCLIMA, EXAPPLM, EXPUREM dropped (low predictive value / overlap /
#     additional booklet-rotation complications).
#   - IMMIG dropped to avoid multicollinearity with LANGN.

final_preds_12      <- c("ESCS", "LANGN", "MATHEFF", "ANXMAT", "TEACHSUP")
final_cont_preds_12 <- final_preds_12[!final_preds_12 %in% "LANGN"]

stopifnot(all(final_preds_12 %in% names(STU_NOR_12)))
stopifnot(all(final_preds_12 %in% names(STU_FIN_12)))

cat("\n--- Final predictor set: 2012 ---\n"); print(final_preds_12)
cat("\nFinal continuous predictors:\n");      print(final_cont_preds_12)
cat("\nFinal sample sizes:\n")
cat("Norway:",  nrow(STU_NOR_12), "\n")
cat("Finland:", nrow(STU_FIN_12), "\n")

# ==============================================================================
# 10. Multiple imputation (MICE)
# ==============================================================================
#
# Strategy
# --------
# For each of the 5 plausible values, a separate dataset is constructed in
# which that PV is renamed PV_MATH. mice() is run on each PV-specific dataset
# with m = 40 imputations, yielding 5 × 40 = 200 imputed datasets per country.
# (m = 40 used here vs m = 20 in 2022 to compensate for the larger missing
# fraction induced by the booklet rotation.)
#
# Configuration
# -------------
#   PV_MATH       — included as a predictor but not imputed (plausible values
#                   are treated as fixed draws from the posterior distribution).
#   W_FSTUWT      — excluded from the imputation model.
#   QuestID       — handled in a sensitivity analysis (see Section 10.2):
#                   imputation is run both with and without QuestID as an
#                   auxiliary predictor.
#   LANGN         — imputed via logistic regression (binary factor).
#   ESCS, MATHEFF,
#   ANXMAT,
#   TEACHSUP      — imputed via predictive mean matching (PMM).
#
# Sensitivity analysis (2012-specific)
# ------------------------------------
# Two parallel MI branches are run:
#   Branch A (withQ): QuestID is included as a predictor in the imputation
#                     model. Theoretically this helps mice() exploit the
#                     systematic booklet-rotation pattern.
#   Branch B (noQ):   QuestID is excluded. Provides a benchmark that does not
#                     rely on QuestID for plausibility.
# Branches are compared on convergence and downstream stability before the
# final analysis dataset is selected (see Section 10.6).

# ------------------------------------------------------------------------------
# 10.1  Helper functions for MI pipeline
# ------------------------------------------------------------------------------

build_imp_config <- function(data, cont_preds, factor_preds,
                             predictor_only_vars = "PV_MATH",
                             weight_col = "W_FSTUWT") {
  pred <- mice::make.predictorMatrix(data)
  
  # Exclude survey weight from imputation entirely
  if (weight_col %in% colnames(pred)) {
    pred[weight_col, ] <- 0
    pred[, weight_col] <- 0
  }
  
  # Variables used only as predictors, not themselves imputed
  for (v in predictor_only_vars) {
    if (v %in% colnames(pred)) pred[v, ] <- 0
  }
  
  meth <- rep("", ncol(data))
  names(meth) <- colnames(data)
  meth[cont_preds]          <- "pmm"
  meth[factor_preds]        <- "logreg"
  meth[predictor_only_vars] <- ""
  meth[weight_col]          <- ""
  
  list(method = meth, predictorMatrix = pred)
}

make_pv_datasets <- function(df, pv_cols, base_cols) {
  lapply(pv_cols, function(pv) {
    dat <- df[, c(base_cols, pv)]
    names(dat)[names(dat) == pv] <- "PV_MATH"
    dat
  })
}

extract_imputed <- function(imp_list, pv_cols, m) {
  lapply(seq_along(pv_cols), function(i) {
    lapply(1:m, function(j) {
      dat <- complete(imp_list[[i]], j)
      dat$pv_index  <- i
      dat$imp_index <- j
      dat
    })
  })
}

# Wrapper that runs the full MI pipeline (test run + full imputation +
# validation diagnostics) for one branch of the sensitivity analysis.
run_mi_block <- function(pv_data_nor, pv_data_fin, pv_cols, config,
                         label = "withQ", m_test = 5, maxit_test = 5,
                         m_full = 40, maxit_full = 20, seed_base = 2012) {
  
  cat("\n====================================================\n")
  cat("Running MI branch:", label, "\n")
  cat("====================================================\n")
  
  # ---- Quick test run ------------------------------------------------------
  
  cat("\n--- Test run:", label, "| PV1, Norway 2012, m =", m_test,
      ", maxit =", maxit_test, "---\n")
  set.seed(seed_base)
  test_imp_nor <- mice(pv_data_nor[[1]],
                       m = m_test, maxit = maxit_test,
                       method          = config$method,
                       predictorMatrix = config$predictorMatrix,
                       printFlag       = TRUE)
  cat("Norway test passed — no fatal errors\n")
  
  cat("\n--- Test run:", label, "| PV1, Finland 2012, m =", m_test,
      ", maxit =", maxit_test, "---\n")
  set.seed(seed_base)
  test_imp_fin <- mice(pv_data_fin[[1]],
                       m = m_test, maxit = maxit_test,
                       method          = config$method,
                       predictorMatrix = config$predictorMatrix,
                       printFlag       = TRUE)
  cat("Finland test passed — no fatal errors\n")
  
  cat("\n--- Logged events: Norway |", label, "---\n"); print(test_imp_nor$loggedEvents)
  cat("\n--- Logged events: Finland |", label, "---\n"); print(test_imp_fin$loggedEvents)
  
  # ---- Full imputation — Norway --------------------------------------------
  
  cat("\n--- Full imputation:", label, "| Norway 2012 (",
      length(pv_cols), " PVs x m =", m_full, ") ---\n")
  
  imp_nor_list <- vector("list", length(pv_cols))
  names(imp_nor_list) <- pv_cols
  
  for (i in seq_along(pv_cols)) {
    cat("\n  Norway 2012 |", label, "| PV", i, "of", length(pv_cols), "\n")
    set.seed(seed_base + i)
    imp_nor_list[[i]] <- mice(pv_data_nor[[i]],
                              m = m_full, maxit = maxit_full,
                              method          = config$method,
                              predictorMatrix = config$predictorMatrix,
                              printFlag       = FALSE)
  }
  
  cat("\nNorway imputation complete —", label, "\n")
  cat("Imputed datasets:", length(imp_nor_list) * m_full, "\n")
  
  # ---- Full imputation — Finland -------------------------------------------
  
  cat("\n--- Full imputation:", label, "| Finland 2012 (",
      length(pv_cols), " PVs x m =", m_full, ") ---\n")
  
  imp_fin_list <- vector("list", length(pv_cols))
  names(imp_fin_list) <- pv_cols
  
  for (i in seq_along(pv_cols)) {
    cat("\n  Finland 2012 |", label, "| PV", i, "of", length(pv_cols), "\n")
    set.seed(seed_base + i)
    imp_fin_list[[i]] <- mice(pv_data_fin[[i]],
                              m = m_full, maxit = maxit_full,
                              method          = config$method,
                              predictorMatrix = config$predictorMatrix,
                              printFlag       = FALSE)
  }
  
  cat("\nFinland imputation complete —", label, "\n")
  cat("Imputed datasets:", length(imp_fin_list) * m_full, "\n")
  
  # ---- Validation: convergence and density diagnostics ---------------------
  
  cat("\n--- Convergence plots: Norway 2012 (PV1) |", label, "---\n")
  plot(imp_nor_list[[1]],
       main = paste("Convergence diagnostics — Norway 2012 (PV1) |", label))
  
  cat("\n--- Convergence plots: Finland 2012 (PV1) |", label, "---\n")
  plot(imp_fin_list[[1]],
       main = paste("Convergence diagnostics — Finland 2012 (PV1) |", label))
  
  cat("\n--- Density plots: Norway 2012 (PV1) |", label, "---\n")
  for (v in c("ESCS", "MATHEFF", "ANXMAT", "TEACHSUP")) {
    cat("\nDensity plot for", v, "— Norway 2012 |", label, "\n")
    print(densityplot(imp_nor_list[[1]], as.formula(paste("~", v))))
  }
  
  cat("\n--- Density plots: Finland 2012 (PV1) |", label, "---\n")
  for (v in c("ESCS", "MATHEFF", "ANXMAT", "TEACHSUP")) {
    cat("\nDensity plot for", v, "— Finland 2012 |", label, "\n")
    print(densityplot(imp_fin_list[[1]], as.formula(paste("~", v))))
  }
  
  # ---- LANGN imputed proportions (sanity check) ----------------------------
  
  cat("\n--- LANGN imputed proportions: Norway |", label, "---\n")
  nor_langn_props <- sapply(1:m_full, function(j)
    prop.table(table(complete(imp_nor_list[[1]], j)$LANGN)))
  print(round(nor_langn_props, 3))
  cat("\n--- LANGN observed proportion: Norway ---\n")
  print(round(prop.table(table(STU_NOR_12$LANGN, useNA = "no")), 3))
  
  cat("\n--- LANGN imputed proportions: Finland |", label, "---\n")
  fin_langn_props <- sapply(1:m_full, function(j)
    prop.table(table(complete(imp_fin_list[[1]], j)$LANGN)))
  print(round(fin_langn_props, 3))
  cat("\n--- LANGN observed proportion: Finland ---\n")
  print(round(prop.table(table(STU_FIN_12$LANGN, useNA = "no")), 3))
  
  # ---- Zero-missingness check across all imputed datasets ------------------
  
  cat("\n--- Missing values after imputation: Norway |", label, "---\n")
  nor_missing <- sapply(seq_along(pv_cols), function(i) {
    sapply(1:m_full, function(j)
      sum(is.na(complete(imp_nor_list[[i]], j)[, final_preds_12])))
  })
  colnames(nor_missing) <- pv_cols
  rownames(nor_missing) <- paste0("imp_", 1:m_full)
  print(nor_missing)
  cat("All zero?", all(nor_missing == 0), "\n")
  
  cat("\n--- Missing values after imputation: Finland |", label, "---\n")
  fin_missing <- sapply(seq_along(pv_cols), function(i) {
    sapply(1:m_full, function(j)
      sum(is.na(complete(imp_fin_list[[i]], j)[, final_preds_12])))
  })
  colnames(fin_missing) <- pv_cols
  rownames(fin_missing) <- paste0("imp_", 1:m_full)
  print(fin_missing)
  cat("All zero?", all(fin_missing == 0), "\n")
  
  # ---- Extract imputed datasets --------------------------------------------
  
  imp_datasets_nor <- extract_imputed(imp_nor_list, pv_cols, m = m_full)
  imp_datasets_fin <- extract_imputed(imp_fin_list, pv_cols, m = m_full)
  
  cat("\n--- Imputed datasets extracted |", label, "---\n")
  cat("Norway:  ", length(imp_datasets_nor), "PVs x",
      length(imp_datasets_nor[[1]]), "imputations =",
      length(imp_datasets_nor) * length(imp_datasets_nor[[1]]), "total datasets\n")
  cat("Finland: ", length(imp_datasets_fin), "PVs x",
      length(imp_datasets_fin[[1]]), "imputations =",
      length(imp_datasets_fin) * length(imp_datasets_fin[[1]]), "total datasets\n")
  
  # ---- Column-structure consistency ----------------------------------------
  
  cat("\n--- Column names (should be identical across all datasets) |", label, "---\n")
  print(names(imp_datasets_nor[[1]][[1]]))
  
  nor_cols_consistent <- all(sapply(seq_along(pv_cols), function(i)
    sapply(1:m_full, function(j)
      identical(names(imp_datasets_nor[[i]][[j]]),
                names(imp_datasets_nor[[1]][[1]])))))
  
  fin_cols_consistent <- all(sapply(seq_along(pv_cols), function(i)
    sapply(1:m_full, function(j)
      identical(names(imp_datasets_fin[[i]][[j]]),
                names(imp_datasets_nor[[1]][[1]])))))
  
  cat("Column structure consistent — Norway:",  nor_cols_consistent, "\n")
  cat("Column structure consistent — Finland:", fin_cols_consistent, "\n")
  
  # ---- Predictor means across all 200 datasets vs observed -----------------
  
  cat("\n--- Average predictor means across all datasets: Norway |", label, "---\n")
  nor_means <- Reduce("+", lapply(seq_along(pv_cols), function(i)
    Reduce("+", lapply(1:m_full, function(j)
      colMeans(imp_datasets_nor[[i]][[j]][, final_cont_preds_12], na.rm = TRUE))))) /
    (length(pv_cols) * m_full)
  print(round(nor_means, 3))
  
  cat("\n--- Average predictor means across all datasets: Finland |", label, "---\n")
  fin_means <- Reduce("+", lapply(seq_along(pv_cols), function(i)
    Reduce("+", lapply(1:m_full, function(j)
      colMeans(imp_datasets_fin[[i]][[j]][, final_cont_preds_12], na.rm = TRUE))))) /
    (length(pv_cols) * m_full)
  print(round(fin_means, 3))
  
  cat("\n--- Observed predictor means (available cases): Norway ---\n")
  print(round(colMeans(STU_NOR_12[, final_cont_preds_12], na.rm = TRUE), 3))
  cat("\n--- Observed predictor means (available cases): Finland ---\n")
  print(round(colMeans(STU_FIN_12[, final_cont_preds_12], na.rm = TRUE), 3))
  
  # ---- Row-count consistency ----------------------------------------------
  
  cat("\n--- Row counts consistent: Norway |", label, "---\n")
  nor_rows <- all(sapply(seq_along(pv_cols), function(i)
    sapply(1:m_full, function(j) nrow(imp_datasets_nor[[i]][[j]]) == nrow(STU_NOR_12))))
  cat("All datasets have", nrow(STU_NOR_12), "rows:", nor_rows, "\n")
  
  cat("\n--- Row counts consistent: Finland |", label, "---\n")
  fin_rows <- all(sapply(seq_along(pv_cols), function(i)
    sapply(1:m_full, function(j) nrow(imp_datasets_fin[[i]][[j]]) == nrow(STU_FIN_12))))
  cat("All datasets have", nrow(STU_FIN_12), "rows:", fin_rows, "\n")
  
  list(
    label             = label,
    config            = config,
    test_imp_nor      = test_imp_nor,
    test_imp_fin      = test_imp_fin,
    imp_nor_list      = imp_nor_list,
    imp_fin_list      = imp_fin_list,
    imp_datasets_nor  = imp_datasets_nor,
    imp_datasets_fin  = imp_datasets_fin
  )
}

# ------------------------------------------------------------------------------
# 10.2  Configuration — Branch A (with QuestID as auxiliary predictor)
# ------------------------------------------------------------------------------

base_cols_12_withQ <- c(final_preds_12, "QuestID", "W_FSTUWT")

stopifnot(all(base_cols_12_withQ %in% names(STU_NOR_12)))
stopifnot(all(base_cols_12_withQ %in% names(STU_FIN_12)))

pv_data_nor_12_withQ <- make_pv_datasets(STU_NOR_12, pv_cols, base_cols_12_withQ)
pv_data_fin_12_withQ <- make_pv_datasets(STU_FIN_12, pv_cols, base_cols_12_withQ)

cat("\n--- PV datasets prepared: 2012 | with QuestID ---\n")
cat("Norway: ",  length(pv_data_nor_12_withQ), "datasets, each with",
    ncol(pv_data_nor_12_withQ[[1]]), "columns and",
    nrow(pv_data_nor_12_withQ[[1]]), "rows\n")
cat("Finland:",  length(pv_data_fin_12_withQ), "datasets, each with",
    ncol(pv_data_fin_12_withQ[[1]]), "columns and",
    nrow(pv_data_fin_12_withQ[[1]]), "rows\n")

config_12_withQ <- build_imp_config(
  data                = pv_data_nor_12_withQ[[1]],
  cont_preds          = final_cont_preds_12,
  factor_preds        = "LANGN",
  predictor_only_vars = c("PV_MATH", "QuestID")
)

config_12_withQ_fin <- build_imp_config(
  data                = pv_data_fin_12_withQ[[1]],
  cont_preds          = final_cont_preds_12,
  factor_preds        = "LANGN",
  predictor_only_vars = c("PV_MATH", "QuestID")
)

# Sanity check: configurations should match across countries
cat("\n--- Methods identical across countries? | with QuestID ---\n")
print(all(config_12_withQ$method == config_12_withQ_fin$method))
cat("\n--- Predictor matrices identical across countries? | with QuestID ---\n")
print(all(config_12_withQ$predictorMatrix == config_12_withQ_fin$predictorMatrix))

# ------------------------------------------------------------------------------
# 10.3  Configuration — Branch B (without QuestID)
# ------------------------------------------------------------------------------

base_cols_12_noQ <- c(final_preds_12, "W_FSTUWT")

stopifnot(all(base_cols_12_noQ %in% names(STU_NOR_12)))
stopifnot(all(base_cols_12_noQ %in% names(STU_FIN_12)))

pv_data_nor_12_noQ <- make_pv_datasets(STU_NOR_12, pv_cols, base_cols_12_noQ)
pv_data_fin_12_noQ <- make_pv_datasets(STU_FIN_12, pv_cols, base_cols_12_noQ)

cat("\n--- PV datasets prepared: 2012 | without QuestID ---\n")
cat("Norway: ",  length(pv_data_nor_12_noQ), "datasets, each with",
    ncol(pv_data_nor_12_noQ[[1]]), "columns and",
    nrow(pv_data_nor_12_noQ[[1]]), "rows\n")
cat("Finland:",  length(pv_data_fin_12_noQ), "datasets, each with",
    ncol(pv_data_fin_12_noQ[[1]]), "columns and",
    nrow(pv_data_fin_12_noQ[[1]]), "rows\n")

config_12_noQ <- build_imp_config(
  data                = pv_data_nor_12_noQ[[1]],
  cont_preds          = final_cont_preds_12,
  factor_preds        = "LANGN",
  predictor_only_vars = "PV_MATH"
)

config_12_noQ_fin <- build_imp_config(
  data                = pv_data_fin_12_noQ[[1]],
  cont_preds          = final_cont_preds_12,
  factor_preds        = "LANGN",
  predictor_only_vars = "PV_MATH"
)

# Sanity check: configurations should match across countries
cat("\n--- Methods identical across countries? | without QuestID ---\n")
print(all(config_12_noQ$method == config_12_noQ_fin$method))
cat("\n--- Predictor matrices identical across countries? | without QuestID ---\n")
print(all(config_12_noQ$predictorMatrix == config_12_noQ_fin$predictorMatrix))

# ------------------------------------------------------------------------------
# 10.4  Run both MI branches
# ------------------------------------------------------------------------------

mi_2012_withQ <- run_mi_block(
  pv_data_nor = pv_data_nor_12_withQ,
  pv_data_fin = pv_data_fin_12_withQ,
  pv_cols     = pv_cols,
  config      = config_12_withQ,
  label       = "withQ",
  m_test      = 5,
  maxit_test  = 5,
  m_full      = 40,
  maxit_full  = 20,
  seed_base   = 2012
)

saveRDS(mi_2012_withQ, "mi_2012_withQ.rds")

mi_2012_noQ <- run_mi_block(
  pv_data_nor = pv_data_nor_12_noQ,
  pv_data_fin = pv_data_fin_12_noQ,
  pv_cols     = pv_cols,
  config      = config_12_noQ,
  label       = "noQ",
  m_test      = 5,
  maxit_test  = 5,
  m_full      = 40,
  maxit_full  = 20,
  seed_base   = 3012
)

saveRDS(mi_2012_noQ, "mi_2012_noQ.rds")

# ------------------------------------------------------------------------------
# 10.5  Compare branches and select final imputed datasets
# ------------------------------------------------------------------------------

# Reload from disk so the comparison block can be re-run independently
mi_2012_withQ <- readRDS("mi_2012_withQ.rds")
mi_2012_noQ   <- readRDS("mi_2012_noQ.rds")

cat("\n====================================================\n")
cat("Comparison summary: withQ vs noQ\n")
cat("====================================================\n")

cat("\n--- Unique logged-event predictors: Norway | withQ ---\n")
print(unique(mi_2012_withQ$test_imp_nor$loggedEvents$out))
cat("\n--- Unique logged-event predictors: Finland | withQ ---\n")
print(unique(mi_2012_withQ$test_imp_fin$loggedEvents$out))

cat("\n--- Logged events: Norway | noQ ---\n")
print(mi_2012_noQ$test_imp_nor$loggedEvents)
cat("\n--- Logged events: Finland | noQ ---\n")
print(mi_2012_noQ$test_imp_fin$loggedEvents)

# Final decision: noQ branch selected based on convergence behaviour and
# downstream stability (full justification in methods chapter).
imp_datasets_nor_12 <- mi_2012_noQ$imp_datasets_nor
imp_datasets_fin_12 <- mi_2012_noQ$imp_datasets_fin

cat("\n--- Final imputed datasets confirmed: 2012 ---\n")
cat("Norway:  200 datasets (5 PVs x 40 imputations)\n")
cat("Finland: 200 datasets (5 PVs x 40 imputations)\n")
cat("Model: noQ branch (QuestID excluded from imputation model)\n")

# ==============================================================================
# 11. Pooled regression analysis
# ==============================================================================
#
# Pooling structure
# -----------------
#   Stage 1 (within PV)    — Rubin's rules across the m imputations using
#                            MIcombine() to obtain a pooled coefficient vector
#                            and variance–covariance matrix per PV.
#                            Rubin's (1 + 1/m) factor uses true m = 40.
#   Stage 2 (across PVs)   — PV combination rules (NAEP/PISA) applied to the
#                            Q pooled per-PV estimates: the final point
#                            estimate is the mean across PVs; the final
#                            variance is mean(within) + (1 + 1/Q) * between.
#                            avg_pv_var captures sampling + MI uncertainty
#                            within each PV; between_var captures measurement
#                            uncertainty across plausible values.

# ------------------------------------------------------------------------------
# 11.1  Helper functions
# ------------------------------------------------------------------------------

# Re-attach replicate weights to imputed data (mice strips columns it doesn't
# need to impute, so the replicate weights must be added back for survey
# variance estimation).
add_replicate_weights <- function(imp_datasets, rw, w_reps) {
  lapply(imp_datasets, function(pv_list) {
    lapply(pv_list, function(dat) {
      stopifnot(nrow(dat) == nrow(rw))
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
# 11.2  Add replicate weights to imputed datasets
# ------------------------------------------------------------------------------

rw_nor_12 <- STU_NOR_12[, w_reps]
rw_fin_12 <- STU_FIN_12[, w_reps]

imp_datasets_nor_12 <- add_replicate_weights(imp_datasets_nor_12, rw_nor_12, w_reps)
imp_datasets_fin_12 <- add_replicate_weights(imp_datasets_fin_12, rw_fin_12, w_reps)

cat("\n--- Replicate weights added: 2012 ---\n")

cat("Norway columns:", ncol(imp_datasets_nor_12[[1]][[1]]), "\n")
cat("Norway W_FSTR1 present:",
    "W_FSTR1"  %in% names(imp_datasets_nor_12[[1]][[1]]), "\n")
cat("Norway W_FSTR80 present:",
    "W_FSTR80" %in% names(imp_datasets_nor_12[[1]][[1]]), "\n")

cat("\nFinland columns:", ncol(imp_datasets_fin_12[[1]][[1]]), "\n")
cat("Finland W_FSTR1 present:",
    "W_FSTR1"  %in% names(imp_datasets_fin_12[[1]][[1]]), "\n")
cat("Finland W_FSTR80 present:",
    "W_FSTR80" %in% names(imp_datasets_fin_12[[1]][[1]]), "\n")

# ------------------------------------------------------------------------------
# 11.3  Regression formula and BRR design builder
# ------------------------------------------------------------------------------

reg_formula_12 <- PV_MATH ~ ESCS + LANGN + MATHEFF + ANXMAT + TEACHSUP

make_brr_design_12_imp <- function(dat) {
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
# 11.4  Two-stage pooling
# ------------------------------------------------------------------------------

pool_regression_nested_12 <- function(imp_datasets, formula, label) {
  
  cat("\n--- Running nested regression pipeline:", label, "---\n")
  
  Q <- length(imp_datasets)
  m <- length(imp_datasets[[1]])
  cat("PVs:", Q, "| Imputations per PV:", m, "\n")
  
  pv_coefs <- vector("list", Q)
  pv_vcovs <- vector("list", Q)
  
  # Stage 1: pool m imputations within each PV via MIcombine()
  for (i in seq_len(Q)) {
    cat("  Stage 1 — PV", i, "of", Q, ": fitting", m, "models\n")
    
    pv_models <- lapply(imp_datasets[[i]], function(dat) {
      design <- make_brr_design_12_imp(dat)
      svyglm(formula, design = design, family = gaussian())
    })
    
    pooled_pv     <- MIcombine(pv_models)
    pv_coefs[[i]] <- coef(pooled_pv)
    pv_vcovs[[i]] <- vcov(pooled_pv)
  }
  
  # Verify coefficient names are consistent across PVs
  stopifnot(all(sapply(pv_coefs, function(x)
    identical(names(x), names(pv_coefs[[1]])))))
  
  # Verify vcov column names are consistent across PVs
  stopifnot(all(sapply(pv_vcovs, function(x)
    identical(colnames(x), colnames(pv_vcovs[[1]])))))
  
  # Stage 2: combine across PVs using PV combination rules
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

reg_nor_12 <- pool_regression_nested_12(imp_datasets_nor_12, reg_formula_12, "Norway 2012")
reg_fin_12 <- pool_regression_nested_12(imp_datasets_fin_12, reg_formula_12, "Finland 2012")

# ------------------------------------------------------------------------------
# 11.5  Format regression results
# ------------------------------------------------------------------------------
# p-values use the normal approximation (appropriate after two-stage pooling
# where the final degrees-of-freedom does not reduce to a simple closed form).

reg_results_nor_12 <- format_reg_results(reg_nor_12, "NOR", 2012)
reg_results_fin_12 <- format_reg_results(reg_fin_12, "FIN", 2012)
reg_results_12     <- rbind(reg_results_nor_12, reg_results_fin_12)

cat("\n--- Pooled regression results: Norway 2012 ---\n")
print(reg_results_nor_12)
cat("\n--- Pooled regression results: Finland 2012 ---\n")
print(reg_results_fin_12)

# ==============================================================================
# 12. Pooled weighted means (point estimates for OBD)
# ==============================================================================

# For the Oaxaca–Blinder decomposition, only pooled point estimates of the
# predictor and outcome means are required as inputs. Means are computed under
# the BRR design for each of the 200 datasets and then averaged. Variance and
# inference for the means are computed separately in Section 13.

compute_pooled_means_12 <- function(imp_datasets, cont_preds, label) {
  
  cat("\n--- Computing weighted means:", label, "---\n")
  
  all_means <- list()
  
  for (i in seq_along(imp_datasets)) {
    for (j in seq_along(imp_datasets[[i]])) {
      
      dat    <- imp_datasets[[i]][[j]]
      design <- make_brr_design_12_imp(dat)
      
      # Continuous predictor means
      cont_means <- as.numeric(
        svymean(as.formula(paste("~", paste(cont_preds, collapse = " + "))),
                design, na.rm = TRUE)
      )
      names(cont_means) <- cont_preds
      
      # LANGN: use names returned by svymean to avoid mislabelling
      langn_obj         <- svymean(~ LANGN, design, na.rm = TRUE)
      langn_mean        <- as.numeric(langn_obj)
      names(langn_mean) <- names(coef(langn_obj))
      
      # Outcome mean
      outcome_mean        <- as.numeric(svymean(~ PV_MATH, design, na.rm = TRUE))
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

pooled_means_nor_12 <- compute_pooled_means_12(imp_datasets_nor_12,
                                               final_cont_preds_12,
                                               "Norway 2012")
pooled_means_fin_12 <- compute_pooled_means_12(imp_datasets_fin_12,
                                               final_cont_preds_12,
                                               "Finland 2012")

# Verify names are aligned before building table
stopifnot(identical(names(pooled_means_nor_12), names(pooled_means_fin_12)))

cat("\n--- Pooled weighted means: Norway 2012 ---\n");  print(round(pooled_means_nor_12, 4))
cat("\n--- Pooled weighted means: Finland 2012 ---\n"); print(round(pooled_means_fin_12, 4))

means_table_12 <- data.frame(
  variable           = names(pooled_means_nor_12),
  mean_NOR           = round(pooled_means_nor_12, 4),
  mean_FIN           = round(pooled_means_fin_12, 4),
  diff_NOR_minus_FIN = round(pooled_means_nor_12 - pooled_means_fin_12, 4),
  row.names          = NULL
)

cat("\n--- Means comparison table: 2012 ---\n")
print(means_table_12)

# Export for use in the Excel-based OBD calculation
write.csv(reg_results_12, "reg_results_2012.csv", row.names = FALSE)
write.csv(means_table_12, "means_table_2012.csv", row.names = FALSE)

cat("\n--- Files saved ---\n")
cat("reg_results_2012.csv\n")
cat("means_table_2012.csv\n")

# ==============================================================================
# 13. Pooled means with standard errors (for cross-cycle inference)
# ==============================================================================

# Required to test whether the within-country shift in any predictor's mean
# from 2012 to 2022 (X_22 - X_12) is statistically significant. Pooling
# structure mirrors that used for the regression coefficients in Section 11:
#   Stage 1 — Rubin's rules within each PV, using BRR design-based SEs.
#   Stage 2 — PV combination rules across the Q pooled per-PV estimates.

compute_pooled_means_with_se_12 <- function(imp_datasets, cont_preds, label) {
  
  cat("\n--- Computing pooled means with SEs:", label, "---\n")
  
  Q <- length(imp_datasets)
  m <- length(imp_datasets[[1]])
  
  pv_means_list <- vector("list", Q)
  pv_vars_list  <- vector("list", Q)
  
  for (i in seq_len(Q)) {
    
    # Storage: rows = imputations within this PV, columns = variables
    imp_means <- matrix(NA, nrow = m, ncol = length(cont_preds) + 3)
    
    # First pass: collect point estimates from each imputation
    for (j in seq_len(m)) {
      dat    <- imp_datasets[[i]][[j]]
      design <- make_brr_design_12_imp(dat)
      
      cont_obj        <- svymean(as.formula(paste("~", paste(cont_preds, collapse = " + "))),
                                 design, na.rm = TRUE)
      cont_est        <- as.numeric(cont_obj)
      names(cont_est) <- cont_preds
      
      langn_obj        <- svymean(~ LANGN, design, na.rm = TRUE)
      langn_est        <- as.numeric(langn_obj)
      names(langn_est) <- names(coef(langn_obj))
      
      pv_est        <- as.numeric(svymean(~ PV_MATH, design, na.rm = TRUE))
      names(pv_est) <- "PV_MATH"
      
      imp_means[j, ] <- c(cont_est, langn_est, pv_est)
    }
    
    var_names <- c(cont_preds, names(coef(langn_obj)), "PV_MATH")
    colnames(imp_means) <- var_names
    
    # Stage 1: within-PV pooling via Rubin's rules
    pv_means_list[[i]] <- colMeans(imp_means)
    
    # Second pass: collect BRR-based SEs from each imputation
    imp_ses <- matrix(NA, nrow = m, ncol = length(var_names))
    colnames(imp_ses) <- var_names
    
    for (j in seq_len(m)) {
      dat    <- imp_datasets[[i]][[j]]
      design <- make_brr_design_12_imp(dat)
      
      cont_obj  <- svymean(as.formula(paste("~", paste(cont_preds, collapse = " + "))),
                           design, na.rm = TRUE)
      langn_obj <- svymean(~ LANGN, design, na.rm = TRUE)
      pv_obj    <- svymean(~ PV_MATH, design, na.rm = TRUE)
      
      imp_ses[j, cont_preds]             <- SE(cont_obj)
      imp_ses[j, names(coef(langn_obj))] <- SE(langn_obj)
      imp_ses[j, "PV_MATH"]              <- SE(pv_obj)
    }
    
    within_var  <- colMeans(imp_ses^2)
    between_var <- apply(imp_means, 2, var)
    total_var   <- within_var + (1 + 1/m) * between_var
    
    pv_vars_list[[i]] <- total_var
    cat("  PV", i, "complete\n")
  }
  
  # Stage 2: combine across PVs
  means_mat <- do.call(rbind, pv_means_list)
  vars_mat  <- do.call(rbind, pv_vars_list)
  
  final_means <- colMeans(means_mat)
  avg_pv_var  <- colMeans(vars_mat)
  between_var <- apply(means_mat, 2, var)
  total_var   <- avg_pv_var + (1 + 1/Q) * between_var
  final_se    <- sqrt(total_var)
  
  cat("Done —", label, "\n")
  
  data.frame(
    variable  = names(final_means),
    mean      = round(final_means, 4),
    se        = round(final_se, 4),
    row.names = NULL
  )
}

means_se_nor_12 <- compute_pooled_means_with_se_12(imp_datasets_nor_12,
                                                   final_cont_preds_12,
                                                   "Norway 2012")
means_se_fin_12 <- compute_pooled_means_with_se_12(imp_datasets_fin_12,
                                                   final_cont_preds_12,
                                                   "Finland 2012")

cat("\n--- Pooled means with SEs: Norway 2012 ---\n");  print(means_se_nor_12)
cat("\n--- Pooled means with SEs: Finland 2012 ---\n"); print(means_se_fin_12)

write.csv(means_se_nor_12, "means_se_nor_2012.csv", row.names = FALSE)
write.csv(means_se_fin_12, "means_se_fin_2012.csv", row.names = FALSE)

# ==============================================================================
# End of script
# ==============================================================================
