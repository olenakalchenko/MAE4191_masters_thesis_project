# ==============================================================================
# Packages
# ==============================================================================

library(haven)    
library(dplyr)      
library(naniar)    
library(survey)
library(mitools)
library(VIM)       
library(mice)      
library(gridExtra) 
library(ggplot2)   
library(mitools)   
library(tidyverse)
library(corrplot)  
library(patchwork) 
library(car)  
library(DescTools)  
library(EdSurvey)
library(lsr)

# ------------------------------------------------------------------------------
# Package citations & R version
# ------------------------------------------------------------------------------

packages <- c("haven", "dplyr", "naniar", "survey", "mitools", "VIM", "mice", "gridExtra", "ggplot2", "tidyverse", "corrplot", "patchwork", "car", "DescTools", "EdSurvey", "lsr")

for (pkg in packages) {
  cat("\n===", pkg, "===\n")
  print(citation(pkg))
}

R.version.string

# ==============================================================================
# Load data 
# ==============================================================================

# Source: PISA 2012 Student Questionnaire Data (OECD)
# Link: https://www.oecd.org/en/data/datasets/pisa-2012-database.html#data
# Original data: TXT format, converted to .sav using official OECD SPSS 
# control file (Syntax to read in student questionnaire data file)

# Set working directory
# Not needed if I upload everything as an R project
setwd("C:/Users/elly2/Desktop/thesis")

# Load PISA 2012 Student Questionnaire data from SPSS file
INT_STU12_DEC03 <- read_sav("INT_STU12_DEC03.sav", user_na = TRUE)

# Save the original dataset as master copy
MDATA_12_STU_QQQ <- INT_STU12_DEC03

# ------------------------------------------------------------------------------
# Merge rescaled ESCS for trend analysis
# ------------------------------------------------------------------------------

# The rescaled ESCS file is downloaded separately from the OECD 2022 database:
# https://www.oecd.org/en/data/datasets/pisa-2022-database.html#indices
# It contains ESCS recomputed using 2022 methodology for 2012, 2015 and 2018.

escs_trend <- read.csv("escs_trend.csv")

cat("--- escs_trend file structure ---\n")
head(escs_trend)
names(escs_trend)

# Filter to 2012 only
escs_2012 <- escs_trend %>%
  filter(cycle == 5) %>%
  select(cnt, schoolid, studentid, escs_trend)

cat("Rows in escs_2012:", nrow(escs_2012), "\n")  # should be 453,304

# Harmonise ID formats for merging:
# escs file stores IDs as numeric (1, 2, 3...)
# PISA 2012 file stores them as zero-padded character strings
# ("0000001" for SCHOOLID, "00001" for StIDStd)

escs_2012 <- escs_2012 %>%
  mutate(
    cnt      = toupper(cnt),                          # ensure uppercase to match CNT
    SCHOOLID = sprintf("%07d", as.integer(schoolid)), # pad to 7 characters
    StIDStd  = sprintf("%05d", as.integer(studentid)) # pad to 5 characters
  ) %>%
  select(cnt, SCHOOLID, StIDStd, escs_trend)

# Verify formats match before merging
cat("\nSample SCHOOLID from escs_2012:\n")
head(escs_2012$SCHOOLID)
cat("\nSample SCHOOLID from MDATA_12_STU_QQQ:\n")
head(MDATA_12_STU_QQQ$SCHOOLID[MDATA_12_STU_QQQ$CNT == "NOR"])

cat("\nSample StIDStd from escs_2012:\n")
head(escs_2012$StIDStd)
cat("\nSample StIDStd from MDATA_12_STU_QQQ:\n")
head(MDATA_12_STU_QQQ$StIDStd[MDATA_12_STU_QQQ$CNT == "NOR"])

# Merge rescaled ESCS into the main dataset
MDATA_12_STU_QQQ <- MDATA_12_STU_QQQ %>%
  left_join(escs_2012, by = c("CNT" = "cnt", "SCHOOLID", "StIDStd")) %>%
  mutate(ESCS = escs_trend) %>%  # overwrite original ESCS with rescaled version
  select(-escs_trend)

# Verify merge quality
cat("\nRows after merge:", nrow(MDATA_12_STU_QQQ), "\n")  # should be unchanged
cat("Missing rescaled ESCS:", sum(is.na(MDATA_12_STU_QQQ$ESCS)), "\n")
cat("Missing ESCS for NOR:", sum(is.na(MDATA_12_STU_QQQ$ESCS[MDATA_12_STU_QQQ$CNT == "NOR"])), "\n")
cat("Missing ESCS for FIN:", sum(is.na(MDATA_12_STU_QQQ$ESCS[MDATA_12_STU_QQQ$CNT == "FIN"])), "\n")

# ==============================================================================
# Variable selection
# ==============================================================================

# ------------------------------------------------------------------------------
# Step 1: Define columns and subset by country
# ------------------------------------------------------------------------------

pv_cols     <- paste0("PV", 1:5, "MATH")           # 5 PVs in 2012
w_reps      <- paste0("W_FSTR", 1:80)              # BRR replicate weights
design_vars <- c("CNT", "STRATUM", "SCHOOLID", "W_FSTUWT", "QuestID")

# Note: DISCLIM is named DISCLIMA in 2012
# Note: EXPOFA splits into EXAPPLM and EXPUREM in 2012 — keeping both for diagnostics
cands <- c("ESCS", "IMMIG", "LANGN",           # student background / demographics
           "MATHEFF", "ANXMAT",                # learning dispositions
           "TEACHSUP", "DISCLIMA",             # school climate / learning environment
           "EXAPPLM", "EXPUREM")               # opportunity to learn (2012 equivalents)

keep_cols <- unique(c(design_vars, cands, w_reps, pv_cols))

STU_NOR_12 <- MDATA_12_STU_QQQ %>% filter(CNT == "NOR") %>% select(all_of(keep_cols))
STU_FIN_12 <- MDATA_12_STU_QQQ %>% filter(CNT == "FIN") %>% select(all_of(keep_cols))

# Quick checks
stopifnot(all(keep_cols %in% names(STU_NOR_12)))
stopifnot(all(keep_cols %in% names(STU_FIN_12)))
cat("NOR rows:", nrow(STU_NOR_12), "| FIN rows:", nrow(STU_FIN_12), "\n") # NOR should be 4,686 | FIN should be 8,829

# ------------------------------------------------------------------------------
# Step 2: Strip SPSS labels and recode PISA missing value codes to NA
# ------------------------------------------------------------------------------

invalid_codes <- c(9, 99, 999, 9999, 99999, 999999, 9999999,   # Item level non-response
                   7, 97, 997, 9997, 99997, 999997, 9999997,   # Not-administered
                   8, 98, 998, 9998, 99998, 999998, 9999998)   # Multiple or invalid responses

# Note: read_sav(user_na = TRUE) preserves SPSS user-missing metadata rather than
# converting all such values to ordinary NA automatically. Therefore zap_missing()
# is used first to convert declared user-missing values to NA. The sentinel
# recoding below is retained as an additional safeguard.

recode_pisa <- function(df) {
  df %>%
    mutate(across(everything(), haven::zap_missing)) %>%
    mutate(across(everything(), haven::zap_labels)) %>%
    mutate(across(all_of(c(cands, pv_cols)),
                  ~ { x <- .x; x[!is.na(x) & x %in% invalid_codes] <- NA; x }))
}

STU_NOR_12 <- recode_pisa(STU_NOR_12)
STU_FIN_12 <- recode_pisa(STU_FIN_12)

# Verify: no labels remain, no sentinel codes remain as non-NA values
stopifnot(!any(sapply(STU_NOR_12, haven::is.labelled)))
stopifnot(!any(sapply(STU_FIN_12, haven::is.labelled)))
stopifnot(!any(sapply(c(cands, pv_cols), function(v)
  any(STU_NOR_12[[v]] %in% invalid_codes, na.rm = TRUE))))
stopifnot(!any(sapply(c(cands, pv_cols), function(v)
  any(STU_FIN_12[[v]] %in% invalid_codes, na.rm = TRUE))))

# ------------------------------------------------------------------------------
# Step 3: Diagnostic — inspect sentinel code types in raw data
# ------------------------------------------------------------------------------

cat("\n--- Sentinel code presence in raw data (tagged NAs): Norway ---\n")
sapply(cands, function(v) {
  any(MDATA_12_STU_QQQ[[v]][MDATA_12_STU_QQQ$CNT == "NOR"] %in% invalid_codes,
      na.rm = TRUE)
})

cat("\n--- Sentinel code presence in raw data (tagged NAs): Finland ---\n")
sapply(cands, function(v) {
  any(MDATA_12_STU_QQQ[[v]][MDATA_12_STU_QQQ$CNT == "FIN"] %in% invalid_codes,
      na.rm = TRUE)
})

# ------------------------------------------------------------------------------
# Step 4: Recode LANGN, IMMIG, and QuestID
# ------------------------------------------------------------------------------

# LANGN

# Check value labels
attr(MDATA_12_STU_QQQ$LANGN, "labels")

# Check LANGN distributions in 2012 raw data
table(MDATA_12_STU_QQQ$LANGN[MDATA_12_STU_QQQ$CNT == "NOR"])
table(MDATA_12_STU_QQQ$LANGN[MDATA_12_STU_QQQ$CNT == "FIN"])

# !! Verify majority language codes from the output above before running !!
# Norwegian code in 2022 was 523 — verify it is the same in 2012
# Finnish code in 2022 was 420, Swedish was 494 — verify in 2012

class(STU_NOR_12$LANGN)
typeof(STU_NOR_12$LANGN)
class(STU_FIN_12$LANGN)
typeof(STU_FIN_12$LANGN)

table(STU_NOR_12$LANGN, useNA = "ifany")
table(STU_FIN_12$LANGN, useNA = "ifany")

# Norway: Norwegian (523) = Majority
STU_NOR_12 <- STU_NOR_12 %>%
  mutate(LANGN = factor(
    case_when(
      LANGN == "523" ~ "Majority",
      is.na(LANGN)   ~ NA_character_,
      TRUE           ~ "Other"
    ),
    levels = c("Majority", "Other")
  ))

# Finland: Finnish (420) and Swedish (494) = Majority
STU_FIN_12 <- STU_FIN_12 %>%
  mutate(LANGN = factor(
    case_when(
      LANGN %in% c("420", "494") ~ "Majority",
      is.na(LANGN)               ~ NA_character_,
      TRUE                       ~ "Other"
    ),
    levels = c("Majority", "Other")
  ))

#-------------------------------------------------------------------------------
# Checks:
# Check distributions after recoding
cat("\nLANGN distribution — Norway 2012:\n")
print(table(STU_NOR_12$LANGN, useNA = "ifany"))
# Majority should be 4134, Other should be 360 (11+9+28+312), NA should be 192

cat("\nLANGN distribution — Finland 2012:\n")
print(table(STU_FIN_12$LANGN, useNA = "ifany"))
# Majority should be 7454 (6253+1201), Other should be 1210, NA should be 165

# Check it is now a factor
cat("\nClass of LANGN — Norway:", class(STU_NOR_12$LANGN), "\n")
cat("Class of LANGN — Finland:", class(STU_FIN_12$LANGN), "\n")

# Check factor levels are correct
cat("\nLevels — Norway:", levels(STU_NOR_12$LANGN), "\n")
cat("Levels — Finland:", levels(STU_FIN_12$LANGN), "\n")

# Confirm no raw numeric codes remain
cat("\nAny raw codes remaining in Norway:", 
    any(STU_NOR_12$LANGN %in% c("523", "264", "494", "540", "840"), na.rm = TRUE), "\n")
cat("Any raw codes remaining in Finland:", 
    any(STU_FIN_12$LANGN %in% c("420", "494", "121", "137", "316", "344", "381", "495", "555", "815"), na.rm = TRUE), "\n")
#-------------------------------------------------------------------------------

# IMMIG

#-------------------------------------------------------------------------------
# Checks
# Check IMMIG structure in 2012 before recoding
cat("Class of IMMIG — Norway:", class(STU_NOR_12$IMMIG), "\n")
cat("Type of IMMIG — Norway:", typeof(STU_NOR_12$IMMIG), "\n")
cat("\nRaw IMMIG distribution — Norway 2012:\n")
print(table(STU_NOR_12$IMMIG, useNA = "ifany"))
cat("\nRaw IMMIG distribution — Finland 2012:\n")
print(table(STU_FIN_12$IMMIG, useNA = "ifany"))
#-------------------------------------------------------------------------------

# Recode IMMIG to factor
STU_NOR_12 <- STU_NOR_12 %>%
  mutate(IMMIG = factor(
    case_when(
      is.na(IMMIG) ~ NA_character_,
      TRUE         ~ as.character(IMMIG)
    ),
    levels = c("1", "2", "3"),
    labels = c("Native", "Second-generation", "First-generation")
  ))

STU_FIN_12 <- STU_FIN_12 %>%
  mutate(IMMIG = factor(
    case_when(
      is.na(IMMIG) ~ NA_character_,
      TRUE         ~ as.character(IMMIG)
    ),
    levels = c("1", "2", "3"),
    labels = c("Native", "Second-generation", "First-generation")
  ))

#-------------------------------------------------------------------------------
# Checks:
#-------------------------------------------------------------------------------
cat("\nIMMIG distribution — Norway 2012:\n")
print(table(STU_NOR_12$IMMIG, useNA = "ifany"))
# Native should be 4113, Second-generation 229, First-generation 224, NA 120

cat("\nIMMIG distribution — Finland 2012:\n")
print(table(STU_FIN_12$IMMIG, useNA = "ifany"))
# Native should be 7406, Second-generation 583, First-generation 687, NA 153

cat("\nClass of IMMIG — Norway:", class(STU_NOR_12$IMMIG), "\n")
cat("Class of IMMIG — Finland:", class(STU_FIN_12$IMMIG), "\n")

cat("\nLevels of IMMIG — Norway:", levels(STU_NOR_12$IMMIG), "\n")
cat("Levels of IMMIG — Finland:", levels(STU_FIN_12$IMMIG), "\n")

# Confirm no raw numeric codes remain
cat("\nAny raw codes remaining in Norway:",
    any(STU_NOR_12$IMMIG %in% c("1", "2", "3"), na.rm = TRUE), "\n")
cat("Any raw codes remaining in Finland:",
    any(STU_FIN_12$IMMIG %in% c("1", "2", "3"), na.rm = TRUE), "\n")

# ------------------------------------------------------------------------------

# QuestID

cat("\nQuestID class:", class(STU_NOR_12$QuestID), "\n")
cat("QuestID type:", typeof(STU_NOR_12$QuestID), "\n")

cat("\nQuestID distribution — Norway 2012:\n")
print(table(STU_NOR_12$QuestID, useNA = "ifany"))

cat("\nQuestID distribution — Finland 2012:\n")
print(table(STU_FIN_12$QuestID, useNA = "ifany"))

STU_NOR_12 <- STU_NOR_12 %>%
  mutate(QuestID = factor(QuestID))

STU_FIN_12 <- STU_FIN_12 %>%
  mutate(QuestID = factor(QuestID))

cat("\nQuestID levels — Norway:", levels(STU_NOR_12$QuestID), "\n")
cat("QuestID levels — Finland:", levels(STU_FIN_12$QuestID), "\n")

# ------------------------------------------------------------------------------
# Step 5: IMMIG vs LANGN — diagnostic comparison
# Note: uses W_FSTUWT for weighted point estimates only
# ------------------------------------------------------------------------------

weighted_cor <- function(x, y, w) {
  complete <- !is.na(x) & !is.na(y) & !is.na(w)
  x <- x[complete]; y <- y[complete]; w <- w[complete]
  if (length(x) < 3) return(NA_real_)
  wx <- w * (x - weighted.mean(x, w))
  wy <- w * (y - weighted.mean(y, w))
  sum(wx * wy) / sqrt(sum(wx^2) * sum(wy^2))
}

# Convert to binary numeric indicators
immig_bin_nor_12 <- as.numeric(STU_NOR_12$IMMIG == "Native")
immig_bin_fin_12 <- as.numeric(STU_FIN_12$IMMIG == "Native")
langn_bin_nor_12 <- as.numeric(STU_NOR_12$LANGN == "Majority")
langn_bin_fin_12 <- as.numeric(STU_FIN_12$LANGN == "Majority")

# Correlations with ESCS
cor_immig_escs_nor_12 <- weighted_cor(immig_bin_nor_12, STU_NOR_12$ESCS, STU_NOR_12$W_FSTUWT)
cor_langn_escs_nor_12 <- weighted_cor(langn_bin_nor_12, STU_NOR_12$ESCS, STU_NOR_12$W_FSTUWT)
cor_immig_escs_fin_12 <- weighted_cor(immig_bin_fin_12, STU_FIN_12$ESCS, STU_FIN_12$W_FSTUWT)
cor_langn_escs_fin_12 <- weighted_cor(langn_bin_fin_12, STU_FIN_12$ESCS, STU_FIN_12$W_FSTUWT)

# Correlations with math (averaged across all 5 PVs)
cor_immig_math_nor_12 <- mean(sapply(pv_cols, function(pv)
  weighted_cor(immig_bin_nor_12, STU_NOR_12[[pv]], STU_NOR_12$W_FSTUWT)), na.rm = TRUE)
cor_langn_math_nor_12 <- mean(sapply(pv_cols, function(pv)
  weighted_cor(langn_bin_nor_12, STU_NOR_12[[pv]], STU_NOR_12$W_FSTUWT)), na.rm = TRUE)
cor_immig_math_fin_12 <- mean(sapply(pv_cols, function(pv)
  weighted_cor(immig_bin_fin_12, STU_FIN_12[[pv]], STU_FIN_12$W_FSTUWT)), na.rm = TRUE)
cor_langn_math_fin_12 <- mean(sapply(pv_cols, function(pv)
  weighted_cor(langn_bin_fin_12, STU_FIN_12[[pv]], STU_FIN_12$W_FSTUWT)), na.rm = TRUE)

# Correlation between IMMIG and LANGN
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

cat("\n--- IMMIG vs LANGN diagnostic: 2012 ---\n")
print(dem_comparison_12)

cat("\n--- Correlation between IMMIG and LANGN: 2012 ---\n")
cat("NOR:", round(cor_immig_langn_nor_12, 3), "\n")
cat("FIN:", round(cor_immig_langn_fin_12, 3), "\n")

# Decision rule:
# Prefer the variable that shows less overlap with ESCS while retaining
# a meaningful association with mathematics performance.
# Also consider conceptual relevance, consistency across countries,
# and interpretability in the final model.

# ------------------------------------------------------------------------------
# Step 6: Identify and remove strata with fewer than 2 PSUs
# Note: PSU variable is SCHOOLID in 2012 (not CNTSCHID)
# ------------------------------------------------------------------------------

# Check variable structure before filtering
class(STU_NOR_12$STRATUM)
typeof(STU_NOR_12$STRATUM)
class(STU_NOR_12$SCHOOLID)
typeof(STU_NOR_12$SCHOOLID)

class(STU_FIN_12$STRATUM)
typeof(STU_FIN_12$STRATUM)
class(STU_FIN_12$SCHOOLID)
typeof(STU_FIN_12$SCHOOLID)

# Count distinct PSUs within each stratum before filtering
psu_check_nor_12 <- STU_NOR_12 %>%
  group_by(STRATUM) %>%
  summarise(
    n_psu      = n_distinct(SCHOOLID),
    n_students = n(),
    .groups    = "drop"
  ) %>%
  arrange(n_psu, STRATUM)

psu_check_fin_12 <- STU_FIN_12 %>%
  group_by(STRATUM) %>%
  summarise(
    n_psu      = n_distinct(SCHOOLID),
    n_students = n(),
    .groups    = "drop"
  ) %>%
  arrange(n_psu, STRATUM)

cat("\n--- PSU count by stratum before filtering: Norway 2012 ---\n")
print(psu_check_nor_12)
cat("\n--- PSU count by stratum before filtering: Finland 2012 ---\n")
print(psu_check_fin_12)

cat("\nNorway: strata with <2 PSUs =", sum(psu_check_nor_12$n_psu < 2), "\n")
cat("Finland: strata with <2 PSUs =", sum(psu_check_fin_12$n_psu < 2), "\n")

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

# Verify PSU structure after filtering
psu_check_nor_12_after <- STU_NOR_12 %>%
  group_by(STRATUM) %>%
  summarise(
    n_psu      = n_distinct(SCHOOLID),
    n_students = n(),
    .groups    = "drop"
  ) %>%
  arrange(n_psu, STRATUM)

psu_check_fin_12_after <- STU_FIN_12 %>%
  group_by(STRATUM) %>%
  summarise(
    n_psu      = n_distinct(SCHOOLID),
    n_students = n(),
    .groups    = "drop"
  ) %>%
  arrange(n_psu, STRATUM)

cat("\n--- PSU count by stratum after filtering: Norway 2012 ---\n")
print(psu_check_nor_12_after, n = Inf)
cat("\n--- PSU count by stratum after filtering: Finland 2012 ---\n")
print(psu_check_fin_12_after, n = Inf)

cat("\nNorway: any strata with <2 PSUs after filtering? ",
    any(psu_check_nor_12_after$n_psu < 2), "\n")
cat("Finland: any strata with <2 PSUs after filtering? ",
    any(psu_check_fin_12_after$n_psu < 2), "\n")

# ------------------------------------------------------------------------------
# Step 7: Build BRR survey designs (2012)
# PISA 2012 uses Fay's BRR replication; the final student weight (W_FSTUWT)
# and the 80 replicate weights (W_FSTR1-W_FSTR80) are used here.
# Note: 2012 replicate weights are named W_FSTR1-W_FSTR80
#       (not W_FSTURWT1-W_FSTURWT80 as in 2022)
# ------------------------------------------------------------------------------

# Check structure of the full weight and replicate weights
class(STU_NOR_12$W_FSTUWT)
typeof(STU_NOR_12$W_FSTUWT)
class(STU_FIN_12$W_FSTUWT)
typeof(STU_FIN_12$W_FSTUWT)

# Check that all replicate-weight columns are present
stopifnot(all(w_reps %in% names(STU_NOR_12)))
stopifnot(all(w_reps %in% names(STU_FIN_12)))

# Verify replicate weights are numeric
stopifnot(all(sapply(STU_NOR_12[, w_reps], is.numeric)))
stopifnot(all(sapply(STU_FIN_12[, w_reps], is.numeric)))

# Check for missing values in the final weight and replicate weights
cat("\nMissing values in W_FSTUWT — Norway:", sum(is.na(STU_NOR_12$W_FSTUWT)), "\n")
cat("Missing values in W_FSTUWT — Finland:", sum(is.na(STU_FIN_12$W_FSTUWT)), "\n")

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
cat("\n--- Survey design summary: Norway 2012 ---\n")
print(summary(des_nor_12))
cat("\n--- Survey design summary: Finland 2012 ---\n")
print(summary(des_fin_12))

cat("\nWeighted mean ESCS — Norway 2012:\n")
print(svymean(~ESCS, des_nor_12, na.rm = TRUE))
cat("\nWeighted mean ESCS — Finland 2012:\n")
print(svymean(~ESCS, des_fin_12, na.rm = TRUE))

# ------------------------------------------------------------------------------
# Step 8: Weighted missingness per predictor (2012)
# Computes the weighted percentage of missing values for each predictor using
# the BRR survey design.
# ------------------------------------------------------------------------------

weighted_missing_12 <- function(design, vars) {
  res <- sapply(vars, function(v) {
    f <- as.formula(paste0("~ I(as.numeric(is.na(", v, ")))"))
    m <- svymean(f, design = design, na.rm = TRUE)
    round(as.numeric(m) * 100, 2)
  })
  data.frame(
    variable    = vars,
    missing_pct = as.numeric(res),
    row.names   = NULL
  )
}

miss_nor_12 <- weighted_missing_12(des_nor_12, cands)
miss_fin_12 <- weighted_missing_12(des_fin_12, cands)

miss_summary_12 <- merge(
  miss_nor_12, miss_fin_12,
  by = "variable", suffixes = c("_NOR", "_FIN")
)

cat("\n--- Weighted missingness (%) by predictor: 2012 ---\n")
print(miss_summary_12)

# Thresholds for interpretation:
# > 5% warrants discussion
# > 30% suggests exclusion unless strong substantive reason to retain

#------------------------------------------------------------------------------- 
# Checks:
#------------------------------------------------------------------------------- 
# Extract QuestID from the master copy for Norway and Finland
questid_nor <- STU_NOR_12$QuestID
questid_fin <- STU_FIN_12$QuestID

# Check booklet assignment vs missingness on ANXMAT
cat("\n--- Booklet assignment vs ANXMAT missingness: Norway 2012 ---\n")
print(table(questid_nor, is.na(STU_NOR_12$ANXMAT)))

cat("\n--- Booklet assignment vs ANXMAT missingness: Finland 2012 ---\n")
print(table(questid_fin, is.na(STU_FIN_12$ANXMAT)))

# Document the analytic sample for 2012 after accounting for booklet design
cat("\nStudents who received ANXMAT items — Norway 2012:",
    sum(questid_nor %in% c(2, 3)), "\n")
cat("Students who received ANXMAT items — Finland 2012:",
    sum(questid_fin %in% c(2, 3, 5)), "\n")

# Effective missing rate among students who received the items
cat("\nGenuine nonresponse rate among booklet recipients:\n")
cat("Norway:", round(sum(questid_nor == 2 & is.na(STU_NOR_12$ANXMAT)) +
                       sum(questid_nor == 3 & is.na(STU_NOR_12$ANXMAT)),0),
    "out of", sum(questid_nor %in% c(2,3)), "=",
    round((sum(questid_nor == 2 & is.na(STU_NOR_12$ANXMAT)) +
             sum(questid_nor == 3 & is.na(STU_NOR_12$ANXMAT))) /
            sum(questid_nor %in% c(2,3)) * 100, 2), "%\n")
cat("Finland:", round(sum(questid_fin %in% c(2, 3, 5) & is.na(STU_FIN_12$ANXMAT)), 0),
    "out of", sum(questid_fin %in% c(2, 3, 5)), "=",
    round(sum(questid_fin %in% c(2, 3, 5) & is.na(STU_FIN_12$ANXMAT)) /
            sum(questid_fin %in% c(2, 3, 5)) * 100, 2), "%\n")

# Check booklet pattern for all rotated variables
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
#-------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Step 9: Complete cases summary (2012)
# ------------------------------------------------------------------------------

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

cat("\n--- Complete cases summary: 2012 ---\n")
print(cc_table_12)
stopifnot(all(cc_table_12$n_complete + cc_table_12$n_dropped == cc_table_12$n_total))

# Weighted complete cases
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
# Step 10: Little's MCAR test (2012)
# ------------------------------------------------------------------------------

little_data_nor_12 <- STU_NOR_12[, cands]
little_data_fin_12 <- STU_FIN_12[, cands]

little_data_nor_12[] <- lapply(little_data_nor_12,
                               function(x) if (is.factor(x)) as.numeric(x) else x)
little_data_fin_12[] <- lapply(little_data_fin_12,
                               function(x) if (is.factor(x)) as.numeric(x) else x)

cat("\n--- Little's MCAR test: Norway 2012 ---\n")
print(mcar_test(little_data_nor_12))

cat("\n--- Little's MCAR test: Finland 2012 ---\n")
print(mcar_test(little_data_fin_12))

# Note on Little's MCAR test result for 2012:
# The significant result (p = 0) reflects the systematic booklet-driven
# missing pattern rather than student-characteristic-driven nonresponse.
# Little's test cannot distinguish between missing-by-design (MCAR by
# random booklet assignment) and missing-by-student-characteristics (MAR).
# The booklet assignment analysis above provides stronger and more direct
# evidence that missingness in 2012 is MCAR by design — Form 1 students
# were randomly assigned and systematically lack ANXMAT/TEACHSUP/DISCLIMA,
# while Form 2 students lack EXAPPLM/EXPUREM, and Form 3 students lack
# MATHEFF. Genuine nonresponse within booklet forms is below 3.2%.

# ------------------------------------------------------------------------------
# MCAR diagnostics
# ------------------------------------------------------------------------------

miss_vars_12  <- cands[!cands %in% c("ESCS", "IMMIG", "LANGN")]
mis_predictors_12 <- c("ESCS", "IMMIG", "LANGN", "QuestID")

add_missing_indicators_12 <- function(df, vars) {
  for (v in vars) df[[paste0("mis_", v)]] <- as.numeric(is.na(df[[v]]))
  df
}

STU_NOR_12 <- add_missing_indicators_12(STU_NOR_12, miss_vars_12)
STU_FIN_12 <- add_missing_indicators_12(STU_FIN_12, miss_vars_12)

# Rebuild designs with missingness indicators
des_nor_12 <- make_design_12(STU_NOR_12, w_reps)
des_fin_12 <- make_design_12(STU_FIN_12, w_reps)

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

# Little's MCAR test
library(naniar)
little_data_nor_12 <- STU_NOR_12[, cands]
little_data_fin_12 <- STU_FIN_12[, cands]
little_data_nor_12[] <- lapply(little_data_nor_12, function(x) if (is.factor(x)) as.numeric(x) else x)
little_data_fin_12[] <- lapply(little_data_fin_12, function(x) if (is.factor(x)) as.numeric(x) else x)

cat("\n--- Little's MCAR test: Norway 2012 ---\n")
print(mcar_test(little_data_nor_12))
cat("\n--- Little's MCAR test: Finland 2012 ---\n")
print(mcar_test(little_data_fin_12))

# ==============================================================================
# Helper functions (defined here for self-contained 2012 script)
# ==============================================================================

# Weighted Pearson correlation
weighted_cor <- function(x, y, w) {
  complete <- !is.na(x) & !is.na(y) & !is.na(w)
  x <- x[complete]; y <- y[complete]; w <- w[complete]
  if (length(x) < 3) return(NA_real_)
  wx <- w * (x - weighted.mean(x, w))
  wy <- w * (y - weighted.mean(y, w))
  sum(wx * wy) / sqrt(sum(wx^2) * sum(wy^2))
}

# Weighted correlation matrix via svyvar
weighted_cor_matrix <- function(vars, design) {
  f       <- as.formula(paste("~", paste(vars, collapse = " + ")))
  cov_mat <- as.matrix(svyvar(f, design, na.rm = TRUE))
  sd_vec  <- sqrt(diag(cov_mat))
  cov_mat / (sd_vec %o% sd_vec)
}

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

# ------------------------------------------------------------------------------
# Step 11: Weighted inter-predictor correlation matrix (2012)
# Note: svyvar() fails due to high booklet-driven missingness (~35%) on
# rotated variables. Using pairwise weighted_cor() instead, which handles
# NA values by computing each pair on complete cases only.
# ------------------------------------------------------------------------------

cont_cands_12 <- cands[!cands %in% c("IMMIG", "LANGN")]

cat("\n--- Continuous predictors included in correlation matrix: 2012 ---\n")
print(cont_cands_12)

# Build pairwise weighted correlation matrix
pairwise_wcor_matrix <- function(df, vars, weight_var) {
  n    <- length(vars)
  mat  <- matrix(NA, n, n, dimnames = list(vars, vars))
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

cat("\n--- Weighted predictor correlations: Norway 2012 ---\n")
print(round(wcor_nor_12, 3))

cat("\n--- Weighted predictor correlations: Finland 2012 ---\n")
print(round(wcor_fin_12, 3))

# Visualise
corrplot::corrplot(
  wcor_nor_12,
  method      = "color",
  type        = "upper",
  tl.col      = "black",
  addCoef.col = "black",
  number.cex  = 0.7,
  title       = "Norway 2012",
  mar         = c(0, 0, 1, 0)
)

corrplot::corrplot(
  wcor_fin_12,
  method      = "color",
  type        = "upper",
  tl.col      = "black",
  addCoef.col = "black",
  number.cex  = 0.7,
  title       = "Finland 2012",
  mar         = c(0, 0, 1, 0)
)

# Flag high correlations
cat("\n--- Predictor pairs with |r| > .80: Norway 2012 ---\n")
print(find_high_corr_pairs(wcor_nor_12))

cat("\n--- Predictor pairs with |r| > .80: Finland 2012 ---\n")
print(find_high_corr_pairs(wcor_fin_12))

# ------------------------------------------------------------------------------
# Step 12: VIF check (unweighted, diagnostic only) — 2012
# Note: Due to missing-by-design booklet structure, no student has complete
# data on all predictors simultaneously. VIF is therefore computed separately
# for each booklet form's variable subset using the relevant student subsample.
# ------------------------------------------------------------------------------

predictors_screen_12 <- c("ESCS", "IMMIG", "LANGN", 
                          cont_cands_12[cont_cands_12 != "ESCS"])

cat("\n--- Provisional predictors for VIF screening: 2012 ---\n")
print(predictors_screen_12)

stopifnot(all(predictors_screen_12 %in% names(STU_NOR_12)))
stopifnot(all(predictors_screen_12 %in% names(STU_FIN_12)))

# Form 2 variables: MATHEFF, ANXMAT, TEACHSUP, DISCLIMA available
# (Form 1 students missing these — exclude Form 1)
form2_vars_nor <- !is.na(STU_NOR_12$MATHEFF) & !is.na(STU_NOR_12$ANXMAT)
form2_vars_fin <- !is.na(STU_FIN_12$MATHEFF) & !is.na(STU_FIN_12$ANXMAT)

vif_formula_form2 <- as.formula(
  paste("PV1MATH ~ ESCS + IMMIG + LANGN + MATHEFF + ANXMAT + TEACHSUP + DISCLIMA")
)

cat("\n--- VIF (Form 2 subsample — MATHEFF/ANXMAT/TEACHSUP/DISCLIMA): Norway 2012 ---\n")
print(round(car::vif(lm(vif_formula_form2, 
                        data = STU_NOR_12[form2_vars_nor, ])), 2))

cat("\n--- VIF (Form 2 subsample — MATHEFF/ANXMAT/TEACHSUP/DISCLIMA): Finland 2012 ---\n")
print(round(car::vif(lm(vif_formula_form2, 
                        data = STU_FIN_12[form2_vars_fin, ])), 2))

# Form 1 variables: EXAPPLM, EXPUREM available
# (Form 2 and 3 students missing these — exclude them)
form1_vars_nor <- !is.na(STU_NOR_12$EXAPPLM)
form1_vars_fin <- !is.na(STU_FIN_12$EXAPPLM)

vif_formula_form1 <- as.formula(
  paste("PV1MATH ~ ESCS + IMMIG + LANGN + EXAPPLM + EXPUREM")
)

cat("\n--- VIF (Form 1 subsample — EXAPPLM/EXPUREM): Norway 2012 ---\n")
print(round(car::vif(lm(vif_formula_form1,
                        data = STU_NOR_12[form1_vars_nor, ])), 2))

cat("\n--- VIF (Form 1 subsample — EXAPPLM/EXPUREM): Finland 2012 ---\n")
print(round(car::vif(lm(vif_formula_form1,
                        data = STU_FIN_12[form1_vars_fin, ])), 2))

# Interpretation guideline:
# - VIF > 5: warrants attention
# - VIF > 10: problematic

# ------------------------------------------------------------------------------
# Step 13: Country-level entirely-missing check — 2012
# ------------------------------------------------------------------------------

entirely_missing <- function(df, vars) {
  sapply(vars, function(v) all(is.na(df[[v]])))
}

cat("\n--- Entirely missing: Norway 2012 ---\n")
print(entirely_missing(STU_NOR_12, predictors_screen_12))

cat("\n--- Entirely missing: Finland 2012 ---\n")
print(entirely_missing(STU_FIN_12, predictors_screen_12))

cat("\nProvisional predictor set retained for now: 2012\n")
print(predictors_screen_12)

# ------------------------------------------------------------------------------
# Step 14: Confirm final predictor set
# This block defines the confirmed predictor set for use in multiple imputation 
# and Oaxaca-Blinder decomposition. No further variables are dropped or added
# beyond this point.
# ------------------------------------------------------------------------------

# Final predictor set: ESCS, LANGN, MATHEFF, ANXMAT, TEACHSUP

final_preds_12 <- c("ESCS", "LANGN", "MATHEFF", "ANXMAT", "TEACHSUP")
final_cont_preds_12 <- final_preds_12[!final_preds_12 %in% "LANGN"]

# Confirm all final predictors are present in both country datasets
stopifnot(all(final_preds_12 %in% names(STU_NOR_12)))
stopifnot(all(final_preds_12 %in% names(STU_FIN_12)))

cat("\n--- Final predictor set: 2012 ---\n")
print(final_preds_12)

cat("\nFinal continuous predictors:\n")
print(final_cont_preds_12)

cat("\nFinal sample sizes:\n")
cat("Norway:", nrow(STU_NOR_12), "\n")
cat("Finland:", nrow(STU_FIN_12), "\n")

# ==============================================================================
# Multiple Imputation — PISA 2012
# Sensitivity analysis:
#   A) with QuestID in the imputation model
#   B) without QuestID in the imputation model
# ==============================================================================

# ------------------------------------------------------------------------------
# Helper functions for MI pipeline
# ------------------------------------------------------------------------------

build_imp_config <- function(data, cont_preds, factor_preds,
                             predictor_only_vars = "PV_MATH",
                             weight_col = "W_FSTUWT") {
  pred <- mice::make.predictorMatrix(data)
  
  # Exclude survey weight entirely
  if (weight_col %in% colnames(pred)) {
    pred[weight_col, ] <- 0
    pred[, weight_col] <- 0
  }
  
  # Variables used only as predictors, not imputed
  for (v in predictor_only_vars) {
    if (v %in% colnames(pred)) {
      pred[v, ] <- 0
    }
  }
  
  meth <- rep("", ncol(data))
  names(meth) <- colnames(data)
  
  meth[cont_preds] <- "pmm"
  meth[factor_preds] <- "logreg"
  meth[predictor_only_vars] <- ""
  meth[weight_col] <- ""
  
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

run_mi_block <- function(pv_data_nor, pv_data_fin, pv_cols, config,
                         label = "withQ", m_test = 5, maxit_test = 5,
                         m_full = 40, maxit_full = 20, seed_base = 2012) {
  
  cat("\n====================================================\n")
  cat("Running MI branch:", label, "\n")
  cat("====================================================\n")
  
  # ---------------------------------------------------------------------------
  # Quick test run
  # ---------------------------------------------------------------------------
  
  cat("\n--- Test run:", label, "| PV1, Norway 2012, m =", m_test, ", maxit =", maxit_test, "---\n")
  set.seed(seed_base)
  test_imp_nor <- mice(
    pv_data_nor[[1]],
    m               = m_test,
    maxit           = maxit_test,
    method          = config$method,
    predictorMatrix = config$predictorMatrix,
    printFlag       = TRUE
  )
  cat("Norway test passed — no fatal errors\n")
  
  cat("\n--- Test run:", label, "| PV1, Finland 2012, m =", m_test, ", maxit =", maxit_test, "---\n")
  set.seed(seed_base)
  test_imp_fin <- mice(
    pv_data_fin[[1]],
    m               = m_test,
    maxit           = maxit_test,
    method          = config$method,
    predictorMatrix = config$predictorMatrix,
    printFlag       = TRUE
  )
  cat("Finland test passed — no fatal errors\n")
  
  cat("\n--- Logged events: Norway |", label, "---\n")
  print(test_imp_nor$loggedEvents)
  
  cat("\n--- Logged events: Finland |", label, "---\n")
  print(test_imp_fin$loggedEvents)
  
  # ---------------------------------------------------------------------------
  # Full imputation — Norway
  # ---------------------------------------------------------------------------
  
  cat("\n--- Full imputation:", label, "| Norway 2012 (", length(pv_cols), " PVs x m =", m_full, ") ---\n")
  
  imp_nor_list <- vector("list", length(pv_cols))
  names(imp_nor_list) <- pv_cols
  
  for (i in seq_along(pv_cols)) {
    cat("\n  Norway 2012 |", label, "| PV", i, "of", length(pv_cols), "\n")
    set.seed(seed_base + i)
    imp_nor_list[[i]] <- mice(
      pv_data_nor[[i]],
      m               = m_full,
      maxit           = maxit_full,
      method          = config$method,
      predictorMatrix = config$predictorMatrix,
      printFlag       = FALSE
    )
  }
  
  cat("\nNorway imputation complete —", label, "\n")
  cat("Imputed datasets:", length(imp_nor_list) * m_full, "\n")
  
  # ---------------------------------------------------------------------------
  # Full imputation — Finland
  # ---------------------------------------------------------------------------
  
  cat("\n--- Full imputation:", label, "| Finland 2012 (", length(pv_cols), " PVs x m =", m_full, ") ---\n")
  
  imp_fin_list <- vector("list", length(pv_cols))
  names(imp_fin_list) <- pv_cols
  
  for (i in seq_along(pv_cols)) {
    cat("\n  Finland 2012 |", label, "| PV", i, "of", length(pv_cols), "\n")
    set.seed(seed_base + i)
    imp_fin_list[[i]] <- mice(
      pv_data_fin[[i]],
      m               = m_full,
      maxit           = maxit_full,
      method          = config$method,
      predictorMatrix = config$predictorMatrix,
      printFlag       = FALSE
    )
  }
  
  cat("\nFinland imputation complete —", label, "\n")
  cat("Imputed datasets:", length(imp_fin_list) * m_full, "\n")
  
  # ---------------------------------------------------------------------------
  # Validation
  # ---------------------------------------------------------------------------
  
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
  
  # LANGN proportions
  cat("\n--- LANGN imputed proportions: Norway |", label, "---\n")
  nor_langn_props <- sapply(1:m_full, function(j) {
    dat <- complete(imp_nor_list[[1]], j)
    prop.table(table(dat$LANGN))
  })
  print(round(nor_langn_props, 3))
  
  cat("\n--- LANGN observed proportion: Norway ---\n")
  print(round(prop.table(table(STU_NOR_12$LANGN, useNA = "no")), 3))
  
  cat("\n--- LANGN imputed proportions: Finland |", label, "---\n")
  fin_langn_props <- sapply(1:m_full, function(j) {
    dat <- complete(imp_fin_list[[1]], j)
    prop.table(table(dat$LANGN))
  })
  print(round(fin_langn_props, 3))
  
  cat("\n--- LANGN observed proportion: Finland ---\n")
  print(round(prop.table(table(STU_FIN_12$LANGN, useNA = "no")), 3))
  
  # Missing values after imputation
  cat("\n--- Missing values after imputation: Norway |", label, "---\n")
  nor_missing <- sapply(seq_along(pv_cols), function(i) {
    sapply(1:m_full, function(j) {
      sum(is.na(complete(imp_nor_list[[i]], j)[, final_preds_12]))
    })
  })
  colnames(nor_missing) <- pv_cols
  rownames(nor_missing) <- paste0("imp_", 1:m_full)
  print(nor_missing)
  cat("All zero?", all(nor_missing == 0), "\n")
  
  cat("\n--- Missing values after imputation: Finland |", label, "---\n")
  fin_missing <- sapply(seq_along(pv_cols), function(i) {
    sapply(1:m_full, function(j) {
      sum(is.na(complete(imp_fin_list[[i]], j)[, final_preds_12]))
    })
  })
  colnames(fin_missing) <- pv_cols
  rownames(fin_missing) <- paste0("imp_", 1:m_full)
  print(fin_missing)
  cat("All zero?", all(fin_missing == 0), "\n")
  
  # ---------------------------------------------------------------------------
  # Extract imputed datasets
  # ---------------------------------------------------------------------------
  
  imp_datasets_nor <- extract_imputed(imp_nor_list, pv_cols, m = m_full)
  imp_datasets_fin <- extract_imputed(imp_fin_list, pv_cols, m = m_full)
  
  cat("\n--- Imputed datasets extracted |", label, "---\n")
  cat("Norway:  ", length(imp_datasets_nor), "PVs x",
      length(imp_datasets_nor[[1]]), "imputations =",
      length(imp_datasets_nor) * length(imp_datasets_nor[[1]]), "total datasets\n")
  cat("Finland: ", length(imp_datasets_fin), "PVs x",
      length(imp_datasets_fin[[1]]), "imputations =",
      length(imp_datasets_fin) * length(imp_datasets_fin[[1]]), "total datasets\n")
  
  # Column structure
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
  
  cat("Column structure consistent — Norway:", nor_cols_consistent, "\n")
  cat("Column structure consistent — Finland:", fin_cols_consistent, "\n")
  
  # Average predictor means
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
  
  # Compare against observed means
  cat("\n--- Observed predictor means (available cases): Norway ---\n")
  print(round(colMeans(STU_NOR_12[, final_cont_preds_12], na.rm = TRUE), 3))
  
  cat("\n--- Observed predictor means (available cases): Finland ---\n")
  print(round(colMeans(STU_FIN_12[, final_cont_preds_12], na.rm = TRUE), 3))
  
  # Row counts
  cat("\n--- Row counts consistent: Norway |", label, "---\n")
  nor_rows <- all(sapply(seq_along(pv_cols), function(i)
    sapply(1:m_full, function(j)
      nrow(imp_datasets_nor[[i]][[j]]) == nrow(STU_NOR_12))))
  cat("All datasets have", nrow(STU_NOR_12), "rows:", nor_rows, "\n")
  
  cat("\n--- Row counts consistent: Finland |", label, "---\n")
  fin_rows <- all(sapply(seq_along(pv_cols), function(i)
    sapply(1:m_full, function(j)
      nrow(imp_datasets_fin[[i]][[j]]) == nrow(STU_FIN_12))))
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
# Step 15A: Prepare inputs — WITH QuestID
# ------------------------------------------------------------------------------

base_cols_12_withQ <- c(final_preds_12, "W_FSTUWT", "QuestID")

stopifnot(all(base_cols_12_withQ %in% names(STU_NOR_12)))
stopifnot(all(base_cols_12_withQ %in% names(STU_FIN_12)))

pv_data_nor_12_withQ <- make_pv_datasets(STU_NOR_12, pv_cols, base_cols_12_withQ)
pv_data_fin_12_withQ <- make_pv_datasets(STU_FIN_12, pv_cols, base_cols_12_withQ)

cat("\n--- PV datasets prepared: 2012 | with QuestID ---\n")
cat("Norway: ", length(pv_data_nor_12_withQ), "datasets, each with",
    ncol(pv_data_nor_12_withQ[[1]]), "columns and",
    nrow(pv_data_nor_12_withQ[[1]]), "rows\n")
cat("Finland:", length(pv_data_fin_12_withQ), "datasets, each with",
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

cat("\n--- Methods identical across countries? | with QuestID ---\n")
print(all(config_12_withQ$method == config_12_withQ_fin$method))

cat("\n--- Predictor matrices identical across countries? | with QuestID ---\n")
print(all(config_12_withQ$predictorMatrix == config_12_withQ_fin$predictorMatrix))

# ------------------------------------------------------------------------------
# Step 15B: Prepare inputs — WITHOUT QuestID
# ------------------------------------------------------------------------------

base_cols_12_noQ <- c(final_preds_12, "W_FSTUWT")

stopifnot(all(base_cols_12_noQ %in% names(STU_NOR_12)))
stopifnot(all(base_cols_12_noQ %in% names(STU_FIN_12)))

pv_data_nor_12_noQ <- make_pv_datasets(STU_NOR_12, pv_cols, base_cols_12_noQ)
pv_data_fin_12_noQ <- make_pv_datasets(STU_FIN_12, pv_cols, base_cols_12_noQ)

cat("\n--- PV datasets prepared: 2012 | without QuestID ---\n")
cat("Norway: ", length(pv_data_nor_12_noQ), "datasets, each with",
    ncol(pv_data_nor_12_noQ[[1]]), "columns and",
    nrow(pv_data_nor_12_noQ[[1]]), "rows\n")
cat("Finland:", length(pv_data_fin_12_noQ), "datasets, each with",
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

cat("\n--- Methods identical across countries? | without QuestID ---\n")
print(all(config_12_noQ$method == config_12_noQ_fin$method))

cat("\n--- Predictor matrices identical across countries? | without QuestID ---\n")
print(all(config_12_noQ$predictorMatrix == config_12_noQ_fin$predictorMatrix))

# ------------------------------------------------------------------------------
# Step 17–21: Run both MI branches
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
# Comparison summary
# ------------------------------------------------------------------------------

mi_2012_withQ <- readRDS("mi_2012_withQ.rds")
mi_2012_noQ <- readRDS("mi_2012_noQ.rds")

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

# Final model: noQ selected based on convergence and sensitivity analysis
imp_datasets_nor_12 <- mi_2012_noQ$imp_datasets_nor
imp_datasets_fin_12 <- mi_2012_noQ$imp_datasets_fin

cat("\n--- Final imputed datasets confirmed: 2012 ---\n")
cat("Norway:  200 datasets (5 PVs x 40 imputations)\n")
cat("Finland: 200 datasets (5 PVs x 40 imputations)\n")
cat("Model: noQ branch (QuestID excluded from imputation model)\n")

# ==============================================================================
# Analysis script — PISA 2012
# Pooling structure:
#   Stage 1: MIcombine across m imputations within each PV
#   Stage 2: PV combination rules across pooled PV estimates
# ==============================================================================

# ------------------------------------------------------------------------------
# Step 1: Add replicate weights back to imputed datasets
# Guard against duplicate columns if function is called more than once
# ------------------------------------------------------------------------------

rw_nor_12 <- STU_NOR_12[, w_reps]
rw_fin_12 <- STU_FIN_12[, w_reps]

add_replicate_weights <- function(imp_datasets, rw, w_reps) {
  lapply(imp_datasets, function(pv_list) {
    lapply(pv_list, function(dat) {
      stopifnot(nrow(dat) == nrow(rw))
      if (any(w_reps %in% names(dat))) return(dat)
      cbind(dat, rw)
    })
  })
}

imp_datasets_nor_12 <- add_replicate_weights(imp_datasets_nor_12, rw_nor_12, w_reps)
imp_datasets_fin_12 <- add_replicate_weights(imp_datasets_fin_12, rw_fin_12, w_reps)

cat("\n--- Replicate weights added ---\n")
cat("Norway columns:", ncol(imp_datasets_nor_12[[1]][[1]]), "\n")
cat("W_FSTR1 present:", "W_FSTR1" %in% names(imp_datasets_nor_12[[1]][[1]]), "\n")
cat("W_FSTR80 present:", "W_FSTR80" %in% names(imp_datasets_nor_12[[1]][[1]]), "\n")

# ------------------------------------------------------------------------------
# Step 2: Define regression formula and BRR design builder
# ------------------------------------------------------------------------------

reg_formula_12 <- PV_MATH ~ ESCS + LANGN + MATHEFF + ANXMAT + TEACHSUP

make_brr_design_12 <- function(dat) {
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
#
# Stage 1: within each PV, pool m imputations using MIcombine
#          Rubin's correction factor (1 + 1/m) uses true m = 40
#          Result per PV: pooled coefficient vector + variance-covariance matrix
#
# Stage 2: combine Q pooled PV estimates using PV combination rules
#          point estimate = mean of Q PV estimates
#          total variance = avg_pv_var + (1 + 1/Q) * between_var
#          where avg_pv_var = average variance of PV-specific pooled estimates
#          (already contains sampling variance + MI uncertainty within each PV)
#          and between_var = variance of coefficients across PVs
#          (captures measurement uncertainty from plausible values)
# ------------------------------------------------------------------------------

pool_regression_nested <- function(imp_datasets, formula, label) {
  
  cat("\n--- Running nested regression pipeline:", label, "---\n")
  
  Q <- length(imp_datasets)
  m <- length(imp_datasets[[1]])
  
  cat("PVs:", Q, "| Imputations per PV:", m, "\n")
  
  # Stage 1: pool m imputations within each PV
  pv_coefs <- vector("list", Q)
  pv_vcovs <- vector("list", Q)
  
  for (i in seq_len(Q)) {
    cat("  Stage 1 — PV", i, "of", Q, ": fitting", m, "models\n")
    
    pv_models <- lapply(imp_datasets[[i]], function(dat) {
      design <- make_brr_design_12(dat)
      svyglm(formula, design = design, family = gaussian())
    })
    
    pooled_pv     <- MIcombine(pv_models)
    pv_coefs[[i]] <- coef(pooled_pv)
    pv_vcovs[[i]] <- vcov(pooled_pv)
  }
  
  # Verify coefficient names are consistent across all PVs
  stopifnot(all(sapply(pv_coefs, function(x)
    identical(names(x), names(pv_coefs[[1]])))))
  
  # Stage 2: combine Q PV estimates
  cat("\n  Stage 2 — combining", Q, "PV estimates\n")
  
  coef_mat <- do.call(rbind, pv_coefs)
  var_mat  <- do.call(rbind, lapply(pv_vcovs, diag))
  
  # Final point estimates
  final_coefs <- colMeans(coef_mat)
  
  # Total variance
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

reg_nor_12 <- pool_regression_nested(
  imp_datasets_nor_12, reg_formula_12, "Norway 2012"
)

reg_fin_12 <- pool_regression_nested(
  imp_datasets_fin_12, reg_formula_12, "Finland 2012"
)

# ------------------------------------------------------------------------------
# Step 4: Format regression results
# p-values use normal approximation — appropriate after two-stage pooling
# where final df does not reduce to a simple formula
# ------------------------------------------------------------------------------

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

reg_results_nor_12 <- format_reg_results(reg_nor_12, "NOR", 2012)
reg_results_fin_12 <- format_reg_results(reg_fin_12, "FIN", 2012)
reg_results_12     <- rbind(reg_results_nor_12, reg_results_fin_12)

cat("\n--- Pooled regression results: Norway 2012 ---\n")
print(reg_results_nor_12)

cat("\n--- Pooled regression results: Finland 2012 ---\n")
print(reg_results_fin_12)

# ------------------------------------------------------------------------------
# Step 5: Compute pooled weighted means for OBD endowments
# For OBD input, only pooled point estimates are needed.
# Weighted means are averaged across all PV x imputation datasets.
# No pooled variance or significance testing is computed for these means here.
# ------------------------------------------------------------------------------

compute_pooled_means <- function(imp_datasets, cont_preds, label) {
  
  cat("\n--- Computing weighted means:", label, "---\n")
  
  all_means <- list()
  
  for (i in seq_along(imp_datasets)) {
    for (j in seq_along(imp_datasets[[i]])) {
      
      dat    <- imp_datasets[[i]][[j]]
      design <- make_brr_design_12(dat)
      
      # Continuous predictor means
      cont_means <- as.numeric(
        svymean(as.formula(paste("~",
                                 paste(cont_preds, collapse = " + "))),
                design, na.rm = TRUE)
      )
      names(cont_means) <- cont_preds
      
      # LANGN: explicitly name levels to avoid mislabelling
      langn_obj  <- svymean(~ LANGN, design, na.rm = TRUE)
      langn_mean <- as.numeric(langn_obj)
      names(langn_mean) <- c("LANGN_Majority", "LANGN_Other")
      
      # Outcome mean
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

pooled_means_nor_12 <- compute_pooled_means(
  imp_datasets_nor_12, final_cont_preds_12, "Norway 2012"
)

pooled_means_fin_12 <- compute_pooled_means(
  imp_datasets_fin_12, final_cont_preds_12, "Finland 2012"
)

cat("\n--- Pooled weighted means: Norway 2012 ---\n")
print(round(pooled_means_nor_12, 4))

cat("\n--- Pooled weighted means: Finland 2012 ---\n")
print(round(pooled_means_fin_12, 4))

means_table_12 <- data.frame(
  variable           = names(pooled_means_nor_12),
  mean_NOR           = round(pooled_means_nor_12, 4),
  mean_FIN           = round(pooled_means_fin_12, 4),
  diff_NOR_minus_FIN = round(pooled_means_nor_12 - pooled_means_fin_12, 4),
  row.names          = NULL
)

cat("\n--- Means comparison table: 2012 ---\n")
print(means_table_12)

# ------------------------------------------------------------------------------
# Step 6: Export to CSV for Excel OBD calculation
# ------------------------------------------------------------------------------

write.csv(reg_results_12, "reg_results_2012.csv", row.names = FALSE)
write.csv(means_table_12, "means_table_2012.csv", row.names = FALSE)

cat("\n--- Files saved ---\n")
cat("reg_results_2012.csv\n")
cat("means_table_2012.csv\n")

# plot(mi_2012_noQ$imp_nor_list[[1]])
# plot(mi_2012_noQ$imp_fin_list[[1]])

# ==============================================================================
# Extract pooled weighted means WITH standard errors — PISA 2012
# ==============================================================================

# Note: uses make_brr_design_12 and imp_datasets_nor_12 / imp_datasets_fin_12
# Reuse the same compute_pooled_means_with_se function but with 2012 design

compute_pooled_means_with_se_12 <- function(imp_datasets, cont_preds, label) {
  
  cat("\n--- Computing pooled means with SEs:", label, "---\n")
  
  Q <- length(imp_datasets)
  m <- length(imp_datasets[[1]])
  
  pv_means_list <- vector("list", Q)
  pv_vars_list  <- vector("list", Q)
  
  for (i in seq_len(Q)) {
    
    imp_means <- matrix(NA, nrow = m, ncol = length(cont_preds) + 3)
    
    for (j in seq_len(m)) {
      dat    <- imp_datasets[[i]][[j]]
      design <- make_brr_design_12(dat)
      
      cont_obj   <- svymean(as.formula(paste("~", paste(cont_preds, collapse = " + "))), design, na.rm = TRUE)
      cont_est   <- as.numeric(cont_obj)
      names(cont_est) <- cont_preds
      
      langn_obj  <- svymean(~ LANGN, design, na.rm = TRUE)
      langn_est  <- as.numeric(langn_obj)
      names(langn_est) <- names(coef(langn_obj))
      
      pv_est <- as.numeric(svymean(~ PV_MATH, design, na.rm = TRUE))
      names(pv_est) <- "PV_MATH"
      
      imp_means[j, ] <- c(cont_est, langn_est, pv_est)
    }
    
    var_names <- c(cont_preds, names(coef(langn_obj)), "PV_MATH")
    colnames(imp_means) <- var_names
    pv_means_list[[i]] <- colMeans(imp_means)
    
    imp_ses <- matrix(NA, nrow = m, ncol = length(var_names))
    colnames(imp_ses) <- var_names
    
    for (j in seq_len(m)) {
      dat    <- imp_datasets[[i]][[j]]
      design <- make_brr_design_12(dat)
      
      cont_obj   <- svymean(as.formula(paste("~", paste(cont_preds, collapse = " + "))), design, na.rm = TRUE)
      langn_obj  <- svymean(~ LANGN, design, na.rm = TRUE)
      pv_obj     <- svymean(~ PV_MATH, design, na.rm = TRUE)
      
      imp_ses[j, cont_preds]             <- SE(cont_obj)
      imp_ses[j, names(coef(langn_obj))] <- SE(langn_obj)
      imp_ses[j, "PV_MATH"]             <- SE(pv_obj)
    }
    
    within_var  <- colMeans(imp_ses^2)
    between_var <- apply(imp_means, 2, var)
    total_var   <- within_var + (1 + 1/m) * between_var
    
    pv_vars_list[[i]] <- total_var
    
    cat("  PV", i, "complete\n")
  }
  
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

# Run for Norway and Finland 2012
means_se_nor_12 <- compute_pooled_means_with_se_12(
  imp_datasets_nor_12, final_cont_preds_12, "Norway 2012"
)

means_se_fin_12 <- compute_pooled_means_with_se_12(
  imp_datasets_fin_12, final_cont_preds_12, "Finland 2012"
)

cat("\n--- Pooled means with SEs: Norway 2012 ---\n")
print(means_se_nor_12)

cat("\n--- Pooled means with SEs: Finland 2012 ---\n")
print(means_se_fin_12)

write.csv(means_se_nor_12, "means_se_nor_2012.csv", row.names = FALSE)
write.csv(means_se_fin_12, "means_se_fin_2012.csv", row.names = FALSE)

cat("\nFiles saved: means_se_nor_2012.csv, means_se_fin_2012.csv\n")