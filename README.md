# MAE4191_masters_thesis_project
MAE4191 Master's Thesis

This repository contains the code and materials needed to reproduce the analyses for the master’s thesis. The Oaxaca–Blinder decomposition (OBD) was performed in Excel using processed outputs from R (see file: OBD).

Files:
final_code_2022.R — Full analysis pipeline for PISA 2022
final_code_2012.R — Full analysis pipeline for PISA 2012
OBD — Oaxaca–Blinder decomposition calculations

Analytical Workflow Overview
The analytical procedure follows the same conceptual structure for both datasets, with necessary adjustments due to differences in PISA design (e.g., number of plausible values, variable availability, and scaling).

PISA 2022 analysis workflow:
0. Packages and session info
1. Load data
2. Variable selection and country subsetting
  - Define column groups
  - Subset by country
3. Recoding
  3.0  Candidate-variable inspection before recoding
  3.1  Strip SPSS labels and recode missing-value sentinel codes to NA
  3.2  Post-cleaning validation
  3.3  Recode LANGN to a binary factor
  3.4  Recode IMMIG to a three-level factor
4. IMMIG vs LANGN — diagnostic comparison
5. PSU structure and stratum filtering
6. Build BRR survey designs
7. Missingness analysis
  7.1  Weighted missingness per predictor
  7.2  Complete-case summary (unweighted and weighted)
  7.3  MAR plausibility — survey-weighted logistic regressions of missingness
  7.4  Group differences in missingness (optional descriptive diagnostics)
  7.5  Little's MCAR test (supplementary, unweighted)
8. Pre-imputation predictor screening
  8.1  Weighted predictor–math correlations (averaged across PVs)
  8.2  Weighted inter-predictor correlation matrix
  8.3  VIF (unweighted, diagnostic only)
  8.4  Country-level entirely-missing check
9. Final predictor set
10. Multiple imputation (MICE)
  10.1  Build predictor matrix and method vector
  10.2  Build PV-specific datasets
  10.3  Test runs (small m, maxit) — confirms config before full execution
  10.4  Full imputation — Norway (10 PVs × m = 20)
  10.5  Full imputation — Finland (10 PVs × m = 20)
  10.6  Validate imputations
  10.7  Extract imputed datasets
11. Pooled regression analysis  
  11.1  Helper functions
  11.2  Add replicate weights to imputed datasets
  11.3  Regression formula and BRR design builder
  11.4  Two-stage pooling
  11.5  Format regression results
12. Pooled weighted means (point estimates for OBD)  
13. Pooled means with standard errors (for cross-cycle inference)

PISA 2012 analysis workflow:
0. Packages and session info
1. Load data
   1.1  Load PISA 2012 Student Questionnaire
   1.2  Merge rescaled ESCS for cross-cycle trend analysis
2. Variable selection and country subsetting
   2.1  Define column groups
   2.2  Subset by country
3. Recoding
   3.0  Candidate-variable inspection before recoding
   3.1  Strip SPSS labels and recode missing-value sentinel codes to NA
   3.2  Post-cleaning validation
   3.3  Recode LANGN to a binary factor
   3.4  Recode IMMIG to a three-level factor
   3.5  Recode QuestID (booklet-form indicator, 2012-specific)
4. IMMIG vs LANGN — diagnostic comparison
5. PSU structure and stratum filtering
6. Build BRR survey designs
7. Missingness analysis
  7.1  Weighted missingness per predictor
  7.2  Booklet-aware missingness diagnostic (2012-specific)
  7.3  Complete-case summary (unweighted and weighted)
  7.4  MAR plausibility — survey-weighted logistic regressions of missingness
  7.5  Little's MCAR test (supplementary, unweighted)
8. Pre-imputation predictor screening
  8.1  Weighted inter-predictor correlation matrix
  8.2  VIF (unweighted, diagnostic only) — booklet-aware
  8.3  Country-level entirely-missing check
9. Final predictor set
10. Multiple imputation (MICE)
  10.1  Helper functions for MI pipeline
  10.2  Configuration — Branch A (with QuestID as auxiliary predictor)
  10.3  Configuration — Branch B (without QuestID)
  10.4  Run both MI branches
  10.5  Compare branches and select final imputed datasets
11. Pooled regression analysis
  11.1  Helper functions
  11.2  Add replicate weights to imputed datasets
  11.3  Regression formula and BRR design builder
  11.4  Two-stage pooling
  11.5  Format regression results
12. Pooled weighted means (point estimates for OBD)
13. Pooled means with standard errors (for cross-cycle inference)

Notes on Reproducibility:
All analyses rely on publicly available PISA datasets:
PISA 2012 Database
PISA 2022 Database
The code assumes access to raw SPSS (.sav) files and auxiliary files (e.g., escs_trend.csv)
File paths may need to be adjusted depending on the local environment

Contact
For questions regarding the code or analytical approach, contact the author.
