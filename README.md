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
  - Candidate-variable inspection before recoding
  - Strip SPSS labels and recode missing-value sentinel codes to NA
  - Post-cleaning validation
  - Recode LANGN to a binary factor
  - Recode IMMIG to a three-level factor
4. IMMIG vs LANGN — diagnostic comparison
5. PSU structure and stratum filtering
6. Build BRR survey designs
7. Missingness analysis
  - Weighted missingness per predictor
  - Complete-case summary (unweighted and weighted)
  - MAR plausibility — survey-weighted logistic regressions of missingness
  - Group differences in missingness (optional descriptive diagnostics)
  - Little's MCAR test (supplementary, unweighted)
8. Pre-imputation predictor screening
  - Weighted predictor–math correlations (averaged across PVs)
  - Weighted inter-predictor correlation matrix
  - VIF (unweighted, diagnostic only)
  - Country-level entirely-missing check
9. Final predictor set
10. Multiple imputation (MICE)
  - Build predictor matrix and method vector
  - Build PV-specific datasets
  - Test runs (small m, maxit) — confirms config before full execution
  - Full imputation — Norway (10 PVs × m = 20)
  - Full imputation — Finland (10 PVs × m = 20)
  - Validate imputations
  - Extract imputed datasets
11. Pooled regression analysis
  - Helper functions
  - Add replicate weights to imputed datasets
  - Regression formula and BRR design builder
  - Two-stage pooling
  - Format regression results
12. Pooled weighted means (point estimates for OBD)  
13. Pooled means with standard errors (for cross-cycle inference)

PISA 2012 analysis workflow:
0. Packages and session info
1. Load data
  - Load PISA 2012 Student Questionnaire
  - Merge rescaled ESCS for cross-cycle trend analysis
2. Variable selection and country subsetting
  - Define column groups
  - Subset by country
3. Recoding
  - Candidate-variable inspection before recoding
  - Strip SPSS labels and recode missing-value sentinel codes to NA
  - Post-cleaning validation
  - Recode LANGN to a binary factor
  - Recode IMMIG to a three-level factor
  - Recode QuestID (booklet-form indicator, 2012-specific)
4. IMMIG vs LANGN — diagnostic comparison
5. PSU structure and stratum filtering
6. Build BRR survey designs
7. Missingness analysis
  - Weighted missingness per predictor
  - Booklet-aware missingness diagnostic (2012-specific)
  - Complete-case summary (unweighted and weighted)
  - MAR plausibility — survey-weighted logistic regressions of missingness
  - Little's MCAR test (supplementary, unweighted)
8. Pre-imputation predictor screening
  - Weighted inter-predictor correlation matrix
  - VIF (unweighted, diagnostic only) — booklet-aware
  - Country-level entirely-missing check
9. Final predictor set
10. Multiple imputation (MICE)
  - Helper functions for MI pipeline
  - Configuration — Branch A (with QuestID as auxiliary predictor)
  - Configuration — Branch B (without QuestID)
  - Run both MI branches
  - Compare branches and select final imputed datasets
11. Pooled regression analysis
  - Helper functions
  - Add replicate weights to imputed datasets
  - Regression formula and BRR design builder
  - Two-stage pooling
  - Format regression results
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
