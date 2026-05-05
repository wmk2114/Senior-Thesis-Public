################################################################################
### REPLICATION CODE FOR WE DON'T DO THAT: NATIONAL IDENTITY AND NUCLEAR NON-USE
### TABLES 1, 2, 3
################################################################################

################################################################################
# DISCLAIMER
# ChatGPT was used to generate pseudocode.
# The pseudocode and documentation were subsequently edited
# and reviewed by the author, Wyatt King.
# Most code was initially written by Wyatt, with occasional syntax issues 
# fixed using ChatGPT. All areas where ChatGPT was used more intensively
# are noted accordingly.
################################################################################

################################################################################
# STEP 0: ENVIRONMENT SETUP
# Ensure that the following packages are installed:
# install.packages("tidyverse", "haven", "survey", "readxl", "speedycode", "readroper")
################################################################################

rm(list = ls())
setwd("~/Desktop/Columbia 2025/Senior Thesis/Data Analysis")

clean_env <- new.env()
source("Data Preprocessing.R", local = clean_env)
survey_df <- clean_env$survey_cleaned
lasso_covariates<- clean_env$lasso_covariates
ols_models_func<- clean_env$ols_models_func

library(modelsummary)
library(glmnet)
library(estimatr)

################################################################################
# STEP 1: OLS REGRESSION MODELS
#
# Pseudocode:
#
# 1. Define a set of demographic and treatment covariates.
# 2. Construct regression formulas for ICW and PCA outcome indices.
# 3. Estimate four OLS models:
#       - ICW index
#       - PCA index
#       - ICW index with Likert imputation
#       - PCA index with Likert imputation
################################################################################

outcomes <- c(
  "attack_index_likert_icw_z",
  "attack_index_likert_pca_z",
  "attack_index_na_icw_z",
  "attack_index_na_pca_z"
)

ols_models <- lapply(outcomes, function(x) {
  ols_models_func(
    outcome = x,
    blocks = "Party_clean",
    treatments = c("NFU", "National_Identity"),
    data = survey_df,
    controls = c("Gender_clean", "Border_Pakistan_clean", "Age_1", "Party_clean", 
                 "Education_clean", "Religion_clean"),
    weight = "weight_std"
  )
})

coef_map <- c(
  "(Intercept)" = "Constant",
  "NFU1" = "NFU",
  "National_Identity1" = "Identity",
  "NFU1:National_Identity1" = "NFU x Identity",
  "Party_clean2" = "BJP",
  "Party_clean3" = "INC",
  "Religion_clean1" = "Not Hindu"
)

modelsummary(
  list(ols_models[[1]]),
  stars = TRUE,
  coef_map = coef_map,
  gof_omit = "AIC|BIC|Log.Lik|RMSE",
  notes = c(
    "Robust HC2 standard errors in parentheses. 
    Block fixed effects for party and LASSO-selected fixed effects included but not shown. 
    Midpoint imputation used. Respondents who fail manipulation checks are not dropped."
  ),
  title = "Estimated Effects of NFU and Identity on Composite Nuclear Attitudes",
  output = "latex"
)

################################################################################
# HETEROGENEOUS EFFECTS
################################################################################

het_icw<- lm_robust(attack_index_likert_icw_z~ NFU+National_Identity+ Party_clean
                    + Party_clean:NFU + Party_clean:National_Identity + National_Identity:NFU
                    +Religion_clean, 
                     weights = weight_std, 
                     data=survey_df)

het_pca<- lm_robust(attack_index_likert_pca_z~ Party_clean+NFU+National_Identity
                    + Party_clean:NFU + Party_clean:National_Identity + National_Identity:NFU
                    +Religion_clean,
                    weights = weight_std, 
                    data=survey_df)

coef_map <- c(
  "NFU1" = "NFU",
  "National_Identity1" = "Identity",
  "NFU1:National_Identity1" = "NFU x Identity",
  "Party_clean2" = "BJP",
  "Party_clean3" = "INC",
  "Party_clean2:NFU1" = "BJP x NFU",
  "Party_clean3:NFU1" = "INC x NFU",
  "Party_clean2:National_Identity1" = "BJP x Identity",
  "Party_clean3:National_Identity1" = "INC x Identity",
  "Party_clean2:NFU1:National_Identity1" = "BJP x NFU x Identity",
  "Party_clean3:NFU1:National_Identity1" = "INC x NFU x Identity"
)

modelsummary(
  list(het_icw),
  stars = TRUE,
  coef_map = coef_map,
  gof_omit = "AIC|BIC|Log.Lik|RMSE",
  notes = c(
    "Robust HC2 standard errors in parentheses. 
    Block fixed effects for party and LASSO-selected fixed effects included but not shown. 
    Midpoint imputation used. Respondents who fail manipulation checks are not dropped."
  ),
  title = "Estimated Effects of NFU and Identity on Composite Nuclear Attitudes",
  output = "latex"
)

################################################################################
# TREATMENT EFFECTIVENESS
################################################################################

id_m1<- lm_robust(NationalID_Attack_LikertImputed~NFU*National_Identity+Party_clean+
                    Religion_clean, data=survey_df, weights = weight_std)

coef_map <- c(
  "(Intercept)" = "Constant",
  "NFU1" = "NFU",
  "National_Identity1" = "Identity",
  "NFU1:National_Identity1" = "NFU x Identity",
  "Party_clean2" = "BJP",
  "Party_clean3" = "INC",
  "Religion_clean1" = "Not Hindu"
)

modelsummary(
  id_m1,
  stars = TRUE,
  coef_map = coef_map,
  gof_omit = "AIC|BIC|Log.Lik|RMSE",
  notes = c(
    "Robust HC2 standard errors in parentheses. 
    Block fixed effects for party and LASSO-selected fixed effects included for age and party but not shown. 
    Midpoint imputation used. Respondents who fail manipulation checks are not dropped."
  ),
  title = "Estimated Effects of NFU and Identity on Composite Nuclear Attitudes",
  output = "latex"
)