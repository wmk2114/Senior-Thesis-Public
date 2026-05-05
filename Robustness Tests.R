################################################################################
### REPLICATION CODE FOR WE DON'T DO THAT: NATIONAL IDENTITY AND NUCLEAR NON-USE
### ROBUSTNESS TESTS
################################################################################

################################################################################
# STEP 0: ENVIRONMENT SETUP
################################################################################

rm(list = ls())
setwd("~/Desktop/Columbia 2025/Senior Thesis/Data Analysis")

clean_env <- new.env()

source("Data Preprocessing.R", local = clean_env)

survey_df <- clean_env$survey_cleaned
survey_df_manip<- clean_env$survey_pass_manipulation_checks
lasso_covariates<- clean_env$lasso_covariates
ols_models_func<- clean_env$ols_models_func
data_1994 <- clean_env$data_1994

source("Main Analysis.R", local = clean_env)
outcomes<- clean_env$outcomes
ols_models<- clean_env$ols_models

library(modelsummary)
library(glmnet)
library(estimatr)

################################################################################
# STEP 1: OLS MODELS
################################################################################

### DROP PEOPLE WHO FAIL ATTENTION CHECKS
ols_models_robust <- lapply(outcomes, function(x) {
  ols_models_func(
    outcome = x,
    blocks = "Party_clean",
    treatments = c("NFU", "National_Identity"),
    data = survey_df_manip,
    controls = c("Gender_clean", "Age_1", "Party_clean", 
                 "Education_clean", "Religion_clean"),
    weight = "weight_std"
  )
})

### INCLUDE ALL CONTROLS
all_controls1 <- lm_robust(attack_index_likert_icw ~ NFU*National_Identity + Gender_clean + Age_1 +
                            Party_clean + Education_clean + Religion_clean + Border_Pakistan_clean, 
                          data = survey_df, weights= weight_std, se_type = "HC2")

all_controls2 <- lm_robust(attack_index_likert_pca ~ NFU*National_Identity + Gender_clean + Age_1 +
                            Party_clean + Education_clean + Religion_clean + Border_Pakistan_clean, 
                          data = survey_df, weights= weight_std, se_type = "HC2")

all_controls1_na <- lm_robust(attack_index_na_icw ~ NFU*National_Identity + Gender_clean + Age_1 +
                             Party_clean + Education_clean + Religion_clean + Border_Pakistan_clean, 
                           data = survey_df, weights= weight_std, se_type = "HC2")

all_controls2_na <- lm_robust(attack_index_na_pca ~ NFU*National_Identity + Gender_clean + Age_1 +
                             Party_clean + Education_clean + Religion_clean + Border_Pakistan_clean, 
                           data = survey_df, weights= weight_std, se_type = "HC2")

all_controls1_manip <- lm_robust(attack_index_likert_icw ~ NFU*National_Identity + Gender_clean + Age_1 +
                             Party_clean + Education_clean + Religion_clean + Border_Pakistan_clean, 
                           data = survey_df_manip, weights= weight_std, se_type = "HC2")

all_controls2_manip <- lm_robust(attack_index_likert_pca ~ NFU*National_Identity + Gender_clean + Age_1 +
                             Party_clean + Education_clean + Religion_clean + Border_Pakistan_clean, 
                           data = survey_df_manip, weights= weight_std, se_type = "HC2")

################################################################################
# HISTORICAL ATTITUDES
################################################################################

coef_map <- c(
  "(Intercept)" = "Constant",
  "SexFemale" = "Female",
  "Age" = "Age",
  "EducationSome Secondary" = "Some Secondary Schooling",
  "EducationSecondary Completed " = "Secondary Completed",
  "EducationUniversity+ " = "University",
  "ReligionMuslim" = "Muslim",
  "ReligionChristian" = "Christian",
  "ReligionSikh" = "Sikh",
  "ReligionOther" = "Other Religion"
)

regNPT<-lm_robust(NPT~Sex+Age+Education+Income+Religion, data=data_1994)
regNukeOption<-lm_robust(NukeOption~Sex+Age+Education+Income+Religion, data=data_1994)

modelsummary(
  list(regNPT, regNukeOption),
  estimate = "{estimate}{stars} ({std.error})",
  statistic = NULL,
  stars = TRUE,
  coef_map = coef_map,
  gof_omit = "AIC|BIC|Log.Lik|RMSE",
  notes = c(
    "Robust HC2 standard errors in parentheses."
  ),
  title = "Replicate \autoref{tab:ols-main-effects}, Full Table Output Including Controls",
  output = "latex"
)

################################################################################
# Threshold Analysis
################################################################################

ols_vars_likert_imputed<- colnames(survey_df)[grepl("_LikertImputed", colnames(survey_df)) 
                                              & !grepl("Dichot", colnames(survey_df))]

ols_vars_na_imputed<- colnames(survey_df)[grepl("_NAImputed", colnames(survey_df)) 
                                              & !grepl("Dichot", colnames(survey_df))]

thresholds <- 1:6
for (t in thresholds) {
  survey_df <- survey_df %>%
    
    mutate(across(
      .cols = all_of(ols_vars_likert_imputed),
      ~ if_else(is.na(.x), NA, ifelse(.x <= t, 0, 1)),
      .names = paste0("{.col}_Dichot_", t)
    )) %>%
    
    mutate(across(
      .cols = all_of(ols_vars_na_imputed),
      ~ if_else(is.na(.x), NA, ifelse(.x <= t, 0, 1)),
      .names = paste0("{.col}_Dichot_", t)
    ))
}

dep_likert_dichot_thresholds <- colnames(survey_df)[
  grepl("LikertImputed_Dichot_[1-6]$", colnames(survey_df))
]

rhs<- "~ NFU*National_Identity + Party_clean + Religion_clean" 
vec_formula<-sapply(dep_likert_dichot_thresholds, function(x) as.formula(paste0(x, rhs, collapse="~")))


dichot_regs_likert_thresholds <- lapply(
  vec_formula,
  function(x) lm_robust(x,data=survey_df, weights= weight_std)
)


################################################################################
# LATEX OUTPUTS
################################################################################

coef_map <- c(
  "(Intercept)" = "Constant",
  "NFU1" = "NFU",
  "National_Identity1" = "Identity",
  "NFU1:National_Identity1" = "NFU x Identity",
  "Party_clean2" = "BJP",
  "Party_clean3" = "INC",
  "Education_clean1" = "Bachelor's+",
  "Religion_clean1" = "Not Hindu",
  "Caste_clean1"= "General Category",
  "Caste_clean2" = "SC",
  "Cast_clean3" = "ST",
  "Caste_clean4" = "OBC",
  "Gender_cleanMale" = "Male",
  "Income_clean1" = "< Rs. 1,500",
  "Income_clean2" = "Rs. 1,501-3,500",
  "Income_clean3" = "Rs. 3,501-5,500",
  "Income_clean4" = "Rs. 5,501-6,500",
  "Income_clean5" = "Rs. 6,501-8,500",
  "Income_clean6" = "Rs. 8,501-10,000",
  "Income_clean7" = "Rs. 10,001-15,000",
  "Income_clean8" = "Rs. 15,001-20,000",
  "Income_clean9" = "Rs. 20,001-40,000",
  "Income_clean10" = "Rs. >40,001",
  "Age_1" = "Age",
  "Border_Pakistan_clean1" = "State Borders Pakistan"
)


# KEEP FAILED MANIPULATION CHECKS, IMPUTE MIDPOINT
modelsummary(
  list(ols_models[[1]], ols_models[[2]]),
  estimate = "{estimate}{stars} ({std.error})",
  statistic = NULL,
  stars = TRUE,
  coef_map = coef_map,
  gof_omit = "AIC|BIC|Log.Lik|RMSE",
  notes = c(
    "Robust HC2 standard errors in parentheses."
  ),
  title = "Replicate \autoref{tab:ols-main-effects}, Full Table Output Including Controls",
  output = "latex"
)

# KEEP FAILED MANIPULATION CHECKS, IMPUTE NAs
modelsummary(
  list(ols_models[[3]], ols_models[[4]]),
  estimate = "{estimate}{stars} ({std.error})",
  statistic = NULL,
  stars = TRUE,
  coef_map = coef_map,
  gof_omit = "AIC|BIC|Log.Lik|RMSE",
  notes = c(
    "Robust HC2 standard errors in parentheses. 
    Party is a block fixed effect; all other covariates are LASSO-selected. 
    NA imputation, keeping respondents that fail manipulation checks."
  ),
  title = "Replicate \autoref{tab:ols-main-effects}, Keep Respondents who Fail Manipulation Checks & Use NA Imputation",
  output = "latex"
)

# DROP FAILED MANIPULATION CHECKS, IMPUTE MIDPOINT
modelsummary(
  list(ols_models_robust[[1]], ols_models_robust[[2]]),
  estimate = "{estimate}{stars} ({std.error})",
  statistic = NULL,
  stars = TRUE,
  coef_map = coef_map,
  gof_omit = "AIC|BIC|Log.Lik|RMSE",
  notes = c(
    "Robust HC2 standard errors in parentheses. 
    Block fixed effects for party and LASSO-selected fixed effects included but not shown."
  ),
  title = "Replicate \autoref{tab:ols-main-effects}, Drop Respondents that Fail Attention Checks & Use Midpoint Imputation",
  output = "latex"
)

### DROP FAILED MANIPULATION CHECKS, IMPUTE NAs
modelsummary(
  list(ols_models_robust[[3]], ols_models_robust[[4]]),
  estimate = "{estimate}{stars} ({std.error})",
  statistic = NULL,
  stars = TRUE,
  coef_map = coef_map,
  gof_omit = "AIC|BIC|Log.Lik|RMSE",
  notes = c(
    "Robust HC2 standard errors in parentheses. 
    Block fixed effects for party and LASSO-selected fixed effects included but not shown."
  ),
  title = "Replicate \autoref{tab:ols-main-effects}, Drop Respondents that Fail Attention Checks & Use NA Imputation",
  output = "latex"
)

### INCLUDE ALL CONTROL VARIABLES
modelsummary(
  list("ICW"=all_controls1, "PCA"=all_controls2),
  estimate = "{estimate}{stars} ({std.error})",
  statistic = NULL,
  stars = TRUE,
  coef_map = coef_map,
  gof_omit = "AIC|BIC|Log.Lik|RMSE",
  notes = c(
    "Robust HC2 standard errors in parentheses. 
    Keep all people who failed manipulation checks;
    use midpoint imputation."
  ),
  title = "Replicate \autoref{tab:ols-main-effects}, Include All Control Variables",
  output = "latex"
)

modelsummary(
  list("ICW"=all_controls1_manip, "PCA"=all_controls2_manip),
  estimate = "{estimate}{stars} ({std.error})",
  statistic = NULL,
  stars = TRUE,
  coef_map = coef_map,
  gof_omit = "AIC|BIC|Log.Lik|RMSE",
  notes = c(
    "Robust HC2 standard errors in parentheses. 
    Drop people who failed attention checks;
    use midpoint imputation."
  ),
  title = "Replicate \autoref{tab:ols-main-effects}, Include All Control Variables",
  output = "latex"
)

# THRESHOLD ANALYSES

list_names<- c("Ahimsa", "Approve", "Ethical", "NationalID", "Prefer", "Protest", "Vote")
latex_outputs<- vector(mode = "list", length = length(list_names))

for(j in seq_along(list_names)){
  get_regs<- dichot_regs_likert_thresholds[grepl(list_names[j], dichot_regs_likert_thresholds)]
  names(get_regs)<- c(1, 2, 3, 4, 5, 6)
  
  output<- modelsummary(
    get_regs,
    stars = TRUE,
    coef_map = coef_map,
    gof_omit = "AIC|BIC|Log.Lik|RMSE",
    notes = c(
    "Robust HC2 standard errors in parentheses. 
    Midpoint imputation used. Respondents who fail manipulation checks are not dropped."
    ),
    title = paste0("Estimated Effects of NFU and Identity on ", list_names[j]),
    output = "latex"
    
  )
  
  latex_outputs[[j]]<-output
}

