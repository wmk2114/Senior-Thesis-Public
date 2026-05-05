################################################################################
### REPLICATION CODE FOR WE DON'T DO THAT: NATIONAL IDENTITY AND NUCLEAR NON-USE
### DATA CLEANING
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
#
# Pseudocode:
# 
# 1. Clear the R workspace to remove previously stored objects.
# 2. Load required R packages for data manipulation, statistical modeling,
#    and regression table generation.
# 3. Set the working directory to the project folder containing the dataset.
# 4. Import the survey dataset from a .sav file.
################################################################################

rm(list = ls())
library(tidyverse)
library(haven)
library(survey)
library(readxl)
library(speedycode)
library(readroper)

survey<- read_sav("Thesis Data.sav")


################################################################################
# STEP 1: DATA CLEANING
#
# Pseudocode:
#
# 1. Identify all dependent variables related to nuclear attack approval by
#    selecting variables containing the string "_Attack".
#
# 2. Clean the dataset by performing the following operations:
#       a. Convert the survey completion date to a standard date format.
#       b. Remove responses submitted before March 11, 2026 (indicating simulated data).
#       c. Remove metadata columns that are not relevant to the analysis.
#
# 3. Construct treatment indicators:
#       - NFU treatment
#       - National Identity treatment
#
# 4. Handle "Don't Know" responses (coded as 98) in two ways:
#       a. Replace with the midpoint of the Likert scale (4).
#       b. Replace with NA (missing).
#
# 5. Convert relevant demographic and treatment variables to factors.
################################################################################

dep_vars<- colnames(survey)[grepl("_Attack", colnames(survey))]
survey_cleaned<- survey %>%
  
  # REMOVE SIMULATED DATA
  
  filter(
    StartDate >= ymd("2026-03-18")
  ) %>%
  
  select(-c(1:19)) %>%
  
  # FORMAT TREATMENT COLUMNS
  
  mutate(
    
    NFU= case_when(
      is.na(NFU_Confirm) & is.na(NFU_NI_Confirm) ~ 0,
      NFU_Confirm | NFU_NI_Confirm ~ 1
    ),
    
    National_Identity= case_when(
      is.na(Control_NI_Confirm) & is.na(NFU_NI_Confirm) ~ 0,
      Control_NI_Confirm | NFU_NI_Confirm ~ 1
    )) %>%
  
  # CREATE NEW COLUMNS, IMPUTING VALUES OF 4 FOR RESPONDENTS WHO SAY DON'T KNOW
  
  mutate(across(
    .cols = all_of(dep_vars),
    ~ if_else(.x == 98, 4, .x),
    .names = "{.col}_LikertImputed"
  )) %>%
  
  mutate(Party = case_when( # Reorder party
    is.na(Party) ~ NA,
    Party == 98 ~ 98,
    Party == 1 ~ 2, 
    Party == 2 ~ 3,
    Party == 3 ~ 1 # Neither becomes base category
  )) %>%
  
  # CREATE NEW COLUMNS, IMPUTING NA VALUES FOR RESPONDENTS WHO SAY DON'T KNOW
  
  mutate(across(
    .cols = all_of(dep_vars),
    ~ if_else(.x == 98, NA_real_, .x),
    .names = "{.col}_NAImputed"
  )) %>%
  
  mutate(AgeGrouped = case_when(
    Age_1 <= 29 & Age_1 >= 18 ~ "18-29",
    Age_1 > 29 & Age_1 < 45 ~ "30-44",
    Age_1 > 44 & Age_1 < 60 ~ "45-59",
    Age_1 > 59 ~ "60+"
  )) %>%
  
  mutate(Party_clean = case_when(
    Party > 90 ~ 1, # Impute neither for people who respond don't know
    Party <= 90 ~ Party
  )) %>%
  
  mutate(Religion_clean = case_when(
    is.na(Religion) | Religion != 1 ~ 1,  # Impute Hindu for people who select prefer not to say
    Religion == 1 ~ 0
  )) %>%
  
  mutate(Income_clean = case_when(
    is.na(Income) ~ 10, # Impute highest income bracket for people who do not report
    !is.na(Income) ~ Income
  )) %>%
  
  mutate(Education_clean = case_when(
    is.na(Education) | Education >= 90 ~ 0, # Impute bachelor's for people who do not report education
    Education > 4 ~ 1,
    Education <= 4 ~ 0
  )) %>%
  
  mutate(Gender_clean= case_when(
    is.na(Gender) | Gender == 1 | Gender > 2 ~ "Male",
    Gender == 2 ~ "Female"
  )) %>%
  
  mutate(Border_Pakistan_clean = case_when(
    is.na(State) ~ NA,
    State == 32 | State == 33 | State == 20 | State == 19 | State == 6 ~ 1,
    !(State == 32 | State == 33 | State == 20 | State == 19 | State == 6) ~ 0
  )) %>%
  
  mutate(across(.cols = c("Gender", "Gender_clean", "Party", "Party_clean", "Caste", "Border_Pakistan_clean",
                          "Religion", "Religion_clean", "Income", "Income_clean", "NFU", 
                          "National_Identity", "Education", "Education_clean"), as.factor)) %>%
  
  mutate(Treatment_Combined= NFU:National_Identity)

survey_cleaned<- survey_cleaned[-c(1:3),] # Remove surveys that I filled out to test Qualtrics

################################################################################
# STEP 2: CREATE DICHOTOMIZED VARIABLES
#
# Pseudocode:
#
# 1. Generate binary outcome variables from Likert-scale responses.
#
# 2. For NA-imputed variables:
#       - Keep NA values unchanged.
#       - Assign 1 if response >= 4 (opposition or indifference to nuclear use).
#       - Assign 0 if response < 4 (support for nuclear use).
#
# 3. Repeat the same dichotomization procedure for midpoint-imputed variables.
################################################################################

survey_cleaned<- survey_cleaned %>%
  
  ### DICHOTOMIZE FOR COLUMNS WITH IMPUTED NAs
  mutate(across(
    .cols = colnames(survey_cleaned)[grepl("_NAImputed",  colnames(survey_cleaned))],
    ~ if_else(is.na(.x), NA, if_else(.x >= 4, 1, 0)),
    .names = "{.col}_Dichot"
  )) %>%
  
  ### DICHOTOMIZE FOR COLUMNS WITH IMPUTED MIDPOINT
  mutate(across(
    .cols = colnames(survey_cleaned)[grepl("_LikertImputed",  colnames(survey_cleaned))],
    ~ if_else(is.na(.x), NA, if_else(.x >= 4, 1, 0)),
    .names = "{.col}_Dichot"
  ))


survey_cleaned$Protest_Attack_LikertImputed_Dichot<- ifelse(survey_cleaned$Protest_Attack_LikertImputed > 4,
                                                            1, 
                                                            0)

survey_cleaned$Vote_Attack_LikertImputed_Dichot<- ifelse(survey_cleaned$Vote_Attack_LikertImputed > 4,
                                                         1, 
                                                         0)

################################################################################
# STEP 3: SURVEY WEIGHTING
################################################################################

age_demos<- read_xls("DDW-0000C-14.xls", skip = 6)

total_population<- as.numeric(colnames(age_demos)[6])
age_15_19<- age_demos[age_demos$`All ages` %in% c("15-19") & age_demos$India=="India",]
total_15_19<- sum(age_15_19[,6])*.4
age_20_29<- age_demos[age_demos$`All ages` %in% c("20-24", "25-29") & age_demos$India=="India",]
total_18_29<-sum(age_20_29[,6])+total_15_19

age_30_44<- age_demos[age_demos$`All ages` %in% c("30-34", "35-39", "40-44") & age_demos$India=="India",]
total_30_44<-sum(age_30_44[,6])

age_45_60<- age_demos[age_demos$`All ages` %in% c("45-49", "50-54", "55-59") & age_demos$India=="India",]
total_45_60<-sum(age_45_60[,6])

age_60<- age_demos[13:17,]
total_60<- sum(age_60[,6])

total_over18<- sum(total_18_29, total_30_44, total_45_60, total_60)

prop_18_29<- total_18_29/total_over18
prop_30_44<- total_30_44/total_over18
prop_45_59<- total_45_60/total_over18
prop_60<- total_60/total_over18

design <- svydesign(ids = ~1, data = survey_cleaned)

design_raked <- rake(
  design,
  sample.margins = list(~Gender_clean, ~AgeGrouped),
  population.margins = list(
    data.frame(
      Gender_clean = c("Male","Female"),
      Freq = c(0.515, 0.485)
    ),
    data.frame(
      AgeGrouped = c("18-29", "30-44", "45-59", "60+"),
      Freq = c(prop_18_29, prop_30_44, prop_45_59, prop_60)
    )
  )
)

design_trim <- trimWeights(design_raked, upper = 4)
survey_cleaned$weight <- weights(design_raked)
survey_cleaned$weight_std <- survey_cleaned$weight / mean(survey_cleaned$weight)

################################################################################
# STEP 3: CONSTRUCT OUTCOME INDICES
#
# Two index construction methods are used:
#
# 1. Inverse Covariance Weighting (ICW)
# 2. Principal Components Analysis (PCA)
#
# Both indices are estimated using only respondents in the pure control group,
# defined as observations where:
#       NFU = 0
#       National_Identity = 0
################################################################################

################################################################################
# ICW INDEX CONSTRUCTION
# CREATED WITH CHATGPT, CROSS-COMPARED WITH ANDERSON (2008) EXPLANATION OF INDEX CREATION
################################################################################

make_icw_index <- function(data, cols, treat_vars, new_var_name = "icw_index") {
  
  Y <- data %>%
    dplyr::select(dplyr::all_of(cols)) %>%
    as.matrix()
  
  control_rows <- apply(data[, treat_vars] == 0, 1, all)
  
  control_means <- colMeans(Y[control_rows, ], na.rm = TRUE)
  control_sds   <- apply(Y[control_rows, ], 2, sd, na.rm = TRUE)
  
  Y_std <- sweep(Y, 2, control_means, "-")
  Y_std <- sweep(Y_std, 2, control_sds, "/")
  
  Sigma <- cov(
    Y_std[control_rows, ],
    use = "pairwise.complete.obs"
  )
  
  Sigma_inv <- solve(Sigma)
  
  ones <- rep(1, ncol(Y_std))
  
  weights <- Sigma_inv %*% ones /
    as.numeric(t(ones) %*% Sigma_inv %*% ones)
  
  data[[new_var_name]] <- as.numeric(Y_std %*% weights)
  
  control_mean <- mean(data[[new_var_name]][control_rows], na.rm = TRUE)
  control_sd   <- sd(data[[new_var_name]][control_rows], na.rm = TRUE)
  
  new_col_d<- data %>%
    mutate(weights_z = (.data[[new_var_name]] - control_mean) / control_sd)
  
  new_col_d[[paste0(new_var_name, "_z")]] <- new_col_d[["weights_z"]]
  
  return(new_col_d)
}

icw_vars_likert<- c("Approve_Attack_LikertImputed",
                     "Protest_Attack_LikertImputed",
                     "Vote_Attack_LikertImputed")

icw_vars_na<- c("Approve_Attack_NAImputed",
                "Protest_Attack_NAImputed",
                "Vote_Attack_NAImputed")

survey_cleaned <- make_icw_index(
  data = survey_cleaned,
  cols = icw_vars_likert,
  treat_vars = c("NFU", "National_Identity"),
  new_var_name = "attack_index_likert_icw"
)

survey_cleaned <- make_icw_index(
  data = survey_cleaned,
  cols = icw_vars_na,
  treat_vars = c("NFU", "National_Identity"),
  new_var_name = "attack_index_na_icw"
)
 
################################################################################
# PCA INDEX CONSTRUCTION
# CREATED WITH CHATGPT
################################################################################

make_pca_index <- function(data, cols, treat_vars, new_var_name = "pca_index") {
  
  # outcome matrix
  Y <- data %>%
    dplyr::select(dplyr::all_of(cols)) %>%
    as.matrix()
  
  # identify pure control group
  control_rows <- apply(data[, treat_vars] == 0, 1, all)
  
  # control-group standardization
  control_means <- colMeans(Y[control_rows, ], na.rm = TRUE)
  control_sds   <- apply(Y[control_rows, ], 2, sd, na.rm = TRUE)
  
  Y_std <- sweep(Y, 2, control_means, "-")
  Y_std <- sweep(Y_std, 2, control_sds, "/")
  
  # complete controls only
  control_complete <- control_rows & complete.cases(Y_std)
  
  # PCA estimated on controls only
  pca_model <- prcomp(
    Y_std[control_complete, ],
    center = FALSE,
    scale. = FALSE
  )
  
  # PC1 loadings
  weights <- pca_model$rotation[,1]
  
  # raw PCA index
  data[[new_var_name]] <- as.numeric(Y_std %*% weights)
  
  # final control-group standardization
  control_mean <- mean(data[[new_var_name]][control_rows], na.rm = TRUE)
  control_sd   <- sd(data[[new_var_name]][control_rows], na.rm = TRUE)
  
  data[[paste0(new_var_name, "_z")]] <-
    (data[[new_var_name]] - control_mean) / control_sd
  
  return(data)
}

ols_vars_likert_imputed<- colnames(survey_cleaned)[grepl("_LikertImputed", 
                                                         colnames(survey_cleaned)) 
                                                   & !grepl("_Dichot", colnames(survey_cleaned))]

ols_vars_na_imputed<- colnames(survey_cleaned)[grepl("_NAImputed", 
                                                     colnames(survey_cleaned)) 
                                               & !grepl("_Dichot", colnames(survey_cleaned))]

survey_cleaned <- make_pca_index(
  data = survey_cleaned,
  cols = ols_vars_likert_imputed,
  treat_vars = c("NFU", "National_Identity"),
  new_var_name = "attack_index_likert_pca"
)

survey_cleaned <- make_pca_index(
  data = survey_cleaned,
  cols = ols_vars_na_imputed,
  treat_vars = c("NFU", "National_Identity"),
  new_var_name = "attack_index_na_pca"
)

################################################################################
# STEP 4: FILTER TO ONLY THOSE THAT PASS MANIPULATION CHECKS
# Reweight survey & create new indices
################################################################################x

survey_pass_manipulation_checks<-survey_cleaned %>%
  mutate(Manipulation_Check= ifelse(is.na(Manipulation1_Treat),
                                    ifelse(Manipulation1 == 3 &
                                             Manipulation2 == 4 &
                                             Manipulation3 == 1,
                                           1, 0),
                                    ifelse(Manipulation1_Treat == 3 &
                                             Manipulation2_Treat == 1 &
                                             Manipulation3_Treat == 1,
                                           1, 0))) %>%
  filter(Manipulation_Check == 1)

design <- svydesign(ids = ~1, data = survey_pass_manipulation_checks)

design_raked <- rake(
  design,
  sample.margins = list(~Gender_clean, ~AgeGrouped),
  population.margins = list(
    data.frame(
      Gender_clean = c("Male","Female"),
      Freq = c(0.515, 0.485)
    ),
    data.frame(
      AgeGrouped = c("18-29", "30-44", "45-59", "60+"),
      Freq = c(prop_18_29, prop_30_44, prop_45_59, prop_60)
    )
  )
)

design_trim <- trimWeights(design_raked, upper = 4)
survey_pass_manipulation_checks$weight <- weights(design_raked)
survey_pass_manipulation_checks$weight_std <- survey_pass_manipulation_checks$weight / mean(survey_pass_manipulation_checks$weight)


survey_pass_manipulation_checks <- make_icw_index(
  data = survey_pass_manipulation_checks,
  cols = icw_vars_likert,
  treat_vars = c("NFU", "National_Identity"),
  new_var_name = "attack_index_likert_icw"
)

survey_pass_manipulation_checks <- make_icw_index(
  data = survey_pass_manipulation_checks,
  cols = icw_vars_na,
  treat_vars = c("NFU", "National_Identity"),
  new_var_name = "attack_index_na_icw"
)

survey_pass_manipulation_checks <- make_pca_index(
  data = survey_pass_manipulation_checks,
  cols = ols_vars_likert_imputed,
  treat_vars = c("NFU", "National_Identity"),
  new_var_name = "attack_index_likert_pca"
)

survey_pass_manipulation_checks <- make_pca_index(
  data = survey_pass_manipulation_checks,
  cols = ols_vars_na_imputed,
  treat_vars = c("NFU", "National_Identity"),
  new_var_name = "attack_index_na_pca"
)

################################################################################
# STEP 5: HISTORICAL ATTITUDES
################################################################################

data_1994<-read_rpr(
  col_positions = c(66, 67, 75, 76, 79, 82, 83),
  widths = c(1, 1, 1, 1, 1, 1, 1),
  col_names = c("NPT", "NukeOption", "Sex", "Age", "Education", "Income", "Religion"),
  filepath = "USIA Survey/USIA 1994.dat"
) %>%
  mutate(Sex = factor(Sex, 
                      levels = c(1, 2),
                      labels = c("Male", "Female")),
         Religion = factor(Religion,
                           levels = c(1:5),
                           labels = c("Hindu", "Muslim", "Christian", "Sikh", "Other")),
         Education = factor(Education,
                            levels = c(1:4),
                            labels = c("Primary", "Some Secondary", "Secondary Completed", "University+")),
         Income = factor(Income,
                         levels = c(0:9),
                         labels = c(NA, "< 1000", "1001-1250", "1251-1500", "1501-2000",
                                    "2001-2500", "2501-3000", "3001-4000", "4001-5000", "5000+")),
         NPT = case_when(
           NPT == 3 | NPT == 4 ~ NPT + 1,
           NPT == 8 ~ 3,
           NPT == 9 ~ NA,
           NPT == 1 | NPT == 2 ~ NPT
         ),
         
         NukeOption = case_when(
           NukeOption == 3 | NukeOption == 4 ~ NukeOption + 1,
           NukeOption == 8 ~ 3,
           NukeOption == 9 ~ NA,
           NukeOption == 1 | NukeOption == 2 ~ NukeOption
         ))


attr(data_1994$NPT, "label")<- "Do you agree or disagree that India 
should sign the nuclear Non-Proliferation Treaty (NPT), agreeing not to 
acquire nuclear weapons and acception international inspectin of its nuclear power 
facilities? Strongly or only somewhat?"

attr(data_1994$NukeOption, "label")<- "Prime Minister Narasimha Rao stated in June
this year that India would like to retain the option of making a nuclear bomb so long as
there was no international agreement to ban nuclear weapons on all nations. Do you agree
or disagree with this statement? Strongly or only somewhat?"

################################################################################
# STEP 5: BUILD FUNCTIONS
# LASSO FUNCTION BUILT USING CHATGPT
################################################################################

lasso_covariates <- function(data, outcome, controls) {
  
  # build control matrix
  X <- model.matrix(
    as.formula(
      paste("~", paste(controls, collapse = " + "))
    ),
    data = data
  )[, -1]
  
  # outcome vector
  y <- data[[outcome]]
  
  # LASSO on outcome
  cvfit <- cv.glmnet(
    x = X,
    y = y,
    alpha = 1
  )
  
  # selected controls
  selected <- rownames(coef(cvfit, s = "lambda.min"))[
    coef(cvfit, s = "lambda.min")[, 1] != 0
  ]
  
  selected <- selected[selected != "(Intercept)"]
  
  return(selected)
  
}

ols_models_func<- function(outcome, treatments, controls, blocks, data, weight){
  
  data <- data %>%
    select(all_of(c(treatments, controls, outcome, weight))) %>%
    na.omit()
  
  set.seed(135)
  covs<- lasso_covariates(data = data, 
                          outcome = outcome, 
                          controls = controls)
  
  cov_find<- sapply(controls, function(x) any(grepl(x, covs)))
  covs<- controls[cov_find]
  
  covs<- c(if (!(blocks %in% covs)) covs else covs, if (!(blocks %in% covs)) blocks)
  
  treatments<- c(treatments, paste0(treatments, collapse = ":"))
  
  reg_form <- as.formula(paste(paste(outcome, "~"), paste(c(covs, treatments), collapse = "+")))
  
  reg <- lm_robust(reg_form, data = data, weights= data[[weight]], se_type = "HC2")
  
  return(reg)
  
}

