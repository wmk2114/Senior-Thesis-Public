
###################################################################################
### DATA VISUALIZATIONS FOR WE DON'T DO THAT: NATIONAL IDENTITY AND NUCLEAR NON-USE
### SUBMISSION FOR DEPARTMENTAL HONORS
### COLUMBIA UNIVERSITY DEPARTMENT OF POLITICAL SCIENCE
### AUTHOR: WYATT KING
################################################################################

rm(list = ls())

library(tidyverse)
library(showtext)

setwd("~/Desktop/Columbia 2025/Senior Thesis/Data Analysis")
clean_env <- new.env()

source("Data Preprocessing.R", local = clean_env)

survey_df <- clean_env$survey_cleaned
survey_df_manip<- clean_env$survey_pass_manipulation_checks


################################################################################
# STEP 2: GROUP OUTCOMES INTO SUPPORT, OPPOSITION, INDIFFERENCE
################################################################################

survey_df<- survey_df %>%
  
  ### DICHOTOMIZE FOR COLUMNS WITH IMPUTED NAs
  mutate(across(
    .cols = colnames(survey_df)[grepl("_NAImputed",  colnames(survey_df))
                                & !grepl("Dichot",  colnames(survey_df))],
    ~ if_else(is.na(.x), NA, if_else(.x ==4, "Indifferent", if_else(.x > 4, "Oppose", "Support"))),
    .names = "{.col}_Three"
  )) %>%
  
  ### DICHOTOMIZE FOR COLUMNS WITH IMPUTED MIDPOINT
  mutate(across(
    .cols = colnames(survey_df)[grepl("_LikertImputed",  colnames(survey_df)) 
                                & !grepl("Dichot",  colnames(survey_df))],
    ~ if_else(is.na(.x), NA, if_else(.x ==4, "Indifferent", if_else(.x > 4, "Oppose", "Support"))),
    .names = "{.col}_Three"
  ))

################################################################################
# STEP 3: CREATE BAR CHART FUNCTION
################################################################################

label_names <- c(
  Approve_Attack_LikertImputed_Three = "Approve Nuclear Attack",
  Vote_Attack_LikertImputed_Three = "Vote Against Government",
  Protest_Attack_LikertImputed_Three = "Protest",
  Prefer_Attack_LikertImputed_Three= "Prefer Nuclear Attack to Ground War",
  NationalID_Attack_LikertImputed_Three = "Violates Indian Identity",
  Ethical_Attack_LikertImputed_Three = "Unethical",
  Ahimsa_Attack_LikertImputed_Three = "Violates Ahimsa"
)

gen_plot_df<-function(data, vars){
  plot_df <- data %>%
    pivot_longer(
      cols = all_of(vars),
      names_to = "Outcome",
      values_to = "Support"
    ) %>%
    filter(!is.na(Support)) %>%
    group_by(Outcome, Support) %>%
    summarise(
      n = n(),
      .groups = "drop_last"
    ) %>%
    mutate(
      total = sum(n),
      prop = n / total,
      se = sqrt(prop * (1 - prop) / total),
      lower = prop - 1.96 * se,
      upper = prop + 1.96 * se
    ) %>%
    ungroup() %>%
    filter((Outcome %in% c("Approve_Attack_LikertImputed_Three", 
                           "Ethical_Attack_LikertImputed_Three",
                           "Ahimsa_Attack_LikertImputed_Three",
                           "NationalID_Attack_LikertImputed_Three",
                           "Prefer_Attack_LikertImputed_Three",
                           "Protest_Attack_LikertImputed_Three",
                           "Vote_Attack_LikertImputed_Three")))
  return(plot_df)
}

generate_plot <- function(data, title, subtitle, caption){
  
  ggplot(data, aes(x = Outcome, y = prop, fill = Support)) +
    geom_col(
      position = position_dodge(width = 0.8),
      width = 0.65,
      color = "black",
      linewidth = 0.2
    ) +
    geom_errorbar(
      aes(ymin = lower, ymax = upper),
      position = position_dodge(width = 0.8),
      width = 0.18,
      linewidth = 0.5
    ) +
    geom_hline(
      yintercept = 0.5,
      linetype = "dashed",
      linewidth = 0.4,
      alpha = 0.6
    ) +
    scale_x_discrete(labels = label_names) +
    scale_y_continuous(
      limits = c(0, 1),
      expand = expansion(mult = c(0, 0.02))
    ) +
    scale_fill_manual(values = c(
      "Oppose" = "grey70",
      "Indifferent" = "grey45",
      "Support" = "grey20"
    )) +
    labs(
      x = NULL,
      y = "Proportion",
      title = title,
      subtitle = subtitle,
      caption = caption,
      fill = "Opinion toward Nuclear Use"
    ) +
    theme_classic(base_size = 15) +
    theme(
      axis.title.y = element_text(size = 15),
      axis.text = element_text(size = 13),
      axis.text.x = element_text(size = 13, angle = 20, hjust = 1),
      
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 13),
      plot.caption = element_text(size = 11, hjust = 0),
      
      legend.position = "bottom",
      legend.title = element_text(size = 13),
      legend.text = element_text(size = 12),
      
      panel.border = element_blank()
    )
}

################################################################################
# FIGURE 1
################################################################################

control_df<-survey_df %>%
  filter(NFU== 0 & National_Identity==0)


control_vars<- colnames(control_df)[grepl("LikertImputed", colnames(control_df)) &
                                      grepl("_Three", colnames(control_df))]

control_df<-zap_labels(control_df)

plot1_df<- gen_plot_df(data = control_df, vars = control_vars) 

title1<- "Figure 1"
subtitle1<- "Support for Nuclear Attack, Control Group (Midpoint Imputation)"
cap1<-paste(
  "Note: (1) Error bars represent 95% confidence intervals, (2) Midpoint imputation is used",
  "(3) Respondents who fail comprehension checks are not dropped.",
  sep = "\n"
)

plot_1<-generate_plot(plot1_df, title = title1, subtitle = subtitle1, caption = cap1)
# ggsave("Data Visualizations/1_approve_prefer.pdf", width = 11, height = 7.5, units = "in")

################################################################################
# FIGURE 2
################################################################################

run_lp_reg<-function(dep_var, ind_vars, data){
  rhs<-paste0(ind_vars, collapse = "+")
  lhs_rhs<- paste0(c(dep_var, rhs), collapse = "~")
  form<- as.formula(lhs_rhs)
  log_model<- lm_robust(form, data = data)
  return(log_model)
}

dep_dichot_likert_impute<- colnames(survey_df)[
  grepl("_LikertImputed_Dichot", colnames(survey_df))
]

ind_vars<- c("Party_clean", "Religion_clean", 
             "NFU", "National_Identity", "NFU:National_Identity")

log_regs_likert_impute <- lapply(
  dep_dichot_likert_impute,
  run_lp_reg,
  ind_vars = ind_vars,
  data = survey_df
)

extract_all <- function(data){
  
  wanted <- c("NFU1", "National_Identity1", "NFU1:National_Identity1")
  
  purrr::map_dfr(data, function(reg_output){
    
    data.frame(
      outcome = rep(reg_output$outcome, length(wanted)),
      term = c("NFU", "Identity", "NFU:Identity"),
      est = as.numeric(reg_output$coefficients[wanted]),
      conf_low = as.numeric(reg_output$conf.low[wanted]),
      conf_high = as.numeric(reg_output$conf.high[wanted])
    )
    
  })
}

lin_prob_data<-extract_all(log_regs_likert_impute)


outcome_labels <- c(
  Approve_Attack_LikertImputed_Dichot = "Approve Nuclear Attack",
  Vote_Attack_LikertImputed_Dichot = "Vote Against Government",
  Protest_Attack_LikertImputed_Dichot = "Protest",
  Prefer_Attack_LikertImputed_Dichot= "Prefer Nuclear Attack to Ground War",
  Ethical_Attack_LikertImputed_Dichot = "Unethical",
  Ahimsa_Attack_LikertImputed_Dichot = "Violates Ahimsa"
)

lin_prob_data <- lin_prob_data %>%
  mutate(outcome = recode(outcome, !!!outcome_labels)) %>%
  filter(outcome != "NationalID_Attack_LikertImputed_Dichot")

### GENERATE PLOT

fig_2<-ggplot(lin_prob_data, aes(est, outcome)) +
  facet_wrap(~ term, scales = "free_x") +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = conf_low, xmax = conf_high), height = 0) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "darkred") +
  labs(
    y = NULL,
    x = "",
    title = "Figure 2",
    subtitle = "Point Estimates and Confidence Intervals for Linear Probability Models",
    caption = paste(
      "Note: (1) Error bars represent 95% confidence intervals;",
      "(2) dots represent point estimates for treatment effects in linear",
      "\nprobability models that control for religion and party;",
      "(3) midpoint imputation is used;",
      "(4) respondents who fail \ncomprehension checks are not dropped."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text = element_text(size = 12),
    axis.text.x = element_text(size = 12, angle = 20, hjust = 1),
    
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 13),
    plot.caption = element_text(size = 11, hjust = 0),
    
    panel.border = element_rect(fill = NA, color = "darkgrey", linewidth = 0.5),
    
    plot.background = element_rect(fill = NA, color = "black", linewidth = 0.8),
    
    plot.margin = margin(15, 15, 15, 15))

# ggsave(filename = "Data Visualizations/1_dot_whiskers.pdf", plot = fig_2, width= 11.5, height =8)

################################################################################
# FIGURE A1
################################################################################

nfu_df<-survey_df %>%
  filter(NFU== 1 & National_Identity==0)

nfu_vars<- colnames(nfu_df)[grepl("LikertImputed", colnames(nfu_df)) &
                                      grepl("_Three", colnames(nfu_df))]

nfu_df<-zap_labels(nfu_df)

plotA1_df<- gen_plot_df(data = nfu_df, vars = nfu_vars) 

titleA1<- "Figure A1"
subtitleA1 <-  "Opinion Toward Nuclear Attack, NFU Treatment"

plot_2<- generate_plot(plotA1_df, title = titleA1, subtitle = subtitleA1, cap1)
# ggsave(filename = "Data Visualizations/NFU_Treatment.jpg", plot = plot_2, width= 11.5, height =8)

################################################################################
# FIGURE A2
################################################################################

identity_df<-survey_df %>%
  filter(NFU== 0 & National_Identity==1)

identity_vars<- colnames(identity_df)[grepl("LikertImputed", colnames(identity_df)) &
                              grepl("_Three", colnames(identity_df))]

identity_df<-zap_labels(identity_df)

plotA2_df<- gen_plot_df(data = identity_df, vars = identity_vars) 

titleA2<- "Figure A2"
subtitleA2 <-  "Opinion Toward Nuclear Attack, Identity Treatment"

plot_3<- generate_plot(plotA2_df, title = titleA2, subtitle = subtitleA2, cap1)
# ggsave(filename = "Data Visualizations/Identity_Treatment.jpg", plot = plot_3, width= 11.5, height =8)

################################################################################
# FIGURE A3
################################################################################

inter_df<-survey_df %>%
  filter(NFU== 1 & National_Identity==1)

inter_vars<- colnames(inter_df)[grepl("LikertImputed", colnames(inter_df)) &
                                        grepl("_Three", colnames(inter_df))]

inter_df<-zap_labels(inter_df)

plotA3_df<- gen_plot_df(data = inter_df, vars = inter_vars) 

titleA3<- "Figure A3"
subtitleA3 <-  "Opinion Toward Nuclear Attack, NFU x Identity"

plot_4<- generate_plot(plotA3_df, title = titleA3, subtitle = subtitleA3, caption =  cap1)
# ggsave(filename = "Data Visualizations/Interaction.jpg", plot = plot_4, width= 11.5, height =8)
 