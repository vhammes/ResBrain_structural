#### Preamble ####

# Run 00a_metadata_and_packages.R first to load dependencies

# Define required packages
required_packages_05 <- c("haven", "dplyr", "here", "writexl", "readxl", "tidyverse", "glmnet", "patchwork", "corrplot", "hrbrthemes", "ggpubr", "naniar", "psych")

# Check and load packages
for (pkg in required_packages_05) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(paste("Package", pkg, "is required but not installed."))
  }
  library(pkg, character.only = TRUE) # Load the package into the environment
}

# Clean up temporary variables
rm(list = c("required_packages_05", "pkg"))

# ----------------------------------------------------------------------------------------------------------------------------------------- # 



#### Preprocessing T1 and T2 data ####
ResBrain_All_T1_T2$Outcome_T2 <- ResBrain_All_T1_T2$HAMD_Sum17_2

ResBrain_MLR_T2_retained <- ResBrain_All_T1_T2 %>%
  select(Proband, FSozU_Sum_2, SozDemo5_2, LEQ_NegativeEventScore_2, 
         LEQ_PositiveEventScore_2, CTQ_Sum_2, Gesichertes_Risiko_Affektiv_2, 
         Gesichertes_Risiko_Psychotisch_2, Bildungsjahre_2, IQ_2, PSS_Sum_2, 
         ACE_Sum, RSQ_Secure, RSQ_Fearful, RSQ_Dismissing, RSQ_Preoccupied,                      
         NEOFFI_Extraversion, NEOFFI_Neurotizismus, NEOFFI_Vertraeglichkeit, NEOFFI_Offenheit, NEOFFI_Gewissenhaftigkeit,                     
         Immigration, Haushaltsnetto_2, Alter_2, Geschlecht_2, Outcome_T2)

#create dataframe with only predictors AND outcome. MUST NOT contain NAs
ResBrain_MLR_T2 <- subset(ResBrain_MLR_T2_retained, select = -Proband)

# ----------------------------------------------------------------------------------------------------------------------------------------- # 

# ----------------------------------------------------------------------------------------------------------------------------------------- # 

#### Nested CV (Ridge) with out-of-sample prediction ####
##### Model Specification #####

#fit regression model containing all predictors and extract design matrix
vars_T2 <- names(ResBrain_MLR_T2[,-ncol(ResBrain_MLR_T2)])
frml_T2 <- as.formula(paste("Outcome_T2 ~", paste(vars_T2, collapse = " +")))
fit_lm_T2 <- lm(frml_T2, data = ResBrain_MLR_T2)
X_T2 <- model.matrix(fit_lm_T2)[,-1]
y_T2 <- ResBrain_MLR_T2$Outcome_T2


# ----------------------------------------------------------------------------------------------------------------------------------------- # 

# ----------------------------------------------------------------------------------------------------------------------------------------- # 

#### Nested CV (Ridge) – OOS predictions/residuals/cumulative risk (T2) ####

set.seed(123)

K_outer <- 10
K_inner <- 10

outer_id_T2 <- sample(rep(1:K_outer, length.out = length(y_T2)))

preds_nested_oos_T2 <- rep(NA_real_, length(y_T2))
residuals_nested_T2 <- rep(NA_real_, length(y_T2))
cumulative_risk_nested_oos_T2 <- rep(NA_real_, length(y_T2))

lambda_star_T2 <- rep(NA_real_, K_outer)
lambda_min_outer_T2 <- rep(NA_real_, K_outer) # for inspection, not used
mse_lambda_min_outer_T2 <- rep(NA_real_, K_outer) # for inspection, not used

fold_mse_T2  <- rep(NA_real_, K_outer)
fold_rmse_T2 <- rep(NA_real_, K_outer)
fold_r2_T2   <- rep(NA_real_, K_outer)

# store coefficients per fold (incl intercept) for stability summaries
coef_mat_outer_T2 <- matrix(NA_real_, nrow = ncol(X_T2) + 1, ncol = K_outer)
rownames(coef_mat_outer_T2) <- c("(Intercept)", colnames(X_T2))
colnames(coef_mat_outer_T2) <- paste0("fold", 1:K_outer)

for (k in 1:K_outer) {
  
  test_idx  <- which(outer_id_T2 == k)
  train_idx <- which(outer_id_T2 != k)
  
  X_train <- X_T2[train_idx, , drop = FALSE]
  y_train <- y_T2[train_idx]
  
  X_test  <- X_T2[test_idx, , drop = FALSE]
  y_test  <- y_T2[test_idx]
  
  # Inner folds (reproducible)
  set.seed(1000 + k)
  foldid_inner <- sample(rep(1:K_inner, length.out = length(train_idx)))
  
  cv_in <- cv.glmnet(
    x = X_train,
    y = y_train,
    alpha = 0,
    foldid = foldid_inner,
    type.measure = "mse"
  )
  
  lam <- cv_in$lambda.1se   # choose: lambda.1se (be consistent with T1)
  lambda_star_T2[k] <- lam
  lambda_min_outer_T2[k] <- cv_in$lambda.min # for inspection, not used
  idx_min_T2 <- which(cv_in$lambda == cv_in$lambda.min) # for inspection, not used
  mse_lambda_min_outer_T2[k] <- cv_in$cvm[idx_min_T2] # for inspection, not used
  
  fit_out <- glmnet(
    x = X_train,
    y = y_train,
    alpha = 0,
    lambda = lam
  )
  
  # Save coefficients (includes intercept)
  coef_k <- as.numeric(coef(fit_out, s = lam))
  coef_mat_outer_T2[, k] <- coef_k
  
  # OOS predictions
  yhat_test <- as.numeric(predict(fit_out, newx = X_test, s = lam))
  preds_nested_oos_T2[test_idx] <- yhat_test
  
  # OOS residuals
  residuals_nested_T2[test_idx] <- y_test - yhat_test
  
  # OOS cumulative risk = X * beta (NO intercept)
  beta_k <- coef_k[-1]
  cumulative_risk_nested_oos_T2[test_idx] <- as.numeric(X_test %*% beta_k)
  
  # Fold metrics
  fold_mse_T2[k]  <- mean((y_test - yhat_test)^2)
  fold_rmse_T2[k] <- sqrt(fold_mse_T2[k])
  fold_r2_T2[k]   <- 1 - sum((y_test - yhat_test)^2) / sum((y_test - mean(y_train))^2)
}


##### Model Performance #####

nested_mse_T2  <- mean((y_T2 - preds_nested_oos_T2)^2)
nested_rmse_T2 <- sqrt(nested_mse_T2)
nested_r2_T2   <- 1 - sum((y_T2 - preds_nested_oos_T2)^2) / sum((y_T2 - mean(y_T2))^2)

cat("\nT2 Nested CV performance:\n")
cat("MSE :", nested_mse_T2, "\n")
cat("RMSE:", nested_rmse_T2, "\n")
cat("R2  :", nested_r2_T2, "\n\n")

cat("T2 Lambda (outer folds) summary (lambda.1se per fold):\n")
print(summary(lambda_star_T2))

cat("Lambda.min summary (NOT used for model fitting):\n")
cat("Mean :", mean(lambda_min_outer_T2, na.rm = TRUE), "\n")
cat("SD   :", sd(lambda_min_outer_T2, na.rm = TRUE), "\n\n")
print(summary(lambda_min_outer_T2))


cat("MSE at lambda.min (inner CV):\n")
cat("Mean :", mean(mse_lambda_min_outer_T2, na.rm = TRUE), "\n")
cat("SD   :", sd(mse_lambda_min_outer_T2, na.rm = TRUE), "\n\n")
print(summary(mse_lambda_min_outer_T2))


# Attach nested outputs to df
ResBrain_MLR_T2$outer_fold_T2 <- outer_id_T2
ResBrain_MLR_T2$preds_nested_oos_T2 <- preds_nested_oos_T2
ResBrain_MLR_T2$residuals_nested_T2 <- residuals_nested_T2
ResBrain_MLR_T2$cumulative_risk_nested_oos_T2 <- cumulative_risk_nested_oos_T2



# Attach nested outputs to retained df (WITH Proband)
ResBrain_MLR_T2_retained$outer_fold_T2 <- outer_id_T2
ResBrain_MLR_T2_retained$preds_nested_oos_T2 <- preds_nested_oos_T2
ResBrain_MLR_T2_retained$residuals_nested_T2 <- residuals_nested_T2
ResBrain_MLR_T2_retained$cumulative_risk_nested_oos_T2 <- cumulative_risk_nested_oos_T2


stopifnot(identical(ResBrain_All_T1_T2$Proband, ResBrain_MLR_T2_retained$Proband))

ResBrain_All_T1_T2$outer_fold_T2 <- ResBrain_MLR_T2_retained$outer_fold_T2
ResBrain_All_T1_T2$preds_nested_oos_T2 <- ResBrain_MLR_T2_retained$preds_nested_oos_T2
ResBrain_All_T1_T2$residuals_nested_T2 <- ResBrain_MLR_T2_retained$residuals_nested_T2
ResBrain_All_T1_T2$cumulative_risk_nested_oos_T2 <- ResBrain_MLR_T2_retained$cumulative_risk_nested_oos_T2


# ----------------------------------------------------------------------------------------------------------------------------------------- #
##### Coefficient stability across outer folds (T2) – mean ± SD; color numeric vs factor #####

beta_outer_T2 <- coef_mat_outer_T2[rownames(coef_mat_outer_T2) != "(Intercept)", , drop = FALSE]
beta_mean_T2 <- rowMeans(beta_outer_T2, na.rm = TRUE)
beta_sd_T2   <- apply(beta_outer_T2, 1, sd, na.rm = TRUE)

sign_stability_T2 <- sapply(seq_len(nrow(beta_outer_T2)), function(i) {
  b <- beta_outer_T2[i, ]
  m <- beta_mean_T2[i]
  if (is.na(m) || m == 0) return(NA_real_)
  mean(sign(b) == sign(m), na.rm = TRUE)
})

coef_stability_T2 <- data.frame(
  Variable_raw = rownames(beta_outer_T2),
  MeanBeta = as.numeric(beta_mean_T2),
  SDBeta   = as.numeric(beta_sd_T2),
  SignStability = as.numeric(sign_stability_T2),
  stringsAsFactors = FALSE
)

# SD scaling: continuous vars get SD from original modelling df; dummy columns keep scale 1
numeric_vars_T2 <- names(ResBrain_MLR_T2)[sapply(ResBrain_MLR_T2, is.numeric)]
numeric_vars_T2 <- setdiff(numeric_vars_T2, "Outcome_T2")

std_devs_X_T2 <- rep(1, length(colnames(X_T2)))
names(std_devs_X_T2) <- colnames(X_T2)

numeric_X_T2 <- intersect(colnames(X_T2), numeric_vars_T2)
std_devs_X_T2[numeric_X_T2] <- sapply(ResBrain_MLR_T2[numeric_X_T2], sd, na.rm = TRUE)

coef_stability_T2$AdjustedMean <- coef_stability_T2$MeanBeta * std_devs_X_T2[coef_stability_T2$Variable_raw]
coef_stability_T2$AdjustedSD   <- coef_stability_T2$SDBeta   * std_devs_X_T2[coef_stability_T2$Variable_raw]

coef_stability_T2$Type <- ifelse(coef_stability_T2$Variable_raw %in% numeric_vars_T2, "Numeric", "Factor")

coef_stability_T2 <- coef_stability_T2 %>%
  mutate(
    Variable = recode(
      Variable_raw,
      "FSozU_Sum_2" = "FSozU sum score (T2)",
      "LEQ_NegativeEventScore_2" = "LEQ negative sum score (T2)",
      "LEQ_PositiveEventScore_2" = "LEQ positive sum score (T2)",
      "CTQ_Sum_2" = "CTQ sum score (T2)",
      "Gesichertes_Risiko_Affektiv_21" = "Familial risk AD (T2)",
      "Gesichertes_Risiko_Psychotisch_21" = "Familial risk PD (T2)",
      "Bildungsjahre_2" = "Education (T2)",
      "IQ_2" = "IQ (T2)",
      "PSS_Sum_2" = "PSS sum score (T2)",
      "ACE_Sum" = "ACE sum score (T1)",
      "RSQ_Secure1" = "RSQ secure (T1)",
      "RSQ_Fearful1" = "RSQ fearful (T1)",
      "RSQ_Dismissing1" = "RSQ dismissing (T1)",
      "RSQ_Preoccupied1" = "RSQ preoccupied (T1)",
      "NEOFFI_Extraversion" = "NEO-FFI extraversion (T1)",
      "NEOFFI_Neurotizismus" = "NEO-FFI neuroticism (T1)",
      "NEOFFI_Vertraeglichkeit" = "NEO-FFI agreeableness (T1)",
      "NEOFFI_Offenheit" = "NEO-FFI openness (T1)",
      "NEOFFI_Gewissenhaftigkeit" = "NEO-FFI conscientiousness (T1)",
      "Immigration1" = "Immigration (T1)",
      "Haushaltsnetto_2" = "Household income (T2)",
      "Alter_2" = "Age (T2)",
      "Geschlecht_22" = "Sex (T2)",
      "SozDemo5_22" = "Social interactions  (A2): \"once per week\" (T2)",
      "SozDemo5_23" = "Social interactions  (A3): \"once every two weeks\" (T2)",
      "SozDemo5_24" = "Social interactions  (A4): \"once per month\" (T2)",
      "SozDemo5_25" = "Social interactions  (A5): \"none except at work or similar\" (T2)",
      "SozDemo5_26" = "Social interactions  (A6): \"none under any circumstances\" (T2)",
      .default = Variable_raw
    )
  )

plot_df_T2 <- coef_stability_T2 %>%
  mutate(
    Lower = AdjustedMean - AdjustedSD,
    Upper = AdjustedMean + AdjustedSD
  ) %>%
  arrange(abs(AdjustedMean))


p_coef_stability_T2 <- ggplot(plot_df_T2, aes(x = reorder(Variable, AdjustedMean), y = AdjustedMean, color = Type)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
  geom_pointrange(aes(ymin = Lower, ymax = Upper), linewidth = 0.6) +
  coord_flip() +
  theme_minimal(base_size = 16) +
  scale_color_manual(values = c("Numeric" = "blue2", "Factor" = "red2")) +
  labs(
    title = "T2 predictor weights across nested CV outer folds",
    x = "Predictor",
    y = "Adjusted coefficient (mean ± SD across folds)",
    color = "Variable type"
  )

print(p_coef_stability_T2)

output_dir <- here("05_figures/001_defaultexp_figures")
ggsave(file.path(output_dir, "coefficients_T2_nested_stability_mean_sd.jpeg"),
       plot = p_coef_stability_T2, width = 12, height = 8, dpi = 300)




# ----------------------------------------------------------------------------------------------------------------------------------------- #

##### Plot: Nested CV (T2) – OOS cumulative risk + OOS residuals #####

plot_data_nested_T2 <- data.frame(
  Cumulative_Risk = ResBrain_MLR_T2$cumulative_risk_nested_oos_T2,
  Outcome = ResBrain_MLR_T2$Outcome_T2,
  Residuals = ResBrain_MLR_T2$residuals_nested_T2,
  Predicted = ResBrain_MLR_T2$preds_nested_oos_T2
)

se_nested_T2 <- sqrt(sum(plot_data_nested_T2$Residuals^2) / (nrow(ResBrain_MLR_T2) - ncol(X_T2)))
threshold_T2 <- se_nested_T2

plot_data_nested_T2$Color <- ifelse(
  plot_data_nested_T2$Residuals < -threshold_T2, "better-than-expected",
  ifelse(plot_data_nested_T2$Residuals >  threshold_T2, "worse-than-expected", "as-expected")
)

color_mapping <- c(
  "better-than-expected" = "#009E73",
  "worse-than-expected"  = "#D55E00",
  "as-expected"          = "black"
)

p_nested_T2 <- ggplot(plot_data_nested_T2, aes(x = Cumulative_Risk, y = Outcome)) +
  geom_point(aes(color = Color), size = 2.5) +
  geom_segment(
    #data = subset(plot_data_nested_T2, Color != "as-expected"),
    aes(x = Cumulative_Risk, y = Outcome, xend = Cumulative_Risk, yend = Predicted, color = Color),
    linewidth = 0.3, linetype = "dotted"
  ) +
  scale_color_manual(values = color_mapping) +
  theme_minimal(base_size = 18) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    plot.margin = unit(c(20, 20, 20, 20), "pt"),
    panel.grid = element_blank()
  ) +
  labs(
    title = "Ridge-penalized regression (T2) – Nested CV (OOS predictions)",
    x = "Cumulative risk",
    y = "Outcome (HAM-D score)",
    color = "Interpretation"
  ) +
  scale_x_continuous(
    breaks = scales::pretty_breaks(n = 8),
    labels = function(x) sprintf("%.1f", x),
    expand = c(0.01, 0.01)
  )


print(p_nested_T2)

ggsave(file.path(output_dir, "ridge_penalized_regression_T2_nested_true.jpeg"),
       plot = p_nested_T2, width = 14, height = 6, dpi = 300)

# Honest association (OOS risk vs outcome)
print(cor.test(plot_data_nested_T2$Cumulative_Risk, plot_data_nested_T2$Outcome, method = "spearman"))


# ----------------------------------------------------------------------------------------------------------------------------------------- #

##### Extreme Group Classification (T2) – based on nested OOS residuals #####

preds_oos_T2 <- plot_data_nested_T2$Predicted
residuals_oos_T2 <- plot_data_nested_T2$Residuals

se_resid_oos_T2 <- sqrt(sum(residuals_oos_T2^2) / (nrow(ResBrain_MLR_T2) - ncol(X_T2)))

outlier_above_idx_T2 <- which(ResBrain_MLR_T2$Outcome_T2 > preds_oos_T2 + se_resid_oos_T2)
outlier_below_idx_T2 <- which(ResBrain_MLR_T2$Outcome_T2 < preds_oos_T2 - se_resid_oos_T2)

ResBrain_MLR_T2$Outlier_Above_nested_T2 <- FALSE
ResBrain_MLR_T2$Outlier_Below_nested_T2 <- FALSE
ResBrain_MLR_T2$Outlier_Above_nested_T2[outlier_above_idx_T2] <- TRUE
ResBrain_MLR_T2$Outlier_Below_nested_T2[outlier_below_idx_T2] <- TRUE

ResBrain_MLR_T2$Mental_Health_nested_T2 <- dplyr::case_when(
  ResBrain_MLR_T2$Outlier_Above_nested_T2 ~ "Vulnerable_T2",
  ResBrain_MLR_T2$Outlier_Below_nested_T2 ~ "Resilience_T2",
  TRUE ~ "As_Expected_T2"
)
ResBrain_MLR_T2$Mental_Health_nested_T2 <- factor(
  ResBrain_MLR_T2$Mental_Health_nested_T2,
  levels = c("Resilience_T2", "As_Expected_T2", "Vulnerable_T2")
)

print(table(ResBrain_MLR_T2$Mental_Health_nested_T2, useNA = "ifany"))


# transfer to full sample
ResBrain_MLR_T2_retained$Mental_Health_nested_T2 <- ResBrain_MLR_T2$Mental_Health_nested_T2
stopifnot(identical(ResBrain_All_T1_T2$Proband, ResBrain_MLR_T2_retained$Proband))
ResBrain_All_T1_T2$Mental_Health_nested_T2 <- ResBrain_MLR_T2_retained$Mental_Health_nested_T2


# ----------------------------------------------------------------------------------------------------------------------------------------- #


# ----------------------------------------------------------------------------------------------------------------------------------------- #



#### Correlation matrix (T2) – predictors + nested OOS risk/residuals + outcome; save as figure ####

ResBrain_corr_T2 <- ResBrain_All_T1_T2 %>%
  dplyr::select(
    Geschlecht_2, Alter_2, Gesichertes_Risiko_Affektiv_2, Gesichertes_Risiko_Psychotisch_2, IQ_2,
    NEOFFI_Neurotizismus, NEOFFI_Extraversion, NEOFFI_Offenheit, NEOFFI_Vertraeglichkeit, NEOFFI_Gewissenhaftigkeit,
    RSQ_AngstVorNaehe, RSQ_AngstVorTrennung, 
    CTQ_Sum_2, ACE_Sum, LEQ_NegativeEventScore_2, LEQ_PositiveEventScore_2,
    PSS_Sum_2, Immigration, FSozU_Sum_2, SozDemo5_2, Bildungsjahre_2, Haushaltsnetto_2,
    cumulative_risk_nested_oos_T2, residuals_nested_T2, Outcome_T2
  ) %>%
  as.data.frame()

# Rename explicitly (safer than rename_with if order changes)
names(ResBrain_corr_T2) <- c(
  "Sex_T2","Age_T2","Familial risk AD_T2","Familial risk PD_T2","IQ_T2",
  "NEO-FFI neuroticism_T1","NEO-FFI extraversion_T1","NEO-FFI openness_T1","NEO-FFI agreeableness_T1","NEO-FFI conscientiousness_T1",
  "RSQ fear of closeness_T1","RSQ fear of abandonment_T1",
  "CTQ sum score_T2","ACE sum score_T1","LEQ negative sum score_T2","LEQ positive sum score_T2",
  "PSS sum score_T2","Immigration_T1","FSozU sum score_T2","Social socio-demographic item_T2",
  "Education_T2","Household income_T2",
  "Cumulative Risk_T2","Residuals_T2","HAMD-17 sum score_T2"
)

ordinal_vars_T2 <- c(
  "Familial risk AD_T2","Familial risk PD_T2",
  "Immigration_T1","Social socio-demographic item_T2"
)
ResBrain_corr_T2[ordinal_vars_T2] <- lapply(ResBrain_corr_T2[ordinal_vars_T2], as.factor)

cor_mat_T2 <- hetcor(ResBrain_corr_T2, ML = FALSE)

output_file <- file.path(output_dir, "correlation_matrix_T2_nested.jpeg")
jpeg(output_file, width = 3000, height = 3000, res = 300)

corrplot(
  cor_mat_T2$correlations,
  method = "color",
  type = "upper",
  tl.cex = 0.8,
  tl.col = "black",
  tl.srt = 45,
  number.cex = 0.6,
  addCoef.col = "black",
  family = "Arial"
)

dev.off()



# ----------------------------------------------------------------------------------------------------------------------------------------- #
#### MRI Export (T2) ####
ResBrain_All_T1_T2 <- ResBrain_All_T1_T2 %>%
  mutate(
    medication_T2 = case_when(
      is.na(MedIndex_Sum_2) ~ NA_character_,
      MedIndex_Sum_2 > 0    ~ "1",
      TRUE ~ "0"
    ) %>% factor(levels = c("0","1"))
  )

ResBrain_MRI_T2 <- ResBrain_All_T1_T2 %>%
  mutate(
    DurDep    = ifelse(Group == 1, 0, DurDep),          
    DurDep_T2 = ifelse(Group_T2 == 1, 0, DurDep_T2)    
  ) %>%
  dplyr::select(
    Proband, TIV, TIV_2,
    Dummy_BC_MR_pre, Dummy_BC_MR_pre_2, Dummy_BC_MR_post, Dummy_BC_MR_post_2,
    Dummy_GS_MR_post, Dummy_GC_MR_pre_2, Dummy_GC_MR_post_2,
    Interscan_Intervall,
    Alter, Alter_2, Geschlecht, Geschlecht_2, Group, Group_T2,
    residuals_nested, cumulative_risk_nested_oos,
    residuals_nested_T2, cumulative_risk_nested_oos_T2,
    medication, medication_T2, DurDep, DurDep_T2,
    Komorbid, Komorbid_T2, Outcome, Outcome_T2,
    Mental_Health_nested, Mental_Health_nested_T2,
  ) %>%
  mutate(
    Group    = ifelse(Group == 8, 2, Group),
    Group_T2 = ifelse(Group_T2 == 8, 2, Group_T2)
  )
write_xlsx(ResBrain_MRI_T2, here("03_data/999_processed_data", "ResBrain_MRI_T2.xlsx"))

ResBrain_MRI_T2_DurDep <- ResBrain_All_T1_T2 %>%
  mutate(
    DurDep    = ifelse(Group == 1, 0, DurDep),          
    DurDep_T2 = ifelse(Group_T2 == 1, 0, DurDep_T2)    
  ) %>%
  dplyr::select(
    Proband, TIV, TIV_2,
    Dummy_BC_MR_pre, Dummy_BC_MR_pre_2, Dummy_BC_MR_post, Dummy_BC_MR_post_2,
    Dummy_GS_MR_post, Dummy_GC_MR_pre_2, Dummy_GC_MR_post_2,
    Interscan_Intervall,
    Alter, Alter_2, Geschlecht, Geschlecht_2, Group, Group_T2,
    residuals_nested, cumulative_risk_nested_oos,
    residuals_nested_T2, cumulative_risk_nested_oos_T2,
    medication, medication_T2, DurDep, DurDep_T2,
    Komorbid, Komorbid_T2, Outcome, Outcome_T2,
    Mental_Health_nested, Mental_Health_nested_T2,
  ) %>%
  mutate(
    Group    = ifelse(Group == 8, 2, Group),
    Group_T2 = ifelse(Group_T2 == 8, 2, Group_T2)
  )%>%
  drop_na(DurDep_T2)

write_xlsx(ResBrain_MRI_T2_DurDep, here("03_data/999_processed_data", "ResBrain_MRI_T2_DurDep.xlsx"))

ResBrain_MRI_extreme_groups_T2 <- ResBrain_All_T1_T2 %>%
  mutate(
    DurDep    = ifelse(Group == 1, 0, DurDep),          
    DurDep_T2 = ifelse(Group_T2 == 1, 0, DurDep_T2)    
  ) %>%
  filter(Mental_Health_nested_T2 %in% c("Resilience_T2", "Vulnerable_T2")) %>%
  dplyr::select(
    Proband, TIV, TIV_2,
    Dummy_BC_MR_pre, Dummy_BC_MR_pre_2, Dummy_BC_MR_post, Dummy_BC_MR_post_2,
    Dummy_GS_MR_post, Dummy_GC_MR_pre_2, Dummy_GC_MR_post_2,
    Interscan_Intervall,
    Alter, Alter_2, Geschlecht, Geschlecht_2, Group, Group_T2,
    residuals_nested, cumulative_risk_nested_oos,
    residuals_nested_T2, cumulative_risk_nested_oos_T2,
    medication, medication_T2, DurDep, DurDep_T2,
    Outcome, Outcome_T2,
    Mental_Health_nested, Mental_Health_nested_T2
  ) %>%
  mutate(
    Group    = ifelse(Group == 8, 2, Group),
    Group_T2 = ifelse(Group_T2 == 8, 2, Group_T2)
  )
write_xlsx(
  ResBrain_MRI_extreme_groups_T2,
  here("03_data/999_processed_data", "ResBrain_MRI_extreme_groups_T2.xlsx")
)

ResBrain_MRI_extreme_groups_T1_all_data <- ResBrain_All_T1_T2 %>%
  mutate(
    DurDep    = ifelse(Group == 1, 0, DurDep),          # HC -> 0 statt NA
    DurDep_T2 = ifelse(Group_T2 == 1, 0, DurDep_T2)    # HC -> 0 statt NA
  ) %>%
  filter(Mental_Health_nested %in% c("Resilience", "Vulnerable")) %>%
  dplyr::select(
    Proband, TIV, TIV_2,
    Dummy_BC_MR_pre, Dummy_BC_MR_pre_2, Dummy_BC_MR_post, Dummy_BC_MR_post_2,
    Dummy_GS_MR_post, Dummy_GC_MR_pre_2, Dummy_GC_MR_post_2,
    Interscan_Intervall,
    Alter, Alter_2, Geschlecht, Geschlecht_2, Group, Group_T2,
    residuals_nested, cumulative_risk_nested_oos,
    residuals_nested_T2, cumulative_risk_nested_oos_T2,
    medication, medication_T2, DurDep, DurDep_T2,
    Outcome, Outcome_T2,
    Mental_Health_nested, Mental_Health_nested_T2
  ) %>%
  mutate(
    Group    = ifelse(Group == 8, 2, Group),
    Group_T2 = ifelse(Group_T2 == 8, 2, Group_T2)
  )
write_xlsx(
  ResBrain_MRI_extreme_groups_T1_all_data,
  here("03_data/999_processed_data", "ResBrain_MRI_extreme_groups_T1_all_data.xlsx")
)


ResBrain_MRI_extreme_groups_T1_all_data_DurDep <- ResBrain_All_T1_T2 %>%
  mutate(
    DurDep    = ifelse(Group == 1, 0, DurDep),          # HC -> 0 statt NA
    DurDep_T2 = ifelse(Group_T2 == 1, 0, DurDep_T2)    # HC -> 0 statt NA
  ) %>%
  filter(Mental_Health_nested %in% c("Resilience", "Vulnerable")) %>%
  dplyr::select(
    Proband, TIV, TIV_2,
    Dummy_BC_MR_pre, Dummy_BC_MR_pre_2, Dummy_BC_MR_post, Dummy_BC_MR_post_2,
    Dummy_GS_MR_post, Dummy_GC_MR_pre_2, Dummy_GC_MR_post_2,
    Interscan_Intervall,
    Alter, Alter_2, Geschlecht, Geschlecht_2, Group, Group_T2,
    residuals_nested, cumulative_risk_nested_oos,
    residuals_nested_T2, cumulative_risk_nested_oos_T2,
    medication, medication_T2, DurDep, DurDep_T2,
    Outcome, Outcome_T2,
    Mental_Health_nested, Mental_Health_nested_T2
  ) %>%
  mutate(
    Group    = ifelse(Group == 8, 2, Group),
    Group_T2 = ifelse(Group_T2 == 8, 2, Group_T2)
  )%>%
  drop_na(DurDep_T2)

write_xlsx(
  ResBrain_MRI_extreme_groups_T1_all_data_DurDep,
  here("03_data/999_processed_data", "ResBrain_MRI_extreme_groups_T1_all_data_DurDep.xlsx")
)

# ----------------------------------------------------------------------------------------------------------------------------------------- # 

# ----------------------------------------------------------------------------------------------------------------------------------------- # 



#### Environment Cleaning ####


keep <- c(
  "ResBrain",
  "ResBrain_MRI_T1",
  "ResBrain_MRI_extreme_groups_T1",
  "ResBrain_extreme_groups",
  "FOR2107_unfiltered", 
  "dev_ratio_final_1se", 
  "nested_r2", 
  "nested_mse", 
  "nested_rmse", 
  "spearman_results_T1", 
  "ResBrain_All_T1_T2", 
  "ResBrain_extreme_groups_T2", 
  "ResBrain_MRI_T2", 
  "T2_FOR2107_unfiltered", 
  "dev_ratio_final_1se_T2",
  "nested_r2_T2", 
  "nested_mse_T2", 
  "nested_rmse_T2", 
  "threshold_T1", 
  "threshold_T2"
  
)

rm(list = setdiff(ls(), keep))

