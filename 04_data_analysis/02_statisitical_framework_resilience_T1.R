#### Preamble ####

# Run 00a_metadata_and_packages.R first to load dependencies

# Define required packages
required_packages_02 <- c("haven", "here", "readxl", "writexl", "tidyverse", "ggplot2", "glmnet", "patchwork", "corrplot", "hrbrthemes", "ggpubr", "naniar", "psych")

# Check and load packages
for (pkg in required_packages_02) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(paste("Package", pkg, "is required but not installed."))
  }
  library(pkg, character.only = TRUE) # Load the package into the environment
}

# Clean up temporary variables
rm(list = c("required_packages_02", "pkg"))


# Softcode outcome variable
ResBrain$Outcome <- ResBrain$HAMD_Sum17

# ----------------------------------------------------------------------------------------------------------------------------------------- # 


# ----------------------------------------------------------------------------------------------------------------------------------------- # 

#### Nested CV (Ridge) with out-of-sample prediction ####

##### Model Specification #####
#create dataframe with only predictors AND outcome. MUST NOT contain NAs
ResBrain_MLR<- ResBrain[c("Alter", "Geschlecht","GenRisiko_Affektiv1","GenRisiko_Psycho1","CTQ_Sum","ACE_Sum","RSQ_Secure","RSQ_Preoccupied","RSQ_Dismissing","RSQ_Fearful","PSS_Sum","NEOFFI_Extraversion","NEOFFI_Neurotizismus","NEOFFI_Gewissenhaftigkeit","NEOFFI_Offenheit","NEOFFI_Vertraeglichkeit","Immigration","LEQ_NegativeEventScore","LEQ_PositiveEventScore","FSozU_Sum","SozDemo5","Bildungsjahre","Haushaltsnetto","IQ", "Outcome")]

##fit rml model
##note that due to cross-validation, exact number of outliers and model parameters may vary slightly across different runs of the code!

#fit regression model containing all predictors and extract design matrix
vars <- names(ResBrain_MLR[,-ncol(ResBrain_MLR)])
frml <- as.formula(paste("Outcome ~", paste(vars, collapse = " +")))
fit_lm <- lm(frml, data = ResBrain_MLR)
X <- model.matrix(fit_lm)[,-1] #exclude intercept


set.seed(123)

X_all <- X
y_all <- ResBrain_MLR$Outcome

K_outer <- 10
K_inner <- 10

# Outer folds (balanced)
outer_id <- sample(rep(1:K_outer, length.out = length(y_all)))

# Storage vectors (length N)
preds_nested_oos <- rep(NA_real_, length(y_all))
residuals_nested <- rep(NA_real_, length(y_all))

# TRUE nested cumulative risk per person (OOS, fold-specific betas)
cumulative_risk_nested_oos <- rep(NA_real_, length(y_all))

# Store chosen lambdas per outer fold
lambda_star <- rep(NA_real_, K_outer)
lambda_min_outer <- rep(NA_real_, K_outer) # for inspection, not used
mse_lambda_min_outer <- rep(NA_real_, K_outer) # for inspection, not used

# Store per-fold performance
fold_mse  <- rep(NA_real_, K_outer)
fold_rmse <- rep(NA_real_, K_outer)
fold_r2   <- rep(NA_real_, K_outer)

# Store coefficients for each outer fold (rows = predictors + intercept, cols = folds)
coef_mat_outer <- matrix(NA_real_, nrow = ncol(X_all) + 1, ncol = K_outer)
rownames(coef_mat_outer) <- c("(Intercept)", colnames(X_all))
colnames(coef_mat_outer) <- paste0("fold", 1:K_outer)

for (k in 1:K_outer) {
  
  test_idx  <- which(outer_id == k)
  train_idx <- which(outer_id != k)
  
  X_train <- X_all[train_idx, , drop = FALSE]
  y_train <- y_all[train_idx]
  
  X_test  <- X_all[test_idx, , drop = FALSE]
  y_test  <- y_all[test_idx]
  
  # Inner folds (explicit + reproducible)
  set.seed(1000 + k)
  foldid_inner <- sample(rep(1:K_inner, length.out = length(train_idx)))
  
  # Inner CV to choose lambda on TRAINING only
  cv_in <- cv.glmnet(
    x = X_train,
    y = y_train,
    alpha = 0,
    foldid = foldid_inner,
    type.measure = "mse"
  )
  
  lam <- cv_in$lambda.1se    # OR cv_in$lambda.min (choose one and be consistent)
  lambda_star[k] <- lam
  lambda_min_outer[k] <- cv_in$lambda.min # for inspection, not used
  idx_min <- which(cv_in$lambda == cv_in$lambda.min) # for inspection, not used
  mse_lambda_min_outer[k] <- cv_in$cvm[idx_min] # for inspection, not used
  
  # Store corresponding CV MSE
  mse_lambda_min_outer[k] <- cv_in$cvm[idx_min]
  
  # Refit on full outer-training with chosen lambda
  fit_out <- glmnet(
    x = X_train,
    y = y_train,
    alpha = 0,
    lambda = lam
  )
  
  # Save coefficients for this outer fold (includes intercept)
  coef_k <- as.numeric(coef(fit_out, s = lam))
  coef_mat_outer[, k] <- coef_k
  
  # OOS predictions for this test fold
  yhat_test <- as.numeric(predict(fit_out, newx = X_test, s = lam))
  preds_nested_oos[test_idx] <- yhat_test
  
  # OOS residuals for this test fold
  residuals_nested[test_idx] <- y_test - yhat_test
  
  # Extract coefficients (includes intercept)
  coef_k <- as.numeric(coef(fit_out, s = lam))
  beta0_k <- coef_k[1]
  beta_k  <- coef_k[-1]
  
  # OOS cumulative risk (NO intercept, consistent with your previous definition)
  cumulative_risk_nested_oos[test_idx] <- as.numeric(X_test %*% beta_k)
  
  # Optional: fold metrics
  fold_mse[k]  <- mean((y_test - yhat_test)^2)
  fold_rmse[k] <- sqrt(fold_mse[k])
  fold_r2[k]   <- 1 - sum((y_test - yhat_test)^2) / sum((y_test - mean(y_train))^2)
}

# ----------------------------------------------------------------------------------------------------------------------------------------- # 

##### Model Performance #####

# Attach to main df
ResBrain$outer_fold <- outer_id
ResBrain$preds_nested_oos <- preds_nested_oos
ResBrain$residuals_nested <- residuals_nested
ResBrain$cumulative_risk_nested_oos <- cumulative_risk_nested_oos

# Aggregate nested performance
nested_mse  <- mean((y_all - preds_nested_oos)^2)
nested_rmse <- sqrt(nested_mse)
nested_r2   <- 1 - sum((y_all - preds_nested_oos)^2) / sum((y_all - mean(y_all))^2)

cat("\nNested CV performance:\n")
cat("MSE :", nested_mse, "\n")
cat("RMSE:", nested_rmse, "\n")
cat("R2  :", nested_r2, "\n\n")

cat("Lambda (outer folds) summary (these are lambda.1se per fold):\n")
print(summary(lambda_star))


cat("Lambda.min summary (NOT used for model fitting):\n")
cat("Mean :", mean(lambda_min_outer, na.rm = TRUE), "\n")
cat("SD   :", sd(lambda_min_outer, na.rm = TRUE), "\n\n")
print(summary(lambda_min_outer))


cat("MSE at lambda.min (inner CV):\n")
cat("Mean :", mean(mse_lambda_min_outer, na.rm = TRUE), "\n")
cat("SD   :", sd(mse_lambda_min_outer, na.rm = TRUE), "\n\n")
print(summary(mse_lambda_min_outer))

# ----------------------------------------------------------------------------------------------------------------------------------------- #
# Coefficient stability across nested CV outer folds (mean ± SD) 
# ----------------------------------------------------------------------------------------------------------------------------------------- #


# 1) Summarise coefficients across outer folds

# Exclude intercept
beta_outer <- coef_mat_outer[rownames(coef_mat_outer) != "(Intercept)", , drop = FALSE]

beta_mean <- rowMeans(beta_outer, na.rm = TRUE)
beta_sd   <- apply(beta_outer, 1, sd, na.rm = TRUE)

# Sign stability: proportion of folds where coefficient has the same sign as the mean
sign_stability <- sapply(seq_len(nrow(beta_outer)), function(i) {
  b <- beta_outer[i, ]
  m <- beta_mean[i]
  if (is.na(m) || m == 0) return(NA_real_)
  mean(sign(b) == sign(m), na.rm = TRUE)
})

coef_stability <- data.frame(
  Variable = rownames(beta_outer),     # MUST match colnames(X_all)
  MeanBeta = as.numeric(beta_mean),
  SDBeta = as.numeric(beta_sd),
  SignStability = as.numeric(sign_stability),
  stringsAsFactors = FALSE
)

# Keep raw names (needed for SD scaling lookup)
coef_stability$Variable_raw <- coef_stability$Variable

# 2) SD scaling (continuous vars get SD from ResBrain_MLR; dummy variables get scaling 1)

# Identify continuous/numeric vars in the original dataframe (not in model.matrix)
numeric_vars_mlr <- names(ResBrain_MLR)[sapply(ResBrain_MLR, is.numeric)]
numeric_vars_mlr <- setdiff(numeric_vars_mlr, "Outcome")

# Map to columns in X (only those that exist verbatim in colnames(X_all))
numeric_X <- intersect(colnames(X_all), numeric_vars_mlr)

# Default scaling = 1 (covers all dummy columns)
std_devs_X <- rep(1, length(colnames(X_all)))
names(std_devs_X) <- colnames(X_all)

# SDs for continuous vars
std_devs_X[numeric_X] <- sapply(ResBrain_MLR[numeric_X], sd, na.rm = TRUE)

# Apply scaling using RAW names (matching X column names)
coef_stability$AdjustedMean <- coef_stability$MeanBeta * std_devs_X[coef_stability$Variable_raw]
coef_stability$AdjustedSD   <- coef_stability$SDBeta   * std_devs_X[coef_stability$Variable_raw]


# 3a) Labels for plotting 

coef_stability <- coef_stability %>%
  mutate(
    Variable = recode(Variable,
                      "Alter" = "Age",
                      "Geschlecht2" = "Sex",
                      "GenRisiko_Affektiv11" = "Familial risk AD",
                      "GenRisiko_Psycho11" = "Familial risk PD",
                      "CTQ_Sum" = "CTQ sum score",
                      "ACE_Sum" = "ACE sum score",
                      "RSQ_Secure1" = "RSQ secure",
                      "RSQ_Preoccupied1" = "RSQ preoccupied",
                      "RSQ_Dismissing1" = "RSQ dismissing",
                      "RSQ_Fearful1" = "RSQ fearful",
                      "PSS_Sum" = "PSS sum score",
                      "NEOFFI_Extraversion" = "NEO-FFI extraversion",
                      "NEOFFI_Neurotizismus" = "NEO-FFI neuroticism",
                      "NEOFFI_Gewissenhaftigkeit" = "NEO-FFI conscientiousness",
                      "NEOFFI_Offenheit" = "NEO-FFI openness",
                      "NEOFFI_Vertraeglichkeit" = "NEO-FFI agreeableness",
                      "Immigration1" = "Immigration",
                      "LEQ_NegativeEventScore" = "LEQ negative sum score",
                      "LEQ_PositiveEventScore" = "LEQ positive sum score",
                      "FSozU_Sum" = "FSozU sum score",
                      "SozDemo52" = "Social interactions  (A2): \"once per week\"",
                      "SozDemo53" = "Social interactions  (A3): \"once every two weeks\"",
                      "SozDemo54" = "Social interactions  (A4): \"once per month\"",
                      "SozDemo55" = "Social interactions  (A5): \"none except at work or similar\"",
                      "SozDemo56" = "Social interactions  (A6): \"none under any circumstances\"",
                      "Bildungsjahre" = "Education",
                      "Haushaltsnetto" = "Household Income",
                      "IQ" = "IQ")
  )


# 3b) Identify variable type (Numeric vs Factor)

numeric_vars_mlr <- names(ResBrain_MLR)[sapply(ResBrain_MLR, is.numeric)]
numeric_vars_mlr <- setdiff(numeric_vars_mlr, "Outcome")

coef_stability$Type <- ifelse(coef_stability$Variable_raw %in% numeric_vars_mlr,
                              "Numeric",
                              "Factor")


# 4) Plot mean ± SD across folds (adjusted coefficients)

plot_df <- coef_stability %>%
  mutate(
    Display = AdjustedMean,
    DisplaySD = AdjustedSD,
    Lower = Display - DisplaySD,
    Upper = Display + DisplaySD
  ) %>%
  arrange(abs(Display))

p_coef_stability <- ggplot(plot_df,
                           aes(x = reorder(Variable, Display),
                               y = Display,
                               color = Type)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.4) +
  geom_pointrange(aes(ymin = Lower, ymax = Upper), linewidth = 0.6) +
  coord_flip() +
  theme_minimal(base_size = 16) +
  scale_color_manual(values = c("Numeric" = "blue2",
                                "Factor" = "red2")) +
  labs(
    title = "Predictor weights across nested CV outer folds",
    x = "Predictor",
    y = "Adjusted coefficient (mean ± SD across folds)",
    color = "Variable type"
  )

print(p_coef_stability)


output_dir <- here("05_figures/001_defaultexp_figures")
ggsave(file.path(output_dir, "coefficients_nested_stability_mean_sd.jpeg"),
       plot = p_coef_stability, width = 12, height = 8, dpi = 300)


# ----------------------------------------------------------------------------------------------------------------------------------------- # 


##### Plot: Nested CV ridge (OOS cumulative risk + OOS residuals) #####

library(ggplot2)

plot_data_nested <- data.frame(
  Cumulative_Risk = ResBrain$cumulative_risk_nested_oos,
  Outcome = ResBrain$Outcome,
  Residuals = ResBrain$residuals_nested,
  Predicted = ResBrain$preds_nested_oos
)

# threshold_T1 based on nested residuals (same logic as before)
standard_errors_residuals_nested <- sqrt(
  sum(plot_data_nested$Residuals^2) / (nrow(ResBrain_MLR) - ncol(X))
)
threshold_T1 <- standard_errors_residuals_nested

plot_data_nested$Color <- ifelse(
  plot_data_nested$Residuals < -threshold_T1, "better-than-expected",
  ifelse(plot_data_nested$Residuals >  threshold_T1, "worse-than-expected", "as-expected")
)

color_mapping <- c(
  "better-than-expected" = "#009E73",
  "worse-than-expected"  = "#D55E00",
  "as-expected"          = "black"
)

p_nested <- ggplot(plot_data_nested, aes(x = Cumulative_Risk, y = Outcome)) +
  geom_point(aes(color = Color), size = 2) +
  # A single trend line for visualization (not the per-person predictions)
  geom_segment(
    # data = subset(plot_data_nested, Color != "as-expected"),
    aes(
      x = Cumulative_Risk, y = Outcome,
      xend = Cumulative_Risk, yend = Predicted,
      color = Color
    ),
    linewidth = 0.3, linetype = "dotted"
  ) +
  scale_color_manual(values = color_mapping) +
  labs(
    title = "Ridge-penalized Regression (T1) – Nested CV (OOS predictions)",
    x = "Cumulative Risk",
    y = "Outcome (HAM-D Score)",
    color = "Interpretation"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    plot.margin = unit(c(20, 20, 20, 20), "pt"),
    panel.grid = element_blank()
  ) +
  scale_x_continuous(
    breaks = scales::pretty_breaks(n = 8),
    labels = function(x) sprintf("%.1f", x),
    expand = c(0.01, 0.01)
  )


print(p_nested)

# Save
output_dir <- here("05_figures/001_defaultexp_figures")
output_file <- file.path(output_dir, "ridge_penalized_regression_T1_nested_true.jpeg")
ggsave(filename = output_file, plot = p_nested, width = 14, height = 6, dpi = 300)

cor.test(ResBrain$cumulative_risk_nested_oos, ResBrain$Outcome, method="spearman")
#cor.test(ResBrain$cumulative_risk_nested_oos, ResBrain$Outcome, method="pearson")



# ----------------------------------------------------------------------------------------------------------------------------------------- # 

# ----------------------------------------------------------------------------------------------------------------------------------------- # 


##### Extreme Group Classification (NESTED CV out-of-sample residuals) #####

# Use nested out-of-sample predictions/residuals
preds_oos <- as.numeric(ResBrain$preds_nested_oos)
residuals_oos <- as.numeric(ResBrain$residuals_nested)

# Sanity Check)
stopifnot(length(preds_oos) == nrow(ResBrain_MLR))
stopifnot(length(residuals_oos) == nrow(ResBrain_MLR))

# Standard residual error (nested, out-of-sample)
standard_errors_residuals_nested <- sqrt(
  sum(residuals_oos^2, na.rm = TRUE) / (nrow(ResBrain_MLR) - ncol(X))
)

# Identify subjects whose actual outcome exceeds predicted +/- 1*standard residual error
outlier_subjects_above <- which(ResBrain_MLR$Outcome > preds_oos + standard_errors_residuals_nested)
outlier_subjects_below <- which(ResBrain_MLR$Outcome < preds_oos - standard_errors_residuals_nested)

# Extract info about these subjects (within the modeling dataframe)
outlier_info_above <- ResBrain_MLR[outlier_subjects_above, ]
outlier_info_below <- ResBrain_MLR[outlier_subjects_below, ]

# Indicators (aligned to full dataset rows)
outliers_above <- rep(FALSE, nrow(ResBrain))
outliers_below <- rep(FALSE, nrow(ResBrain))
outliers_above[outlier_subjects_above] <- TRUE
outliers_below[outlier_subjects_below] <- TRUE

# Add nested predicted values and residuals to the original dataframe
ResBrain$Predicted_nested_oos <- preds_oos
ResBrain$residuals_nested <- residuals_oos

# Add indicators for outliers above and below
ResBrain$Outlier_Above_nested <- outliers_above
ResBrain$Outlier_Below_nested <- outliers_below

# Data Preprocessing for further analyses 
ResBrain <- ResBrain %>%
  mutate(sex = recode(sex, "female" = "2", "male" = "1")) %>%  # Convert to "1" and "2"
  mutate(sex = factor(sex, levels = c("1", "2")))

# Create extreme group label using NESTED flags
ResBrain <- ResBrain %>%
  mutate(
    Mental_Health_nested = case_when(
      Outlier_Above_nested == TRUE ~ "Vulnerable",
      Outlier_Below_nested == TRUE ~ "Resilience",
      TRUE ~ "As_Expected"
    ),
    Mental_Health_nested = factor(
      Mental_Health_nested,
      levels = c("Resilience", "As_Expected", "Vulnerable")
    )
  )

# quick counts
print(table(ResBrain$Mental_Health_nested, useNA = "ifany"))


# ----------------------------------------------------------------------------------------------------------------------------------------- #


# ----------------------------------------------------------------------------------------------------------------------------------------- # 



#### Miscellaneous ####

# Correlation between OOS cumulative risk and outcome (this is the "honest" one)
cor.test(ResBrain$cumulative_risk_nested_oos, ResBrain$Outcome, method = "spearman")

# Residual inspection (nested OOS residuals)
shapiro.test(ResBrain$residuals_nested)
cor.test(ResBrain$residuals_nested, ResBrain$Outcome, method = "spearman")

plot(ResBrain$residuals_nested, ResBrain$Outcome,
     pch = 1, col = "black",
     main = "Residuals (nested OOS) vs Outcome",
     xlab = "Residuals (nested, OOS)", ylab = "Outcome")
abline(lm(Outcome ~ residuals_nested, data = ResBrain), col = "red", lwd = 2)



# ----------------------------------------------------------------------------------------------------------------------------------------- # 


##### Exploring multicollinearity: predictors + nested risk/residuals + outcome #####


ResBrain_corr <- ResBrain[c("sex", "Alter", "GenRisiko_Affektiv1", "GenRisiko_Psycho1", "IQ",
                            "NEOFFI_Neurotizismus", "NEOFFI_Extraversion", "NEOFFI_Offenheit",
                            "NEOFFI_Vertraeglichkeit", "NEOFFI_Gewissenhaftigkeit",
                            "RSQ_AngstVorNaehe", "RSQ_AngstVorTrennung", 
                            "CTQ_Sum", "ACE_Sum", "LEQ_NegativeEventScore", "LEQ_PositiveEventScore",
                            "PSS_Sum", "Immigration", "FSozU_Sum", "SozDemo5", "Bildungsjahre",
                            "Haushaltsnetto",
                            "cumulative_risk_nested_oos", "residuals_nested", "Outcome")]

ResBrain_corr <- as.data.frame(ResBrain_corr) %>%
  dplyr::rename_with(~ c("Sex", "Age", "Familial risk AD", "Familial risk PD", "IQ",
                         "NEO-FFI neuroticism", "NEO-FFI extraversion", "NEO-FFI openness",
                         "NEO-FFI agreeableness", "NEO-FFI conscientiousness",
                         "RSQ fear of closeness", "RSQ fear of abandonment", 
                         "CTQ sum score", "ACE sum score",
                         "LEQ negative sum score", "LEQ positive sum score", "PSS sum score",
                         "Immigration", "FSozU sum score", "Social socio-demographic item",
                         "Education", "Household Income",
                         "Cumulative Risk", "Residuals", "HAMD-17 sum score"))

ordinal_vars <- c("Familial risk AD", "Familial risk PD", "Immigration", "Social socio-demographic item")

ResBrain_corr[ordinal_vars] <- lapply(ResBrain_corr[ordinal_vars], as.factor)
ResBrain_corr$Residuals <- as.numeric(ResBrain_corr$Residuals)

correlation_matrix_T1 <- hetcor(ResBrain_corr, ML = FALSE)


# Define output directory
output_dir <- here("05_figures/001_defaultexp_figures")

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Define file path
output_file <- file.path(output_dir, "correlation_matrix_T1_nested.jpeg")

# Open graphics device
jpeg(output_file, width = 3000, height = 3000, res = 300)

# Plot correlation matrix
corrplot(
  correlation_matrix_T1$correlations,
  method = "color",
  type = "upper",
  tl.cex = 0.8,
  tl.col = "black",
  tl.srt = 45,
  number.cex = 0.6,
  addCoef.col = "black",
  family = "Arial"
)

# Close graphics device
dev.off()



# ----------------------------------------------------------------------------------------------------------------------------------------- # 


# ----------------------------------------------------------------------------------------------------------------------------------------- # 



#### MRI Analyses: Data Preprocessing and Export ####

ResBrain <- ResBrain %>%
  mutate(
    medication = case_when(
      is.na(Sum_MED) ~ NA_character_,
      Sum_MED > 0    ~ "1",
      TRUE           ~ "0"
    ) %>% factor(levels = c("0","1"))
  )

ResBrain_MRI_T1 <- ResBrain %>%
  mutate(
    DurDep = ifelse(Group == 1, 0, DurDep),        
    DurDep = as.numeric(gsub(",", ".", DurDep))       
  ) %>%
  select(Proband, TIV, Dummy_BC_MR_pre, Dummy_BC_MR_post, Dummy_GS_MR_post,
         Alter, Geschlecht, Group, HAMD_Sum17,
         residuals_nested, cumulative_risk_nested_oos,
         medication, DurDep, Komorbid, Mental_Health_nested) %>%
  mutate(Group = ifelse(Group == 8, 2, Group))
write_xlsx(ResBrain_MRI_T1, here("03_data/999_processed_data", "ResBrain_MRI_T1.xlsx"))


ResBrain_MRI_T1_DurDep <- ResBrain %>%
  mutate(
    DurDep = ifelse(Group == 1, 0, DurDep),        
    DurDep = as.numeric(gsub(",", ".", DurDep))       
  ) %>%
  select(Proband, TIV, Dummy_BC_MR_pre, Dummy_BC_MR_post, Dummy_GS_MR_post,
         Alter, Geschlecht, Group, HAMD_Sum17,
         residuals_nested, cumulative_risk_nested_oos,
         medication, DurDep, Komorbid, Mental_Health_nested) %>%
  mutate(Group = ifelse(Group == 8, 2, Group))  %>%
  drop_na(DurDep)

write_xlsx(ResBrain_MRI_T1_DurDep, here("03_data/999_processed_data", "ResBrain_MRI_T1_DurDep.xlsx"))


# Extreme groups 
ResBrain_extreme_groups <- ResBrain %>%
  mutate(extreme_group_nested = case_when(
    Outlier_Above_nested == TRUE ~ "Vulnerable",
    Outlier_Below_nested == TRUE ~ "Resilience",
    TRUE ~ NA_character_
  )) %>%
  drop_na(extreme_group_nested)

ResBrain_MRI_extreme_groups_T1 <- ResBrain_extreme_groups %>%
  mutate(
    DurDep = ifelse(Group == 1, 0, DurDep),          # HC -> 0 statt NA
    DurDep = as.numeric(gsub(",", ".", DurDep))       # Komma -> Punkt für MATLAB
  ) %>%
  select(Proband, TIV, Dummy_BC_MR_pre, Dummy_BC_MR_post, Dummy_GS_MR_post,
         Alter, Geschlecht, Group, HAMD_Sum17,
         residuals_nested, cumulative_risk_nested_oos,
         medication, DurDep, Komorbid, Mental_Health_nested, extreme_group_nested) %>%
  mutate(Group = ifelse(Group == 8, 2, Group))
write_xlsx(ResBrain_MRI_extreme_groups_T1,
           here("03_data/999_processed_data", "ResBrain_MRI_extreme_groups_T1.xlsx"))


ResBrain_MRI_extreme_groups_T1_DurDep <- ResBrain_extreme_groups %>%
  mutate(
    DurDep = ifelse(Group == 1, 0, DurDep),          # HC -> 0 statt NA
    DurDep = as.numeric(gsub(",", ".", DurDep))       # Komma -> Punkt für MATLAB
  ) %>%
  select(Proband, TIV, Dummy_BC_MR_pre, Dummy_BC_MR_post, Dummy_GS_MR_post,
         Alter, Geschlecht, Group, HAMD_Sum17,
         residuals_nested, cumulative_risk_nested_oos,
         medication, DurDep, Komorbid, Mental_Health_nested, extreme_group_nested) %>%
  mutate(Group = ifelse(Group == 8, 2, Group))%>%
  drop_na(DurDep)

write_xlsx(ResBrain_MRI_extreme_groups_T1_DurDep,
           here("03_data/999_processed_data", "ResBrain_MRI_extreme_groups_T1_DurDep.xlsx"))

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
  "threshold_T1"
)

rm(list = setdiff(ls(), keep))


