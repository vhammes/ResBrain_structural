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


#### Ridge-Penalized Regression Model ####

##### Preprocessing T1 and T2 data #####
ResBrain_All_T1_T2$Outcome_T2 <- ResBrain_All_T1_T2$HAMD_Sum17_2

ResBrain_MLR_T2 <- ResBrain_All_T1_T2 %>%
  select(Proband, FSozU_Sum_2, SozDemo5_2, LEQ_NegativeEventScore_2, 
         LEQ_PositiveEventScore_2, CTQ_Sum_2, Gesichertes_Risiko_Affektiv_2, 
         Gesichertes_Risiko_Psychotisch_2, Bildungsjahre_2, IQ_2, PSS_Sum_2, 
         ACE_Sum, RSQ_Secure, RSQ_Fearful, RSQ_Dismissing, RSQ_Preoccupied,                      
         NEOFFI_Extraversion, NEOFFI_Neurotizismus, NEOFFI_Vertraeglichkeit, NEOFFI_Offenheit, NEOFFI_Gewissenhaftigkeit,                     
         Immigration, Haushaltsnetto_2, Alter_2, Geschlecht_2, Outcome_T2)




##### Model Specification #####

#create dataframe with only predictors AND outcome. MUST NOT contain NAs
ResBrain_MLR_T2 <- subset(ResBrain_MLR_T2, select = -Proband)


#fit regression model containing all predictors and extract design matrix
vars_2 <- names(ResBrain_MLR_T2[,-ncol(ResBrain_MLR_T2)])
frml_2 <- as.formula(paste("Outcome_T2 ~", paste(vars_2, collapse = " +")))
fit_lm_2 <- lm(frml_2, data = ResBrain_MLR_T2)
X_T2 <- model.matrix(fit_lm_2)


##fit rml model

set.seed(123)  # Ensures same cross-validation splits each time
#fit and inspect regularized regression model (ridge penalty)
fit_rlm_T2 <- cv.glmnet(y = ResBrain_MLR_T2$Outcome_T2, x = X_T2, alpha = 0, nfolds = 10)

mse_lambda_1se_T2 <- fit_rlm_T2$cvm[which(fit_rlm_T2$lambda == fit_rlm_T2$lambda.1se)]
mse_lambda_min_T2 <- fit_rlm_T2$cvm[which(fit_rlm_T2$lambda == fit_rlm_T2$lambda.min)]


#the optimal regularization parameter lambda is determined by 10-fold internal cross validation
#common optimality criterion is min(lambda) + 1*se: https://www.r-bloggers.com/2021/10/lambda-min-lambda-1se-and-cross-validation-in-lasso-binomial-response/
plot(fit_rlm_T2)

#compare predictions from conventional and regularized regression model
preds_lm_T2 <- predict(fit_lm_2)
preds_rlm_T2 <- predict.glmnet(fit_rlm_T2$glmnet.fit, newx = X_T2, s = fit_rlm_T2$lambda.1se)

plot(preds_lm_T2, preds_rlm_T2)
abline(a=0, b=1, lty=2, col="blue")
grid()
#the extent of regularization of the predictions is indicated by the deviation from the blue line
#smaller lambda (regularization parameter) -> less regularized predictions
#optimal lambda decreases when ratio of number of observations / number of predictors decreases

#Obtain summary of the linear regression model
summary_fit_lm_2 <- summary(fit_lm_2)

# Assuming fit_rlm_T2 is your cv.glmnet model object
lambda_specific_T2 <- fit_rlm_T2$lambda.1se  # This extracts the lambda.1se value from your model

# Extract coefficients at lambda.1se
coefficients_at_lambda_1se_T2 <- predict(fit_rlm_T2$glmnet.fit, s = lambda_specific_T2, type = "coefficients")



# ----------------------------------------------------------------------------------------------------------------------------------------- # 


##### Model Performance (Parameters) #####

# Ensure coefficients_at_lambda_1se is extracted
best_coef_T2 <- as.numeric(coefficients_at_lambda_1se_T2[-1])  # Remove intercept
X_T2 <- model.matrix(frml_2, data = ResBrain_MLR_T2)  # Use the formula but extract it directly

# Compute cumulative risk
cumulative_risk_T2 <- X_T2 %*% best_coef_T2
# Convert cumulative_risk to a vector
cumulative_risk_T2 <- as.numeric(cumulative_risk_T2)
ResBrain_All_T1_T2$cumulative_risk_T2 <- cumulative_risk_T2

# Residuals from the ridge regression model
ResBrain_All_T1_T2$residuals_T2 <- ResBrain_All_T1_T2$Outcome_T2 - preds_rlm_T2
ResBrain_All_T1_T2$residuals_T2 <- as.numeric(ResBrain_All_T1_T2$residuals_T2)

# Prepare data for plotting
plot_data_T2 <- data.frame(
  Cumulative_Risk_T2 = ResBrain_All_T1_T2$cumulative_risk_T2,
  Outcome_T2 = ResBrain_All_T1_T2$Outcome_T2,  # Actual HAMD scores
  Residuals_T2 = ResBrain_All_T1_T2$residuals_T2
)
# Predicted values are calculated as:
plot_data_T2$Predicted_T2 <- plot_data_T2$Outcome_T2 - plot_data_T2$Residuals_T2  # Predicted = Outcome - Residuals

# Calculate standard residual error (threshold)
standard_errors_residuals_rlm_T2 <- sqrt(sum(plot_data_T2$Residuals_T2^2) / (nrow(ResBrain_MLR_T2) - ncol(X_T2)))  # Assuming homoscedasticity

# Define color coding based on residual thresholds
threshold_T2 <- standard_errors_residuals_rlm_T2  # Ensure this is correctly calculated
plot_data_T2$Color <- ifelse(
  plot_data_T2$Residuals_T2 < -threshold_T2, "better-than-expected",  
  ifelse(plot_data_T2$Residuals_T2 > threshold_T2, "worse-than-expected", "as-expected")  # Red/Grey
)

# Assign colors to categories
color_mapping <- c(
  "better-than-expected" = "#009E73",  # Custom darker green (Forest Green)
  "worse-than-expected" = "#D55E00",   
  "as-expected" = "black"              # black for "As expected"
)


# Create the plot
p_T2 <- ggplot(plot_data_T2, aes(x = Cumulative_Risk_T2, y = Outcome_T2)) +
  geom_point(aes(color = Color), size = 2.5) +  # Larger data points
  geom_line(aes(x = Cumulative_Risk_T2, y = Predicted_T2), col = "black", size = 1) +  # Regression line using predictions
  geom_segment(
    data = subset(plot_data_T2, Color != "as-expected"),  # Exclude "As expected" group
    aes(
      x = Cumulative_Risk_T2, y = Outcome_T2, 
      xend = Cumulative_Risk_T2, yend = Predicted_T2,  # Residual lines
      color = Color
    ),
    linewidth = 0.3, linetype = "dotted"
  ) +
  scale_color_manual(values = color_mapping) +  # Apply color mapping
  labs(
    title = "Ridge-penalized Regression (T2)",
    x = "Cumulative Risk",
    y = "Outcome (HAM-D Score)",
    color = "Interpretation"
  ) +
  theme_minimal(base_size = 18) +
  theme(
    legend.position = "right",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
    plot.margin = unit(c(20, 20, 20, 20), "pt"), 
    panel.grid = element_blank(),  # Removes all grid lines
  ) +
  scale_x_continuous(
    breaks = scales::pretty_breaks(n = 8),
    labels = function(x) sprintf("%.1f", x),
    expand = c(0.01, 0.01)
  )

# Print the plot
print(p_T2)


# save the plot
output_dir <- here("05_figures/001_defaultexp_figures")
output_file <- file.path(output_dir, "ridge_penalized_regression_T2.jpeg")  # Change to .pdf if needed
ggsave(filename = output_file, plot = p_T2, width = 14, height = 6, dpi = 300)



# ----------------------------------------------------------------------------------------------------------------------------------------- # 
## Variance Explained ##
# Fraction of (Null) Deviance Explained: This is calculated as 1 - (Model Deviance / Null Deviance). 
lambda_1se_index_T2 <- which(fit_rlm_T2$lambda == fit_rlm_T2$lambda.1se)
dev_ratio_at_lambda_1se_T2 <- fit_rlm_T2$glmnet.fit[["dev.ratio"]][lambda_1se_index_T2]

# Print the dev.ratio at lambda.1se
print(paste("Deviance Ratio at lambda.1se:", dev_ratio_at_lambda_1se_T2))

# Plotting all deviance ratios across lambda values for visualization
plot(fit_rlm_T2$lambda, fit_rlm_T2$glmnet.fit[["dev.ratio"]], type = 'l', 
     xlab = "Lambda", ylab = "Deviance Explained (dev.ratio)",
     main = "Deviance Explained Across Lambda Values")
abline(v = fit_rlm_T2$lambda[lambda_1se_index_T2], col = "red", lwd = 2, lty = 2)  # Highlight lambda.1se

# ----------------------------------------------------------------------------------------------------------------------------------------- # 

## Relative Predictor Impact ##
# Step 1: Extract coefficients at lambda.1se
lambda_1se_value_T2 <- fit_rlm_T2$lambda.1se
coefficients_matrix_T2 <- coef(fit_rlm_T2$glmnet.fit, s = lambda_1se_value_T2)

# Remove intercepts from coefficients matrix
coefficients_matrix_T2 <- coefficients_matrix_T2[-c(1, which(rownames(coefficients_matrix_T2) == "(Intercept)")), , drop = FALSE]

# Convert coefficients to a numeric vector (assuming the first entry is the intercept)
coeff_vector_T2 <- coefficients_matrix_T2[ drop = FALSE]@x  # Exclude the intercept
names(coeff_vector_T2) <- rownames(coefficients_matrix_T2)[]  # Ensure correct naming

# Step 2: Calculate standard deviations for numeric predictors
numeric_vars_T2 <- names(ResBrain_MLR_T2)[sapply(ResBrain_MLR_T2, is.numeric) & names(ResBrain_MLR_T2) %in% names(coeff_vector_T2)]
std_devs_T2 <- sapply(ResBrain_MLR_T2[numeric_vars_T2], sd, na.rm = TRUE)

# Step 3: Adjust coefficients by multiplying by the corresponding standard deviations
adjusted_coefficients_T2 <- coeff_vector_T2
adjusted_coefficients_T2[numeric_vars_T2] <- coeff_vector_T2[numeric_vars_T2] * std_devs_T2

# Step 4: Create a dataframe for visualization
coefficients_df_T2 <- data.frame(
  Variable = names(adjusted_coefficients_T2),
  AdjustedCoefficient_T2 = adjusted_coefficients_T2,
  Type = ifelse(names(adjusted_coefficients_T2) %in% numeric_vars_T2, "Numeric", "Factor"),
  stringsAsFactors = FALSE
)

# Insert: Prepare dataframe for further steps, rename factors (from german to english)
coefficients_df_T2 <- coefficients_df_T2 %>%
  mutate(Variable = recode(Variable,
                           "FSozU_Sum_2" = "FSozU sum score_2",
                           "SozDemo5_22" = "Social socio-demographic item: A2_2", 
                           "SozDemo5_23" = "Social socio-demographic item: A3_2",
                           "SozDemo5_24" = "Social socio-demographic item: A4_2",
                           "SozDemo5_25" = "Social socio-demographic item: A5_2",
                           "SozDemo5_26" = "Social socio-demographic item: A6_2",
                           "LEQ_NegativeEventScore_2" = "LEQ negative sum score_2",
                           "LEQ_PositiveEventScore_2" = "LEQ positive sum score_2",
                           "CTQ_Sum_2" = "CTQ sum score_2",
                           "Gesichertes_Risiko_Affektiv_21" = "Familial risk AD_2",
                           "Gesichertes_Risiko_Psychotisch_21" = "Familial risk PD_2", 
                           "Bildungsjahre_2" = "Education_2",
                           "IQ_2" = "IQ_2",
                           "PSS_Sum_2" = "PSS sum score_2",
                           "ACE_Sum" = "ACE sum score",
                           "RSQ_Secure1" = "RSQ secure",
                           "RSQ_Fearful1" = "RSQ fearful",
                           "RSQ_Dismissing1" = "RSQ dismissing",
                           "RSQ_Preoccupied1" = "RSQ preoccupied",
                           "NEOFFI_Extraversion" = "NEO-FFI extraversion",
                           "NEOFFI_Neurotizismus" = "NEO-FFI neuroticism",
                           "NEOFFI_Vertraeglichkeit" = "NEO-FFI agreeableness",
                           "NEOFFI_Offenheit" = "NEO-FFI openness",
                           "NEOFFI_Gewissenhaftigkeit" = "NEO-FFI conscientiousness",
                           "Immigration1" = "Immigration",
                           "Haushaltsnetto_2" = "Household Income_2",
                           "Alter_2" = "Age_2", 
                           "Geschlecht_22" = "Sex_2", 
                           
  ))

# Distinguishing between measurement times (T1 and T2)
coefficients_df_T2$Variable <- ifelse(
  grepl("_2", coefficients_df_T2$Variable),
  sub("_2", " T2", coefficients_df_T2$Variable),
  paste0(coefficients_df_T2$Variable, " T1")
)


# Step 5: Visualize the coefficients
coefficients_T2 <- ggplot(coefficients_df_T2, aes(x = reorder(Variable, AdjustedCoefficient_T2), y = AdjustedCoefficient_T2, fill = Type)) +
  geom_col() +
  coord_flip() +
  labs(x = "Predictor",
       y = "Adjusted Coefficient") +
  theme_minimal() +
  scale_fill_manual(name = "Variable Type", values = c("Numeric" = "blue2", "Factor" = "red2"))+
  theme(text = element_text(family = "Arial", size = 16))
# for numeric 

# Optionally, print the coefficients dataframe
print(coefficients_T2)


# save the plot
output_dir <- here("05_figures/001_defaultexp_figures")
output_file <- file.path(output_dir, "coefficients_T2.jpeg")  # Change to .pdf if needed
ggsave(filename = output_file, plot = coefficients_T2, width = 12, height = 8, dpi = 300)


# ----------------------------------------------------------------------------------------------------------------------------------------- # 

#### Miscellaneous ####
## model performance cum risk ~ hamd##
ggplot(ResBrain_All_T1_T2, aes(x = cumulative_risk_T2)) +
  geom_density(fill = "red", alpha = 0.5, color = "black") +
  stat_function(fun = dnorm, 
                args = list(mean = mean(ResBrain_All_T1_T2$cumulative_risk_T2, na.rm = TRUE), 
                            sd = sd(ResBrain_All_T1_T2$cumulative_risk_T2, na.rm = TRUE)), 
                color = "blue", 
                linetype = "dashed", 
                size = 1) +
  theme_minimal() +
  labs(title = "Dichteverteilung mit Normalverteilungslinie",
       x = "Cumulative Risk T2",
       y = "Dichte")

ggplot(ResBrain_All_T1_T2, aes(x = HAMD_Sum17_2)) +
  geom_density(fill = "red", alpha = 0.5, color = "black") +
  stat_function(fun = dnorm, 
                args = list(mean = mean(ResBrain_All_T1_T2$HAMD_Sum17_2, na.rm = TRUE), 
                            sd = sd(ResBrain_All_T1_T2$HAMD_Sum17_2, na.rm = TRUE)), 
                color = "blue", 
                linetype = "dashed", 
                size = 1) +
  theme_minimal() +
  labs(title = "Dichteverteilung mit Normalverteilungslinie",
       x = "HAMD_Sum17 T2",
       y = "Dichte")


shapiro.test(ResBrain_All_T1_T2$cumulative_risk_T2)
shapiro.test(ResBrain_All_T1_T2$HAMD_Sum17_2)

spearman_result <- cor.test(ResBrain_All_T1_T2$cumulative_risk_T2, ResBrain_All_T1_T2$HAMD_Sum17_2, method = "spearman")
spearman_result


## Residual Inspection ##
shapiro.test(ResBrain_All_T1_T2$residuals_T2)
shapiro.test(ResBrain_All_T1_T2$Outcome_T2)

cor.test(ResBrain_All_T1_T2$residuals_T2, ResBrain_All_T1_T2$Outcome_T2, method = "spearman")


## Exploring risk and protection factor multicolinearity with residuals and outcome ##
#Create a new data frame
ResBrain_prepro_T2_residuals <- ResBrain_All_T1_T2[c("Geschlecht_2", "Alter_2", "Gesichertes_Risiko_Affektiv_2", "Gesichertes_Risiko_Psychotisch_2", "IQ_2", "NEOFFI_Neurotizismus", "NEOFFI_Extraversion",
                                                     "NEOFFI_Offenheit", "NEOFFI_Vertraeglichkeit", "NEOFFI_Gewissenhaftigkeit", 
                                                     "RSQ_Secure", "RSQ_Preoccupied", "RSQ_Dismissing", "RSQ_Fearful", "CTQ_Sum_2", "ACE_Sum", "LEQ_NegativeEventScore_2",
                                                     "LEQ_PositiveEventScore_2", "PSS_Sum_2", "Immigration",  "FSozU_Sum_2", "SozDemo5_2",
                                                     "Bildungsjahre_2", "Haushaltsnetto_2", "cumulative_risk_T2", "residuals_T2", "HAMD_Sum17_2")]

#Rename variables
ResBrain_prepro_T2_residuals <- as.data.frame(ResBrain_prepro_T2_residuals) %>%
  dplyr::rename_with(~ c("Sex_2", "Age_2", "Familial risk AD_2", "Familial risk PD_2", "IQ_2", "NEO-FFI neuroticism", "NEO-FFI extraversion",
                         "NEO-FFI openness", "NEO-FFI agreeableness", "NEO-FFI conscientiousness", "RSQ secure", "RSQ preoccupied", 
                         "RSQ dismissing", "RSQ fearful", "CTQ sum score_2", "ACE sum score", 
                         "LEQ negative sum score_2", "LEQ positive sum score_2", "PSS sum score_2", "Immigration", "FSozU sum score_2", "Social socio-demographic item_2",
                         "Education_2", "Household Income_2", "Cumulative Risk_2", "Residuals_2", "HAMD-17 sum score_2" ))

# Distinguishing between measurement times (T1 and T2)
names(ResBrain_prepro_T2_residuals) <- ifelse(grepl("_2", names(ResBrain_prepro_T2_residuals)),
                                              sub("_2", "_T2", names(ResBrain_prepro_T2_residuals)),
                                              paste0(names(ResBrain_prepro_T2_residuals), "_T1"))

# Prepare for correlation
ordinal_vars <- c(
  "Familial risk AD_T2", "Familial risk PD_T2", "RSQ secure_T1", 
  "RSQ preoccupied_T1", "RSQ dismissing_T1", "RSQ fearful_T1",
  "Immigration_T1", "Social socio-demographic item_T2"
)

ResBrain_prepro_T2_residuals[ordinal_vars] <- lapply(ResBrain_prepro_T2_residuals[ordinal_vars], as.factor)

# Compute mixed-type correlations
correlation_matrix_T2_residuals <- hetcor(ResBrain_prepro_T2_residuals, ML = FALSE)

# Define the output file path
correlation_matrix_T2_residuals <- tryCatch({
  hetcor(ResBrain_prepro_T2_residuals, ML = FALSE)
})

#Change order of the factors, define order
new_order <- c("Sex_T2", "Age_T2", "Familial Risk AD_T2", "Familial Risk PD_T2", "IQ_T2")

# Check if the computation was successful
if (is.null(correlation_matrix_T2_residuals)) {
  stop("Correlation matrix calculation failed. Check data formatting.")
}

# Define the output directory and create it if it doesn't exist
# This will update correltaion matrix from 04_data_preprocessing_T3 with residual score, cumulative risk, and HAM-D score
output_dir <- here("05_figures/001_defaultexp_figures")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)  # Create directory if missing
}

# Define the output file path
output_file <- file.path(output_dir, "correlation_matrix_T2.jpeg")

# Open the JPEG graphics device
#jpeg(output_file, width = 1500, height = 1200, res = 150)
jpeg(output_file, width = 3000, height = 3000, res = 300)

# Visualize the correlation matrix
corrplot(
  correlation_matrix_T2_residuals$correlations,
  method = "color",
  tl.cex = 0.8,
  tl.col = "black",
  tl.srt = 45,
  type = 'upper',
  number.cex = 0.6,
  addCoef.col = "black",
  family = "Arial"
)

dev.off()


# ----------------------------------------------------------------------------------------------------------------------------------------- # 

#### Extreme Group Classification ####

# Calculate standard residual errors
# Assuming 'fit_rlm' is your regularized regression model
residuals_T2 <- ResBrain_All_T1_T2$residuals_T2

standard_errors_residuals_rlm_T2 <- sqrt(sum(residuals_T2^2) / (nrow(ResBrain_MLR_T2) - ncol(X_T2)))  # Assuming homoscedasticity

# Identify subjects whose actual outcome score exceeds predicted +/- 1*standard residual error
outlier_subjects_above_T2 <- which(residuals_T2 > standard_errors_residuals_rlm_T2)
outlier_subjects_below_T2 <- which(residuals_T2 < -standard_errors_residuals_rlm_T2)

# Extract information about these subjects
outlier_info_above_T2 <- ResBrain_MLR_T2[outlier_subjects_above_T2, ]
outlier_info_below_T2 <- ResBrain_MLR_T2[outlier_subjects_below_T2, ]

# Identify outliers above and below
outliers_above_T2 <- rep(FALSE, nrow(ResBrain_All_T1_T2))
outliers_below_T2 <- rep(FALSE, nrow(ResBrain_All_T1_T2))
outliers_above_T2[outlier_subjects_above_T2] <- TRUE
outliers_below_T2[outlier_subjects_below_T2] <- TRUE

# Add predicted values and residuals to the original dataframe
ResBrain_All_T1_T2$Predicted_T2 <- preds_rlm_T2
#ResBrain_All_T1_T2$Residuals_T2 <- residuals_T2_vgl

# Add indicators for outliers above and below to the original dataframe
ResBrain_All_T1_T2$Outlier_Above_T2 <- outliers_above_T2
ResBrain_All_T1_T2$Outlier_Below_T2 <- outliers_below_T2

ResBrain_All_T1_T2 = ResBrain_All_T1_T2 %>%
  mutate(Mental_Health_T2 = case_when(Outlier_Above_T2==TRUE ~ 'Vulnerable_T2',
                                      Outlier_Below_T2==TRUE ~ 'Resilience_T2', 
                                      Outlier_Above_T2==FALSE & Outlier_Below_T2==FALSE ~ 'As_Expected_T2'))


# ----------------------------------------------------------------------------------------------------------------------------------------- # 

#### MRI Analyses: Data Preprocessing and Export ####
# For subsequent MRI analyses, relevant data is preprocessed and isolated 

ResBrain_All_T1_T2 <- ResBrain_All_T1_T2 %>%
  mutate(
    medication_T2 = case_when(
      is.na(MedIndex_Sum_2) ~ NA_character_,      # keep missing if Sum_MED missing
      MedIndex_Sum_2 > 0    ~ "1",
      TRUE           ~ "0"
    ) %>% factor(levels = c("0","1"))
  )


ResBrain_MRI_T2 <- ResBrain_All_T1_T2 %>%
  select(Proband, TIV, TIV_2, Dummy_BC_MR_pre, Dummy_BC_MR_pre_2, Dummy_BC_MR_post, Dummy_BC_MR_post_2, Dummy_GS_MR_post, Dummy_GC_MR_pre_2, Dummy_GC_MR_post_2, Interscan_Intervall, 
         Alter, Alter_2, Geschlecht, Geschlecht_2, Group, Group_T2, residuals, residuals_T2, cumulative_risk, cumulative_risk_T2, medication, medication_T2, Mental_Health, Mental_Health_T2)%>%
  mutate(Group = ifelse(Group == 8, 2, Group))%>%
  mutate(Group_T2 = ifelse(Group_T2 == 8, 2, Group_T2))

write_xlsx(ResBrain_MRI_T2, here("03_data/999_processed_data", "ResBrain_MRI_T2.xlsx"))

# Extreme Groups
ResBrain_extreme_groups_T2 = ResBrain_All_T1_T2 %>%
  mutate(extreme_group_T2 = case_when(Outlier_Above_T2==TRUE ~ 'Vulnerable_T2',
                                      Outlier_Below_T2==TRUE ~ 'Resilience_T2' 
                          ))

ResBrain_extreme_groups_T2 <- ResBrain_extreme_groups_T2 %>%
  drop_na(extreme_group_T2)

ResBrain_MRI_extreme_groups_T2 <- ResBrain_extreme_groups_T2%>%
  select(Proband, TIV, TIV_2, Dummy_BC_MR_pre, Dummy_BC_MR_pre_2, Dummy_BC_MR_post, Dummy_BC_MR_post_2, Dummy_GS_MR_post, Dummy_GC_MR_pre_2, Dummy_GC_MR_post_2, Interscan_Intervall, 
         Alter, Alter_2, Geschlecht, Geschlecht_2, Group, Group_T2, residuals, residuals_T2, cumulative_risk, cumulative_risk_T2, medication, medication_T2, Mental_Health, Mental_Health_T2)%>%
  mutate(Group = ifelse(Group == 8, 2, Group))%>%
  mutate(Group_T2 = ifelse(Group_T2 == 8, 2, Group_T2))

write_xlsx(ResBrain_MRI_extreme_groups_T2, here("03_data/999_processed_data", "ResBrain_MRI_extreme_groups_T2.xlsx"))


##### Extreme Groups with T1+T2 data #####
ResBrain_MRI_extreme_groups_T1_with_T2_data <- subset(
  ResBrain_All_T1_T2,
  (Mental_Health == "Resilience" | Mental_Health == "Vulnerable"
  ))

ResBrain_MRI_extreme_groups_T1_with_T2_data <- ResBrain_MRI_extreme_groups_T1_with_T2_data%>%
  select(Proband, TIV, TIV_2, Dummy_BC_MR_pre, Dummy_BC_MR_pre_2, Dummy_BC_MR_post, Dummy_BC_MR_post_2, Dummy_GS_MR_post, Dummy_GC_MR_pre_2, Dummy_GC_MR_post_2, Interscan_Intervall, 
         Alter, Alter_2, Geschlecht, Geschlecht_2, Group, Group_T2, residuals, residuals_T2, cumulative_risk, cumulative_risk_T2, medication, medication_T2, Mental_Health, Mental_Health_T2)%>%
  mutate(Group = ifelse(Group == 8, 2, Group))%>%
  mutate(Group_T2 = ifelse(Group_T2 == 8, 2, Group_T2))

write_xlsx(ResBrain_MRI_extreme_groups_T1_with_T2_data, here("03_data/999_processed_data", "ResBrain_MRI_extreme_groups_T1_with_T2_data.xlsx"))


# ----------------------------------------------------------------------------------------------------------------------------------------- # 



#### Environment Cleaning ####

# Cleanup: Remove temporary variables and objects
rm(list = c("adjusted_coefficients",        "best_coef" ,                   
            "coeff_vector",                  "coefficients_at_lambda_1se",   
            "coefficients_df",              "coefficients_matrix",          
            "color_mapping" ,                "common_cols",                
            "cumulative_risk",               "fit_lm",
            "dev_ratio_at_lambda_1se",      "lambda_1se_index", 
            "lambda_specific",               "name_changes",                 
            "numeric_vars",                  "outlier_info_above" ,          
            "outlier_info_below",            "outlier_subjects_above",       
            "outlier_subjects_below",        "outliers_above",               
            "outliers_below",                "p",                            
            "plot_data",                     "preds_lm",                     
            "preds_rlm",                     "PRS_Data_FOR2107_T1",          
            "ResBrain_final",                "ResBrain_prepro", 
            "ResBrain_MLR",                  "sorted_as_expected",
            "standard_errors_residuals",     "residuals",  
            "std_devs",                      "summary_fit_lm", 
            "threshold",                     "top_200_smallest_abs_residuals", 
            "X_2",                           "coefficients_at_lambda_1se_T2",
            "coefficients_df_T2",            "coefficients_matrix_T2",
            "fit_lm_2",                      "p_T2",
            "residuals_T2_vgl",              "sorted_as_expected_T2",
            "top_100_smallest_abs_residuals_T2", "adjusted_coefficients_T2", 
            "coeff_vector_T2",               "identifier_var", 
            "lambda_1se_index_T2",           "lambda_specific_T2", 
            "numeric_vars_T2",               "outliers_above_T2", 
            "outliers_below_T2",             "output_dir", 
            "output_file",                   "std_devs_T2", 
            "vars_2",                        "as_expected_group_T2", 
            "correlation_matrix_T2_residuals", "plot_data_T2", 
            "preds_lm_T2",                   "ResBrain_extreme_groups_T2", 
            "ResBrain_MRI_extreme_groups_T2", "spearman_result", 
            "ResBrain_prepro_T2_residuals",   "X_T2", 
            "best_coef_T2",                   "coefficients_T2", 
            "cumulative_risk_T2",             "frml_2", 
            "mse_lambda_min_T2",              "new_order", 
            "ordinal_vars",                   "preds_lm_T2", 
            "residuals_T2",                   "threshold_T2"
))              


# Verify cleanup
print(ls())  # List remaining objects in the environment

