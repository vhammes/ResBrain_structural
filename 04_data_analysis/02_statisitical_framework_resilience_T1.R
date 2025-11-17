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

#### Ridge-Penalized Regression Model ####

##### Model Specification #####
#create dataframe with only predictors AND outcome. MUST NOT contain NAs
ResBrain_MLR<- ResBrain[c("Alter", "Geschlecht","GenRisiko_Affektiv1","GenRisiko_Psycho1","CTQ_Sum","ACE_Sum","RSQ_Secure","RSQ_Preoccupied","RSQ_Dismissing","RSQ_Fearful","PSS_Sum","NEOFFI_Extraversion","NEOFFI_Neurotizismus","NEOFFI_Gewissenhaftigkeit","NEOFFI_Offenheit","NEOFFI_Vertraeglichkeit","Immigration","LEQ_NegativeEventScore","LEQ_PositiveEventScore","FSozU_Sum","SozDemo5","Bildungsjahre","Haushaltsnetto","IQ", "Outcome")]

##fit rml model
##note that due to cross-validation, exact number of outliers and model parameters may vary slightly across different runs of the code!

#fit regression model containing all predictors and extract design matrix
vars <- names(ResBrain_MLR[,-ncol(ResBrain_MLR)])
frml <- as.formula(paste("Outcome ~", paste(vars, collapse = " +")))
fit_lm <- lm(frml, data = ResBrain_MLR)
X <- model.matrix(fit_lm)

set.seed(123)  # Ensures same cross-validation splits each time
#fit and inspect regularized regression model (ridge penalty)
fit_rlm <- cv.glmnet(y = ResBrain_MLR$Outcome, x = X, alpha = 0, nfolds = 10)

# mean cross-validated error for optimal regzularization parameter lambda
mse_lambda_1se <- fit_rlm$cvm[which(fit_rlm$lambda == fit_rlm$lambda.1se)]
mse_lambda_min <- fit_rlm$cvm[which(fit_rlm$lambda == fit_rlm$lambda.min)]

#the optimal regularization parameter lambda is determined by 10-fold internal cross validation
#common optimal criterion is min(lambda) + 1*se: https://www.r-bloggers.com/2021/10/lambda-min-lambda-1se-and-cross-validation-in-lasso-binomial-response/
plot(fit_rlm)

#compare predictions from conventional and regularized regression model
preds_lm <- predict(fit_lm)
preds_rlm <- predict.glmnet(fit_rlm$glmnet.fit, newx = X, s = fit_rlm$lambda.1se)

plot(preds_lm, preds_rlm)
abline(a=0, b=1, lty=2, col="blue")
grid()
#the extent of regularization of the predictions is indicated by the deviation from the blue line
#smaller lambda (regularization parameter) -> less regularized predictions
#optimal lambda decreases when ratio of number of observations / number of predictors decreases

#Obtain summary of the linear regression model
summary_fit_lm <- summary(fit_lm)

# Assuming fit_rlm is your cv.glmnet model object
lambda_specific <- fit_rlm$lambda.1se  # This extracts the lambda.1se value from your model

# Extract coefficients at lambda.1se
coefficients_at_lambda_1se <- predict(fit_rlm$glmnet.fit, s = lambda_specific, type = "coefficients")


# ----------------------------------------------------------------------------------------------------------------------------------------- # 

##### Model Performance (Parameters) #####

# Ensure coefficients_at_lambda_1se is extracted
best_coef <- as.numeric(coefficients_at_lambda_1se[-1])  # Remove intercept
X <- model.matrix(fit_lm)  # Predictor matrix (already created, input for glmnet) 

# Compute cumulative risk
cumulative_risk <- X %*% best_coef
# Convert cumulative_risk to a vector
cumulative_risk <- as.numeric(cumulative_risk)
ResBrain$cumulative_risk <- cumulative_risk

# Residuals from the ridge regression model
ResBrain$residuals <- ResBrain$Outcome - preds_rlm
ResBrain$residuals <- as.numeric(ResBrain$residuals)

# Prepare data for plotting
plot_data <- data.frame(
  Cumulative_Risk = ResBrain$cumulative_risk,
  Outcome = ResBrain_MLR$Outcome,  # Actual HAMD scores
  Residuals = ResBrain$residuals
)
# Predicted values are calculated as:
plot_data$Predicted <- plot_data$Outcome - plot_data$Residuals  # Predicted = Outcome - Residuals

# Calculate standard residual error (threshold)
standard_errors_residuals_rlm <- sqrt(sum(plot_data$Residuals^2) / (nrow(ResBrain_MLR) - ncol(X)))  # Assuming homoscedasticity

# Define color coding based on residual thresholds
threshold <- standard_errors_residuals_rlm  # Ensure this is correctly calculated
plot_data$Color <- ifelse(
  plot_data$Residuals < -threshold, "better-than-expected",  
  ifelse(plot_data$Residuals > threshold, "worse-than-expected", "as-expected")  # Red/Grey
)

# Assign colors to categories
color_mapping <- c(
  "better-than-expected" = "#009E73",  # Custom darker green (Forest Green)
  "worse-than-expected" = "#D55E00",   
  "as-expected" = "black"              # black for "As expected"
)

# Create the plot
p <- ggplot(plot_data, aes(x = Cumulative_Risk, y = Outcome)) +
  geom_point(aes(color = Color), size = 2.5) +  # Larger data points
  geom_line(aes(x = Cumulative_Risk, y = Predicted), col = "black", size = 1) +  # Regression line using predictions
  geom_segment(
    data = subset(plot_data, Color != "as-expected"),  # Exclude "As expected" group
    aes(
      x = Cumulative_Risk, y = Outcome, 
      xend = Cumulative_Risk, yend = Predicted,  # Residual lines
      color = Color
    ),
    linewidth = 0.3, linetype = "dotted"
  ) +
  scale_color_manual(values = color_mapping) +  # Apply color mapping
  labs(
    title = "Ridge-penalized Regression (T1)",
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
print(p)

# save the plot
output_dir <- here("05_figures/001_defaultexp_figures")
output_file <- file.path(output_dir, "ridge_penalized_regression_T1.jpeg")  # Change to .pdf if needed
ggsave(filename = output_file, plot = p, width = 14, height = 6, dpi = 300)


# ----------------------------------------------------------------------------------------------------------------------------------------- # 
## Variance Explained ##
#Fraction of (Null) Deviance Explained: This is calculated as 1 - (Model Deviance / Null Deviance). 
lambda_1se_index <- which(fit_rlm$lambda == fit_rlm$lambda.1se)
dev_ratio_at_lambda_1se <- fit_rlm$glmnet.fit[["dev.ratio"]][lambda_1se_index]

# Print the dev.ratio at lambda.1se
print(paste("Deviance Ratio at lambda.1se:", dev_ratio_at_lambda_1se))

# Plotting all deviance ratios across lambda values for visualization
plot(fit_rlm$lambda, fit_rlm$glmnet.fit[["dev.ratio"]], type = 'l', 
     xlab = "Lambda", ylab = "Deviance Explained (dev.ratio)",
     main = "Deviance Explained Across Lambda Values")
abline(v = fit_rlm$lambda[lambda_1se_index], col = "red", lwd = 2, lty = 2)  # Highlight lambda.1se


# ----------------------------------------------------------------------------------------------------------------------------------------- # 
## Relative Predictor Impact ##
#  Extract coefficients at lambda.1se
lambda_1se_value <- fit_rlm$lambda.1se
coefficients_matrix <- coef(fit_rlm$glmnet.fit, s = lambda_1se_value)

# Remove intercepts from coefficients matrix
coefficients_matrix <- coefficients_matrix[-c(1, which(rownames(coefficients_matrix) == "(Intercept)")), , drop = FALSE]

# Convert coefficients to a numeric vector (assuming the first entry is the intercept)
coeff_vector <- coefficients_matrix[ drop = FALSE]@x  # Exclude the intercept
names(coeff_vector) <- rownames(coefficients_matrix)[]  # Ensure correct naming

# Calculate standard deviations for numeric predictors
numeric_vars <- names(ResBrain_MLR)[sapply(ResBrain_MLR, is.numeric) & names(ResBrain_MLR) %in% names(coeff_vector)]
std_devs <- sapply(ResBrain_MLR[numeric_vars], sd, na.rm = TRUE)

# Adjust coefficients by multiplying by the corresponding standard deviations
adjusted_coefficients <- coeff_vector
adjusted_coefficients[numeric_vars] <- coeff_vector[numeric_vars] * std_devs

# Create a dataframe for visualization
coefficients_df <- data.frame(
  Variable = names(adjusted_coefficients),
  AdjustedCoefficient = adjusted_coefficients,
  Type = ifelse(names(adjusted_coefficients) %in% numeric_vars, "Numeric", "Factor"),
  stringsAsFactors = FALSE
)


# Visualize the coefficients
# Insert: Prepare dataframe for further steps, rename factors (from german to english)
coefficients_df <- coefficients_df %>%
  mutate(Variable = recode(Variable,
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
                           "SozDemo52" = "Social socio-demographic item: A2", #A1-A5 oder so lassen,weil bezieht sich auf konkrete Antworten
                           "SozDemo53" = "Social socio-demographic item: A3",
                           "SozDemo54" = "Social socio-demographic item: A4",
                           "SozDemo55" = "Social socio-demographic item: A5",
                           "SozDemo56" = "Social socio-demographic item: A6",
                           "Bildungsjahre" = "Education",
                           "Haushaltsnetto" = "Household Income",
                           "IQ" = "IQ"
  ))


coefficients_T1 <- ggplot(coefficients_df, aes(x = reorder(Variable, AdjustedCoefficient), y = AdjustedCoefficient, fill = Type)) +
  geom_col() +
  coord_flip() +
  labs(x = "Predictor",
       y = "Adjusted Coefficient") +
  theme_minimal() +
  scale_fill_manual(name = "Variable Type", values = c("Numeric" = "blue2", "Factor" = "red2"))+
  theme(text = element_text(family = "Arial", size = 16))
# for numeric 

# Optionally, print the coefficients dataframe
print(coefficients_df)

# save the plot
output_file <- file.path(output_dir, "coefficients_T1.jpeg")  # Change to .pdf if needed
ggsave(filename = output_file, plot = coefficients_T1, width = 12, height = 8, dpi = 300)


# ----------------------------------------------------------------------------------------------------------------------------------------- # 

# Association between cumulative risk (ridge-penalized) and outcome (HAM-D)
ggplot(ResBrain, aes(x = cumulative_risk)) +
  geom_density(fill = "red", alpha = 0.5, color = "black") +
  stat_function(fun = dnorm, 
                args = list(mean = mean(ResBrain$cumulative_risk, na.rm = TRUE), 
                            sd = sd(ResBrain$cumulative_risk, na.rm = TRUE)), 
                color = "blue", 
                linetype = "dashed", 
                size = 1) +
  theme_minimal() +
  labs(title = "Dichteverteilung mit Normalverteilungslinie",
       x = "Cumulative Risk T1",
       y = "Dichte")

ggplot(ResBrain, aes(x = HAMD_Sum17)) +
  geom_density(fill = "red", alpha = 0.5, color = "black") +
  stat_function(fun = dnorm, 
                args = list(mean = mean(ResBrain$HAMD_Sum17, na.rm = TRUE), 
                            sd = sd(ResBrain$HAMD_Sum17, na.rm = TRUE)), 
                color = "blue", 
                linetype = "dashed", 
                size = 1) +
  theme_minimal() +
  labs(title = "Dichteverteilung mit Normalverteilungslinie",
       x = "HAMD_Sum17 T1",
       y = "Dichte")


shapiro.test(ResBrain$cumulative_risk)
shapiro.test(ResBrain$Outcome)

# Check correlation between cumulative risk and outcome = HAM-D sum score
spearman_result <- cor.test(ResBrain$cumulative_risk, ResBrain$HAMD_Sum17, method = "spearman")
spearman_result

# ----------------------------------------------------------------------------------------------------------------------------------------- # 


#### Miscellaneous ####

##Residual Inspection ##
shapiro.test(ResBrain$residuals)
shapiro.test(ResBrain$Outcome)

cor.test(ResBrain$residuals, ResBrain$Outcome, method = "spearman")

plot(ResBrain$residuals, ResBrain$Outcome,
     pch = 1, col = "black",
     main = "Non-linear Relationship Between Residuals and HAMD",
     xlab = "Residuals", ylab = "Outcome")

# Add linear regression line (in red)
abline(lm(Outcome ~ residuals, data = ResBrain), col = "red", lwd = 2)

# Add quadratic regression curve (in blue)
curve(predict(lm(Outcome ~ poly(residuals, 2), data = ResBrain),
              newdata = data.frame(residuals = x)),
      from = min(ResBrain$residuals), to = max(ResBrain$residuals),
      col = "blue", lwd = 2, add = TRUE)

# Add legend
legend("topleft", legend = c("Linear Fit", "Quadratic Fit"),
       col = c("red", "blue"), lwd = 2)




## Exploring risk and protection factor multicolinearity with residuals and outcome ##
#Create a new data frame
ResBrain_corr <- ResBrain[c( "sex", "Alter", "GenRisiko_Affektiv1", "GenRisiko_Psycho1", "IQ", "NEOFFI_Neurotizismus", "NEOFFI_Extraversion",
                             "NEOFFI_Offenheit", "NEOFFI_Vertraeglichkeit", "NEOFFI_Gewissenhaftigkeit", 
                             "RSQ_Secure", "RSQ_Preoccupied", "RSQ_Dismissing", "RSQ_Fearful", "CTQ_Sum", "ACE_Sum", "LEQ_NegativeEventScore",
                             "LEQ_PositiveEventScore", "PSS_Sum", "Immigration",  "FSozU_Sum", "SozDemo5",  "Bildungsjahre", 
                             "Haushaltsnetto", "cumulative_risk", "residuals", "HAMD_Sum17")]

## Prepare data for correlation analysis
# Check for correct order (Renaming process)
ResBrain_corr<- as.data.frame(ResBrain_corr) %>%
  dplyr::rename_with(~ c( c( "Sex", "Age", "Familial risk AD", "Familial risk PD", "IQ", "NEO-FFI neuroticism", "NEO-FFI extraversion",
                             "NEO-FFI openness", "NEO-FFI agreeableness", "NEO-FFI conscientiousness", "RSQ secure", "RSQ preoccupied", 
                             "RSQ dismissing", "RSQ fearful", "CTQ sum score", "ACE sum score", 
                             "LEQ negative sum score", "LEQ positive sum score", "PSS sum score", "Immigration", "FSozU sum score", 
                             "Social socio-demographic item", "Education", "Household Income", "Cumulative Risk", "Residuals", "HAMD-17 sum score")))


# Convert ordinal variables to factors
ordinal_vars <- c(
  "Familial risk AD", "Familial risk PD", "RSQ secure", 
  "RSQ preoccupied", "RSQ dismissing", "RSQ fearful",
  "Immigration", "Social socio-demographic item"
)
ResBrain_corr[ordinal_vars] <- lapply(ResBrain_corr[ordinal_vars], as.factor)
ResBrain_corr$Residuals <- as.numeric(ResBrain_corr$Residuals)

# Compute mixed-type correlations
correlation_matrix_T1 <- hetcor(ResBrain_corr, ML = FALSE)

#Change order of the factors, define order
new_order <- c("Sex", "Age", "Familial Risk AD", "Familial Risk PD", "IQ")

# Check if the computation was successful
if (is.null(correlation_matrix_T1)) {
  stop("Correlation matrix calculation failed. Check data formatting.")
}

# Define the output directory and create it if it doesn't exist
# This will update correltaion matrix from 01_data_preprocessing_T1 with residual score, cumulative risk, and HAM-D score
output_dir <- here("05_figures/001_defaultexp_figures")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)  # Create directory if missing
}

# Define the output file path
output_file <- file.path(output_dir, "correlation_matrix_T1.jpeg")

#jpeg(output_file, width = 1500, height = 1200, res = 150)
jpeg(output_file, width = 3000, height = 3000, res = 300)

# Visualize the correlation matrix
corrplot(
  correlation_matrix_T1$correlations,
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

# Extract standard errors of the residuals
standard_errors_residuals <- summary_fit_lm$sigma

# better than expected outcome using the regulated model
# Calculate residuals from the model predictions
# Assuming 'fit_rlm' is your regularized regression model
# predicted_values <- predict.glmnet(fit_rlm$glmnet.fit, newx = X, s = fit_rlm$lambda.1se)
residuals <- ResBrain_MLR$Outcome - preds_rlm
ResBrain$residuals <- ResBrain_MLR$Outcome - preds_rlm

# Calculate standard residual errors
# Assuming 'fit_rlm' is your regularized regression model
standard_errors_residuals_rlm <- sqrt(sum(residuals^2) / (nrow(ResBrain_MLR) - ncol(X)))  # Assuming homoscedasticity

# Identify subjects whose actual outcome score exceeds predicted +/- 1*standard residual error
outlier_subjects_above <- which(ResBrain_MLR$Outcome > preds_rlm + standard_errors_residuals_rlm)
outlier_subjects_below <- which(ResBrain_MLR$Outcome < preds_rlm - standard_errors_residuals_rlm)

# Extract information about these subjects
outlier_info_above <- ResBrain_MLR[outlier_subjects_above, ]
outlier_info_below <- ResBrain_MLR[outlier_subjects_below, ]

# Identify outliers above and below
outliers_above <- rep(FALSE, nrow(ResBrain))
outliers_below <- rep(FALSE, nrow(ResBrain))
outliers_above[outlier_subjects_above] <- TRUE
outliers_below[outlier_subjects_below] <- TRUE

# Add predicted values and residuals to the original dataframe
ResBrain$Predicted <- preds_rlm
ResBrain$residuals <- residuals

# Add indicators for outliers above and below to the original dataframe
ResBrain$Outlier_Above <- outliers_above
ResBrain$Outlier_Below <- outliers_below

# Data Preprocessing for further analyses
ResBrain <- ResBrain %>%
  mutate(sex = recode(sex, "female" = "2", "male" = "1")) %>%  # Convert to "1" and "2"
  mutate(sex = factor(sex, levels = c("1", "2")))  # Ensure the correct factor levels

ResBrain = ResBrain %>%
  mutate(Mental_Health = case_when(Outlier_Above==TRUE ~ 'Vulnerable',
                                   Outlier_Below==TRUE ~ 'Resilience', 
                                   Outlier_Above==FALSE & Outlier_Below==FALSE ~ 'As_Expected'))%>%
  mutate(Mental_Health = factor(Mental_Health, levels = c("Resilience", "As_Expected", "Vulnerable")))


# ----------------------------------------------------------------------------------------------------------------------------------------- # 


#### MRI Analyses: Data Preprocessing and Export ####
# For subsequent MRI analyses, relevant data is preprocessed and isolated 
ResBrain <- ResBrain %>%
  mutate(
    medication = case_when(
      is.na(Sum_MED) ~ NA_character_,      # keep missing if Sum_MED missing
      Sum_MED > 0    ~ "1",
      TRUE           ~ "0"
    ) %>% factor(levels = c("0","1"))
  )

# Select MRI-relevant variables
ResBrain_MRI_T1 <- ResBrain %>%
  select(Proband, TIV, Dummy_BC_MR_pre, Dummy_BC_MR_post, Dummy_GS_MR_post,  
         Alter, Geschlecht, Group, residuals, cumulative_risk, medication, Mental_Health)%>%
  mutate(Group = ifelse(Group == 8, 2, Group))

# Save MRI-relevant data
write_xlsx(ResBrain_MRI_T1, here("03_data/999_processed_data", "ResBrain_MRI_T1.xlsx"))

# Extreme Groups
ResBrain_extreme_groups = ResBrain %>%
  mutate(extreme_group = case_when(Outlier_Above==TRUE ~ 'Vulnerable',
                                   Outlier_Below==TRUE ~ 'Resilience'))

ResBrain_extreme_groups <- ResBrain_extreme_groups %>%
  drop_na(extreme_group)


ResBrain_MRI_extreme_groups_T1 <- ResBrain_extreme_groups%>%
  select(Proband, TIV, Dummy_BC_MR_pre, Dummy_BC_MR_post, Dummy_GS_MR_post,  
         Alter, Geschlecht, Group, residuals, cumulative_risk, medication, Mental_Health)%>%
  mutate(Group = ifelse(Group == 8, 2, Group))

write_xlsx(ResBrain_MRI_extreme_groups_T1, here("03_data/999_processed_data", "ResBrain_MRI_extreme_groups_T1.xlsx"))


# ----------------------------------------------------------------------------------------------------------------------------------------- # 


#### Environment Cleaning ####

# Cleanup: Remove temporary variables and objects
rm(list = c("adjusted_coefficients",      "best_coef" ,                   
            "coeff_vector",                  "coefficients_at_lambda_1se",   
            "coefficients_df",              "coefficients_matrix",          
            "color_mapping" ,                "common_cols",                
            "cumulative_risk",               "fit_lm",
            "lambda_1se_index",              "new_order",
            "lambda_specific",               "name_changes",  
            "output_df",                     "output_dir",                    
            "output_file",                   "factor_cols",
            "numeric_vars",                  "outlier_info_above" ,          
            "outlier_info_below",            "outliers_above",               
            "outliers_below",                "p",                            
            "plot_data",                     "preds_lm",                     
            "preds_rlm",                     "PRS_Data_FOR2107_T1",          
            "ResBrain_final",                "ResBrain_prepro", 
            "ResBrain_MLR",                  "sorted_as_expected",
            "standard_errors_residuals",     "residuals",  
            "std_devs",                      "summary_fit_lm", 
            "threshold",                     "top_200_smallest_abs_residuals",
            "coefficients_T1",               "coefficients_matrix_T1",
            "spearman_result",               "frml",
            "mse_lambda_min",                "ResBrain_MRI_extreme_groups_T1", 
            "ResBrain_corr",                 "X"
))              


# Verify cleanup
print(ls())  # List remaining objects in the environment

