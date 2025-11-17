#### Preamble ####

# Run 00a_metadata_and_packages.R first to load dependencies

# Define required packages
required_packages_07 <- c("haven", "dplyr", "here", "readxl", "tidyverse", "polycor", "corrplot", "ggplot2", "ggpubr", "patchwork")

# Check and load packages
for (pkg in required_packages_07) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(paste("Package", pkg, "is required but not installed."))
  }
  library(pkg, character.only = TRUE) # Load the package into the environment
}

# Clean up temporary variables
rm(list = c("required_packages_07", "pkg"))


# ----------------------------------------------------------------------------------------------------------------------------------------- # 


#### GMV Regression Cluster Analyses ####
# Cluster eigenvariates (unadjusted) were extracted from SPM 
# ROI and whole-braine base models, significant clusters

Cluster_Values_GMV_T2 <- read_xlsx(here("03_data/999_processed_data", "regression_T2_eigenvars.xlsx"))

common_cols <- intersect(names(Cluster_Values_GMV_T2), names(ResBrain_All_T1_T2))
ResBrain_All_T1_T2 <- merge(
  ResBrain_All_T1_T2,
  Cluster_Values_GMV_T2,
  by = "Proband")

ResBrain_All_T1_T2$ROI_cluster_T2_base <- as.numeric(ResBrain_All_T1_T2$ROI_cluster_T2_base)
ResBrain_All_T1_T2$wb_cluster_T2_base <- as.numeric(ResBrain_All_T1_T2$wb_cluster_T2_base)

# ----------------------------------------------------------------------------------------------------------------------------------------- # 


##### ROI Analysis  #####

## Compute Partial Residuals (Covariate Adjustment) ##
# 1. Define the model covariates (exactly as in SPM)
covars <- c("Alter_2", "Geschlecht_2", "Interscan_Intervall", "Dummy_BC_MR_post_2", "Dummy_GC_MR_pre_2", "Dummy_GC_MR_post_2", "TIV_2")  

# 2. Compute covariate-adjusted residuals
gmv_model_ROI <- lm(ROI_cluster_T2_base ~ ., data = ResBrain_All_T1_T2[, c("ROI_cluster_T2_base", covars)])
ResBrain_All_T1_T2$gmv_adj_ROI <- resid(gmv_model_ROI)

# For Resilience
res_model_ROI <- lm(residuals ~ ., data = ResBrain_All_T1_T2[, c("residuals", covars)])
ResBrain_All_T1_T2$res_adj_ROI <- resid(res_model_ROI)

# 3. Check Assumption of Normality 
shapiro.test(ResBrain_All_T1_T2$res_adj_ROI)  # Strong deviation from Normality
shapiro.test(ResBrain_All_T1_T2$gmv_adj_ROI)  # No deviation from normality

# 4. Check correlation between adjusted values (partial correlation)
cor.test(ResBrain_All_T1_T2$res_adj_ROI, ResBrain_All_T1_T2$gmv_adj_ROI, method = "spearman")

# Plotting the distribution
ggplot(ResBrain_All_T1_T2, aes(x = gmv_adj_ROI)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "gray80", color = "black") +
  geom_density(color = "blue", size = 1) +
  labs(title = "Histogram of ROI_regression_T2 with Density Curve",
       x = "ROI_cluster_T2_base",
       y = "Density") +
  theme_minimal()

ggplot(ResBrain_All_T1_T2, aes(sample = gmv_adj_ROI)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot of ROI_regression_T2") +
  theme_minimal()

# ----------------------------------------------------------------------------------------------------------------------------------------- # 


##### Whole-Brain Based Analysis #####
## Compute Partial Residuals (Covariate Adjustment) ##

# 1. Define the model covariates (exactly as in SPM)
covars <- c("Alter_2", "Geschlecht_2", "Interscan_Intervall", "Dummy_BC_MR_post_2", "Dummy_GC_MR_pre_2", "Dummy_GC_MR_post_2", "TIV_2")  

# 2. Compute covariate-adjusted residuals
# For GMV (cluster eigenvariate)
gmv_model_wb <- lm(wb_cluster_T2_base ~ ., data = ResBrain_All_T1_T2[, c("wb_cluster_T2_base", covars)])
ResBrain_All_T1_T2$gmv_adj_wb <- resid(gmv_model_wb)

# For Resilience
res_model_wb <- lm(residuals ~ ., data = ResBrain_All_T1_T2[, c("residuals", covars)])
ResBrain_All_T1_T2$res_adj_wb <- resid(res_model_wb)

# 3. Check Assumption of Normality 
shapiro.test(ResBrain_All_T1_T2$res_adj_wb)  # Strong deviation from Normality
shapiro.test(ResBrain_All_T1_T2$gmv_adj_wb)  # Strong deviation from Normality

# 4. Check correlation between adjusted values (partial correlation)
cor.test(ResBrain_All_T1_T2$gmv_adj_wb, ResBrain_All_T1_T2$res_adj_wb, method = "spearman")


# Plotting the distribution
ggplot(ResBrain_All_T1_T2, aes(x = gmv_adj_wb)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "gray80", color = "black") +
  geom_density(color = "blue", size = 1) +
  labs(title = "Histogram of wb_regression_T2 with Density Curve",
       x = "wb_cluster_T2_base",
       y = "Density") +
  theme_minimal()

ggplot(ResBrain_All_T1_T2, aes(sample = gmv_adj_wb)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Q-Q Plot of wb_regression_T2") +
  theme_minimal()

# ----------------------------------------------------------------------------------------------------------------------------------------- # 

##### Plotting #####

# 1. ROI Plot partial residuals (covariate-adjusted relationship)
partial_plot_ROI <- ggplot(ResBrain_All_T1_T2, aes(x = res_adj_ROI, y = gmv_adj_ROI)) +
  geom_point(alpha = 0.7, color = "blue") +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(
    title = "Residual-Based Resilience (T1) ~ ROI Cluster GMV (T2)",
    x = "Residual Score T1",
    y = "ROI Cluster GMV T2"
  ) +
  scale_y_continuous(limits = c(-0.1, 0.15),
                     breaks = seq(-0.1, 0.15, 0.05)) +
  theme_classic(base_size = 16)+
  theme(
    plot.title = element_text(size = 22, face = "bold"),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20)
  )
# 2. Display and save ROI
print(partial_plot_ROI)
ggsave(
  here("05_figures/001_defaultexp_figures", "PartialResidual_ROI_GMV_T2.jpeg"),
  plot = partial_plot_ROI, width = 12, height = 6, dpi = 300
)


# 3. Whole-Brain Plot partial residuals (covariate-adjusted relationship)
partial_plot_wb <- ggplot(ResBrain_All_T1_T2, aes(x = res_adj_wb, y = gmv_adj_wb)) +
  geom_point(alpha = 0.7, color = "darkcyan") +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(
    title = "Residual-Based Resilience (T1) ~ Whole-Brain Cluster GMV (T2)",
    x = "Residual Score T1",
    y = "Whole-Brain Cluster GMV T2"
  ) +
  theme_classic(base_size = 16)+
  theme(
    plot.title = element_text(size = 22, face = "bold"),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20)
  )
# 2. Display and save Whole-Brain
print(partial_plot_wb)
ggsave(
  here("05_figures/001_defaultexp_figures", "PartialResidual_WB_GMV_T2.jpeg"),
  plot = partial_plot_wb, width = 12, height = 6, dpi = 300
)

# ----------------------------------------------------------------------------------------------------------------------------------------- # 

#### Effect Sizes Regression Analyses ####
# T and df values are entered from SPM output

# Effect Size T2 GMV Regression ROI #
t <- 5.24
df <- 800
r_partial <- sqrt(t^2 / (t^2 + df))
R2_partial <- (t^2 / (t^2 + df))
d_equiv <- (2 * r_partial) / sqrt(1 - r_partial^2)
r_partial; R2_partial; d_equiv

# Effect Size T2 GMV Regression whole-brain #
t <- 5.28
df <- 800
r_partial <- sqrt(t^2 / (t^2 + df))
R2_partial <- (t^2 / (t^2 + df))
d_equiv <- (2 * r_partial) / sqrt(1 - r_partial^2)
r_partial; R2_partial; d_equiv

# Effect Size T2 GMV Regression ROI cumulative risk #
t <- 5.60
df <- 799
r_partial <- sqrt(t^2 / (t^2 + df))
R2_partial <- (t^2 / (t^2 + df))
d_equiv <- (2 * r_partial) / sqrt(1 - r_partial^2)
r_partial; R2_partial; d_equiv

# Effect Size T2 GMV Regression ROI diagnosis #
t <- 5.40
df <- 799
r_partial <- sqrt(t^2 / (t^2 + df))
R2_partial <- (t^2 / (t^2 + df))
d_equiv <- (2 * r_partial) / sqrt(1 - r_partial^2)
r_partial; R2_partial; d_equiv

# Effect Size T2 GMV Regression ROI medication #
t <- 5.85
df <- 799
r_partial <- sqrt(t^2 / (t^2 + df))
R2_partial <- (t^2 / (t^2 + df))
d_equiv <- (2 * r_partial) / sqrt(1 - r_partial^2)
r_partial; R2_partial; d_equiv




# ----------------------------------------------------------------------------------------------------------------------------------------- # 



#### GMV Extreme Group Comparisons ####
# ROI column contains NAs only, as no significant cluster emerged in SPM-based MRI results
Group_Cluster_Values_GMV_T2 <- read_xlsx(here("03_data/999_processed_data", "group_comparison_T2_eigenvars.xlsx"))

common_cols <- intersect(names(Group_Cluster_Values_GMV_T2), names(ResBrain_All_T1_T2))
Group_Cluster_Values_GMV_T2 <- merge(
  Group_Cluster_Values_GMV_T2,
  ResBrain_All_T1_T2,
  by = "Proband")

Group_Cluster_Values_GMV_T2$ROI_cluster_T2_group <- as.numeric(Group_Cluster_Values_GMV_T2$ROI_cluster_T2_group)
Group_Cluster_Values_GMV_T2$wb_cluster_T2_group <- as.numeric(Group_Cluster_Values_GMV_T2$wb_cluster_T2_group)

# ----------------------------------------------------------------------------------------------------------------------------------------- # 

##### Adjust cluster values for covariates (ANCOVA-style) #####

# Keep only the two groups of interest
ResBrain_violin <- Group_Cluster_Values_GMV_T2

# Define the same covariates as in SPM
covars <- c("Alter_2", "Geschlecht_2", "TIV_2",
            "Dummy_BC_MR_post_2", "Dummy_GC_MR_pre_2",
            "Dummy_GC_MR_post_2", "Interscan_Intervall")

# Build a linear model with covariates only
adj_model_group_wb <- lm(wb_cluster_T2_group ~ Alter_2 + Geschlecht_2 + TIV_2 +
                           Dummy_BC_MR_post_2 + Dummy_GC_MR_pre_2 +
                           Dummy_GC_MR_post_2 + Interscan_Intervall,
                         data = ResBrain_violin)

# Extract covariate-adjusted residuals (same as partial GMV)
ResBrain_violin$gmv_adj_wb <- resid(adj_model_group_wb)



##### Plotting #####

# --- run adjusted test ---
wilcox_test_adj_wb <- wilcox.test(gmv_adj_wb ~ Mental_Health, data = ResBrain_violin)
p_label <- ifelse(
  wilcox_test_adj$p.value < 0.05,
  paste0("*p = ", format.pval(wilcox_test_adj$p.value, digits = 3, eps = 0.001)),
  paste0("n.s. (p = ", format.pval(wilcox_test_adj$p.value, digits = 3, eps = 0.001), ")")
)
print(wilcox_test_adj_wb)

violin_wb <- ggplot(ResBrain_violin, aes(x = Mental_Health, y = gmv_adj_wb, fill = Mental_Health)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.8, color = "black") +
  geom_jitter(width = 0.1, alpha = 0.5, color = "black", size = 1) +
  labs(
    title = "Extreme Group (T1) Comparison: Whole-Brain Cluster GMV (T2)",
    x = NULL,
    y = "Whole-Brain Cluster GMV T2"
  )  +
  scale_y_continuous(
    limits = c(-0.15, 0.15),
    breaks = seq(-0.15, 0.15, 0.05),
    labels = c("-0.15", "-0.10", "-0.05", "0.00", "0.05", "0.10", "0.15") 
  ) +
  theme_classic(base_size = 18) +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 20)
  ) +
  scale_fill_manual(values = c("Resilience" = "#009E73", "Vulnerable" = "#D55E00"))


print(violin_wb)


# Save
ggsave(
  here("05_figures/001_defaultexp_figures", "Violin_wb_GMV_T2_Resilience_vs_Vulnerability.jpeg"),
  plot = violin_wb, width = 12, height = 6, dpi = 300
)


#### Effect Sizes Group Comparisons ####
# Effect Size T2 GMV group comparison whole-brain #
t <- 5.00
df <- 184
r_partial <- sqrt(t^2 / (t^2 + df))
R2_partial <- (t^2 / (t^2 + df))
d_equiv <- (2 * r_partial) / sqrt(1 - r_partial^2)
r_partial; R2_partial; d_equiv

#Effect Size T2 GMV group comparison#
#ROI: Sensitivity Model Cumulative Risk
t <- 4.66
df <- 183
r_partial <- sqrt(t^2 / (t^2 + df))
R2_partial <- (t^2 / (t^2 + df))
d_equiv <- (2 * r_partial) / sqrt(1 - r_partial^2)
r_partial; R2_partial; d_equiv

#wb: Sensitivity Model Medication
t <- 4.95
df <- 183
r_partial <- sqrt(t^2 / (t^2 + df))
R2_partial <- (t^2 / (t^2 + df))
d_equiv <- (2 * r_partial) / sqrt(1 - r_partial^2)
r_partial; R2_partial; d_equiv

#Whole-Brain: Sensitivity Model Cumulative Risk
t <- 4.73
df <- 183
r_partial <- sqrt(t^2 / (t^2 + df))
R2_partial <- (t^2 / (t^2 + df))
d_equiv <- (2 * r_partial) / sqrt(1 - r_partial^2)
r_partial; R2_partial; d_equiv

#Whole-Brain: Sensitivity Model Diagnosis
t <- 5.48
df <- 183
r_partial <- sqrt(t^2 / (t^2 + df))
R2_partial <- (t^2 / (t^2 + df))
d_equiv <- (2 * r_partial) / sqrt(1 - r_partial^2)
r_partial; R2_partial; d_equiv

#Whole-Brain: Sensitivity Model Medication
t <- 5.85
df <- 799
r_partial <- sqrt(t^2 / (t^2 + df))
R2_partial <- (t^2 / (t^2 + df))
d_equiv <- (2 * r_partial) / sqrt(1 - r_partial^2)
r_partial; R2_partial; d_equiv




#### GMV Regression Clinical Course ####
##### ROI Cluster ~ Clinical Course #####
cor.test(ResBrain_All_T1_T2$gmv_adj_ROI, ResBrain_All_T1_T2$SOFAS_Sum_2, method = "spearman")
cor.test(ResBrain_All_T1_T2$gmv_adj_ROI, ResBrain_All_T1_T2$HAMD_Sum17_2, method = "spearman")
cor.test(ResBrain_All_T1_T2$gmv_adj_ROI, ResBrain_All_T1_T2$DurSymptomFree_T2, method = "spearman")
cor.test(ResBrain_All_T1_T2$gmv_adj_ROI, ResBrain_All_T1_T2$FAST_SUM_2, method = "spearman")
cor.test(ResBrain_All_T1_T2$gmv_adj_ROI, ResBrain_All_T1_T2$DurDep_T2, method = "spearman")
cor.test(ResBrain_All_T1_T2$gmv_adj_ROI, ResBrain_All_T1_T2$residuals_T2, method = "spearman")


clinical_vars <- c("DurDep_T2", "DurSymptomFree_T2", "HAMD_Sum17_2", "SOFAS_Sum_2")
labels <- c("Duration Depression (Interval)", "Duration Symptom Free (Interval)", "HAM-D T2", "SOFAS T2")

# --- Convert data to long format ---
plot_data_ROI <- ResBrain_All_T1_T2 %>%
  select(gmv_adj_ROI, all_of(clinical_vars)) %>%
  tidyr::pivot_longer(cols = all_of(clinical_vars),
                      names_to = "variable",
                      values_to = "value") %>%
  mutate(label = factor(variable, levels = clinical_vars, labels = labels))

# --- Generate plots ---
plots_ROI <- plot_data_ROI %>%
  group_by(variable) %>%
  group_split() %>%
  lapply(function(df) {
    ggplot(df, aes(x = value, y = gmv_adj_ROI)) +
      geom_point(alpha = 0.7) +
      geom_smooth(method = "lm", se = TRUE, color = "black") +
      stat_cor(
        method = "spearman",
        label.x.npc = "left",
        label.y.npc = "top",
        size = 4,
        r.accuracy = 0.02,
        p.accuracy = 0.001,
        label.sep = ", ",
        cor.coef.name = "r" 
      ) +
      theme_classic(base_size = 14) +
      labs(
        x = unique(df$label),
        y = "ROI Cluster GMV T2"
      )
  })

final_plot_ROI <- wrap_plots(plots_ROI, ncol = 2) +
  plot_annotation(title = "ROI GMV and Clinical Course Measures")

final_plot_ROI

# Save
ggsave(
  here("05_figures/001_defaultexp_figures", "ROI GMV and Clinical Course.jpeg"),
  plot = final_plot_ROI, width = 12, height = 6, dpi = 300
)


##### Whole-Brain Cluster ~ Clinical Course #####
cor.test(ResBrain_All_T1_T2$gmv_adj_wb, ResBrain_All_T1_T2$SOFAS_Sum_2, method = "spearman")
cor.test(ResBrain_All_T1_T2$gmv_adj_wb, ResBrain_All_T1_T2$HAMD_Sum17_2, method = "spearman")
cor.test(ResBrain_All_T1_T2$gmv_adj_wb, ResBrain_All_T1_T2$DurSymptomFree_T2, method = "spearman")
cor.test(ResBrain_All_T1_T2$gmv_adj_wb, ResBrain_All_T1_T2$FAST_SUM_2, method = "spearman")
cor.test(ResBrain_All_T1_T2$gmv_adj_wb, ResBrain_All_T1_T2$DurDep_T2, method = "spearman")
cor.test(ResBrain_All_T1_T2$gmv_adj_wb, ResBrain_All_T1_T2$residuals_T2, method = "spearman")


clinical_vars <- c("DurDep_T2", "DurSymptomFree_T2", "HAMD_Sum17_2", "SOFAS_Sum_2")
labels <- c("Duration Depression (Interval)", "Duration Symptom Free (Interval)", "HAM-D T2", "SOFAS T2")

# --- Convert data to long format ---
plot_data_wb <- ResBrain_All_T1_T2 %>%
  select(gmv_adj_wb, all_of(clinical_vars)) %>%
  tidyr::pivot_longer(cols = all_of(clinical_vars),
                      names_to = "variable",
                      values_to = "value") %>%
  mutate(label = factor(variable, levels = clinical_vars, labels = labels))

# --- Generate plots ---
plots_wb <- plot_data_wb %>%
  group_by(variable) %>%
  group_split() %>%
  lapply(function(df) {
    ggplot(df, aes(x = value, y = gmv_adj_wb)) +
      geom_point(alpha = 0.7) +
      geom_smooth(method = "lm", se = TRUE, color = "black") +
      stat_cor(
        method = "spearman",
        label.x.npc = "left",
        label.y.npc = "top",
        size = 4,
        r.accuracy = 0.02,
        p.accuracy = 0.001,
        label.sep = ", ",
        cor.coef.name = "r" 
      ) +
      theme_classic(base_size = 14) +
      labs(
        x = unique(df$label),
        y = "Whole-Brain Cluster GMV T2"
      )
  })

final_plot_wb <- wrap_plots(plots_wb, ncol = 2) +
  plot_annotation(title = "Whole-Brain GMV and Clinical Course Measures")

final_plot_wb

# Save
ggsave(
  here("05_figures/001_defaultexp_figures", "Whole-Brain GMV and Clinical Course.jpeg"),
  plot = final_plot_wb, width = 12, height = 6, dpi = 300
)


# ----------------------------------------------------------------------------------------------------------------------------------------- # 



#### Environment Cleaning ####

# Cleanup: Remove temporary variables and objects
rm(list = c("adj_model_group_wb", "Cluster_Values_GMV_T2", "gmv_adj_wb", "gmv_adj_ROI", "gmv_model_ROI", "gmv_model_wb", 
            "Group_Cluster_Values_GMV_T2", "plot_data_ROI", "plot_data_wb", "plots_ROI", "plots_wb", 
            "res_model_ROI", "res_model_wb", "ResBrain_violin", "wilcox_test_adj_wb", "clinical_vars", 
            "covars", "final_plot_ROI", "final_plot_wb", "partial_plot_ROI", "partial_plot_wb", 
            "violin_wb"
))

# Verify cleanup
print(ls())  # List remaining objects in the environment
