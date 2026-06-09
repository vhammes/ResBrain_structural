#### Preamble ####

# Run 00a_metadata_and_packages.R first to load dependencies

# Define required packages
required_packages_06 <- c("haven", "dplyr", "missForest", "here", "readxl", "tidyverse", "polycor", "corrplot")

# Check and load packages
for (pkg in required_packages_06) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(paste("Package", pkg, "is required but not installed."))
  }
  library(pkg, character.only = TRUE) # Load the package into the environment
}

# Clean up temporary variables
rm(list = c("required_packages_06", "pkg"))

# ----------------------------------------------------------------------------------------------------------------------------------------- # 


#### Full Sample Analyses ####

#MEDICATION: in T2 use MedIndex_Sum_2
ResBrain_All_T1_T2 <- ResBrain_All_T1_T2 %>%
  mutate(
    medication_T2 = case_when(
      is.na(MedIndex_Sum_2) ~ NA_character_,      # keep missing if Sum_MED missing
      MedIndex_Sum_2 > 0    ~ "Yes",
      TRUE           ~ "No"
    ) %>% factor(levels = c("No","Yes"))
  )

ResBrain_All_T1_T2 <- ResBrain_All_T1_T2 %>%
  mutate(across(c(Geschlecht_2, Diagnosis_T2, Rem_Diagnose1_T2, Komorbid_T2, medication_T2), as.factor))


vars_T2 <- c("Alter_2","Geschlecht_2","Diagnosis_T2","residuals_nested","cumulative_risk_nested_oos","residuals_nested_T2","cumulative_risk_nested_oos_T2",
             "BDI_Sum_2","HAMD_Sum17_2","preds_nested_oos_T2","HAMD_Sum21_2","GAFscore_2",
             "Rem_Diagnose1_T2","Komorbid_T2","DurDep_T2","DepEp_T2","Hosp_T2","DurHosp_T2","RS25_Sum_2","TimeSinceTreat","medication_T2",
             "AgeOfOnset", "medication_T2", "Rem_Diagnose1_T2")

df_T2 <- ResBrain_All_T1_T2 %>% select(all_of(vars_T2))

# helper for numeric
summarise_num <- function(x) {
  sprintf("%.2f (%.2f)", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))
}

# helper for categorical
summarise_cat <- function(x) {
  tab <- table(x, useNA = "ifany")
  n_tot <- sum(tab)
  paste0(names(tab), ": ", tab, " (", round(100*tab/n_tot,1), "%)", collapse = "; ")
}

# build table
out_T2 <- map_dfr(names(df_T2), function(v) {
  x <- df_T2[[v]]
  if (is.numeric(x)) {
    tibble(Variable = v, Summary = summarise_num(x))
  } else {
    tibble(Variable = v, Summary = summarise_cat(as.factor(x)))
  }
})

print(out_T2, n = Inf, width = Inf)


# ----------------------------------------------------------------------------------------------------------------------------------------- # 


#### MDD sample analysis ####
vars_dep_T2 <- c("Rem_Diagnose1_T2","Komorbid_T2","DurDep_T2","DepEp_T2","Hosp_T2","DurHosp_T2","medication_T2","TimeSinceTreat","AgeOfOnset", "Rem_Diagnose1_T2")

ResBrain_MDD_T2 <- ResBrain_All_T1_T2 %>%
  filter(Diagnosis_T2 == "MDD")

df_dep_T2 <- ResBrain_MDD_T2 %>% select(all_of(vars_dep_T2))

# helper for numeric
summarise_num <- function(x) {
  sprintf("%.2f (%.2f)", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))
}

# helper for categorical
summarise_cat <- function(x) {
  tab <- table(x, useNA = "ifany")
  n_tot <- sum(tab)
  paste0(names(tab), ": ", tab, " (", round(100*tab/n_tot,1), "%)", collapse = "; ")
}

# build table
out_dep_T2 <- map_dfr(names(df_dep_T2), function(v) {
  x <- df_dep_T2[[v]]
  if (is.numeric(x)) {
    tibble(Variable = v, Summary = summarise_num(x))
  } else {
    tibble(Variable = v, Summary = summarise_cat(as.factor(x)))
  }
})

out_dep_T2


# ----------------------------------------------------------------------------------------------------------------------------------------- # 


#### Extreme Group Comparison ####
# Extreme Groups from T1 are called, not Extreme Groups from reassessed model at T2

# 1) Build one combined df with a clean 2-level group factor
df_extreme_groups_T2 <- ResBrain_All_T1_T2 %>%
  dplyr::filter(Outlier_Below_nested == TRUE | Outlier_Above_nested == TRUE) %>%
  dplyr::mutate(
    Mental_Health_nested = dplyr::case_when(
      Outlier_Below_nested ~ "Resilience",
      Outlier_Above_nested ~ "Vulnerable"
    ),
    Mental_Health_nested = factor(Mental_Health_nested, levels = c("Resilience","Vulnerable"))
  )

# 2) Helpers (same as baseline)
is_cat <- function(x) is.factor(x) || is.character(x) || is.logical(x)

fmt_mean_sd <- function(x) sprintf("%.2f (%.2f)", mean(x, na.rm=TRUE), stats::sd(x, na.rm=TRUE))
fmt_median_iqr <- function(x) {
  q <- stats::quantile(x, probs = c(.25,.5,.75), na.rm=TRUE, type=7)
  sprintf("%.2f [%.2f; %.2f]", q[2], q[1], q[3])
}

summ_cat <- function(x) {
  tab <- table(x, useNA="ifany"); n_tot <- sum(tab)
  paste0(names(tab), ": ", tab, " (", round(100*tab/n_tot,1), "%)", collapse="; ")
}

shapiro_ok <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 3) return(FALSE)
  if (length(x) > 5000) return(TRUE)
  stats::shapiro.test(x)$p.value > 0.05
}

# 3) Tests (formula + data)
test_numeric <- function(formula, data) {
  mf <- model.frame(formula, data = data)
  y <- mf[[1]]; g <- droplevels(factor(mf[[2]]))
  if (nlevels(g) != 2) return(list(p=NA_real_, method=NA_character_, parametric=NA))
  y1 <- y[g==levels(g)[1]]; y2 <- y[g==levels(g)[2]]
  y1 <- y1[!is.na(y1)]; y2 <- y2[!is.na(y2)]
  if (length(y1) < 3 || length(y2) < 3) return(list(p=NA_real_, method=NA_character_, parametric=NA))
  
  normal_both <- shapiro_ok(y1) && shapiro_ok(y2)
  if (normal_both) {
    var_eq <- tryCatch(stats::var.test(y1, y2)$p.value > 0.05, error=function(e) FALSE)
    tt <- stats::t.test(y1, y2, var.equal = var_eq)
    method <- if (var_eq) "Student t-test" else "Welch t-test"
    list(p = tt$p.value, method = method, parametric = TRUE)
  } else {
    wt <- stats::wilcox.test(y1, y2, exact = FALSE)
    list(p = wt$p.value, method = "Wilcoxon rank-sum test", parametric = FALSE)
  }
}

test_categorical <- function(formula, data) {
  mf <- model.frame(formula, data = data)
  x <- droplevels(factor(mf[[1]])); g <- droplevels(factor(mf[[2]]))
  tab <- table(group = g, value = x)
  chi <- tryCatch(stats::chisq.test(tab, correct = FALSE), error=function(e) NULL)
  if (is.null(chi) || any(chi$expected < 5)) {
    ft <- stats::fisher.test(tab)
    list(p = ft$p.value, method = "Fisher's exact test")
  } else {
    list(p = chi$p.value, method = "Pearson chi-squared test")
  }
}

# 4) Effect sizes 
es_cont <- function(formula, data) {
  d <- effectsize::cohens_d(formula, data = data, ci = 0.95, pooled_sd = TRUE)
  c(ES = as.numeric(d$Cohens_d),
    CI = sprintf("[%.2f, %.2f]", d$CI_low, d$CI_high))
}

es_cat_bin <- function(x, g) {
  g <- droplevels(factor(g)); x <- droplevels(factor(x))
  tab <- table(g, x)
  if (!all(dim(tab) == c(2L, 2L))) return(c(ES = NA_real_, CI = NA_character_))
  if (any(tab == 0)) tab <- tab + 0.5
  or <- effectsize::oddsratio(tab, ci = 0.95)
  c(ES = as.numeric(or$OR),
    CI = sprintf("[%.2f, %.2f]", or$CI_low, or$CI_high))
}

es_cat_multi <- function(x, g) {
  g <- droplevels(factor(g)); x <- droplevels(factor(x))
  tab <- table(g, x)
  if (nrow(tab) != 2L || ncol(tab) < 2L) return(c(ES=NA_real_, CI=NA_character_))
  v <- effectsize::cramers_v(tab, ci = 0.95)
  c(ES = as.numeric(v$Cramer_v),
    CI = sprintf("[%.2f, %.2f]", v$CI_low, v$CI_high))
}

# 5) Build the T2 comparison table
out_T2 <- purrr::map_dfr(vars_T2, function(v) {
  x <- df_extreme_groups_T2[[v]]
  g <- df_extreme_groups_T2$Mental_Health_nested
  
  if (is.numeric(x)) {
    fm <- stats::as.formula(paste0(v, " ~ Mental_Health_nested"))
    res <- test_numeric(fm, data = df_extreme_groups_T2)
    es  <- es_cont(fm, data = df_extreme_groups_T2)
    
    tibble::tibble(
      Variable   = v,
      Resilience = fmt_mean_sd(x[g=="Resilience"]),
      Vulnerable = fmt_mean_sd(x[g=="Vulnerable"]),
      Test       = res$method,
      p_value    = res$p,
      Effect     = "Cohen's d",
      ES         = round(as.numeric(es["ES"]), 2),
      CI95       = es["CI"]
    )
    
  } else {
    res  <- test_categorical(stats::as.formula(paste0(v, " ~ Mental_Health_nested")),
                             data = df_extreme_groups_T2)
    lvls <- nlevels(droplevels(factor(x)))
    if (lvls == 2) {
      es <- es_cat_bin(x, g); eff_label <- "Odds Ratio"
    } else {
      es <- es_cat_multi(x, g); eff_label <- "Cramér's V"
    }
    
    tibble::tibble(
      Variable   = v,
      Resilience = summ_cat(x[g=="Resilience"]),
      Vulnerable = summ_cat(x[g=="Vulnerable"]),
      Test       = res$method,
      p_value    = res$p,
      Effect     = eff_label,
      ES         = round(as.numeric(es["ES"]), 2),
      CI95       = es["CI"]
    )
  }
})

# 6) p-value formatting
out_T2 <- out_T2 %>%
  dplyr::mutate(
    p_value = dplyr::case_when(
      is.na(p_value)   ~ NA_character_,
      p_value < 0.001 ~ "<0.001",
      TRUE            ~ sprintf("%.3f", p_value)
    )
  )

print(out_T2, n = Inf, width = Inf)


## categorical variables adjustment / error debuggin ##
# CAVE includes HCs -> Rem, Komorbid, etc should be assessed in MDD subsample

tab_sex <- table(df_extreme_groups_T2$Mental_Health_nested, df_extreme_groups_T2$Geschlecht_2)
oddsratio(tab_sex, ci = 0.95)

tab_diagnosis <- table(df_extreme_groups_T2$Mental_Health_nested, df_extreme_groups_T2$Diagnosis_T2)
oddsratio(tab_diagnosis, ci = 0.95)

# ----------------------------------------------------------------------------------------------------------------------------------------- # 


#### MDD Group Comparison ####

# Variables (MDD-only at T2)
# keep your existing vars_dep_T2 vector
# vars_dep_T2 <- c(...)

# 1) Combined df restricted to MDD at T2
df_extreme_groups_MDD_T2 <- ResBrain_All_T1_T2 %>%
  dplyr::filter(Diagnosis_T2 == "MDD",
                Outlier_Below_nested == TRUE | Outlier_Above_nested == TRUE) %>%
  dplyr::mutate(
    Mental_Health_nested = dplyr::case_when(
      Outlier_Below_nested ~ "Resilience",
      Outlier_Above_nested ~ "Vulnerable"
    ),
    Mental_Health_nested = factor(Mental_Health_nested, levels = c("Resilience","Vulnerable"))
  )

# 2) Build the MDD T2 comparison table
out_dep_T2 <- purrr::map_dfr(vars_dep_T2, function(v) {
  x <- df_extreme_groups_MDD_T2[[v]]
  g <- df_extreme_groups_MDD_T2$Mental_Health_nested
  
  if (is.numeric(x)) {
    fm <- stats::as.formula(paste0(v, " ~ Mental_Health_nested"))
    res <- test_numeric(fm, data = df_extreme_groups_MDD_T2)
    es  <- es_cont(fm, data = df_extreme_groups_MDD_T2)
    
    tibble::tibble(
      Variable   = v,
      Resilience = fmt_mean_sd(x[g=="Resilience"]),
      Vulnerable = fmt_mean_sd(x[g=="Vulnerable"]),
      Test       = res$method,
      p_value    = res$p,
      Effect     = "Cohen's d",
      ES         = round(as.numeric(es["ES"]), 2),
      CI95       = es["CI"]
    )
    
  } else {
    res  <- test_categorical(stats::as.formula(paste0(v, " ~ Mental_Health_nested")),
                             data = df_extreme_groups_MDD_T2)
    lvls <- nlevels(droplevels(factor(x)))
    if (lvls == 2) {
      es <- es_cat_bin(x, g); eff_label <- "Odds Ratio"
    } else {
      es <- es_cat_multi(x, g); eff_label <- "Cramér's V"
    }
    
    tibble::tibble(
      Variable   = v,
      Resilience = summ_cat(x[g=="Resilience"]),
      Vulnerable = summ_cat(x[g=="Vulnerable"]),
      Test       = res$method,
      p_value    = res$p,
      Effect     = eff_label,
      ES         = round(as.numeric(es["ES"]), 2),
      CI95       = es["CI"]
    )
  }
})

# 3) p-value formatting
out_dep_T2 <- out_dep_T2 %>%
  dplyr::mutate(
    p_value = dplyr::case_when(
      is.na(p_value)   ~ NA_character_,
      p_value < 0.001 ~ "<0.001",
      TRUE            ~ sprintf("%.3f", p_value)
    )
  )


out_dep_T2

## MDD subset categorical variables adjustment / error debuggin ##

tab_komorbid <- table(df_extreme_groups_MDD_T2$Mental_Health_nested, df_extreme_groups_MDD_T2$Komorbid_T2)
oddsratio(tab_komorbid, ci = 0.95)

tab_medication <- table(df_extreme_groups_MDD_T2$Mental_Health_nested, df_extreme_groups_MDD_T2$medication_T2)
oddsratio(tab_medication, ci = 0.95)

tab_rem <- table(df_extreme_groups_MDD_T2$Mental_Health_nested, df_extreme_groups_MDD_T2$Rem_Diagnose1_T2)
tab_rem  # optional: inspect the table
# Cramér's V with 95% CI
v_rem <- cramers_v(tab_rem, ci = 0.95)
v_rem



# ----------------------------------------------------------------------------------------------------------------------------------------- # 

# ----------------------------------------------------------------------------------------------------------------------------------------- # 



#### Stability Analyses: Residual T1 ~ T2 ####

# Correlation Resilience Score T1 - T2
# check normal distribution of residuals
data_stability_check <- ResBrain_All_T1_T2

shapiro.test(data_stability_check$residuals_nested)  # Test for normality in T1 residuals
shapiro.test(data_stability_check$residuals_nested_T2)  # Test for normality in T2 residuals
#residuals are not normally distributed


# Plotting the distribution
data_stability_check_long <- data.frame(
  Residuals = c(data_stability_check$residuals_nested, data_stability_check$residuals_nested_T2),
  Timepoint = rep(c("T1", "T2"), each = length(data_stability_check$residuals_nested))
)

# Plot histogram with density overlay
residual_distribution <- ggplot(data_stability_check_long, aes(x = Residuals, fill = Timepoint)) +
  geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.5, position = "identity") +
  geom_density(alpha = 0.7) +
  labs(title = "Distribution of Residuals (T1 vs. T2)", x = "Residuals", y = "Density") +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "red"))


output_dir <- here("05_figures/001_defaultexp_figures")
output_file <- file.path(output_dir, "Residual Distribution T1 vs T2 env.jpeg")  # Change to .pdf if needed
ggsave(filename = output_file, plot = residual_distribution, width = 12, height = 6, dpi = 600)


# Check Correlation between residuals T1 ~ T2
correlation_residuals <- cor(data_stability_check$residuals_nested, data_stability_check$residuals_nested_T2, 
                             use = "complete.obs", method = "spearman") #spearman because of non-normally distribution

# Check for significance
cor_test_result <- cor.test(data_stability_check$residuals_nested, data_stability_check$residuals_nested_T2, 
                            method = "spearman", use = "complete.obs")
cor_test_result

# Create plot
correlation_residuals_plot <- ggplot(data_stability_check, aes(x = residuals_nested, y = residuals_nested_T2)) +
  
  # Add shaded region for the expected residual range
  geom_rect(aes(xmin = -threshold_T1, xmax = threshold_T1, ymin = -Inf, ymax = Inf), fill = "grey95", alpha = 0.1) +  # Vertical region
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -threshold_T2, ymax = threshold_T2), fill = "grey95", alpha = 0.1) +  # Horizontal region
  
  # Scatter points
  geom_point(alpha = 0.7) +
  
  # Regression line with confidence intervals
  geom_smooth(method = "lm", color = "red", fill = "blue", alpha = 0.2, se = TRUE) +
  
  # Threshold lines
  geom_hline(yintercept = c(-threshold_T2, threshold_T2), linetype = "dashed", color = "blue") +
  geom_vline(xintercept = c(-threshold_T1, threshold_T1), linetype = "dashed", color = "blue") +
  
  # Labels and theme
  labs(title = paste("Spearman Correlation:", "r =", round(cor_test_result$estimate, 2),  
                     "(p =", round(cor_test_result$p.value, 3), ")"),
       x = "Residuals T1", y = "Residuals T2") +
  theme_minimal()+
  theme(
    axis.title = element_text(size = 18),   # axis labels
    axis.text  = element_text(size = 14),   # axis numbers
    plot.title = element_text(size = 20, face = "bold") # title
  )

print(correlation_residuals_plot)

output_file <- file.path(output_dir, "residual_correlation.jpeg")  # Change to .pdf if needed
ggsave(filename = output_file, plot = correlation_residuals_plot, width = 12, height = 9, dpi = 600)


# Perform Bland-Altman Analysis
#data_stability_check$residuals <- as.numeric(data_stability_check$residuals[, 1])  # Convert to vector

#bland_altman_plot <- blandr.draw(data_stability_check$residuals_nested, data_stability_check$residuals_nested_T2,
#                                 ciShading = TRUE, ciDisplay = TRUE, plotTitle = "Bland-Altman Plot: Residual Stability")
#output_file <- file.path(output_dir, "Bland_Altman_Plot_residuals_nested_env.jpeg")  # Change to .pdf if needed
#ggsave(filename = output_file, plot = bland_altman_plot, width = 12, height = 6, dpi = 300)

#Interpretation: 
# Sligth mean shift below zero -> small systemic bias, residuals_nested T2 are little higher than at T1.
# see also blue line sligthly lower than 0 -> very small systemic difference
# Majority of points cluster around 0 → Indicates good agreement between T1 and T2 residuals_nested.
# Most points fall inside the agreement limits, meaning residuals_nested are relatively stable.


# Spaghetti Plot

# Convert dataset to long format
#data_long <- data_stability_check %>%
#  select(Proband, residuals_nested, residuals_nested_T2, Mental_Health) %>%  # Include identifier variable
#  pivot_longer(cols = c(residuals_nested, residuals_nested_T2), names_to = "Timepoint", values_to = "Residual")

# Convert timepoint to factor for correct ordering
#data_long$Timepoint <- factor(data_long$Timepoint, levels = c("residuals_nested", "residuals_nested_T2"), labels = c("T1", "T2"))

# Define custom colors for groups
#group_colors <- c("As_Expected" = "grey", "Vulnerable" = "red", "Resilience" = "darkgreen")

# Spaghetti plot with defined group colors
#spaghetti_plot <- ggplot(data_long, aes(x = Timepoint, y = Residual, group = Proband, color = Mental_Health)) +
#  geom_point(alpha = 0.6) + 
#  geom_line(alpha = 0.6) +
#  scale_color_manual(values = group_colors) +  # Apply custom colors
#  labs(title = "residuals_nested Trajectory (T1 to T2) by Group", x = "Timepoint", y = "residuals_nested", color = "Group") +
#  theme_minimal()

#output_file <- file.path(output_dir, "spaghetti_plot_env.jpeg")  # Change to .pdf if needed
#ggsave(filename = output_file, plot = spaghetti_plot, width = 12, height = 9, dpi = 300)



# ----------------------------------------------------------------------------------------------------------------------------------------- # 

# ----------------------------------------------------------------------------------------------------------------------------------------- # 



#### Descriptive Longitudinal Analyses ####

## Compute delta scores ##
ResBrain_All_T1_T2 <- ResBrain_All_T1_T2 %>%
  mutate(
    delta_resilience = residuals_nested_T2 - residuals_nested,
    delta_HAMD = Outcome_T2 - Outcome
  )

## Compute predictor delta scores ##
ResBrain_All_T1_T2 <- ResBrain_All_T1_T2 %>%
  mutate(
    delta_IQ = IQ_2 - IQ,
    delta_CTQ = CTQ_Sum_2 - CTQ_Sum,
    delta_LEQ_neg = LEQ_NegativeEventScore_2 - LEQ_NegativeEventScore,
    delta_LEQ_pos = LEQ_PositiveEventScore_2 - LEQ_PositiveEventScore,
    delta_PSS = PSS_Sum_2 - PSS_Sum,
    delta_FSozU = FSozU_Sum_2 - FSozU_Sum,
    delta_Bildung = Bildungsjahre_2 - Bildungsjahre,
    delta_Haushalt = Haushaltsnetto_2 - Haushaltsnetto,
     )

## Matrix 3: Clinical Course + Predictor Change x Resilience ##
ResBrain_corr_clinical_res <- ResBrain_All_T1_T2 %>%
  dplyr::select(
    residuals_nested, residuals_nested_T2, delta_resilience,
    cumulative_risk_nested_oos, cumulative_risk_nested_oos_T2,
    Outcome, Outcome_T2, delta_HAMD,
    DurDep,
    DurDep_T2, DurSymptomFree_T2, SOFAS_Sum_2,
    delta_IQ, delta_CTQ, delta_LEQ_neg, delta_LEQ_pos,
    delta_PSS, delta_FSozU, delta_Bildung, delta_Haushalt
  ) %>%
  as.data.frame()

names(ResBrain_corr_clinical_res) <- c(
  "Residuals_T1",
  "Residuals_T2",
  "Resilience change score (T2-T1)",
  "Cumulative Risk_T1",
  "Cumulative Risk_T2",
  "HAMD-17 sum score_T1",
  "HAMD-17 sum score_T2",
  "HAMD-17 change score (T2-T1)",
  "Duration of Depression_T1",
  "Duration of Depression_T2",
  "Duration symptom free_T2",
  "SOFAS_T2",
  "IQ change score (T2-T1)",
  "CTQ change score (T2-T1)",
  "LEQ negative change score (T2-T1)",
  "LEQ positive change score (T2-T1)",
  "PSS change score (T2-T1)",
  "FSozU change score (T2-T1)",
  "Education change score (T2-T1)",
  "Household income change score (T2-T1)"
)


cor_mat_clinical_res <- cor(
  ResBrain_corr_clinical_res,
  use = "pairwise.complete.obs",
  method = "spearman"
)

output_file <- file.path(output_dir, "correlation_matrix_clinical_resilience.jpeg")
jpeg(output_file, width = 4000, height = 4000, res = 300)
corrplot(
  cor_mat_clinical_res,  # kein $correlations
  method = "color",
  type = "upper",
  tl.cex = 0.95,
  tl.col = "black",
  tl.srt = 55,
  number.cex = 0.75,
  addCoef.col = "black",
  family = "Arial",
  mar = c(0, 0, 0, 2)
)
dev.off()




# ----------------------------------------------------------------------------------------------------------------------------------------- # 

# ----------------------------------------------------------------------------------------------------------------------------------------- # 





# helper
summarise_continuous <- function(data, var) {
  if (!var %in% names(data)) return(NA_character_)
  data %>%
    summarise(
      mean = round(mean(.data[[var]], na.rm = TRUE), 2),
      sd   = round(sd(.data[[var]],   na.rm = TRUE), 2)
    ) %>%
    mutate(label = paste0(mean, " (", sd, ")")) %>%
    pull(label)
}

summarise_factor <- function(data, var) {
  if (!var %in% names(data)) return(NA_character_)
  tbl <- table(data[[var]], useNA = "no")
  n_total <- sum(tbl)
  paste(
    paste0(names(tbl), ": ", tbl, " (", round(100 * tbl / n_total, 1), "%)"),
    collapse = "; "
  )
}

change_score <- function(data, t1, t2) {
  if (!t1 %in% names(data) || !t2 %in% names(data)) return(NA_character_)
  data %>%
    mutate(diff = .data[[t2]] - .data[[t1]]) %>%
    summarise(
      mean = round(mean(diff, na.rm = TRUE), 2),
      sd   = round(sd(diff,   na.rm = TRUE), 2)
    ) %>%
    mutate(label = paste0(mean, " (", sd, ")")) %>%
    pull(label)
}

change_factor_binary <- function(data, t1, t2) {
  if (!t1 %in% names(data) || !t2 %in% names(data)) return(NA_character_)
  data %>%
    filter(!is.na(.data[[t1]]), !is.na(.data[[t2]])) %>%
    mutate(
      t1_chr  = as.character(.data[[t1]]),
      t2_chr  = as.character(.data[[t2]]),
      changed = t1_chr != t2_chr
    ) %>%
    summarise(
      n_changed = sum(changed),
      n_0to1    = sum(t1_chr == "0" & t2_chr == "1"),
      n_1to0    = sum(t1_chr == "1" & t2_chr == "0"),
      pct       = round(100 * mean(changed), 1)
    ) %>%
    mutate(label = paste0(
      n_changed, " changed (", pct, "%): ",
      n_0to1, " (0→1), ", n_1to0, " (1→0)"
    )) %>%
    pull(label)
}

# list of variables
vars <- list(
  list(label = "Age",                       type = "continuous",    t1 = "Alter",                     t1_rb = "Alter",                   t2 = "Alter_2"),
  list(label = "IQ",                        type = "continuous",    t1 = "IQ",                        t1_rb = "IQ",                      t2 = "IQ_2"),
  list(label = "NEO-FFI neuroticism",       type = "continuous",    t1 = "NEOFFI_Neurotizismus",      t1_rb = "NEOFFI_Neurotizismus",    t2 = NULL),
  list(label = "NEO-FFI extraversion",      type = "continuous",    t1 = "NEOFFI_Extraversion",       t1_rb = "NEOFFI_Extraversion",     t2 = NULL),
  list(label = "NEO-FFI openness",          type = "continuous",    t1 = "NEOFFI_Offenheit",          t1_rb = "NEOFFI_Offenheit",        t2 = NULL),
  list(label = "NEO-FFI agreeableness",     type = "continuous",    t1 = "NEOFFI_Vertraeglichkeit",   t1_rb = "NEOFFI_Vertraeglichkeit", t2 = NULL),
  list(label = "NEO-FFI conscientiousness", type = "continuous",    t1 = "NEOFFI_Gewissenhaftigkeit", t1_rb = "NEOFFI_Gewissenhaftigkeit", t2 = NULL),
  list(label = "CTQ sum score",             type = "continuous",    t1 = "CTQ_Sum",                   t1_rb = "CTQ_Sum",                 t2 = "CTQ_Sum_2"),
  list(label = "ACE sum score",             type = "continuous",    t1 = "ACE_Sum",                   t1_rb = "ACE_Sum",                 t2 = NULL),
  list(label = "LEQ negative sum score",    type = "continuous",    t1 = "LEQ_NegativeEventScore",    t1_rb = "LEQ_NegativeEventScore",  t2 = "LEQ_NegativeEventScore_2"),
  list(label = "LEQ positive sum score",    type = "continuous",    t1 = "LEQ_PositiveEventScore",    t1_rb = "LEQ_PositiveEventScore",  t2 = "LEQ_PositiveEventScore_2"),
  list(label = "PSS sum score",             type = "continuous",    t1 = "PSS_Sum",                   t1_rb = "PSS_Sum",                 t2 = "PSS_Sum_2"),
  list(label = "FSozU sum score",           type = "continuous",    t1 = "FSozU_Sum",                 t1_rb = "FSozU_Sum",               t2 = "FSozU_Sum_2"),
  list(label = "Education",                 type = "continuous",    t1 = "Bildungsjahre",             t1_rb = "Bildungsjahre",           t2 = "Bildungsjahre_2"),
  list(label = "Household income",          type = "continuous",    t1 = "Haushaltsnetto",            t1_rb = "Haushaltsnetto",          t2 = "Haushaltsnetto_2"),
  list(label = "RSQ fear of closeness",     type = "continuous",    t1 = "RSQ_AngstVorNaehe",         t1_rb = "RSQ_AngstVorNaehe",       t2 = NULL),
  list(label = "RSQ fear of abandonment",   type = "continuous",    t1 = "RSQ_AngstVorTrennung",      t1_rb = "RSQ_AngstVorTrennung",    t2 = NULL),
  list(label = "HAMD-17 sum score",         type = "continuous",    t1 = "HAMD_Sum17",                t1_rb = "HAMD_Sum17",              t2 = "HAMD_Sum17_2"),
  list(label = "Sex",                       type = "factor_binary", t1 = "Geschlecht",                t1_rb = "Geschlecht",              t2 = "Geschlecht_2"),
  list(label = "Familial risk AD",          type = "factor_binary", t1 = "GenRisiko_Affektiv1",       t1_rb = "GenRisiko_Affektiv1",     t2 = "Gesichertes_Risiko_Affektiv_2"),
  list(label = "Familial risk PD",          type = "factor_binary", t1 = "GenRisiko_Psycho1",         t1_rb = "GenRisiko_Psycho1",       t2 = "Gesichertes_Risiko_Psychotisch_2"),
  list(label = "Immigration",               type = "factor_binary", t1 = "Immigration",               t1_rb = "Immigration",             t2 = NULL)
)


table_data <- map_dfr(vars, function(v) {
  if (v$type == "continuous") {
    rb_val <- summarise_continuous(ResBrain, v$t1_rb)
    t1_val <- summarise_continuous(ResBrain_All_T1_T2, v$t1)
    t2_val <- if (!is.null(v$t2)) summarise_continuous(ResBrain_All_T1_T2, v$t2) else NA_character_
    ch_val <- if (!is.null(v$t2)) change_score(ResBrain_All_T1_T2, v$t1, v$t2) else NA_character_
    
  } else if (v$type == "factor_binary") {
    rb_val <- summarise_factor(ResBrain, v$t1_rb)
    t1_val <- summarise_factor(ResBrain_All_T1_T2, v$t1)
    t2_val <- if (!is.null(v$t2)) summarise_factor(ResBrain_All_T1_T2, v$t2) else NA_character_
    ch_val <- if (!is.null(v$t2)) change_factor_binary(ResBrain_All_T1_T2, v$t1, v$t2) else NA_character_
  }
  
  tibble(
    Variable                    = v$label,
    Type                        = v$type,
    T1_ResBrain_N1804           = rb_val,
    T1_ResBrain_All             = t1_val,
    T2_ResBrain_All             = t2_val,
    Change_T2_T1                = ch_val
  )
})

# SozDemo5: 
sozdemo_labels <- c(
  "SozDemo: daily",
  "SozDemo: once/week",
  "SozDemo: once/2 weeks",
  "SozDemo: once/month",
  "SozDemo: none except work",
  "SozDemo: none at all"
)

sozdemo_rows <- map_dfr(1:6, function(lvl) {
  
  # ResBrain N=1804
  rb_n   <- sum(ResBrain$SozDemo5 == lvl, na.rm = TRUE)
  rb_tot <- sum(!is.na(ResBrain$SozDemo5))
  rb_val <- paste0(rb_n, " (", round(100 * rb_n / rb_tot, 1), "%)")
  
  # T1 ResBrain_All
  t1_n   <- sum(ResBrain_All_T1_T2$SozDemo5 == lvl, na.rm = TRUE)
  t1_tot <- sum(!is.na(ResBrain_All_T1_T2$SozDemo5))
  t1_val <- paste0(t1_n, " (", round(100 * t1_n / t1_tot, 1), "%)")
  
  # T2 ResBrain_All
  if ("SozDemo5_2" %in% names(ResBrain_All_T1_T2)) {
    t2_n   <- sum(ResBrain_All_T1_T2$SozDemo5_2 == lvl, na.rm = TRUE)
    t2_tot <- sum(!is.na(ResBrain_All_T1_T2$SozDemo5_2))
    t2_val <- paste0(t2_n, " (", round(100 * t2_n / t2_tot, 1), "%)")
  } else {
    t2_val <- NA_character_
  }
  
  # Median change 
  if (lvl == 1) {
    ch_val <- ResBrain_All_T1_T2 %>%
      filter(!is.na(SozDemo5), !is.na(SozDemo5_2)) %>%
      mutate(
        t1_num = as.numeric(as.character(SozDemo5)),
        t2_num = as.numeric(as.character(SozDemo5_2)),
        diff   = t2_num - t1_num
      ) %>%
      summarise(
        median_change = round(median(diff, na.rm = TRUE), 2),
        n_changed     = sum(diff != 0, na.rm = TRUE),
        pct           = round(100 * mean(diff != 0, na.rm = TRUE), 1)
      ) %>%
      mutate(label = paste0(
        "Overall median change: ", median_change,
        "; ", n_changed, " changed (", pct, "%)"
      )) %>%
      pull(label)
  } else {
    ch_val <- NA_character_
  }
  
  tibble(
    Variable              = if (lvl == 1) paste0("SozDemo: overall median change (T2-T1) | ", sozdemo_labels[lvl]) else sozdemo_labels[lvl],
    Type                  = "factor_ordinal_level",
    T1_ResBrain_N1804     = rb_val,
    T1_ResBrain_All       = t1_val,
    T2_ResBrain_All       = t2_val,
    Change_T2_T1          = ch_val
  )
})

table_data <- bind_rows(table_data, sozdemo_rows)

print(table_data, n = 30)

# Export
write_xlsx(table_data, here("03_data/999_processed_data", "Predictor_Overview_T1_T2.xlsx"))


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


