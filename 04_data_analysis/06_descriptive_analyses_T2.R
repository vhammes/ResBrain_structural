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


vars_T2 <- c("Alter_2","Geschlecht_2","Diagnosis_T2","residuals","cumulative_risk","residuals_T2","cumulative_risk_T2",
             "BDI_Sum_2","HAMD_Sum17_2","Predicted_T2","HAMD_Sum21_2","GAFscore_2",
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

out_T2


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
  dplyr::filter(Outlier_Below == TRUE | Outlier_Above == TRUE) %>%
  dplyr::mutate(
    Mental_Health = dplyr::case_when(
      Outlier_Below ~ "Resilience",
      Outlier_Above ~ "Vulnerable"
    ),
    Mental_Health = factor(Mental_Health, levels = c("Resilience","Vulnerable"))
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
  g <- df_extreme_groups_T2$Mental_Health
  
  if (is.numeric(x)) {
    fm <- stats::as.formula(paste0(v, " ~ Mental_Health"))
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
    res  <- test_categorical(stats::as.formula(paste0(v, " ~ Mental_Health")),
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

out_T2

## categorical variables adjustment / error debuggin ##
# CAVE includes HCs -> Rem, Komorbid, etc should be assessed in MDD subsample

tab_sex <- table(df_extreme_groups_T2$Mental_Health, df_extreme_groups_T2$Geschlecht_2)
oddsratio(tab_sex, ci = 0.95)

tab_diagnosis <- table(df_extreme_groups_T2$Mental_Health, df_extreme_groups_T2$Diagnosis_T2)
oddsratio(tab_diagnosis, ci = 0.95)



#### MDD Group Comparison ####

# Variables (MDD-only at T2)
# keep your existing vars_dep_T2 vector
# vars_dep_T2 <- c(...)

# 1) Combined df restricted to MDD at T2
df_extreme_groups_MDD_T2 <- ResBrain_All_T1_T2 %>%
  dplyr::filter(Diagnosis_T2 == "MDD",
                Outlier_Below == TRUE | Outlier_Above == TRUE) %>%
  dplyr::mutate(
    Mental_Health = dplyr::case_when(
      Outlier_Below ~ "Resilience",
      Outlier_Above ~ "Vulnerable"
    ),
    Mental_Health = factor(Mental_Health, levels = c("Resilience","Vulnerable"))
  )

# 2) Build the MDD T2 comparison table
out_dep_T2 <- purrr::map_dfr(vars_dep_T2, function(v) {
  x <- df_extreme_groups_MDD_T2[[v]]
  g <- df_extreme_groups_MDD_T2$Mental_Health
  
  if (is.numeric(x)) {
    fm <- stats::as.formula(paste0(v, " ~ Mental_Health"))
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
    res  <- test_categorical(stats::as.formula(paste0(v, " ~ Mental_Health")),
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

tab_komorbid <- table(df_extreme_groups_MDD_T2$Mental_Health, df_extreme_groups_MDD_T2$Komorbid_T2)
oddsratio(tab_komorbid, ci = 0.95)

tab_medication <- table(df_extreme_groups_MDD_T2$Mental_Health, df_extreme_groups_MDD_T2$medication_T2)
oddsratio(tab_medication, ci = 0.95)

tab_rem <- table(df_extreme_groups_MDD_T2$Mental_Health, df_extreme_groups_MDD_T2$Rem_Diagnose1_T2)
tab_rem  # optional: inspect the table
# Cramér's V with 95% CI
v_rem <- cramers_v(tab_rem, ci = 0.95)
v_rem



# ----------------------------------------------------------------------------------------------------------------------------------------- # 



#### Environment Cleaning ####

# Cleanup: Remove temporary variables and objects
rm(list = c("anova_result", "DemoData_vars_T2", "identifier_var", 
            "MDD_DemoData_vars_T2", "variables_of_interest_continuous_T2",
            "variables_of_interest_dichotomous_T2", "variables_of_interest_polytomous_T2",
            "variable", "compute_posthoc_dichotomous_T2", "compute_posthoc_polytomous_T2",
            "perform_anova_tukey_T2", "data", "DemoData_MDD_ResBrain_T2", 
            "DemoData_ResBrain_T2", "MDD_subset_T2", "ResBrain_des_T2",
            "results_numeric_T2", "threshold_T2", "preds_lm_T2",
            "residuals_T2" , "coefficients_T2", "correlation_matrix_T2_residuals", 
            "outlier_info_above_T2", "outlier_info_below_T2", "plot_data_T2", 
            "ResBrain_extreme_groups_T2", "ResBrain_MLR_T2", "ResBrain_MRI_T2", "spearman_result", 
            "summary_fit_lm_2", "X_T2", "best_coef_T2", "cumulative_risk_T2", 
            "dev_ratio_at_lambda_1se_T2", "frml_2", "mse_lambda_min_T2",  "new_order",
            "ordinal_vars", "Outlier_Below_T2", "Outlier_Above_T2", "target_var", 
            "df_dep_T2", "df_res_dep_T2", "df_res_T2",
            "df_vul_dep_T2", "df_vul_T2", "df_T2", "out_dep_T2", "out_T2", 
            "ResBrain_MDD_T2", "ResBrain_prepro_T2_residuals", "spearman_results", "test",
            "common_cols", "i", "outlier_subjects_above_T2", "outlier_subjects_below_T2", 
            "var", "vars_dep_T2", "vars_T2", "is_cat", "p_cat", "p_num", "summ_cat", "summ_num", 
            "summarise_cat", "summarise_num", "coefficients_at_lambda_1se_T2", "coefficients_df_T2", 
            "coefficients_matrix_T2", "df_extreme_groups_MDD", "df_extreme_groups_MDD_T2", 
            "fit_lm_2", "preds_rlm_T2", "df_extreme_groups_T2", "v_rem", "adjusted_coefficients_T2", 
            "coeff_vector_T2", "color_mapping", "lambda_1se_index_T2", "lambda_specific_T2", "numeric_vars_T2", 
            "output_file", "p_T2", "std_devs_T2", "tab_diagnosis", "tab_komorbid", "tab_medication", "tab_rem", 
            "tab_sex", "vars_2", "es_cat_bin", "es_cat_multi", "es_cont", "fmt_mean_sd", "fmt_median_iqr", 
            "shapiro_ok", "test_categorical", "test_numeric"   
            ))

# Verify cleanup
print(ls())  # List remaining objects in the environment

