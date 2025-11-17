#### Preamble ####

# Run 00a_metadata_and_packages.R first to dependencies

# Define required packages
required_packages_03 <- c("haven", "dplyr", "missForest", "here", "readxl", "tidyverse", "polycor", "corrplot", "writexl", "effectsize")

# Check and load packages
for (pkg in required_packages_03) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(paste("Package", pkg, "is required but not installed."))
  }
  library(pkg, character.only = TRUE) # Load the package into the environment
}

# Clean up temporary variables
rm(list = c("required_packages_03", "pkg"))

# ----------------------------------------------------------------------------------------------------------------------------------------- # 


#### Check Variables ####

# Check Normality of Outcome Variables
ResBrain$Rem <- as.factor(ResBrain$Rem)
ResBrain$Komorbid <- as.factor(ResBrain$Komorbid)

# variable list
vars <- c(
  "Alter","Geschlecht","Diagnosis","residuals","cumulative_risk",
  "BDI_Sum","HAMD_Sum17","Predicted","HAMD_Sum21","GAFscore",
  "Rem","Komorbid","DurDep","DepEp","Hosp","DurHosp",
  "RS25_Sum","TimeSinceTreat","medication","AgeOfOnset"
)

is_cont <- function(x) is.numeric(x) || is.integer(x)

plot_list <- map(vars, function(v) {
  x <- ResBrain[[v]]
  
  if (is_cont(x)) {
    mu    <- mean(x, na.rm = TRUE)
    sigma <- sd(x,   na.rm = TRUE)
    
    p1 <- ggplot(ResBrain, aes(x = .data[[v]])) +
      geom_histogram(aes(y = ..density..), bins = 30,
                     fill = "skyblue", color = "black") +
      geom_density(color = "red", size = 1) +
      stat_function(
        fun = dnorm,
        args = list(mean = mu, sd = sigma),
        color = "darkgreen", linetype = "dashed", size = 1
      ) +
      labs(title = paste("Histogram & Density of", v))
    
    p2 <- ggplot(ResBrain, aes(sample = .data[[v]])) +
      stat_qq(color = "blue") +
      stat_qq_line(color = "red") +
      labs(title = paste("Q-Q Plot of", v))
    
    p1 + p2
  } else {
    ggplot(ResBrain, aes(x = .data[[v]])) +
      geom_bar(fill = "steelblue") +
      labs(title = paste("Distribution of", v))
  }
}) |> set_names(vars)

## print f plots
vars_to_show <- vars

walk(plot_list[vars_to_show], print)




# Repeat for predictor variables
predictors <- c("GenRisiko_Affektiv1","GenRisiko_Psycho1","IQ","NEOFFI_Extraversion",
                "NEOFFI_Neurotizismus", "NEOFFI_Offenheit", "NEOFFI_Vertraeglichkeit","NEOFFI_Gewissenhaftigkeit","CTQ_Sum","RSQ_Secure",
                "ACE_Sum","LEQ_PositiveEventScore","LEQ_NegativeEventScore","PSS_Sum","Immigration","FSozU_Sum","SozDemo5","Bildungsjahre","Haushaltsnetto"
)

# function to check if variable is numeric
is_cont <- function(x) is.numeric(x) || is.integer(x)

plot_list <- map(predictors, function(v) {
  x <- ResBrain[[v]]
  
  if (is_cont(x)) {
    mu    <- mean(x, na.rm = TRUE)
    sigma <- sd(x,   na.rm = TRUE)
    
    p1 <- ggplot(ResBrain, aes(x = .data[[v]])) +
      geom_histogram(aes(y = ..density..), bins = 30,
                     fill = "skyblue", color = "black") +
      geom_density(color = "red", size = 1) +
      stat_function(
        fun = dnorm,
        args = list(mean = mu, sd = sigma),
        color = "darkgreen", linetype = "dashed", size = 1
      ) +
      labs(title = paste("Histogram & Density of", v))
    
    p2 <- ggplot(ResBrain, aes(sample = .data[[v]])) +
      stat_qq(color = "blue") +
      stat_qq_line(color = "red") +
      labs(title = paste("Q-Q Plot of", v))
    
    p1 + p2
  } else {
    ggplot(ResBrain, aes(x = .data[[v]])) +
      geom_bar(fill = "steelblue") +
      labs(title = paste("Distribution of", v))
  }
}) |> set_names(predictors)

## print f plots
preds_to_show <- predictors

walk(plot_list[preds_to_show], print)

# ----------------------------------------------------------------------------------------------------------------------------------------- # 


#### Descriptive Analyses ####

##### Full Sample Analyses #####
# Create medication variable
ResBrain <- ResBrain %>%
  mutate(
    medication = case_when(
      is.na(Sum_MED) ~ NA_character_,      # keep missing if Sum_MED missing
      Sum_MED > 0    ~ "Yes",
      TRUE           ~ "No"
    ) %>% factor(levels = c("No","Yes"))
  )

# adjust variables to factors
ResBrain <- ResBrain %>%
  mutate(across(c(Geschlecht, Diagnosis, Rem, Komorbid, medication), as.factor))


vars <- c("Alter","Geschlecht","Diagnosis","residuals","cumulative_risk",
          "BDI_Sum","HAMD_Sum17","Predicted","HAMD_Sum21","GAFscore",
          "Rem","Komorbid","DurDep","DepEp","Hosp","DurHosp","RS25_Sum","TimeSinceTreat","medication",
          "AgeOfOnset")

df <- ResBrain %>% select(all_of(vars))

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
out <- map_dfr(names(df), function(v) {
  x <- df[[v]]
  if (is.numeric(x)) {
    tibble(Variable = v, Summary = summarise_num(x))
  } else {
    tibble(Variable = v, Summary = summarise_cat(as.factor(x)))
  }
})

#summary full sample mean values of outcome variables of interest
out


##### MDD sample analyses #####
# some clinical outcome variables are of interest only in the MDD subsample

vars_dep <- c("Rem","Komorbid","DurDep","DepEp","Hosp","DurHosp","medication","TimeSinceTreat","AgeOfOnset")

ResBrain_MDD <- ResBrain %>%
  filter(Diagnosis == "MDD")

df_dep <- ResBrain_MDD %>% select(all_of(vars_dep))

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
out_dep <- map_dfr(names(df_dep), function(v) {
  x <- df_dep[[v]]
  if (is.numeric(x)) {
    tibble(Variable = v, Summary = summarise_num(x))
  } else {
    tibble(Variable = v, Summary = summarise_cat(as.factor(x)))
  }
})

#summary MDD subsample mean values of MDD specific variables of interest
out_dep


# ----------------------------------------------------------------------------------------------------------------------------------------- # 



##### Extreme Group Comparisons #####
## Build one combined df with a clean 2-level group factor ##
df_extreme_groups <- ResBrain %>%
  dplyr::filter(Outlier_Below == TRUE | Outlier_Above == TRUE) %>%
  dplyr::mutate(
    Mental_Health = dplyr::case_when(
      Outlier_Below ~ "Resilience",
      Outlier_Above ~ "Vulnerable"
    ),
    Mental_Health = factor(Mental_Health, levels = c("Resilience","Vulnerable"))
  )

## Helpers ##
is_cat <- function(x) is.factor(x) || is.character(x) || is.logical(x)

fmt_mean_sd <- function(x) sprintf("%.2f (%.2f)", mean(x, na.rm=TRUE), stats::sd(x, na.rm=TRUE))
fmt_median_iqr <- function(x) {
  q <- stats::quantile(x, probs = c(.25,.5,.75), na.rm=TRUE, type=7)
  sprintf("%.2f [%.2f; %.2f]", q[2], q[1], q[3])
}

summ_cat <- function(x) {
  tab <- table(x, useNA="ifany")
  n_tot <- sum(tab)
  paste0(names(tab), ": ", tab, " (", round(100*tab/n_tot,1), "%)", collapse="; ")
}

# Normality helper (same logic as before)
shapiro_ok <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 3) return(FALSE)
  if (length(x) > 5000) return(TRUE)
  stats::shapiro.test(x)$p.value > 0.05
}

## Tests rewritten to accept formula + data ##
# Numeric: t-test (Student/Welch) if normal in both groups; else Wilcoxon
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

# Categorical: Fisher if any expected cell <5; else Pearson chi-square
test_categorical <- function(formula, data) {
  mf <- model.frame(formula, data = data)
  x <- droplevels(factor(mf[[1]]))
  g <- droplevels(factor(mf[[2]]))
  tab <- table(group = g, value = x)
  chi <- tryCatch(stats::chisq.test(tab, correct = FALSE), error=function(e) NULL)
  if (is.null(chi) || any(chi$expected < 5)) {
    ft <- stats::fisher.test(tab)
    list(p = ft$p.value, method = "Fisher's exact test")
  } else {
    list(p = chi$p.value, method = "Pearson chi-squared test")
  }
}

## Effect size helpers (now using formula/data or (x,g) vectors) ##
# Continuous: Cohen's d with 95% CI
es_cont <- function(formula, data) {
  d <- effectsize::cohens_d(formula, data = data, ci = 0.95, pooled_sd = TRUE)
  c(ES = as.numeric(d$Cohens_d),
    CI = sprintf("[%.2f, %.2f]", d$CI_low, d$CI_high))
}


# Binary categorical: Odds Ratio with 95% CI (+0.5 if zero cells)
# Odds ratio helper
es_cat_bin <- function(x, g) {
  g <- droplevels(factor(g))
  x <- droplevels(factor(x))
  tab <- table(g, x)
  if (!all(dim(tab) == c(2L, 2L))) return(c(ES = NA_real_, CI = NA_character_))
  if (any(tab == 0)) tab <- tab + 0.5
  or <- effectsize::oddsratio(tab, ci = 0.95)
  c(ES = as.numeric(or$OR), 
    CI = sprintf("[%.2f, %.2f]", or$CI_low, or$CI_high))
}


# Multinomial categorical: Cramér's V with 95% CI
es_cat_multi <- function(x, g) {
  x <- droplevels(factor(x)); g <- droplevels(factor(g))
  tab <- table(g, x)
  if (nrow(tab) != 2L || ncol(tab) < 2L) return(c(ES=NA_real_, CI=NA_character_))
  v <- effectsize::cramers_v(tab, ci = 0.95)
  c(ES = as.numeric(v$Cramer_v),
    CI = sprintf("[%.2f, %.2f]", v$CI_low, v$CI_high))
}

## Build comparison table from the combined df ##
out <- purrr::map_dfr(vars, function(v) {
  x <- df_extreme_groups[[v]]
  g <- df_extreme_groups$Mental_Health
  
  if (is.numeric(x)) {
    # test
    res <- test_numeric(stats::as.formula(paste0(v, " ~ Mental_Health")), data = df_extreme_groups)
    # effect size
    es  <- es_cont(stats::as.formula(paste0(v, " ~ Mental_Health")), data = df_extreme_groups)
    
    tibble::tibble(
      Variable   = v,
      Resilience = fmt_mean_sd(x[g=="Resilience"]),
      Vulnerable = fmt_mean_sd(x[g=="Vulnerable"]),
      Test       = res$method,
      p_value    = res$p,
      Effect = "Cohen's d",
      ES         = round(as.numeric(es["ES"]), 2),
      CI95       = es["CI"]
    )
    
  } else {
    # test (categorical)
    res <- test_categorical(stats::as.formula(paste0(v, " ~ Mental_Health")), data = df_extreme_groups)
    
    # effect size decision (2x2 → OR; else → Cramér's V)
    lvls <- nlevels(droplevels(factor(x)))
    if (lvls == 2) {
      es <- es_cat_bin(x, g)
      eff_label <- "Odds Ratio"
    } else {
      es <- es_cat_multi(x, g)
      eff_label <- "Cramér's V"
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

# p-value formatting
out <- out %>%
  dplyr::mutate(
    p_value = dplyr::case_when(
      is.na(p_value)   ~ NA_character_,
      p_value < 0.001 ~ "<0.001",
      TRUE            ~ sprintf("%.3f", p_value)
    )
  )


# Extreme group comparisons of variables of interest 
out



## categorical variables adjustment / error debuggin ##
# OR and Cramers v output is bugged in results table, is specifically printed here

tab_sex <- table(df_extreme_groups$Mental_Health, df_extreme_groups$Geschlecht)
oddsratio(tab_sex, ci = 0.95)

tab_diagnosis <- table(df_extreme_groups$Mental_Health, df_extreme_groups$Diagnosis)
oddsratio(tab_diagnosis, ci = 0.95)

tab_komorbid <- table(df_extreme_groups$Mental_Health, df_extreme_groups$Komorbid)
oddsratio(tab_komorbid, ci = 0.95)

tab_medication <- table(df_extreme_groups$Mental_Health, df_extreme_groups$medication)
oddsratio(tab_medication, ci = 0.95)

tab_rem <- table(df_extreme_groups$Mental_Health, df_extreme_groups$Rem)
tab_rem  # optional: inspect the table
# Cramér's V with 95% CI
v_rem <- cramers_v(tab_rem, ci = 0.95)
v_rem



# ----------------------------------------------------------------------------------------------------------------------------------------- # 

##### MDD Group Comparisons ####

# Variables (MDD-only)
vars_dep <- c("Rem","Komorbid","DurDep","DepEp","Hosp","DurHosp",
              "medication","TimeSinceTreat","AgeOfOnset")

# Build one combined df with clean 2-level group factor, restricted to MDD
df_extreme_groups_MDD <- ResBrain %>%
  dplyr::filter(Diagnosis == "MDD",
                Outlier_Below == TRUE | Outlier_Above == TRUE) %>%
  dplyr::mutate(
    Mental_Health = dplyr::case_when(
      Outlier_Below ~ "Resilience",
      Outlier_Above ~ "Vulnerable"
    ),
    Mental_Health = factor(Mental_Health, levels = c("Resilience","Vulnerable"))
  )

# Helpers (same style as good code)
is_cat <- function(x) is.factor(x) || is.character(x) || is.logical(x)

fmt_mean_sd <- function(x) sprintf("%.2f (%.2f)", mean(x, na.rm=TRUE), stats::sd(x, na.rm=TRUE))
fmt_median_iqr <- function(x) {
  q <- stats::quantile(x, probs = c(.25,.5,.75), na.rm=TRUE, type=7)
  sprintf("%.2f [%.2f; %.2f]", q[2], q[1], q[3])
}

summ_cat <- function(x) {
  tab <- table(x, useNA="ifany")
  n_tot <- sum(tab)
  paste0(names(tab), ": ", tab, " (", round(100*tab/n_tot,1), "%)", collapse="; ")
}

shapiro_ok <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 3) return(FALSE)
  if (length(x) > 5000) return(TRUE)
  stats::shapiro.test(x)$p.value > 0.05
}

# Tests (formula + data)
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
  x <- droplevels(factor(mf[[1]]))
  g <- droplevels(factor(mf[[2]]))
  tab <- table(group = g, value = x)
  chi <- tryCatch(stats::chisq.test(tab, correct = FALSE), error=function(e) NULL)
  if (is.null(chi) || any(chi$expected < 5)) {
    ft <- stats::fisher.test(tab)
    list(p = ft$p.value, method = "Fisher's exact test")
  } else {
    list(p = chi$p.value, method = "Pearson chi-squared test")
  }
}

# Effect sizes 
# Continuous: Cohen's d with 95% CI
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

# Build the MDD comparison table
out_dep <- purrr::map_dfr(vars_dep, function(v) {
  x <- df_extreme_groups_MDD[[v]]
  g <- df_extreme_groups_MDD$Mental_Health
  
  if (is.numeric(x)) {
    # test + ES via formula
    fm <- stats::as.formula(paste0(v, " ~ Mental_Health"))
    res <- test_numeric(fm, data = df_extreme_groups_MDD)
    es  <- es_cont(fm, data = df_extreme_groups_MDD)
    
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
    # categorical test + ES (OR for 2x2, V otherwise)
    res  <- test_categorical(stats::as.formula(paste0(v, " ~ Mental_Health")),
                             data = df_extreme_groups_MDD)
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

# p-value formatting
out_dep <- out_dep %>%
  dplyr::mutate(
    p_value = dplyr::case_when(
      is.na(p_value)   ~ NA_character_,
      p_value < 0.001 ~ "<0.001",
      TRUE            ~ sprintf("%.3f", p_value)
    )
  )

# Extreme group comparisons of MDD-specific variables of interest 
out_dep


# ----------------------------------------------------------------------------------------------------------------------------------------- # 


#### Explorative Residual ~ Predictor Regression ####
ResBrain$residuals <- as.vector(ResBrain$residuals)

# Define the target variable
target_var <- "residuals"

# Define the predictor variables
predictors <- c(
  "Alter", "cumulative_risk", 
  "CTQ_Sum", "ACE_Sum", "PSS_Sum", "LEQ_NegativeEventScore", "LEQ_PositiveEventScore", 
  "NEOFFI_Extraversion", "NEOFFI_Neurotizismus", "NEOFFI_Vertraeglichkeit", "NEOFFI_Offenheit", "NEOFFI_Gewissenhaftigkeit",
  "FSozU_Sum", "Bildungsjahre", "Haushaltsnetto", "IQ", "BDI_Sum", "HAMD_Sum17", "GAFscore",
  "DurDep", "DepEp", "Hosp", "DurHosp", "RS25_Sum"
)

# Create a result data frame
spearman_results_T1 <- data.frame(
  Variable = predictors,
  Spearman_rho = NA,
  p_value = NA,
  p_value_formatted = NA
)

# Loop over predictors
for (i in seq_along(predictors)) {
  var <- predictors[i]
  test <- cor.test(
    ResBrain[[target_var]],
    ResBrain[[var]],
    method = "spearman"
  )
  
  # Store results
  spearman_results_T1$Spearman_rho[i] <- test$estimate
  spearman_results_T1$p_value[i] <- test$p.value
  spearman_results_T1$p_value_formatted[i] <- ifelse(
    test$p.value < 0.001,
    "<0.001",
    formatC(test$p.value, format = "f", digits = 3)
  )
}


#### Environment Cleaning ####

# Cleanup: Remove temporary variables and objects
rm(list = c("DemoData_vars", "idenifier_var", "MDD_DemoData_vars", 
            "variables_of_interest_continuous", "variables_of_interest_dichotomous", 
            "variables_of_interest_polytomous", "vars", "compute_posthoc_dichotomous",
            "compute_posthoc_polytomous", "perform_anova_tukey", "as_expected_group", 
            "data", "DemoData_MDD_ResBrain", "DemoData_ResBrain", "MDD_subset", 
            "ResBrain_des","results_numeric", "correlation_matrix_T1", "df", "df_dep", 
            "df_res", "df_res_dep", "df_vul", "df_vul_dep", "out", "out_dep", 
            "ResBrain_corr", "ResBrain_extreme_groups", "ResBrain_MDD", 
            "ResBrain_MRI_extreme_groups", "ResBrain_MRI_T1", "spearman_results_T1", 
            "test", "X", "i", "new_order", "ordinal_vars", "var", "vars_dep", 
            "is_cat", "p_cat", "p_num", "perform_group_tests", "summ_cat", 
            "summ_num", "summarise_cat", "summarise_num", "tab", "tab_diagnosis", 
            "tab_komorbid", "tab_medication", "tab_rem", "tab_sex", "es", "p", "p1", "p2", 
            "v_rem",  "plot_list", "mu", "preds_to_show", "sigma", "target_var", 
            "v", "vars_to_show", "es_cat_bin", "es_cat_multi", "es_cont", "fmt_mean_sd",
            "fmt_median_iqr", "is_cont", "shapiro_ok", "test_categorical", "test_numeric",
            "df_extreme_groups"))

# Verify cleanup
print(ls())  # List remaining objects in the environment

