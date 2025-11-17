#### Preamble ####

# Run 00a_metadata_and_packages.R first to load dependencies
# Define required packages
required_packages_04 <- c("haven", "dplyr", "missForest", "here", "readxl", "tidyverse", "polycor", "corrplot", "naniar")

# Check and load packages
for (pkg in required_packages_04) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(paste("Package", pkg, "is required but not installed."))
  }
  library(pkg, character.only = TRUE) # Load the package into the environment
}

# Clean up temporary variables
rm(list = c("required_packages_04", "pkg"))

# ----------------------------------------------------------------------------------------------------------------------------------------- # 

#### Filer Data ####
#FOR2107/MACS 2-year follow-up (T2) Data, last updated 10.01.2025, N=2590
T2_FOR2107_unfiltered <- read_sav(here("03_data", "Datenbank_DataFreeze3-4_Follow-Up_10012025.sav"), encoding = "latin1")


# Convert variables to factors
factor_vars <- c("Geschlecht_2", "Group_T2")
T2_FOR2107_unfiltered[factor_vars] <- lapply(T2_FOR2107_unfiltered[factor_vars], as.factor)

# Create new optimized variables
T2_FOR2107_unfiltered <- T2_FOR2107_unfiltered %>%
  mutate(
    Sex_T2 = case_when(Geschlecht_2 == 1 ~ "male",
                       Geschlecht_2 == 2 ~ "female"),
    Diagnosis_T2 = case_when(Group_T2 %in% c(1) ~ "HC",
                             Group_T2 %in% c(2, 8) ~ "MDD")
  ) %>%
  mutate(across(c(Sex_T2, Diagnosis_T2), as.factor))


# Filter data based on general inclusion criteria, enforce NAs in dataframe across ALL columns
T2_FOR2107_unfiltered[T2_FOR2107_unfiltered == -99] <- NA

T2_ResBrain <- T2_FOR2107_unfiltered %>%
  filter(
    DO_Klinisch_final_2 == 0,
    MRT_fehlt_komplett_2 == 0,
    T1_fehlt_2 == 0,
    TIV_2 != -99,
    Diagnosis_T2 %in% c("HC", "MDD")
  ) %>%
  drop_na(TIV_2)


# Explore missing data for relevant variables
relevant_vars <- c(
  "Gesichertes_Risiko_Affektiv_2", "Gesichertes_Risiko_Psychotisch_2", "CTQ_Sum_2",
  "CTQ_EmotionalerMissbrauch_2", "CTQ_KoerperlicherMissbrauch_2", "CTQ_SexuellerMissbrauch_2",
  "CTQ_EmotionaleVernachlaessigung_2", "CTQ_KoerperlicheVernachlaessigung_2",
  "PSS_Sum_2", "LEQ_NegativeEventScore_2", "LEQ_PositiveEventScore_2", "FSozU_Sum_2", "SozDemo5_2",
  "Bildungsjahre_2", "Haushaltsnetto_2", "IQ_2"
)

missing_data <- T2_ResBrain %>%
  select(all_of(relevant_vars))

T2_ResBrain_missing_data <- vis_miss(missing_data, warn_large_data = FALSE)
show(T2_ResBrain_missing_data)


# Exclude NAs for included relevant risk and protection variables (exception with "Haushaltsnetto" as NAs will be imputed later)
# Note: Other variables have been imputed during quality control of the FOR2107 cohort. NAs here exceed the imputation threshold.

predictor_vars <- c(
  "FSozU_Sum_2", "SozDemo5_2", "LEQ_NegativeEventScore_2", "LEQ_PositiveEventScore_2",
  "CTQ_Sum_2", "Gesichertes_Risiko_Affektiv_2", "Gesichertes_Risiko_Psychotisch_2", 
  "Bildungsjahre_2", "IQ_2", "PSS_Sum_2"
)

T2_ResBrain <- T2_ResBrain %>%
  drop_na(all_of(predictor_vars))%>%
  drop_na(HAMD_Sum17_2)

# ----------------------------------------------------------------------------------------------------------------------------------------- # 



##### Merge T1 and T2 Data #####
# Full Dataframe T1+T2: N=808
common_cols <- intersect(names(T2_ResBrain), names(ResBrain))
ResBrain_All_T1_T2 <- merge(T2_ResBrain, ResBrain, by = common_cols)

# ----------------------------------------------------------------------------------------------------------------------------------------- # 


#### Data Imputation ####

# predictor vars are updated T2 measures, others are not reassessed at T2 and therefore carried on from T1
imputation_vars <- c(predictor_vars, "ACE_Sum", "RSQ_Secure", "RSQ_Preoccupied", "RSQ_Dismissing", "RSQ_Fearful", "NEOFFI_Extraversion", "NEOFFI_Neurotizismus", "NEOFFI_Vertraeglichkeit", "NEOFFI_Offenheit", "NEOFFI_Gewissenhaftigkeit", "Immigration", "Haushaltsnetto_2")


#CAVE HAUSHALTSNETTO is > 9999 here (max 17500)
ResBrain_imputation_T2 <- ResBrain_All_T1_T2 %>%
  filter(Proband %in% T2_ResBrain$Proband) %>%
  select(all_of(imputation_vars))


sum(is.na(ResBrain_imputation_T2$Haushaltsnetto_2))

imp_vars_to_factor <- c("Gesichertes_Risiko_Affektiv_2", "Gesichertes_Risiko_Psychotisch_2", "RSQ_Secure", "RSQ_Preoccupied", "RSQ_Dismissing", "RSQ_Fearful", "Immigration", "SozDemo5_2")
ResBrain_imputation_T2[imp_vars_to_factor] <- lapply(ResBrain_imputation_T2[imp_vars_to_factor], as.factor)


# Select all predictor variables (excluding Haushaltsnetto)
predictor_vars_imputation <- setdiff(imputation_vars, "Haushaltsnetto_2")

# Include Haushaltsnetto as the response variable
predictors_df <- ResBrain_imputation_T2[predictor_vars_imputation]
response_var_imputation <- ResBrain_imputation_T2$Haushaltsnetto_2
# Combine predictor and response variables into a single data frame
data_for_imputation <- cbind(response_var_imputation, predictors_df)

# Set a seed for reproducibility
set.seed(123)

# Impute missing values using missForest
imputed_data <- missForest(data_for_imputation)

# Access the imputed values
imputed_values <- imputed_data$ximp

# Convert imputed values back to a data frame (if needed)
imputed_df <- as.data.frame(imputed_values)

# Check the structure of the imputed dataset
str(imputed_df)


#replace Haushaltsnetto with NAs with the imputed variables
ResBrain_All_T1_T2$Haushaltsnetto_2 <- imputed_df$response_var_imputation

# ----------------------------------------------------------------------------------------------------------------------------------------- # 

#### Variable adjustment ####

# Convert variables to appropriate types
risk_factor_vars <- imp_vars_to_factor
risk_numeric_vars <- c(
  "CTQ_Sum_2", "ACE_Sum", "PSS_Sum_2", "NEOFFI_Neurotizismus",
  "NEOFFI_Extraversion", "NEOFFI_Vertraeglichkeit", "NEOFFI_Offenheit", "NEOFFI_Gewissenhaftigkeit",
  "LEQ_NegativeEventScore_2", "LEQ_PositiveEventScore_2",
  "FSozU_Sum_2", "Bildungsjahre_2", "Haushaltsnetto_2", "IQ_2"
)

ResBrain_All_T1_T2[risk_factor_vars] <- lapply(ResBrain_All_T1_T2[risk_factor_vars], as.factor)
ResBrain_All_T1_T2[risk_numeric_vars] <- lapply(ResBrain_All_T1_T2[risk_numeric_vars], as.numeric)

ResBrain_prepro_T2 <- ResBrain_All_T1_T2[imputation_vars]


# ----------------------------------------------------------------------------------------------------------------------------------------- # 


#### Exploring risk and protection factor multicolinearity ####
common_cols <- intersect(names(ResBrain_prepro_T2), names(ResBrain_All_T1_T2))

# Merge based on common columns
ResBrain_prepro_T2 <- ResBrain_prepro_T2 %>%
  left_join(select(ResBrain_All_T1_T2, all_of(common_cols), HAMD_Sum17_2), by = common_cols)

# Compute mixed-type correlations
correlation_matrix_T2 <- hetcor(ResBrain_prepro_T2, ML = FALSE)

# Define the output file path
output_dir <- here("05_figures/001_defaultexp_figures")
output_file <- file.path(output_dir, "correlation_matrix_T2.jpeg")  # Change to .pdf if needed

jpeg(output_file, width = 2400, height = 1800, res = 150)  # Adjust size and resolution


# Visualize the correlation matrix
corrplot(
  correlation_matrix_T2$correlations,
  method = "color",
  tl.cex = 0.8,
  tl.col = "black",
  tl.srt = 45,
  type = 'upper',
  number.cex = 0.6,
  addCoef.col = "black",
  family = "serif"
)

dev.off()


# ----------------------------------------------------------------------------------------------------------------------------------------- # 



#### Environment Cleaning ####

# Cleanup: Remove temporary variables and objects
rm(list = c("T2_ResBrain_missing_data", "factor_vars", "relevant_vars", 
            "missing_data", "data_for_imputation", "imputed_data", "imputed_df", 
            "imputed_values", "predictor_vars", "predictors_df", 
            "predictor_vars_imputation", "ResBrain_imputation_T2", 
            "ResBrain_prepro", "ResBrain_prepro_T2", "imp_vars_to_factor", 
            "response_var_imputation", "risk_factor_vars", "risk_numeric_vars", 
            "output_dir", "output_file","imputation_vars", 
            "correlation_matrix_T2", "T2_ResBrain", "predictors"))

# Verify cleanup
print(ls())  # List remaining objects in the environment
