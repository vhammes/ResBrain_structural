#### Preamble ####

# Run 00a_metadata_and_packages.R first to load dependencies
# Define required packages
required_packages_01 <- c("haven", "dplyr", "naniar", "here", "tidyverse", "missForest", "readxl", "polycor", "corrplot")

# Check and load packages
for (pkg in required_packages_01) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(paste("Package", pkg, "is required but not installed."))
  }
  library(pkg, character.only = TRUE) # Load the package into the environment
}

# Clean up temporary variables
rm(list = c("required_packages_01", "pkg"))

# ----------------------------------------------------------------------------------------------------------------------------------------- # 


#### Filter Data ####
#FOR2107/MACS Baseline (T1) Data, last updated 10.01.2025, N=2973

# Load data, filter relevant variables for this project
FOR2107_unfiltered <- read_sav(here("03_data", "Datenbank_Update_DataFreeze1-4-10012025.sav"), encoding = "latin1")
FOR_Resilience = FOR2107_unfiltered[c("Proband","DataFreeze","DO_Klinisch_final","DO_Medikation_fMRT","MRT_fehlt_komplett","RS_fehlt","DTI_fehlt","DTI_dropout","TIV","Dummy_BC_MR_pre","Dummy_BC_MR_post","Dummy_GS_MR_post","T1_fehlt","Datum_Interview","Datum_MRT","SKID_Diagnose_1","SKID_Diagnose_2","SKID_Diagnose_3","SKID_Diagnose_4","SKID_Diagnose_5","DSM_DiagnoseCodierung_1","DSM_DiagnoseCodierung_2","DSM_DiagnoseCodierung_3","DSM_DiagnoseCodierung_4","DSM_DiagnoseCodierung_5","Komorbid","Rem","AgeOfOnset","Sum_SACKEIM","Sum_MED","CTQ_Umweltrisiko_1Skala","GenRisiko_Affektiv1","GenRisiko_Psycho1","Group","Alter","Geschlecht","BDI_Sum","HAMD_Sum17","HAMD_Sum21","CTQ_Sum","CTQ_SexuellerMissbrauch","CTQ_EmotionalerMissbrauch","CTQ_KoerperlicherMissbrauch","CTQ_EmotionaleVernachlaessigung","CTQ_KoerperlicheVernachlaessigung","ACE_Sum","FSozU_Sum","PSS_Sum","LEQ_NegativeEventScore","LEQ_PositiveEventScore","GAFscore","DurDep","DepEp","Hosp","DurHosp","LEQ_TotalEventScore","RS25_Sum","RS25_Akzeptanz","RS25_PersoenlicheKompetenz","NEOFFI_Extraversion","NEOFFI_Neurotizismus","NEOFFI_Offenheit","NEOFFI_Vertraeglichkeit","NEOFFI_Gewissenhaftigkeit","RSQ_AngstVorNaehe","RSQ_WenigAngstVorNaehe","RSQ_AngstVorTrennung","RSQ_WenigAngstVorTrennung","RSQ_Dismissing","RSQ_Fearful","RSQ_Preoccupied","RSQ_Secure","RSQ_FehlendesVertrauen","RSQ_WunschNachUnabhaengigkeit","FEB_FuersorgeMutter","FEB_CutOff_FuersorgeMutter","FEB_FuersorgeVater","FEB_CutOff_FuersorgeVater","FEB_KontrolleMutter","FEB_CutOff_KontrolleMutter","FEB_CutOff_KontrolleVater","FEB_Beziehungsquadrant_Mutter","FEB_Beziehungsquadrant_Vater","TimeSinceTreat","Bildungsjahre","IQ","Haushaltsnetto","SozDemo5","Famil_Belastung","Immigration")]

# enforce NAs in dataframe across ALL columns
FOR_Resilience[FOR_Resilience == -99] <- NA

# Convert variables to factors
factor_vars <- c("Geschlecht", "MRT_fehlt_komplett", "Group")
FOR_Resilience[factor_vars] <- lapply(FOR_Resilience[factor_vars], as.factor)

# Create new optimized variables
FOR_Resilience <- FOR_Resilience %>%
  mutate(
    sex = case_when(Geschlecht == 1 ~ "male",
                    Geschlecht == 2 ~ "female"),
    Diagnosis = case_when(Group %in% c(1) ~ "HC",
                          Group %in% c(2, 8) ~ "MDD")
  ) %>%
  mutate(across(c(sex, Diagnosis), as.factor))

# Filter data based on general inclusion criteria

ResBrain <- FOR_Resilience %>%
  filter(
    DO_Klinisch_final == 0,
    MRT_fehlt_komplett == 0, 
    T1_fehlt == 0, 
    Diagnosis %in% c("HC", "MDD")
  ) %>%
  drop_na(TIV) %>% 
  slice(-(1:9)) # Exclude subjects 1-9 with deviating MRI parameters

# Explore missing data for project-relevant variables
relevant_vars <- c(
  "GenRisiko_Affektiv1", "GenRisiko_Psycho1", "Famil_Belastung", "CTQ_Sum",
  "CTQ_EmotionalerMissbrauch", "CTQ_KoerperlicherMissbrauch", "CTQ_SexuellerMissbrauch",
  "CTQ_EmotionaleVernachlaessigung", "CTQ_KoerperlicheVernachlaessigung", "ACE_Sum",
  "RSQ_Secure", "RSQ_Preoccupied", "RSQ_Dismissing", "RSQ_Fearful", "PSS_Sum", "Immigration",
  "LEQ_NegativeEventScore", "LEQ_PositiveEventScore", "FSozU_Sum", "SozDemo5",
  "NEOFFI_Neurotizismus", "NEOFFI_Extraversion", "NEOFFI_Gewissenhaftigkeit", "NEOFFI_Offenheit", "NEOFFI_Vertraeglichkeit", "Bildungsjahre", "Haushaltsnetto", "IQ", "HAMD_Sum17"
)

missing_data <- ResBrain %>%
  select(all_of(relevant_vars))

ResBrain_missing_data <- vis_miss(missing_data, warn_large_data = FALSE)
show(ResBrain_missing_data)


# Exclude NAs for included relevant risk and protection variables (exception with "Haushaltsnetto" as NAs will be imputed later)
# Note: Other variables have been imputed during quality control of the FOR2107 cohort. NAs here exceed the imputation threshold.

predictor_vars <- c(
  "FSozU_Sum", "SozDemo5", "LEQ_NegativeEventScore", "LEQ_PositiveEventScore",
  "CTQ_Sum", "GenRisiko_Affektiv1", "GenRisiko_Psycho1", "ACE_Sum", 
  "Immigration", "Bildungsjahre", "IQ", "NEOFFI_Extraversion", 
  "NEOFFI_Neurotizismus", "NEOFFI_Gewissenhaftigkeit", "NEOFFI_Offenheit", 
  "NEOFFI_Vertraeglichkeit", "Famil_Belastung", "PSS_Sum", "RSQ_Secure", 
  "RSQ_Preoccupied", "RSQ_Dismissing", "RSQ_Fearful"
)

ResBrain <- ResBrain %>%
  drop_na(all_of(predictor_vars))%>%
  drop_na(HAMD_Sum17)


# ----------------------------------------------------------------------------------------------------------------------------------------- # 

#### Data Imputation ####

# impute income ("Haushaltsnetto") in missing cases
ResBrain_imputation <- ResBrain[c("GenRisiko_Affektiv1","GenRisiko_Psycho1","CTQ_Sum","ACE_Sum","RSQ_Secure", "RSQ_Preoccupied", "RSQ_Dismissing", "RSQ_Fearful", "PSS_Sum","NEOFFI_Neurotizismus", "NEOFFI_Gewissenhaftigkeit", "NEOFFI_Offenheit", "NEOFFI_Vertraeglichkeit","Immigration","LEQ_NegativeEventScore", "LEQ_PositiveEventScore","FSozU_Sum","SozDemo5","NEOFFI_Extraversion","Bildungsjahre","Haushaltsnetto","IQ")]
sum(is.na(ResBrain_imputation$Haushaltsnetto))

imp_vars_to_factor <- c("GenRisiko_Affektiv1", "GenRisiko_Psycho1", "RSQ_Secure", "RSQ_Preoccupied", "RSQ_Dismissing", "RSQ_Fearful", "Immigration", "SozDemo5")
ResBrain_imputation[imp_vars_to_factor] <- lapply(ResBrain_imputation[imp_vars_to_factor], as.factor)

# Impute data 
ResBrain_complete <- ResBrain_imputation

# Select all predictor variables (excluding Haushaltsnetto)
predictor_vars_imputation <- ResBrain_complete[, -which(names(ResBrain_complete) == "Haushaltsnetto")]

# Include Haushaltsnetto as the response variable
response_var_imputation <- ResBrain_complete$Haushaltsnetto

# Combine predictor and response variables into a single data frame
data_for_imputation <- cbind(response_var_imputation, predictor_vars_imputation)

# Set a seed for reproducibility
set.seed(123)

# Impute missing values using missForest
imputed_data <- missForest(data_for_imputation)

# Access the imputed values
imputed_values <- imputed_data$ximp

# Convert imputed values back to a data frame if needed
imputed_df <- as.data.frame(imputed_values)

#replace Haushaltsnetto with NAs with the imputed variables
ResBrain$Haushaltsnetto <- imputed_df$response_var_imputation
ResBrain_complete$Haushaltsnetto <- imputed_df$response_var_imputation




#### Variable adjustment ####

# Convert variables to appropriate types
risk_factor_vars <- c("GenRisiko_Affektiv1", "GenRisiko_Psycho1", "RSQ_Secure", "RSQ_Preoccupied", "RSQ_Dismissing", "RSQ_Fearful", "Immigration", "SozDemo5")
risk_numeric_vars <- c(
  "CTQ_Sum", "ACE_Sum", "PSS_Sum", "NEOFFI_Neurotizismus",
  "NEOFFI_Extraversion", "NEOFFI_Gewissenhaftigkeit", "NEOFFI_Offenheit", 
  "NEOFFI_Vertraeglichkeit", "LEQ_NegativeEventScore", "LEQ_PositiveEventScore",
  "FSozU_Sum", "Bildungsjahre", "Haushaltsnetto", "IQ"
)

ResBrain[risk_factor_vars] <- lapply(ResBrain[risk_factor_vars], as.factor)
ResBrain[risk_numeric_vars] <- lapply(ResBrain[risk_numeric_vars], as.numeric)

ResBrain_complete[risk_factor_vars] <- lapply(ResBrain_complete[risk_factor_vars], as.factor)
ResBrain_complete[risk_numeric_vars] <- lapply(ResBrain_complete[risk_numeric_vars], as.numeric)


# ----------------------------------------------------------------------------------------------------------------------------------------- # 


#### Exploring risk and protection factor multicolinearity ####
common_cols <- intersect(names(ResBrain_complete), names(ResBrain))

# Merge based on common columns
ResBrain_complete <- ResBrain_complete %>%
  left_join(select(ResBrain, all_of(common_cols), HAMD_Sum17), by = common_cols)


# Prepare data for correlation analysis
ResBrain_corr <- as.data.frame(ResBrain_complete) %>%
  dplyr::rename_with(~ c(
    "Familial risk AD", "Familial risk PD", "CTQ sum score", "ACE sum score", "RSQ secure", "RSQ preoccupied", 
    "RSQ dismissing", "RSQ fearful", "PSS sum score", "NEO-FFI neuroticism", "NEO-FFI conscientiousness", 
    "NEO-FFI openess", "NEO-FFI agreeableness", "Immigration", "LEQ negative sum score",
    "LEQ positive sum score", "FSozU sum score", "Social socio-demographic item",
    "NEO-FFI extraversion", "Education", "Household Income", "IQ", "HAMD-17 sum score"
  ))


# Convert ordinal variables to factors
ordinal_vars <- c(
  "Familial risk AD", "Familial risk PD", "RSQ secure", 
  "RSQ preoccupied", "RSQ dismissing", "RSQ fearful",
  "Immigration", "Social socio-demographic item"
)
ResBrain_corr[ordinal_vars] <- lapply(ResBrain_corr[ordinal_vars], as.factor)

# Compute mixed-type correlations
correlation_matrix_T1 <- hetcor(ResBrain_corr, ML = FALSE)

# Define the output file path
correlation_matrix_T1 <- tryCatch({
  hetcor(ResBrain_corr, ML = FALSE)
})

# Check if the computation was successful
if (is.null(correlation_matrix_T1)) {
  stop("Correlation matrix calculation failed. Check data formatting.")
}

# Define the output directory and create it if it doesn't exist
output_dir <- here("05_figures/001_defaultexp_figures")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)  # Create directory if missing
}

# Define the output file path
output_file <- file.path(output_dir, "correlation_matrix_T1.jpeg")

# Open the JPEG graphics device
jpeg(output_file, width = 1500, height = 1200, res = 150)

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
  family = "serif"
)

dev.off()

# ----------------------------------------------------------------------------------------------------------------------------------------- # 


#### Environment Cleaning ####

# Cleanup: Remove temporary variables and objects
rm(list = c("ResBrain_missing_data", "factor_vars", "predictor_vars", "relevant_vars", "missing_data","FOR_Resilience","correlation_matrix_T1" ,"data_for_imputation", "imputed_data", "imputed_df", "imputed_values", "predictor_vars_imputation", "ResBrain_complete", "ResBrain_corr", "ResBrain_imputation", "imp_vars_to_factor", "output_dir", "output_file", "ordinal_vars", "response_var_imputation", "risk_factor_vars", "risk_numeric_vars"))

# Verify cleanup
print(ls())  # List remaining objects in the environment
