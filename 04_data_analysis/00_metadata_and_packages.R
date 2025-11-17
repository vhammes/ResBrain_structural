#### Session and author information####
# Function to generate project metadata
generate_metadata <- function(author = "Vincent Hammes", project = "ResBrain_structural", date_last_modified = "14.11.2025") {
  metadata <- list(
    Author = author,
    Project = project,
    Date_lasr_modified = date_last_modified,
    R_Version = R.version.string,
    Platform = R.version$platform,
    Date = Sys.Date(),
    Session_Info = sessionInfo()
  )
  return(metadata)
}

# Generate metadata
ResBrain_Metadata <- generate_metadata(author = "Vincent Hammes", project = "ResBrain")
print(ResBrain_Metadata)


#### List of required packages####
packages <- c(
  'haven',          # Import and export of SPSS, Stata, and SAS files
  'foreign',        # Read data from other statistical systems (e.g., SPSS, Stata)
  'plyr',           # Tools for splitting, applying, and combining data
  'dplyr',          # Data manipulation with a grammar of data
  'psych',          # Procedures for psychological research (e.g., factor analysis)
  'tidyverse',      # Collection of packages for data science (e.g., ggplot2, dplyr)
  'writexl',        # Export data frames to Excel files
  'openxlsx',       # Read, write, and format Excel files
  'finalfit',       # Create summary tables and regression results for medical research
  'arsenal',        # Tools for data summarization and comparison
  'effectsize',     # Allowing computation of and conversion between indices such as Cohen's d, r, odds, etc.
  'MatchIt',        # Perform propensity score matching
  'ggpattern',      # Add patterns to ggplot2 geoms
  'patchwork',      # Combine multiple ggplot2 plots into a single layout
  'hrbrthemes',     # Themes and typography for ggplot2
  'ggsignif',       # Add significance annotations to ggplot2 plots
  'ggpubr',         # Simplify the creation of publication-ready ggplot2 plots
  'anticlust',      # Create balanced groups for clustering
  'readxl',         # Import Excel files into R
  'glmnet',         # Regularized regression (e.g., lasso, ridge)
  'corrplot',       # Visualize correlation matrices
  'randomForest',   # Create random forest models for classification/regression
  #'missForest',     # Impute missing data using random forests -> version 1.5 was used (newer version changes output that leads to slightly different extreme group sizes later on)
  'polycor',        # Compute polychoric and polyserial correlations
  'naniar',         # Handle and visualize missing data
  'chisq.posthoc.test', # Post-hoc tests for chi-squared tests
  'BayesFactor',    # Perform Bayesian analysis
  'here',            # Manage file paths relative to the project root
  'blandr'          # Perform Bland-Altman analysis and create agreement plots
  )


# Ensure renv is installed
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

# Initialize renv (only the first time, if renv.lock does not exist)
if (!file.exists("renv.lock")) {
  renv::init()
}

# Function to install and load packages
install_and_load <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

# Automatically install and load all packages in the list
invisible(lapply(packages, install_and_load))

#### Pin missForest to version 1.5, which was used for publication analyses ####
if (!requireNamespace("missForest", quietly = TRUE) ||
    packageVersion("missForest") != "1.5") {
  # install specific version via renv
  renv::install("missForest@1.5")
}

library(missForest)


# Create a snapshot of the current package environment
renv::snapshot()

# Remove temporary variables
rm(ResBrain_Metadata, packages, generate_metadata, install_and_load)

