In order to reproduce these analyses exactly, use the same package versions (updated imputation or cross-validation packages might differ otherwise), as per "renv" as follows: 

# In a fresh R session
install.packages("renv")
renv::restore()    # restores package versions
source("00a_metadata_and_packages.R")