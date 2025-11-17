In order to reproduce these analyses exactly, use the same package versions as per renv.lock. 00_metadata_and_packages.R will install all packages but might update package versions upon first run. With updated package versions, imputation or cross-validation packages might produce slightly different outcomes.

# In a fresh R session
install.packages("renv")
renv::restore()    # restores package versions

source("00a_metadata_and_packages.R")
