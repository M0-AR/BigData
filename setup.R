# Package dependencies
packages <- c(
  "tidyverse",
  "caret",
  "rpart",
  "e1071",
  "ggplot2",
  "cluster",
  "ggdendro",
  "rpart.plot"
)

# Install missing packages
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load packages
lapply(packages, library, character.only = TRUE)

# Print session info
sessionInfo()
