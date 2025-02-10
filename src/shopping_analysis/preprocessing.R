# Shopping Intention Analysis - Data Preprocessing
# Author: M0-AR
# Date: 2025-02-10

library(tidyverse)
library(caret)

# Load and preprocess data
load_and_preprocess_data <- function(file_path) {
  # Read the dataset
  tryCatch({
    dataset <- read.csv(file_path)
    message("Successfully loaded data from ", file_path)
    
    # Encode categorical variables
    dataset$Month <- factor(dataset$Month,
                          levels = c('Feb', 'Mar', 'May', 'June', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'),
                          labels = c(2, 3, 5, 6, 7, 8, 9, 10, 11, 12))
    
    dataset$VisitorType <- factor(dataset$VisitorType,
                                levels = c('New_Visitor', 'Other', 'Returning_Visitor'),
                                labels = c(1, 2, 3))
    
    dataset$Weekend <- factor(dataset$Weekend,
                            levels = c('FALSE', 'TRUE'),
                            labels = c(0, 1))
    
    dataset$Revenue <- factor(dataset$Revenue,
                            levels = c('FALSE', 'TRUE'),
                            labels = c(0, 1))
    
    return(dataset)
  }, error = function(e) {
    stop("Error loading data: ", e$message)
  })
}

# Analyze missing data
analyze_missing_data <- function(dataset) {
  missing_summary <- sapply(dataset, function(x) sum(is.na(x)))
  missing_percent <- round(100 * missing_summary / nrow(dataset), 2)
  
  missing_df <- data.frame(
    Variable = names(missing_summary),
    Missing_Count = missing_summary,
    Missing_Percent = missing_percent
  )
  
  return(missing_df[order(-missing_df$Missing_Percent), ])
}

# Split data into training and testing sets
split_data <- function(dataset, split_ratio = 0.8) {
  set.seed(123)  # For reproducibility
  train_index <- createDataPartition(dataset$Revenue, p = split_ratio, list = FALSE)
  train_set <- dataset[train_index, ]
  test_set <- dataset[-train_index, ]
  
  return(list(train = train_set, test = test_set))
}

# Feature scaling
scale_features <- function(train_set, test_set) {
  # Select numeric columns
  numeric_cols <- sapply(train_set, is.numeric)
  
  # Scale numeric features
  preprocess_params <- preProcess(train_set[, numeric_cols], method = c("center", "scale"))
  
  train_set[, numeric_cols] <- predict(preprocess_params, train_set[, numeric_cols])
  test_set[, numeric_cols] <- predict(preprocess_params, test_set[, numeric_cols])
  
  return(list(train = train_set, test = test_set))
}
