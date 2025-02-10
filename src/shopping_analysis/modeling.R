# Shopping Intention Analysis - Modeling
# Author: M0-AR
# Date: 2025-02-10

library(rpart)
library(rpart.plot)
library(e1071)
library(caret)

# Decision Tree Model
train_decision_tree <- function(train_data, max_depth = 5) {
  tryCatch({
    model <- rpart(Revenue ~ .,
                  data = train_data,
                  method = "class",
                  control = rpart.control(maxdepth = max_depth))
    
    return(model)
  }, error = function(e) {
    stop("Error training decision tree: ", e$message)
  })
}

# SVM Model
train_svm <- function(train_data) {
  tryCatch({
    model <- svm(Revenue ~ .,
                data = train_data,
                type = "C-classification",
                kernel = "radial")
    
    return(model)
  }, error = function(e) {
    stop("Error training SVM: ", e$message)
  })
}

# KNN Model
train_knn <- function(train_data, k = 5) {
  tryCatch({
    model <- train(Revenue ~ .,
                  data = train_data,
                  method = "knn",
                  tuneGrid = data.frame(k = k))
    
    return(model)
  }, error = function(e) {
    stop("Error training KNN: ", e$message)
  })
}

# Model Evaluation
evaluate_model <- function(model, test_data, model_name) {
  predictions <- predict(model, test_data)
  
  # Calculate confusion matrix
  cm <- confusionMatrix(predictions, test_data$Revenue)
  
  # Create results summary
  results <- list(
    model_name = model_name,
    accuracy = cm$overall["Accuracy"],
    precision = cm$byClass["Precision"],
    recall = cm$byClass["Recall"],
    f1_score = cm$byClass["F1"],
    confusion_matrix = cm$table
  )
  
  return(results)
}

# Cross Validation
perform_cv <- function(train_data, model_type, k = 5) {
  ctrl <- trainControl(method = "cv", number = k)
  
  model <- switch(model_type,
                 "decision_tree" = train(Revenue ~ ., 
                                       data = train_data, 
                                       method = "rpart",
                                       trControl = ctrl),
                 "svm" = train(Revenue ~ .,
                              data = train_data,
                              method = "svmRadial",
                              trControl = ctrl),
                 "knn" = train(Revenue ~ .,
                              data = train_data,
                              method = "knn",
                              trControl = ctrl),
                 stop("Invalid model type"))
  
  return(model)
}
