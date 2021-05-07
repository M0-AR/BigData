#https://machinelearningmastery.com/machine-learning-in-r-step-by-step/
# Data Preprocessing Template

# Importing the dataset
dataset <-  read.csv("online_shoppers_intention.csv")

# dimensions of dataset
dim(dataset)

# list types fo each attribute 
sapply(dataset, class)

# take a peek at the first 5 rows of the data 
head(dataset)

# summarize attribute distributions 
summary(dataset)

# finding missing data
is.na(dataset)
sum(is.na(dataset))

# Encoding categorical data
summary(dataset$Month) # See how many months should I include. 
dataset$Month = factor(dataset$Month,
                       levels = c('Feb', 'Mar', 'May', 'June', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'),
                       labels = c(2, 3, 5, 6, 7, 8, 9, 10, 11, 12))

summary(dataset$VisitorType) # See how many categorical of VisitorType are there 
dataset$VisitorType = factor(dataset$VisitorType,
                             levels = c('New_Visitor', 'Other', 'Returning_Visitor'),
                             labels = c(1, 2, 3))

summary(dataset$Weekend) # Mode :logical
dataset$Weekend = factor(dataset$Weekend,
                         levels = c('FALSE', 'TRUE'),
                         labels = c(0, 1))

summary(dataset$Revenue) # Mode :logical 
dataset$Revenue = factor(dataset$Revenue,
                         levels = c('FALSE', 'TRUE'),
                         labels = c(0, 1))





##############################################################################
# 4.22: Scatter plot and histogram and boxplot, ref. [Prabhakaran, 2018] 
#
#install.packages("knitr",dependencies=TRUE)
install.packages("knitr")
#install.packages("ggplot2", dependencies=TRUE)
install.packages("ggplot2")
install.packages("ggExtra")
install.packages("yaml")
library(knitr)
library(ggplot2)
library(ggExtra)
library(yaml)
#
opar.org <- par(no.readonly=TRUE) # Store original graphical parameter setting.
# Scatterplot   --------------------------------------
#
theme_set(theme_bw())  # pre-set the bw theme.
mpg_select <- dataset
g <- ggplot(dataset, aes(Month, Revenue)) + 
    geom_count() + 
    geom_smooth(method="lm", se=F)

ggMarginal(g, type = "histogram", fill="transparent")
ggMarginal(g, type = "boxplot", fill="transparent")# ??? Not working 
#
# Scatterplot to pdf          ------------------------
pdf("Fig_4_2_Scatter.pdf")
g <- ggplot(dataset, aes(Month, Revenue)) + 
    geom_count() + 
    geom_smooth(method="lm", se=F)

ggMarginal(g, type = "histogram", fill="transparent")
ggMarginal(g, type = "boxplot", fill="transparent")

dev.off()
par(opar.org)
#
###########################################################
# 4.23: Hierarchical Dendrogram, ref. [Prabhakaran, 2018] 
#
install.packages("ggdendro")
library(ggplot2)
library(ggdendro)
theme_set(theme_bw())
#
# Dataset USArrests
#str(USArrests)
#head(USArrests,5)

hc <- hclust(dist(dataset), "ave")  # hierarchical clustering

# plot
ggdendrogram(hc, rotate = FALSE, size = 2) #??? not visible 

pdf("Fig_4_2_USArrests.pdf")
ggdendrogram(hc, rotate = TRUE, size = 2)
dev.off()
par(opar.org)
#####################################################################

####################################################################
# 4.25: "Data Visualization with R", [Kabacoff, 2020]
#
#
pkgs <- c("ggplot2", "dplyr", "tidyr", 
          "mosaicData", "carData",
          "VIM", "scales", "treemapify",
          "gapminder", "ggmap", "choroplethr",
          "choroplethrMaps", "CGPfunctions",
          "ggcorrplot", "visreg",
          "gcookbook", "forcats",
          "survival", "survminer",
          "ggalluvial", "ggridges",
          "GGally", "superheat",
          "waterfalls", "factoextra",
          "networkD3", "ggthemes",
          "hrbrthemes", "ggpol",
          "ggbeeswarm")
install.packages(pkgs)

#
# Example [Kabacoff, 2020] Chap. 8 "Linear Regression".
#
# gg: Poduce a ggplor2 graph
#
dataset_lm <- lm(Revenue ~ Administrative + Administrative_Duration +
                    Informational + Informational_Duration + 
                    ProductRelated + ProductRelated_Duration +
                    BounceRates + ExitRates + PageValues + 
                    SpecialDay + Month + OperatingSystems +
                    Browser + Region + TrafficType + VisitorType +
                    Weekend, 
                data = dataset)
# conditional plot of price vs. living area
library(ggplot2)
install.packages('visreg')
library(visreg)
visreg(dataset_lm, "Revenue", gg = TRUE) # ??? 

pdf("fig_4_25_House_Price_1.pdf")
visreg(houses_lm, "livingArea", gg = TRUE)
dev.off()
par(opar.org)
#
#
# conditional plot of price vs. waterfront location
visreg(dataset_lm, "waterfront", gg = TRUE) +
    scale_y_continuous(label = scales::dollar) +
    labs(title = "Relationship between price and location",
         subtitle = "controlling for lot size, age, land value, bedrooms and bathrooms",
         caption = "source: Saratoga Housing Data (2006)",
         y = "Home Price",
         x = "Waterfront")
pdf("fig_4_25_House_Price_2.pdf")
visreg(houses_lm, "waterfront", gg = TRUE) +
    scale_y_continuous(label = scales::dollar) +
    labs(title = "Relationship between price and location",
         subtitle = "controlling for lot size, age, land value, bedrooms and bathrooms",
         caption = "source: Saratoga Housing Data (2006)",
         y = "Home Price",
         x = "Waterfront")
visreg(houses_lm, "livingArea", gg = TRUE)
dev.off()
par(opar.org)




#------------------------------------------------------------------------------------
# Classification template

# Splitting the dataset into the Training set and Test set
#install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Revenue, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling ???

# Fitting Logistic Regression to the Training set #########
classifier = glm(formula = Revenue ~ .,
                 family = binomial,
                 data = training_set)

# Predicting the Test set results
prob_pred = predict(classifier, type = 'response', newdata = test_set[-18])
y_pred = ifelse(prob_pred > 0.5, 1, 0)

# Making the Confusion Matrix
cm_logistic_regression = table(test_set[, 18], y_pred > 0.5)
cm_logistic_regression


# Visualising the Training set results

# Visualising the Test set results




# Fitting K-NN to the Training set and Predicting the Test set results
library(class)
y_pred = knn(train = training_set[, -18],
             test = test_set[, -18],
             cl = training_set[, 18],
             k = 5,
             prob = TRUE)

# Making the Confusion Matrix
cm_knn = table(test_set[, 18], y_pred)
cm_knn


# Fitting SVM to the Training set
# install.packages('e1071')
library(e1071)
classifier = svm(formula = Revenue ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear')

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-18])

# Making the Confusion Matrix
cm_SVM = table(test_set[, 18], y_pred)
cm_SVM



# Fitting Kernel SVM to the Training set
# install.packages('e1071')
library(e1071)
classifier = svm(formula = Revenue ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'radial')

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-18])

# Making the Confusion Matrix
cm_kernel_SVM = table(test_set[, 18], y_pred)
cm_kernel_SVM




# Fitting Naive Bayes to the Training set
# install.packages('e1071')
library(e1071)
classifier = naiveBayes(x = training_set[-18],
                        y = training_set$Revenue)
# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-18])

# Making the Confusion Matrix
cm_naive_bayes = table(test_set[, 18], y_pred)
cm_naive_bayes


# Fitting Decision Tree Classification to the Training set
# install.packages('rpart')
library(rpart)
classifier = rpart(formula = Revenue ~ .,
                   data = training_set)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-18], type = 'class')

# Making the Confusion Matrix
cm_decision_tree = table(test_set[, 18], y_pred)
cm_decision_tree


# Fitting Random Forest Classification to the Training set
#install.packages('randomForest')
library(randomForest)
set.seed(123)
classifier = randomForest(x = training_set[-18],
                          y = training_set$Revenue,
                          ntree = 500)
# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-18])

# Making the Confusion Matrix
cm_random_forest = table(test_set[, 18], y_pred)
cm_random_forest

#-------------------------------------------------------
# K-Means Clustering
# Using the elbow method to find the optimal number of clusters
set.seed(6)
wcss = vector()
for (i in 1:10) wcss[i] = sum(kmeans(dataset, i)$withinss)
plot(1:10,
     wcss,
     type = 'b',
     main = paste('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')

# Fitting K-Means to the dataset
set.seed(29)
kmeans = kmeans(x = dataset, centers = 5)
y_kmeans = kmeans$cluster

# Visualising the clusters
library(cluster)
clusplot(dataset,
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of customers'),
         xlab = 'Annual Income',
         ylab = 'Spending Score')



# Hierarchical Clustering
# Using the dendrogram to find the optimal number of clusters
dendrogram = hclust(d = dist(dataset, method = 'euclidean'), method = 'ward.D')
plot(dendrogram,
     main = paste('Dendrogram'),
     xlab = 'Customers',
     ylab = 'Euclidean distances')

# Fitting Hierarchical Clustering to the dataset
hc = hclust(d = dist(dataset, method = 'euclidean'), method = 'ward.D')
y_hc = cutree(hc, 5)

# Visualising the clusters
library(cluster)
clusplot(dataset,
         y_hc,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels= 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of customers'),
         xlab = 'Annual Income',
         ylab = 'Spending Score')

##############################################################################
#                      "R Intro Classification"  TABLE OF CONTENTS
#
# There are two objectives:
# 
# Objective 1:
# 6.0 - 6.5 Introduce the common steps in data classification mainly 
#     following the Chap. 17 "Classification", pp. 339- [Kabacoff, 2015] 
#     and referencing "Package 'e1071', [Meyer, 2015] and [James, 2013].
# 
# Objective 2:
# 6.6: Comparing Logistic Regression (LR), RandomForest (RF) and Support 
#      Vector Machine (SVM) on synthetic 3D datasets, and visualizing 
#      parameters of each classifier using plot3d() function from the 
#      package rgl.
# 
# 6.0: Enter the main R Language documentation, and install packages
#      needed for classification.
# 6.1: Preparing data for classification examples. p. 391 [Kabacoff, 2015].
# 6.2.0: Examples on applications of the exponential function.
# 6.2.1: Plotting the logistic function.
# 6.2.2: Logistic regression example, p. 392 [Kabacoff, 2015].
# 6.3: Creating a decision tree, p. 394 [Kabacoff, 2015].
# 6.4: Random forest, p. 399 [Kabacoff, 2015].
# 6.5: Support vector machines, p. 401 [Kabacoff, 2015].
#
# 6.6: Comparing Logistic Regression (LR), RandomForest (RF) and Support 
#      Vector Machine (SVM) on synthetic datasets, and 3D plot 
#      visualizing kernel parameters of each classifier.
# 6.6.1: Logistic Regression (LR) for separating two classes.
# 6.6.2: Support Vector Machine (SVM) for separating two classes.
# 6.6.3: Random Forest (RF) for separating two classes.
#
########################################################################### 
#
#                            REFERENCES
#
# [Greenacre, 2013] Michael Greenacre, Raul Primicerio 
#   "Multivariate Analysis of Echological Data", 2013.
# [James, 2015] Gareth James, Daniela Witten, Trevor Hastie, 
#     Robert Tibshirani, "An Introduction to Statistical Learning with
#     Applications in R", Springer, 2015.
#     www-bcf.usc.edu/~gareth/ISL/ISLR%20First%20Printing.pdf
# [Kabacoff, 2015]  Robert I. Kabacoff, "R in Action", 
#     2'Ed, Manning Publications, 2015.
# [Kabacoff, 2015a] R.I. Kabacoff, "R in Action", Errata, 2015.
# [Meyer, 2019] David Meyer et al., "Package 'e1071',
#   https://cran.r-project.org/web/packages/e1071/e1071.pdf
#
############################################################################
# 6.0: Enter the main R Language documentation.
#      Install packages needed for examples on classification methods.
#
help.start()    # This is the main entry to the R project documentation.
opar.org <- par(no.readonly=TRUE)
# loc='C:/R_packages/'        # Use this folder for packages, if needed.
#
############################################################################## 
#                      Remove installed Packages before installing
packages1 <- c('utils','rpart','rpart.plot','randomForest','e1071','ggplot2','gridExtra','rgl')
remove.packages(packages1)
#
#############################################################################
#############################################################################
#                      Install packages
# Packages alphabetized: 
# e1071, ggplot2, gridExtra, randomForest, rgl, rpart, rpart.plot, utils. 
#
#install.packages("utils")         # Applied in 6.1, 
library(utils)
#install.packages("rpart")         # Applied in 6.3
library("rpart")
#install.packages("rpart.plot")    # Applied in 6.3
library("rpart.plot")
#install.packages("randomForest")  # Applied in 6.4
library("randomForest")
#install.packages("e1071")         # Applied in 6.5, 6.6
library("e1071")
#install.packages("ggplot2")       # Applied in 6.2.0, 6.2.1
library("ggplot2")
#install.packages("gridExtra")     # Applied in 6.2.0, 6.2.1, 6.2.2
library("gridExtra")
###
# sudo apt-get install libglu1-mesa-dev
# sudo apt install libfreetype6-dev
#install.packages("rgl")           # Applied in 6.6
library("rgl") 
#
####################################################################
# 6.1: Preparing data for classification examples. 
#      p. 391 [Kabacoff, 2015].
# Objective: Create a dataset for classifier training and testing a 
# classifier. Use approx 70% for training and the rest for testing.
#
#--------------------------------------------------------------------
#
# Use the sample() function for extracting rows for training and validation.
#
set.seed(1234)
train <- sample(nrow(dataset), 0.7*nrow(dataset)) # Use 70% of dataset for training.
dataset.train <- dataset[train,]      # Form training set.
dataset.validate <- dataset[-train,]  # Use rest of dataset (30%) for validation.
table(dataset.train$Revenue) 
table(dataset.validate$Revenue)
#

########################################################################
# 6.2.2: Logistic regression example, p. 392 p. [Kabacoff, 2015].
#   Logistic regression is here for binary output 
#     e.g. ("benign", "malignant").
#
fit.logit <- glm(Revenue~., 
                 data=dataset.train, 
                 family=binomial())  
summary(fit.logit) 
prob <- predict(fit.logit, dataset.validate, type="response") 
logit.pred <- factor(prob > .5, levels=c(FALSE, TRUE),  
                     labels=c("0", "1")) 
logit.perf <- table(dataset.validate$Revenue, logit.pred, 
                    dnn=c("Actual", "Predicted")) 
logit.perf 

# https://en.wikipedia.org/wiki/Confusion_matrix
actual = dataset.validate$Revenue
prediction = logit.pred

#https://www.journaldev.com/46732/confusion-matrix-in-r
#Insatll required packages
#install.packages('caret')
#install.packages('lattice')
#install.packages('ggplot2')

#Import required library
library(caret)

confusionMatrix(prediction, actual)
##############################################################################
# 6.3: Creating a decision tree, p. 394 [Kabacoff, 2015].
#
#-----------------------------------------------------------------------
# Functions used and their corresponding packages:
#   rpart() from from package rpart.
#   plotcp() from package rpart. 
#------------------------------------------------------------------------
#
set.seed(1234) 
dtree <- rpart(Revenue ~ ., data=dataset.train, method="class",       
               parms=list(split="information")) 
dtree$cptable 
plotcp(dtree)  # Plot x-validated error vers. complexity cp.
#
pdf(file="R_6_Fig_3_classification_Dec_Tree_No_1.pdf")  # Plot x-validated error vers. complexity cp.
plotcp(dtree)  # Plot x-validated error vers. complexity cp.
dev.off()
#
dtree.pruned <- prune(dtree, cp=.0125)  # prune the tree   
# 
# Plot the dicision tree, type=2: label all nodes.
prp(dtree.pruned, type = 2, extra = 104,   
    fallen.leaves = TRUE, main="Decision Tree") 
#
pdf(file="R_6_Fig_4_classification_Dec_Tree_No_2.pdf")  # Plot decision tree
prp(dtree.pruned, type = 2, extra = 104,   
    fallen.leaves = TRUE, main="Decision Tree") 
dev.off()
#
dtree.pred <- predict(dtree.pruned, dataset.validate, type="class") 
dtree.perf <- table(dataset.validate$Revenue, dtree.pred,  
                    dnn=c("Actual", "Predicted")) 
dtree.perf
# Save for later performance comparison: ???
save(dtree.perf, file="R_6_Fig_4a_perf_dtree.perf") 

actual = dataset.validate$Revenue
prediction = logit.pred
confusionMatrix(prediction, actual)

##############################################################################
# 6.4: Random forest, p. 399 [Kabacoff, 2015].
#-----------------------------------------------------------------------
# Functions used and their corresponding packages:
#   randomForest() from package randomForest.
#   importance() from package randomForest.
#   predict() from package stats.
#   table() from R base.
#   save() from R base.
#------------------------------------------------------------------------
#
set.seed(1234) 
fit.forest <- randomForest(Revenue~., data=dataset.train,         
                           na.action=na.roughfix, 
                           importance=TRUE)              
fit.forest 
importance(fit.forest, type=2) # Type = 2 minimize the total node impurity, ???
# measured by the Gini index, cf. p. 312 [James, 2013]. 
# A small Gini index means that a node mainly contains observation from 
#  one class.
forest.pred <- predict(fit.forest, dataset.validate)          
forest.perf <- table(dataset.validate$Revenue, forest.pred,  
                     dnn=c("Actual", "Predicted")) 
forest.perf 
# Save for later performance comparison:
save(forest.perf, file="R_6_Fig_4b_perf_forest.perf") #???
#
actual = dataset.validate$Revenue
prediction = logit.pred
confusionMatrix(prediction, actual)
#######################################################################
# 6.5: Support vector machines, p. 401 [Kabacoff, 2015]
#
#-----------------------------------------------------------------------
# Functions used and their corresponding packages:
#   paste() from package R base.
#   read.table from package utils.
#   factor() from package R base.
#   sample() from package R base.
#   set.seed() from  package R base.
#   svm() from package e1071.
#------------------------------------------------------------------------
#
# Prepare data, cf. [Kabacoff, 2015] p. 391
#
# [Kabacoff, 2015] p. 403 A support vector machine
#
set.seed(1234) 
fit.svm <- svm(Revenue~., data=dataset.train) 
fit.svm 
svm.pred <- predict(fit.svm, na.omit(dataset.validate)) 
svm.perf <- table(na.omit(dataset.validate$Revenue),
                  svm.pred, dnn=c("Actual", "Predicted")) 
svm.perf 

actual = dataset.validate$Revenue
prediction = logit.pred
confusionMatrix(prediction, actual)

### ??? How can I use this code below 
###########################################################################
# 6.6: Comparing the classifiers:
#         Logistic Regression, 
#         RandomForest (RF) and 
#         Support Vector Machine (SVM) 
#       on synthetic datasets, and use 3d plot for visualizing 
#       parameters from each classifier.
#-----------------------------------------------------------------------
# Functions used and their corresponding packages:
#   runif() from package stats
#   set.seed() from package R base.
#   rep() from package R base.
#   data.frame() from package R base.
#   plot3d() from package rgl.
#   rbind() from package R base.
#   seq() from package R base.
#   length() from package R base.
#   rep() from package R base.
#   factor() from package R base.
#   svm() from package e1091.
#
#----------------------------------------------------------------------
# Create 3D cluster, uniform distributed, augmented with color vector.
?runif() ???
N1 <- 100  # Use N1 samples in cluster 1 .
set.seed(24)
v1 <- runif(N1, min=-1.50, max=-0.50)   # Uniform centred in (-1,-1,-1)
v2 <- runif(N1, min=-1.50, max=-0.50)
v3 <- runif(N1, min=-1.50, max=-0.50)

class <- rep(1,N1)                     # Custer No. 1
df1 <- data.frame(v1,v2,v3,class)
xlim1 <- c(-4,4);ylim1 <- c(-4,4); zlim1 <- c(-4,4);
library(rgl)

plot3d(x=df1$v1, y=df1$v2, z=df1$v3, col=df1$class, 
       size=2, type='p', xlim=xlim1, ylim=ylim1, zlim=zlim1)

N2 <- 100  # Use N2 samples in cluster 2.
set.seed(24)
v1 <- rnorm(N2, mean=0, sd=1)   # Center in (0,0,0)
v2 <- rnorm(N2, mean=0, sd=1)
v3 <- rnorm(N2, mean=0, sd=1)

class <- rep(2,N2)                    # Class no. 2
df2 <- data.frame(v1,v2,v3,class)
xlim1 <- c(-4,4);ylim1 <- c(-4,4); zlim1 <- c(-4,4);
plot3d(df2$v1, df2$v2, df2$v3, col=df2$class, 
       size=2, type='p', xlim=xlim1, ylim=ylim1, zlim=zlim1)

# Combine the two clusters into a single dataframe df3:
df3 <- rbind(df1,df2)
plot3d(df3$v1, df3$v2, df3$v3, col=df3$class, 
       size=2, type='p', xlim=xlim1, ylim=ylim1, zlim=zlim1)
#
# Include x, y and z axes
#
xA <- seq(from = xlim1[1], to = xlim1[2], by = 0.05)
yA <- seq(from = ylim1[1], to = ylim1[2], by = 0.05)
zA <- seq(from = zlim1[1], to = zlim1[2], by = 0.05)
size_xA <- length(xA)
zeros_xA <- rep(0,size_xA)
xAxis <- c(xA,zeros_xA,zeros_xA)
yAxis <- c(zeros_xA,yA,zeros_xA)
zAxis <- c(zeros_xA,zeros_xA,zA)
colA <- rep(4,3*size_xA)
df4 <- data.frame(xAxis,yAxis,zAxis,colA)
plot3d(df4$xAxis, df4$yAxis, df4$zAxis, col=df4$colA, size=2, type='p', 
       main="One Uniform and one Gaussian cluster")

#----------------------------------------------------------------
# Generate training and validation set used for 
#        - Logistic Regression (LR) classifier
#        - Support Vector machine (SVM) classifier
#        - Random Forest (RF) classifier.
#
classQ <- factor(df3$class, levels=c(1,2))
dfQ <- data.frame(df3$v1, df3$v2, df3$v3, classQ)
#
# Generate training and test set from dfQ
set.seed(24)  # Improve reproducibility.
train <- sample(nrow(dfQ), 0.7*nrow(dfQ), replace=FALSE) # Use 70% of dataset for training.

dfQ.train <- dfQ[train,]      # Form training set data frame.
dfQ.validate <- dfQ[-train,]  # Use the rest of dataset (30%) for test.
table(dfQ.train$classQ) 
table(dfQ.validate$classQ)
#
# Visualize the training and validation set.
plot3d(dfQ.train$df3.v1, dfQ.train$df3.v2, dfQ.train$df3.v3, col=dfQ.train$classQ, 
       size=3, type='p', xlim=xlim1, ylim=ylim1, zlim=zlim1, 
       main="Training set Class 1 (B) and 2 (R)")

plot3d(dfQ.validate$df3.v1, dfQ.validate$df3.v2, dfQ.validate$df3.v3, col=dfQ.validate$classQ, 
       size=3, type='p', xlim=xlim1, ylim=ylim1, zlim=zlim1, 
       main="Validation set Class 1 (B) and 2 (R)")
#
# Include x,y and z axes on the validation set.
xA <- seq(from = xlim1[1], to = xlim1[2], by = 0.05)
yA <- seq(from = ylim1[1], to = ylim1[2], by = 0.05)
zA <- seq(from = zlim1[1], to = zlim1[2], by = 0.05)
size_xA <- length(xA)
zeros_xA <- rep(0,size_xA)
xAxis <- c(xA,zeros_xA,zeros_xA)
yAxis <- c(zeros_xA,yA,zeros_xA)
zAxis <- c(zeros_xA,zeros_xA,zA)
colA <- rep(4,3*size_xA)
df4 <- data.frame(xAxis,yAxis,zAxis,colA)

# Convert the class factor with two levels into numeric, thus
#  enabling more color levels.
dfQ.validate1 <- within(dfQ.validate, {
    classQ <- as.numeric(as.character(classQ))
})

names(df4)<-names(dfQ.validate1)  # Rename columns before rbind()

df5 <- rbind(dfQ.validate1,df4)
plot3d(df5$df3.v1, df5$df3.v2, df5$df3.v3, col=df5$classQ, size=3, type='p',
       main="Validation set Class 1 (B) and 2 (R)")
#
#---------------------------------------------------------------
# 6.6.1: Logistic Regression (LR) for separating two classes.
# 
fit.logit <- glm(classQ~., data=dfQ.train, family=binomial()) 
summary(fit.logit) 

prob <- predict(fit.logit, dfQ.validate, type="response") 
logit.pred <- factor(prob > .5, levels=c(FALSE, TRUE),  
                     labels=c("1", "2"))

logit.perf <- table(dfQ.validate$class, logit.pred, 
                    dnn=c("1", "2")) 
logit.perf

fit.logit$coefficients

w0 <- fit.logit$coefficients[1]
w1 <- fit.logit$coefficients[2]
w2 <- fit.logit$coefficients[3]
w3 <- fit.logit$coefficients[4]

# Generate v1 and v2 sequences for generating the separting plane
#   in logistic regression.
# xlim1 <- c(-4,4);ylim1 <- c(-4,4); zlim1 <- c(-4,4);
xyN <- 20    # Number of grid points along each axis.
xv1 <- seq(from=xlim1[1], to=xlim1[2],length.out=xyN)
yv2 <- seq(from=ylim1[1], to=ylim1[2],length.out=xyN)
zeros1 <- rep(0.0,xyN*xyN)
zv3 <- matrix(zeros1,nrow=xyN, ncol=xyN)

# Generate vectors for dataframe with points for separating plane.
dfplane.v1 <- rep(0,xyN*xyN) 
dfplane.v2 <- rep(0,xyN*xyN)
dfplane.v3 <- rep(0,xyN*xyN)
dfplane.col <- rep(4,xyN*xyN) # Set color value.

rowC <- 1   # Row count number in data point dataframe.

for (ii in 1:xyN) {
    for (jj in 1:xyN) {
        zv3[ii,jj] <- (-1)*(1/w3)*(w0+w1*xv1[ii]+w2*yv2[jj])
        dfplane.v1[rowC] <- xv1[ii]
        dfplane.v2[rowC] <- yv2[jj]
        dfplane.v3[rowC] <- zv3[ii,jj]
        rowC <- rowC + 1
    }
}

dfp <- data.frame(dfplane.v1,dfplane.v2,dfplane.v3,dfplane.col)

colnames(dfp) <- c("v1","v2","v3","class")

plot3d(dfp$v1, dfp$v2, dfp$v3, 
       col=dfp$class, size=3, type='p', xlim=xlim1, ylim=ylim1, zlim=zlim1,
       main="Logistic regression sep. plane")
#
# Combine the original data matrix with the separating plane dataset
# for visualization
#
dfT <- rbind(df3,dfp)
plot3d(dfT$v1, dfT$v2, dfT$v3, 
       col=dfT$class, size=3, type='p', xlim=xlim1, ylim=ylim1, zlim=zlim1,
       main="Logistic regression with separating plane")
# 
# Construct x, y and z axes for insertion into the dataframe with separating
#  plane.
#
x_4 <- seq(from = -4, to = 4, by = 0.05)
y_4 <- seq(from = -4, to = 4, by = 0.05)
z_4 <- seq(from = -4, to = 4, by = 0.05)
size_ax <- length(x_4)
zeros_ax <- rep(0,size_ax)
x4 <- c(x_4,zeros_ax,zeros_ax)
y4 <- c(zeros_ax,y_4,zeros_ax)
z4 <- c(zeros_ax,zeros_ax,z_4)
col_4 <- rep(4,3*size_ax)
df4 <- data.frame(x4,y4,z4,col_4)

# 
# Combine the dataframe representing:
#   dfT: original two class dataframe (df3) and 
#        separating plane (dfp),
#   df4: x, y, z axes
# into a single dataframe for visualization.
#
# Convert the class factor with two levels into numeric, thus
#  enabling more color levels.

names(df4)<-names(dfT)  # Rename columns before rbind()

df6 <- rbind(dfT,df4)
plot3d(df6$v1, df6$v2, df6$v3, col=df6$class, size=3, type='p',
       main="Two class dataset Class 1 (B) and 2 (R)")
# plot No 3
#----------------------------------------------------------------
# 6.6.2: Use Support Vector Machine (SVM) for separating two classes.
#
# Use the above training and validation set: 
#     dfQ.train and dfQ.validate
#
set.seed(1234)
# Use typ="C" for classification and 
#     kernel="radial" for radial basis function.
fit.svm1 <- svm(classQ~., data=dfQ.train, type="C", 
                kernel="radial") 
fit.svm1 
svm.pred1 <- predict(fit.svm1, na.omit(dfQ.validate))
#svm.pred1 <- predict(fit.svm1)
svm.perf1 <- table(na.omit(dfQ.validate)$class,  
                   svm.pred1, dnn=c("1", "2"))
svm.perf1 
#
# Get support vectors. Notice they are scaled and reindexed 
#   in relation to the dfQ.train dataset.
fit.svm1$SV   # Print support vectors in console.
SVindex <- as.integer(row.names(fit.svm1$SV))
SVindex
#
#---------------------------------------------------------------------------
# 6.6.3: Use Random Forest (RF) for separating two classes.
#
# Use the above training and validation set: 
#     dfQ.train and dfQ.validate
#
set.seed(1234) 
fit.forest <- randomForest(classQ~., data=dfQ.train,         
                           na.action=na.roughfix, 
                           importance=TRUE)              
fit.forest
#
# Get the row names as numbers of fit.forest$votes and the corresponding 
#   class probability votes for Class 1 (column 1) and Class 2 (column 2).
#
rowT <- as.numeric(rownames(fit.forest$votes))
colT <- as.numeric(colnames(fit.forest$votes))
fit.forest$votes[,]
#
plot(fit.forest$votes[,1], main="fit.forest$votes[,1], Class 1")
pdf(file="R_6_Fig_5_Classification_RF_votes_1.pdf")
plot(fit.forest$votes[,1], main="fit.forest$votes[,1], Class 1")
dev.off()

hist(fit.forest$votes[,1], breaks=40, main="fit.forest$votes[,1], Class 1")
pdf(file="Fig_R_6_Fig_6_Classification_RF_hvotes_1.pdf")
hist(fit.forest$votes[,1], breaks=40, main="fit.forest$votes[,1], Class 1")
dev.off()

plot(fit.forest$votes[,2], main="fit.forest$votes[,2], Class 2")
pdf(file="Fig_R_6_Fig_7_Classification_RF_votes_2.pdf")
plot(fit.forest$votes[,2], main="fit.forest$votes[,2], Class 2")
dev.off()

hist(fit.forest$votes[,2], breaks=40, main="fit.forest$votes[,2], Class 2")
pdf(file="Fig_R_6_Fig_8_Classification_RF_hvotes_2.pdf")
hist(fit.forest$votes[,2], breaks=40, main="fit.forest$votes[,2], Class 2")
dev.off()
#
importance(fit.forest, type=2) # Type = 2 minimize the total node impurity,
# measured by the Gini index, cf. p. 312 [James, 2013]. 
# A small Gini index means that a node mainly contains observation from 
#  one class.
#
forest.pred <- predict(fit.forest, dfQ.validate)
rowVPredict <- attributes(forest.pred)$class
rowVIndex <- as.numeric(attributes(forest.pred)$names)  # Row numbers.
#
# Print the randomForest classifier performance, as a table.
#
forest.perf <- table(dfQ.validate$classQ, forest.pred,  
                     dnn=c("Actual", "Predicted")) 
forest.perf 
# Save for later performance comparison:
save(forest.perf, file="R_6_Fig_9_perf_forest.perf") 
#
# Augment the dfQ.validate dataframe with one more column, forest.pred, 
#   with the forest prediction class numbers, either Class 1 or Class 2.
# Insert one more column, with the randomForest predictions.
dfQ.val.p <-cbind(dfQ.validate, forest.pred) 
rowsdfQ <- nrow(dfQ.val.p)      # Number of rows in dataframe.
#
# Then augment dfQ.val.p dataframe with yet another column named p.col
#   and assign the 4 color factors to this column. The colors as assigned
#   in the column p.col as follows:
#   If the prediction of Class 1 is TRUE then "blue".
#   If the prediction of Class 1 is FALSE then "cyan".
#   If the prediction of Class 2 is TRUE then "green".
#   If the prediction of Class 2 is FALSE then "red".
#
p.col <- factor(rep(c("blue","green","cyan","red"),length.out=rowsdfQ))

dfQ.val.p.col <- cbind(dfQ.val.p,p.col) # Insert p.col factors.
str(dfQ.val.p.col) # Check the structure.
#
# Define color values for the correctly predicted vectors:
colClass1 <- "blue"
colClass2 <- "green"  
# Define color values for erroneously predicted vectors:
colErrFromClass1_into_2 <- "cyan"  
colErrFromClass2_into_1 <- "red"
#
# Scan all rows in validation dataframe dfQ.val.p.col and encode the
#   prediction results into the dfQ.val.p$p.col according the 
#   above description:
#
for (rr in 1:rowsdfQ) {             # Scan all rows in dfQ.val.p.col
    if (dfQ.val.p.col[rr,4] != dfQ.val.p.col[rr,5]) {    
        print(rr)
        if (dfQ.val.p.col[rr,4] == 1) {
            dfQ.val.p.col[rr,6] <- colErrFromClass1_into_2
        } else {
            dfQ.val.p.col[rr,6] <- colErrFromClass2_into_1
        }
    } else {
        if (dfQ.val.p.col[rr,4] == 1) {
            dfQ.val.p.col[rr,6] <- colClass1
        } else {
            dfQ.val.p.col[rr,6] <- colClass2
        }
    }
}
#
plot3d(dfQ.val.p.col$df3.v1, dfQ.val.p.col$df3.v2, dfQ.val.p.col$df3.v3, 
       col=dfQ.val.p.col$p.col, 
       size=2, type='s', xlim=xlim1, ylim=ylim1, zlim=zlim1, 
       main="randomForest classification of validation set.",
       sub=" Blue: Class 1->1, Green: Class 2->2, Red: Class 2 -> 1")

##########################################################################
##########################################################################
##########################################################################
############################################################################
# 5.0: Enter the main R Language documentation.
#      Install packages needed for clustering.
#
help.start()    # This is the main entry to the R project documentation.
opar.org <- par(no.readonly=TRUE)  # Remember the initial IO parameter settings.
#
# Remove packages before installing
packages <- c('yaml','knitr','cluster','flexclust','NbClust','rattle')
packages1 <- c('rattle')
remove.packages(packages1)

# Now install packages
#install.packages("yaml")        # Convert between R and YAML (XML like language).
#install.packages("knitr")       # Dynamic Report Generation.
#install.packages("cluster")     # Ref. [Maechler, 2018]. 
#install.packages("NbClust")     # Ref. [Charrad, 2014].
#install.packages("flexclust")   # supply the dataset nutrient.
#install.packages("rattle", dependencies = T)
#install.packages("rattle")      # Include the wine dataset.
#install.packages("rattle", repos="http://rattle.togaware.com", type='source')
#
library(yaml)
library(knitr)
library(cluster)
library(NbClust)
library(flexclust)
library(rattle)
#
######################################################################
# 5.2: Step 2: Scale the data.
#      The data scaling is included in Step. 5.
#
######################################################################
# 5.3: Step 3: Screen for outliers.
#      This step is exemplified using ref. [Filzmoser, 2005].
#
# Ref. [Kabacoff, 2015] p. 371 
# screen and remove univariate  outliers using the package: outliers 
# install.packages("outliers")   # Moved to top of this script.
# install.packages("mvoutlier")  # Moved to top of this script.
# library(outliers)
# library(mvoutlier)
# help(package="outliers")    # Single variable outliers.
# help(package="mvoutliers")  # Multivariable outliers.
#
# Demonstrate the function symbol.plot(), ref. [Filzmoser, 2005].
# Objective: A method for multivariate outlier detection able 
#            to distinguish between extreme values of a normal 
#            distribution and values originating from a different 
#            distribution (outliers).
#            Method: Find subset of observations, h, with 
#                    MCD (Minimum Covariance Determinant).
# 
######################################################################
# 5.4: Step 4: Calculate distances.
#
# Compute the distance matrix between the first 4 rows of nutrient,
# using the dist() function in R base installation.
# dist() contains the following distances using ?dist()
#        "euclidean", "maximum", "manhattan", 
#        "canberra", "binary", "minkowski"
# 
?dist()                # Check manual.
d <- dist(nutrient)    # 
as.matrix(d)[1:4,1:4]  # Symmetric distance matrix, with 0 diagonal.
# Notice the very different number ranges
# of the variables.
# If mixed data types: binary, nomial (category), ordinal (eg. Likert) 
# and continous (e.g. temperature), use the daisy() function in the 
# cluster package.
# Example on packages for clustering on mixed types:
# Functions for agglomerative clustering: agnes()
# Functions for partitionng around medoids: pam().
######################################################################
# 5.5: Step 5: Select a clustering algorithm.
# Average-linkage clustering of neutrient data. 
# Listing 16.1 p. 375 [Kabacoff, 2015].
#
data(nutrient, package="flexclust")
?tolower()          # Check manual. 
row.names(nutrient) <- tolower(row.names(nutrient))
?scale()            # Check manual for the scaling function.
nutrient.scaled <- scale(nutrient) # Scale all variables to
#  mean = 0 and spread = 1.
# Check the scaled variables properties.
#    Mean values = 0
#    Standard deviations = 1.
#
attributes(nutrient.scaled)
summary(nutrient.scaled)           # Verify that mean values are = 0.
#
# Check that the standard deviations after scaling are = 1.
rows <- nrow(nutrient.scaled)         # Number of rows in data frame
energy <- nutrient.scaled[1:rows,1]   # energy vector
protein <- nutrient.scaled[1:rows,2]  # protein vector
fat <- nutrient.scaled[1:rows,3]      # fat vector
calcium <- nutrient.scaled[1:rows,4]  # calcium vector
iron <- nutrient.scaled[1:rows,5]     # iron vector
#
?sd()                                 # Manual for standard deviation.
sd_energy <- sd(energy); sd_energy    # sd = 1
sd_protein <- sd(protein); sd_protein # sd = 1
sd_fat <- sd(fat); sd_fat             # sc =1
sd_calcium <- sd(calcium); sd_calcium # sd = 1
sd_iron <- sd(iron); sd_iron # sd = 1
#
# Plot histograms of the normalized variables.
#
par(opar.org)
par(mfrow=c(2,3)) 
hist(energy, main="Scaled Energy")
hist(protein, main="Scaled Protein" )
hist(fat, main="Scaled Fat")
hist(calcium, main="Scaled Calcium")
hist(iron, main="Scaled Iron")
par(opar.org) 
#
pdf("fig_5_1_hist.pdf")
par(mfrow=c(2,3)) 
hist(energy, main="Scaled Energy")
hist(protein, main="Scaled Protein" )
hist(fat, main="Scaled Fat")
hist(calcium, main="Scaled Calcium")
hist(iron, main="Scaled Iron")
dev.off()
par(opar.org) 
#
str(nutrient.scaled)
#
# Display the euclidean distance matrix of 
#  nutrient.scaled
#
?dist()                                # Manual for dist()
d_eucli <- dist(nutrient.scaled,"euclidean")
d_eucli
#
attributes(d_eucli)
summary(d_eucli)                         # Check 
#
# Label: hclust() from R-package stats
par(opar.org)
fit.average_eucli <- hclust(d_eucli, method="average")
plot(fit.average_eucli, hang=-1, cex=0.8, 
     main="Average Linkage Clustering, Euclidean")
# hang=-1, the labels are below the plot, rotated 90 Degree.
# cex=.8, scale the text cx times, c.f. [Kabacoff, 2015] p. 53.
#
par(opar.org)
pdf("fig_5_2_AV_Link_Clust.pdf")
plot(fit.average_eucli, hang=-1, cex=0.8, 
     main="Average Linkage Clustering, Euclidean")
dev.off()
par(opar.org) 
#
# It is proposed to repeat for some other distance measures, and 
# compare the resulting clustering trees by plotting.
#####################################################################
# 5.6: Step 6: Obtain one or more cluster solutions.
# 5.7: Step 7: Determine the number of clusters present.
#
# Selecting the number of clusters, p.376 [Kabacoff, 2015]
par(opar.org)
nc <- NbClust(dataset, distance="euclidean", 
              min.nc=2, max.nc=15, method="average")
table(nc$Best.n[1,])
barplot(table(nc$Best.n[1,]),
        xlab="Number of clusters", ylab="Number of criteria",
        main="Number of Clusters Chosen among 26 Criteria.")
par(opar.org)
#
pdf("fig_5_3_Number_of_Clusters.pdf")
barplot(table(nc$Best.n[1,]),
        xlab="Number of clusters", ylab="Number of criteria",
        main="Number of Clusters Chosen among 26 Criteria.")
dev.off()
par(opar.org) 
######################################################################
# 5.8: Step 8: Obtain final clustering solution.
# 
# Listing 16.3 page 377 [Kabacoff, 2015]
#
?cutree()    # Manual for tree cutting, from R-package stats.
clusters <- cutree(fit.average_eucli, k=5) # Use k=5 groups as desired.
table(clusters)
aggregate(nutrient, by=list(cluster=clusters), median)
#
aggregate(as.data.frame(nutrient.scaled), by=list(cluster=clusters), 
          median)
par(opar.org)
#
######################################################################
# 5.9: Step 9 Visualize the results.
#
# Listing 16.3 page 377 [Kabacoff, 2015]
#
par(opar.org)     
plot(fit.average_eucli, hang=-1, cex=0.8,
     main="Average Linkage Clustering\n k=5 Cluster Solution.")
rect.hclust(fit.average_eucli, k=5) # Display the k=5 cluster solution.

par(opar.org)
pdf("fig_5_4_ALC_k_5_Clust.pdf")
plot(fit.average_eucli, hang=-1, cex=0.8,
     main="Average Linkage Clustering\n k=5 Cluster Solution.")
rect.hclust(fit.average_eucli, k=5) # Display rectangles around k=5 clusters.
dev.off()
par(opar.org)
#
######################################################################
# 5.10 The Cubic Cluster criterium.
#
# Plot the CCC (Cubic Cluster Criteria) from NbClust() for assessing
# the quality of the clustering, cf. [Kabacoff, 2015] page 387.
# If CCC is negative and decreasing for increasing number of clusters, 
# then the dataset has a tendency to unimodality.
# Ref. page 7 Equation 9 in [Charrad, 2014].
#
# nc refers to the nutrient dataset used above.
#
plot(nc$All.index[,4], type="o", main="nutrient dataset", ylab="CCC", 
     xlab="Number of clusters", col="blue")
# The reference nc$All.index[,4] refers to the CCC index.
pdf("fig_5_4_CCC_index_nutrient.pdf")
# then the dataset has a tendency to unimodality.
plot(nc$All.index[,4], type="o", main="nutrient dataset", ylab="CCC", 
     xlab="Number of clusters", col="blue")
dev.off()
par(opar.org)
######################################################################
# 5.30: Examples using the Partitioning Around Medoids (PAM) from
#       package cluster on wine dataset from package rattle.
#       Reference to the wine dataset  [Asuncion, 2007].
# Notice that the library(cluster) is moved to top of script.
set.seed(1234)
fit.pam_3 <- pam(wine[-1], k=3, stand=TRUE)
fit.pam_3$medoids
clusplot(fit.pam_3, main="Bivariate Cluster Plot, k=3")
par(opar.org)
#
pdf("fig_5_5_Partitioning_3.pdf")
clusplot(fit.pam_3, main="Bivariate Cluster Plot, k=3")
dev.off()
par(opar.org)
#
# Repeat for k=4
#
set.seed(1234)
fit.pam_4 <- pam(wine[-1], k=4, stand=TRUE)
fit.pam_4$medoids
clusplot(fit.pam_4, main="Bivariate Cluster Plot, k=4")
par(opar.org)
#
pdf("fig_5_6_Partitioning_4.pdf")
clusplot(fit.pam_4, main="Bivariate Cluster Plot, k=4")
dev.off()
par(opar.org)
#####################################################################


