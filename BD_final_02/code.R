# Importing the dataset
dataset <-  read.csv("online_shoppers_intention.csv")
##########################################################################
#https://machinelearningmastery.com/machine-learning-in-r-step-by-step/
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

##########################################################################
# Data Preprocessing Template

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

##########################################################################
### 11: " Intermediate graphs with R", [Kabacoff, 2020]
##################
# 11.3: Correlograms
# Give the corrgram file a name.
png(file = "corrgram.png")

install.packages('corrgram')
library(corrgram)
corrgram(dataset, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Correlogram of Online Shoppers Purchasing Intention intercorrelations")

# Save the file.
dev.off()

##################
#https://www.tutorialspoint.com/r/r_histograms.htm
# Pie chart
# Create data for the graph.
x <- summary(dataset$Month)
labels <-  c('Feb', 'Mar', 'May', 'June', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')

piepercent<- round(100*x/sum(x), 1)

# Give the chart file a name.
png(file = "percentage_legends.png")

# Plot the chart.
pie(x, labels = piepercent, main = "Month pie chart",col = rainbow(length(x)))
legend("topright", labels, cex = 0.8,
       fill = rainbow(length(x)))

# Save the file.
dev.off()

##################
# 11.1: Scatter plot
# Scatter plot with best fit lines 
x <- dataset$ProductRelated
y <- dataset$ProductRelated_Duration

# Give the scatter plot file a name.
png(file = "scatter_plot.png")

plot(dataset$ProductRelated, dataset$ProductRelated_Duration,
     main="Basic scatter plot of ProductRelated vs. Duration",
     xlab="Product related",
     ylab="Duration")
abline(lm(y~x), col="red", lwd=2, lty=1)
lines(lowess(x, y), col="blue", lwd=2, lty=2)

# Save the file.
dev.off()

# Scatter plot matrices 
# ??? which attributes to use 

# Give the scatter plot file a name.
png(file = "scatter_plot_matrices.png")

pairs(~dataset$SpecialDay+dataset$Month+
          dataset$TrafficType+dataset$VisitorType+dataset$Revenue,
      main="Basic Scatter Plot Matrix")

# Save the file.
dev.off()

##################
# 11.4: Mosaic plot
install.packages('vcd')
library(vcd)
TrafficType <- dataset$TrafficType
Weekend <- dataset$Weekend
Revenue <- dataset$Revenue

# Give the Mosaic plot file a name.
png(file = "mosaic.png")

mosaic(~TrafficType+Weekend++Revenue,
       data=dataset, shade=TRUE, legend=TRUE)

# Save the file.
dev.off()
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
#pdf("Fig_4_2_Scatter.pdf")
png("Fig_4_2_Scatter.png")
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

pdf("Fig_4_2_.pdf")
ggdendrogram(hc, rotate = TRUE, size = 2)
dev.off()
par(opar.org)
#####################################################################

####################################################################
# 4.25: "Data Visualization with R", [Kabacoff]
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
#pdf(file="R_6_Fig_3_classification_Dec_Tree_No_1.pdf")  # Plot x-validated error vers. complexity cp.
png(file="R_6_Fig_3_classification_Dec_Tree_No_1.png")  # Plot x-validated error vers. complexity cp.

plotcp(dtree)  # Plot x-validated error vers. complexity cp.
dev.off()
#
dtree.pruned <- prune(dtree, cp=.0125)  # prune the tree   
# 
# Plot the dicision tree, type=2: label all nodes.
prp(dtree.pruned, type = 2, extra = 104,   
    fallen.leaves = TRUE, main="Decision Tree") 
#
#pdf(file="R_6_Fig_4_classification_Dec_Tree_No_2.pdf")  # Plot decision tree
png(file="R_6_Fig_4_classification_Dec_Tree_No_2.png")  # Plot decision tree

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

###########################################################################
# 6.6: Comparing the classifiers:
#         Logistic Regression, 
#         RandomForest (RF) and 
#         Support Vector Machine (SVM) 
#       on synthetic datasets, and use 3d plot for visualizing 
#       parameters from each classifier.
#-----------------------------------------------------------------------



##########################################################################
##########################################################################
##########################################################################
############################################################################
#-------------------------------------------------------
# Clustering
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
