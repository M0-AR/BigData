dataset <-  read.csv("online_shoppers_intention.csv", sep=",")
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
#### Chapter 4, Table 4.5
### Page 84 , Table 4.5 
dataset$Month <- factor(dataset$Month)
dataset$VisitorType <- factor(dataset$VisitorType)
dataset$Weekend <- factor(dataset$Weekend)
dataset$Revenue <- factor(dataset$Revenue)
dataset$OperatingSystems <- factor(dataset$OperatingSystems)
dataset$Browser <- factor(dataset$Browser)
dataset$Region <- factor(dataset$Region)
dataset$TrafficType <- factor(dataset$TrafficType)
dataset$ProductRelated <- factor(dataset$ProductRelated)

# https://stackoverflow.com/questions/2288485/how-to-convert-a-data-frame-column-to-numeric-type
dataset$Administrative_Duration <-as.numeric(as.factor(dataset$Administrative_Duration))
dataset$Informational_Duration <-as.numeric(as.factor(dataset$Informational_Duration))
dataset$ProductRelated_Duration <-as.numeric(as.factor(dataset$ProductRelated_Duration))
dataset$PageValues <-as.numeric(as.factor(dataset$PageValues))
##########################################################################
#### EDA

#1. How many different 'Month' are there?
table(dataset$Month)   ### 10 months only
#2. Which is the most common 'Month'?   ###May

#3. How many special days are there in the data? (i.e. marked by  value  equal to 1)   #### 154 special days
library(dplyr)
special_days <- filter(dataset, SpecialDay == 1 )

#4.Which months has special days?    ### Feb and May
table(special_days$Month) 

special_days_notzero = filter(dataset, SpecialDay > 0 )
table(special_days_notzero$Revenue)
##########################################################################
##CHapter 6, section 6.1.1 [Kabacoff]
library(vcd)

counts_revenue <- table(dataset$Revenue)
counts_revenue

###section 6.1.2 [Kabacoff]
####################################
# Give the bar plot file a name.
png(file = "revenue_count_bar_plot.png")

# TIP 
plot(dataset$Revenue, main="Revenue count Bar Plot",
     xlab="Revenue", ylab="Frequency")
# Save the file.
dev.off()
####################################
counts_weekend <- table(dataset$Weekend)
counts_weekend

####################################
# Give the bar plot file a name.
png(file = "weekend_count_bar_plot.png")
#Page 121 TIP 
plot(dataset$Weekend, main="Weekend count Bar Plot",
     xlab="counts_weekend", ylab="Frequency")
# Save the file.
dev.off()
####################################

counts_rev_weekend <- table(dataset$Revenue, dataset$Weekend)
counts_rev_weekend


counts_product <- table(dataset$ProductRelated)
counts_product

####################################
# Give the bar plot file a name.
png(file = "product_count_bar_plot.png")
#Page 121 TIP 
plot(dataset$ProductRelated, main="Product count Bar Plot",
     xlab="Product", ylab="Frequency")
# Save the file.
dev.off()
####################################

## Page 151 , two way tables [Kabacoff]
mytable <- xtabs(~ Revenue+Weekend, data=dataset)
mytable

conversionrate_weekends = (499/(499+2369)) * 100
conversionrate_weekdays = (1409/(1409+8053)) * 100
conversionrate_weekends
conversionrate_weekdays

mytable <- xtabs(~ ProductRelated+Revenue, data=dataset)
mytable

mytable <- xtabs(~ SpecialDay+Revenue, data=dataset)
mytable


####section 6.5 [Kabacoff]
##Pg 126 
par(mfrow=c(1,1))
slices <- c(conversionrate_weekends, conversionrate_weekdays)
lbls <- c("weekends_conversion_percentage", "weekdays_conversion_percentage") 
####################################
# Give the pie chart file a name.
png(file = "pie_chart.png")
pie( slices, labels = lbls,
     main="Pie Chart")
# Save the file.
dev.off()
####################################

####################################
# Give the pie chart file a name.
png(file = "boxplot.png")
##section 6.5
### page 133 
boxplot(dataset$ExitRates , data=dataset,
        notch=TRUE,
        varwidth=TRUE,
        col="red",
        main="Exit rates box plot",
        xlab="Boxplot to find the median ",
        ylab="Exit rate distribution")

# Save the file.
dev.off()

#boxplot(dataset$ExitRates, main="Exit Rates Box plot", ylab="Exit Rates")
summary(dataset$ExitRates)   ####Median is 0.02 
##########################################################################
#### Logistic Regression   ##section 13.2
###page 318 

# Splitting the dataset into the Training set and Test set
#install.packages('caTools')
library(caTools)
set.seed(123)
## 70% of the sample size
smp_size <- floor(0.70 * nrow(dataset))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(dataset)), size = smp_size)

train <- dataset[train_ind, ]
test <- dataset[-train_ind, ]


fit.full <- glm(Revenue ~ .,
                data=train,family=binomial())


summary(fit.full)
##########################################################################

## Refer : https://stackoverflow.com/questions/22315394/factor-has-new-levels-error-for-variable-im-not-using 

fit.full$xlevels[["ProductRelated"]] <- union(fit.full$xlevels[["ProductRelated"]], levels(test$ProductRelated))
fit.full$xlevels[["Browser"]] <- union(fit.full$xlevels[["Browser"]], levels(test$Browser))
fit.full$xlevels[["TrafficType"]] <- union(fit.full$xlevels[["TrafficType"]], levels(test$TrafficType))

test$prob = predict(fit.full, newdata=test, type="response")

# Predicting the Test set results
y_pred = ifelse(test$prob > 0.5, FALSE, TRUE)

# Making the Confusion Matrix
cm_logistic_regression = table(test[, 18], y_pred )
cm_logistic_regression

# load Caret package for computing Confusion matrix
library(caret) 
confusionMatrix(cm_logistic_regression)

##########################################################################

#### KNN 
#https://dataaspirant.com/knn-implementation-r-using-caret-package/
library(caret)


trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3333)

knn_fit <- train(Revenue ~., data = train, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 10)

knn_fit

library(tidyverse)
dev.off()
plot(knn_fit)

test_pred <- predict(knn_fit, newdata = test)

confusionMatrix(test_pred, test$Revenue )
##########################################################################
#####Decision TRee
library(caTools)
set.seed(123)
## 70% of the sample size
smp_size <- floor(0.70 * nrow(dataset))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(dataset)), size = smp_size)

train <- dataset[train_ind, ]
test <- dataset[-train_ind, ]


library(rpart)
library(rpart.plot)
fit <- rpart(Revenue~., data = train, method = 'class')
rpart.plot(fit, extra = 106)

fit
predict_unseen <-predict(fit, test, type = 'class')

table_mat <- table(test$Revenue, predict_unseen)

confusionMatrix(table_mat)
##########################################################################
###### Hierarchical clustering
## https://www.datacamp.com/community/tutorials/hierarchical-clustering-R
seeds_df <- df
seeds_label <- seeds_df$type.of.seed
seeds_df$type.of.seed <- NULL
str(seeds_df)

seeds_df_sc <- as.data.frame(scale(seeds_df))
summary(seeds_df_sc)

dist_mat <- dist(seeds_df_sc, method = 'euclidean')

dist_mat
dev.off()

hclust_avg <- hclust(dist_mat, method = 'average')
plot(hclust_avg)

cut_avg <- cutree(hclust_avg, k = 2)

plot(hclust_avg)
rect.hclust(hclust_avg , k = 2, border = 2:6)
abline(h = 3, col = 'red')

#install.packages('dendextend', dependencies = TRUE)
suppressPackageStartupMessages(library(dendextend))
avg_dend_obj <- as.dendrogram(hclust_avg)
avg_col_dend <- color_branches(avg_dend_obj, h = 2)
plot(avg_col_dend)

suppressPackageStartupMessages(library(dplyr))
seeds_df_cl <- mutate(seeds_df, cluster = cut_avg)
count(seeds_df_cl,cluster)

suppressPackageStartupMessages(library(ggplot2))
ggplot(seeds_df_cl, aes(x=Administrative_Duration, y = ProductRelated_Duration, color = factor(cluster))) + geom_point()

seeds_df_cl$cluster[seeds_df_cl$cluster==1] <- "FALSE"
seeds_df_cl$cluster[seeds_df_cl$cluster==2] <- "TRUE"


table_hierarchical = table(seeds_df_cl$cluster,dataset$Revenue)
table_hierarchical
library(caret) 
confusionMatrix(table_hierarchical)
##########################################################################
##### SVM 
#https://www.datacamp.com/community/tutorials/support-vector-machines-r
library(e1071)
library(caTools)
set.seed(123)
## 70% of the sample size
smp_size <- floor(0.70 * nrow(dataset))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(dataset)), size = smp_size)

train <- dataset[train_ind, ]
test <- dataset[-train_ind, ]

svm_model <- svm(Revenue ~ ., data=train,
                 kernel="radial") #linear/polynomial/sigmoid
#test <- dataset[-train_ind, ]
pred = predict(svm_model,test)
tab = table(Predicted=pred, Actual = test$Revenue)
confusionMatrix(tab)


plot(svm_model, data=train,
     ExitRates~BounceRates)
##########################################################################
########  PCA
#data prep
library(dplyr)

reduced_dataset = dataset %>% select(Administrative, Administrative_Duration,Informational_Duration,  Browser, Region, TrafficType, ProductRelated, ProductRelated_Duration, PageValues, Month) 
sum(is.na(dataset))

library(mltools)
library(data.table)

newdata <- one_hot(as.data.table(reduced_dataset))
newdata = newdata %>% select(Administrative, Administrative_Duration, Browser_12, Browser_4 , Region_8, Region_2, TrafficType_8, ProductRelated_25, ProductRelated_Duration, PageValues,Month_Mar,Month_May) 


newdata$Revenue = dataset$Revenue

######## 
#https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_pca.html

library(ggfortify)
df <- newdata[,1:12]
pca_res <- prcomp(df, scale. = TRUE)

pc_all =data.frame(pca_res$x)
autoplot(pca_res)

autoplot(pca_res, data = newdata, colour = 'Revenue')
##########################################################################
#### K-means clustering
#https://www.datanovia.com/en/lessons/k-means-clustering-in-r-algorith-and-practical-examples/

clustering_data = dataset %>% select( Administrative, Administrative_Duration,Informational_Duration,  PageValues, Month) 
clustering_data <- one_hot(as.data.table(clustering_data))


df <- scale(clustering_data) # Scaling the data

# View the firt 3 rows of the data
head(df, n = 3)

#install.packages("factoextra")
library(factoextra)

# load required packages
library(factoextra)
library(NbClust)

# Elbow method
fviz_nbclust(df, kmeans, method = "wss") +
        geom_vline(xintercept = 4, linetype = 2) + # add line for better visualisation
        labs(subtitle = "Elbow method") # add subtitle



# Compute k-means with k = 2
set.seed(123)
km.res <- kmeans(df, 2, nstart = 25)

# Print the results
print(km.res)

aggregate(df, by=list(cluster=km.res$cluster), mean)

dd <- cbind(df, cluster = km.res$cluster)
head(dd)

# Cluster size
km.res$size


library(factoextra)

fviz_cluster(km.res, df, ellipse.type = "norm")

# Predicting the Test set results
y_pred = ifelse(km.res$cluster > 1,  FALSE ,  TRUE)

# Making the Confusion Matrix
cm_logistic_regression = table(dataset$Revenue, y_pred )
cm_logistic_regression

# load Caret package for computing Confusion matrix
library(caret) 
confusionMatrix(cm_logistic_regression)
