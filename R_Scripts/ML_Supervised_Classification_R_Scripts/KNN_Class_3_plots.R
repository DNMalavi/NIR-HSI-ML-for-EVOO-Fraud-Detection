#-----------OLIVE OIL ADULTERATION---------------------------------------------

#k-Nearest Neighbor (kNN)algorithm
#For classification of oils into different classes
#------------------------------------------------------------------------------
#Set the working directory
setwd("~/Derick Malavi_PhD Docs_UGent/Manuscript 3 EVOO/PLS_R/DATA_A_Oil_Modified")
library(class)
library(caret)
library(tidyr)
library(tidyverse)
library(readxl)
library(readr)

### Import and check DATA-------------------------------------------------------
#Import data
HSIOil<-read.csv("SNV_SG_1D_A.csv")
HSIOil<-as.data.frame(HSIOil)
head(HSIOil)
anyNA(HSIOil)#check any missing values
str(HSIOil)
dim(HSIOil)
colnames(HSIOil[c(1:9)])

#Change some variables to factors and numeric
HSIOil$class_1<-as.factor(HSIOil$class_1)
HSIOil$class_1_number<-as.numeric(HSIOil$class_1_number)
HSIOil$class_2<-as.factor(HSIOil$class_2)
HSIOil$class_2_number<-as.numeric(HSIOil$class_2_number)
HSIOil$class_3<-as.factor(HSIOil$class_3)
HSIOil$class_3_number<-as.numeric(HSIOil$class_3_number)
HSIOil$Cal_Val<-as.numeric(HSIOil$Cal_Val)
#Check whether the no. of samples match for the classes

summary(HSIOil[,c(2:7)])
##class1 is EVOO, HZO, PMO, RFOVO, EVOO+HZO, EVOO+PMO, EVOO+RFOVO (0-6: 7 groups)
##class2 is EVOO, Adulterant and Adulterated Olive oil(0-2: 3 groups)
##class_3 is EVOO and Adulterated olive oil (0:1, 2 groups) 
##------------------------------------------------------------------------------

#Get the spectra/predictors (X). The independent variables
HSIFull<-as.matrix(HSIOil[,-c(1:9)])
class(HSIOil)
class(HSIFull)

#subset samples for cross-validation and external validation (70/30)
#Record 1 under Cal_Val variable is for calibration while 2 will be for testing the model
sample<-HSIOil$Cal_Val== 1 
##split the X test and train variables
X_train<-HSIFull[sample, ]
X_test<-HSIFull[!sample, ]
Y_train<-(HSIOil[,6])[sample]#subset from the class_factor(class_3)
Y_test<-(HSIOil[,6])[!sample]

#Check length of the subsets for training and validation
dim(X_train)
dim(X_test)
length(Y_train)
length(Y_test)
#-------------------------------------------------------------------------------
##merge the columns
#train data
data_train<-as.data.frame(cbind(X_train,Y_train))
data_train$Y_train<-as.factor(data_train$Y_train)#convert Y to factor
colnames(data_train)[colnames(data_train) == "Y_train"] <- "class_3"
table(data_train$class_3)
str(data_train$class_3)
# Assign custom labels
levels(data_train$class_3) <- c("Adulterated","EVOO")
levels(data_train$class_3) <- make.names(levels(data_train$class_3))

#-------------------------------------------------------------------------------
#test data
data_test<-as.data.frame(cbind(X_test,Y_test))
data_test$Y_test<-as.factor(data_test$Y_test)
colnames(data_test)[colnames(data_test) == "Y_test"] <- "class_3"
# Assign custom labels
levels(data_test$class_3) <- c("Adulterated","EVOO")
levels(data_test$class_3) <- make.names(levels(data_test$class_3))
#-------------------------------------------------------------------------------
set.seed (123)#For reproducibility

##Set parameters for running the kNN cross-validation model
# Set up the training control(10 folds cross_validation)

trControl <- caret::trainControl(method = "repeatedcv", number = 10, 
                          repeats = 10,savePredictions = "final",summaryFunction = multiClassSummary)
##control<-trainControl(method = "repeatedcv",number = 10, repeats =10)
metric<-"Accuracy"

# Set up the grid for tuning to find the optimal value of k
grid <- expand.grid(.k=seq(1,30,by=1))

#Train the model using cross-validation (10 folds, 10 iterations)
fit_knn<-caret::train(y = data_train[,225],x = data_train[,-225], method = "knn", 
                      metric = metric, tuneGrid = grid, trControl = trControl)

print(fit_knn)#model summary
plot(fit_knn, col = 'red', pch = 1)

#Find the optimum number of k
print(fit_knn$bestTune)
ideal_k<-fit_knn$bestTune# to be used for the test set results

#Save CV model results 
CC_k_class_3_train_SNV_SG_0D_A<-as.data.frame(fit_knn$results)
write.csv(CC_k_class_3_train_SNV_SG_0D_A, file = "CC_k_class_3_train_SNV_SG_0D_A.csv")

##Find important variables from KNN model
impo_variable<-varImp(fit_knn)
ImptV<-(impo_variable$importance)
write.csv(ImptV, file = "important_variable_SNV_SG_0D_class_3.csv")

##Use the optimal model to predict the external samples
#predict the test set
test_class_3<-predict(fit_knn,newdata = data_test[,-225],y = data_test$class_3)

#Get the confusion matrix for testing data    
cf_class_3_test<-confusionMatrix(test_class_3,data_test$class_3)
print(cf_class_3_test)

##Save the testing confusion matrix matrix 
cf_class_3_test_SNV_SG_0D_A<-as.data.frame(cf_class_3_test$table)
write.csv(cf_class_3_test_SNV_SG_0D_A, 
          "cf_class_3_test_SNV_SG_0D_A.csv")

##Save the testing metrics
cf_class_3_test_metrics_SNV_SG_0D_A<-as.data.frame(cf_class_3_test$byClass)
write.csv(cf_class_3_test_metrics_SNV_SG_0D_A,file = "cf_class_3_test_metrics_SNV_SG_0D_A.csv")

#-----------------------THE END-------------------------------------------------

##Plots

# Plotting the confusion matrix
library(ggplot2)
ggplot(data = as.data.frame(cf_class_3_test$table), aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Freq), color = "black") +
  geom_text(aes(label = sprintf("%0.0f", Freq)), vjust = 1) +
  scale_fill_gradient(low = "white", high = "#663600", name = "Frequency") +
  theme_minimal() +
  labs(x = "Actual Class", y = "Predicted Class", color = 'black')+
  theme(legend.position =  "none")+
  annotate("text",x = 2.4, y = 2.4, label = "(b)", color = "black")
#------------------------------------------------------------------------------
##k-Neighbors 
fit_knn
k_data<-fit_knn$results
k= 1
y_Acc = 0.9983915
plot(k_data$k,k_data$Accuracy, type = "b",col = "red",pch = 20,cex.axis = 0.8, 
     xlab = "k-Neighbors", ylab = "Accuracy (%)",cex.lab = 0.9)
# Add line from x-axis to the specific point
segments(x0 = k, y0 = 0, x1 = k, y1 = y_Acc, col = "black", lty = 2)

# Add line from y-axis to the specific point
segments(x0 = 0, y0 = y_Acc, x1 = k, y1 = y_Acc, col = "black", lty = 2)
text(x = 30, y = 0.998, labels = "(c)", col = "black", cex = 0.8)

#ROC
# Assuming your test labels are in the 225th column and features in the other columns
test_labels <- data_test[, 225]
test_data <- data_test[, -225]

# Predict probabilities. Ensure that type = "prob" for probability predictions
prob_predictions <- predict(fit_knn, newdata = test_data, type = "prob")
prob
# Adjust the indexing ['class1'] based on your factor levels
positive_probs <- prob_predictions[, 'Adulterated']

roc_curve <- roc(test_labels, positive_probs)
auc_value <- auc(roc_curve)
print(paste("AUC:", auc_value))

plot(roc_curve, main="ROC Curve for kNN Model")
abline(a=0, b=1, lty=2, col="red")  # Diagonal reference line



library(pROC)

# Assuming your test labels are in the 225th column and features in the other columns
test_labels <- data_test[, 225]
test_data <- data_test[, -225]

# Ensure test_labels is a factor with correct levels
test_labels <- factor(test_labels, levels = c("EVOO", "Adulterated"))

# Predict probabilities. Ensure that type = "prob" for probability predictions
prob_predictions <- predict(fit_knn, newdata = test_data, type = "prob")

# Extract probabilities for the positive class ('Adulterated')
positive_probs <- prob_predictions[, 'Adulterated']

# Compute ROC curve and AUC
roc_curve <- roc(test_labels, positive_probs)
auc_value <- auc(roc_curve)
print(paste("AUC:", auc_value))

# Plot ROC curve
plot(roc_curve, main="ROC Curve for kNN Model")
abline(a=0, b=1, lty=2, col="red")  # Diagonal reference line

