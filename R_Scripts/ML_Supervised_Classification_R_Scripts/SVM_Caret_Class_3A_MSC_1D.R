#-----------OLIVE OIL ADULTERATION---------------------------------------------

#Support Vector Machines (SVM)

#For classification of oils into different classes

setwd("~/Derick Malavi_PhD Docs_UGent/Manuscript 3 EVOO/SVM_R/DATA_A_Oil_Modified")
library(caret)
library(tidyr)
library(tidyverse)
library(readxl)
library(readr)
library(e1071)
library(kernlab)

### Import and check DATA-------------------------------------------------------
#Import data
HSIOil<-read.csv("SNV_SG_2D_A.csv")
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

##Set parameters for running the SVM cross-validation model
# Set up the training control(10 folds cross_validation)
control<-trainControl(method = "repeatedcv", number = 10, repeats =10)

#metric<-"Accuracy"

# Set up the grid for tuning to find the optimal value of sigma and cost

sigma_values <- c(0.9, 0.8, 0.5, 0.3, 0.2, 0.1, 0.05, 0.02, 0.01)
C_values <- c(0.0001, 0.0005, 0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1, 5, 10, 25, 50, 100, 200)

# Create a grid of parameter values
grid<- expand.grid(.sigma = sigma_values,.C = C_values)


#Train the model using cross-validation (10 folds, 10 iterations)

fit_svm<-caret::train(y = data_train[,225], x = data_train[,-225], 
                      method = "svmRadial", trControl = control, tuneGrid = grid)

print(fit_svm)#model summary
plot(fit_SVM, pch = 1)

#Find the optimum number of sigma and cost from tuning
print(fit_svm$bestTune)
ideal_LVs<-fit_svm$bestTune# to be used for the test set results

#Find number of SVs
svm$finalModel

#Save CV SVM model results 
CC_k_class_3_train_SNV_SG_2D_A<-as.data.frame(fit_svm$results)
write.csv(CC_k_class_3_train_SNV_SG_2D_A, file = "CC_k_class_3_train_SNV_SG_2D_A.csv")

##Find important variables from SVM model
impo_variable<-varImp(fit_svm)
ImptV<-(impo_variable$importance)
write.csv(ImptV, file = "important_variable_SNV_SG_2D_class_3.csv")

#------------------END OF TRAINING AND CROSS-VALIDATION-------------------------

##Use the optimal model to predict the external samples
#predict the test set
test_class_3<-predict(fit_svm,newdata = data_test[,-225],y = data_test$class_3)

#Get the confusion matrix for testing data    
cf_class_3_test<-confusionMatrix(test_class_3,data_test$class_3)
print(cf_class_3_test)

##Save the testing confusion matrix matrix 
cf_class_3_test_SNV_SG_2D_A<-as.data.frame(cf_class_3_test$table)
write.csv(cf_class_3_test_SNV_SG_2D_A, 
          "cf_class_3_test_SNV_SG_2D_A.csv")

##Save the testing metrics
cf_class_3_test_metrics_SNV_SG_2D_A<-as.data.frame(cf_class_3_test$byClass)
write.csv(cf_class_3_test_metrics_SNV_SG_2D_A,file = "cf_class_3_test_metrics_SNV_SG_2D_A.csv")
#-----------------------THE END-------------------------------------------------

