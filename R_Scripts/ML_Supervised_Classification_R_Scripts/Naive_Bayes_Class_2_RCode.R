#-----------OLIVE OIL ADULTERATION---------------------------------------------

#Naive Bayes Model
#For classification of oils into different classes
#Set working directory
setwd("~/Derick Malavi_PhD Docs_UGent/Manuscript 3 EVOO/naive_bayes_R/DATA_A_Oil_Modified")
library(e1071)
library(caTools)
library(caret)
library(caret)
library(tidyr)
library(tidyverse)
library(readxl)
library(readr)
library(naivebayes)

### Import and check DATA-------------------------------------------------------
#Import data
HSIOil<-read.csv("MSC_SG_1D_A.csv")
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
##class_2 is EVOO and Adulterated olive oil (0:1, 2 groups) 
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
Y_train<-(HSIOil[,4])[sample]#subset from the class_factor(class_2)
Y_test<-(HSIOil[,4])[!sample]

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
colnames(data_train)[colnames(data_train) == "Y_train"] <- "class_2"
str(data_train$class_2)
# Assign custom labels
levels(data_train$class_2) <- c("Adulterants","Adulterated","EVOO")
levels(data_train$class_2) <- make.names(levels(data_train$class_2))
table(data_train$class_2)
#-------------------------------------------------------------------------------
#test data
data_test<-as.data.frame(cbind(X_test,Y_test))
data_test$Y_test<-as.factor(data_test$Y_test)
colnames(data_test)[colnames(data_test) == "Y_test"] <- "class_2"
# Assign custom labels
levels(data_test$class_2) <- c("Adulterants","Adulterated","EVOO")
levels(data_test$class_2) <- make.names(levels(data_test$class_2))
table(data_test$class_2)
#-------------------------------------------------------------------------------
set.seed (123)#For reproducibility

##Set parameters for running the naive_bayes cross-validation model
# Set up the training control(10 folds cross_validation)
control<-trainControl(method = "repeatedcv", number = 10, repeats =10)
                      
#Set the accuracy metric
metric<-"Accuracy"

# Set up the grid for tuning to find the optimal value of latent variables
grid <- expand.grid(laplace = seq(0.1, 0.3, by = 0.1),
                    usekernel = c(TRUE, FALSE),
                    adjust = seq(0.1, 0.3, by = 0.1))  

#Train the model using cross-validation (10 folds, 10 iterations)
fit_naive_bayes<-caret::train(y = data_train[,225], x = data_train[,-225], method = "naive_bayes", 
                      trControl = control, metric = metric, tuneGrid = grid)

print(fit_naive_bayes)#model summary
plot(fit_naive_bayes, col = 'red', pch = 1)

#Find the optimum parameters for naive_bayes classification model
print(fit_naive_bayes$bestTune)



#Save CV naive_bayes model results 
CC_k_class_2_train_MSC_SG_1D_A<-as.data.frame(fit_naive_bayes$results)
write.csv(CC_k_class_2_train_MSC_SG_1D_A, file = "CC_k_class_2_train_MSC_SG_1D_A.csv")

##Find important variables from naive_bayes model
impo_variable<-varImp(fit_naive_bayes)
ImptV<-(impo_variable$importance)
write.csv(ImptV, file = "important_variable_MSC_SG_1D_class_2.csv")

##Use the optimal model to predict the external samples
#predict the test set
test_class_2<-predict(fit_naive_bayes,newdata = data_test[,-225],y = data_test$class_2)

#Get the confusion matrix for testing data    
cf_class_2_test<-confusionMatrix(test_class_2,data_test$class_2)
print(cf_class_2_test)

##Save the testing confusion matrix matrix 
cf_class_2_test_MSC_SG_1D_A<-as.data.frame(cf_class_2_test$table)
write.csv(cf_class_2_test_MSC_SG_1D_A, 
          "cf_class_2_test_MSC_SG_1D_A.csv")

##Save the testing metrics
cf_class_2_test_metrics_MSC_SG_1D_A<-as.data.frame(cf_class_2_test$byClass)
write.csv(cf_class_2_test_metrics_MSC_SG_1D_A,file = "cf_class_2_test_metrics_MSC_SG_1D_A.csv")
#-----------------------THE END-------------------------------------------------
