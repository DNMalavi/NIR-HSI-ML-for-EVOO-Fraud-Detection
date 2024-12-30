
#-----------OLIVE OIL ADULTERATION---------------------------------------------
             #Random Forest Algorithm#
#For classification of oils into different classes
#------------------------------------------------------------------------------

#Set the working directory
setwd("~/Derick Malavi_PhD Docs_UGent/Manuscript 3 EVOO/PLS_R/DATA_A_Oil_Modified")

##Load packages-----------------------------------------------------------------
library(randomForest)
library(caret)
library(readr)
library(readxl)
library(tidyverse)
library(MLmetrics)
library(MLeval)
install.packages(c("foreach","parallel","doParallel"))
library(parallel)
library(doParallel)
library(foreach)
#detectCores()
#registerDoParallel(5)#speed up the modelling process

start_time<-Sys.time()#Initial time 
### Import and check DATA-------------------------------------------------------
#Import data
HSIOil<-read.csv("MSC_SG_2D_A.csv")
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
#Set seed for reproducible results
set.seed(123)

##Perform cross-validation
#Set train control for 10 fold cross-validation repeated 10 times
#This to refine the model further and avoid over fitting. Here we will with a constant 
#of 500 trees but find the optimal value of mtry through a grid search based on
#sqrt of the number of variables (224)----> random search will be used

# Create the train control
metric<-"Accuracy"
#Use oneSE to select the best model
control <- trainControl(method = "repeatedcv", number = 10, repeats = 10, 
                        classProbs = TRUE, savePredictions = "final", 
                        summaryFunction = multiClassSummary,
                        search = "random",selectionFunction = "oneSE")

#We are using oneSE rule to select the simplest model-->parsimony

##Train the Random forest model with cross-validation
rf_cv_model<-caret::train(class_3~., data = data_train, method = 'rf',metric = metric,
                   trControl = control, tuneLength = 30)
##Check model CV model performance 
final_model<-rf_cv_model
print(final_model)
plot(final_model, col = "red")
best_mtry<-final_model$bestTune
best_mtry#print
print(paste("The optiml value for mtry as per oneSE rule is", best_mtry))
#-------------------------------------------------------------------------------
##Save the CV model results
CV_class_3_train_MSC_SG_2D_A <- as.data.frame(final_model$results)
CV_class_3_train_MSC_SG_2D_A
write.csv(CV_class_3_train_MSC_SG_2D_A, file = "CV_class_3_train_MSC_SG_2D_A.csv")
#-------------------------------------------------------------------------------
#Save important variables 
Impo_v<-varImp(rf_cv_model$finalModel, scale =FALSE)
write.csv(Impo_v,file = "Impo_variable_MSC_SG_2D_A.csv")
#-------------------------------------------------------------------------------
#Plot the important variables
varImpPlot(rf_cv_model$finalModel, color = "red", pch = 18, main = "Variable Importance")

#Save OOB Errors
oob_error<-rf_cv_model$finalModel$err.rate
write.csv(oob_error,file = "OOB_Error_MSC_SG_2D_A.csv",row.names = FALSE)
#-------------------------------------------------------------------------------
#Visualization of OOB Errors
#Use the optimal mtry values to run random Forest and plot OOB Error

rf_model_2<-randomForest(y = data_train[,225], x = data_train[,-225], 
                         proximity = TRUE, mtry = best_mtry$mtry, ntree = 500)

print(rf_model_2) #Get an idea of OOB error and # of trees
#-------------------------------------------------------------------------------
#Visualize to check the errors
plot(rf_model_2,cex.axis = 0.8, cex.lab = 0.8,main = "")
legend_items<-colnames(rf_model_2$err.rate)
# Add a legend
legend("topright", legend = legend_items, 
       col = 1:length(legend_items), lty = 1, cex = 0.8)
text(x = 500, y = 0.19, labels = "(a)", col = "black", cex = 0.8)
#-------------------------------------------------------------------------------
#Test/Prediction  on the External Validation Set
#Test the model (External validation) with the optimal values
test_model_class_3<-predict(rf_cv_model,newdata = data_test, y = Y_test)

#Get the confusion matrix for test data    
cf_class_3_test<-confusionMatrix(test_model_class_3,data_test$class_3)
print(cf_class_3_test)
#Get and save metrics table for test set
test_metric_class_3_MSC_SG_2D_A<-as.data.frame(cf_class_3_test$byClass)
write.csv(test_metric_class_3_MSC_SG_2D_A,file = "test_metric_class_3_MSC_SG_2D_A.csv")

#Get and save confusion matrix table for testing
cf_table_class_3_test_MSC_SG_2D_A<-as.data.frame(cf_class_3_test$table)
cf_table_class_3_test_MSC_SG_2D_A<-write.csv(cf_table_class_3_test_MSC_SG_2D_A, 
                                             file = "cf_table_class_3_test_MSC_SG_2D_A.csv")
print(cf_class_3_test)

#Check for any wrong classifications 
#Save the observed and predicted csv file
check_misclas<-as.data.frame(cbind(data_test$class_3,test_model_class_3))
colnames(check_misclas)<-c("observed","predicted")
write.csv(check_misclas,file = "check_misclass_table_MSC_SG_2D.csv",row.names = FALSE)
#-------------------------------------------------------------------------------
#Calculating Matthews Correlation Coefficient (MC)
# Extracting the components of the confusion matrix
confusion_matrix <- cf_class_3_test$table
TP <- confusion_matrix[1,1] 
TN <- confusion_matrix[2,2] 
FP <- confusion_matrix[2,1] 
FN <- confusion_matrix[1,2] 

# Calculating MCC
MCC <- (TP * TN - FP * FN) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))

# Printing the MCC value
print(paste("MCC",MCC))

#Extract the accuracy

Accuracy<-cf_class_3_test$overall["Accuracy"]
print(paste("The ACCURACY of the prediction test set is","ACC", Accuracy))

end_time<-Sys.time()
runtime<-end_time-start_time
print(paste("The total run time is",round(runtime,1),"minutes"))
#-----------------------THE END-------------------------------------------------
