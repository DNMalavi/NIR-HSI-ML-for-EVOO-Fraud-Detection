
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
library(themis)
library(ROSE)
library(parallel)
library(doParallel)
library(foreach)
detectCores()
registerDoParallel(5)#speed up the modelling process

start_time<-Sys.time()#Initial time 
### Import and check DATA-------------------------------------------------------
#Import data
HSIOil<-read.csv("SG_0D_A.csv")
HSIOil<-as.data.frame(HSIOil)
head(HSIOil)
anyNA(HSIOil)#check any missing values
str(HSIOil)
dim(HSIOil)
colnames(HSIOil[c(1:9)])

#Change some variables to factors and numeric
HSIOil$class_1<-as.factor(HSIOil$class_1)
HSIOil$class_2_number<-as.numeric(HSIOil$class_2_number)
HSIOil$class_3<-as.factor(HSIOil$class_3)
HSIOil$class_1_number<-as.numeric(HSIOil$class_1_number)
HSIOil$class_2<-as.factor(HSIOil$class_2)
HSIOil$class_3_number<-as.numeric(HSIOil$class_3_number)
HSIOil$Cal_Val<-as.numeric(HSIOil$Cal_Val)
#Check whether the no. of samples match for the classes

summary(HSIOil[,c(2:7)])
##class1 is EVOO, HZO, PMO, RFOVO, EVOO+HZO, EVOO+PMO, EVOO+RFOVO (0-6: 7 groups)
##class2 is EVOO, Adulterant and Adulterated Olive oil(0-2: 3 groups)
##class_1 is EVOO and Adulterated olive oil (0:1, 2 groups) 
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
Y_train<-(HSIOil[,2])[sample]#subset from the class_factor(class_1)
Y_test<-(HSIOil[,2])[!sample]

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
colnames(data_train)[colnames(data_train) == "Y_train"] <- "class_1"
table(data_train$class_1)
str(data_train$class_1)
# Assign custom labels
levels(data_train$class_1) <- c("EVOO","EVOO_HZO","EVOO_POO",
                                "EVOO_ROO","HZO",
                                "POO","ROO")
levels(data_train$class_1) <- make.names(levels(data_train$class_1))
table(data_train$class_1)
#-------------------------------------------------------------------------------
#test data
data_test<-as.data.frame(cbind(X_test,Y_test))
data_test$Y_test<-as.factor(data_test$Y_test)
colnames(data_test)[colnames(data_test) == "Y_test"] <- "class_1"
# Assign custom labels
levels(data_test$class_1) <- c("EVOO","EVOO_HZO","EVOO_POO",
                               "EVOO_ROO","HZO",
                               "POO","ROO")
levels(data_test$class_1) <- make.names(levels(data_test$class_1))
table(data_test$class_1)
#------------------------------------------------------------------------------
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
rf_cv_model<-caret::train(class_1~., data = data_train, method = 'rf',metric = metric,
                   trControl = control, tuneLength = 10)
##Check model CV model performance 
final_model<-rf_cv_model
print(final_model)
plot(final_model, col = "red")
best_mtry<-final_model$bestTune              
best_mtry#print
print(paste("The optimal value for mtry as per oneSE rule is", best_mtry))
#-------------------------------------------------------------------------------
##Save the CV model results
CV_class_1_train_MSC_SG_2D <- as.data.frame(final_model$results)
CV_class_1_train_MSC_SG_2D
write.csv(CV_class_1_train_MSC_SG_2D, file = "CV_class_1_train_MSC_SG_2D.csv")
#-------------------------------------------------------------------------------
#Save important variables 
Impo_v<-varImp(rf_cv_model$finalModel, scale =FALSE)
write.csv(Impo_v,file = "Impo_variable_MSC_SG_2D.csv")
#-------------------------------------------------------------------------------
#Plot the important variables
varImpPlot(rf_cv_model$finalModel, color = "red", pch = 18, main = "Variable Importance")

#Save OOB Errors
oob_error<-rf_cv_model$finalModel$err.rate
write.csv(oob_error,file = "OOB_Error_MSC_SG_2D.csv",row.names = FALSE)
#-------------------------------------------------------------------------------
#Test/Prediction  on the External Validation Set
#Test the model (External validation) with the optimal values
test_model_class_1<-predict(rf_cv_model,newdata = data_test, y = Y_test)

#Get the confusion matrix for test data    
cf_class_1_test<-confusionMatrix(test_model_class_1,data_test$class_1)
print(cf_class_1_test)
#Get and save metrics table for test set
test_metric_class_1_MSC_SG_2D<-as.data.frame(cf_class_1_test$byClass)
write.csv(test_metric_class_1_MSC_SG_2D,file = "test_metric_class_1_MSC_SG_2D.csv")

#Get and save confusion matrix table for testing
cf_table_class_1_test_MSC_SG_2D<-as.data.frame(cf_class_1_test$table)
cf_table_class_1_test_MSC_SG_2D<-write.csv(cf_table_class_1_test_MSC_SG_2D, 
                                             file = "cf_table_class_1_test_MSC_SG_2D.csv")
print(cf_class_1_test)

#Check for any wrong classifications 
#Save the observed and predicted csv file
check_misclas<-as.data.frame(cbind(data_test$class_1,test_model_class_1))
colnames(check_misclas)<-c("observed","predicted")
write.csv(check_misclas,file = "check_misclass_table_MSC_SG_2D.csv",row.names = FALSE)
#-------------------------------------------------------------------------------
#Extract the accuracy
Accuracy<-cf_class_1_test$overall["Accuracy"]
print(paste("The ACCURACY of the prediction test set is","ACC", Accuracy))
#Calculate the run time for training the model
end_time<-Sys.time()
runtime<-end_time-start_time
print(paste("The total run time is",round(runtime,1),"minutes"))
#-----------------------THE END-------------------------------------------------

ggplot(data = as.data.frame(cf_class_1_test$table), aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Freq), color = "black") +
  geom_text(aes(label = sprintf("%0.0f", Freq)), vjust = 1,color = "black",size = 4) +
  scale_fill_gradient(low = "cornsilk", high = "#556b2f", name = "Frequency") +
  theme_minimal() +
  labs(x = "Actual Class", y = "Predicted Class", color = 'black')+
  theme(legend.position =  "none")+
  annotate("text",x = 7.3, y = 7.3, label = "(c)", color = "black")+theme(
    axis.text.x = element_text(color = "black",size = 12,angle = 60,hjust = 1))+
  theme(
    axis.text.y = element_text(color = "black",size = 12),
    axis.title.x  = element_text(size = 12),
    axis.title.y = element_text(size = 12),aspect.ratio = 1)

