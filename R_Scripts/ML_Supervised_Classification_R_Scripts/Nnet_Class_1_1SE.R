#-----------OLIVE OIL ADULTERATION---------------------------------------------

#Artificial Neural Network
#For classification of oils into different classes
#"Seven Class" Classification
#------------------------------------------------------------------------------
#Set working directory
setwd("~/Derick Malavi_PhD Docs_UGent/Manuscript 3 EVOO/PLS_R/DATA_A_Oil_Modified")
#Load libraries
library(caret)
library(tidyr)
library(ggplot2)
library(dplyr)
library(readxl)
library(readr)
library(nnet)
library(janitor)
library(skimr)

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
levels(data_train$class_1) <- c("EVOO","EVOO_HZO","EVOO_Olive_Pomace",
                                "EVOO_Refined_Olive","Hazelnut_Oil",
                                "Olive_Pomace_Oil","Refined_Olive_Oil")
levels(data_train$class_1) <- make.names(levels(data_train$class_1))
table(data_train$class_1)
#-------------------------------------------------------------------------------
#test data
data_test<-as.data.frame(cbind(X_test,Y_test))
data_test$Y_test<-as.factor(data_test$Y_test)
colnames(data_test)[colnames(data_test) == "Y_test"] <- "class_1"
# Assign custom labels
levels(data_test$class_1) <- c("EVOO","EVOO_HZO","EVOO_Olive_Pomace",
                               "EVOO_Refined_Olive","Hazelnut_Oil",
                               "Olive_Pomace_Oil","Refined_Olive_Oil")
levels(data_test$class_1) <- make.names(levels(data_test$class_1))
table(data_test$class_1)
#-------------------------------------------------------------------------------
set.seed (123)#For reproducibility
#Reduction dimensionality to eliminate collinearity
pca_Preprocess<- preProcess(data_train, method = "pca")
data_trainPreprocessed <- predict(pca_Preprocess, data_train)
data_testPreprocessed <- predict(pca_Preprocess, data_test)

# Set up the training control(10 folds cross_validation)
control<-trainControl(method = "repeatedcv", number = 10, repeats =10,
                      savePredictions = "final",summaryFunction = multiClassSummary)

#Set the metric of  classification
metric<-"Accuracy"


# Set up the grid for tuning to find the optimal value of latent variables
grid <- expand.grid(laplace = seq(0.1, 0.3, by = 0.1),
                    usekernel = c(TRUE, FALSE),
                    adjust = seq(0,5, by = 1))  

#Train the model using cross-validation (10 folds, 10 iterations)

fit_naive_bayes<-caret::train(y = data_trainPreprocessed[,1], 
                              x = data_trainPreprocessed[,-1], method = "naive_bayes", 
                              trControl = control, metric = metric, tuneGrid = grid)

print(fit_naive_bayes)#model summary
plot(fit_naive_bayes, col = 'red', pch = 1)

#Save CV initial nnet model results 
CC_k_class_3_train_MSC_SG_2D_MSC_SG_2D<-as.data.frame(fit_naive_bayes$results)
write.csv(CC_k_class_3_train_MSC_SG_2D_MSC_SG_2D, file = "CC_k_class_1_train_MSC_SG_2D.csv")

#Find the optimum parameters for naive_bayes classification model
print(fit_naive_bayes$bestTune)

#Find the optimal model using one standard error rule (1SE)
# Finding the maximum accuracy and its standard deviation
results<-fit_naive_bayes$results
results[is.na(results)]<-0 #Remove NaN
max_accuracy <- max(results$Accuracy)
max_accuracy_std_dev <- results$AccuracySD[which.max(results$Accuracy)]
# Defining the target accuracy within one standard deviation of the best
target_accuracy <- max_accuracy - max_accuracy_std_dev

# Finding the simplest model that meets or exceeds the target accuracy
parsimonious_model_row <- which(results$Accuracy >= target_accuracy)[1]
parsimonious_model <- results[parsimonious_model_row, ]

# Print the details of the parsimonious model
print(paste("The most parsimonious model is with", parsimonious_model$laplace, "laplace", 
            parsimonious_model$usekernel,"usekernel","and",parsimonious_model$adjust,"as adjust"))
print(paste("Accuracy:", parsimonious_model$Accuracy))
print(paste("Standard Deviation:", parsimonious_model$AccuracySD))

#Rerun the model with the optimal values selected by the oneSE rule
optimal_laplace<-parsimonious_model$laplace
optimal_usekernel<-parsimonious_model$usekernel
optimal_adjust<-parsimonious_model$adjust
grid_1<-expand.grid(.laplace = optimal_laplace,.usekernel = optimal_usekernel,
                    .adjust=optimal_adjust)
grid_1

#Retrain the model with oneSE optimal factors
fit_naive_bayes_1<-caret::train(y = data_trainPreprocessed[,1], 
                                x = data_trainPreprocessed[,-1], method = "naive_bayes", 
                                trControl = control, metric = metric, tuneGrid = grid_1)
print(fit_naive_bayes_1)
#Save CV model results for the CV Model
CC_k_class_1_train_MSC_SG_2D<-as.data.frame(fit_naive_bayes_1$results)
write.csv(CC_k_class_1_train_MSC_SG_2D, file = "CC_k_class_1_train_MSC_SG_2D_A_1.csv")

#Important variables
varImpotance<-varImp(fit_naive_bayes)
varImpotance
plot(varImpotance)
#------------------------END OF TRAINING----------------------------------------
#Test/Prediction  on the External Validation Set

#Test the model (External validation) with the optimal value k
test_model_class_1<-predict(fit_naive_bayes_1,newdata = 
                              data_testPreprocessed[,-1], y = data_testPreprocessed[,1])

#Get the confusion matrix for test data    
cf_class_1_test<-confusionMatrix(test_model_class_1,data_testPreprocessed$class_1)
print(cf_class_1_test)
#Get and save metrics table for testing
test_metric_class_1_MSC_SG_2D_A<-as.data.frame(cf_class_1_test$byClass)
write.csv(test_metric_class_1_MSC_SG_2D_A,file = "test_metric_class_1_MSC_SG_2D_A.csv")

#Get and save confusion matrix table for testing
cf_table_class_1_test_MSC_SG_2D_A<-as.data.frame(cf_class_1_test$table)
cf_table_class_1_test_MSC_SG_2D_A<-write.csv(cf_table_class_1_test_MSC_SG_2D_A, 
                                             file = "cf_table_class_1_test_MSC_SG_2D_A.csv")
print(cf_class_1_test)

#Check for any misclassifcations 
#Save the observed and predicted csv file
check_misclas<-as.data.frame(cbind(data_test$class_1,test_model_class_1))
colnames(check_misclas)<-c("observed","predicted")
write.csv(check_misclas,file = "check_misclass_table_MSC_SG_2D.csv",row.names = FALSE)
Accuracy<-cf_class_1_test$overall["Accuracy"]
print(paste("The Accuracy of the model is ACC",Accuracy))
#-----------------------THE END-------------------------------------------------

