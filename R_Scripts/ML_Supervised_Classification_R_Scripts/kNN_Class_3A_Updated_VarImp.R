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
##class3 is EVOO and Adulterated olive oil (0:1, 2 groups) 
##-----------------------------------------------------------------------------

#Get the spectra/predictors (X). The independent variables
#HSIFull<-as.matrix(HSIOil[,-c(1:9)])
HSIFull<-as.matrix(HSIOil[,c(24,39:42,82,92:94,101,104,122,128:130,167,181,182,186)])
class(HSIOil)
class(HSIFull)

#subset samples for cross-validation and external validation (70/30)
#Record 1 under Cal_Val variable is for calibration while 2 will be for testing the model
sample<-HSIOil$Cal_Val== 1 
##split the X test and train variables
X_train<-HSIFull[sample, ]
X_test<-HSIFull[!sample, ]
Y_train<-(HSIOil[,6])[sample]#subset from the class_factor
Y_test<-(HSIOil[,6])[!sample]

#Check length of the subsets for training and validation
dim(X_train)
dim(X_test)
length(Y_train)
length(Y_test)
#------------------------------------------------------------------------------
#set seed for reproducibility, set number of folds and iterations
set.seed (50)
times<-10
##Set parameters for running the kNN cross-validation model
# Set up the training control(10 folds cross_validation)

trControl <- trainControl(method = "repeatedcv", number = 10, 
                          repeats = 10,savePredictions = "final",summaryFunction = multiClassSummary)
##control<-trainControl(method = "repeatedcv",number = 10, repeats =10)
metric<-"Accuracy"

# Set up the grid for tuning to find the optimal value of k
grid <- expand.grid(.k=seq(1,30,by=1))

#Train the model
fit_knn<-caret::train(y = Y_train,x = X_train, method = "knn", 
               metric = metric, tuneGrid = grid, trControl = trControl)
                
print(fit_knn)
plot(fit_knn, col = 'red')

#Find the optimum number of k
print(fit_knn$bestTune)
ideal_k<-fit_knn$bestTune# to be used for the test set
optimal_model<-fit_knn$finalModel
impo_variable<-varImp(fit_knn)
impo_variable

#Save the Important Variables
impo_variables<-impo_variable$importance
write.csv(impo_variables, file = "impo_variable_MSC_SG_2D.csv")

#Save CV model results 
CC_k_class_3_train_MSC_SG_2D_A<-as.data.frame(fit_knn$results)
write.csv(CC_k_class_3_train_MSC_SG_2D_A, file = "CC_k_class-3_train_MSC_SG_2D_A.csv")
#--------------------END of TRAINING-------------------------------------------

#Test the model (External validation) with the optimal value k
test_model_class_3<-predict(fit_knn,newdata = X_test, y = Y_test)

#Get the confusion matrix for test data    
cf_class3_test<-confusionMatrix(test_model_class_3,Y_test)

#get and save metrics table for testing
test_metric_class_3_MSC_SG_2D_A<-as.data.frame(cf_class3_test$byClass)
write.csv(test_metric_class_3_MSC_SG_2D_A,file = "test_metric_class_3_MSC_SG_2D_A.csv")


#Get and save confusion matrix table for testing
cf_table_class_3_test_MSC_SG_2D_A<-as.data.frame(cf_class3_test$table)
cf_table_class_3_test_MSC_SG_2D_A<-write.csv(cf_table_class_3_test_MSC_SG_2D_A, 
                                       file = "cf_table_class_3_test_MSC_SG_2D_A.csv")
print(cf_class3_test)
#___________________________END OF CODE________________________________________


##Plotting 
##k-Neighbors 
fit_knn
k_data<-fit_knn$results[,c(1:2)]
k= 5
y_Acc = 0.9965639
plot(k_data$k,k_data$Accuracy, type = "b",col = "red",pch = 20,cex.axis = 0.8, 
     xlab = "k-Neighbors", ylab = "Accuracy (%)",cex.lab = 0.9)
# Add line from x-axis to the specific point
segments(x0 = k, y0 = 0, x1 = k, y1 = y_Acc, col = "black", lty = 2)

# Add line from y-axis to the specific point
segments(x0 = 0, y0 = y_Acc, x1 = k, y1 = y_Acc, col = "black", lty = 2)
text(x = 30, y = 0.9965639, labels = "(c)", col = "black", cex = 0.8)