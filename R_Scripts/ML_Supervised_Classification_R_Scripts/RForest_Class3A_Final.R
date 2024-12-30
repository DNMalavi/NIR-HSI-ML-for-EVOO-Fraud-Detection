
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
set.seed (123)#For reproducibility
#Train the initial Random Forest model(to get insights of the parameters)

rf_model_1<-randomForest(y = data_train[,225], x = data_train[,-225], 
                       proximity = TRUE)

print(rf_model_1) #Get an idea of OOB error and # of trees
#-------------------------------------------------------------------------------
#Visualize to check the errors
plot(rf_model_1,cex.axis = 0.8, cex.lab = 0.8,main = "")
legend_items<-colnames(rf_model_1$err.rate)
# Add a legend
legend("topright", legend = legend_items, 
       col = 1:length(legend_items), lty = 1, cex = 0.8)
text(x = 500, y = 0.19, labels = "(a)", col = "black", cex = 0.8)
#Important variables
imp_var<-as.data.frame(varImp(rf_model_1, sort = TRUE))
write.csv(imp_var, file = 'important_variable_MSC_SG_2D_A.csv')
varImpPlot(rf_model_1, col = 'blue', cex = 0.8)
#-------------------------------------------------------------------------------
##Tune the model using the using the RF Tune. To check on the effect on OOB error

rf_model_tune <- tuneRF(data_train[,-225], data_train[,225],
                        stepFactor = 0.5, improve = 0.05, 
                        plot = TRUE, ntreeTry = 500, trace = TRUE)
print(rf_model_tune)
##find the best mtry value from RF Tune
print(which.min(rf_model_tune[,2]))
#-------------------------------------------------------------------------------
##Check for optimum mtry and ntree combination values that give the lowest OOB error

results_list<-list()
ntree_values<-c(100,200,300,400,500,1000,1500,2000)
for (ntree_values in ntree_values){
rf_model_tune_2<- tuneRF(data_train[,-225], data_train[,225],
                         stepFactor = 0.5, improve = 0.05, 
                         plot = TRUE, ntreeTry = ntree_values, trace = TRUE)
  results_list[[as.character(ntree_values)]] <- rf_model_tune_2  
}
print(results_list)

# Convert the list to a data frame
results_df <- as.data.frame(do.call(rbind, results_list))
# Save the data frame as a .csv file
write.csv(results_df, "tuning_class_3_MSC_SG_2D_A.csv", row.names = FALSE)

#find the best n tree value from the data frame based on low OOB
best_ntree <- 100
best_mtry<- 7
#--------------------------------------------------------------------------------
##Perform cross-validation# Use the optimal values from RF Tune
#Set train control for 10 fold cross-validation
#This to refine the model further and avoid over fitting
# Create the train control
metric<-"Accuracy"
control <- trainControl(method = "repeatedcv", number = 10, repeats = 10, 
                        classProbs = TRUE, savePredictions = "final", 
                        summaryFunction = multiClassSummary)

##Train the Random forest model with cross-validation
rf_cv_model<-caret::train(class_3~., data = data_train, method = 'rf',metric = metric,
                   trControl = control, .mtry = best_mtry,.ntree = best_ntree)
##Check model CV model performance 
final_model<-rf_cv_model
print(final_model)
plot(final_model, col = "blue")
best_mtry<-final_model$bestTune
best_mtry#print
#-------------------------------------------------------------------------------
#Optimal parameters 
optimal_mtry <- 7
optimal_ntree <- 100
#-------------------------------------------------------------------------------
##Save the CV model results
CV_class_3_train_MSC_SG_2D_A <- as.data.frame(final_model$results)
CV_class_3_train_MSC_SG_2D_A
write.csv(CV_class_3_train_MSC_SG_2D_A, file = "CV_class_3_train_MSC_SG_2D_A.csv")
#-------------------------------------------------------------------------------
##Train the Random Forest model with optimal parameters
train_class_3<-predict(final_model,newdata = data_train[,-225],y = data_train$class_3)

#Get the confusion matrix for training data    
cf_class_3_train<-confusionMatrix(train_class_3,data_train$class_3)
print(cf_class_3_train)

##Save the training confusion  matrix 
cf_class_3_train_MSC_SG_2D_A<-as.data.frame(cf_class_3_train$table)
write.csv(cf_class_3_train_MSC_SG_2D_A, 
          "cf_class_3_train_MSC_SG_2D_A.csv")

##Save the training metrics
cf_class_3_train_metrics_MSC_SG_2D_A<-as.data.frame(cf_class_3_train$byClass)
write.csv(cf_class_3_train_metrics_MSC_SG_2D_A,file = "cf_class_3_train_metrics_MSC_SG_2D_A.csv")

#----END OF TRAINING AND CROSS-VALIDATION--------------------------------------

#-----------------#TESTING THE MODEL ON EXTERNAL DATA SET-----------------------
#predict the test set
test_class_3<-predict(final_model,newdata = data_test[,-225],y = data_test$class_3)

#Get the confusion matrix for testing data    
cf_class_3_test<-confusionMatrix(test_class_3,data_test$class_3)
print(cf_class_3_test)

##Save the testing confusion matrix matrix 
cf_class_3_test_MSC_SG_2D_A<-as.data.frame(cf_class_3_test$table)
write.csv(cf_class_3_test_MSC_SG_2D_A, 
          "cf_class_3_test_MSC_SG_2D_A.csv")

##Save the testing metrics
cf_class_3_test_metrics_MSC_SG_2D_A<-as.data.frame(cf_class_3_test$byClass)
write.csv(cf_class_3_test_metrics_MSC_SG_2D_A,file = "cf_class_3_test_metrics_MSC_SG_2D_A.csv")
#------------------------------------------------------------------------------
#Plot mtry and OOB Error
oob<-0.0007309942
mt<-7

mtry<-as.matrix(c(3,7,14,28,56))
OOBError<-as.matrix(c(0.0021929825,0.0007309942,0.0029239766,0.0014619883,0.0021929825
))
mtrydata<-cbind(mtry,OOBError)
View(mtrydata)
colnames(mtrydata)<-c("mtry","OOBError")
mtrydata<-as.data.frame(mtrydata,row.names = FALSE)
plot(mtrydata$mtry,mtrydata$OOBError, type = "b", 
     pch = 8, col = "blue", xlab = "mtry", ylab = "OOB Error",
     cex.axis = 0.8,cex.lab = 0.8, lty = 2)
text(x = 55, y = 0.0029, labels = "(b)", col = "black", cex = 0.8)
segments(x0 = mt, y0 = 0, x1 = mt, y1 = oob, col = "red", lty = 1)
segments(x0 = 0, y0 = oob, x1 = mt, y1 = oob, col = "red", lty = 1)






