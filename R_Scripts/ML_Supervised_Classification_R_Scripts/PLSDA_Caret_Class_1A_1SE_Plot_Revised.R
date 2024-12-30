#-----------OLIVE OIL ADULTERATION---------------------------------------------

#Partial Least Squares Discriminant Analysis (PLSDA)
#For classification of oils into different classes
#Set working directory
setwd("~/Derick Malavi_PhD Docs_UGent/Manuscript 3 EVOO/PLS_R/DATA_A_Oil_Modified")
library(caret)
library(tidyr)
library(tidyverse)
library(readxl)
library(readr)

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
#-------------------------------------------------------------------------------
set.seed (123)#For reproducibility

##Set parameters for running the PLS cross-validation model
# Set up the training control(10 folds cross_validation)
control<-trainControl(method = "repeatedcv", number = 10, repeats =10)

#metric<-"Accuracy"

# Set up the grid for tuning to find the optimal value of latent variables
grid <- expand.grid(ncomp=seq(1,100,by=1))

#Train the model using cross-validation (10 folds, 10 iterations)
fit_pls<-caret::train(y = data_train[,225], x = data_train[,-225], method = "pls", 
                       tuneGrid = grid, trControl = control)

print(fit_pls)#model summary
plot(fit_pls, col = 'red', pch = 1)

#Find the optimum number of latent variables (LVs)
print(fit_pls$bestTune)
ideal_LVs<-fit_pls$bestTune# to be used for the test set results

# Finding the maximum accuracy and its standard deviation
results<-fit_pls$results
max_accuracy <- max(results$Accuracy)
max_accuracy_std_dev <- results$AccuracySD[which.max(results$Accuracy)]

# Defining the target accuracy within one standard deviation of the best
target_accuracy <- max_accuracy - max_accuracy_std_dev

# Finding the simplest model that meets or exceeds the target accuracy
parsimonious_model_row <- which(results$Accuracy >= target_accuracy)[1]
parsimonious_model <- results[parsimonious_model_row, ]

# Print the details of the parsimonious model
print(paste("The most parsimonious model is with", parsimonious_model$ncomp, "latent variables (LVs)."))
print(paste("Accuracy:", parsimonious_model$Accuracy))
print(paste("Standard Deviation:", parsimonious_model$AccuracySD))

# Saving optimal LVs for prediction
ideal_LVs <- parsimonious_model$ncomp
#-------------------------------------------------------------------------------
#Save CV PLS model results 
CC_k_class_1_train_SG_0D_A<-as.data.frame(fit_pls$results)
write.csv(CC_k_class_1_train_SG_0D_A, file = "CC_k_class_1_train_SG_0D_A.csv")

##Find important variables from PLS model
impo_variable<-varImp(fit_pls)
ImptV<-(impo_variable$importance)
write.csv(ImptV, file = "important_variable_SG_0D_class_1.csv")

##Use the optimal model to predict the external samples
#predict the test set
test_class_1<-predict(fit_pls,newdata = data_test[,-225],y = data_test$class_1,ncomp=ideal_LVs)

#Get the confusion matrix for testing data    
cf_class_1_test<-confusionMatrix(test_class_1,data_test$class_1)
print(cf_class_1_test)

##Save the testing confusion matrix matrix 
cf_class_1_test_SG_0D_A<-as.data.frame(cf_class_1_test$table)
write.csv(cf_class_1_test_SG_0D_A, 
          "cf_class_1_test_SG_0D_A.csv")

##Save the testing metrics
cf_class_1_test_metrics_SG_0D_A<-as.data.frame(cf_class_1_test$byClass)
write.csv(cf_class_1_test_metrics_SG_0D_A,file = "cf_class_1_test_metrics_SG_0D_A.csv")
#-------------------------------------------------------------------------------
#Save the observed and predicted csv file
check_misclas<-as.data.frame(cbind(data_test$class_1,test_class_1))
colnames(check_misclas)<-c("observed","predicted")
misc_columns<-HSIOil %>% select(class_1, adulteration_level,Cal_Val) %>% filter(Cal_Val == 2)
check_class<-cbind(misc_columns,check_misclas)
write.csv(check_class,file = "check_misclass_table_SG_0D.csv",row.names = FALSE)
#-----------------------THE END OF TRAINING AND VALIDATION-------------------------------------------------

#Visualization

pls<-fit_pls$results
x_Lvs= 30
y_Acc = 0.946376185950157
x2_Lvs<-99
y2_Acc<-0.9618090
plot(pls$ncomp, pls$Accuracy, type = "b",ylab = "Accuracy (%)", xlab = "Latent Variables (LVs)", 
     cex.axis = 0.9,col = "grey", pch = 4, cex.lab = 0.9)

# Add line from x-axis to the specific point
segments(x0 = x_Lvs, y0 = 0, x1 = x_Lvs, y1 = y_Acc, col = "black", lty = 2)
segments(x0 = x2_Lvs, y0 = 0, x1 = x2_Lvs, y1 = y2_Acc, col = "red", lty = 2)

# Add line from y-axis to the specific point
segments(x0 = 0, y0 = y_Acc, x1 = x_Lvs, y1 = y_Acc, col = "black", lty = 2)
segments(x0 = 0, y0 = y2_Acc, x1 = x2_Lvs, y1 = y2_Acc, col = "red", lty = 2)
text(x = 95, y = 0.935, labels = "(b)", col = "black", cex = 0.9)
#------------------------------------------------------------------------------

# Plotting the confusion matrix
library(ggplot2)
ggplot(data = as.data.frame(cf_class_1_test$table), aes(x = Reference, y = Prediction)) +
  geom_tile(aes(fill = Freq), color = "black") +
  geom_text(aes(label = sprintf("%0.0f", Freq)), vjust = 1,color = "black",size = 4) +
  scale_fill_gradient(low = "white", high = "#C1FFC1", name = "Frequency") +
  theme_minimal() +
  labs(x = "Actual Class", y = "Predicted Class", color = 'black')+
  theme(legend.position =  "none")+
  annotate("text",x = 7.2, y = 7.2, label = "(d)", size = 4, color = "black")+theme(
    axis.text.x = element_text(color = "black",size = 10,angle = 90,hjust = 1))+
  theme(
    axis.text.y = element_text(color = "black",size = 10),
    axis.title.x  = element_text(size = 10),
    axis.title.y = element_text(size = 10),aspect.ratio = 1)

    


