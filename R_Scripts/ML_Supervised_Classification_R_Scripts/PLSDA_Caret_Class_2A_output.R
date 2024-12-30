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
library(pls)

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
HSIOil$class_2<-as.factor(HSIOil$class_2)
HSIOil$class_2_number<-as.numeric(HSIOil$class_2_number)
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

##Set parameters for running the PLS cross-validation model
# Set up the training control(10 folds cross_validation)
control<-trainControl(method = "repeatedcv", number = 10, repeats =10)

#metric<-"Accuracy"

# Set up the grid for tuning to find the optimal value of latent variables
grid <- expand.grid(ncomp=seq(1,100,by=1))

#Train the model using cross-validation (10 folds, 10 iterations)
fit_pls<-caret::train(y = data_train[,225], x = data_train[,-225], method = "pls", 
                       tuneGrid = grid, trControl = control, metric = metric)

print(fit_pls)#model summary
plot(fit_pls, col = 'red', pch = 1)

#Find the optimum number of latent variables (LVs)
print(fit_pls$bestTune)
ideal_LVs<-fit_pls$bestTune# to be used for the test set results


#Save CV PLS model results 
CC_k_class_2_train_MSC_A<-as.data.frame(fit_pls$results)
write.csv(CC_k_class_2_train_MSC_A, file = "CC_k_class_2_train_MSC_A.csv")

##Find important variables from PLS model
impo_variable<-varImp(fit_pls)
ImptV<-(impo_variable$importance)
write.csv(ImptV, file = "important_variable_MSC_class_2.csv")

##Use the optimal model to predict the external samples
#predict the test set
test_class_2<-predict(fit_pls,newdata = data_test[,-225],y = data_test$class_2)

#Get the confusion matrix for testing data    
cf_class_2_test<-confusionMatrix(test_class_2,data_test$class_2)
print(cf_class_2_test)

##Save the testing confusion matrix matrix 
cf_class_2_test_MSC_A<-as.data.frame(cf_class_2_test$table)
write.csv(cf_class_2_test_MSC_A, 
          "cf_class_2_test_MSC_A.csv")

##Save the testing metrics
cf_class_2_test_metrics_MSC_A<-as.data.frame(cf_class_2_test$byClass)
write.csv(cf_class_2_test_metrics_MSC_A,file = "cf_class_2_test_metrics_MSC_A.csv")
#-----------------------THE END-------------------------------------------------

###plotting misclassifications
##Plots###
##subset the necessary columns from original data set
columns_msc_class_2<-HSIOil  %>% filter(Cal_Val == 1) %>% 
  select(4,8)
table(columns_msc_class_2[,])
#Check the pls model 
table(columns_msc_class_2[,1])
#get the scores and check proportions
fit_pls$finalModel$loadings

scores_msc_class2<-as.data.frame.matrix(fit_pls$finalModel$scores)
scores_msc_class2

scores_1<-cbind(columns_msc_class_2,scores_msc_class2)
scores_1<-clean_names(scores_1)
head(scores_1)


msc_plsda_plot_class2 <- scores_1 %>%
  ggplot(mapping = aes(x = comp_1, y = comp_2, color = class_2, 
                       shape = class_2)) +
  geom_point() +
  theme_bw() +
  labs(x = 'LV1', y = 'LV2') +
  scale_color_manual(values = c("black", "red", "blue")) +
  scale_shape_manual(values = c(18, 20, 17)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        axis.text.x = element_text(color = 'black'),
        axis.text.y = element_text(color = 'black'),
        aspect.ratio = 1, legend.position = "bottom")+
  theme(legend.title = element_blank())+
  annotate("text",x = 0.06, y = 0.035, label = "(b)", color = "black")

msc_plsda_plot_class2                 
ggsave("output_msc_plsda_plot_class2.png", plot = msc_plsda_plot_class2, dpi = 300)

###Latent Variables
##Latent Variable Plot
pls<-fit_pls$results
x_Lvs= 35
y_Acc = 0.9973722

plot(pls$ncomp, pls$Accuracy, type = "b",ylab = "Accuracy (%)", xlab = "LVs", 
     cex.axis = 0.8,col = "black", pch = 4, cex.lab = 0.9)

# Add line from x-axis to the specific point
segments(x0 = x_Lvs, y0 = 0, x1 = x_Lvs, y1 = y_Acc, col = "black", lty = 2)

# Add line from y-axis to the specific point
segments(x0 = 0, y0 = y_Acc, x1 = x_Lvs, y1 = y_Acc, col = "black", lty = 2)
text(x = 100, y = 0.99, labels = "(d)", col = "black", cex = 0.8)



