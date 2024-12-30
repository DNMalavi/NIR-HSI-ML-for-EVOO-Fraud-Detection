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

### Import and check DATA-------------------------------------------------------
#Import data
HSIOil<-read_excel("Raw_A.xlsx")
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
HSIFull<-as.matrix(HSIOil[,-c(1:9)])
class(HSIOil)
class(HSIFull)

#subset samples for cross-validation and external validation (70/30)
#Record 1 under Cal_Val variable is for calibration while 2 will be for testing the model

sample<-HSIOil$Cal_Val== 1 

##split the X test and train variables
HSIFull_train<-HSIFull[sample, ]
HSIFull_test<-HSIFull[!sample, ]

##Split the Y variables for classes (labels)
##Class_3 whether the oil is authentic or adulterated
Y_class_3<-as.numeric(unlist(HSIOil[,7]))
Y_class_3_train<-(Y_class_3[sample])
Y_class_3_test<-Y_class_3[!sample]

#Check length of the subsets for training and validation
dim(HSIFull_train)
dim(HSIFull_test)
length(Y_class_3_train)
length(Y_class_3_test)

##combine the subsets for X and Y
data_train<-as.data.frame(cbind(Y_class_3_train,HSIFull_train))
data_train
data_test<-as.data.frame(cbind(Y_class_3_test,HSIFull_test))

dim(data_train)
dim(data_test)
#------------------------------------------------------------------------------
#set seed for reproducibility, set number of folds and iterations
set.seed (50)
folds<-10
times<-10

#Calculate the possible # of neighbors
poss_k<-sqrt(nrow(data_test))
print(poss_k)

# Set up the grid for tuning
grid <- expand.grid(k = c(25, 27, 29, 23, 21))  # Define the range of values for k

#Run algorithm for k-Nearest Neighbor (kNN)
# Set up the training control
trControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

fit_knn<-caret::train(x = HSIFull_train, y = factor(Y_class_3_train), method = "knn",
               trControl=trControl, tuneGrid = grid)
plot(fit_knn, col = 'red')

#Find the optimum number of k
print(fit_knn)
print(fit_knn$bestTune)
fit_knn$ptype

table(fit_knn,Y_class_3_train)

# Make predictions on the same training data
predictions <- predict(fit_knn, newdata = HSIFull_train)
table(predictions,Y_class_3_train)



