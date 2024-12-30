#-----------OLIVE OIL ADULTERATION---------------------------------------------

#k-Nearest Neighbor (kNN)algorithm
#For classification of oils into different classes
#------------------------------------------------------------------------------

#Set the working directory
setwd("~/Derick Malavi_PhD Docs_UGent/Manuscript 3 EVOO/PLS_R/DATA_A_Oil_Modified")

##Load packages-----------------------------------------------------------------
install.packages('class')
library(class)
library("Rcpp")
library("pls")
library("plsRglm")
library("plsdof")
library("plsRglm")
library("crossval")
library("data.table")
library("graphics")
library("pracma")
library("smooth")
library("Mcomp")
library("forecast")
library("prospectr")
require(smooth)
require(Mcomp)
library("MASS")
library("PredPsych")
library("ggplot2")
library("caret")
library("plsgenomics")
library("RSNNS")
library("elmNNRcpp")
library(readxl)
library('e1071')
library("lattice")
library("latticeExtra")
library("ContourFunctions")
library("laGP")
library("pid")
library(tidyverse)

### Import and check DATA-------------------------------------------------------
#Import data
HSIOil<-read.csv("MSC_SG_2D_A.csv")
HSIOil<-as.data.frame(HSIOil)
head(HSIOil)
anyNA(HSIOil)#check any missing values
str(HSIOil)
dim(HSIOil)
colnames(HSIOil[c(1:9)])

#Change some variables to factors
HSIOil$class_1<-as.factor(HSIOil$class_1)
HSIOil$class_1_number<-as.factor(HSIOil$class_1_number)
HSIOil$class_1<-as.factor(HSIOil$class_1)
HSIOil$class_1_number<-as.factor(HSIOil$class_1_number)
HSIOil$class_1<-as.factor(HSIOil$class_1)
HSIOil$class_1_number<-as.factor(HSIOil$class_1_number)
#Change some to numeric
HSIOil$adulteration_level<-as.numeric(HSIOil$adulteration_level)
HSIOil$Cal_Val<-as.numeric(HSIOil$Cal_Val)
str(HSIOil[c(1:9)])

#Check whether the no. of samples match for the classes
summary(HSIOil[,c(2:7)])
##class1 is EVOO, HZO, PMO, RFOVO, EVOO+HZO, EVOO+PMO, EVOO+RFOVO (0-6: 7 groups)
##class1 is EVOO, Adulterant and Adulterated Olive oil(0-2: 3 groups)
##class1 is EVOO and Adulterated olive oil (0:1, 2 groups) 
##-----------------------------------------------------------------------------
#Get the spectra/predictors (X). The independent variables
HSIFull<-as.matrix(HSIOil[,-c(1:9)])
class(HSIOil)
class(HSIFull)
View(HSIFull)

#subset samples for cross-validation and external validation
#Record 1 under Cal_Val variable is for calibration while 2 will be for testing the model

sample<-HSIOil$Cal_Val== 1 
##split the X test and train variables
HSIFull_train<-HSIFull[sample, ]
HSIFull_test<-HSIFull[!sample, ]
