### Import and check DATA-------------------------------------------------------
#Import data
HSIOil<-read.csv("SNV_A.csv")
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
X<-as.matrix(HSIOil[,-c(1:9)])
class(HSIOil)
Y<-HSIOil[,2]

model_1<-svm(HSIOil$class_1~., data = HSIOil)

tune_results<-tune(svm, train.x = X, train.y = Y, kernel = 'radial')
tune_results <- tune(svm, 
                     train.x = X, 
                     train.y = Y, 
                     kernel = 'radial', 
                     ranges = list(
                       cost = c(0.0001, 0.0005, 0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1, 5, 10, 25, 50, 100, 200),
                       epsilon = c(0.9, 0.8, 0.5, 0.3, 0.2, 0.1, 0.05, 0.02, 0.01)
                     )
)

summary(tune_results)


