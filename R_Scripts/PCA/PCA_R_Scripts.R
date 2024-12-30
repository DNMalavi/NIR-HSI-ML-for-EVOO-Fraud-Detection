
#-----------OLIVE OIL ADULTERATION---------------------------------------------

#Reduction Dimesnion and Exploration by #Principal Component Analysis (PCA)

#Set the working directory
setwd("~/Derick Malavi_PhD Docs_UGent/Manuscript 3 EVOO/PLS_R/DATA_A_Oil_Modified/PCA")

#Load packages
library(caret)
library(tidyr)
library(tidyverse)
library(readxl)
library(readr)
library(FactoMineR)
library(mdatools)
#-------------------------------------------------------------------------------

### Import and check DATA-------------------------------------------------------
#Import data
HSIOil<-read_excel("Raw_A.xlsx")
HSIOil<-as.data.frame(HSIOil)
head(HSIOil)
anyNA(HSIOil)#check any missing values
dim(HSIOil)
colnames(HSIOil[c(1:9)])

#Change some variables to factors and numeric
HSIOil$class_1<-as.factor(HSIOil$class_1)
HSIOil$class_1_number<-as.numeric(HSIOil$class_1_number)
HSIOil$class_2<-as.factor(HSIOil$class_2)
HSIOil$class_2_number<-as.numeric(HSIOil$class_2_number)
HSIOil$class_3<-as.factor(HSIOil$class_3)
HSIOil$class_3_number<-as.numeric(HSIOil$class_3_number)
HSIOil$adulteration_level<-as.numeric(HSIOil$adulteration_level)
HSIOil$Cal_Val<-as.numeric(HSIOil$Cal_Val)

str(HSIOil[,c(1:9)])
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
#-------------------------------------------------------------------------------
#Perform PCA

pca_Raw_A<-pca(HSIFull, 7, scale = FALSE, info = "PCA_Raw_Data")
pca_Raw_A$res$cal$expvar
scores<-pca_Raw_A$calres$scores[,c(1:2)]#PC Scores
pca_data_Raw_A<-cbind(HSIOil[,c(4,8)],scores)
head(pca_data_Raw_A)
colnames(pca_data_Raw_A)<-c("oil_type","adulter_level", "PC1","PC2")

#Plot PC Scores

pc_plot_Raw_A <- pca_data_Raw_A %>% 
  ggplot(mapping = aes(x = PC1, y = PC2)) +
  geom_point(aes(shape = oil_type, color = adulter_level)) +
  labs(x = "PC1(92.9%)", y = "PC2(6.0%)",cex.axis =0.9) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = 'black', fill = NA),
    axis.text.x = element_text(color = 'black',size = 10),
    axis.text.y = element_text(color = 'black',size = 10), aspect.ratio = 1,
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    legend.position = "none" )+
  annotate("text",x = 0.2, y = 0.11, label = "Raw spectra", color = "black")+
  scale_color_gradient(low = "#000000", high = "red")
  
pc_plot_Raw_A<-pc_plot_Raw_A+stat_ellipse(aes(group = oil_type), 
                                          level = 0.95, 
                                          geom = "polygon", alpha = 0.2,
                                          color = 'black',size = 0.6)
pc_plot_Raw_A

ggsave("output_plot_Raw_A.png", plot = pc_plot_Raw_A, dpi = 600, 
       width = 6, height = 6)
#-------------------------------------------------------------------------------
#Savitzky Golay without derivative (SG_0D)

### Import and check DATA-------------------------------------------------------
#Import data
HSIOil<-read.csv("SG_0D_A.csv")
HSIOil<-as.data.frame(HSIOil)
head(HSIOil)
anyNA(HSIOil)#check any missing values
dim(HSIOil)
colnames(HSIOil[c(1:9)])

#Change some variables to factors and numeric
HSIOil$class_1<-as.factor(HSIOil$class_1)
HSIOil$class_1_number<-as.numeric(HSIOil$class_1_number)
HSIOil$class_2<-as.factor(HSIOil$class_2)
HSIOil$class_2_number<-as.numeric(HSIOil$class_2_number)
HSIOil$class_3<-as.factor(HSIOil$class_3)
HSIOil$class_3_number<-as.numeric(HSIOil$class_3_number)
HSIOil$adulteration_level<-as.numeric(HSIOil$adulteration_level)
HSIOil$Cal_Val<-as.numeric(HSIOil$Cal_Val)

str(HSIOil[,c(1:9)])
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
#-------------------------------------------------------------------------------
#Perform PCA

pca_SG_0D_A<-pca(HSIFull, 7, scale = FALSE, info = "PCA_SG_0D_Data")
pca_SG_0D_A$res$cal$expvar
scores<-pca_SG_0D_A$calres$scores[,c(1:2)]
pca_data_SG_0D_A<-cbind(HSIOil[,c(4,8)],scores)
head(pca_data_SG_0D_A)
colnames(pca_data_SG_0D_A)<-c("oil_type","adulter_level", "PC1","PC2")

#Plot PC Scores

pc_plot_SG_0D_A <- pca_data_SG_0D_A %>% 
  ggplot(mapping = aes(x = PC1, y = PC2)) +
  geom_point(aes(shape = oil_type, color = adulter_level)) +
  labs(x = "PC1(92.9%)", y = "PC2(6.0%)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        axis.text.x = element_text(color = 'black',size =10),
        axis.text.y = element_text(color = 'black',size =10), aspect.ratio = 1,
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        legend.position = "none" )+
  annotate("text",x = 0.3, y = 0.2, label = "Savitzky-Golay (SG)", 
           color = "black",size = 3)+
  scale_color_gradient(low = "#000000", high = "red")

pc_plot_SG_0D_A<-pc_plot_SG_0D_A+stat_ellipse(aes(group = oil_type), 
                                          level = 0.95, 
                                          geom = "polygon", alpha = 0.2,
                                          color = 'black',size = 0.6)
pc_plot_SG_0D_A

ggsave("output_plot_SG_0D_A.png", plot = pc_plot_Raw_A, dpi = 600, 
       width = 6, height = 6)
#------------------------------END----------------------------------------------
#Savitzky Golay and first derivative (SG_1D)

### Import and check DATA-------------------------------------------------------
#Import data
HSIOil<-read.csv("SG_1D_A.csv")
HSIOil<-as.data.frame(HSIOil)
head(HSIOil)
anyNA(HSIOil)#check any missing values
dim(HSIOil)
colnames(HSIOil[c(1:9)])

#Change some variables to factors and numeric
HSIOil$class_1<-as.factor(HSIOil$class_1)
HSIOil$class_1_number<-as.numeric(HSIOil$class_1_number)
HSIOil$class_2<-as.factor(HSIOil$class_2)
HSIOil$class_2_number<-as.numeric(HSIOil$class_2_number)
HSIOil$class_3<-as.factor(HSIOil$class_3)
HSIOil$class_3_number<-as.numeric(HSIOil$class_3_number)
HSIOil$adulteration_level<-as.numeric(HSIOil$adulteration_level)
HSIOil$Cal_Val<-as.numeric(HSIOil$Cal_Val)

str(HSIOil[,c(1:9)])
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
#-------------------------------------------------------------------------------
#Perform PCA

pca_SG_1D_A<-pca(HSIFull, 7, scale = FALSE, info = "PCA_SG_1D_Data")
pca_SG_1D_A$res$cal$expvar
scores<-pca_SG_1D_A$calres$scores[,c(1:2)]
pca_data_SG_1D_A<-cbind(HSIOil[,c(4,8)],scores)
colnames(pca_data_SG_1D_A)<-c("oil_type","adulter_level", "PC1","PC2")
head(pca_data_SG_1D_A)

#Plot PC Scores

pc_plot_SG_1D_A <- pca_data_SG_1D_A %>% 
  ggplot(mapping = aes(x = PC1, y = PC2)) +
  geom_point(aes(shape = oil_type, color = adulter_level)) +
  labs(x = "PC1(54.9%)", y = "PC2(25.5%)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        axis.text.x = element_text(color = 'black',size = 10),
        axis.text.y = element_text(color = 'black',size = 10), aspect.ratio = 1,
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        legend.text = element_text(size = 6),legend.title = element_text(size = 6))+
  annotate("text",x = -0.01, y = 0.014, label = "SG+1st Deriv", color = "black",size =3)+
  scale_color_gradient(low = "#000000", high = "red")

pc_plot_SG_1D_A<-pc_plot_SG_1D_A+stat_ellipse(aes(group = oil_type), 
                                            level = 0.95, 
                                            geom = "polygon", alpha = 0.2,
                                            color = "black",size = 0.6)
pc_plot_SG_1D_A

ggsave("output_plot_SG_1D_A.png", plot = pc_plot_Raw_A, dpi = 600, 
       width = 6, height = 6)
#-----------------------------END-----------------------------------------------
#Savitzky Golay and first derivative (SG_2D)

### Import and check DATA-------------------------------------------------------
#Import data
HSIOil<-read.csv("SG_2D_A.csv")
HSIOil<-as.data.frame(HSIOil)
head(HSIOil)
anyNA(HSIOil)#check any missing values
dim(HSIOil)
colnames(HSIOil[c(1:9)])

#Change some variables to factors and numeric
HSIOil$class_1<-as.factor(HSIOil$class_1)
HSIOil$class_1_number<-as.numeric(HSIOil$class_1_number)
HSIOil$class_2<-as.factor(HSIOil$class_2)
HSIOil$class_2_number<-as.numeric(HSIOil$class_2_number)
HSIOil$class_3<-as.factor(HSIOil$class_3)
HSIOil$class_3_number<-as.numeric(HSIOil$class_3_number)
HSIOil$adulteration_level<-as.numeric(HSIOil$adulteration_level)
HSIOil$Cal_Val<-as.numeric(HSIOil$Cal_Val)

str(HSIOil[,c(1:9)])
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
#-------------------------------------------------------------------------------
#Perform PCA

pca_SG_2D_A<-pca(HSIFull, 7, scale = FALSE, info = "PCA_SG_2D_Data")
pca_SG_2D_A$res$cal$expvar
scores<-pca_SG_2D_A$calres$scores[,c(1:2)]
pca_data_SG_2D_A<-cbind(HSIOil[,c(4,8)],scores)
colnames(pca_data_SG_2D_A)<-c("oil_type","adulter_level", "PC1","PC2")
head(pca_data_SG_2D_A)

#Plot PC Scores

pc_plot_SG_2D_A <- pca_data_SG_2D_A %>% 
  ggplot(mapping = aes(x = PC1, y = PC2)) +
  geom_point(aes(shape = oil_type, color = adulter_level)) +
  labs(x = "PC1(54.9%)", y = "PC2(25.5%)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        axis.text.x = element_text(color = 'black',size = 10),
        axis.text.y = element_text(color = 'black',size = 10), aspect.ratio = 1,
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        legend.position = "none")+
  annotate("text",x = -0.01, y = 0.015, label = "SG+2nd Deriv", color = "black")+
  scale_color_gradient(low = "#000000", high = "red")

pc_plot_SG_2D_A<-pc_plot_SG_2D_A+stat_ellipse(aes(group = oil_type), 
                              level = 0.95, 
                              geom = "polygon", alpha = 0.2,
                              color = "black",size = 0.6)
pc_plot_SG_2D_A

ggsave("output_plot_SG_2D_A.png", plot = pc_plot_Raw_A, dpi = 600, 
       width = 6, height = 6)
#----------------------------END------------------------------------------------
#Standard Normal Variate (SNV)

### Import and check DATA-------------------------------------------------------
#Import data
HSIOil<-read.csv("SNV_A.csv")
HSIOil<-as.data.frame(HSIOil)
head(HSIOil)
anyNA(HSIOil)#check any missing values
dim(HSIOil)
colnames(HSIOil[c(1:9)])

#Change some variables to factors and numeric
HSIOil$class_1<-as.factor(HSIOil$class_1)
HSIOil$class_1_number<-as.numeric(HSIOil$class_1_number)
HSIOil$class_2<-as.factor(HSIOil$class_2)
HSIOil$class_2_number<-as.numeric(HSIOil$class_2_number)
HSIOil$class_3<-as.factor(HSIOil$class_3)
HSIOil$class_3_number<-as.numeric(HSIOil$class_3_number)
HSIOil$adulteration_level<-as.numeric(HSIOil$adulteration_level)
HSIOil$Cal_Val<-as.numeric(HSIOil$Cal_Val)

str(HSIOil[,c(1:9)])
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
#-------------------------------------------------------------------------------
#Perform PCA

pca_SNV_A<-pca(HSIFull, 7, scale = FALSE, info = "PCA_SNV_Data")
pca_SNV_A$res$cal$expvar
scores<-pca_SNV_A$calres$scores[,c(1:2)]
pca_data_SNV_A<-cbind(HSIOil[,c(4,8)],scores)
colnames(pca_data_SNV_A)<-c("oil_type","adulter_level", "PC1","PC2")
head(pca_data_SNV_A)

#Plot PC Scores

pc_plot_SNV_A <- pca_data_SNV_A %>% 
  ggplot(mapping = aes(x = PC1, y = PC2)) +
  geom_point(aes(shape = oil_type, color = adulter_level)) +
  labs(x = "PC1(54.9%)", y = "PC2(25.5%)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        axis.text.x = element_text(color = 'black',size = 10),
        axis.text.y = element_text(color = 'black',size = 10), aspect.ratio = 1,
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        legend.position = "none")+
  annotate("text",x = -0.15, y = 0.3, label = "SNV", color = "black")+
  scale_color_gradient(low = "#000000", high = "red")

pc_plot_SNV_A <- pc_plot_SNV_A+stat_ellipse(aes(group = oil_type), 
                              level = 0.95, 
                              geom = "polygon", alpha = 0.2,
                              color = "black",size = 0.6)
pc_plot_SNV_A

ggsave("output_plot_SNV_A.png", plot = pc_plot_Raw_A, dpi = 600, 
       width = 6, height = 6)
#--------------------------------END--------------------------------------------
#Standard Normal Variate and no derivative (SNV_SG_0D)

### Import and check DATA-------------------------------------------------------
#Import data
HSIOil<-read.csv("SNV_SG_0D_A.csv")
HSIOil<-as.data.frame(HSIOil)
head(HSIOil)
anyNA(HSIOil)#check any missing values
dim(HSIOil)
colnames(HSIOil[c(1:9)])

#Change some variables to factors and numeric
HSIOil$class_1<-as.factor(HSIOil$class_1)
HSIOil$class_1_number<-as.numeric(HSIOil$class_1_number)
HSIOil$class_2<-as.factor(HSIOil$class_2)
HSIOil$class_2_number<-as.numeric(HSIOil$class_2_number)
HSIOil$class_3<-as.factor(HSIOil$class_3)
HSIOil$class_3_number<-as.numeric(HSIOil$class_3_number)
HSIOil$adulteration_level<-as.numeric(HSIOil$adulteration_level)
HSIOil$Cal_Val<-as.numeric(HSIOil$Cal_Val)

str(HSIOil[,c(1:9)])
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
#-------------------------------------------------------------------------------
#Perform PCA

pca_SNV_SG_0D_A<-pca(HSIFull, 7, scale = FALSE, info = "PCA_SNV_SG_0D_Data")
pca_SNV_SG_0D_A$res$cal$expvar
scores<-pca_SNV_SG_0D_A$calres$scores[,c(1:2)]
pca_data_SNV_SG_0D_A<-cbind(HSIOil[,c(4,8)],scores)
colnames(pca_data_SNV_SG_0D_A)<-c("oil_type","adulter_level", "PC1","PC2")
head(pca_data_SNV_SG_0D_A)

#Plot PC Scores

pc_plot_SNV_SG_0D_A <- pca_data_SNV_SG_0D_A %>% 
  ggplot(mapping = aes(x = PC1, y = PC2)) +
  geom_point(aes(shape = oil_type, color = adulter_level)) +
  labs(x = "PC1(69.4%)", y = "PC2(12.0%)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        axis.text.x = element_text(color = 'black',size = 10),
        axis.text.y = element_text(color = 'black',size = 10), aspect.ratio = 1,
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        legend.text = element_text(size = 6),legend.title = element_text(size = 6))+
  annotate("text",x = -0.1, y = 0.28, label = "SNV+SG", color = "black")+ 
  scale_color_gradient(low = "#000000", high = "red")

pc_plot_SNV_SG_0D_A<-pc_plot_SNV_SG_0D_A+stat_ellipse(aes(group = oil_type), 
                                  level = 0.95, 
                                  geom = "polygon", alpha = 0.2,
                                  color = "black",size = 0.6)
pc_plot_SNV_SG_0D_A
ggsave("output_plot_SNV_SG_0D_A.png", plot = pc_plot_Raw_A, dpi = 600, 
       width = 6, height = 6)
#--------------------------------END--------------------------------------------
#Standard Normal Variate and first derivative (SNV_SG_1D)

### Import and check DATA-------------------------------------------------------
#Import data
HSIOil<-read.csv("SNV_SG_1D_A.csv")
HSIOil<-as.data.frame(HSIOil)
head(HSIOil)
anyNA(HSIOil)#check any missing values
dim(HSIOil)
colnames(HSIOil[c(1:9)])

#Change some variables to factors and numeric
HSIOil$class_1<-as.factor(HSIOil$class_1)
HSIOil$class_1_number<-as.numeric(HSIOil$class_1_number)
HSIOil$class_2<-as.factor(HSIOil$class_2)
HSIOil$class_2_number<-as.numeric(HSIOil$class_2_number)
HSIOil$class_3<-as.factor(HSIOil$class_3)
HSIOil$class_3_number<-as.numeric(HSIOil$class_3_number)
HSIOil$adulteration_level<-as.numeric(HSIOil$adulteration_level)
HSIOil$Cal_Val<-as.numeric(HSIOil$Cal_Val)

str(HSIOil[,c(1:9)])
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
#-------------------------------------------------------------------------------
#Perform PCA

pca_SNV_SG_1D_A<-pca(HSIFull, 7, scale = FALSE, info = "PCA_SNV_SG_1D_Data")
pca_SNV_SG_1D_A$res$cal$expvar
scores<-pca_SNV_SG_1D_A$calres$scores[,c(1:2)]
pca_data_SNV_SG_1D_A<-cbind(HSIOil[,c(4,8)],scores)
colnames(pca_data_SNV_SG_1D_A)<-c("oil_type","adulter_level", "PC1","PC2")
head(pca_data_SNV_SG_1D_A)

#Plot PC Scores

pc_plot_SNV_SG_1D_A <- pca_data_SNV_SG_1D_A %>% 
  ggplot(mapping = aes(x = PC1, y = PC2)) +
  geom_point(aes(shape = oil_type, color = adulter_level)) +
  labs(x = "PC1(57.3%)", y = "PC2(21.2%)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        axis.text.x = element_text(color = 'black',size = 10),
        axis.text.y = element_text(color = 'black',size = 10), aspect.ratio = 1,
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        legend.position = "none")+
  annotate("text",x = 0.0, y = 0.06, label = "SNV+SG+1st Deriv", color = "black")+
  scale_color_gradient(low = "#000000", high = "red")

pc_plot_SNV_SG_1D_A<-pc_plot_SNV_SG_1D_A+stat_ellipse(aes(group = oil_type), 
                                                      level = 0.95, 
                                                      geom = "polygon", alpha = 0.2,
                                                      color = "black",size = 0.6)
pc_plot_SNV_SG_1D_A
ggsave("output_plot_SNV_SG_1D_A.png", plot = pc_plot_Raw_A, dpi = 600, 
       width = 6, height = 6)
#------------------------END----------------------------------------------------

#Standard Normal Variate and second derivative (SNV_SG_2D)

### Import and check DATA-------------------------------------------------------
#Import data
HSIOil<-read.csv("SNV_SG_2D_A.csv")
HSIOil<-as.data.frame(HSIOil)
head(HSIOil)
anyNA(HSIOil)#check any missing values
dim(HSIOil)
colnames(HSIOil[c(1:9)])

#Change some variables to factors and numeric
HSIOil$class_1<-as.factor(HSIOil$class_1)
HSIOil$class_1_number<-as.numeric(HSIOil$class_1_number)
HSIOil$class_2<-as.factor(HSIOil$class_2)
HSIOil$class_2_number<-as.numeric(HSIOil$class_2_number)
HSIOil$class_3<-as.factor(HSIOil$class_3)
HSIOil$class_3_number<-as.numeric(HSIOil$class_3_number)
HSIOil$adulteration_level<-as.numeric(HSIOil$adulteration_level)
HSIOil$Cal_Val<-as.numeric(HSIOil$Cal_Val)

str(HSIOil[,c(1:9)])
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
#-------------------------------------------------------------------------------
#Perform PCA

pca_SNV_SG_2D_A<-pca(HSIFull, 7, scale = FALSE, info = "PCA_SNV_SG_2D_Data")
pca_SNV_SG_2D_A$res$cal$expvar
scores<-pca_SNV_SG_2D_A$calres$scores[,c(1:2)]
pca_data_SNV_SG_2D_A<-cbind(HSIOil[,c(4,8)],scores)
colnames(pca_data_SNV_SG_2D_A)<-c("oil_type","adulter_level", "PC1","PC2")
head(pca_data_SNV_SG_2D_A)

#Plot PC Scores

pc_plot_SNV_SG_2D_A <- pca_data_SNV_SG_2D_A %>% 
  ggplot(mapping = aes(x = PC1, y = PC2)) +
  geom_point(aes(shape = oil_type, color = adulter_level)) +
  labs(x = "PC1(49.5%)", y = "PC2(20.5%)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        axis.text.x = element_text(color = 'black',size = 10),
        axis.text.y = element_text(color = 'black',size = 10), aspect.ratio = 1,
        axis.title.x = element_text(size =9),
        axis.title.y = element_text(size =9),
        legend.position = "none")+
  annotate("text",x = -0.015, y = 0.02, label = "SNV+SG+2nd Deriv", color = "black")+
  scale_color_gradient(low = "#000000", high = "red")

pc_plot_SNV_SG_2D_A<-pc_plot_SNV_SG_2D_A+
stat_ellipse(aes(group = oil_type),level = 0.95,geom = "polygon", alpha = 0.2,
color = "black",size = 0.6)     

pc_plot_SNV_SG_2D_A

ggsave("output_plot_SNV_SG_2D_A.png", plot = pc_plot_Raw_A, dpi = 600, 
       width = 6, height = 6)
#---------------------------------END-------------------------------------------
#Multiplicative Scatter Correction(MSC)

### Import and check DATA-------------------------------------------------------
#Import data
HSIOil<-read.csv("MSC_A.csv")
HSIOil<-as.data.frame(HSIOil)
head(HSIOil)
anyNA(HSIOil)#check any missing values
dim(HSIOil)
colnames(HSIOil[c(1:9)])

#Change some variables to factors and numeric
HSIOil$class_1<-as.factor(HSIOil$class_1)
HSIOil$class_1_number<-as.numeric(HSIOil$class_1_number)
HSIOil$class_2<-as.factor(HSIOil$class_2)
HSIOil$class_2_number<-as.numeric(HSIOil$class_2_number)
HSIOil$class_3<-as.factor(HSIOil$class_3)
HSIOil$class_3_number<-as.numeric(HSIOil$class_3_number)
HSIOil$adulteration_level<-as.numeric(HSIOil$adulteration_level)
HSIOil$Cal_Val<-as.numeric(HSIOil$Cal_Val)

str(HSIOil[,c(1:9)])
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
#-------------------------------------------------------------------------------
#Perform PCA

pca_MSC_A<-pca(HSIFull, 7, scale = FALSE, info = "PCA_MSC_Data")
pca_MSC_A$res$cal$expvar
scores<-pca_MSC_A$calres$scores[,c(1:2)]
pca_data_MSC_A<-cbind(HSIOil[,c(4,8)],scores)
colnames(pca_data_MSC_A)<-c("oil_type","adulter_level", "PC1","PC2")
head(pca_data_MSC_A)

#Plot PC Scores

pc_plot_MSC_A <- pca_data_MSC_A %>% 
  ggplot(mapping = aes(x = PC1, y = PC2)) +
  geom_point(aes(shape = oil_type, color = adulter_level)) +
  labs(x = "PC1(69.0%)", y = "PC2(11.9%)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        axis.text.x = element_text(color = 'black',size = 10),
        axis.text.y = element_text(color = 'black',size = 10), aspect.ratio = 1,
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
       legend.title = element_text(size = 6),legend.text = element_text(size = 6))+
  annotate("text",x = -0.01, y = 0.065, label = "MSC", color = "black")+
  scale_color_gradient(low = "#000000", high = "red")

pc_plot_MSC_A<-pc_plot_MSC_A+stat_ellipse(aes(group = oil_type),
            level = 0.95,geom = "polygon", alpha = 0.2,
            color = "black",size = 0.6) 

pc_plot_MSC_A

ggsave("output_plot_MSC_A.png", plot = pc_plot_Raw_A, dpi = 600, 
       width = 6, height = 6)
#------------------------------END----------------------------------------------

#Multiplicative Scatter Correction(MSC) and Savitzky Golay Filter 

### Import and check DATA-------------------------------------------------------
#Import data
HSIOil<-read.csv("MSC_SG_0D_A.csv")
HSIOil<-as.data.frame(HSIOil)
head(HSIOil)
anyNA(HSIOil)#check any missing values
dim(HSIOil)
colnames(HSIOil[c(1:9)])

#Change some variables to factors and numeric
HSIOil$class_1<-as.factor(HSIOil$class_1)
HSIOil$class_1_number<-as.numeric(HSIOil$class_1_number)
HSIOil$class_2<-as.factor(HSIOil$class_2)
HSIOil$class_2_number<-as.numeric(HSIOil$class_2_number)
HSIOil$class_3<-as.factor(HSIOil$class_3)
HSIOil$class_3_number<-as.numeric(HSIOil$class_3_number)
HSIOil$adulteration_level<-as.numeric(HSIOil$adulteration_level)
HSIOil$Cal_Val<-as.numeric(HSIOil$Cal_Val)

str(HSIOil[,c(1:9)])
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
#-------------------------------------------------------------------------------
#Perform PCA

pca_MSC_SG_0D_A<-pca(HSIFull, 7, scale = FALSE, info = "PCA_MSC_SG_0D_Data")
pca_MSC_SG_0D_A$res$cal$expvar
scores<-pca_MSC_SG_0D_A$calres$scores[,c(1:2)]
pca_data_MSC_SG_0D_A<-cbind(HSIOil[,c(4,8)],scores)
colnames(pca_data_MSC_SG_0D_A)<-c("oil_type","adulter_level", "PC1","PC2")
head(pca_data_MSC_SG_0D_A)

#Plot PC Scores

pc_plot_MSC_SG_0D_A <- pca_data_MSC_SG_0D_A %>% 
  ggplot(mapping = aes(x = PC1, y = PC2)) +
  geom_point(aes(shape = oil_type, color = adulter_level)) +
  labs(x = "PC1(69.4%)", y = "PC2(12.0%)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        axis.text.x = element_text(color = 'black',size = 10),
        axis.text.y = element_text(color = 'black',size = 10), aspect.ratio = 1,
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        legend.position = "none")+
  annotate("text",x = -0.03, y = 0.07, label = "MSC+SG", color = "black")+
  scale_color_gradient(low = "#000000", high = "red")


pc_plot_MSC_SG_0D_A<-pc_plot_MSC_SG_0D_A+stat_ellipse(aes(group = oil_type),
                                          level = 0.95,geom = "polygon", alpha = 0.2,
                                          color = "black",size = 0.6)
pc_plot_MSC_SG_0D_A
ggsave("output_plot_MSC_SG_0D_A.png", plot = pc_plot_Raw_A, dpi = 600, 
       width = 6, height = 6)
#--------------------------------END--------------------------------------------
#Multiplicative Scatter Correction (MSC), Savitzky Golay and Filter
### Import and check DATA-------------------------------------------------------
#Import data
HSIOil<-read.csv("MSC_SG_1D_A.csv")
HSIOil<-as.data.frame(HSIOil)
head(HSIOil)
anyNA(HSIOil)#check any missing values
dim(HSIOil)
colnames(HSIOil[c(1:9)])

#Change some variables to factors and numeric
HSIOil$class_1<-as.factor(HSIOil$class_1)
HSIOil$class_1_number<-as.numeric(HSIOil$class_1_number)
HSIOil$class_2<-as.factor(HSIOil$class_2)
HSIOil$class_2_number<-as.numeric(HSIOil$class_2_number)
HSIOil$class_3<-as.factor(HSIOil$class_3)
HSIOil$class_3_number<-as.numeric(HSIOil$class_3_number)
HSIOil$adulteration_level<-as.numeric(HSIOil$adulteration_level)
HSIOil$Cal_Val<-as.numeric(HSIOil$Cal_Val)

str(HSIOil[,c(1:9)])
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
#-------------------------------------------------------------------------------
#Perform PCA

pca_MSC_SG_1D_A<-pca(HSIFull, 7, scale = FALSE, info = "PCA_MSC_SG_1D_Data")
pca_MSC_SG_1D_A$res$cal$expvar
scores<-pca_MSC_SG_1D_A$calres$scores[,c(1:2)]
pca_data_MSC_SG_1D_A<-cbind(HSIOil[,c(4,8)],scores)
colnames(pca_data_MSC_SG_1D_A)<-c("oil_type","adulter_level", "PC1","PC2")
head(pca_data_MSC_SG_1D_A)

#Plot PC Scores

pc_plot_MSC_SG_1D_A <- pca_data_MSC_SG_1D_A %>% 
  ggplot(mapping = aes(x = PC1, y = PC2)) +
  geom_point(aes(shape = oil_type, color = adulter_level)) +
  labs(x = "PC1(54.9%)", y = "PC2(25.5%)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        axis.text.x = element_text(color = 'black',size = 10),
        axis.text.y = element_text(color = 'black',size = 10), aspect.ratio = 1,
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        legend.position = "none")+
  annotate("text",x = -0.008, y = 0.014, label = "MSC+SG+1st Deriv", color = "black")+
  scale_color_gradient(low = "#000000", high = "red")

pc_plot_MSC_SG_1D_A<-pc_plot_MSC_SG_1D_A+stat_ellipse(aes(group = oil_type),
                                                      level = 0.95,geom = "polygon", alpha = 0.2,
                                                      color = "black",size = 0.6)

pc_plot_MSC_SG_1D_A
ggsave("output_plot_MSC_SG_1D_A.png", plot = pc_plot_Raw_A, dpi = 600, 
       width = 6, height = 6)
#--------------------------------END--------------------------------------------

#Multiplicative Scatter Correction (MSC), Savitzky Golay and Filter
### Import and check DATA-------------------------------------------------------
#Import data
HSIOil<-read.csv("MSC_SG_2D_A.csv")
HSIOil<-as.data.frame(HSIOil)
head(HSIOil)
anyNA(HSIOil)#check any missing values
dim(HSIOil)
colnames(HSIOil[c(1:9)])

#Change some variables to factors and numeric
HSIOil$class_1<-as.factor(HSIOil$class_1)
HSIOil$class_1_number<-as.numeric(HSIOil$class_1_number)
HSIOil$class_2<-as.factor(HSIOil$class_2)
HSIOil$class_2_number<-as.numeric(HSIOil$class_2_number)
HSIOil$class_3<-as.factor(HSIOil$class_3)
HSIOil$class_3_number<-as.numeric(HSIOil$class_3_number)
HSIOil$adulteration_level<-as.numeric(HSIOil$adulteration_level)
HSIOil$Cal_Val<-as.numeric(HSIOil$Cal_Val)

str(HSIOil[,c(1:9)])
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
#-------------------------------------------------------------------------------
#Perform PCA

pca_MSC_SG_2D_A<-pca(HSIFull, 7, scale = FALSE, info = "PCA_MSC_SG_2D_Data")
pca_MSC_SG_2D_A$res$cal$expvar
scores<-pca_MSC_SG_2D_A$calres$scores[,c(1:2)]
pca_data_MSC_SG_2D_A<-cbind(HSIOil[,c(4,8)],scores)
colnames(pca_data_MSC_SG_2D_A)<-c("oil_type","adulter_level", "PC1","PC2")
head(pca_data_MSC_SG_2D_A)

#Plot PC Scores

pc_plot_MSC_SG_2D_A <- pca_data_MSC_SG_2D_A %>% 
  ggplot(mapping = aes(x = PC1, y = PC2)) +
  geom_point(aes(shape = oil_type, color = adulter_level)) +
  labs(x = "PC1(52.8%)", y = "PC2(17.4%)",size = 6) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = 'black', fill = NA),
        axis.text.x = element_text(color = 'black',size = 10),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        axis.text.y = element_text(color = 'black',size = 10), aspect.ratio = 1,
        legend.title = element_text(size = 6),legend.text = element_text(size = 6))+
  annotate("text",x = -0.003, y = 0.0038, label = "MSC+SG+2nd Deriv", color = "black")+
  scale_color_gradient(low = "#000000", high = "red")


pc_plot_MSC_SG_2D_A<-pc_plot_MSC_SG_2D_A+stat_ellipse(aes(group = oil_type),
                                                      level = 0.95,geom = "polygon", alpha = 0.2,
                                                      color = "black",size = 0.6)
pc_plot_MSC_SG_2D_A
ggsave("output_plot_MSC_SG_2D_A.png", plot = pc_plot_Raw_A, dpi = 600, 
       width = 6, height = 6)
#-----------------------------END-----------------------------------------------
