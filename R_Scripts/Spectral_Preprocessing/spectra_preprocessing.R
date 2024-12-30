setwd("C:/Users/RC2/OneDrive - UGent/Documenten/Derick Malavi_PhD Docs_UGent/Manuscript 3 EVOO/PLS_R")

library(readxl)
library(mdatools)
library(writexl)
library(readr)

prep<-read_csv("SNV.csv")
col_names<-prep[,c(1:8)]
head(col_names)
prep_spectra<-prep[,-c(1:8)]

#SNV and SG smoothing 0 derivative
SNV_SG_0D<-prep.savgol(prep_spectra, width = 7,porder = 2,dorder = 0)
SNV_SG_0D<-cbind(col_names,SNV_SG_0D)
head(SNV_SG_0D)
View(SNV_SG_0D)
write.csv(SNV_SG_0D,"SNV_SG_0D.csv")

#SNV combined with Savitzky Golay and 1st Derivative: 

SNV_SG_1D<-prep.savgol(prep_spectra, width = 7,porder = 2,dorder = 1)
SNV_SG_1D<-cbind(col_names,SNV_SG_1D)
head(SNV_SG_1D)
View(SNV_SG_1D)
write.csv(SNV_SG_1D,"SNV_SG_1D.csv")

#SNV combined with Savitzky Golay and 2nd Derivative:

SNV_SG_2D<-prep.savgol(prep_spectra, width = 7,porder = 2,dorder = 2)
SNV_SG_2D<-cbind(col_names,SNV_SG_2D)
View(SNV_SG_2D)
write.csv(SNV_SG_2D,"SNV_SG_2D.csv")

#Multiplicative Scatter Correction and Savitzky Golay smoothing
prepmsc<-read.csv("MSC.csv")
col_names_msc<-prepmsc[,c(1:8)]
View(col_names_msc)
msc_spectra<-prepmsc[,-c(1:8)]
MSC_SG_0D<-prep.savgol(msc_spectra,width = 7,porder = 2,dorder = 0)
View(MSC_SG_0D)
MSC_SG_0D<-cbind(col_names_msc,MSC_SG_0D)
write.csv(MSC_SG_0D,"MSC_SG_0D.csv")

#Multiplicative Scatter Correction, Savitzky Golay and 1st Derivative

prepmsc<-read.csv("HSIFull_MSC.csv")
col_names_msc<-prepmsc[,c(1:8)]
View(col_names_msc)
msc_spectra<-prepmsc[,-c(1:8)]
MSC_SG_1D<-prep.savgol(msc_spectra,width = 7,porder = 2,dorder = 1)
View(MSC_SG_1D)
MSC_SG_1D<-cbind(col_names_msc,MSC_SG_1D)
write.csv(MSC_SG_1D,"MSC_SG_1D.csv")

#Multiplicative Scatter Correction, Savitzky Golay and 2nd Derivative
MSC_SG_2D<-prep.savgol(msc_spectra,width = 7,porder = 2,dorder = 2)
MSC_SG_2D<-cbind(col_names_msc,MSC_SG_2D)
View(MSC_SG_2D)
write.csv(MSC_SG_2D,"MSC_SG_2D.csv")

#Smoothing with Savitzky Golay without derivatives
SG<-read_excel("RAW.xlsx")
SG_col<-SG[,c(1:8)]
print(SG_col)
SG_spectra<-SG[,-c(1:8)]
SG_smooth<-prep.savgol(SG_spectra,width = 7,porder = 2,dorder = 0)
View(SG_smooth)
SG_smooth<-cbind(SG_col,SG_smooth)
write.csv(SG_smooth,"SG_smooth.csv")


