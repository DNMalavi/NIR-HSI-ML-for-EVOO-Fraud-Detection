Raw<-read.csv("cf_class_3_test_SG_2D_A.csv")
head(Raw)
Raw_T<-table(Raw)
print(Raw_T)
write.csv(Raw_T, file = "SG_2D.csv")
