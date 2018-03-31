
tennis <- read.csv(file.choose(),header =TRUE,sep =";")
tennis
str(tennis)
library(e1071)

x <- c("Sunny" ,"Cool" , "High" ,"True" ,NA)

tennis <- rbind(tennis,x)

traindata <- tennis[1:14,]
testdata <- tennis[15,]

m <- naiveBayes(traindata[1:4],traindata[,5])

pred<- predict(m,testdata[,1:4])
print(pred)
