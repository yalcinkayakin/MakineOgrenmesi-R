
loan <- read.csv(file.choose(),header = TRUE,sep = ",")
loan
View(loan)
library(e1071)

x <- c("blue-collar" ,"single" ,"secondary","no","cellular","failure",NA)

loan <- rbind(loan,x)

traindata <- loan[1:127,]
testdata <- loan[128,]

m <- naiveBayes(traindata[1:6],traindata[,7])

pred <- predict(m,testdata[,1:6])

print(pred)