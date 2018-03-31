
spam <- read.csv(file.choose(),header =TRUE,sep =",")
spam
str(spam)
library(e1071)

table(spam$Viagra)
table(spam$Money)
table(spam$Groceries)
table(spam$Unsubscribe)
table(spam$Likelihood)


x <- c("Yes" ,"No" , "No" ,"Yes" ,NA)


spam <- rbind(spam,x)

traindata <- spam[1:100,]
testdata <- spam[101,]

m <- naiveBayes(traindata[1:4],traindata[,5])

pred<- predict(m,testdata[,1:4])
print(pred)
