
police_shootings <-read.csv(file.choose(),header = TRUE ,sep = ",")
police_shootings

library(e1071)

x <- c("gun" ,"F" , "shot" ,"attack" ,"Not fleeing" , "False",NA)

police_shootings <- rbind(police_shootings ,x)

traindata <- police_shootings[1:2143,]
testdata <- police_shootings[2144,]

m <- naiveBayes(traindata[1:6],traindata[,7])

pred <- predict(m,testdata[1:6])
print(pred)