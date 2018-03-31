
wbcd <-read.csv(file.choose(),header = TRUE,sep = ",")
wbcd
str(wbcd)

wbcd <- wbcd[-1] # drop the id feature-first column

table(wbcd$diagnosis) #B -> bening M-> malignant

wbcd$diagnosis <-factor(wbcd$diagnosis,levels = c("B","M"),labels = c("Bening","Malignant"))
#classifies that the target features is coded...
#we need to record the diagnosis variable...

round(prop.table(table(wbcd$diagnosis))*100,digits = 1)
#prob.table() output,Bening and Malignant with 62,7 percent and Bening 62,7 percent

summary(wbcd[c("radius_mean","area_mean","smoothness_mean")])
#for purposes,we will only take a close look at three of these features

normalize <- function(x){
    return((x-min(x)/(max(x)-min(x))))  
}
#normalizinf numeric data

normalize(c(1,2,3,4,5)) # test the function on a couple of vectors

normalize(c(10,20,30,40,50)) 
#10 times larger than first vector

wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))
#lapply() function takes a list 
#is to convert the list returned by lapply()
#wbcn_n -n means that normalize

summary(wbcd_n$area_mean)

#Data preparation -> creat traind and test datasets
wbcd_train <- wbcd_n[1:469,]
wbcd_test  <- wbcd_n[470:569,]

#k-nn model,we will need to store these class in factor vectors
wbcd_train_labels <- wbcd[1:469,1]
wbcd_test_labels  <- wbcd[470:569,1]

library(class) # to classift our test instances,we will use a k-NN application form the "class" package 

wbcd_test_pred <-knn(train = wbcd_train,test = wbcd_test,cl=wbcd_train_labels,k=21)

#evaluatig model performance
library(gmodels) #we can create c across tabulation indicating the agreement between two vectors....

CrossTable(x=wbcd_test_labels,y=wbcd_test_pred,prop.chisq = FALSE)

#improving model performance
wbcd_z <- as.data.frame(scale(wbcd[-1])) #create z z-score standardized version wbcd data

summary(wbcd_z$area_mean)
#the mean of a z-score standardized variable should always be zero
wbcd_train <-wbcd_z[1:469,]
wbcd_test  <-wbcd_z[470:569,]
wbcd_train_labels <-wbcd[1:469,1]
wbcd_test_labels  <-wbcd[470:569,1]

wbcd_test_pred <- knn(train = wbcd_train,test = wbcd_test,cl=wbcd_train_labels,k=21)

CrossTable(x=wbcd_test_labels,y=wbcd_test_pred,prop.chisq = FALSE)
#98 percent of examples correctly classified

