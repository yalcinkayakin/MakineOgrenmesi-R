#Akin Yalcinkaya
library(arules)
groceries <- read.transactions(file.choose(),sep = ",") #transaction look likes read.csv
groceries
summary(groceries)
inspect(groceries[1:5]) #inspect() function in combination with vector operators
itemFrequency(groceries[,1:3]) #itemfreguency function allows us to see the proportion of transactions that contain the item

#Visualizing item support
itemFrequencyPlot(groceries,support=0.1)
itemFrequencyPlot(groceries,topN=20)

#Visualizng the trasaction data
image(groceries[1:5]) #to visualize the entire sparse matrix,image function..to dispaly sparse matrix
image(sample(groceries,100))

apriori(groceries) #support 0.1,confidence 0.8

groceryrules <- apriori(groceries,parameter = list(support=0.006,confidence=0.25,minlen=2))
summary(groceryrules) #min support-0.006 min support-0,25

inspect(groceryrules[1:3])
#specific rules using inspect function...
#for example,first three rules in the groceryrules object
#lift value tells us how much more likely a customer is to buy whole milk relative to the average customer

#model performance
inspect(sort(groceryrules,by="lift")[1:5])
#sort function with vector operators,we can obtain a specific number of interesting rules...
#five rules according to the lift statistic can be examined using

berryrules <- subset(groceryrules,items %in% "berries")
#subset function providess a method to search for subsets of transacitons,items
inspect(berryrules)

write.csv(groceryrules,file = "groceryrules.csv" , sep = "," , quote = TRUE, row.names = FALSE)

groceryrules_df <- as(groceryrules, "data.frame")
str(groceryrules_df)




