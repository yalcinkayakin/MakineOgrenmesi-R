
letters <- read.csv(file.choose(),header = TRUE,sep = ",")
letters
str(letters)

letters_train <- letters[1:16000,]
letters_test  <- letters[16001:20000,]

library(kernlab)

letter_classiier <- ksvm(letter ~ ., data=letters_train,kernel="vanilladot")
letter_classiier

letter_predictions <- predict(letter_classiier,letters_test)
head(letter_predictions)

table(letter_predictions,letters_test$letter)

agreement <- letter_predictions == letters$letter
table(agreement)

prob.table(table(agreement))

letter_classiier_rbf <- ksvm(letter ~ ., data=letters_train,kernel="rbfdot")

letter_predictions_rbf <- predict(letter_classiier_rbf,letters_test)

agreement_rbf <- letter_predictions_rbf == letters_test$letter
table(agreement_rbf)

prop.table(table(agreement_rbf))
