
dataset = read.csv('train.csv')
test_set=read.csv('test.csv')
summary(dataset)

library(caTools)

#dealing with missing values
dataset$Age = ifelse(is.na(dataset$Age),
                     ave(dataset$Age,FUN=function(x) mean(x,na.rm = TRUE)),
                     dataset$Age);
test_set$Age = ifelse(is.na(test_set$Age),
                    ave(test_set$Age,FUN=function(x) mean(x,na.rm = TRUE)),
                     test_set$Age);

#Encoding categorical data

dataset$Sex = factor(dataset$Sex,
                           levels = c('female', 'male'),
                           labels = c(0,1))
dataset$Embarked = factor(dataset$Embarked,
                     levels = c('C', 'S','Q'),
                     labels = c(0,1,2))

test_set$Embarked = factor(test_set$Embarked,
                          levels = c('C', 'S','Q'),
                          labels = c(0,1,2))

test_set$Sex = factor(test_set$Sex,
                     levels = c('female', 'male'),
                   labels = c(0,1))
                         

#multiple regression

regressor = lm(formula = Survived ~  Embarked+Sex+Age+Pclass+SibSp,
               data = dataset)
summary(regressor)
test_set$Survived=round(predict(regressor,newdata = test_set))
test_set$Survived[test_set$Survived < 0] <- 0
test_set$Survived[test_set$Survived > 1] <- 1
submit <- data.frame(PassengerId = test_set$PassengerId, Survived = test_set$Survived)

write.csv(submit, file = "submission.csv", row.names = F)
read.csv('submission.csv')