training_set=read.csv('train.csv')
test_set=read.csv('test.csv')

#adding the missing column in test set in order to row bind later
test_set$Survived <-0

#combining both the datasets
dataset = rbind(training_set,test_set)

#Encoding categorical data

dataset$Sex = factor(dataset$Sex,
                     levels = c('female', 'male'),
                     labels = c(0,1))
dataset$Embarked = factor(dataset$Embarked,
                          levels = c('C', 'S','Q'),
                          labels = c(0,1,2))

#checking for columns with missing values
colSums(is.na(dataset) | dataset=='')

#missing values in fare filled by the median
dataset$Fare[is.na(dataset$Fare)==TRUE] = median(dataset$Fare, na.rm=TRUE)

#missing values of age substituted by the average of age of each class
ggplot(dataset,aes(Pclass,Age)) +
  geom_boxplot(aes(fill=factor(Pclass)),alpha=0.5) +
  ggtitle("Age distribution based on Pclass")

dataset$Age[ dataset$Pclass == '1' & is.na(dataset$Age)] <- round(mean(subset(dataset,dataset$Pclass==1)$Age, na.rm=TRUE),0)
dataset$Age[ dataset$Pclass == '2' & is.na(dataset$Age)] <- round(mean(subset(dataset$Age,dataset$Pclass==2)$Age, na.rm=TRUE),0)
dataset$Age[ dataset$Pclass == '3' & is.na(dataset$Age)] <- round(mean(subset(dataset$Age,dataset$Pclass==3)$Age, na.rm=TRUE),0)

#missing values of embarked

# Extracting the rows which contain the missing Embarked values
subset(dataset, is.na(dataset$Embarked)==TRUE|dataset$Embarked=='')

#same cabin,same fare--
#plot of embarked and fare
ggplot(dataset,aes(Embarked,Fare)) +
  geom_boxplot(aes(fill=factor(Embarked)),alpha=0.5) +
  geom_hline(aes(yintercept=80), colour='red', linetype='dashed', size=2) +
  ggtitle("Age distribution based on Pclass")
  ggtitle("Embarked vs Fare")
  
#the line with fare=80$ corresponds close enough to the box plot of people who embarked from port C hence replacing
#the missing values with that
dataset$Embarked[dataset$Embarked ==""] <- "C"
  
  
#Decision Trees
library(rpart)
fit <- rpart(Survived ~ Pclass + Sex + Age +SibSp + Parch + Fare + Embarked,
             data=training_set,
             method="class")

rpart.plot(fit, extra=4)

#predictions
y_predict <- predict(fit, test_set, type = "class")
submit <- data.frame(PassengerId = test_set$PassengerId, Survived = y_predict)
write.csv(submit, file = "submission_dtree.csv", row.names = FALSE)


