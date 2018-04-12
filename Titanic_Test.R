setwd("E:/Masters/Semester 2/Data Mining CS6405/Titanic/Titanic/data/external")

#Reading Data from the CSV file
titanic.df <- read.csv(file="train.csv",header=TRUE, na.strings = "")

#Check Dimension of the dataset
dim(titanic.df)

#Check the structure of the dataset
str(titanic.df)

#Check the total 'NA' datas
sum(is.na(titanic.df))

#Check the 'NA' datas according to the variables
sapply(titanic.df, function (x) sum(is.na(x)))

#Making the data variables as factors
titanic.df$Survived <- as.factor(titanic.df$Survived)
titanic.df$Pclass <- as.factor(titanic.df$Pclass)
#titanic.df$SibSp <- as.factor(titanic.df$SibSp)
#titanic.df$Parch <- as.factor(titanic.df$Parch)

#Extract the title from the name
extractTitle <- function(name, nop) {
  name <- as.character(name)
  nop <- as.integer(nop)
  
  if (length(grep("Miss.", name)) > 0) {
    return ("Miss")
  }else if (length(grep("Master.", name)) > 0) {
    return ("Master")
  } else if (length(grep("Mrs.", name)) > 0) {
    return ("Mrs")
  } else if (length(grep("Mr.", name)) > 0) {
    return ("Mr")
  } else {
    return ("Other")
  }
}

titles <- NULL
for (i in 1:nrow(titanic.df)) {
  titles <- c(titles, extractTitle(titanic.df[i,"Name"], titanic.df[i,"Parch"]))
}
titanic.df$Title <- as.factor(titles)

#Function for Setting the mode value in dataset for missing values
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#Setting mode value to Embarked variable
titanic.df$Embarked[is.na(titanic.df$Embarked)] <- getmode(titanic.df$Embarked)

#Dropping Cabin as there are more than 70% of values as NA
titanic.df$Cabin <- NULL

#Taking median of Age as the missing values
titanic.df$Age[is.na(titanic.df$Age)] <- median(titanic.df$Age, na.rm=TRUE)

familySize <- titanic.df$SibSp+titanic.df$Parch+1
titanic.df$FamilySize <- as.integer(familySize)
#Attaching Data to the R Environment
attach(titanic.df)

str(titanic.df)
#Logistic Regression Model
titanic.glm <- glm(Survived ~ Pclass+Sex+Age+Parch+Embarked+Title+FamilySize, family=binomial(link="logit"))
summary(titanic.glm)

#Removing Parch and Embarked as these are not significant
titanic.glm2 <- glm(Survived ~ Pclass+Sex+Age+Title+FamilySize:Age, family=binomial(link="logit"))
summary(titanic.glm2)

#Comparing the glm models
anova(titanic.glm2, titanic.glm)

#Use GGPlot Package for graph
library('ggplot2')

#Plotting the Age vs PClass
ggplot(titanic.df, aes(x=Age, fill = Survived))+
  geom_bar(binwidth = 0.5)+
  facet_wrap(~Pclass)+
  ggtitle("Pclass")+
  xlab("Sex")+
  ylab("Total Count")+
  labs(fill="Survived")

#Plotting Sex vs PClass
ggplot(titanic.df, aes(x = Sex, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) + 
  ggtitle("Pclass") +
  xlab("Sex") +
  ylab("Total Count") +
  labs(fill = "Survived")

#Plotting Title vs PClass
ggplot(titanic.df, aes(x = Title, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  ylim(0,300) +
  labs(fill = "Survived")

#Test dataset
titanic_test.df <- read.csv(file="test.csv",header=TRUE, na.strings = "")

#Check the total 'NA' datas
sum(is.na(titanic_test.df))

#Check the 'NA' datas according to the variables
sapply(titanic_test.df, function (x) sum(is.na(x)))

#Adding Survived variable to the test data with None value
titanic_test.df <- data.frame('Survived' = rep("None", nrow(titanic_test.df)), titanic_test.df[,])

#Cabin is not significant, hence dropping the column
titanic_test.df$Cabin <- NULL

titles <- NULL
for (i in 1:nrow(titanic_test.df)) {
  titles <- c(titles, extractTitle(titanic_test.df[i,"Name"], titanic_test.df[i,"Parch"]))
}
titanic_test.df$Title <- as.factor(titles)

titanic_test.df$Survived <- as.factor(titanic_test.df$Survived)
titanic_test.df$Pclass <- as.factor(titanic_test.df$Pclass)
#titanic_test.df$SibSp <- as.factor(titanic_test.df$SibSp)
#titanic_test.df$Parch <- as.factor(titanic_test.df$Parch)
str(titanic_test.df)
titanic_test.df$Age[is.na(titanic_test.df$Age)] <- median(titanic_test.df$Age, na.rm=TRUE)
titanic_test.df$Fare[is.na(titanic_test.df$Fare)] <- median(titanic_test.df$Fare, na.rm=TRUE)

familySize <- titanic_test.df$SibSp+titanic_test.df$Parch+1
titanic_test.df$FamilySize <- as.integer(familySize)

#Predicting the test data from the training model
titanic_predict <- c('Pclass', 'Title','FamilySize','Fare')
titanic_test_predict.df <- predict(titanic.glm2, titanic_test.df[,titanic_predict], type="response")
head(titanic_test_predict.df)
head(titanic_test.df)

y_pred = ifelse(titanic_test_predict.df > 0.5, 1, 0)
head(y_rf_pred)

output <- data.frame(cbind('PassengerId' = titanic_test.df$PassengerId, 'Survived' =y_pred))
write.csv(output, "output6.csv",row.names = FALSE)

#Random Forest Implementation
library(randomForest)
set.seed(1234)
rf <- randomForest(Survived ~ Pclass+Title+FamilySize+Fare, titanic.df)

predict(rf, titanic_test.df[,titanic_predict])
rf_predict <- predict(rf, titanic_test.df[,titanic_predict])
head(rf_predict)

output <- data.frame(PassengerId = titanic_test.df$PassengerId, Survived =rf_predict)
write.csv(output, "output13.csv",row.names = FALSE)