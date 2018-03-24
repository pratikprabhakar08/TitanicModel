setwd("E:/Masters/Semester 2/Data Mining CS6405/Titanic/Titanic/data/external")

#Reading Data from the CSV file
titanic.df <- read.csv(file="train.csv",header=TRUE, na.strings = "")
#Attaching Data to the R Environment
attach(titanic.df)
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
titanic.df$SibSp <- as.factor(titanic.df$SibSp)
titanic.df$Parch <- as.factor(titanic.df$Parch)

#Taking median of Age as the missing values
titanic.df$Age[is.na(titanic.df$Age)] <- median(titanic.df$Age, na.rm=TRUE)

#Function for Setting the mode value in dataset for missing values
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#Setting mode value to Embarked variable
titanic.df$Embarked[is.na(titanic.df$Embarked)] <- getmode(titanic.df$Embarked)

#Dropping Cabin as there are more than 70% of values as NA
titanic.df$Cabin <- NULL

#Logistic Regression Model
titanic.glm <- glm(Survived ~ Pclass+Sex+Age+SibSp+Parch+Embarked, family=binomial(link="logit"))
summary(titanic.glm)

#Installing GGPlot Package for graph
install.packages("ggplot2")
library('ggplot2')

#Plotting the Age vs PClass
ggplot(titanic.df, aes(x=Age, fill = Survived))+
  geom_bar(binwidth = 0.5)+
  facet_wrap(~Pclass)+
  ggtitle("Pclass")+
  xlab("Sex")+
  ylab("Total Count")+
  labs(fill="Survived")