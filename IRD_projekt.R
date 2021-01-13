rm(list = ls())
dane <- read.csv("C:/Users/marta/Downloads/archive (1)/turnover.csv", dec = '.')

View(dane)


### wymiary zbioru danych
dim(dane)

###liczba rekordów w klasach zmiennej objaśnianej
table(dane$event)

###ile 0 ile 1 (w %)
prop.table(table(dane$event))

### braki danych
sum(is.na(dane))

### typy danych
str(dane)

### boxploty
boxplot(dane$event)
boxplot(dane$age)
boxplot(dane$extraversion)
boxplot(dane$independ)
boxplot(dane$selfcontrol)
boxplot(dane$anxiety)
boxplot(dane$novator)
boxplot(dane$stag)
histogram(dane$stag)
histogram(dane$age)
histogram(dane$extraversion)
histogram(dane$independ)
histogram(dane$selfcontrol)



###


install.packages("smbinning")
library(smbinning)
smbinning(df=dane,y= "event",x= "age")
smbinning(df=dane,y= "event",x= "stag")
smbinning(df=dane,y= "event",x= "extraversion")
smbinning(df=dane,y= "event",x= "independ")
smbinning(df=dane,y= "event",x= "selfcontrol")
smbinning(df=dane,y= "event",x= "anxiety")
smbinning(df=dane,y= "event",x= "novator")


install.packages("InformationValue")
library(InformationValue)

#sila predykcji

IV(dane$stag, dane$event, valueOfGood = 1)
IV(dane$traffic, dane$event, valueOfGood = 1)
IV(dane$profession, dane$event, valueOfGood = 1)
IV(dane$industry, dane$event, valueOfGood = 1)
IV(dane$coach, dane$event, valueOfGood = 1)
IV(dane$gender, dane$event, valueOfGood = 1)
IV(dane$head_gender, dane$event, valueOfGood = 1)
IV(dane$greywage, dane$event, valueOfGood = 1)
IV(dane$way, dane$event, valueOfGood = 1)


quantile(dane$age, probs= seq(0,1,0.1) )

library(tidyverse)
library(dplyr)


dane <- dane %>%
  mutate(agecat = case_when(age<23 ~ 1,
                            23 <= age & age <25 ~ 2,
                            25 <= age & age <26 ~ 3,
                            26 <= age & age <28 ~ 4,
                            28 <= age & age <30 ~ 5,
                            30 <= age & age <32 ~ 6,
                            32 <= age & age <34 ~ 7,
                            34 <= age & age <37 ~ 8,
                            37 <= age & age <41 ~ 9,
                            41 <= age ~ 10))

dane$agecat <- as.factor(dane$agecat)
IV(dane$agecat, dane$event, valueOfGood = 1)

dane <- dane %>%
  mutate(anxietycat = case_when(anxiety <= 4 ~ 1,
                             4 < anxiety ~ 2))
dane$anxietycat <- as.factor(dane$anxietycat)

IV(dane$anxietycat, dane$event, valueOfGood = 1)

dane <- dane %>%
  mutate(novatorcat = case_when(novator <= 4 ~ 1,
                                4 < novator ~ 2))
dane$novatorcat <- as.factor(dane$novatorcat)

IV(dane$novatorcat, dane$event, valueOfGood = 1)


dane <- dane %>%
  mutate(extraversioncat = case_when(extraversion <= 4 ~ 1,
                                4 < extraversion ~ 2))
dane$extraversioncat <- as.factor(dane$extraversioncat)

IV(dane$extraversioncat, dane$event, valueOfGood = 1)

dane <- dane %>%
  mutate(selfcontrolcat = case_when(selfcontrol <= 4 ~ 1,
                                     4 < selfcontrol ~ 2))
dane$selfcontrolcat <- as.factor(dane$selfcontrolcat)

IV(dane$extraversioncat, dane$event, valueOfGood = 1)

dane <- dane %>%
  mutate(independcat = case_when(independ <= 4 ~ 1,
                                    4 < independ ~ 2))
dane$independcat <- as.factor(dane$independcat)

IV(dane$independcat, dane$event, valueOfGood = 1)

quantile(dane$stag, probs= seq(0,1,0.1) )
quantile(dane$stag, probs= seq(0,1,0.25) )


dane <- dane %>%
  mutate(stagcat = case_when(stag< 4.96 ~ 1,
                            4.96 <= stag & stag <9.12 ~ 2,
                            9.12 <= stag & stag <13.88 ~ 3,
                            13.88 <= stag & stag <19.09 ~ 4,
                            19.09 <= stag & stag <24.35 ~ 5,
                            24.35 <= stag & stag <34.2 ~ 6,
                            34.2 <= stag & stag <45.31 ~ 7,
                            45.31 <= stag & stag <58.59 ~ 8,
                            58.59 <= stag & stag <86.14 ~ 9,
                            86.14 <= stag ~ 10))

dane$stagcat <- as.factor(dane$stagcat)
IV(dane$stagcat, dane$event, valueOfGood = 1)




########################################################################################################
# D R Z E W O rpart
##################################################################################################
library(caret)
library(rpart) # do drzewa
library(rpart.plot) # do rysowania drzewa
library(caret)
install.packages('caret')


install.packages('ROCR')
library(ROCR) # do krzywej ROC

dane2 <- subset(dane, select = c("stagcat", "event", "gender", "agecat", "industry","profession", "traffic", "coach", "head_gender", "greywage","way", "extraversioncat","anxietycat", "novatorcat", "selfcontrolcat", "independcat"))


View(dane2)
inTraining <- createDataPartition(dane2$event, p = .8, list = FALSE)
train <- dane2[inTraining,]
test  <- dane2[-inTraining,]


tree <- rpart(event ~ ., data = train, 
                  method = "class", 
                  control = list(maxdepth = 5), cp =0.005)   
tree2 <- rpart(event ~ ., data = train, 
              method = "class", 
              control = list(maxdepth = 4), cp =0.005)   

#plot
rpart.plot(tree, under=FALSE, tweak=1.3, fallen.leaves = TRUE)
rpart.plot(tree2, under=FALSE, tweak=1.3, fallen.leaves = TRUE)




#ocena


CM <- list()
CM[["tree2"]] <- table(predict(tree2, new = test, type = "class"), test$event)

EvaluateModel <- function(classif_mx)
{
  # Sciagawka: https://en.wikipedia.org/wiki/Sensitivity_and_specificity#Confusion_matrix
  true_positive <- classif_mx[1,1]
  true_negative <- classif_mx[2,2]
  condition_positive <- sum(classif_mx[ ,1])
  condition_negative <- sum(classif_mx[ ,2])
  predicted_positive <- sum(classif_mx[1, ])
  predicted_negative <- sum(classif_mx[2, ])
  # Uzywanie zmiennych pomocniczych o sensownych nazwach
  # ulatwia zrozumienie, co sie dzieje w funkcji
  accuracy <- (true_positive + true_negative) / sum(classif_mx)
  MER <- 1 - accuracy # Misclassification Error Rate
  # inaczej: MER < - (false_positive + false_positive) / sum(classif_mx)
  precision <- true_positive / predicted_positive
  sensitivity <- true_positive / condition_positive # inaczej - Recall / True Positive Rate (TPR)
  specificity <- true_negative / condition_negative
  F1 <- (2 * precision * sensitivity) / (precision + sensitivity)
  return(list(accuracy = accuracy, 
              MER = MER,
              precision = precision,
              sensitivity = sensitivity,
              specificity = specificity,
              F1 = F1))
  # Notacja "accuracy = accuracy" itd. jest potrzebna,
  # zeby elementy listy mialy nazwy.
}

EvaluateModel(CM[["tree2"]])


prognoza_ciagla <- predict(tree2, newdata = test)
prognoza_ciagla <- as.vector(prognoza_ciagla[,2])

# krzywa ROC - potrzebuje "ciaglej" prognozy
plot(performance(prediction(prognoza_ciagla,test$event),"tpr","fpr"),lwd=2, colorize=T) 

###auc o co chodziiiiii
performance(prediction(prognoza_ciagla, test$event),"auc")

auc(plot(performance(prediction(prognoza_ciagla,test$event))))


plot(performance(prediction(prognoza_ciagla,test$event),"sens","spec"),lwd=2) 



