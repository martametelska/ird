rm(list = ls())
dane <- read.csv("C:/Users/marta/Downloads/archive (1)/turnover.csv", dec = '.')

View(dane)


### wymiary zbioru danych
dim(dane)

###liczba rekordów w klasach zmiennej objaœnianej
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

dane$anxiety <- ifelse(dane$anxiety <= 4, 'low', 'high')
View(dane)

install.packages("InformationValue")
library(InformationValue)

#sila predykcji
IV(dane$stag, dane$event, valueOfGood = 1)
IV(dane$age, dane$event, valueOfGood = 1)
IV(dane$anxiety, dane$event, valueOfGood = 1)
IV(dane$novator, dane$event, valueOfGood = 1)
IV(dane$selfcontrol, dane$event, valueOfGood = 1)
IV(dane$independ, dane$event, valueOfGood = 1)
IV(dane$extraversion, dane$event, valueOfGood = 1)
IV(dane$traffic, dane$event, valueOfGood = 1)
IV(dane$profession, dane$event, valueOfGood = 1)
IV(dane$industry, dane$event, valueOfGood = 1)
IV(dane$coach, dane$event, valueOfGood = 1)
IV(dane$gender, dane$event, valueOfGood = 1)
IV(dane$head_gender, dane$event, valueOfGood = 1)
IV(dane$greywage, dane$event, valueOfGood = 1)
IV(dane$way, dane$event, valueOfGood = 1)




########################################################################################################
# D R Z E W O
##################################################################################################
library(caret)
library(rpart) # do drzewa
library(rpart.plot) # do rysowania drzewa
library(caret)
inTraining <- createDataPartition(dane$event, p = .8, list = FALSE)
train <- dane[inTraining,]
test  <- dane[-inTraining,]


tree <- rpart(event ~ ., data = train, 
                  method = "class", 
                  control = list(maxdepth = 5), cp =0.005)   
tree2 <- rpart(event ~ ., data = train, 
              method = "class", 
              control = list(maxdepth = 5), cp =0.001)   

tree
#plot
rpart.plot(tree, under=FALSE, tweak=1.3, fallen.leaves = TRUE)
rpart.plot(tree2, under=FALSE, tweak=1.3, fallen.leaves = TRUE)
