library(tidyverse)
library(ggplot2)
library(tidymodels)
library(kknn)
library(corrplot)
library(e1071)
library(caret)
library(pROC)
library(class)

## Load 'firestationTotal.csv' from 'FirestationScrape.R'
setwd("C:/Users/ejtur/OneDrive/Desktop/Data/projectCAP")
twk <- read.csv("firestationTotal.csv") %>% 
  drop_na(Precip) %>%  drop_na(TempMax) %>% 
  select(-c("X", "StartDate.x", "EndDate","SizeClass","State", "FIPS", "Week", 
            "Date", "D0.D4", "D1.D4", "D2.D4", "D3.D4", "D4"))
twk$TempMax <- twk$TempMax/10
human <- twk %>% filter(twk$Cause != "Missing data/not specified/undetermined", twk$Cause != "Other causes", twk$CauseClass == "Human")
human %>% count(Cause)

twk %>% count(Cause)

twk$CauseClass <- ifelse(twk$CauseClass=="Human", 1, 0)
twk$CauseClass <- as.factor(twk$CauseClass)
twk$Cause<- as.factor(twk$Cause)
#write.csv(twk, "twk.csv")
colSums(is.na(twk))
str(twk)
twknum <- twk %>% select(c("Size", "LAT", "LON", "BurnTime", "Precip", "TempMax", "DSCI"))
corrplot(cor(twknum))

#################
## Classification
#################
attach(twk)
test=1:4000 
train.Y=CauseClass[-test]
test.Y=CauseClass[test]

## Model 1: CauseClass~ Human or Naturally Caused Logistic Regression
CClogit <- glm(CauseClass~LAT+LON+Precip+TempMax+DSCI, data = twk, family = "binomial", subset = -test)
summary(CClogit) #AIC = 9491
CClogit.prob=predict(CClogit,twk[test,],type="response")
glm.pred=rep(0, 4000)
glm.pred[CClogit.prob>.25]=1
table(glm.pred,test.Y)
test.Y <- as.factor(if_else(test.Y==1, "Human", "Natural"))
glm.pred <- as.factor(if_else(glm.pred==1, "Human", "Natural"))
confusionMatrix(glm.pred, test.Y)
(3190+125)/4000 ## 82.88% accuracy @ 0.5 threshold 
125/(125+130) ## 49.02% accurate on Natural Wildfires @ 0.5 threshold 

(3315+23)/4000 ## 83.45% accuracy @ 0.25 threshold
26/(26+6) ## 82.14% accuracy on Naturall Wildfires @ 0.78 threshold

CClogit.roc = roc(response = test.Y, predictor = as.numeric(CClogit.prob))
plot(CClogit.roc, legacy.axes = TRUE, print.auc.y = 1.0, print.auc = TRUE)

hist(twk$Year)

## Model 2: Cause KNN Model w/o Naturally Caused Wildfires --------------- 10 Cause factors
detach("twkC")
twkCNN <- twk %>% select(-c("CauseClass", "Year", "CauseAgeCategory", "Size", "BurnTime", "None")) %>% filter(Cause != "Other causes", Cause != "Natural")
attach(twkCNN)
CNNstandardized.X=scale(twkCNN[,-2])
twkCNN %>% count(Cause)

trainCNN.X=CNNstandardized.X[-test, ]
testCNN.X=CNNstandardized.X[test,]
trainCNN.Y=Cause[-test]
testCNN.Y=as.factor(Cause[test])


set.seed(5)
CNNknn.pred=knn(trainCNN.X,testCNN.X,trainCNN.Y,k=12)
confusionMatrix(CNNknn.pred,testCNN.Y)
mean(CNNknn.pred==testCNN.Y)
## 47.325% accuracy w/ k=12


## nnet?
library(nnet)
library(caret)

twkCNN <- twk %>% select(-c("CauseClass", "Year", "CauseAgeCategory", "Size", "BurnTime", "None")) %>% filter(Cause != "Other causes", Cause != "Natural")

trainNN=twkCNN[-test, ]
testNN=twkCNN[test,]
traiCNN.Y=Cause[-test]
testNN.Y=as.factor(Cause[test])
testNN.Y
colSums(is.na(twkCNN))
?nnet
m=nnet(Cause~., data=trainNN, size=55)
m.pred=as.factor(predict(m, testNN, type="class"))
confusionMatrix(data=m.pred, reference = testNN.Y)

