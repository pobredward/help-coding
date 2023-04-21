rm(list = ls())
install.packages("party")
install.packages("readr")
library(datasets)
library(readr)
library(dplyr)
library(tidyr)
library(party)
library(rpart)
library(rpart.plot)


heart_data <- read.table("C:/Users/pobre/Desktop/Repositories/bio-bigdata-r/processed.cleveland.data", sep=",", header=F)

data(heart_data)
str(heart_data) #structure
colnames(heart_data) <- c("V1","V2","V3","V4","V5","V6","V7","V8","V9","V10",
                          "V11","V12","V13","V14")
heart_data

# 결측치가 포함된 행 제거
heart_data[heart_data == '?'] <- NA
heart_data <- na.omit(heart_data)

#chr -> numeric으로 변경경
heart_data$V12 <- as.numeric(heart_data$V12)
heart_data$V13 <- as.numeric(heart_data$V13)

#V14 factor로 변경
class(heart_data$V14)
heart_data$V14 <- as.factor(heart_data$V14)

set.seed(1234)  # 샘플 추출값 고정

#dataset 생성
ind <- sample(2, nrow(heart_data), replace=TRUE, prob=c(0.7, 0.3)) 
#sampling takes place from 1:2
trainData <- heart_data[ind==1,]
testData <- heart_data[ind==2,]

#Decision tree 생성
heart_data$V14 <- as.numeric(heart_data$V14)
heart_data_rpart <- rpart(V14 ~ ., data=trainData)
print(heart_data_rpart)

#Decision tree plotting
rpart.plot(heart_data_rpart)

#Classificatioin with test set
testPred <- predict(heart_data_rpart, newdata = testData, type = "class")

#분류결과 plotting
testPred
plot(testPred)

predict_table <- table(testPred, testData$V14)
names(dimnames(predict_table)) <- c("predicted", "observed")   

predict_table

#예측 결과와 실제 데이터의 정확도 확인
sum(testPred==testData$V14)/length(testPred)*100
