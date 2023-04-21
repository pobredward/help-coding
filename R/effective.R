heart_data <- read.table("C:/Users/pobre/Desktop/Repositories/bio-bigdata-r/processed.cleveland.data", sep=",", header=F)

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
heart_data_ctree <- ctree(V14 ~ ., data=trainData)

#Classificatioin with test set
testPred <- predict(heart_data_ctree, newdata = testData)

predict_table <- table(testPred, testData$V14)
names(dimnames(predict_table)) <- c("predicted", "observed")

#예측 결과와 실제 데이터의 정확도 확인
sum(testPred==testData$V14)/length(testPred)*100
