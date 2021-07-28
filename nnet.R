# iris데이터를 이용한 신경망 에측 모형을 구축
data(iris)
iris.scaled <- cbind(scale(iris[-5]),iris[5])
set.seed(1000)
index <- c(sample(1:50,35),sample(51:100,35),sample(101:150,))
train <- iris.scaled[index,]
test <- iris.scaled[-index,]
library(nnet)
#size는 hidden layer, 5e-04는 0과 비슷하다는 뜻
model.nnet <- nnet(Species ~ ., data = train, size=2, decay = 5e-04)
summary(model.nnet)
#예측 방식
pre <- predict(model.nnet,test,type = "class")
pre
#결과 확인
actual <- test$Species
table(actual,pre)
