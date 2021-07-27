#KNN을 이용한 예측 예제
install.packages('class')
library('class')

data<-read.table("data/knndata.txt",header=T)
View(data)
str(data)
boxplot(data[,1:7])
scale <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
normalize<-function(x){
  return ((x-mean(x))/sd(x))
}
#scale함수를 사용하여 표준화
data2<-as.data.frame(lapply(data[,-8],scale))
#normalize함수를 사용하여 정규화
#data2<as.data.frame(lapply(data[,-8],normalize))
#boxplot 그래프를 통해 확인.
boxplot(data2[,1:7])
set.seed(10)
index<-sample(1:210,168,replace=F)
train<-data2[index,-8]
test<-data2[-index,-8]
train_label<- data[index,8]
test_label <- data[-index,8]
table(train_label)
table(test_label)
index
pre<- knn(train=train,cl=train_label,test=test,k=5)
pre
table(pred=pre,true=test_label)
