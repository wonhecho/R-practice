# for() Loop를 이용한 Fibonacci

# 0 15개의 행렬을 미리 만든다
Fibonacci <- numeric(15)
# 앞의 2 행렬을 모두 1로 결정
Fibonacci[1:2] <- c(1,1) 
for(i in 3:length(Fibonacci)){Fibonacci[i] <- Fibonacci[i-2] + Fibonacci[i-1]}
# 결과 확인 [1,1,2,3,5,8,13,21,34,55,89,144,233,377,610]

#인덱스 행렬을 미리 만들어서 사용하는 것도 가능
Fibonacci_indx <- seq(3,15,by=1)

for (i in Fibonacci_indx) {Fibonacci[i]<- fibonacci[i-2]+Fibonacci[i-1]}

#apply() 함수를 이용한 평균구하기

m1 <- matrix(1:12, nrow=3)
#or m1<- matrix(1:12 ncol=4) 둘의 결과는 같다

meanByrow <- apply(m1,1,mean)
#열을 기준으로 평균을 구함.length(3)

meanByrow <- apply(m1,2,mean)
#행을 기준으로 평균을 구함.length(4)

m1plus3 <- apply(m1,c(1,2),function(x) x+3)
#행렬의 모든 원소에 x+3 함수 적용

#함수 내의 특정 인수 값을 지정하고 싶은 경우.

z <- c(1,4,5,NA,9,8,3,NA)
m2 <- matrix(z,nrow=4)
#NA를 제외하고 평균을 구함.
meanBycolumn <- apply(m2,2,mean,na.rm=TRUE)

#if 문 활용하기

x<-4
if(x<10){
x<-x+4
print(x)
}

#while문을 이용한 Fibonacci
num1<-1
num2<-1
Fibonacci <- c(num1,num2)
Fibonacci
count<-2
while(count<15){
  count <- count+1
  oldnum2<-num2
  num2<-num1+num2
  Fibonacci<-c(Fibonacci,num2)
  num1<-oldnum2
}
#repeat문을 이용한 Fibonacci repeat문은 break가 없으면 계속 실행된다.
num1 <- 1
num2 <- 1
Fibonacci <- c(num1,num2)
count<-2
repeat{
  count<-count+1
  oldnum2 <- num2
  num2 <- num1+num2
  Fibonacci<- c(Fibonacci,num2)
  num1<-oldnum2
  if(count>=15){break}
}






