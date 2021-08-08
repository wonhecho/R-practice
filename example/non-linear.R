# 비선형회귀
# 사용해야 하는 경우는 이론적으로 변수들 사이의 관계를 알고 있고, 그 관계가 선형이 아닐때

adsorption <- read.csv('adsorption.txt')

# nls()함수를 사용하면 등식을 적합시킬수 있다.
# adsorption 데이터에는 세가지 다른 온도 조건 하에서 측정한 세개의 다른
# 값인 T1,T2,T3가 포함되어 있다
#랭뮤어 공식에 있는 알파의 추정치를 먼저 구한다.

langmuir.T1 <- 'T1 ~(alpha.1*P)/(1+alpha.1*P)'
langmuir.T2 <- 'T2 ~(alpha.2*P)/(1+alpha.2*P)'
langmuir.T3 <- 'T3 ~(alpha.3*P)/(1+alpha.3*P)'
fit.T1<- nls(langmuir.T1, start = list(alpha.1=1),data=adsorption)
fit.T2<- nls(langmuir.T2, start = list(alpha.2=2),data=adsorption)
fit.T3<- nls(langmuir.T3, start = list(alpha.3=3),data=adsorption)
#세 개의 서로 다른 비선형 회귀식을 정의한다. 함수가 리턴한 객체들을 출력하면
# 온도별 알파의 추정치를 알수 있다.
fit.T1
fit.T2
fit.T3

# 시각화를 통한 비선형 탐색

body.measures <- read.csv('nhanes_body.txt')
attach(body.measures)
plot(age,height,xlab="Age",ylab="Height",main="Height vs Age")
# Age의 변수와 Height의 변수가 서로 선형적이지 않음을 알 수 있다.

#더 고도화된 그래프 그리기
library(hexbin)
bin<- hexbin(age,height)
plot(bin, xlab="Age", ylab="Height", main = "Height vs Age")
library(ggplot2)
qplot(age,height,data=body.measures, geom="hex", xlim=c(0,80),ylim=c(80,200),binwidth=c(5,5))
library(graphics)
smoothScatter(height ~ age, xlab="Age", ylab ="Height", main="Height vs Age")
scatter.smooth(age,height,xlab="Age",ylab="Height",main="Height vs Age",col='gray',pch=16)

#선형 프레임워크 확장

#다항회귀 선형 프레임워크를 비선형 관계로 확장하는 가장 간단한 방법
# 회귀식 내 몇몇 예측변수들에 대한 제곱 또는 세제곱항이 포함되어 있으면
# 그 항들을 새로운 예측변수처럼 취급하면 된다는 것이 아이디어.

fit.linear <- lm(height~age)
summary(fit.linear)
#결정계수(R-square)값이 0.304이다
plot(age,height,pch=16,col='gray',xlab='Age',ylab='Height',main='Height vs Age')
points(age,fit.linear$fitted, pch=16,cex=0.1)

#회귀진단을 위한 시각화 방법을 사용하더라도 같은 결론
plot(fit.linear,which=1)

#두 변수에 대한 이론적 배경공식이 없이 모형적합의 방법은 다항회귀이다.
#다항회귀는 선형회귀이며, 회귀식이 기저함수의 결합협태로 구축된 것이다.

fit.quadratic <- lm(height ~ age + I(age^2))
plot(fit.quadratic,which=1)
fit.cubic <- lm(height ~ age + I(age^2) + I(age^3))
plot(fit.cubic,which=1)
fit.quartic <- lm(height ~ age + I(age^2) + I(age^3) + I(age^4))
plot(fit.quartic,which=1)
fit.quintic <- lm(height ~ age + I(age^2) + I(age^3) + I(age^4) + I(age^5))
plot(fit.quintic,which=1)
fit.sixtic <- lm(height ~ age + I(age^2) + I(age^3) + I(age^4) + I(age^5) + I(age^6))
plot(fit.sixtic,which=1)
fit.septic <- lm(height ~ age + I(age^2) + I(age^3) + I(age^4) + I(age^5) + I(age^6) + I(age^7))
plot(fit.septic,which=1)

#높은 차수의 다항식을 사용하면 잔차그림이 좋아지게 되는 것을 볼 수 있다.
#무조건 높은 차수의 다항식을 적합시키는 접근법은 어리석다.
#3차식 이하가 적합하다.
#비수렴 문제는 많은 추정 알고리즘에서 발생하는데, 차수가 높을수록 보간값을 만들게 된다.
#중요한것은 보간점을 어떻게 분포시키느냐이다.

#런지현상을 보여주는 코드


runge <- function(x) {return(1/(1+x^2))}
x <-seq(-5,5, 0.5)
y <- runge(x)
plot(y~x)
fit.runge <- lm(y~x+I(x^2)+I(x^3)+I(x^4)+I(x^5)+I(x^6))
lines(fit.runge$fitted ~ x)

#이런 현상을 완화하려면 양쪽 끝부분에 더 많은 보간점들을 배치하고 중앙에는 듬성듬성 조금만 배치해준다.

x2 <- c(seq(-5,-4.05, 0.05),seq(-4, 4, 1), seq(4.05, 5, 0.05))
y2 <- runge(x2)
fit.runge.2 <- lm(y2~x2+I(x2^2)+I(x2^3)+I(x2^4)+I(x2^5)+I(x2^6))
lines(fit.runge.2$fitted ~ x2, col = 'red')

#키와 나이가 관계가 통상적인 다항함수처럼 보이지 않기 때문에 다항회귀를 시행하는 것은
#옳지 않다. 하지만, 두개의 영역으로 나누어 성장기와 쇠퇴기로 쪼개서 생각하면 더 좋을 것이다.
detach(body.measures)
youths <- which(body.measures$age %in% c(2:18))
adults <- which(body.measures$age %in% c(19:80))
body.measures.youths <- body.measures[youths,]
body.measures.adults <- body.measures[adults,]
attach(body.measures.youths)
plot(height~age)

# 이부분에서는 관계가 있는 것을 확인 할 수 있으며, 다항식으로 모형화하는 것이 자연스러운
# 종류의 곡선임도 알 수 있다.

fit.cubic.youths <- lm(height~age+I(age^2)+I(age^3))
plot(fit.cubic.youths,which=1)

# 잔차들이 랜덤한 오차처럼 흩어져 있는 매우 훌륭한 잔차그림을 볼 수 있다.

plot(age,height,pch=16,col='gray',xlab='Age',ylab='Height',main='Height vs Age(in youths)')
points(fit.cubic.youths$fitted ~ age, pch=16, cex=1)
summary(fit.cubic.youths)
# 결정계수가 0.9304로 매우 좋은 결과
detach(body.measures.youths)
detach(body.measures.adults)
attach(body.measures.adults)
plot(height~age)
fit.cubic.adults <- lm(height~age+I(age^2)+I(age^3))
plot(age,height,pch=16,col='gray',xlab='Age',ylab='Height',main='Height vs Age(in youths)')
points(fit.cubic.adults$fitted ~ age, pch=16, cex=1)
plot(fit.cubic.adults,which=1)
summary(fit.cubic.adults)
# 결정계수가 0.03589로 매우 나쁜 결과

# 스플라인 회귀는 고차의 다항식을 사용하지 않고 곡선 관계를 적합시키기 위해 사용한다.
# X값에 따라 회귀식이 변하는 조각별 함수이다.
# 아까 18세를 기준으로 쪼개는 부분을 매듭점이라고 한다. 스플라인은 매듭점에서 연속인 하무이다.
detach(body.measures.adults)
attach(body.measures)
fit.spline.smooth <- smooth.spline(height~age,nknots=4)
plot(age,height,pch=16,col='gray',xlab='Age',ylab='Height',main='Height vs Age')
lines(fit.spline.smooth, pch=16)


# 비모수적 비선형 방법
# Y = a + BX
# 이것이 아니더라도 X값의 관측치 그리고 X가 주언진 경우 관측한 Y값들을
# 이용한다면 X값에서 관측된 Y값의 평균으로 예측값을 정하면 된다
# 점별회귀 또는 점별 평귄 회귀

pointwise.regression <- function(x,y){
  X <- c(min(x):max(x))
  Y <- vector('numeric',length(X))
  for(i in X){
    Y[i-min(x)+1] <- mean(y[x==i])
  }
  return.frame<-data.frame(X,Y)
  return(return.frame)
}

expected.height <- pointwise.regression(age,height)
plot(age,height,pch=16,col='gray',xlab='Age',ylab="Height",main="Height Vs Age")
lines(expected.height)

pointwise.confint <- function(x,y){
  X <- (min(x):max(x))
  Y.list <- list('numeric',length(X))
  for ( i in X)
  {
    t.temp<- t.test(y[x==i])
    Y.list[[(i-min(x)+1)]] <- c(tmtemp$estimate[[1]],
                                t.temp$conf.int[1],
                                t.temp$conf.int[2])
  }
  Y.mat <- do.call('rbind',Y.list)
  return.frame <- data.frame(cbind(X,Y.mat))
  names(return.frame) <- c('X','Y','Lower.Y','Upper.Y')
  return(return.frame)
  }
boxplot(height ~ age, xlab='Age', ylab='Height',main='Height vs Age')

# 직사각형에 중앙을 지나는 선은 중앙값이고, 밑변과 윗변은 각각 제1사분위수 제 3사분위수이다.
#상자 바깥의 수염은 사분위수 범위를 즉 1.5배 만큼 그리는 것이 기본 귀칙이고, 수염 바깥의 개별 점들을
#이상점이라고 한다.

# 커널 회귀는 비선형자료에 부드러운 함수를 적합시키게 해주는 유연한 방법론이다.
# X값에서 Y의 평균값을 추정할 때 특정 X 값에서의 Y값만이 아닌 주변 값들의 가중분포 정보를
# 이용하는 방법으로 변 값들에 가중치를 부여하는 방식은 어떤 커널 함수를 선택하느냐 달라진다.
# 가장 간단한 방법으로는 내장함수인 ksmooth()를 사용하는 것인데 이 함수는 고정 평활량을 사용한다.
# 함수에는 데이터를 지정하는 인수 외에도 평활량 및 커널 등 명시적으로 지정해야 한다.

smooth.height <- ksmooth(age,height, bandwidth=10,kernel ='normal')
lines(smooth.height,col='red')

#결과물을 보면 나이가 어린 쪽에 키를 과대추정 하는 것으로 나타난다 이것은 과다평활
# 평활량을 줄이면 개선 될 수 있다.
smooth.height <- ksmooth(age,height,bandwidth=2,kernel='normal')

# 이 부분에서는 평활량이 괜찮아진 것으로 보이지만, 나이가 많은 사람의 경우에는
# 키가 조금씩 줄어드는 모습이 잘 나타나지 않는다.
# 하지만 ksmooth함수로는 이 모든것을 적합시키기가 불가능하다.
# 따라서 커널을 이용한 국소다항회귀를 이용한다 KernSmooth 패키지의 locpoly함수를 이용
# ksmooth는 단순한 가중평균값을 이용하였지만 이것은 가중국소다항회귀방법을 이용한다.
# locpoly()함수는 각 점마다 다른 평활량 값을 지정할 수 있는 장점이 잇다.

library("KernSmooth")
bandwidth.vals <- c(rep(1,20),rep(5,30))
smooth.height <- locpoly(age,height, gridsize = 50, bandwidth = bandwidth.vals)
lines(smooth.height, col = "blue")

# 이외에 다양한 평활량 선택법이 개발되었는데 평균제곱오차를 최소로 하는 것을 목표로 하는
# 플러그인 방법이 있다. dpill() 함수를 이용하면 영역 전체에서 사용할 최적의 평활량을 제공
# 영역별로 달리 사용할 수 있는 역역별 평활량을 계산해주기도 한다.
# 그 이외에는 교차확인법이 있는데 np 패키지의 npregbw()함수를 사용하면 된다.

h <- dpill(age, height, gridsize = 80)
plot(age, height, xlab = 'Age', ylab = 'Height', main = 'Height vs Age', col = 'gray', pch = 16)
smooth.height <- locpoly(age, height, bandwidth = h, gridsize = 80, kernel = 'normal')
lines(smooth.height, col = 'red')

smooth.height.2.males <- locpoly(age[gender ==1], height[gender == 1], drv = 2, bandwidth = h, gridsize = 80, kernel = 'normal')
smooth.height.2.males$x[smooth.height.2.males$y == min(smooth.height.2.males$y)]
smooth.height.2.females <- locpoly(age[gender ==2], height[gender == 2], drv = 2, bandwidth = h, gridsize = 80, kernel = 'normal')
smooth.height.2.females$x[smooth.height.2.females$y == min(smooth.height.2.females$y)]

# 비선형 데이터르ㄹ적합시키는 방법은 두개의 변수 사이에서 가능
# 여러개일경우에는 loess()라는 다차원 커널 평활을 이용해야 된다
# 이전과는 다른점은 최근접이운방법으로 평활량을 정한다는 것이다.

male.weight <- weight[gender==1]
male.age <- age[gender==1]
male.height<- height[gender==1]
weight.fit <- loess(male.weight ~ male.age * male.height,span=1,family='gaussian')

# 주어진 나이와 키에 대한 평균 몸무게를 추정할 수 있다.
# 이 모형을 이용해 예측치를 계산하려면 s/w에 의존해야 한다.
age.vals <- seq(from=2,to=85,by=1)
height.vals <- seq(from = 80,to=200,by=1)
predicted.weight <- predict(weight.fit, newdata = expand.grid(male.age=age.vals,male.height=height.vals))
persp(age.vals,height.vals,predicted.weight, theta=40, xlab='Age', ylab= 'Height', zlab="Weight")

# np비선형 분위수 회귀
# 기본적인 추세 및 독립변수들을 사용한 종속변수의 값을 예측할 목적으로
# 자료점에 곡선 및 곡면을 적합시키는 것을 시도했지만,
# 특정 구성원의 모집단 내 상대적 쉰위 혹은 위치를 알아내기 위한 곡선이 필요하다면
# np 패키지의 npqreg()함수를 이용하면 된다.

detach(body.measures)
detach(body.measures.youths)
juveniles <- which (body.measures$age %in% c(2:10))
body.measures.juveniles.1 <- body.measures[juveniles,]
attach(body.measures.juveniles.1)
body.measures.juveniles.2 <- body.measures.juveniles.1[order(age),]
detach(body.measures.juveniles.1)
attach(body.measures.juveniles.2)
install.packages("np")
library(np)
bw.est <- npcdistbw(formula=height~age)df
qreg.10th <- npqreg(bws=bw.est,tau=0.1)
qreg.25th <- npqreg(bws=bw.est,tau=0.25)
qreg.50th <- npqreg(bws=bw.est,tau=0.5)
qreg.75th <- npqreg(bws=bw.est,tau=0.75)
qreg.90th <- npqreg(bws=bw.est,tau=0.9)
plot(height~age,type='n',xlab='Age',ylab = "Height",main="Quantiles of age for height",
     xaxp = c(2,10,(10-2)),xaxp=c(80,160,(160-80)/10))



