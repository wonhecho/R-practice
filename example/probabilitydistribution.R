# 평균이 7.5이고, 표준편차가 4인 정규분포의 0.65 분위수

qnorm(0.65, mean = 7.5, sd=4)

#람다 값이 4인 포아송분포에서 7개의 난수를 생성하는 코드

rpois(7,lambda=4)

# *확률 문제, 12세 미만인 어린이가 하루에 마시는 물의 양이 평균 7.5이고, 표준편차가 1.5인 정규분포를 따른다고
# 했을때, 3-시그마 법칙 혹은 경험법칙으로 알려진 68-95-99.7의 법칙에 따르면 정규분포에서 생성한 난수가
# 평균적으로 표준편차의 세 배 이내의 범위안에서 포함될 확률은 99.7퍼센트이다.
# 평균값
ld.mean <- 7.5
# 표준편차
ld.sd <- 4

ld.mean+3*ld.sd
ld.mean-3*ld.sd
# 값이 2~12에서 난수들이 발생할 것이므로 그래프 구간은 [0,16]
x <- seq(0,16,length=100)
# 평균 및 표준편차 값을 사용해 12세 미만 어린이들의 하루에 마시는 물의 양의 확률밀도함수계산
# dnorm은 확률질량 확률밀도를 구하는데 씀
nd.height <- dnorm(x,mean=7.5,sd=1.5)
plot(x,nd.height,type="l",xlab="Liters per day",ylab="Density",main="Liters of water drank by school children < 12 years old")

# 어린이가 4리터 이하의 물을 마실 확률을 계산하려면 누적분포함수를 리턴하는 pnorm()함수를 이용하면 된다.
# lower.tail = TRUE 는 기본값으로 왼쪽부분의 넓이를 계산하고, lower.tail=FALSE를 하게되면 오른쪽을 계산하게 된다.
pnorm(4,mean=7.5,sd=1.5,lower.tail=TRUE)
# 따라서 이를 적용해서 그래프로 그리려면
ld.cdf = pnorm(x,mean=7.5,sd=1.5,lower.tail=TRUE)
plot(x,ld.cdf, type="l", xlab="Liters per day", ylab = "Cumulative probability")
# 이후 polygon()함수를 이용하여 어린이가 8리터 이상의 물을 마실 확률을 구할수 있다.
ld.lower <- 8
ld.upper <- 15
i <- x>=ld.lower & x<=ld.upper
# 정규분포그래프에서 8 이상인 부분을 빨간색으로 색칠
polygon(c(ld.lower,x[i],ld.upper),c(0,nd.height [i],0), col="red")

# 어린이가 물을 8리터보다 많이 마시는 확률은
pd <- round(pnorm(8,mean=7.5,sd<-1.5,upper.tail=FALSE),2)

