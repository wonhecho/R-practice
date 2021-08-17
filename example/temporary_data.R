# 평균이 10이고, 표준편차가 3인 정규분포를 따르는 난수 4개를 생성하려면
rnorm(4, mean=10 , sd=3)
# rpois(10, lambda = 3 )을 실행하면 집중도의 값이 3인 포아송분포를 따르는 10개를 생성할 수 있다.
# sample() 함수를 사용하면 기존의 벡터로부터 랜덤하게 표본을 추출할 수 있다.
# 함수는 주어진 벡터의 원소 중에서 size 인수에 지정한 개수만큼 랜덤하게 추출해주는데
# replace 인수 값이 TRUE 인지 FALSE인지에 따라 복원 및 비복원 추출해준다.
sample(1:100, size=5, replace=FALSE)
# 주사위를 6번 던지는 시행
sample(1:6, size=6, replace=TRUE)
# 문자형 벡터에서도 sample()를 사용할 수 있다.
fruits <- c("apple","orange","strawberry","lemon","clementine")
sample(fruits, size=2, replace=TRUE)

# 의사난수를 시뮬레이션 하는 방법.
# 의사난수라는 표현은 진정한 의미의 난수가 아니라 난수발생 알고리즘에 의해 만들어지는 숫자
# 0과 1사이에서 일련의 의사 난수를 생성하는 방법
# 승법 합동 의사난수 발생기를 이용해 독립이고, 균일분포를 따르는 확률변수들을 시뮬레이션 하는 것
# 의사난수를 생성하는 데 사용할 수 있는 다양한 방법이 있다.
# m을 크기가 큰 소수로, k는 10보다 작은 정수로서 가급적 m의 제곱근에 가까운 값이 되도록 정하면
# 의사 난수를 생성할 수 있다.

getRandomNbs <- function(n,m,seed){
  pseudorandom.numbers <- numeric(n)
  k <- round(sqrt(m)) -2
  for (i in 1:n){
    seed <- (k*seed) %% m
    pseudorandom.numbers[i] <- seed/m
  }
  return(pseudorandom.numbers)
}

# 초기값은 27000으로, 소스 m의 값은 334,753으로 하여 5개의 의사난수를 생성
getRandomNbs(5,334753,27000)

# 사용자가 직접 작성한 의사난수 발생기를 이용하면 생성된 숫자들이 균일 분포를 따르는지
# 서로 독립인지 확인하는 것은 중요한 일
# 정규분포가 따르는지 확인해보려면, 카이제곱 검정을 이용해야됨.
rng.chisq <- function(x,m){
  Obs <- trunc(m*x)/m
  Obs <- table(Obs)
  p <- rep(1,m)/m
  Exp <- length(x) * p
  chisq <- sum((Obs-Exp)^2/Exp)
  pvalue <- 1-pchisq(chisq,m-1)
  results <- list(test.static=chisq, p.value = pvalue, df=m-1)
  return(results)
}
# x는 의사난수 발생기에 의해 [0,1] 구간내에서 생성된 난수
# m은 카이제곱 검정에 사용할 부분 구간의 개수이다.
v <- getRandomNbs(1000,334753,27000)
rng.chisq(v,m=5)
# p-value가 충분히 크기 때문에 귀무가설 즉, 생서된 난수가 균일분포를 따른다는 것을 기각할 만한
# 증거를 찾을 수 없다.
# 그렇다면 독립인지를 알아보자
# 시차그림 함수
lag.plot(v)

# 직접 의사난수를 방생시키는 함수를 작성할 수 있는데, 내장함수인 runif()함수를 사용하면
# 구간 [a,b] 내에서 균일분포를 따르는 난수를 생성 할 수 있다.
# runif() 함수의 인수는 min = a, max = b로 지정할 수 있다.
# 초기값을 지정하지 않으면 함수 내부에서 알아서 결정하고, getRandomNb()함수와는 다른 알고리즘을 사용해 의사난수 생성

a <- runif(n=5,min=0,max=1)
# 초기값을 특정하고 싶으면 set.seed() 함수를 이용해야 한다.

runif(3)
# 초깃값을 사용하고 runif
set.seed(27000)
runif(3)
runif(3)

# 즉 초깃값을 설정하게 되면, 미리 결정된 수열을 얻을 수 있게 된다.

# 베르누이 확률변수를 모의실험하는 방법.
# 베르누이 시행은 두 가지의 가능한 결과, 성공 및 실패만 관측

set.seed(23457)
guessed.correctly <- runif(30)
table(guessed.correctly < 0.25)
# rbern은 정답을 맞힐 확률 p를 인수로 지정해 맞힐 문제의 개수를 시뮬레이션 할 수 있다.
# p인수에 지정된 확률을 성공 확률로 하여 성공을 1 실패를 0으로 리턴한다.
guessed.correctly <- rbern(n=30,p=.25)
guessed.correctly
sum(guessed.correctly)

# 이항활률 변수를 모의실험하려면 rbinom()함수를 사용한다.
# size 인수를 1로 지정해 실행
set.seed(23457)
rbinom(n=30,size=1,p=.25)

# 문제 : 시간당 100개의 유리병을 생산하는 공장에서 출고된 제품 중 불량품의 갯수를 모의실험
# 불량률은 0.05이고, 하루에 10시간 작업한다고 생각했을때

set.seed(23457)
rbinom(n=10,size=100,p=0.05)

# 포아송 확률변수는 보통 주어진 시구간 내에서 관측된 특정 사건의 발생횟수를 모델링
# rpoi()함수를 사용해 모의실험 
# 15년간 발생하게 될 부상 건수를 매년 43600건 정도 발생하는 것으로 가정

lag.plot(rpois(15,43600))

# 지수분포 확률변수 
# 컴퓨터가 평균적으로 6년만에 고장이 난다고 가정하고 교실에 있는 25대의 동종 컴퓨터의 수명을 rexp()함수를 이용해 
# 모의 실험
set.seed(453)
computer.lifetime <- rexp(25,1/6)
hist(computer.lifetime, probability=TRUE, col="gray", main="Exponential curve for computers with a mean time
     to failure of 6 years", cex.lab=1.5, cex.main=1.5)
curve(dexp(x,1/6),add=T)

# 몬테카를로 모의실험
# 확률변수의 기댓값을 계산하는 것은 중요
# 결정론적인 알고리즘을 정용하거나 딱 떨어지는 수식으로 계산하기 어렵다
# 설명변수와 반응변수 간의 관계를 상수항, 지수함수, 거듭제곱, 로그 등 몇가지 기본적인
# 함수들로만 표현하는 것이 매우 어렵거나 불가능한 경우가 있다.
# 반복적인 난수 생성을 통해 미지의 확률분포를 비슷하게 근사시키고, 넓은 범위의 다양한 문제에 대한
# 해결책을 제공하는 알고리즘 방법
# 확률변수 X 기댓값 E(X) 근사값을 찾기 위해 X와 같은 분포를 갖는 m개의 난수를 생성해
# 이들의 평균값을 계산해 사용하면 된다.
# m이 충분히 큰 경우 Xm은 참값 E(X)에 대한 좋은 근사값이 되기 때문이다.
# 대수의 법칙에 의하면 m이 커짐에 따라 Xm의 분포의 중심위치가 모집단 평균에 가까워지는 경향성이 강해진다.
# 실제 분포의 이론적 밀도함수에 가까워짐을 확인 할 수도 있다.

# 중심극한정리
# 표본 크기가 커짐에 따라 Xm의 분포가 점점 정규분포랑 닮아간다는 것이다.
set.seed(983)
x.exp <- rexp(1000,rate=0.4)
mean(x.exp)
#평균값
sd(x.exp)
#표준편차
hist(x.exp, probability=TRUE, col=gray(0.8),main="",cex.axis=1.2,cex.lab=1.5)

# 난수들이 정규분포랑 거리가 있다. 이번에는 rate값이 0.4인 지수분포로부터 100개의 난수를 생성해
# 평균값을 계산하는 작업을 500회 반복해 얻은 500개의 평균값의 분포가 어떻게 되는지 확인

x.exp.means <- numeric(100)
for(i in 1:500){
  x.exp.means[i] <- mean(rexp(100,0.4))
}
hist(x.exp.means,probability=TRUE , col=gray(0.8), main="", cex.axis=1.2,cex.lab=1.5)

# 중심극한정리에 의해 근사적으로 정규분포를 따르는 평균값들의 표준편차의 값은 
# 모집단 분포의 표준편차 와 다음 관계를 갖게 된다.

mean(x.exp.means)
sd(x.exp.means)

# 이 예제에서 모집단 분포의 평균 및 표준편차는 모두 2.5이므로 다음 코드를 실행하면 계산할 수 있다.
2.5/sqrt(100)

# 몬테카를로 방법으로 구한 값과 방금 계산한 이론적인 값이 매우 비슷함.
# 다른 분포를 따르는 확률변수에도 동일하게 적용
# 메인보드의 평균수명은 8년이고 하드드라이브는 4년이라고 한다.
# 부품의 수명에 대해 지수분포를 가정하고 난수를 발생시켜 컴퓨터 수명의 평균 및 분산을 추정해보자

moterboard.fail <- rexp(10000, rate=1/8)
hard.drive.fail <- rexp(10000, rate=1/6)
par(mfrow=c(1,2))
hist(moterboard.fail, probability=TRUE, col=gray(0.8),main = "Simulated motherboard time to failure", cex.axis=1.2, cex.lab=1.5)
hist(hard.drive.fail, probability=TRUE, col=gray(0.8), main = "Simulated hard drive time to failure", cex.axis=1.2, cex.lab=1.5)

ind <- (hard.drive.fail - moterboard.fail) >0

# 이 인덱스 정보를 이용하면 컴퓨터 고장 시점을 얻을 수 있다

computer.fail <- c(moterboard.fail[ind], hard.drive.fail[!ind])
mean(computer.fail)
var(computer.fail)
sd(computer.fail)

#mc2d 패키지 사용

library(mc2d)

# 1차원 몬테카를로 모의실험은 난수들을 사용해 랜덤한 변이가 모델링 대상 시스템의
# 민감도, 성능, 신뢰도 등에 미치는 영향을 평가하기 위한 방법이다.
# EX) 방출된 농업 빛 산업 폐기물 때문에 비소에 높은 수준으로 오염된 물을 마시는 것이 암
# 발병 위험에 미치는 영향을 평가하는 모형을 생각해본다
# 조건 1. 마을에 공급되는 수돗물의 비소에 대한 평균 집적도는 12ppb로서 최대 허용 기준치 10에 초과
# 조건 2. 전체 주민의 43.2 %만 수돗물을 마셨고, 생수와 섞어마신 비율은 22.6% 생수만 마신 사람은 34.2%
# 조건 3. 수돗물과 생수를 섞어 마시면 비소 노출을 1/3수준으로 줄일 수 있고, 생수만 마시면 1/5수준으로 줄임
# 조건 4. 주민 개인이 마시는 물의 양 평균 6, 형태모수가 60인 역가우스 분포
# 조건 5. 비소 섭취량은 포아송 분포를 따른다
# 조건 6. 비소 섭취에 의한 암 발병 확률은 one-hit모형에 의한 투여-반응 관계로 모델링 되는데
# 매번 비소를 섭취할 때마다 암에 걸릴 확률은 0.0013이다
# 조건 7. 모형 내에 다른 불확실성 요인은 없다.

#난수를 생성
rnorm(4,mean=10,sd=3)
library(statmod)
library(mc2d)

# 마을 사람들 1001명
ndvar(1001)
# 평균 집적도 12
arsenic.conc <- 12
#rempiricalD() 함수로 난수를 생성해 1001명의 주민들이 물을 마시는 습관을 시뮬래이션 한다.
# 그리고 mcstoc()함수를 사용해 나중에 쓸 몬테카를로 객체를 생성하는데 필요한 변수와 관련된 몬테카를로 노드를 생성
drinking.habit <- mcstoc(func=rempiricalD, values=c(1,1/3,1/5),prob=c(0.432,0.226,0.342))
# rinvgauss() 함수를 사용해 역가운스분포를 따르는 난수를 생성해 개인별 물 섭취랑을 시뮬레이션
tap.water.drank <- mcstoc(rinvgauss,mean=6,shape=60)
# 개인별 평균 비소 노출량 계산
arsenic.exposure <- arsenic.conc * drinking.habit * tap.water.drank
#rpois() 함수를 이용해 평균 비소 노출량을 lambda값으로 갖는 난수 생성
arsenic.dose <- mcstoc(rpois,lambda=arsenic.exposure)
# 배소 섭취에 의한 암 발병 확률로 부터 리스크를 계산
prob.per.hit <- 0.0013
risk <- 1 - (1-prob.per.hit)^arsenic.dose
# mc()를 이용해 모든 mcnode 객체를 결합해 몬테카를로 모의실험 결과 분석
As1 <- mc(drinking.habit,tap.water.drank,arsenic.exposure,arsenic.dose,risk)
summary(As1)
# 비소 노출에 의한 위험을 확인할 수 있다. 

# 2차원 몬테카를로 모의실험

# 앞에 예제에는 모형 파라미터에 불확실성 요인이 없다고 가정했는데 실제로는 그러지 않을 수 있고,
# 이렇게 되면 2차원 몬테카를로 시뮬레이션을 사용할 수 있다.
# 하나의 차원은 개별 값의 변이를 위해, 다른 하나의 차원은 모형 내 파라미터에 내재된 불확실성을 위해 사용
# 변이와 불확실성은 별도로 시뮬레이션 하게 된다. 
# V, U, VU, O 차례로 변이, 불확실성, 번이와 불확실성 둘 다, 둘다 아님을 의미

# EX) 마을 수돗물에 따른 암 발병 위험을 추정하기 위해 이차원 몬테카를로 시뮬레이션을 적용하는 예제
# 비소 집적도를 정확히 알 수 없으며 그에 따른 불확시성이 평균 2이고, 표준편차가 0.5인 정규분포에 의해
# 잘 성명되는 상황을 가정, 비소에 노출되었을 때 암 발병 확률이 0.0013으로 고정되어 있지 않고,
# 0.00001부터 0.0017 사이의 구간에서 정의도니 균일분포를 따른다고 가정

set.seed(223)
library(statmod)
library(mc2d)
ndunc(101)
ndvar(1001)
arsenic.conc <- mcstoc(rnorm,type="U",mean=2,sd=0.5)
drinking.habit <- mcstoc(func = rempiricalD, type = "V", values = c(1,1/3,1/5),prob=c(0.432,0.226,0.342))
tap.water.drank <- mcstoc(rinvgauss,type = "V", mean=6, shape=60)
# 개인들의 비소 노출량을 계산
arsenic.exposure <- arsenic.conc * drinking.habit * tap.water.drank
arsenic.dose <- mcstoc(rpois, type = "VU", lambda=arsenic.exposure)
prob.per.hit <- mcstoc(runif,type="U", min=0.00001, max=0.0017)
risk <- 1-(1-prob.per.hit)^arsenic.dose
As1 <- mc(arsenic.conc, drinking.habit, tap.water.drank, arsenic.exposure, arsenic.dose, prob.per.hit,risk)
print(As1, digits=2)
summary(As1)
# 불확실성을 고려했을 때 0.006373 0.6% 되고, 평균에 대한 신뢰구간은 [0.04,1.3]

# mcprobtree() 함수 변이 및 불확실성에 대해 혼합 분포를 사용한 mcnode객체를 구축하 수 있게 해준다.
# EX) 마을 수돗물의 평균 비소 집적량이 평균 12이고 표준편차가 0.5인 정규분포를 따를 가능성이
# 85%이고, 10에서 12.7 사이의 구간에서 정의된 균일분포를 따를 가능성은 15%라고 할때
# 각각의 기능서엥 대해 따로 시뮬레이션 한 수 나중에 mcprobtree()함수로 결합하게 된다.
arsenic.conc1 <- mcstoc(rnorm,type="U",mean=12,sd=0.5)
arsenic.conc2 <- mcstoc(runif,type="U",min=10,max=12.7)
# 평균 비소 집적도가 arsenic.conc1하에 결정될 가능성이 85%이고, arsenic.conc2는 15%
# rbern()를 이용해 0.85 확률의 베르누이 확률변수를 생성해 1이 나오면 arsenic.conc에서 
# 0이 나오면 arsenic.con2에서 나온 것으로 생각 할 수 있다.

arsenic.distr <- mcstoc(rbern, type="U", prob=0.85)
arsenic.conc <- mcprobtree(arsenic.distr, list("0" = arsenic.conc1, "1" = arsenic.conc2),type="U")
summary(arsenic.conc)

# corcode()함수는 두개 이상의 변수 사이의 상과계수를 지정할 대 사용한다.
# 아이먼-코너버 방법을 이용해 mcnode 객체 사이의 순위상관구조를 구축해준다.
# EX) drinking.habit과 tap.water.drank사이의 상관계수가 0.5라면 cornode()함수에서 target = 0.5로
# 인수를 지정하면 이러한 상관관계 정보를 위험 분석 과정에 반영될 수 있다

cornode(drinking.habit , tap.water.drank, target=0.5,result=TRUE, seed=223)

# mcmodel()함수는 이차원 모넽카를로 모의 실험을 위한 As1모형을 단번에 작업할 수 있다.
modelAs1<- mcmodel({
  arsenic.conc <- mcstoc(rnorm, type="U", mean=2, sd=0.5)
  drinking.habit <- mcstoc(func = rempiricalD, type = "V", values = c(1,1/3,1/5),
                           prob = c(0.432,0.226,0.342))
  tap.water.drank <- mcstoc(rinvgauss, type ="V", mean=6, shape=60)
  arsenic.exposure <- arsenic.conc * drinking.habit * tap.water.drank
  arsenic.dose <- mcstoc(rpois, type = "VU", lambda = arsenic.exposure)
  prob.per.hit <- mcstoc(runif , type= "U", min = 0.00001, max = 0.0017)
  risk <- 1 - (1 - prob.per.hit)^arsenic.dose
  mc(arsenic.conc, drinking.habit, tap.water.drank, arsenic.exposure, arsenic.dose, prob.per.hit, risk)
})

# mcmodel() 함수를 사용해 mcmodel 객체를 생성하고 나면 evalmcmod() 함수를 사용해 모형을 평가해 볼 수 있다.
# 변이 차원을 위한 모의 실험 횟수는 nsv 인수로, 불확실성 차원을 위한 모의 실험 횟수는 nsu인수로 지정된다.
# 또한 seed 인수에 초기값을 지정할 수 있다.

As1 <- evalmcmod(modelAs1,nsv=1001,nsu=101,seed=223)
print(As1)

# 변이 및 불확실성 차원에 대한 모의실험 횟수를 변경하고 싶은 경우에도 유용하다
# 하지만 두 차원에 대한 모의실험 회수를 늘리면 수행시간이 길어짐

As2 <- evalmcmod(modelAs1,nsv=100,nsu=10,seed=223)
print(As2)

#이를 히스트그램으로 출력
hist(As1)
plot(As1)
# 개별 mcnode를 출력도 가능하다
hist(prob.per.hit)
plot(risk)

# 다변량 노드
# 비소 수준이 높아짐에 따른 암 발병 위험을 추정하는 것은 모든 변수들이 단변량
# 다변량 자료에 대한 몬테카를로 모의실험도 가능한데, mcstoc() 함수에서 nvariates 인수에 변수의 갯수를 지정하면 된다.
# 디리클레 분포, 다항분포, 다변량정규분포 등의 다변량 분포에 유용

parameter1 <- mcstoc(rdirichlet, type="VU", nvariates=4, alpha=c(1,4,5,7))
parameter1
mcstoc(rmultinomial, type="VU", nvariates = 4, size=100, prob=parameter1)

arsenic.conc <- mcdata(c(arsenic.conc1, arsenic.conc2),type="U", nvariates = 2)

modelAs1.Bivariate<- mcmodel({
  arsenic.conc <- mcdata(c(arsenic.conc1,arsenic.conc2),type="U",nvariates=2)
  drinking.habit <- mcstoc(func = rempiricalD, type = "V", values = c(1,1/3,1/5),
                           prob = c(0.432,0.226,0.342))
  tap.water.drank <- mcstoc(rinvgauss, type ="V", mean=6, shape=60)
  arsenic.exposure <- arsenic.conc * drinking.habit * tap.water.drank
  arsenic.dose <- mcstoc(rpois, type = "VU", lambda = arsenic.exposure)
  prob.per.hit <- mcstoc(runif , type= "U", min = 0.00001, max = 0.0017)
  risk <- 1 - (1 - prob.per.hit)^arsenic.dose
  mc(arsenic.conc, arsenic.dose, risk)
})
As1.Bivariate <- evalmcmod(modelAs1.Bivariate, nsv=1001, nsu=101,seed=223)
print(As1.Bivariate)
summary(As1.Bivariate)
