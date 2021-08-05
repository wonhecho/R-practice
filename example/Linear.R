# 해당의 데이터를 구한다.
plot(c(3,4),c(7,4),ylab="y",xlab="x",main="Slope from coordinate(3,7) and (4,4)",ylim = c(0,10), xlim=c(0))

lines(c(3,3),c(7,4))
lines(c(3,4),c(4,4))
# 그래프 내에 정의를 확인하는 것
text(2,5.5,"delta y")
text(3.5,3.5,"delta x")
# 기울기를 그래프에 표현하는 부분
lines(c(3,4),c(7,4),col="red",lwd=3)
#기울기의 값을 계산 하려면 증가분 나누기 -3
(4-7)/(4-3)
# x절편 값도 알 수 있다. 16
4 -(-3) * 4
# 따라서 이를 이어 그려보면
abline(16,-3)

# 선형회귀분석을 실행하는 예제

gapdh.qPCR <- read.table(header=TRUE,text='GapDH RNA_ng A1 A2 A3
                         std_curve 50 16.5 16.7 16.7
                         std_curve 10 19.3 19.2 19
                         std_curve 2 21.7 21.5 21.2
                         std_curve 0.4 24.5 24.1 23.5
                         std_curve 0.08 26.7 27 26.5
                         std_curve 0.016 36.5 36.4 37.2
                         ')
# 선형회귀 데이터 생성
library("reshape2")
gapdh.qPCR <- melt(gapdh.qPCR, id.value=c("GapDH", "RNA_ng"),value.name = "Ct_value")
#이렇게 되면 variable이라는 칼럼이 하나 더 생겨서 A1,A2,A3등의 인덱스를 갖고 있는다.
#RNA_ng와 Ct_value에 대한 관계 그래프를 생성한다.
par(mfrow = c(1,2))
plot(RNA_ng,Ct_Value)
#이러한 데이터로 산점도를 그리게 되면 곡선모형을 따르는 것처럼 보이는데, 시행착오를 고려하면 피해야한다.
plot(log(RNA_ng),Ct_Value)

#이제 lm()함수를 이용해 선형모형을 적합시킨다
lm(Ct_Value ~ log(RNA_ng))
#결과적으로 a의 최대우도추정치는 23.87이고 b의 추정치는 -2.23이다
#잔차표준오차, 수정 R제곱값 등과 같이 모형정보는 summary() 함수를 이용
#분산분석표를 보려면 summary.aov(model)
par(mfrow=c(2,2))
plot(model)
# 그래프가 4가지가 나오는데 첫번째 그래프는 잔차와 예측값 사이의 산점도이다.
# 적합된 모형이 유효하면 잔차들은 평균이 0이고 등분산성을 만족해야 한다.
# 두번째 그림은 qqnorm그림인데 오차항의 정규성을 확인해준다.
# 오차항의 분포가 대칭성을 만족하려면 점들이 직선을 따라 도열되어 있어야 한다.
# 세번째 그림은 첫번째 그림의 세로축을 표준화잔차 크기의 제곱근으로 대체해 다시 그린 그림
# 특정한 추세가 보이지 않아야 한다.
# 네번째 그림은 잔차 대 레버리지 값 사이의 산점도 위에 쿡 거리를 표시한 것이다.
# 쿡거리는 모수 적합 결과에 큰 영향을 끼친 자료 점들을 알려준다. 
# 결과를 볼때 가장 크게 영향을 미친 번호는 18번이다. 18번의 인덱스를 확인해본다

RNA_ng[18]
Ct_Value[18]
# 만약에 이 점을 제거했을때 바뀌는 현상을 알고싶다면,
model2 <- update(model,subset(Ct_Value != 27.2))
plot(model12)

# 분산분석은 모든 설명변수가 범주형일때, 선형모형을 적합시키는 과정
# 이를 factor이라고 부르며, 레벨은 최소 2가지 이상을 갖는다.
# 설명변수로 세개 이상의 레벨을 가지면 일원배치 분산분석 모형을 사용하고
# 그 요인의 수준 수가 두 개인 경우 스튜던트 t-검정
# 두 개 이상의 요인을 포함시키는 경우에는 이원배치, 삼원배치등의 분산분석을 사용하면 된다.

# 일원배치 분산분석 예제
patient.fatigue <- read.table(header=TRUE, text='
patients fatigue drugA_dose
1 1 low 0.2
2 2 low 0.2
3 3 med 0.2
4 4 med 0.2
5 5 med 0.2
6 6 low 0.4
7 7 low 0.4
8 8 low 0.4
9 9 med 0.4
10 10 med 0.4
11 11 med 0.8
12 12 high 0.8
13 13 med 0.8
14 14 med 0.8
15 15 high 0.8
16 16 high 1.2
17 17 high 1.2
18 18 high 1.2
19 19 high 1.2
20 20 med 1.2 ')

patient.fatigue
attach(patient.fatigue)
patient.fatigue
aov(drugA_dose ~ fatigue)
summary.aov(drugA_dose ~ fatigue)
summary(aov(drugA_dose~fatigue))
model2 <- aov(drugA_dose~fatigue)
par(mfrow = c(2,2))
plot(model2)
# 결과적으로 볼때 4번째 그림에서 20번의 환자가 피로도 수준 영향이 매우 큰 것으로 나타남
# 그렇다면 이 점을 제거해본다
model2 <- update(model2,subplot=(patients != 20))
plot(model2)
summary.lm(model2)
# 이 결과의 계수들의 의미를 해석하려면 lm(y~x)가 y=a+bx로 해석된것을 기억
# 마찬가지로 aov는 y=a + bx1 + cx2로 해석이 된다. intercept는 피로도 수준중 가장 앞에있는 수준을 가리킨다.

# 이원배치 분산분석 이용법
patient.sex <- as.factor(c("F","F","F","M","M","F","M","M","M","F","F","M","M","F","F","F","M","M","F","M"))
model3 <- aov(drugA_dose ~ fatigue*patient.sex)
summary(model3)
plot(model3)
# anova() 함수를 이용해서 유의한 연관성을 확인해보면
anova(model,model3)
# p의 값이 0.97로 두 모형이 서로 크게 다르지 않다. 따라서 아무런 의미가 없다고 볼 수 있다.

#일반화선형모형은 선형회귀를 사용할때 유효성을 담보하려면 등분산성과 오차의 정규성을 가정해야 하는데,
#일반화선형모형 GLM은 오차항의 분포가 정규분포가 아닌 경우를 허용하는 방법이다. 이는 glm()함수를 사용하면 된다.
#3가지가 중요한 점인데 오차구조, 선형예측량, 연결함수 이다
#예를 들면 포아송 분포나, 감마분포를 사용할 때의 예제이다

# 약물투여량과 사망률의 암수별로의 관계를 살피기 위한 그림
cmp1.ld <- read.table(header=TRUE, text='
lethaldose sex numdead numalive
1 0 M 1 19
2 1 M 3 17
3 2 M 9 11
4 3 M 14 6
5 4 M 17 3
6 5 M 20 0
7 0 F 0 20
8 1 F 2 18
9 2 F 2 18
10 3 F 3 17
11 4 F 4 16
12 5 F 6 14
')
attach(cmp1.ld)
plot(cmp1.ld)

proportion_dead <- numdead/20
# 성별에 따른 반수치사량 그래프가 생성된다.
plot(proportion_dead ~ lethaldose, pch = as.character(sex))
# 이후 살아있는 개체수와 죽은 개체수를 바인딩 시킨다
counts <- cbind(numdead,numalive)
cmp1.ld.model <- gml(counts ~ sex * lethaldose,family=bionomial)

# 이러한 GLM의 적합 결과를 살펴보면 sex와 lethaldose간에 p-value가 유의한 작용을 하는 것으로 보인다
# 그렇지만 이를 판단하려면 과대산포문제에 대해서 살펴볼 필요가 있다.
# 잔차편차값을 자유도로 나누어 본다.
# 잔차편차 = Residual deviance 자유도 8
# 이 값이 1보다 작으면 과대산포에 문제가 없음을 뜻한다. 하지만 1보다 큰 경우에는 과대 산포를
#고려하는 오차구조인 준이항을 이용해 다시 모형을 적합시켜야 한다.
summary(glm(counts ~ sex * lethaldose, family=quasibinomial))

# 일반화가법모형은 일반화선형모형의 비모수적 확장으로, 선형 예측자가 예측 변수의 함수에 
# 선형 의존성을 갖는 모형이다. 
# mgcv 패킺의 gam()함수를 사용

pregnancies <- sample(0:25, 300,replace=T)
glucose <- sample(65:200, 300,replace=T)
pressure <- sample(50:120, 300,replace=T)
insulinD <- abs(rnorm(150, 450, 100))
insulinN <- abs(rnorm(150, 65, 75))
insulin <- c(insulinD, insulinN)
weight <- sample(20:70, 300,replace=T)

#mgcv 라이브러리를 가져온다음 더한뒤 summary

library("mgcv")
mouse.data.gam <- gam(pregnancies ~ s(glucose) + s(pressure) +s(insulin) + s(weight))
summary(mouse.data.gam)

# 리턴 값으로 유효자유도, (자유도 값이 모형에 사용된 예측변수의 개수에 상응하지 않음)
# 평활법을 사용하고 있기 때문인데 이것은 edf에 값에 반영됨.
# 또한 일반화교차확인점수(GCV)도 중요한데 이는 평균제곱예측오차의 예측치에 해당됨.
# GCV는 작을수록 좋음

#곡선 구조가 의미가 있는지를 확인하기 위해서는 베이지안 신뢰구간을 검사해야함

par(mfrow=c(2,2))
plot(mouse.data.gam)

#이를 투시도로 시각화 할 수도 있다.

par(mfrow=c(1,1))
vis.gam(mouse.data.gam,theta=-35,color="topo")

# gam.check()함수를 이용해서 살펴볼 수도 있다
gam.check(mouse.data.gam)

#유의하지 않은 설명변수를 제거하면 모형 적합이 어떻게 되는지 확인
mouse.data.gam2 <- gam(pregnancies ~ s(insulin))
summary(mouse.data.gam2)

#AIC 함수를 이요해 적합도와 복잡도를 동시에 고려하는 아카이케 정보기준을 계산해본다

AIC(mouse.data.gam,mouse.data.gam2)

#확인결과 AIC가 낮은 두번째 모델이 조금 더 나은 모형이라고 볼 수 있다.

