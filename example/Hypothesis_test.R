# 가설검정 윌콕슨 부호-순위검정 예시

female_height <- c(117,162,143,120,183,175,147,145,165,167,179,116)

# wilcox.test()를 사용하고 mu 인수값을 171로 지정한다.
wilcox.test(female_height,mu=171)
# p-value가 0.05보다 작기 때문에 대립가설 즉, 자료의 평균값이 171이 아니라는 가설을 채택

# 출력 결과 마지막에 데이터에 동점이 있으면 p-value값이 정확하지 않다는 경고메시지가 나온다.
# p-value값과 함께 인수로 지정한 경우에 정규근사에 기초한 신뢰구간 계산

wilcox.test(female.height,mu=171,conf.int=TRUE,conf.level=0.99)

#연속성 수정 윌콕슨 부호-순위 검정을 사용하는 대신 부트스트랩
# 접근법을 사용해 자료의 평균값이 이론적인 평균값 171과 다른지 검정

f<-numeric(10000)
for (i in 1:10000){f[i]<-mean(sample(female_height,replace=TRUE))}
hist(f,xlab="bootstrap means")

# t-test() t-검정을 이용해 검증
t.test(female_height,mu=171)

# var-test() F-검정을 이용해 두 분포의 평균 검증
var.test(probeA,probeB)

# 정규성 가정 없이 두 분포가 같은 지 알아보기 위한 또 다른 검정방법은 크루스칼-윌리스 검정이다.
head(df)

kruskal.test(expr_probeA ~ drugA_response,data=df)

# 비율을 통한 검증
# 데이터 생성.
waiting.period <- c(3,5,4,5.5,3.5,2.5,3,5,4.5,3,3.5)
above4.hrs <- ifelse(waiting.period >4,"yes","no")
above4.hs.table <- table(above4.hrs)

#z-검정을 시행
prop.test(4,n=11,p=0.5,alternative = "two.sided", correct=FALSE)

prop.test(4,n=11,p=0.5,alternative = "two.sided", correct=TRUE)
#검정 결과 귀무가설을 채택하게 되며, 4시간보다 더 오래 기다릴 확률이 50%가 아니라는 주장을
#위한 충분한 증거는 없다고 결론을 내린다.

#이항검정을 시행하려면 binom.test()
binom.test(4,n=11,p=0.5)
#귀무가설을 기각하기에 충분한 증거가 없다고 하며, 95퍼센트 구간은 11%에서 69% 사이로 본다.

#문제 prop.test()함수를 이용하여 이항검정을 통해 두 비율을 비교합니다
# A회사가 승진 시 남성보다 여성을 선호하는지 여부를 알고싶다고 하자
# 430명의 여성직원중 16명 승진, 1053명 중 63명이 승진했다고 할때 prop.test()를 통해 검정해보도록 하자
promoted.employees <- c(16,63)
total.employees <- c(430, 1053)
prop.test(promoted.employees,total.employees)

# 분할표 분석에서 독립성을 겅정하기 위해 카이제곱 검정과 피셔의 정확검정을 수행하려면
# chisq.test(), fisher.test() 함수를 이용
# 문제 2 백인남성 96명에 대해 눈색깔이 푸른색 갈색인 경우와 머리카락이 금발, 갈색인경우를 계수행렬을 만들었다고 할 때
# 눈 색깔과 머리카락 색깔이 서로 독립인지 검정하기 위해 chisq.test()함수를 이용
trait.counts <- matrix(c(24,14,11,47),nrow=2)
colnames(trait.counts) <- c("Blue eyes", "Brown eyes")
rownames(trait.counts) <- c("Blond hair","Dark brown hair")
chisq.test(trait.counts)
chisq.test(trait.counts,correct=FALSE)
# p-값은 귀무가설이 맞다는 전제 하에 표본에서 실제로 관측된 통계치와 '같거나 더 극단적인' 통계치가
# 관측될 확률이다. 특정값보다 작으면 귀무가설을 기각한다. 대립가설을 채택.
# p-값이 0.05보다 작기 때문에 눈 색깔과 머리카락 색깔이 서로 독립이라는 귀무가설을 기각하게 된다.
data.counts <- matrix(c(7,5,2,6),nrow=2)
data.counts
fisher.test(data.counts)

# 단위근 검정은 자기상관모형을 이용해 시계열이 비정상인지를 검정하는 것이다.
# 대표적으로 증강 디키-풀러 검정이 있다.
# 이 검정의 귀무가설은 시계열이 정상성을 만족하지 않는 것이다.

library("quantmod")
# 페이스북 주식 정보를 가져온다
fbstock <- getSymbols("FB",src="yahoo",from = '2012-06-18',end='2014-11-28',auto.assign=FALSE)
head(fbstock)
# 차트를 그리는 두가지 방법
chartSeries(fbstock[,4:5],theme="white",up.col="black")
plot.ts(as.ts(fbstock[,4:5]),main = "FACEBOOK Stock Information from 2012-06-18 to 2014-11-28")

require(tseries)
# ADF 검정을 시행
adf.test(fbstock[,4])
# p-value에 의해 귀무가설을 기각할 수 없고, 정상시계열이 아니고, 단위근이 존재한다고 결론을 내리게 된다.
# ADF 검정 결과는 퀴아트코우스키-필립스-슈미트-신 검정을 통해 다시 확인 가능
kpss.test(fbstock[,4])
# p-value가 0.05보다 작아서 귀무가설을 기각할 수 있으며, 페이스북 종가 시계열은 비정상 시계열이라는 결론을
#내리게 된다.















