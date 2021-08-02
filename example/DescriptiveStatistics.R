# "affy" packages와 "Biobase" packages 다운받는 방법

if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("affy")


load(url("http://www.ebi.ac.uk/arrayexpress/files/E-GEOD-19577/E-GEOD-19577.eSet.r"))
MLLpartner.ds <- study
library("affy")
library("Biobase")
# rma 정규화 함수
AEsetnorm = rma(MLLpartner.ds)

head(exprs(AEsetnorm))

# 1007_s_at row부분을 가져옴
probeA <- as.numeric(exprs(AEsetnorm)[1,])
# AEsetnorm의 컬럼 이름을 그대로 probeA에 옮겨준다.
probeA <- setNames(probeA,colnames(exprs(AEsetnorm)))
probeA
probeB <- as.numeric(exprs(AEsetnorm)[2,])
probeB
probeB <- setNames(probeB,colnames(exprs(AEsetnorm)))
probeB

# AEsetnorm의 매트릭스화 
MLLpartner.mx <- as.matrix(exprs(AEsetnorm))
MLLpartner.mx
dump("MLLpartner.mx","MLLpartner.R")
class(MLLpartner.mx)
#dim은 shape함수와 비슷함
dim(MLLpartner.mx)

# 기술 통계량
#최소, 1/4, 중간값, 평균값, 3/4, 최댓값
summary(probeA)
summary(MLLpartner.mx)
quantile(probeA,probs = c(0.1,0.2,0.6,0.9))
round(mean(probeA),2)

df<- data.frame(expr_probeA = probeA, expr_probeB = probeB,drugA_response = factor(rep(c("success","fail"),21)))
# 반응값별로 각 열에 대한 요약정보를 얻기 위한 by
by(df,df$drugA_response,summary)

# 자료의 변이 정리
max(probeA) - min(probeA)
range(probeA)
# 평균값
mean(probeA)
# 제곱합
probeA.soq <- sum((probeA-mean(probeA))^2)
probeA.soq
# 제곱합을 자유도 n-1로 나누면 모분산에 대한 불편 추정치인 표본분산을 계산할 수 있다.
# n은 벡터 probeA의 길이로서 length() 함수를 이용해 얻는다.
d.f <- length(probeA) -1
probeA.soq/(d.f)
# 표본분산을 더 쉽게 구하려면 var()함수를 이용하면 된다.
var(probeA)

# 표준편차는 분산의 제곱근으로 정의하거나 sd() 함수를 이용하면 된다
sqrt(var(probeA))
sd(probeA)
# 신뢰도를 측정하기 위해 표본평균에 대한 표준오차, 분산을 표본크기로 나눈 값의 제곱근을 계산
sqrt(var(probeA)/length(probeA))
# 신뢰구간 평가
# 표준오차를 구하고
std.err.s2A <- sqrt(var(probeA)/length(probeA))
# t-분포의 분위수를 계산해주는 qt()함수를 이용.
qt(.975,d.f) * std.err.s2A
