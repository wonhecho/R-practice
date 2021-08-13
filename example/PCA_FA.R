# PCA와 FA

# 공분산 및 상관계수의 수리적 성질에 대해서 확인
# 공분산(covariance)는 두 변수 간의 선형 종속성에 대한 측도이고 상관계수는 공분산을 두 변수의 표준편차의 곱으로 나눈 값
# 상관계수는 표준화된 공분산. 이런 것을 행렬로 나타낸 것은 공분산행렬, 상관행렬이라 한다.
# (0,0) 부터 (3,3) 까지의 번호 1,cor(A,B),cor(A,C),cor(B,A),1,cor(B,C),cor(C,A),cor(C,B),1

# PCA는 데이터의 차원을 축소하는 도구이다.
# 데이터에서 중복성이 있다면 차원 축소가 필요하다.
# PCA는 다차원 데이터에 대해서 낮은 차원으로의 효과적인 모델링이 가능한지를 알아본다.
# 원 자료의 차원과 같은 개수의 차원을 생성하지만, 대부분이 어디에 적재되었는지, 나머지 차원이 갖고 있는 정보의 양이 미미한지 여부

set.seed(20)
x <- sample(c(0:100),replace =TRUE,1000)
y <- x + sample(c(-10:10),replace = TRUE,1000)
plot(y ~ x)

# 좌표축을 적절히 회전시켜 얻게 되는 새로운 좌표축에 대한 점들의 위치를 생각하면
# 크게 잃지 않고 일차원으로 데이터 차원을 축소 할 수 있다
# 수학적으로 PCA는 공분산행렬 또는 상관행렬의 고유앖 분해에 의해 작동된다.
# SVD를 사요해서 사용할 수 있다.
# princomp는 공분산행렬 또는 상관행렬에 대한 고유값분해를 이용
# prcomp()는 원 자료에 대한 SVD
pca.sample <- prcomp(matrix(c(x,y),ncol=2))
summary(pca.sample)
pca.sample$rotation
rotation.matrix <- -pca.sample$rotation
rotated.data <- matrix(c(x,y),ncol=2) %*% rotation.matrix
plot(rotated.data[,2] ~ rotated.data[,1])

# svd()를 사용한 예
svd.sample <- svd(matrix(c(x,y),ncol=2))
manual.rotation <- svd.sample$u %*% -diag(svd.sample$d)
plot(manual.rotation[,1],manual.rotation[,2],xlim=c(0,100),ylim=c(-75,75))

# 표준화 PCA, 비표준화 PCA
# 고유값 분해와 특이값 분해 모두 주 성분의 방향을 제공한다.
# 그러나 구성하는 변수들의 단위가 서로 다른 데이터를 만날때가 있다.
# 이런 경우는 공분산행렬, 고유앖분해를 실시하거나 원 데이터에 대해 
# SVD를 적용하면 큰 값을 가진 변수의 방향에 따라 데이터가 정렬되어 있는 것으로 착각
# 이렇게 되면 분산의 값도 커지게 된다. PCA는 가장 큰 분산 방향이 첫번째 주성분이
# 되도록 회전시키게 되므로 문제인 것
# 변수들이 다른 단위로 측정된 경우 자료값들을 표준화 해야함

red.wine <- read.csv('winequality-red.txt')
# 마지막 변수를 제외한 나머지 변수들의 공분산행렬을 계산해 고유치 분해를 실시하고
# 상관행렬에 대한 고유치 분해 실시
wine.eigen.cov <- eigen(cov(red.wine[,-12]))
wine.eigen.cor <- eigen(cor(red.wine[,-12]))

# 두가지 고유치 분해 결과로부터 주성분이 분산 성분을 얼마나 설명하는지 확인
wine.eigen.cov$values / sum(wine.eigen.cov$values)
wine.eigen.cor$values/sum(wine.eigen.cor$values)
# 두 결과 사이에 차이가 크다. 공분산행렬에 의한 결과는 첫번째 주성분이 분산의
# 95% 가량을 설명하지만, 상관행렬에 의한 결과는 첫번째 주 성분이 분산의
# 30%도 설명하지 못함을 말해준다. 이유는 어떤 변수들이 다른 변수에 비해
# 훨씬 큰 값들로 저장되게 하는 단위로 표현되어 있다.

wine.prcomp <- prcomp (red.wine[,-12])
wine.prcomp.scaled <- prcomp(red.wine[,-12],scale=TRUE)
summary(wine.prcomp)
summary(wine.prcomp.scaled)
# 특정변수 즉 아황산가스 관련 변수가 측정단위 때문에 매우 큰 분산을 가지므로,
# PCA 결과는 오해의 소지가 있는 결과를 준다.
# 이경우 PC1은 본질적으로 아황산가스 변수나 다름이 없다.
# 이것은 대부분의 변수가 미터로 측정되었는데 하나만 센티미터로 측정된 경우
# 나타나는 경우다 sclaed=TRUE 인수를 지정하면 모든 변수에 대해
# 분산이 1이 되도록 표준화한후 PCA를 실히한 결과를 얻을 수 있다.

# 구체적인 활용
# SVD 가반으로 PCA를 수행하는 R의 FactoMineR 패키지를 이용한다.

abalone <- read.csv('abalone.txt')
library(FactoMineR)
abalone<-pca <- PCA(abalone[,c(-1)])

# 레드와인 데이터의 상관행렬
cor(red.wine)
wine.pca <- PCA(red.wine,quanti.sup = 12) 
# quanti.sup을 지정하면 몇 번째 변수 quality를 PCA()함수가 사용하지 말 것을 의미
summary(wine.pca)

# 고유값은 상관행렬에 대해 고유값 분해를 실행하는 과정에서 산출
# 고유값들의 합은 전체 변수의 개수인 11개가 된다.
# 특정 주성분에 의해 설명되는 분산의 비율은 해당 고유치를 전체변수개수로 나눈 값으로 주어진다.
# 고유값이 1보다 작으면 해당 주성분이 개별 변수 한개보다도 설명력이 낮음
# Dim은 주성분에 의해 정의된 새 좌표평면에서의 해당 변수의 좌표에 해당한다.
# ctr 열은 해당 차원을 생성하는데 기여한 정도를 나타낸다.
# cos2열은 코사인 제곱값을 나타낸다. 이 값이 1에 가까울수록 변수가 주성분이
# 만드는 좌표축과 비슷한 방향이고, 주성분에 많이 연관된 변수임을 의미.

# 주성분의 개수를 정하는 방법 카이저-구트만규칙과 스크리 검정법
# 카이저-구트만 규칙은 고유값이 1보다 큰 주성분을 선택하는 것
# 고유값이 1보다 작은 주성분은 개별 변수가 평균적으로 가진 총분산 설명력보다도
# 못한 설명력을 가졌음을 의미한다는 점에서 논리적인 근거를 둔다.
# 레드와인 데이터 예제에서는 네 개의 주성분만 남기고 나머지 무시
# 스크리 검정법은 고유치와 주성분 개수간의 그래프 그리는 방법

plot(wine.pca$eig, type = 'b', xlab = 'Principal Component', ylab = 'Eigenvalue', main = 'Eigenvalues of Principal Components')

plot(abalone.pca$eig,type='b',xlab='Principal Component',ylab='Eigenvalue',
     main='EigenValues of Principal Components')

# PCA를 활용하는 또 다른 예 '탐색적 요인분석과 반영적 구성개념'에서 다루게 될
# 요인분석과 비교
# 형성적 구성개념과 반영적 구성개념에 대해 논의
# 형성적 구성개념은 여러 개의 개별 특질들로 부터 합성변수로써 형성되는 일반적인
# 특질을 가리키는 말. 개별 특질을 나타내는 지표변수에서 구성개념 쪽으로 향해있는데
# 구성개념이 개별 특질로부터 형성되었음
# 반영적 구성개념은 일반적인 특질이 근저에 깔려있으면서 개별 특질의 원인이 됨
# 화살표가 구성개념으로부터 출발해 개별 지표를 향해 있는데, 이는 구성개념이
# 개별 특질을 유도하며 개별 특질은 이 구성개념의  발현이라는 사실

phys.func <- read.csv('phys_func.txt')[,c(-1)]
phys.func.pca <- PCA(phys.func)
summary(phys.func.pca)

# 참고 phys.func.pca$eig$eigvalues를 참고하기에 phys.func.pca$eig까지는 recursive가
# TRUE이므로, 참조되지만 이 이상부터는 recursive가 아닌 atomic형식이기 때문에
# 행렬을 통해 참조해야 한다.
a <- phys.func.pca$eig[1:20]
a
plot(a,type='b',xlab='Principal Componet', ylab='Eigenvalue', main= 'Eigenvalues of Principal Components')

# 신체기능을 1차원 척도로 처리할지 다차원으로 처리할지에 대한 것이다.
# 카이저-구트만 기준에 따르면 네 개의 주성분을 선택하게 된다.
# 그래프를 보면 고유치 감소세의 정체현상이 두군데 보이는데
# 세번째 성분일 때가 있고 다른 하나는 여섯번째 성분 다음이다.
# 따라서 스크리 도표 기준에 따르면 세개의 주성분을 선택하게 된다.

# PCA는 변수에 내재된 총분산을 모델링하는 것이다.
# 다른 접근법인 요인분석은 변수에 내재된 공통분산을 모델링하는 방법
# 탐색적 요인 분석에 대해 논의한다.

# 접근 법인 요인분석은 변수에 내재된 공통 분산을 모델링하는 방법
# EFA는 관측 변수들이 잠재변수라고 부르는 직접 관측이 불가능한 변수드에 의해
# 설명할 수 있다는 것을 가정 잠재변수들은 공통분산의 원천으로서 모형화됨
# 무게 중심 방법 논의

le.matrix <- as.matrix(phys.func[,c(2,3,4,8,9,10,13,14)])
le.cor <- cor(le.matrix)
le.cor.reduced <- le.cor
# 각 행의 합을 구하고 다시 행렬의 총합을 구해보자
# 요인적재계수 추정치는 각 행의 합을 총합의 제곱근으로 나누어 얻는다
row.sums <- le.cor.reduced %*% matrix(rep(1,8),nrow=8)
total.sum <- sum(row.sums)
sqrt.total <- sqrt(total.sum)
row.sums/sqrt.total
# 축소상관행렬을 만드는데 사용하고 이 새로운 행렬에 다시 무게중십 방법을 시행 가능
# 이상의 절차는 요인적재계수 추정치가 수렴조건으로 만족할때 까지 반복

# 다중 요인 모형
# 두 잠재변수 모두 각 관측변수와 상관관계를 가지는 것과 잠재변수들 사이에도
# 상관관계가 있는 것을 가정하고 있다.
# 요인적재계수는 첨자를 붙여 해당 잠재변수와의 관계를 표시하기도 한다.

# 주축요인법
# 반복적 방법으로 잔차행렬로부터 요인을 추출하기보다는 주축요인법을 사용해 원하는
# 개수만큼의 요인을 직접 추출하는 방법
# 축소상관행렬에 대해 고유치분해를 실행해 고유값을 크기순으로 대각에 나열한 대각행렬
# L과 고유벡터행렬 V를 구하는 작업

phys.cor<- cor(phys.func)
# 다중상관계수의 제곱값을 이용해 축소상관행렬을 만든다.
reduce.cor.mat <- function(cor.mat){
  inverted.cor.mat <- solve(cor.mat)
  reduced.cor.mat <- cor.mat
  diag(reduced.cor.mat) <- 1 - (1/diag(inverted.cor.mat))
  return(reduced.cor.mat)
}

phys.cor.reduced <- reduce.cor.mat(phys.cor)

paf.method <- function(reduced.matrix,nfactor){
  row.count <- nrow(reduced.matrix)
  eigen.r <- eigen(reduced.matrix,symmetric = TRUE)
  V <- eigen.r$vectors[,c(1:nfactor)]
  L <- diag(sqrt(eigen.r$values[c(1:nfactor)]),nrow = nfactor)
  return ((V %*% L))
}
path.coef <- paf.method(phys.cor.reduced,3)
path.coef
# 추출한 것은 모든 요인에 대해 모든 관측변수가 적재값을 갖는다는 점이다.

# 주축요인법 -> 최대우도방법을 많이 사용
# 자료 분포가 정규분포에 가까운 경우 매우 좋은 성질을 갖고 있다
# 최대우도 방법은 데이터가 정규분포를 따름을 가정하고 정규분포에 의한 우도함수를 최대로
# 최소잔차요인방법은 잔차상관계수를 최소로 하는 방식으로 최대우도 방법과 비슷한 결과를 줌

# 요인 분석 이후 요인 회전을 진행한다.
# 요인을 회전해 데이터 구조를 보다 간명하게 나타내는 것.
# 회전은 요인패턴행렬에 포함된 요인적재값을 가능한한 많이 0에 가까운 값이 되도록
# 하는 것을 목표로 이루어짐.
# 관측 변수드이 여전히 모든 요인에 대해 적재값을 갖게 되겠지만,
# 각 관측변수는 특정 요인에 대해 나머지 요인보다 훨씬 높은 적재값을 갖게 됨.

# 회전의 종류로는 직교회전과 사각회전으로 대별 되는데, 직교회전이 가장 많이 쓰이고
# 해적석인 면에서 우수한 방법으로 여겨진다. 사각회전은 현실적인 추정치를 제공하는 것
# Quartimax 회전은 요인패턴행렬의 네제곱 값의 총합을 최대로 하는 방법
# Varimax회전은 행 제곱합을 제곱한 값의 평균을 네제곱합에서 뺀 값
# Oblimin회전
# Promax회전

library(GPArotation)
rotated.structure <- oblimin(path.coef)
rotated.structure

# 여기서 생성되는 객체는 요인적재행렬, 요인 간 상관행렬, 회전행렬을 포함
# 요인적재행렬은 20개의 관측항목이 세개의 요인에 어떻게 연관되어 있는지를 알려준다
# 요인에 따라 어떤 적재값의 크기는 매우 작은 것을 보고
# 큰 항목들을 통해 요인의 의미를 해석하면 된다.
# 기준은 0.4 이상 관측변수가 특정요인에 대해 다른 요인에 비해 확실히 
# 큰 적재부하가 걸려있기도 함

loading.matrix <- rotated.structure$loadings
loading.matrix [ abs(loading.matrix)<0.3] <- NA
loading.matrix
# 첫번째 요인은 전반적 운동기능 두번째 요인은 세밀한 운동기능
# 세번째 요인은 가사관리능력을 나타낸다. 

# psych 패키지를 이용한 고급 EFA
# 좋은 추정치를 얻기 위해서는 주어진 기준을 만족하는 최적 해를 구하기 위한
# 반복적인 계산 알고리즘이 필요하다

library(psych)
fit.efa.prep <- polychoric(phys.func)
fit.efa.3 <- fa(fit.efa.prep$rho, nfac=3, roate = 'promax')
fit.efa.3

# 다분상관계수들을 이용해 상관 행렬을 만들어 요인 분석
# h2, u2는 공통성과 특수성 값이고, 두 합은 1
# h2가 클수록 요인들이 더 많은 항목 분산을 설명함을 의미
# 그 아래는 상관계수행렬이 나온다.
# h2의 값이 작다는 것은 요인들이 데이터를 제대로 설명하지 못했다는 의미
# 공통성 값은 요인회전 이전에 계산한 적재계수 값의 제곱합이다.

# 내적 일치성을 구하고 싶은 경우 오메가 함수를 이용하면 된다.

omega(fit.efa.prep$rho, nfac=3 , roatate='promax')

