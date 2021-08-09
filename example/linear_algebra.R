# R 행렬 as.matrix()

a <- c(1,2,3,4)
b <- matrix(a,nrow=1)

# 직사각형, 정사각형, 대각행렬, 삼각행렬, 대칭행렬, 단위행렬, 백터, 희박행렬 등의 종류가 있다.

phys.func <- read.csv('phys_func.txt')[,c(-1)]
phys.func.mat <- as.matrix(phys.func)

#기본적인 행렬 연산
# 상관행렬을 만드는 방법은 cor()함수

cor.mat <- matrix(cor(phys.func),ncol=20)
phys.brief.mat <- as.matrix(phys.func[c(1:30),])

# 전체 그냥 1빼는것
phys.brief.mat-1
# 서로 다른 행렬의 덧샘
phys.brief.mat + matrix(rnorm(600),ncol=20)
# 전체 행렬의 표준편차를 계산한 수 그 값으로 나누는 일
phys.brief.mat / sd(phys.brief.mat)

# 만약, 행렬의 각 열의 평균과 표준편차를 계산해 각 원소값마다 표준화 작업을 하려면
# R의 sweep()를 사용하면 행렬로부터 특정값 또는 요약통계량 값을 소거하는 데 사용된다.
# 행방향으로 소거를 적용할지 열 방향으로 적용할지 통계값을 사용할지 특정값을 사용하는지

# 평균값을 구하는 코드
mean.phys <- apply(phys.brief.mat,2,mean)
# 평균값을 원소에 뺄샘
phys.sweep.1 <- sweep(phys.brief.mat,2,mean.phys,'-')
sd.phys <- apply(phys.sweep.1,2,sd)
phys.sweep.2 <- sweep(phys.sweep.1,2,sd.phys,'/')

# 더 간단한 표준화 방법
phys.scaled <- scale(phys.brief.mat,center=TRUE,scale=TRUE)
# 열 평균 역시 내장함수를 이용해 얻을 수 있다.
colMeans(phys.brief.mat)

phys.sweep.2/phys.scaled

# 행렬 단위 연산
# 전치행렬
# 행과 열을 바꾸는 것을 의미 3:2 -> 2:3
t(phys.brief.mat)

# 행렬의 곱셈
A <- matrix(c(rep(2,3),rep(5,3)),ncol=2,byrow=FALSE)
B <- matrix(c(1:16),nrow=2,byrow=TRUE)
C <- matrix(1,ncol=2,nrow=3,byrow=FALSE)
A %*% B
B %*% C
A %*% t(C)

# 선행의 열의 갯수와 B의 행의 갯수가 같아야 곱셈이 가능하다
# 그래서 non-confortable arguments가 있다는 오류가 나온다.

# 정방행렬을 구하기

small.network <- matrix(c(0,1,1,0,1,0,
                          1,0,1,0,0,0,
                          1,0,0,1,0,1,
                          0,1,0,0,0,1,
                          0,1,0,1,0,0,
                          0,1,0,0,1,0),
                        nrow = 6, byrow = TRUE)
# 각 행의 합을 구하면 네트워크의 영향력을 알 수 있다.
apply(small.network,1,sum)
#1,3은 3이고 나머지는 2
apply((small.network %*% small.network),1,sum)
# 이는 2차 팔로워를 갖고있는지 확인 하는 것이다 둘을 더해주면 1차 및 2차 팔로워 총수를 계산가능
apply((small.network %*% small.network + small.network),1,sum)

# 이를 데이터 셋을 넓혀본다
set.seed(51)
social.network.mat <- matrix(sample(c(0,1),1000000,replace = TRUE, prob = c(0.7,0.3)),ncol=1000)
#대각행렬화
diag(social.network.mat) <- 0
influence.1 <- apply(social.network.mat,1,sum)
second.degree.mat <- social.network.mat %*% social.network.mat
influence.2 <- apply(second.degree.mat,1,sum)
influence.1.2 <- apply(social.network.mat + second.degree.mat,1,sum)
influence.1.2
(social.network.mat + second.degree.mat) %*% rep(1,1000)

# 외적은 두 벡터에 적용되어 행렬을 구성하는 연산으로 각 벡터의 길이가 행렬의 크기를 결정한다.
x<-c(1:3)
y<-c(4:6)
outer(x,y)

# 행렬 곱셈에서 희박 행렬을 사용
M <- matrix(rep(1,9),nrow=3)
N <- diag(c(1:3),nrow=3)
P <- matrix(rep(c(1:3),3),nrow=3)
Q <- matrix(1,nrow=3)

# 행렬의 곱셈을 이용하면 신체기능 인덱스에 대한 사람의 총점을 계산할 수 있다.

phys.func.mat <- as.matrix(phys.func)
total.scores <- phys.func.mat %*% matrix(rep(1,20),nrow=20)

# 이를 파트로 나눠서 세분화해본다
# 인지 혹은 사회적 기능 1,17,18,19
# 하반신 관련 활동 2,3,4,8,9,10,13,14
# 상반신 관련 활동 5,6,7,11,12,15,16,20
# 원래 데이터 행렬을 계획행렬과 곱한다. 계획행렬은 희소성질을 가지며
# 세개의 도메인에 해당되는 세개의 열과 20개의 행으로 구성되어 있다.

design.matrix <- matrix(rep(0,60),nrow=20)
design.matrix[c(1,17,18,19),1]<-1
design.matrix[c(2,3,4,8,9,10,13,14),2] <-1
design.matrix[c(5,6,7,11,12,15,16,20),3] <- 1
total.scores <- phys.func.mat %*% design.matrix
summary(total.scores)

# 역행렬을 구하는 함수 solve()
solve(cor.mat)
cor.mat %*% solve(cor.mat)

C.2 <- matrix(sample(c(1:100),1000000,replace=TRUE),nrow=1000)
Y.2 <- matrix(sample(c(1:1000),1000,replace=TRUE),nrow=1000)
solve(C.2,Y.2)

# 행렬식 정방행렬에 대해서 정의 벡터들이 선형독립인지, 어느 벡터 하나라도 나머지 벡터들의
# 션형결합으로 표현이 불가능한지 여부를 확인하는데 사용할 수 있다.
# 정보의 과잉 혹은 중복성이 있다, 행렬의 역행렬이 존재하지 않는다.
det(cor.mat)
# 행렬값이 0이 아니라는 것은 벡터들이 선형독립이고 역행렬이 존재한다는 것을 의미

#삼각행렬 upper.tri() , lower.tri() 함수는 삼각행렬을 만들때 사용할 수 있다.
triangle.matrix <- cor.mat
triangle.matrix[upper.tri(triangle.matrix)] <- NA
triangle.matrix
# QR분해는 주어진 행렬 M을 서로 다른 두 행렬 Q와 R의 곱 M=QR로 분해하는 방법들 중 하나이다. Q는 직교행렬이고 R은 상삼각행렬이다.
data(trees)
head(trees)
trees.qr <- qr(trees[,c(2:3)])
# qr.Q()함수는 직교행렬을 만들 수 있고, qr.R()행렬은 상삼각행렬을 만들 수 있다.
Q <- qr.Q(trees.qr)
R <- qr.R(trees.qr)
head(Q%*%R)

# QR분해의 가장 큰 용도는 수치해석적으로 안정적인 선형연립방정식의 해를 구하는 것이고, 따라서 선형회귀의 최소제곱추정에 유용하다.
# 선형회귀모형에서 예측변수로 구성된 행렬 X와 예측대상 변수의 관측값들로 구성된 벡터 Y가 있을 때 선형회귀계수 추정치를 B라는 벡터로 저장하는 예
trees.lm <- lm(trees$Volume ~ trees$Girth + trees$Height)
names(trees.lm)
Q.2 <- qr.Q(trees.lm$qr)
R.2 <- qr.R(trees.lm$qr)
# Girth, Height 변수의 값으로만 구성한 행렬에 대해 QR분해가 적용되었다는 것이다.
# 회귀계수 추정치 계산
solve(R.2) %*% t(Q.2) %*% trees$Volume

# 고유값 분해는 정방행렬에만 적용이 가능하다. 이 분해는 고유값들과 각 고유값에 대응되는 고유벡터로 구성된 행렬을 표현
# 각 열이 고유벡터들로 구성된 V, 대각원소가 고유값들로 구성된 대각행렬 L^2, 고유벡터로 구성된 행렬의 전치행렬 V^T
# 고유벡터의 성질 1. 고유백터는 서로 직교. 2. 고유벡터중 하나를 원래 행렬 뒤에 곱하면, 그 고유벡터에 대응되는 고유값을 곱한것과 같아진다.
# 고유벡터 중 하나를 원래 행렬의 뒤에 곱하면 그 고유벡터의 길이를 고유값으로 늘린 벡터가 된다는 것 고유벡터의 방향정보가 변하지 않아서, 고유값을 특성해라고 부름
# 고유값 및 고유벡터를 구하려면 eigen() 함수를 이용하면 된다.
eigen(cor.mat)$values
eigen(cor.mat)$vector

# 주어진 행렬 뒤에 고유벡터를 곱해서 길이가 조절된 고유벡터를 얻는 예제.
cor.mat %*% eigen(cor.mat)$vector[,1]
eigen(cor.mat)$values[1] * eigen(cor.mat)$vectors[,1]
# 수행 시 결과가 같음.

