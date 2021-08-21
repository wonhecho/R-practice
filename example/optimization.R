# 최적화란 f(x)를 최대화하거나, 최소화 하는 문제이다.

# 일차원 비제약 최적화
# 황금 분할법
# golden.method()

golden.method <- function (f,a,b,tolerance=0.000001){
  psi <- (1+sqrt(5))/2
  x1 <- b - (b-a)/psi
  x2 <- a + (b-a)/psi
  fx1 <- f(x1)
  fx2 <- f(x2)
  while(abs(b-a) > tolerance){
    if(fx2>fx1){
      b<-x2
      x2<-x1
      fx2<-fx1
      x1<-b-(b-a)/psi
      fx1<-f(x1)
    }else{
      a<-x1
      x1<-x2
      fx1<-fx2
      x2<-a+(b-a)/psi
      fx2<-f(x2)
    }
  }
  minimizer <- (a+b)/2
  max.error <- (b-a)/2
  results <- list(minimizer= minimizer, maximum.error = max.error)
  return(results)
}

f <- function(x){
  abs(x - 2.5) +(x-1)^2
}
curve(f, from = 0, to=5)

golden.method(f,a=1,b=3)

# 그래프로 표현하게 될 경우
curve(f,from=0,to=5,cex.lab=1.5)
res <- golden.method(f,a=1,b=3)
points(res$minimizer,f(res$minimizer))

g <- function(x){
  abs(x-2.5) - abs(x-1) - abs(x-0.5)
}
curve(g,from=-10,to=10)

h<-function(x){
  -g(x)
}
golden.method(h,a=-5,b=5)

# 그 외에 optimize()를 사용할 수 있다.

optimize(f,interval = c(1,3),tol=0.000001)

optimize(g,lower=-5,upper=5,tol=0.000001, maximum=TRUE)

# 뉴튼-랩슨 방법
# 뉴튼-랩슨 방법은 미분에 기반해 일차원 최적화 문제를 푸는 수치해석적 방법
# 초기값과 목적함수의 미분을 사용해 함수의 최소값을 찾을 수 있는데
# 방정식의 해를 찾기 위해 선형근사를 사용한다.
# 오목함수 f(x)가 주어져 있을 때 방정식 f(x)=0의 해를 r이라고 한다.
# r의 근사값을 x라고 하면, r=x+h로 나타낼 수 있고, 테일러 전개에 의해 성립이 된다.

f <- function(x){
  exp(-x^2) + x^3
}
curve(f,from=-1,to=4)

newton.method <- function(f,ff,fff,x0,tol=0.000001, N = 100){
  i <- 1
  estimates <- numeric(N)
  fvalue <- numeric(N)
  ffvalue <- numeric(N)
  fffvalue <- numeric(N)
  while(i<=N){
    x1 <- x0 - ff(x0)/fff(x0)
    estimates[i] <- x1
    fvalue[i] <- f(x1)
    ffvalue[i] <- ff(x1)
    fffvalue[i] <- fff(x1)
    i <- i+1
    if(abs(x0-x1)<tol) break
    x0 <- x1
  }
  
  estimates <- estimates[1:(i-1)]
  fvalue <- fvalue[1:(i-1)]
  ffvalue<- ffvalue[1:(i-1)]
  fffvalue <- fffvalue[1:(i-1)]
  df <- as.data.frame(cbind(estimates,fvalue,ffvalue,fffvalue))
  return(df)
}

fp <- function(x){
  3*x^2 - 2*x*exp(-x^2)
}
fpp <- function(x){
  4*x^2*exp(-x^2) - 2*exp(-x^2) + 6*x
}
newton.method(f,fp,fpp,x0=1)

# 결과상 최적해의 값은 0.5126120이다

optimize(f,interval = c(0,2),tol=0.000001)

install.packages("spuRs")

library("spuRs")

minimizer.ftn <- function(x){
  ffvalue <- 3*x^2 - 2*x*exp(-x^2)
  fffvalue<- 4*x^2*exp(-x^2) - 2*exp(-x^2) + 6*x
  results <- c(ffvalue, fffvalue)
  return (results)
}
newtonraphson(minimizer.ftn, x0=1, tol=0.000001)

#넬더-미드 심플렉스 방법은 뉴튼-랩슨 방법의 대안이다
# 미분을 사용하지 않고, 다차원에서 목적함수를 최소화하는 비선형 최적화 기법이다.

optim(par = 1, fn =f)

# 출력 결과를 살펴보면 최적해가 $par에 주어진 것으로 알 수 있다
# 브렌트 방법이나, optimize()함수를 사용하라는 경고 메세지가 주어지는데 좀더 정확한 해를 구하기 위해서 이다.
optim(1,f,method="Brent",lower=0,upper=2)
# 결과적으로는 넬더-미드 방법과 결과가 같다.
# EX) 로센브록함수를 최소로 하는 해를 찾는 예제

rosenbrock.f <- function(x1,y1){
  (1-x1)^2 + 100*(y1-x1^2)^2
}

# 함수의 그래프를 그려보자면
x <- seq(-3,3,by=0.2)
y <- seq(-2,3,by=0.2)
z <- outer(x,y,rosenbrock.f)
persp(x,y,z,phi=40,theta=40,col="turquoise", shade=.000001, ticktype = "detailed", cex.lab=1.5, zlab="")

#로젠브록 함수의 성질 중 하나는 함수값은 언제나 양수인데, x^2,x=1을 동시에 만족하는 경우 0이 된다는 것이다.
# optim()함수를 사용하려면 rosenbrock.f()함수의 인수가 벡터형이 되어야 하며, 첫번째 성분에 x 값을
# 두번째 성분에 y값을 저장하도록 해야 한다.

rosenbrock.f2 <- function(x){
  (1-x[1])^2 + 100*(x[2]-x[1]^2)^2
}
optim(par = c(0.7,0.7),rosenbrock.f2)

optim(par=c(1.5,1.5),rosenbrock.f2)

# 최적화 문제에 변형된 넬더-미드 방법들을 사용해 보고 싶으면 neldermead패키키를 확인
# optim()함수로 사용할 수 있는 method = {"Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN", "Brent"}
# 모의담금질이라는 기법을 이용하고 싶으면 SANN

set.seed(267)
optim(par=c(0.7,0.7),rosenbrock.f2, method="SANN")

# 준뉴튼이라는 방법을 이용한 예제로는
h <- function(x){
  (x[1]-2)^2 + (x[2]-1)^2
}

x1 <- seq(-4,4,by=0.5)
x2 <- seq(-4,4,by=0.5)
mat <- as.matrix(expand.grid(x1,x2))
colnames(mat) <- c("x1","x2")
z <- apply(mat,1,h)
df <- data.frame(mat,z)
library(lattice)
wireframe(z~x1*x2, data=df, shade = TRUE, scales = list(arrows=FALSE), screen = list(z=-35,x=-50))

#persp() 함수를 사용하는 것을 선호
hfn <- function(x,y){
  (x-2)^2 + (y-1)^2
}

x <- seq(-5,5,by=0.2)
y <- seq(-5,5, by=0.2)
z <- outer(x,y,hfn)
persp(x,y,z,phi=35,theta=50,col="purple",shade = .00000001, ticktype = "detailed")

# optim() 함수의 초기치로 (0,0)을 사용하면 좋은 것으로 보인다.
# 이차미분행렬을 직접 계산할 필요는 없고, 근사그래디언트 계산을 통해
# 이차미분행렬의 근사값을 사용한다. optim() 함수 hessian =  TRUE와 같이 인수를 지정하면 
# 수치적으로 미분해 얻은 이차미분행렬을 리턴해주고 gr인수에 지정한 함수 이름으로 그래디언트 함수를 만들어 준다.

optim (par = c(0,0),h,method="BFGS",hessian=T)

#이 부분의 최적해는 (2,1)

# 선형계획법은 목적함수와 제약조건이 모두 선형의 득식 혹은 부등식으로 구성된 최적화 문제
# EX) L(x) = 4x(1) + 7x(2) 이를 최소로 하는 값

install.packages("lpSolveAPI")
library("lpSolveAPI")

#make.lp() 함수를 이용해 두 개의 제약조건과 두 개의 변수를 가진 선형계획 모형 객체인 lp1을 만들어보면
lp1 <- make.lp(2,2)
# set.column()함수를 사용해 x1에 대한 a(11), a(21)의 값으로 구성된 백터와 x(2)에 대한 a(12) a(22)로 구성된 벡터값

set.column(lp1,1,c(1,1))
set.column(lp1,2,c(1,2))
# set.objfn()함수를 이용해 목적함수의 c1, c2의 값으로 구성된 벡터를 지정하고 set.constr.type()함수를 사용해
# 부등식 방향을 지정하는 벡터 생성
set.objfn(lp1,c(4,7))
set.constr.type(lp1,rep(">=",2))
# 마지막으로 set.rhs()를 사용해 우변에 있는 b1과 b2의 값으로 벡터를 지정
set.rhs(lp1,c(4,6))
lp1
plot(lp1)
#이 문제의 답은 (2,2)가 되는 것으로 보인다.
write.lp(lp1,'model1.lp',type='lp')

solve(lp1)

get.variables(lp1)
get.objective(lp1)
install.packages("lpSolve")
library(lpSolve)
lp.ex1 <- lp(objective.in=c(4,7), const.mat=matrix(c(1,1,1,2),nrow=2),const.rhs=c(4,6),const.dir=rep(">=",2))
lp.ex1
lp.ex1$solution

# 정수계획법
# Ex) 노트북의 영업이익은 대당 52달러, 테블릿은 82달러
# 노트북 생산은 100대 태블릿은 170대
# 최소한 65대, 92대의 태블릿을 생산 공급해야 한다.

lp2 <- make.lp(1,2)
set.column(lp2,1,c(1))
set.column(lp2,2,c(1))
# 두가지 변수를 모델에 삽입
set.objfn(lp2,c(50,82))
#minimize를 설정해준다
set.constr.type(lp2,c("<="))
set.rhs(lp2,c(200))
# 둘의 물량을 합쳤을때 최대의 양이 200

# 두 수의 범위를 지정해준다
set.bounds(lp2,lower=c(65,92),columns=c(1,2))
set.bounds(lp2,upper=c(100,170),columns = c(1,2))

# 최적해의 정수를 찾는다.
set.type(lp2,1,"integer")
set.type(lp2,2,"integer")
lp2
# 목적함수를 최대로 하는 최적문제를 풀어야 하므로 lp.control() 함수에 sense 인수를 지정
lp.control(lp2,sense='max')
solve(lp2)
get.variables(lp2)
# 최적해
get.objective(lp2)
# 최댓값

#제약이 없는 변수
# lp()는 변수들이 음이 아닌 값이라는 제약 조건을 만족하는 경우 사용하는 함수
lp.ex3 <- lp(objective.in = c(3,4,-4),
             const.mat=matrix(c(1,3,1,
                                2,-1,-1,
                                -2,1,1),
                              nrow=3),
             const.rhs = c(14,0,2),
             const.dir = c("<=",">=","<="),direction="min")
lp.ex3
lp.ex3$solution

# 이차계획법은 목적함수가 2차식이고, 선형인 경우 최적화 문제
# quadprog패키지의 solve.QP()를 이용하면 된다.

install.packages("quadprog")
library(quadprog)
help("solve.QP")
QP <- 2*diag(c(1,2,4))
QP
d <- c(-1,-1,5)
A <- matrix(c(-1,1,0,0,0,-1,-1,0,0),nrow=3)
A
b <- c(-1,5,0)
qp1 <- solve.QP(QP,-d,t(A),b)
qp1$solution
qp1$value

# 일반적인 비선형최적화 
install.packages("Rsolnp")
library(Rsolnp)
help(solnp)
f <- function(x){
  4*x[1] - 2*x[2]
}
ctr <- function(x){
  x[1]^2 + x[2]^2
}
constraints <- c(41)
x0 <- c(1,1)

gnlp1 <- solnp(x0,fun=f,eqfun=ctr,eqB =constraints)
gnlp1$pars

x0 <- c(-5,-5)
gnlp2 <- solnp(x0,fun=f,eqfun=ctr,ineqLB = c(0),ineqUB = c(45))
gnlp2$pars
