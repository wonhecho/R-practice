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
