# 데이터의 높은 차수의 적률은 왜도, 첨도와 같은 형태의 모수를 추정할 때, 정규분포가 얼마나 거리가 있는지를 측정하는 데에 사용.
# probeA 데이터 분포가 정규분포에 비해 왼쪽으로 긴 꼬리를 갖는지, 오른쪽으로 꼬리를 갖는지 알고싶을때,
# 치우친 정도(왜도)는 skewness() 함수를 이용

require("fBasics")
skewness(probeA)

# 왜도의 절대값이 0과 유의하게 다른지 여부를 t-검저을 실시해 알아본다.
# 왜도의 표준오차는 6을 표본크기로 나눈 값의 제곱근으로 근사된다.
abs(skewness(probeA))/sqrt(6/length(probeA))
# 결과물이 0.3885....
# 왜도가 실제로는 0인데 t-값이 우연히 0.389가 될 확률을 계산하기만 하면 된다.
1 - pt(0.389,41)

# p-value가 0.05보다 크무로 귀무가설을 기각할 수 없으며, 왜도는 0과 유의하게 다르지 않다는 결론을 내린다.
# probeA 데이터 분포의 치우친 정도는 정규분포와 크게 다르지 않다.

#첨도값 : 정규곡선은 첨도값이 0 표본으로 구한 첨도값의 표준오차는 24를 표본크기로 나눈 값의 제곱근으로
#잘 근사된다. R에서 첨도값을 계산하려면 fBasics 패키지의 kurtosis()함수를 사용

kurtosis(probeA)
abs(kurtosis(probeA))/sqrt(24/length(probeA))
1-pt(0.75,41)

# 분포적합도를 검정하는 방법
# propagate는 여러가지 분포를 한꺼번에 적합시킨뒤 적절한 분포를 고를수 있게 해준다.
# 각 적합결과는 AIC값의 순서대로 오름차순으로 정렬되는데, AIC값이 최소인 것이 좋은것
# 적합도와 모형의 복잡도, 타협점을 설명해준다.

require("propagate")
set.seed(275)
observations <- rnorm(10000,5)
distTested <- fitDistr(observations)  

# propagate()함수로부터 얻은 결과를 적합시키는 방법.
EXPR <- expression(x^(3*y)-1)
x <- c(6,0.1)
y <- c(2,0.1)
DF <- cbind(x,y)
RES <- propagate(expr=EXPR, data = DF, type="stat",do.sim=TRUE,verbose=TRUE)
RES
testedDistrEXPR <- fitDistr(RES)

