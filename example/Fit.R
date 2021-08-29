# 데이터를 이론적 분포에 적합시키는 방법, 표본의 분포가 이론적 분포에 잘 적합되었는지 확인
# 분위수-분위수 그림을 그리면 자료의 분포가 정규분포에 잘 적합되었는지 알 수 있다. qqnorm함수를 이용하면 된다.
# plot 차트의 가시성을 높이기 위한 수정
par(mfrow=c(1,2))
# probeA의 분위수-분위수 그림
qqnorm(probeA)
# 1,3 사분 분위수를 사용해 정규분포 하에서의 이론적인 실선을 그림
qqline(probeA,distribution=qnorm,probs=c(0.25,0,75))
# 감마분포에 잘 적합되는지 보기 위해서 MASS패키지의 fitdistr() 함수를 이용해 shape, rate 모수를 측정

require("MASS")
fitdistr(probeA,'gamma')
# 감마 모수들을 저장
gamma.per <- fitdistr(probeA,'gamma')
# 감마 모수들을 확인하려면 str(gamma.per)

s <- gamma.par$estimate['shape']
r <- gamma.par$estimate['rate']
theoretical.probs <- seq(1:length(probeA))/(length(probeA)+1)
theoretical.quantiles <- qgamma(theoretical.probs,shape = s,rate = r)
plot(theoretical.quantiles, sort(probeA),xlab="Theoretical Quantiles",ylab="Sample Quantiles",main="Gamma QQ-plot")

qF <- function(p) qgamma(p,shape=s,rate=r)
qqline(y=sort(probeA),distribution=qF)

# Q-Q plot를 사용하는 대신 분포적합에 대한 통계적 검정법인 콜모고로프-스미르노프 검정,
# 앤더슨-달링 검정, 카이제곱 검정등을 고려할 수 있다. 콜모고로프 검정을 하려면 ks.test()함수를 이용

ks.test(probeA,"pgamma",3,2)

# 앤더슨-달링 검정을 이요해 정규분포에 잘 적합되는지 알아보려면 nortest패키지의 ad.test()함수를 이용

ad.test(probeA)

# p-value 를 결과적으로 구할 수 있게 되는데, p-value >0.05이면 가설이 채택되고, p-value <0.05면 기각된다.
# https://ko.wikipedia.org/wiki/%EC%9C%A0%EC%9D%98_%ED%99%95%EB%A5%A0
#



