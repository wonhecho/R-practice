pumpkin <- read.csv('messy_pumpkins.txt', stringsAsFactors = FALSE)
pumpkin

# 패턴 매칭은 문자열 내 문자들의 패턴을 식별하는 작업을 가르키는 말
pumpkin$weight[grep(pattern = "k", pumpkin$weight)]

pumpkin$weight[grep(pattern = ".", pumpkin$weight)]
pumpkin$weight[grep(pattern = "\\.", pumpkin$weight)]

pumpkin$weight[grep(pattern = "0+", pumpkin$weight)]

pumpkin$weight[grep(pattern = "\\D", pumpkin$weight)]

pumpkin$weight[grep(pattern = "\\d$", pumpkin$weight)]

corrected.data <- as.numeric(gsub(pattern = "[[:alpha:]]","",pumpkin$weight))

units.error.grams <- grep(pattern = "[[:digit:]]{4}",pumpkin$weight)
corrected.data[units.error.grams] <- corrected.data[units.error.grams] / 1000

european <- agrep(pattern = "europe", pumpkin$location, ignore.case = TRUE, max.dist=list(insertions = c(1), deletions = c(2)))
american <- agrep(pattern = "us", pumpkin$location, ignore.case = TRUE, max.dist = list(insertions=0, deletion=2, substitutions=0))
corrected.location <- pumpkin$location
corrected.location[european] <-"europe"
corrected.location[american] <-"US"

cleaned.pumpkin <- data.frame(corrected.data, corrected.location)
names(cleaned.pumpkin) <- c('weight', 'location')
cleaned.pumpkin

summary(cleaned.pumpkin)

cleaned.pumpkin.2 <- cleaned.pumpkin[cleaned.pumpkin$weight <= 10,]
cleaned.pumpkin[cleaned.pumpkin$weight >= 10,1] <- NA

x <- as.integer(seq(1,10,by=1))
y <- as.integer(seq(2,20,by=2))

object.size(x)
object.size(y)
object.size(y/x)

# x, y와 달리 y/x는 두 배의 메모리를 차지하고 있다.
# 타입이 double로 달라졌기 때문이다.

memory.limit()
memory.limit(8000)
A <- c(1:2E8)
B <- c(1:2E8)
C <- c(1:2E8)
ls()
rm(A)
ls()

#모든 객체를 삭제해버리려면
rm(list=ls())
memory.size()
