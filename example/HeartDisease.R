#data set Heart.csv
# https://www.kaggle.com/ronitf/heart-disease-uci

library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)

heart_data <- read.csv("./heart.csv")
head(heart_data)

colnames(heart_data)
heart_data <- rename(heart_data, "age"="癤풹ge")
print(colnames(heart_data[1]))

str(heart_data)
summary(heart_data)
nrow(heart_data)
dim(heart_data)

install.packages("corrplot")
library(corrplot)
corrplot(cor(heart_data),type="upper")

# age - 나이
# sex - (1=남성, 0=여성)
# cp - 가슴 통증 유형 (0,1,2,3,4)
# trestbps - 안정 혈압(병원 입원시 mm Hg)
# chol - 혈청 콜레스테롤(mg/dl)
# fbs - (공복혈당 > 120mg/dl)(1 = true, 0 = false)
# restecg - 안정 심전도 결과(0,1,2)
# thalach - 최대 심박동수
# exang - 협심증 유발 운동(1=yes,0=no)
# oldpeak - 비교적 안정되기까지 운동으로 유발되는 ST depression
# slope - 최대 운동 ST segment의 기울기
# ca - 형광 투시된 주요 혈관의 수(0-3)
# thal - (3=보통; 6=해결된 결함; 7 = 해결가능한 결함)
# target - 심장병 진단(1=true;0=false)

corrplot(cor(heart_data),type="lower")

heart_data$sex[heart_data$sex == 0] = "Female"
heart_data$sex[heart_data$sex == 1] = "Male"
head(heart_data$sex)

# angina 협심증

heart_data$cp[heart_data$cp==0] = "typical_angina"
heart_data$cp[heart_data$cp==1] = "atypical_angina"
heart_data$cp[heart_data$cp==2] = "non_anginal_pain"
heart_data$cp[heart_data$cp==3] = "asymptotic"

heart_data$cp

heart_data$exang[heart_data$exang == 1] = "yes"
heart_data$exang[heart_data$exang == 0] = "no"

heart_data$slope

heart_data$slope[heart_data$slope==0]="upsloping"
heart_data$slope[heart_data$slope==1]="flat"
heart_data$slope[heart_data$slope==2]="downsloping"

heart_data$slope

heart_data$target1 = heart_data$target

heart_data$target1[heart_data$target1==0] = "No Disease"
heart_data$target1[heart_data$target1==1] = "Disease"

round(prop.table(table(heart_data$target1)),2)

ggplot(heart_data,aes(x=age))+geom_histogram(bins=50)+
  labs(x="Age",y="Density",title="Age wise distribution of population")

heart_data$age_group <- cut(heart_data$age, breaks=seq(25,75,5))

age_group <- heart_data %>% group_by(age_group) %>% summarize(target = sum(target))
na.omit(age_group)

age_plot <- heart_data %>%
            group_by(age_group) %>%
            summarize(target=sum(target)) %>%
            ggplot(aes(x=age_group, y=target, fill=target))+geom_col(position = "dodge") +
            labs(x = "Age Group", y = "Target", title = "Heart disease wise age group")
na.omit(age_plot)

age_group <- heart_data %>%
             group_by(age_group) %>%
             summarize(heart_dis_prop = round(sum(target)/n(),3)*100)
na.omit(age_group)

age_group <- heart_data %>%
              group_by(age_group) %>%
              summarize(heart_dis_prop = round(sum(target)/n(),3)*100) %>%
              ggplot(aes(x=age_group, y=heart_dis_prop, fill=age_group)) + geom_col(position = "dodge")+
              labs(x="Age Group",y="Heart Disease Propertation",title="Age group heart disease proporation")
na.omit(age_group)

round(prop.table(table(heart_data$sex)),2)

round(prop.table(table(heart_data$sex, heart_data$target1)),2)

heart_rate <- heart_data %>%
              group_by(target1) %>%
              count(cp)
heart_rate
heart_rate <- heart_data %>%
              group_by(sex) %>%
              count(cp)
heart_rate

heart_data %>%
        group_by(sex, target1) %>%
        count(thalach)

heart_data %>%
        group_by(sex, target1) %>%
        count(slope)

ggplot(heart_data, aes(x=slope, fill=target1))+
    geom_bar(position="dodge")+
    labs(x="Slope", y="Count", title = "Analysis types of Slope")

mosaicplot(table(heart_data$target1,heart_data$ca),
           col=c("#754869","coral","skyblue","#423f42","#ed18c6"),las=1,main="Heart Disease for Major Vessels")

mosaicplot(table(heart_data$sex,heart_data$ca),
           col=c("#754869","coral","skyblue","#423f42","#ed18c6"),las=1,main="Heart Disease for Major Vessels")

ggplot(heart_data, aes(x=oldpeak, fill=target1)) + geom_boxplot() + coord_flip() + labs(x="St Depressoin", y="Heart Disease State", title="ST Depressoin Induced by Exercise vs Heart Disease")

ggplot(heart_data, aes(x=oldpeak, fill=sex)) + geom_boxplot() + coord_flip()

ggplot(heart_data,aes(x=trestbps,fill=target1)) + geom_boxplot() + coord_flip() + labs(x="Heart Disease Stage", y="Resting Blood Pressure", title="Resting Blood Pressure by Heart Condition")

ggplot(heart_data, aes(x=thalach, fill=target1)) + geom_boxplot() + coord_flip() + labs(x="Heart Rate", y="Density")

ggplot(heart_data, aes(x=age, y=thalach, color=target1, size=factor(thalach))) + geom_point(alpha=0.3)+
      labs(x="Age",y="Maximum Heart Rate", title="Maximum Heart Rate for Heart Disease by Age", color = "Heart Disease State")+
      guides(size="none")
ggplot(heart_data, aes(x=age, y=trestbps, color=target1, size=factor(trestbps))) + geom_point(alpha=0.3)+
      labs(x="Age",y="Resting Bloop Pressure", title = "Age vs Bloop pressure for the heart Disease", color= "Heart Disease State") + guides(size="none")
ggplot(heart_data,aes(x=age,y=oldpeak, color=target1, size=factor(oldpeak)))+ geom_point(alpha=0.4)+
      labs(x="AGE", y="OLDPEAK",title = "Age vs. Type of Depression for Heart Disease",color="Heart Disease State") + guides(size="none")
