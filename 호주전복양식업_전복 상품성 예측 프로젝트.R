####################################################################################
#################################### Abalone #######################################
####################################################################################

## 데이터 불러오기 & 행의 개수 확인
aba.name <- c("sex", "length" , "diameter", "height", "whole", "shucked", "viscera", "shell", "rings")
abalone <-  read.table('C:/balone_data.txt',sep=",", col.names = aba.name)
nrow(abalone) #4177

## R코드에서의 변수에 대한 설명
# Whole weight -> whole / Shucked weight -> shucked / Viscera weight -> viscera / Shell weight -> shell
# sex : 남 = 3 / 여 = 1 / 치패 = 2
# length : 껍질에 가장 긴 부분                    (mm)
# diameter : length의 수직                        (mm)
# height : meat 부분의 길이                       (mm)
# whole weight : 전체 무게                        (grams)
# Shucked weight : meat 부분의 무게               (grams)
# viscera weight : 내장 무게(소화기관, 피 제외)   (grams)
# Shell weight : 껍질의 무게(건조 후)             (grams)
# rings : 나이테 같은 느낌?, <예측 대상>

##                           < rings dist. >
# 1  :   1개 / 2  :   1개 / 3  :  15개 / 4  :  57개 / 5  : 115개 / 
# 6  : 258개 / 7  : 391개 / 8  : 567개 / 9  : 689개 / 10 : 634개 / 
# 11 : 487개 / 12 : 267개 / 13 : 203개 / 14 : 126개 / 15 : 103개 /
# 16 :  67개 / 17 :  58개 / 18 :  42개 / 19 :  32개 / 20 :  26개 /
# 21 :  22개 / 22 :   6개 / 23 :   9개 / 24 :   2개 / 25 :   1개 /
# 26 :   1개 / 27 :   2개 / 28 :   0개 / 29 :   1개

## 성별 빈도 그래프 + 막대그래프
#install.packages("ggplot2")
library(ggplot2)

table(abalone$sex)
sex_freq <- data.frame(Sex=c("F", "I", "M"),
                       Frequency=c(1307, 1342, 1528))

## Inside bars
ggplot(data=sex_freq, aes(x=Sex, y=Frequency)) +
    geom_bar(stat="identity", fill="steelblue") + labs(title="Frequency of Sex") + theme_minimal() + 
    theme(plot.title = element_text(hjust = 0.5, size=12))

## 기초통계량
summary(abalone)

## 각 변수들의 표준편차
sd(abalone$length)
sd(abalone$diameter)
sd(abalone$height)
sd(abalone$whole)
sd(abalone$shucked)
sd(abalone$viscera)
sd(abalone$shell)

## 각 변수들의 사분범위
IQR(abalone$length)
IQR(abalone$diameter)
IQR(abalone$height)
IQR(abalone$whole)
IQR(abalone$shucked)
IQR(abalone$viscera)
IQR(abalone$shell)


## 각 변수들의 히스토그램

ggplot(data=abalone,aes(x=length,fill="length"))+ labs(title="Histogram of Length") +
    geom_histogram() + theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size=12))

ggplot(data=abalone,aes(x=diameter,fill="diameter"))+ labs(title="Histogram of Diameter") +
    geom_histogram() + theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size=12))

ggplot(data=abalone,aes(x=height,fill="height"))+ labs(title="Histogram of Height") +
    geom_histogram() + theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size=12))

ggplot(data=abalone,aes(x=whole,fill="whole weight"))+ labs(title="Histogram of Whole weight") +
    geom_histogram() + theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size=12))

ggplot(data=abalone,aes(x=shucked,fill="shucked weight"))+ labs(title="Histogram of Shucked weight") +
    geom_histogram() + theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size=12))

ggplot(data=abalone,aes(x=viscera,fill="Viscera weight"))+ labs(title="Histogram of Viscera weight") +
    geom_histogram() + theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size=12))

ggplot(data=abalone,aes(x=shell,fill="shell weight"))+ labs(title="Histogram of Shell weight") +
    geom_histogram() + theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size=12))



## 각 변수들의 히스토그램과 박스플랏 그림
#install.packages("packHV")
library("packHV")
hist_boxplot(abalone$length, main="Summary of Length", xlab="Length")
hist_boxplot(abalone$diameter, main="Summary of Diameter", xlab="Diameter")
hist_boxplot(abalone$height, main="Summary of Height", xlab="Height")
hist_boxplot(abalone$whole, main="Summary of Whole weight", xlab="Whole weight")
hist_boxplot(abalone$shucked, main="Summary of Shucked weight", xlab="Shucked weight")
hist_boxplot(abalone$viscera, main="Summary of Viscera weight", xlab="Viscera weight")
hist_boxplot(abalone$shell, main="Summary of Shell weight", xlab="Shell weight")



## 각 변수들의 박스플랏
ggplot(data=abalone,aes(x=factor(rings),y=length,fill="length"))+ labs(title="Frequency of Rings") +
    geom_boxplot() + theme_minimal() + labs(x="Rings", y="Length") +
    theme(plot.title = element_text(hjust = 0.5, size=12))

ggplot(data=abalone,aes(x=factor(rings),y=diameter,fill="diameter"))+ labs(title="Frequency of Rings") +
    geom_boxplot() + theme_minimal() + labs(x="Rings", y="Diameter") +
    theme(plot.title = element_text(hjust = 0.5, size=12))

ggplot(data=abalone,aes(x=factor(rings),y=height,fill="height"))+ labs(title="Frequency of Rings") +
    geom_boxplot() + theme_minimal() + labs(x="Rings", y="Height") +
    theme(plot.title = element_text(hjust = 0.5, size=12))

ggplot(data=abalone,aes(x=factor(rings),y=whole,fill="Whole weight"))+ labs(title="Frequency of Rings") +
    geom_boxplot() + theme_minimal() + labs(x="Rings", y="Whole weight") +
    theme(plot.title = element_text(hjust = 0.5, size=12))

ggplot(data=abalone,aes(x=factor(rings),y=shucked,fill="Shucked weight"))+ labs(title="Frequency of Rings") +
    geom_boxplot() + theme_minimal() + labs(x="Rings", y="Shucked weight") +
    theme(plot.title = element_text(hjust = 0.5, size=12))

ggplot(data=abalone,aes(x=factor(rings),y=viscera,fill="Viscera weight"))+ labs(title="Frequency of Rings") +
    geom_boxplot() + theme_minimal() + labs(x="Rings", y="Viscera weight") +
    theme(plot.title = element_text(hjust = 0.5, size=12))

ggplot(data=abalone,aes(x=factor(rings),y=shell,fill="Shell weight"))+ labs(title="Frequency of Rings") +
    geom_boxplot() + theme_minimal() + labs(x="Rings", y="Shell weight") +
    theme(plot.title = element_text(hjust = 0.5, size=12))


## rings 변수의 빈도표 + 막대그래프
table(abalone$rings)
ggplot(data=abalone, aes(x=factor(rings))) + labs(title="Frequency of Rings") +
    geom_bar(fill="steelblue") + theme_minimal() + labs(x="Rings", y="Frequency") +
    theme(plot.title = element_text(hjust = 0.5, size=12))


####################################################################################
############################## 데이터 전처리 #########################################
####################################################################################

## Scale된 변수 단위 되돌리기
abalone$length = abalone$length * 200
abalone$diameter = abalone$diameter * 200
abalone$height = abalone$height * 200
abalone$whole = abalone$whole * 200
abalone$shucked = abalone$shucked * 200
abalone$viscera = abalone$viscera * 200
abalone$shell = abalone$shell * 200



## 이상치 대체

abalone1<-abalone
str(abalone1)

# 1. height =0 인 행 

# 1258 height는 rings==6이고 sex=="I" 인 height 평균으로 대체하고,
# 3997는 지우기 (whole weight 보다 shell weight가 더 큼)
abalone1[which(abalone1$height==0 & abalone1$rings==8),"height"]<- mean(subset(abalone1, rings==8 & sex=="I")$height)
abalone1<- abalone1[-which(abalone$height==0 & abalone$rings==6),]
nrow(abalone1) #4176

# 2. whole <= shucked+viscera+shell 인 행 제거 
nrow(subset(abalone1, whole <= shucked + viscera + shell)) 
abalone1 <- abalone1[(abalone1$whole > abalone1$shucked + abalone1$viscera + abalone1$shell),]
nrow(abalone1) #4017개

# 3. 그래프상 이상치 
library(ggplot2)
ggplot(abalone1, aes(x=height,y=whole))+geom_point() #튀는 값 3개 존재

# height만 작은 것 
abalone1[which(abalone1$whole==231.3 & abalone1$height < 10),]  #다른 값들은 거의 평균보다 큰데, height만 특이하게 작음
summary(abalone1[abalone1$rings==9 & abalone1$sex!="I",c(2:8)])
apply(abalone1[abalone1$rings==9 & abalone1$sex!="I",c(2:8)],2,quantile)

abalone1[which(abalone1$whole==231.3 & abalone1$height < 10),]$height<-33 #infant가 아닌 같은 rings 내 Q3값으로 대체


# height만 큰 것 
abalone1[which( abalone1$height > 150),]  #다른 값들은 거의 평균보다 작은데, height만 특이하게 큼 
summary(abalone1[abalone1$rings==8 & abalone1$sex!="I",c(2:8)])
apply(abalone1[abalone1$rings==8 & abalone1$sex!="I",c(2:8)],2,quantile)

abalone1[which(abalone1$height > 150),]$height<-24 #infant가 아닌 같은 rings 내 Q1 값으로 대체 

# height와 whole 둘다 큰 것
abalone1[which(abalone1$whole>400 & abalone1$height > 100),]  #유달리 큰 애 인데 무게가 특히 혼자 많이 나가는 것 같음 
summary(abalone1[abalone1$rings==10 & abalone1$sex!="I",c(2:8)])
apply(abalone1[abalone1$rings==8 & abalone1$sex!="I",c(2:8)],2,quantile) 
quantile(abalone1$height,prob=0.99)

abalone1[which(abalone1$whole>400 & abalone1$height > 100),]$height<-88  #99분위수의 2배로 대체 => 계속 튀는 것이 보임
abalone1<-abalone1[-which(abalone1$whole>400 & abalone1$height==88),]  #그 행 제거
ggplot(abalone1, aes(x=height,y=whole))+geom_point() #튀는 값 제거된 것 확인

# 4. Length < Diameter인 경우 입력 오류라고 판단하여 두 변수값 교체
abalone1$X<-c(1:nrow(abalone1))
subset(abalone1, length < diameter)
abalone1[which(abalone1$X==1163),]$diameter<- 37
abalone1[which(abalone1$X==1163),]$length<- 75 

#앞에서 생성한 X열 다시 삭제
abalone1<-abalone1[,c(1:9)]
str(abalone1) #4016행 9열


# 성별을 제외한 변수들의 상관계수를 확인
#install.packages("corrplot")
library(corrplot)
abalone_cor<-cor(abalone[,-1])
corrplot(abalone_cor, method="num") 

# 성별을 제외한 변수들의 다중공선성 확인
library(car)
lm1<-lm(rings~.-sex,data=abalone) 
vif(lm1) # 대다수의 변수들이 다중공성선이 나타남

## sex 분리
abalone1$adult[abalone1$sex == "M" | abalone1$sex == "F"] <- "1"  # Female,Male==>adult==1
abalone1$adult[abalone1$sex == "I"] <- 0  # infant==>adult==0
abalone1$adult <- as.factor(abalone1$adult)
str(abalone1) #4016열 10행


## 파생변수 생성
abalone2 <- abalone1 # 파생변수는 데이터 복사해서 생성

abalone2$rings <- as.numeric(abalone2$rings)

# volume : 4/3 * π * (가로*1/2) * (세로*1/2) * (높이*1/2) (타원체 부피 공식 적용)
abalone2$volume<-((abalone2$length)*(abalone2$diameter)*(abalone2$height))*1/6*pi

#내장무게/length : 전복의 크기를 한눈에 볼 수 있는 length 당 내장 무게 ==> 해석?

abalone2$vis_len<-(abalone2$viscera)/(abalone2$length)

# wgt_ratio : 지연언니가 생각한 변수, 전체무게에서 내장+살이 차지하는 비율
abalone2$wgt_ratio <- (abalone2$shucked + abalone2$viscera) / abalone2$whole

# shell_size : length/2 * diameter/2 * pi (타원 넓이 공식 적용)
# 고리가 껍질에 생기는것이다보니, 고리수는 아는데에 있어서 껍질의 사이즈가 중요하지 않을까 생각함
abalone2$shell_size <- (abalone2$length/2) * (abalone2$diameter/2) * pi

# size_gr : 나형이가 말했던 성장에 대해 생각하다가 만든 변수
# 위에서 만든 shell_size를 rings+1.5(나이) 로나눔
# 즉, 연간 평균적으로 전복의 껍질이 어느정도 커졌는지를 나타냄
abalone2$size_gr<- (abalone2$shell_size)/((abalone2$rings)+1.5)

# shell_ratio : 껍질 무게 / 껍질 넓이
abalone2$shell_ratio<- with(abalone2, shell/shell_size)

#단위부피 당 무게 변수 생성

# meat_unit : 단위부피당 살+내장의 무게 
# meat_unit이라 한것은 그냥 살+내장을 뭐라 표현할지 몰라서!ㅎㅎ
abalone2$meat_unit <- (abalone2$shucked+abalone2$viscera)/abalone2$volume
abalone2$who_unit<-abalone2$whole/abalone2$volume
abalone2$shu_unit<-abalone2$shucked/abalone2$volume
abalone2$vis_unit<-abalone2$viscera/abalone2$volume
abalone2$she_unit<-abalone2$shell/abalone2$volume

str(abalone2)  #4016열 21변수

abalone2$X<-c(1:nrow(abalone2))



## 파생변수 그래프 확인

ggplot(data=abalone2,aes(x=factor(rings),y=volume,fill=adult))+ labs(title="Frequency of Rings") +
    geom_boxplot() + theme_minimal() + labs(x="Rings", y="Volume") +
    theme(plot.title = element_text(hjust = 0.5, size=12))

ggplot(data=abalone2,aes(x=factor(rings),y=vis_len,fill=adult))+ labs(title="Frequency of Rings") +
    geom_boxplot() + theme_minimal() + labs(x="Rings", y="Viscera/Length") +
    theme(plot.title = element_text(hjust = 0.5, size=12))

ggplot(data=abalone2,aes(x=factor(rings),y=wgt_ratio,fill=adult))+ labs(title="Frequency of Rings") +
    geom_boxplot() + theme_minimal() + labs(x="Rings", y="Wgt_Ratio") +
    theme(plot.title = element_text(hjust = 0.5, size=12))

ggplot(data=abalone2,aes(x=factor(rings),y=shell_size,fill=adult))+ labs(title="Frequency of Rings") +
    geom_boxplot() + theme_minimal() + labs(x="Rings", y="Shell_Size") +
    theme(plot.title = element_text(hjust = 0.5, size=12))

ggplot(data=abalone2,aes(x=factor(rings),y=size_gr,fill=adult))+ labs(title="Frequency of Rings") +
    geom_boxplot() + theme_minimal() + labs(x="Rings", y="Size_Growth") +
    theme(plot.title = element_text(hjust = 0.5, size=12))

ggplot(data=abalone2,aes(x=factor(rings),y=meat_unit,fill=adult))+ labs(title="Frequency of Rings") +
    geom_boxplot() + theme_minimal() + labs(x="Rings", y="Meat_unit") +
    theme(plot.title = element_text(hjust = 0.5, size=12))

ggplot(data=abalone2,aes(x=factor(rings),y=who_unit,fill=adult))+ labs(title="Frequency of Rings") +
    geom_boxplot() + theme_minimal() + labs(x="Rings", y="Whole_unit") +
    theme(plot.title = element_text(hjust = 0.5, size=12))

ggplot(data=abalone2,aes(x=factor(rings),y=shu_unit,fill=adult))+ labs(title="Frequency of Rings") +
    geom_boxplot() + theme_minimal() + labs(x="Rings", y="Shucked_unit") +
    theme(plot.title = element_text(hjust = 0.5, size=12))

ggplot(data=abalone2,aes(x=factor(rings),y=vis_unit,fill=adult))+ labs(title="Frequency of Rings") +
    geom_boxplot() + theme_minimal() + labs(x="Rings", y="Viscera_unit") +
    theme(plot.title = element_text(hjust = 0.5, size=12))

ggplot(data=abalone2,aes(x=factor(rings),y=she_unit,fill=adult))+ labs(title="Frequency of Rings") +
    geom_boxplot() + theme_minimal() + labs(x="Rings", y="Shell_unit") +
    theme(plot.title = element_text(hjust = 0.5, size=12))




## 파생변수 그래프 확인 (adult분리X)

ggplot(data=abalone2,aes(x=factor(rings),y=volume,fill="volume"))+ labs(title="Frequency of Rings") +
    geom_boxplot() + theme_minimal() + labs(x="Rings", y="Volume") +
    theme(plot.title = element_text(hjust = 0.5, size=12))

ggplot(data=abalone2,aes(x=factor(rings),y=vis_len,fill="vis_len"))+ labs(title="Frequency of Rings") +
    geom_boxplot() + theme_minimal() + labs(x="Rings", y="Viscera/Length") +
    theme(plot.title = element_text(hjust = 0.5, size=12))

ggplot(data=abalone2,aes(x=factor(rings),y=wgt_ratio,fill="wgt_ratio"))+ labs(title="Frequency of Rings") +
    geom_boxplot() + theme_minimal() + labs(x="Rings", y="Wgt_Ratio") +
    theme(plot.title = element_text(hjust = 0.5, size=12))

ggplot(data=abalone2,aes(x=factor(rings),y=shell_size,fill="shell_size"))+ labs(title="Frequency of Rings") +
    geom_boxplot() + theme_minimal() + labs(x="Rings", y="Shell_Size") +
    theme(plot.title = element_text(hjust = 0.5, size=12))

ggplot(data=abalone2,aes(x=factor(rings),y=size_gr,fill="size_gr"))+ labs(title="Frequency of Rings") +
    geom_boxplot() + theme_minimal() + labs(x="Rings", y="Size_Growth") +
    theme(plot.title = element_text(hjust = 0.5, size=12))

ggplot(data=abalone2,aes(x=factor(rings),y=meat_unit,fill="meat_unit"))+ labs(title="Frequency of Rings") +
    geom_boxplot() + theme_minimal() + labs(x="Rings", y="Meat_unit") +
    theme(plot.title = element_text(hjust = 0.5, size=12))

ggplot(data=abalone2,aes(x=factor(rings),y=who_unit,fill="who_unit"))+ labs(title="Frequency of Rings") +
    geom_boxplot() + theme_minimal() + labs(x="Rings", y="Whole_unit") +
    theme(plot.title = element_text(hjust = 0.5, size=12))

ggplot(data=abalone2,aes(x=factor(rings),y=shu_unit,fill="shu_unit"))+ labs(title="Frequency of Rings") +
    geom_boxplot() + theme_minimal() + labs(x="Rings", y="Shucked_unit") +
    theme(plot.title = element_text(hjust = 0.5, size=12))

ggplot(data=abalone2,aes(x=factor(rings),y=vis_unit,fill="vis_unit"))+ labs(title="Frequency of Rings") +
    geom_boxplot() + theme_minimal() + labs(x="Rings", y="Viscera_unit") +
    theme(plot.title = element_text(hjust = 0.5, size=12))

ggplot(data=abalone2,aes(x=factor(rings),y=she_unit,fill="she_unit"))+ labs(title="Frequency of Rings") +
    geom_boxplot() + theme_minimal() + labs(x="Rings", y="Shell_unit") +
    theme(plot.title = element_text(hjust = 0.5, size=12))




# 1. volume 이상치 행 추출
subset(abalone2,volume >= 500000) # 1371,1694

summary(abalone2)

# 2. vis_len 이상치 추출
subset(abalone2,vis_len >= 0.93) # 1694

# 3. size_gr 이상치 : 996,1686,2235

subset(abalone2, size_gr >= 1280 & rings == 6 | size_gr >= 1280 & rings == 7)
subset(abalone2, size_gr >= 600 & rings == 23)

# 4. meat_unit 이상치 : 2616,3571 둘다 99퍼센타일보다 큼

subset(abalone2,meat_unit >= 0.0021)
quantile(abalone2$meat_unit, probs=c(0,0.25, 0.5,0.75, 0.99))

# 5. who_unit 이상치 : 2616,3571 둘다 99퍼센타일보다 큼

subset(abalone2,who_unit >= 0.0031)
quantile(abalone2$who_unit, probs=c(0,0.25, 0.5,0.75, 0.99))

# 6. shu_unit 이상치 : 2616,3571 둘다 99퍼센타일보다 큼

subset(abalone2, shu_unit >= 0.00141)
quantile(abalone2$shu_unit, probs=c(0,0.25, 0.5,0.75, 0.99))

# 7. vis_unit 이상치 : 3381 10퍼센타일보다 작음 

subset(abalone2, vis_unit <= 0.00003)
quantile(abalone2$vis_unit, probs=c(0.1,0.25, 0.5,0.75, 0.99))

# 8. she_unit 이상치 : 3571 99퍼센타일보다 큼

subset(abalone2, she_unit >= 0.00093)
quantile(abalone2$she_unit, probs=c(0,0.25, 0.5,0.75, 0.99))




## 파생변수 생성 후 이상치 제거

# 1. volume변수 이상치 => 2개모두 제거
# 1694 : volume, vis_len에서 모두 이상치
# 1371 : 무게가 가벼운건 아니지만, 크기에 비해 무게는 가벼운 편이라 단위부피당 무게가 25퍼센타일이 안됨


# 2. size_gr변수 이상치 => 3개모두 남김
#996,1686,2235 : 같은 rings의 전복에 비해 길이, 무게가 모두 큰 전복 => 문제없이 정상적으로 자람


# 3. unit변수들 이상치 => 3개모두 제거
# 2616,3571 : 모든변수들이 너무 큼 / 3381 : 특정변수는 값이 큰데 vis_unit만 매우 작은것으로보아 내장이 제대로 형성되지 못한 이상한 전복

# 8개의 이상치중에 5개 제거 => 1694,1371, 2616,3571,3381
str(abalone2) # 이상치 제거 전 : 4016행

abalone3 <- subset(abalone2,X !=1694 & X != 1371 & X != 2616 & X !=3571 & X != 3381)

str(abalone3) # 이상치 제거 후 : 4011행



## rings 재범주화 

# 1. Size_Grwoth_sc 그래프만 확인

ggplot(data=abalone3,aes(x=factor(rings),y=size_gr,fill=adult))+ labs(title="Frequency of Rings") +
    geom_boxplot() + theme_minimal() + labs(x="Rings", y="Size_Growth_sc") +
    theme(plot.title = element_text(hjust = 0.5, size=12))+
    geom_vline(xintercept = c(8.5),col="red",lty=2,size=1)

# 2. 8까지 증가, 9,10 일정, 11이후로 감소하는 느낌

abalone3$rings <- as.numeric(abalone3$rings)

abalone3$status[abalone3$rings <=9] <- 0
abalone3$status[abalone3$rings >=10 ] <-1

# 3. 재범주화 후 status 그래프 확인

table(abalone3$status)

status_freq <- data.frame(Status=c(0,1),
                          Frequency=c(1972,2039))

## Inside bars
ggplot(data=status_freq , aes(x=Status, y=Frequency)) +
    geom_bar(stat="identity", fill="steelblue") + labs(title="Frequency of Status") + theme_minimal() + 
    theme(plot.title = element_text(hjust = 0.5, size=12))




#########################################################
##### 파생변수 생성후, 상관계수확인 + 변수제거를 여러단계에 걸쳐 진행함


## 상관계수 확인 1

library(corrplot)
str(abalone3)

# 원본변수, 파생변수 모두
for_corr1 <- cor(abalone3[,-c(1,10,22,23)])
corrplot(for_corr1,method="num") 

# 파생변수만
for_corr2 <- cor(abalone3[,c(11:21)])
corrplot(for_corr2,method="num")

## 변수제거 1
# 원본변수 중 length, diameter, shucked, viscera는 파생변수들이 그 역할을 하고 있으며
# 다른 변수들끼리의 상관관계도 과하게 큰 편이라 지우도록 한다.

abalone3_1 <- abalone3[,-c(2,3,6,7)]
str(abalone3_1)


## 상관계수 확인 2
for_corr3 <- cor(abalone3_1[,-c(1,6,18,19)])
corrplot(for_corr3,method="num")

## 변수제거 2
# 파생변수중, vis_len은 adult를 생성하기 위한 역할에 중점을 둔 변수였으며
# vis_len보다 전복의 특성을 더 잘 설명할 수 있는 변수들이 있다고 판단해 제거

abalone3_2 <- abalone3_1[,-8]
str(abalone3_2)


## 상관계수 확인 3
for_corr4 <- cor(abalone3_2[,-c(1,6,17,18)])
corrplot(for_corr4,method="num")

## 변수제거 3
# 파생변수중, volume은 다중공선성 해결을 위한 unit변수에 사용됨
# 기타 파생변수들에 비해 원본변수들과의 상관계수가 과하게 큼

abalone3_3 <- abalone3_2[,-7]
str(abalone3_3)


## 상관계수 확인 4
for_corr5 <- cor(abalone3_3[,-c(1,6,16,17)])
corrplot(for_corr5,method="num")

## 변수제거 4
# meat_unit 은 단위부피당 고기+내장의 무게인데, 타변수에 비해 이 변수가 제공하는 정보가 유의미하지 않아보임

abalone3_4 <- abalone3_3[,-11]
str(abalone3_4)

## 다중공선성 확인 4

abalone3_4$status<-as.numeric(as.character(abalone3_4$status))
for_vif3_4 <- lm(status~.-X-sex-rings,data=abalone3_4)
vif(for_vif3_4)


## 변수제거 5
# whole변수를 선택, who_unit 제거

abalone3_5 <- abalone3_4[,-11]
str(abalone3_5)

## 다중공선성 확인 5
for_vif3_5 <- lm(status~.-X-sex-rings,data=abalone3_5)
vif(for_vif3_5)


## 변수제거 6
# she_unit변수를 선택, shell 제거

abalone3_6 <- abalone3_5[,-4]
str(abalone3_6)

## 다중공선성 확인 6
for_vif3_6 <- lm(status~.-X-sex-rings-size_gr,data=abalone3_6)
vif(for_vif3_6)


## 변수제거 7
# height제거

abalone3_7 <- abalone3_6[-2]
str(abalone3_7)

## 다중공선성 확인 7
for_vif3_7 <- lm(status~.-X-sex-rings-size_gr,data=abalone3_7)
vif(for_vif3_7)



## 변수제거 8
# vif확인 결과 VIF지수가 가장 큰 whole제거
# 더이상 사용되지 않을 sex,rings,size_gr,X 제거

abalone3_8 <- abalone3_7[,-c(1,2,3,7,12)]
str(abalone3_8) # 총 4011개행, 8개 변수



## 변수 정규화

# 1. 정규화 함수 생성
minmax_scaler <- function(x) {
    (((x - min(x))) / (max(x) - min(x))) }

# 2. 변수 정규화
abalone3_9<-abalone3_8

abalone3_9$wgt_ratio_sc <- minmax_scaler(abalone3_9$wgt_ratio)
abalone3_9$shell_ratio_sc <- minmax_scaler(abalone3_9$shell_ratio)
abalone3_9$shell_size_sc <- minmax_scaler(abalone3_9$shell_size)
abalone3_9$shu_unit_sc <- minmax_scaler(abalone3_9$shu_unit)
abalone3_9$vis_unit_sc <- minmax_scaler(abalone3_9$vis_unit)
abalone3_9$she_unit_sc <- minmax_scaler(abalone3_9$she_unit)

# 3. 변수 정규화 후 상관계수 확인
str(abalone3_9) # 4011행 14개 변수


# 정규화 전 변수들 삭제

abalone4 <- abalone3_9[,c(8,1,9:14)]

str(abalone4) # 4011행, 8개 변수





########################### 모델링 ################################

### 모델링 용 데이터 생성

data<- abalone4 
data$status <- as.factor(data$status)
str(data)
# 최종데이터 abalone4를 data로 복사해 모델을 구축하도록 한다.

################# 로지스틱 모델 #################

########## K-fold 
# 최적의 모델과, 최적의 cut off value를 찾기위해 fold수가 4인 k-fold를 실행한다. 

#install.packages("plyr")
library(plyr)


# idx 설정 (8 : 2) : train_valid set : test set의 비율을 8 : 2로 나누도록 한다.
set.seed(1234)
idx <- sample(x = c("train_valid", "test"),size = nrow(data),replace = TRUE,prob = c(8, 2)) 



# idx에 따라 train_valid / test set 나누기 

train_valid <- data[idx == "train_valid", ] 

test <- data[idx == "test", ] 



# test 데이터 독립변수/종속변수 나누기 

test_x <- test[, -1] 
test_y <- test[, 1]

str(test_x);str(test_y)


# cross validation 결과테이블 생성

result <- data.frame(fold = rep(c(1, 2, 3, 4), each = 82),
                     mdl = rep(c("full", "step"), 41),
                     i = rep(seq(0, 1, length.out = 41), 8),
                     accuracy = rep(rep(NA, 41), 8))



# idx2에 따라 train / valid set 나누기 
set.seed(1234)
idx2 <- sample(x = c(1:4), size = nrow(train_valid), replace = TRUE, prob = c(1, 1, 1,1)) 

for(k in c(1, 2, 3, 4)){
    
    # idx2에 따라 train vs. valid 데이터 나누기 
    
    valid <- train_valid[idx2 == k, ]
    train <- train_valid[idx2 != k, ]
    
    
    # valid 데이터 설명변수/반응변수 나누기 
    
    valid_x <- valid[, -1]
    valid_y <- valid[, 1] 
    
    # 로지스틱 회귀분석 모델 생성 
    full <- glm(formula = status ~ .,data = train,family = binomial())
    
    # Stepwise
    step <- step(full,trace = F)
    
    # full과 step 모델별 확률 예측 
    
    full_pred_p <- as.numeric(predict(object = full,newdata = valid_x,type = "response"))
    step_pred_p <- as.numeric(predict(object = step,newdata = valid_x,type = "response"))
    
    # 분류 정확도의 분모 
    l <- length(valid_y)
    
    for(i in unlist(unique(result$i))){
        
        # i를 기준으로 0 또는 1로 분류 
        full_pred_class <- ifelse(full_pred_p < i, levels(valid_y)[1], levels(valid_y)[2])
        step_pred_class <- ifelse(step_pred_p < i, levels(valid_y)[1], levels(valid_y)[2])
        
        # 분류 정확도 계산 
        full_accuracy <- sum(full_pred_class == valid_y) / l
        step_accuracy <- sum(step_pred_class == valid_y) / l
        
        # result 테이블에 분류 정확도 입력 
        result[result$fold == k& result$mdl == "full"& result$i == i, "accuracy"] <- full_accuracy
        result[result$fold == k& result$mdl == "step"& result$i == i, "accuracy"] <- step_accuracy
        
    } 
}

str(valid)


## k-fold 결과 수치로 확인

# 모델별 임계치별 평균 분류 정확도 계산하기
tmp <- ddply(result, .(mdl, i), summarise, avg_accuracy = mean(accuracy))

# 분류정확도가 가장높은 임계치알아보기
tmp[tmp$avg_accuracy == max(tmp$avg_accuracy), ]


## k-fold결과 시각화

# full 모델 k-Fold = 1 
plot(accuracy ~ i,
     result[result$mdl == "full" & result$fold == 1, ],
     type = "l",col = alpha("purple", 0.4),
     ylim = c(0.3, 1),xlab = "임계치", 
     ylab = "분류 정확도", 
     main = "분류 정확도 in full/step 3-Fold CV") 

# full 모델 k-Fold = 2 
lines(accuracy ~ i,
      result[result$mdl == "full" & result$fold == 2, ],
      col = alpha("orange", 0.4)) 

# full 모델 k-Fold = 3 

lines(accuracy ~ i,
      result[result$mdl == "full" & result$fold == 3, ],
      col = alpha("green", 0.4)) 

# full 모델 k-Fold = 4

lines(accuracy ~ i,
      result[result$mdl == "full" & result$fold == 4, ],
      col = alpha("pink", 0.4)) 


# step 모델 k-Fold = 1 

lines(accuracy ~ i,
      result[result$mdl == "step" & result$fold == 1, ],
      col = alpha("purple", 0.5),lty = 2) 

# step 모델 k-Fold = 2 

lines(accuracy ~ i,
      result[result$mdl == "step" & result$fold == 2, ],
      col = alpha("orange", 0.5),lty = 2) 

# step 모델 k-Fold = 3 
lines(accuracy ~ i,
      result[result$mdl == "step" & result$fold == 3, ],
      col = alpha("green", 0.5),lty = 2) 

# step 모델 k-Fold = 4

lines(accuracy ~ i,
      result[result$mdl == "full" & result$fold == 4, ],
      col = alpha("pink", 0.4)) 



# 범례 그리기 
legend("topleft",
       c("full k=1", "full k=2", "full k=3","full=4",
         "step k=1", "step k=2", "step k=3","step=4"),
       col = c(alpha(c("purple", "orange", "green","pink"), 0.4),
               alpha(c("purple", "orange", "green","pink"), 0.5)),
       lty = rep(c(1, 2), each = 4),bty = "n",cex = 0.9)




# full 모델
lines(avg_accuracy ~ i,
      tmp[tmp$mdl == "full", ],
      col = alpha("red", 0.7),
      lty = 1,
      type = "o",
      pch = 20)

# step 모델
lines(avg_accuracy ~ i,
      tmp[tmp$mdl == "step", ],
      col = alpha("red", 0.7),
      lty = 2,
      type = "o",
      pch = 20)

# 범례 그리기
legend("topright",
       c("full avg accuracy", "step avg accuracy"),
       pch = 20,
       col = alpha("red", 0.7),
       lty = rep(c(1, 2)),
       bty = "n",
       cex = 0.9)


# 가장 최적의 모델과 cut off value : full-model & step-mmodel / 0.525


## k-fold결과를 바탕으로 test 

# confusion matrix#

#install.packages("caret")
library(caret)

cutoff <- 0.525

## full model
probability_f <- predict(full, newdata=test_x,type="response")
predicted_f <- ifelse(probability_f > cutoff ,1,0)
predicted_f <- as.factor(predicted_f)

confusionMatrix(test$status,predicted_f)


## step model
probability_s <- predict(step, newdata=test_x,type="response")
predicted_s <- ifelse(probability_s > cutoff ,1,0)
predicted_s <- as.factor(predicted_s)

confusionMatrix(test$status,predicted_s)


summary(full)
summary(step)

## ROC커브
# full model
library(ROCR)

pred_f<-prediction(probability_f,test_y)

perf_f<-performance(pred_f,'tpr','fpr') #민감도와 1-특이도 계산 과정

plot(perf_f,lty=1,col=2,xlim=c(0,1),ylim=c(0,1),xlab='1-Specificity',ylab='Sensitivity',main='ROC Curve of Full model')

lines(x=c(0,1),y=c(0,1),col='grey')

## ROC커브
# step model


pred_s<-prediction(probability_s,test_y)

perf_s<-performance(pred_s,'tpr','fpr') #민감도와 1-특이도 계산 과정

plot(perf_s,lty=1,col=2,xlim=c(0,1),ylim=c(0,1),xlab='1-Specificity',ylab='Sensitivity',main='ROC Curve of Step model')

lines(x=c(0,1),y=c(0,1),col='grey')


#################################### 랜덤포레스트 #########################################
#install.packages("randomForest")
library(randomForest)



########## K-fold 
# 최적의 모델과, 최적의 cut off value를 찾기위해 fold수가 5인 k-fold를 실행한다. 

set.seed(1234)
t_index <- sample(1:nrow(data), size=nrow(data))
split_index <- split(t_index, 1:5)

accuracy_rf <- c()        # 데이터를 받을 빈 벡터
for(i in 1:5){
    test <- data[split_index[[i]],]  
    train <- data[-split_index[[i]],]  
    
    set.seed(1234)
    abalone_rf <- randomForest(status~., data=train, proximity=TRUE, importance=TRUE)
    windows()
    plot(abalone_rf)
    importance(abalone_rf)  
    windows()
    varImpPlot(abalone_rf)  
    
    
    test_pred <- predict(abalone_rf, test)
    table <- table(real=test$status, predict=test_pred)
    print(confusionMatrix(test$status,test_pred))
    
    #정확도
    accuracy_rf[i] <- sum(diag(table))/sum(table)
}

accuracy_rf
mean(accuracy_rf) 


### 평균정확도랑 가장 비슷한 정확도를 가진 데이터셋의 confusion matrix

test4 <- data[split_index[[4]],]  
train4 <- data[-split_index[[4]],]  

set.seed(1234)
abalone_rf4 <- randomForest(status~., data=train4, proximity=TRUE, importance=TRUE)
plot(abalone_rf4)
importance(abalone_rf4)    
varImpPlot(abalone_rf4,main = "Variable Importance of RandomForest4")     

probability4 <- predict(abalone_rf4, newdata=test4[,-1],type="response")

confusionMatrix(test4$status,probability4)





