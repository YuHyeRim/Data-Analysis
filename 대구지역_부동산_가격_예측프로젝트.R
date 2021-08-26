library(ggplot2)
library(gcookbook)
library(plyr)
library(reshape2)
library(corrplot)
library(car)
library(lmerTest)		# note that the lmerTest package and RLRsim package do not work together.
library(RLRsim)


### Kaggle의 자료를 바탕으로 대구집값을 분석해 보도록 한다.

## 데이터 불러오기
data<-read.csv("C:/Daegu_Real_Estate_data.csv")
str(data)
head(data)


## 데이터 탐색
par(mfrow=c(2,2))
i=17
table(data[,i])
summary(data[,i])
boxplot(data$SalePrice ~ data[,i])

data$TimeToSubway
data$SubwayStation
table(data$TimeToSubway)
summary(data$w)
boxplot(data$SalePrice ~ data$w)

##################### EDA #####################

## 논리적 오류데이터 수정 (data1 생성)
# Station 변수에서 지하철이 없는데, TimeToSubway변수에서 0~5로 되어있는 변수들 행 제거
number<-with(data, which(SubwayStation=="no_subway_nearby"&TimeToSubway!="no_bus_stop_nearby"))
data1<-data[-number,]
nrow(data1)

# TimeToSubway에 "no_bus_stop_nearby" 를 TimeToBusStop으로 변경
data1$TimeToSubway<-as.character(data1$TimeToSubway)
data1$TimeToSubway[data1$TimeToSubway=="no_bus_stop_nearby"]<-"no_subway_stop_nearby"
data1$TimeToSubway<-as.factor(data1$TimeToSubway)
levels(data1$TimeToSubway)
data1$TimeToBusStop<-as.character(data1$TimeToBusStop)
data1$TimeToBusStop<-as.factor(data1$TimeToBusStop)
str(data1)
table(data1$TimeToBusStop)

# N_SchoolNearBy.Total이 N_SchoolNearBy.~의 합이 같은지 확인
table(data1[,24]+data1[,25]+data1[,26]+data1[,27] == data1[,30]) 
table(data1[,27]) 
# 같음


# N_SchoolNearBy.Total., N_FacilitiesNearBy.Total. 변수 제거
data2<-subset(data1,select=-c(N_SchoolNearBy.Total., N_FacilitiesNearBy.Total.))
str(data2)

# N_APT(아파트당 세대수)로 N_manager, N_elevators, 를 나누어 아파트 세대당 갯수로 새로 변수 만들고 원본변수제거
data2$N_manger.APT<-(data2$N_manager/data2$N_APT)
data2$N_elevators.APT<-(data2$N_elevators/data2$N_APT)
data2$N_Parking.G.APT<-(data2$N_Parkinglot.Ground./data2$N_APT)
data2$N_Parking.B.APT<-(data2$N_Parkinglot.Basement./data2$N_APT)

data2<-subset(data2,select=-c(N_manager, N_elevators, N_APT,N_Parkinglot.Ground.,N_Parkinglot.Basement.))
str(data2)

#  YrSold - YearBuilt 값을 이용해서 YearLasting 변수생성 후 YearBulit 변수 제거
# 아파트의 건축년도, 매매년도 보다 아파트가 지어진지 몇년이 되었는지가 더 중요하다고 판단.
data2$YearLasting <- data2$YrSold - data2$YearBuilt
data2<-subset(data2,select=-c(YearBuilt))
str(data2)

write.csv(data2,"C:/Daegu_Real_Estate_data(수정).csv")


############## 파생변수 생성


## 전처리 후, 모델 구축
## 탐색적 자료분석을 통해 확인한 대구 집값 데이터의 특성에 따라, 일반적인 회귀모형이 아닌, mixed effect가 적용된 회귀모형을 구축하도록 한다. 
###lmer###
data<-read.csv("C:/Daegu_Real_Estate_data(수정).csv")
str(data)
sum(is.na(data))

data1<-data[,c(1,3:7,31)]
str(data1)

#install.packages("lme4")
#install.packages("lmerTest")		# note that the lmerTest package and RLRsim package do not work together.
#install.packages("RLRsim")
#library(lme4)
library(lmerTest)		# note that the lmerTest package and RLRsim package do not work together.
library(RLRsim)

library(ggplot2)
library(gcookbook)
library(plyr)
library(reshape2)

data2<-data1
data2$code<-as.factor(data2$code)

#선형성 있는지 확인하기
#with(subset(data2,code=="1"), plot(YrSold, SalePrice))

#par(mfrow=c(3,8))
#for(i in 1:unique(data2$code)){
#	with(subset(data2,as.numeric(code)==i), plot(YrSold, SalePrice))
#}


with(data2, plot(code, YrSold))
with(data2, plot(code, MonthSold))
with(data2, plot(code, Size.sqf.))
with(data2, plot(code, Floor))
with(data2, plot(code, YearLasting))



#code당 회귀선
sp <- ggplot(subset(data2,code=="1"), aes(x = YrSold, y = SalePrice)) 
sp + geom_point(color = "blue") + stat_smooth(method = lm, se = TRUE, color = "red")


#그룹별 회귀선(SalePrice) : 선형이 보이긴 하지만 깔끔하지 않다.
sps <- ggplot(data2, aes(x = YrSold, y = SalePrice, color = code)) + 
    geom_point() + scale_color_brewer(palette = "Set1")
sps + geom_smooth()


#그룹별 회귀선(MonthSold) : 선형성 없다.
#library(grid) # for unit()
library(RColorBrewer)

cols <- colorRampPalette(brewer.pal(9, "Set3"))
myPal <- cols(length(unique(data2$code)))

sps <- ggplot(data2, aes(x = MonthSold, y = SalePrice, color = code)) + 
    geom_point() + scale_color_brewer(palette = Mypal)
sps + stat_smooth(method = lm, se = TRUE)

sp <- ggplot(subset(data2,code=="2"), aes(x = MonthSold, y = SalePrice))
sp+ geom_point(color = "blue") + stat_smooth(method = lm, se = TRUE, color = "red")


#그룹별 회귀선(Size.sqf.) : 선형이 
sps <- ggplot(data2, aes(x = Size.sqf., y = SalePrice, color = code)) + 
    geom_point() + scale_color_brewer(palette = "Set1")
sps + stat_smooth(method = lm, se = TRUE)



install.packages("gridExtra")
library(gridExtra)

unique(data2$code)
sp1 <- ggplot(subset(data2,code=="1"), aes(x = Size.sqf., y = SalePrice)) + geom_point(color = "blue") + stat_smooth(method = lm, se = TRUE, color = "red")
sp2 <- ggplot(subset(data2,code=="2"), aes(x = Size.sqf., y = SalePrice)) + geom_point(color = "blue") + stat_smooth(method = lm, se = TRUE, color = "red")
sp3 <- ggplot(subset(data2,code=="3"), aes(x = Size.sqf., y = SalePrice)) + geom_point(color = "blue") + stat_smooth(method = lm, se = TRUE, color = "red")
sp4 <- ggplot(subset(data2,code=="4"), aes(x = Size.sqf., y = SalePrice)) + geom_point(color = "blue") + stat_smooth(method = lm, se = TRUE, color = "red")
sp5 <- ggplot(subset(data2,code=="5"), aes(x = Size.sqf., y = SalePrice)) + geom_point(color = "blue") + stat_smooth(method = lm, se = TRUE, color = "red")
sp6 <- ggplot(subset(data2,code=="6"), aes(x = Size.sqf., y = SalePrice)) + geom_point(color = "blue") + stat_smooth(method = lm, se = TRUE, color = "red")
sp7 <- ggplot(subset(data2,code=="7"), aes(x = Size.sqf., y = SalePrice)) + geom_point(color = "blue") + stat_smooth(method = lm, se = TRUE, color = "red")
sp8 <- ggplot(subset(data2,code=="8"), aes(x = Size.sqf., y = SalePrice)) + geom_point(color = "blue") + stat_smooth(method = lm, se = TRUE, color = "red")
sp9 <- ggplot(subset(data2,code=="9"), aes(x = Size.sqf., y = SalePrice)) + geom_point(color = "blue") + stat_smooth(method = lm, se = TRUE, color = "red")
sp10 <- ggplot(subset(data2,code=="10"), aes(x = Size.sqf., y = SalePrice)) + geom_point(color = "blue") + stat_smooth(method = lm, se = TRUE, color = "red")

grid.arrange(sp1,sp2,sp3,sp4,sp5,sp6,sp7,sp8,sp9,sp10, nrow=2, ncol=5)

sp11 <- ggplot(subset(data2,code=="11"), aes(x = Size.sqf., y = SalePrice)) + geom_point(color = "blue") + stat_smooth(method = lm, se = TRUE, color = "red")
sp12 <- ggplot(subset(data2,code=="12"), aes(x = Size.sqf., y = SalePrice)) + geom_point(color = "blue") + stat_smooth(method = lm, se = TRUE, color = "red")
sp13 <- ggplot(subset(data2,code=="13"), aes(x = Size.sqf., y = SalePrice)) + geom_point(color = "blue") + stat_smooth(method = lm, se = TRUE, color = "red")
sp14 <- ggplot(subset(data2,code=="14"), aes(x = Size.sqf., y = SalePrice)) + geom_point(color = "blue") + stat_smooth(method = lm, se = TRUE, color = "red")
sp15 <- ggplot(subset(data2,code=="15"), aes(x = Size.sqf., y = SalePrice)) + geom_point(color = "blue") + stat_smooth(method = lm, se = TRUE, color = "red")
sp16 <- ggplot(subset(data2,code=="16"), aes(x = Size.sqf., y = SalePrice)) + geom_point(color = "blue") + stat_smooth(method = lm, se = TRUE, color = "red")
sp17 <- ggplot(subset(data2,code=="17"), aes(x = Size.sqf., y = SalePrice)) + geom_point(color = "blue") + stat_smooth(method = lm, se = TRUE, color = "red")
sp18 <- ggplot(subset(data2,code=="18"), aes(x = Size.sqf., y = SalePrice)) + geom_point(color = "blue") + stat_smooth(method = lm, se = TRUE, color = "red")
sp19 <- ggplot(subset(data2,code=="19"), aes(x = Size.sqf., y = SalePrice)) + geom_point(color = "blue") + stat_smooth(method = lm, se = TRUE, color = "red")
sp20 <- ggplot(subset(data2,code=="20"), aes(x = Size.sqf., y = SalePrice)) + geom_point(color = "blue") + stat_smooth(method = lm, se = TRUE, color = "red")
sp21 <- ggplot(subset(data2,code=="21"), aes(x = Size.sqf., y = SalePrice)) + geom_point(color = "blue") + stat_smooth(method = lm, se = TRUE, color = "red")

grid.arrange(sp11,sp12,sp13,sp14,sp15,sp16,sp17,sp18,sp19,sp20,sp21, nrow=2, ncol=6)


#그룹별 회귀선(Floor) : 선형이 없어보인다.
sps <- ggplot(data2, aes(x = Floor, y = SalePrice, color = code)) + 
    geom_point() + scale_color_brewer(palette = "Set1")
sps + stat_smooth(method = lm, se = TRUE)


#그룹별 회귀선(YearLasting) : 선형이 없어보인다.
sps <- ggplot(data2, aes(x = YearLasting, y = SalePrice, color = code)) + 
    geom_point() + scale_color_brewer(palette = "Set1")
sps + stat_smooth(method = lm, se = TRUE)



### 가능한 모든 model 구축해보기 ###
fit1 <- lmer(SalePrice~.-YearLasting-code+(1|code),data=data2)
summary(fit1)

par(mfrow=c(2,2))
plot(fit1)


fit2 <- lmer(SalePrice~.-YrSold-code+(1+Size.sqf.|code),data=data2)
summary(fit2)

plot(fit2)


fit2_2 <- lmer(SalePrice~.-YrSold-code+(Size.sqf.|code),data=data2)
summary(fit2_2)

plot(fit2)


fit3 <- lmer(SalePrice~.-YrSold-code+(YearLasting|code),data=data2)
summary(fit3)

plot(fit3)


fit4 <- lmer(SalePrice~.-YrSold-code+(Size.sqf.|code)+(YearLasting|code),data=data2)
summary(fit4)

plot(fit4)




######################################################################################
data3<-data2
data3$SalePrice<-log(data3$SalePrice)

fit1_sq <- lmer(SalePrice~.-YearLasting-code+(1|code),data=data3)
summary(fit1_sq)

par(mfrow=c(1,2))
plot(fit1)
plot(fit1_sq)



fit2_sq <- lmer(SalePrice~.-YearLasting-code+(1+Size.sqf.|code),data=data3)
summary(fit2_sq)

plot(fit2_sq)


fit2_2_sq <- lmer(SalePrice~.-YrSold-code+(1+Size.sqf.|code),data=data3)
summary(fit2_2_sq)

plot(fit2_2_sq)


fit3_sq <- lmer(SalePrice~.-YrSold-code+(YearLasting|code),data=data3)
summary(fit3_sq)

plot(fit3_sq)


fit4_sq <- lmer(SalePrice~.-YrSold-code+(Size.sqf.|code)+(YearLasting|code),data=data3)
summary(fit4_sq)

plot(fit4_sq)



#최종 후보모델
###########train/test###########
n<-sample(nrow(data2),nrow(data2)*0.7)
head(n)

train<-data2[n,]
test<-data2[-n,]


fit_1 <- lmer(SalePrice~.-YrSold-code+(1|code),data=train)
summary(fit_1)


y<-predict(fit_1)
head(y)
head(train$SalePrice)

par(mfrow=c(2,2))
plot(fit_1)


#rmse
rmse<-sqrt(sum((y-train$SalePrice)^2)/4007)
#=>rmse=40292.17


#r_squared
sst<-sum((train$SalePrice-mean(train$SalePrice))^2)
ssr<-sst-sum((y-train$SalePrice)^2)
r_sq<-ssr/sst
#=>r_squared=0.8547


#plot
plot(data_test3$SalePrice, y3)


fit_3 <- lmer(SalePrice~.-YrSold-code+(YearLasting|code),data=train)
summary(fit_3)

plot(fit_3)



###month를 분기별로 바꾼다.###
mon_sq<-c()

for(i in 1:nrow(data3)){
    if(data3$MonthSold[i] %in% c(1,2,3)){
        mon_sq[i] <- "Q1"
    }else if(data3$MonthSold[i] %in% c(4,5,6)){
        mon_sq[i] <- "Q2"
    }else if(data3$MonthSold[i] %in% c(7,8,9)){
        mon_sq[i] <- "Q3"
    }else{
        mon_sq[i] <- "Q4"
    }
}

head(mon_sq)
table(mon_sq)

data4<-data3
data4<-cbind(data4, mon_sq)
data4<-data4[,-3]


fit_new<- lmer(SalePrice~.-YrSold-code+(1+Size.sqf.|code),data=data4)
summary(fit_new)

plot(fit_new)


mon_sq <- c()

for(i in 1:nrow(data2)){
    if(data2$MonthSold[i] %in% c(1,2,3)){
        mon_sq[i] <- "Q1"
    }else if(data2$MonthSold[i] %in% c(4,5,6)){
        mon_sq[i] <- "Q2"
    }else if(data2$MonthSold[i] %in% c(7,8,9)){
        mon_sq[i] <- "Q3"
    }else{
        mon_sq[i] <- "Q4"
    }
}

head(mon_sq)
table(mon_sq)

data5<-data2
data5<-cbind(data5, mon_sq)
data5<-data5[,-3]
colnames(data5)


fit_new2<- lmer(SalePrice~.-YrSold-code+(1+Size.sqf.|code),data=data5)
summary(fit_new2)

plot(fit_new2)

anova(fit2_2_sq,fit_new2)

BIC(fit2_2_sq)
BIC(fit_new2)



y<-predict(fit_new2)
head(y)
head(data5$SalePrice)

par(mfrow=c(2,2))
plot(data5$SalePrice,y)


#가정확인
library(lattice)
plot(fit_new2, type=c("p","smooth"))
plot(fit_new2, sqrt(abs(resid(.)))~fitted(.), type=c("p","smooth"))
qqmath(fit_new2, id=0.05)


#rmse
rmse<-sqrt(sum((y-data5$SalePrice)^2)/5725)
#=>rmse=29855.56


#r_squared
sst<-sum((data5$SalePrice-mean(data5$SalePrice))^2)
ssr<-sst-sum((y-data5$SalePrice)^2)
r_sq<-ssr/sst
#=>r_squared=0.9187022


#plot
plot(data5$SalePrice, y)
