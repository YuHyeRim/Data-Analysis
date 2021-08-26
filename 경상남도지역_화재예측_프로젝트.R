### 경상남도 지역 화재발생 기록을 바탕으로 화재 요인분석을 진행하도록 한다.

## 데이터 불러오기
fire <- read.csv("C:/fire_0203.csv",header=T)


#결측치 갯수 먼저 확인 #
colSums(is.na(fire))

## 결측치가 있다는것을 확인했는데, 위 코드로는 결측치가 나오지 않음 ##

nrow(subset(fire,bldng_us == ""))
nrow(subset(fire,bldng_archtctr == ""))
## 건물용도나 건물구조는 NA가아니라 문자가 없는것 ##

## 탐색적 자료분석
# 그래프 그리기

yesfire <- subset(fire, fr_yn == "Y")
nofire <- subset(fire, fr_yn == "N")

#head(yesfire)#

#colSums(is.na(fire))#

nrow(subset(fire,fr_yn == 'Y'))
nrow(subset(fire,fr_yn == 'N'))

library(ggplot2)
library(corrplot)

#### 1. jmk ####
## 그래프로 파악해야 하는것이 무엇인가? : jmk의 항목별로 화재 발생여부가 어떻게 되는지! ##

ggplot(data=fire, aes(fr_yn, fill=jmk))+geom_bar(position='fill') 

ggplot(data=fire, aes(x =fr_yn,y=nrow(fire), fill=jmk))+geom_bar(stat='identity') 

ggplot(data=yesfire, aes(fr_yn, fill=jmk)) + geom_bar(position ='fill')+
    geom_text(aes(label=nrow(fire),position=position_stack(0.5)))
#이건 행 개수를 Y로 놓고 그린거! #

#jmk의 요소들이 너무 많아서 색구분이 잘 안되긴함! 하지만 딱봐도 '대'항목이 많은걸 볼수 있음#
# 참고로 '대' : 주거,사무실 용도의 토지 #



#### 2. rgnl_ar_nm ####

ggplot(data=fire, aes(fr_yn, fill=rgnl_ar_nm))+geom_bar(position='fill') 

# Y/N 공통적으로 계획관리지역과 제2종일반주거지역이 많은걸 알 수 있음 #
# 계획관리지역? : 도시지역으로의 편입이 예상되는 지역 또는 자연환경을 고려하여 제한적인 이용/ 개발을 하려는 지역 #

#### 3. rgnl_ar_nm2 ####

ggplot(data=fire, aes(fr_yn, fill=rgnl_ar_nm2))+geom_bar(position='fill') 


# 지정되지 않음이 압도적으로 많음 #

#### 4. lnd_us_sttn_nm ####

ggplot(data=fire, aes(fr_yn, fill=lnd_us_sttn_nm))+geom_bar(position='fill')



#### 5. rd_sd_nm ####

ggplot(data=fire, aes(fr_yn, fill=rd_sd_nm))+geom_bar(position='fill')

# 다른 변수들과는 달리 한개의 항목이 압도적인 형태가 아님 #
# 세로한면(가)/ 세로한면(불)/ 소로한면 #
# 문제는 이변수에 대해 자세히 알기가 힘들다는것 #
# 우리가 모른다고 없앨수는 없기때문에 변수에 대한 지식이 필요해 보임 #


#### 6. emd_nm ####

par(mfrow=c(3,3))
ggplot(data=fire, aes(fr_yn, fill=emd_nm))+geom_bar(position='fill')
# 항목수가 많아서 barplot같은 간단한꼴로 표현하는게 한계가 있음! #



#### 7. hm_cnt ####

summary(fire$hm_cnt)
# 행정구역별 인구 변수는 행정구역의 인구가 몇명인지를 보여주는데, 문제는 같은 행정구역이라도 
dt_of_fr 에 따라 행정구역명의 인구가 다름 행정구역별 인구는 혼자 가지고는 안될변수인것 같음!! #
    
    
    
    #### 8. fr_sttn_dstnc ####

ggplot(data=fire, aes(x=fr_yn, y = fr_sttn_dstnc)) + geom_boxplot()

# 그래프만 봤을때 우선, 화재가 일어나지 않은 곳이 119센터와의 평균거리가 멀기는 함 
근데 그렇게 드라마틱한 차이를 보이지는 않음 #

#### 9. bldng_ar_prc #### 
# 결측치 21895개 #

ggplot(data=fire, aes(x=fr_yn, y = bldng_ar_prc)) + geom_boxplot()
# 단위 면적당 건물가격은 이 변수만 살피기 보다는 토지의 용도관련 변수랑 같이 살펴야 할것 같음 !!! #



#### 10. fr_wthr_fclt_dstnc ####

ggplot(data=fire, aes(x=fr_yn, y = fr_wthr_fclt_dstnc)) + geom_boxplot()

# 예상 : 화재가 일어난곳이 소화전으로부터의 거리가 조금이라도 더 멀지 않을까? #
# 실제 : 오히려 평균거리가 더 가까움 #
# 어떻게 받아들여야 하나 ? : 소화전이 가깝다 = 상가 또는 주거용 건물? #

ggplot(yesfire, aes(x=rgnl_ar_nm, y=fr_wthr_fclt_dstnc)) + geom_boxplot() +
    theme(axis.text.x=element_text(angle=70, hjust=1))

# 화재가 발생한 경우 용도에 따른 소화전과의 거리 => 별다른 포인트 없어보임 #

ggplot(yesfire, aes(x=jmk, y=fr_wthr_fclt_dstnc)) + geom_boxplot() +
    theme(axis.text.x=element_text(angle=70, hjust=1))


#### 11. fr_mn_cnt #### 

ggplot( data= fire, aes(x=fr_yn, y=fr_mn_cnt)) +  geom_boxplot()
# 수치로 확인해보도록 하자 ##

median(yesfire$fr_mn_cnt,na.rm=TRUE)
median(nofire$fr_mn_cnt, na.rm=TRUE)

# 소수점이 다르다  => 거의 똑같다고볼 수 있을듯 #

#### 12. mlt_us_yn ####

ggplot(data=fire, aes(fr_yn, fill=mlt_us_yn))+geom_bar(position='fill')

# 화재가 발생여부에 상관없이 다중이용시설 포함 안함 #
summary(fire$mlt_us_yn)
# summary통해 확인하니 N이 압도적으로 많음 => 이건 딱히 화재 발생여부에 영향을 미치는 변수가 아닌것같아 삭제하는게 나을것 같다 #

#### 13. cctv_dstnc ####

ggplot( data= fire, aes(x=fr_yn, y=cctv_dstnc)) +  geom_boxplot()
# 화재가 발생하던 안하던 cctv와의 거리는 거기서 거기인것 같다 #
# 실제 수치로확인을 해보도록 하자 #

median(yesfire$cctv_dstnc, na.rm=TRUE) #216#
median(nofire$cctv_dstnc, na.rm=TRUE) #301#

#### 14. tbc_rtl_str_dstnc ####

ggplot( data= fire, aes(x=fr_yn, y=tbc_rtl_str_dstnc)) +  geom_boxplot()

# 불이 나지 않은경우가 담배 판매하는곳과 조금더 멀다 #

#### 15. sft_emrgnc_bll_dstnc ####

ggplot( data= fire, aes(x=fr_yn, y=sft_emrgnc_bll_dstnc)) +  geom_boxplot()

# 불이 난경우가 안전비상벨과 조금더 멀다 #


#### 16. ahsm_dstnc ####

ggplot( data= fire, aes(x=fr_yn, y=ahsm_dstnc)) +  geom_boxplot()

# 불이 난 경우가 자동심장충격기와 조금 더 멀다 #

#### 17. no_tbc_zn_dstnc ####

ggplot( data= fire, aes(x=fr_yn, y=no_tbc_zn_dstnc)) +  geom_boxplot()

# 큰 차이가 관찰되지 않는다. #

################### 대체적으로 최소거리와 같은 변수는 화재 발생에 따라 거리가 어떻게 되는지도 중요하지만
일단 그 토지의 용도변수와 함께 엮어서 건물의 특성을 파악하는게 좋을것 같음 #################################


############ 최소거리 변수를 통해 건물의 특성을 파악해보자 ##############
# yesfire와 nofire를 쓰도록 할 것 같음!! #

# 1. jmk #


# 1.1 지적상지목과 119안전센터와의 거리 #
ggplot( data= fire, aes(x=jmk, y=fr_sttn_dstnc)) +  geom_boxplot() +
    geom_abline(intercept= median(fire$fr_sttn_dstnc,na.rm=TRUE), slope=-0, color='blue', size = 1) 

# 119센터와 거리가 멀다고 할수 있음(일단 평균기준) : 목(목장) 양(양식장) 원(유원지, 휴양지) 임(산림 및 원야)  #

# 하 이게 근데 의미가 있는건지 뭔지 어떻게 연관지어서 해석해야할지 잘 정리가 안되는것 같아 ㅠㅠ #


# 1.2 지적상지목과 소화전과의 최소거리 #


ggplot( data= fire, aes(x=jmk, y=fr_wthr_fclt_dstnc)) +  geom_boxplot() +
    geom_abline(intercept= median(fire$fr_wthr_fclt_dstnc,na.rm=TRUE), slope=-0, color='blue', size = 1) 
###### Q1,Q3를 표시해서 전체데이터의 Q1,Q2,Q3와 비교해서 멀고 가까움을 표현 #



# 1.3 지적장 지목과 관할소방서인원 #


ggplot( data= fire, aes(x=jmk, y=fr_mn_cnt)) +  geom_boxplot() +
    geom_abline(intercept= median(fire$fr_mn_cnt,na.rm=TRUE), slope=-0, color='blue', size = 1) 

# 공(공원) 과(과수원) 대(주거, 사무실) 양(양어장) 유 장(공장) 제(방파제 ,,) 주(주유소) 차(주차장) 철(철도용지) 체(하천) 학(학교) #
# 유동인구가 많은 토지일수록 관할소방서인원이 많은편이라고 할 수 있을것 같음 #

# 1.4 지적상 지목과 공공cctv와의 최소거리 #

ggplot( data= fire, aes(x=jmk, y=cctv_dstnc)) +  geom_boxplot() +
    geom_abline(intercept= mean(fire$cctv_dstnc,na.rm=TRUE), slope=-0, color='blue', size = 1) 

# 일단 대에서 아웃라이어가 굉장히 많음!! #
# cctv와 가까운편 : 대 도 유 잡 장 전 제 주(주유소용지) 철 체(체육용지) 학 # 
# 잡 : 갈대밭 변전소 송유시설, 도축장, 쓰레기처리장,,,,,, 등 #
# 전 : 물을 상시로 이용하는 것은 아니지만 식물을 주로 재배하는곳 #

# 유동인구가 많은 토지일수록 cctv와의 거리가 가까운 편이다 #

# 1.5 반경 100m이내의 공공cctv #
# 얘는 이 변수 자체의 그래프를 깜빡하고 건너뜀 #
summary(fire$cctv_in_100m)

ggplot(data=fire, aes(x=fr_yn, y=cctv_in_100m )) + geom_boxplot()  

ggplot( data= fire, aes(x=jmk, y=cctv_in_100m)) +  geom_boxplot() +
    geom_abline(intercept= median(fire$cctv_in_100m,na.rm=TRUE), slope=-0, color='blue', size = 1) 



################################################################
# 생각정리 #

# 그래프를 그려서 보려는게 뭘까? #
# 1. 유동인구가 많은 토지일수록 화재가 발생하는 경향이 있을까? #
# 화재가 발생하는 토지나 건물의 특징을 발견할 수 있을까? #
# 우선 cctv갯수나 뭐 소방대원수, 최소거리등을 이용해 토지나 건물별로 특징이 어떻게되는지를 보고싶다 #


# 용도지역지구명 & 지적상 지목 #
# 근데 이게 둘이 어떻게 다른건지를 좀 지식적으로 알아야 할것 같음! #

ggplot(data=fire, aes(x=jmk, fill= rgnl_ar_nm))+ geom_bar(position='fill')

ggplot(data=fire, aes(x=rgnl_ar_nm, fill=jmk)) + geom_bar(position = 'fill')+
    theme(axis.text.x=element_text(angle=70, hjust=1))


ggplot(data=yesfire, aes(x=rgnl_ar_nm, fill=jmk)) + geom_bar(position = 'fill')+
    theme(axis.text.x=element_text(angle=70, hjust=1))


ggplot(data=yesfire, aes(x=jmk, fill=rgnl_ar_nm)) + geom_bar(position = 'fill')+
    theme(axis.text.x=element_text(angle=70, hjust=1))

# 2. rgnl_ar_nm 용도지역지구명 #

# 2.1 cctv_in_100m #

ggplot(data=fire, aes(x=cctv_in_100m, fill=rgnl_ar_nm)) + geom_bar(width=.6,position='fill')

# 2종일반주거지역, 2종전용주거지역, 3종일반주거지역 ,,, #

# 2.2 fr_sttn_dstnc #

symbols(fire$fr_sttn_dstnc, fire$fr_wthr_fclt_dstnc , circles=fire$fr_mn_cnt)

# 행수가 너무 많아서 버블차트 그리기에 적합하지 않은것 같다 #

# ggplot(data=fire, aes(x=fr_sttn_dstnc,y=fr_wthr_fclt_dstnc,colour = rgnl_ar_nm)) + geom_point()# 
# 돌리지 않는게 좋을것 같음 #
# 용도에 따라 119안전센터와의 거리와 소화전까지의 거리 산점도를 찍어보려 했는데 겹쳐찍혀서 파악하기 힘듦 #


ggplot(data=fire, aes(x=fr_wthr_fclt_in_100m,y=fr_sttn_dstnc,fill = rgnl_ar_nm)) + geom_bar(stat='identity')

ggplot(data=fire, aes(x=fr_yn,y=fr_wthr_fclt_in_100m)) + geom_boxplot()

## 논리적 오류 확인 ##

# 지상0층, 지하0층, 건물면적이 0인애들중에서 단독주택인게 6개 #
# 논리적으로 옳지 않다. => 추후 삭제 예정

nrow(subset(noflr, bldng_ar == 0 & ttl_ar == 0 & lnd_ar ==0 ))

nrow(subset(noflr, lnd_ar ==0 ))
summary(subset(noflr, lnd_ar ==0 ))


## 결측치 대체

# 기상청 데이터와 비교해 풍향결측치 

library(dplyr)
library(stringr)

#### 거제 ####
# nrow(subset(fire,substr(fire$emd_nm,6,7) == "거제" & is.na(fire$wnd_drctn)==TRUE))
# 1개
fire[substr(fire$emd_nm,6,7) == "거제" & is.na(fire$wnd_drctn)==TRUE,
     c("dt_of_fr","emd_nm","wnd_drctn","id")]

# filter(fire, substr(fire$emd_nm,6,7) == "거제",is.na(fire$wnd_drctn)==TRUE)

fire$wnd_drctn[substr(fire$emd_nm,6,7) == "거제" 
               & is.na(fire$wnd_drctn)==TRUE] <- 45



#### 거창 ####

# nrow(subset(fire,substr(fire$emd_nm,6,7) == "거창" & is.na(fire$wnd_drctn)==TRUE))
# 3개

fire[substr(fire$emd_nm,6,7) == "거창" & is.na(fire$wnd_drctn)==TRUE,
     c("dt_of_fr","emd_nm","wnd_drctn","id")]


fire$wnd_drctn[substr(fire$emd_nm,6,7) == "거창" 
               & is.na(fire$wnd_drctn)==TRUE & fire$id == 72356 ] <- 108

fire$wnd_drctn[substr(fire$emd_nm,6,7) == "거창" 
               & is.na(fire$wnd_drctn)==TRUE & fire$id == 73243] <- 184

fire$wnd_drctn[substr(fire$emd_nm,6,7) == "거창" 
               & is.na(fire$wnd_drctn)==TRUE & fire$id == 128511] <- 86


### 고성 ###

# 4개
fire[substr(fire$emd_nm,6,7) == "고성" & is.na(fire$wnd_drctn)==TRUE,
     c("dt_of_fr","emd_nm","wnd_drctn","id")]


fire$wnd_drctn[substr(fire$emd_nm,6,7) == "고성" 
               & is.na(fire$wnd_drctn)==TRUE & fire$id == 76049] <- 48

fire$wnd_drctn[substr(fire$emd_nm,6,7) == "고성" 
               & is.na(fire$wnd_drctn)==TRUE & fire$id == 72001] <- 293

fire$wnd_drctn[substr(fire$emd_nm,6,7) == "고성" 
               & is.na(fire$wnd_drctn)==TRUE & fire$id == 97792] <- 327

fire$wnd_drctn[substr(fire$emd_nm,6,7) == "고성" 
               & is.na(fire$wnd_drctn)==TRUE & fire$id == 120070] <- 14 


### 남해 ###
# 결측치 없음
fire[substr(fire$emd_nm,6,7) == "남해" & is.na(fire$wnd_drctn)==TRUE,
     c("dt_of_fr","emd_nm","wnd_drctn","id")]


### 밀양 ###

# 4개
fire[substr(fire$emd_nm,6,7) == "밀양" & is.na(fire$wnd_drctn)==TRUE,
     c("dt_of_fr","emd_nm","wnd_drctn","id")]

fire$wnd_drctn[substr(fire$emd_nm,6,7) == "밀양" 
               & is.na(fire$wnd_drctn)==TRUE & fire$id == 89617] <- 326

fire$wnd_drctn[substr(fire$emd_nm,6,7) == "밀양" 
               & is.na(fire$wnd_drctn)==TRUE & fire$id == 114266] <- 324

fire$wnd_drctn[substr(fire$emd_nm,6,7) == "밀양" 
               & is.na(fire$wnd_drctn)==TRUE & fire$id == 70409] <- 313

fire$wnd_drctn[substr(fire$emd_nm,6,7) == "밀양" 
               & is.na(fire$wnd_drctn)==TRUE & fire$id == 117670] <- 42



### 사천 ###

#2개
fire[substr(fire$emd_nm,6,7) == "사천" & is.na(fire$wnd_drctn)==TRUE,
     c("dt_of_fr","emd_nm","wnd_drctn","id")]


fire$wnd_drctn[substr(fire$emd_nm,6,7) == "사천" 
               & is.na(fire$wnd_drctn)==TRUE & fire$id == 116173] <- 117

fire$wnd_drctn[substr(fire$emd_nm,6,7) == "사천" 
               & is.na(fire$wnd_drctn)==TRUE & fire$id == 74427] <- 210



### 산청 ###

#2개
fire[substr(fire$emd_nm,6,7) == "산청" & is.na(fire$wnd_drctn)==TRUE,
     c("dt_of_fr","emd_nm","wnd_drctn","id")]

fire$wnd_drctn[substr(fire$emd_nm,6,7) == "산청" 
               & is.na(fire$wnd_drctn)==TRUE & fire$id == 129589] <- 357 


fire$wnd_drctn[substr(fire$emd_nm,6,7) == "산청" 
               & is.na(fire$wnd_drctn)==TRUE & fire$id == 77192] <- 182


### 양산 ###

#2개 
fire[substr(fire$emd_nm,6,7) == "양산" & is.na(fire$wnd_drctn)==TRUE,
     c("dt_of_fr","emd_nm","wnd_drctn","id")]


fire$wnd_drctn[substr(fire$emd_nm,6,7) == "양산" 
               & is.na(fire$wnd_drctn)==TRUE & fire$id == 100866 ] <- 90

fire$wnd_drctn[substr(fire$emd_nm,6,7) == "양산" 
               & is.na(fire$wnd_drctn)==TRUE & fire$id == 115309 ] <- 220


### 의령 ###

#결측치 없음
fire[substr(fire$emd_nm,6,7) == "의령" & is.na(fire$wnd_drctn)==TRUE,
     c("dt_of_fr","emd_nm","wnd_drctn","id")]


### 진주 ###

#1개
fire[substr(fire$emd_nm,6,7) == "진주" & is.na(fire$wnd_drctn)==TRUE,
     c("dt_of_fr","emd_nm","wnd_drctn","id")]

fire$wnd_drctn[substr(fire$emd_nm,6,7) == "진주" 
               & is.na(fire$wnd_drctn)==TRUE & fire$id == 109494] <- 304

### 창녕 ###
#3개

fire[substr(fire$emd_nm,6,7) == "창녕" & is.na(fire$wnd_drctn)==TRUE,
     c("dt_of_fr","emd_nm","wnd_drctn","id")]

fire$wnd_drctn[substr(fire$emd_nm,6,7) == "창녕" 
               & is.na(fire$wnd_drctn)==TRUE & fire$id == 110088] <- 355


fire$wnd_drctn[substr(fire$emd_nm,6,7) == "창녕" 
               & is.na(fire$wnd_drctn)==TRUE & fire$id == 94476] <- 114


fire$wnd_drctn[substr(fire$emd_nm,6,7) == "창녕" 
               & is.na(fire$wnd_drctn)==TRUE & fire$id == 96379] <- 190


### 창원 ###

#8개
fire[substr(fire$emd_nm,6,7) == "창원" & is.na(fire$wnd_drctn)==TRUE,
     c("dt_of_fr","emd_nm","wnd_drctn","id")]


fire$wnd_drctn[substr(fire$emd_nm,6,7) == "창원" 
               & is.na(fire$wnd_drctn)==TRUE & fire$id == 77996] <- 270

fire$wnd_drctn[substr(fire$emd_nm,6,7) == "창원" 
               & is.na(fire$wnd_drctn)==TRUE & fire$id == 108031] <- 22 

fire$wnd_drctn[substr(fire$emd_nm,6,7) == "창원" 
               & is.na(fire$wnd_drctn)==TRUE & fire$id == 91630] <- 344

fire$wnd_drctn[substr(fire$emd_nm,6,7) == "창원" 
               & is.na(fire$wnd_drctn)==TRUE & fire$id == 133999] <- 69

fire$wnd_drctn[substr(fire$emd_nm,6,7) == "창원" 
               & is.na(fire$wnd_drctn)==TRUE & fire$id == 119757] <- 84

fire$wnd_drctn[substr(fire$emd_nm,6,7) == "창원" 
               & is.na(fire$wnd_drctn)==TRUE & fire$id == 133029] <- 294

fire$wnd_drctn[substr(fire$emd_nm,6,7) == "창원" 
               & is.na(fire$wnd_drctn)==TRUE & fire$id == 85448] <- 18

fire$wnd_drctn[substr(fire$emd_nm,6,7) == "창원" 
               & is.na(fire$wnd_drctn)==TRUE & fire$id == 134898] <- 47

### 통영 ###

#12개
fire[substr(fire$emd_nm,6,7) == "통영" & is.na(fire$wnd_drctn)==TRUE,
     c("dt_of_fr","emd_nm","wnd_drctn","id")]


fire$wnd_drctn[substr(fire$emd_nm,6,7) == "통영" 
               & is.na(fire$wnd_drctn)==TRUE & fire$id == 89074] <- 341
fire$wnd_drctn[substr(fire$emd_nm,6,7) == "통영" 
               & is.na(fire$wnd_drctn)==TRUE & fire$id == 94381] <- 338
fire$wnd_drctn[substr(fire$emd_nm,6,7) == "통영" 
               & is.na(fire$wnd_drctn)==TRUE & fire$id == 134601] <- 360
fire$wnd_drctn[substr(fire$emd_nm,6,7) == "통영" 
               & is.na(fire$wnd_drctn)==TRUE & fire$id == 80839] <- 77
fire$wnd_drctn[substr(fire$emd_nm,6,7) == "통영" 
               & is.na(fire$wnd_drctn)==TRUE & fire$id == 122039] <- 342
fire$wnd_drctn[substr(fire$emd_nm,6,7) == "통영" 
               & is.na(fire$wnd_drctn)==TRUE & fire$id == 116982] <- 254
fire$wnd_drctn[substr(fire$emd_nm,6,7) == "통영" 
               & is.na(fire$wnd_drctn)==TRUE & fire$id == 69188] <- 69
fire$wnd_drctn[substr(fire$emd_nm,6,7) == "통영" 
               & is.na(fire$wnd_drctn)==TRUE & fire$id == 72559] <- 344
fire$wnd_drctn[substr(fire$emd_nm,6,7) == "통영" 
               & is.na(fire$wnd_drctn)==TRUE & fire$id == 82703] <- 334
fire$wnd_drctn[substr(fire$emd_nm,6,7) == "통영" 
               & is.na(fire$wnd_drctn)==TRUE & fire$id == 95790] <- 324
fire$wnd_drctn[substr(fire$emd_nm,6,7) == "통영" 
               & is.na(fire$wnd_drctn)==TRUE & fire$id == 135516] <- 360
fire$wnd_drctn[substr(fire$emd_nm,6,7) == "통영" 
               & is.na(fire$wnd_drctn)==TRUE & fire$id == 89092] <-  354

### 하동 ###

#4개
fire[substr(fire$emd_nm,6,7) == "하동" & is.na(fire$wnd_drctn)==TRUE,
     c("dt_of_fr","emd_nm","wnd_drctn","id")]

fire$wnd_drctn[substr(fire$emd_nm,6,7) == "하동" 
               & is.na(fire$wnd_drctn)==TRUE & fire$id == 137497 ] <- 288 


fire$wnd_drctn[substr(fire$emd_nm,6,7) == "하동" 
               & is.na(fire$wnd_drctn)==TRUE & fire$id == 104751] <- 309 

fire$wnd_drctn[substr(fire$emd_nm,6,7) == "하동" 
               & is.na(fire$wnd_drctn)==TRUE & fire$id == 124782] <- 135

fire$wnd_drctn[substr(fire$emd_nm,6,7) == "하동" 
               & is.na(fire$wnd_drctn)==TRUE & fire$id == 90364] <- 297


### 함안 ###
#결측치 없음
fire[substr(fire$emd_nm,6,7) == "함안" & is.na(fire$wnd_drctn)==TRUE,
     c("dt_of_fr","emd_nm","wnd_drctn","id")]


### 함양 ###
#결측치 없음
fire[substr(fire$emd_nm,6,7) == "함양" & is.na(fire$wnd_drctn)==TRUE,
     c("dt_of_fr","emd_nm","wnd_drctn","id")]

### 합천 ###
#결측치 없음
fire[substr(fire$emd_nm,6,7) == "합천" & is.na(fire$wnd_drctn)==TRUE,
     c("dt_of_fr","emd_nm","wnd_drctn","id")]


nrow(subset(fire,is.na(fire$wnd_drctn)==TRUE))

fire[is.na(fire$wnd_drctn)==TRUE, c("dt_of_fr","emd_nm","wnd_drctn","id")]

# tmprtr 온도 NA:13
summary(fire$tmprtr);sum(is.na(fire$tmprtr))

fire<-fread("C:/Users/choijumsun/Desktop/fire.csv", encoding="UTF-8")


fire[fire$id==94476,'tmprtr']=15.3	#1
fire[fire$id==105298,'tmprtr']=18.5	#2
fire[fire$id==109596,'tmprtr']=18.5	#3
fire[fire$id==124634,'tmprtr']=22	#4
fire[fire$id==100697,'tmprtr']=5.1	#5
fire[fire$id==76049,'tmprtr']=15.3	#6
fire[fire$id==83235,'tmprtr']=23.3	#7
fire[fire$id==112109,'tmprtr']=0.9	#8
fire[fire$id==132499,'tmprtr']=23.6	#9
fire[fire$id==99264,'tmprtr']=23.3	#10
fire[fire$id==129045,'tmprtr']=4.5	#11
fire[fire$id==121001,'tmprtr']=22.3	#12
fire[fire$id==77192,'tmprtr']=22	#13




# wnd_spd 풍속 NA:33
summary(fire$wnd_spd)

fire[fire$id==124707,'wnd_spd']=0.7 #1
fire[fire$id==124782,'wnd_spd']=5	#2
fire[fire$id==76049,'wnd_spd']=3.6	#3
fire[fire$id==85247,'wnd_spd']=9.4	#4
fire[fire$id==97792,'wnd_spd']=9.4	#5
fire[fire$id==73090,'wnd_spd']=4.3	#6
fire[fire$id==135206,'wnd_spd']=6.1	#7
fire[fire$id==132153,'wnd_spd']=4.3	#8
fire[fire$id==83235,'wnd_spd']=4.3		#9
fire[fire$id==99264,'wnd_spd']=4.3		#10
fire[fire$id==121001,'wnd_spd']=4.3	#11
fire[fire$id==77192,'wnd_spd']=9		#12
fire[fire$id==112109,'wnd_spd']=3.2	#13
fire[fire$id==132499,'wnd_spd']=8.3	#14
fire[fire$id==94476,'wnd_spd']=8.3		#15
fire[fire$id==100697,'wnd_spd']=5.4	#16
fire[fire$id==109596,'wnd_spd']=5.4	#17
fire[fire$id==105298,'wnd_spd']=5.4	#18
fire[fire$id==129819,'wnd_spd']=2.9	#19
fire[fire$id==87991,'wnd_spd']=2.9	#20
fire[fire$id==69188,'wnd_spd']=2.9	#21
fire[fire$id==121214 ,'wnd_spd']=3.6	#22
fire[fire$id==103177,'wnd_spd']=8.6	#23
fire[fire$id==97431,'wnd_spd']=8.6		#24
fire[fire$id==97643,'wnd_spd']=8.6		#25
fire[fire$id==126777,'wnd_spd']=8.6	#26
fire[fire$id==91730,'wnd_spd']=3.6		#27
fire[fire$id==115997,'wnd_spd']=6.5	#28
fire[fire$id==122906,'wnd_spd']=7.2		#29 NA
fire[fire$id==81712,'wnd_spd']=6.1		#30
fire[fire$id==106347,'wnd_spd']=6.1	#31
fire[fire$id==113823,'wnd_spd']=6.8	#32
fire[fire$id==118249,'wnd_spd']=4.7	#33


# hmdt 습도
summary(fire$hmdt)
str(fire$hmdt)

fire[fire$id==121001 ,'hmdt']=11	#1
fire[fire$id==91958 ,'hmdt']=48	#2
fire[fire$id==83235 ,'hmdt']=11	#3
fire[fire$id==99264 ,'hmdt']=11	#4
fire[fire$id==76049 ,'hmdt']=76	#5
fire[fire$id==124634 ,'hmdt']=68	#6
fire[fire$id==71958 ,'hmdt']=82	#7
fire[fire$id==130200 ,'hmdt']=84	#8
fire[fire$id==110233 ,'hmdt']=84	#9
fire[fire$id==77192 ,'hmdt']=87	#10 NA
fire[fire$id==100697 ,'hmdt']=48 	#11
fire[fire$id==87781 ,'hmdt']=54	#12 NA
fire[fire$id==105298 ,'hmdt']=81	#13 NA
fire[fire$id==98355 ,'hmdt']=50	#14 NA
fire[fire$id==109596 ,'hmdt']=70	#15 NA
fire[fire$id==112109 ,'hmdt']=66	#16 NA
fire[fire$id==132499 ,'hmdt']=77	#17
fire[fire$id==94476 ,'hmdt']=77	#18
fire[fire$id==84774 ,'hmdt']=35	#19
fire[fire$id==89228 ,'hmdt']=73	#20
fire[fire$id==104700 ,'hmdt']=62	#21
fire[fire$id==123632 ,'hmdt']=68	#22

#dt_of_athrztn - 건물승인일자

fire[fire$dt_of_athrztn==971215 ,'dt_of_athrztn']=199712125	#18?19?
fire[fire$dt_of_athrztn==9780421,'dt_of_athrztn']=19789421
fire[fire$dt_of_athrztn==9800808,'dt_of_athrztn']=19800808
fire[fire$dt_of_athrztn==9900712,'dt_of_athrztn']=19900712
fire[fire$dt_of_athrztn==9820930,'dt_of_athrztn']=19820930
fire[fire$dt_of_athrztn==9990624,'dt_of_athrztn']=19990624


# 논리적 오류가 발견된 행 파악 및 삭제 #

# 지상,지하층수 + 면적3개 다 0인경우
nrow(subset(fire,ttl_grnd_flr == 0 & ttl_dwn_flr ==0 & bldng_ar == 0 & ttl_ar == 0 & lnd_ar == 0))
#6개


# 건물승인일자 결측
nrow(subset(fire,is.na(dt_of_athrztn)==TRUE))
# 1개 : 삭제후 파생변수?


fire$prcpttn[is.na(fire2$prcpttn) == TRUE] <- 0


## 전기 / 가스 사용량 재범주화 : 5개년 사용량을 1개의 변수로 줄임
library(dplyr)
#install.packages("fastmatch")
library(fastmatch)

### 전기, 가스 사용량 1개의 파생변수로 줄이기 ###

# 전기 #
ele_all<-fire%>% select(ele_engry_us_201401,ele_engry_us_201402,ele_engry_us_201403,
                        ele_engry_us_201404,ele_engry_us_201405,ele_engry_us_201406,
                        ele_engry_us_201407,ele_engry_us_201408,ele_engry_us_201409,
                        ele_engry_us_201410,ele_engry_us_201411,ele_engry_us_201412,
                        ele_engry_us_201501,ele_engry_us_201502,ele_engry_us_201503,
                        ele_engry_us_201504,ele_engry_us_201505,ele_engry_us_201506,
                        ele_engry_us_201507,ele_engry_us_201508,ele_engry_us_201509,
                        ele_engry_us_201510,ele_engry_us_201511,ele_engry_us_201512,
                        ele_engry_us_201601,ele_engry_us_201602,ele_engry_us_201603,
                        ele_engry_us_201604,ele_engry_us_201605,ele_engry_us_201606,
                        ele_engry_us_201607,ele_engry_us_201608,ele_engry_us_201609,
                        ele_engry_us_201610,ele_engry_us_201611,ele_engry_us_201612,
                        ele_engry_us_201701,ele_engry_us_201702,ele_engry_us_201703,
                        ele_engry_us_201704,ele_engry_us_201705,ele_engry_us_201706,
                        ele_engry_us_201707,ele_engry_us_201708,ele_engry_us_201709,
                        ele_engry_us_201710,ele_engry_us_201711,ele_engry_us_201712,
                        ele_engry_us_201801,ele_engry_us_201802,ele_engry_us_201803,
                        ele_engry_us_201804,ele_engry_us_201805,ele_engry_us_201806,
                        ele_engry_us_201807,ele_engry_us_201808,ele_engry_us_201809,
                        ele_engry_us_201810,ele_engry_us_201811,ele_engry_us_201812)

fire$ele_engry_us_all<-apply(ele_all,1,median)

# 가스 #
gas_all<-fire%>% select(gas_engry_us_201401,gas_engry_us_201402,gas_engry_us_201403,
                        gas_engry_us_201404,gas_engry_us_201405,gas_engry_us_201406,
                        gas_engry_us_201407,gas_engry_us_201408,gas_engry_us_201409,
                        gas_engry_us_201410,gas_engry_us_201411,gas_engry_us_201412,
                        gas_engry_us_201501,gas_engry_us_201502,gas_engry_us_201503,
                        gas_engry_us_201504,gas_engry_us_201505,gas_engry_us_201506,
                        gas_engry_us_201507,gas_engry_us_201508,gas_engry_us_201509,
                        gas_engry_us_201510,gas_engry_us_201511,gas_engry_us_201512,
                        gas_engry_us_201601,gas_engry_us_201602,gas_engry_us_201603,
                        gas_engry_us_201604,gas_engry_us_201605,gas_engry_us_201606,
                        gas_engry_us_201607,gas_engry_us_201608,gas_engry_us_201609,
                        gas_engry_us_201610,gas_engry_us_201611,gas_engry_us_201612,
                        gas_engry_us_201701,gas_engry_us_201702,gas_engry_us_201703,
                        gas_engry_us_201704,gas_engry_us_201705,gas_engry_us_201706,
                        gas_engry_us_201707,gas_engry_us_201708,gas_engry_us_201709,
                        gas_engry_us_201710,gas_engry_us_201711,gas_engry_us_201712,
                        gas_engry_us_201801,gas_engry_us_201802,gas_engry_us_201803,
                        gas_engry_us_201804,gas_engry_us_201805,gas_engry_us_201806,
                        gas_engry_us_201807,gas_engry_us_201808,gas_engry_us_201809,
                        gas_engry_us_201810,gas_engry_us_201811,gas_engry_us_201812)

fire$gas_engry_us_all<-apply(gas_all,1,median)



fire <- fire[,-c(18:137)]

summary(fire)

fire <- fire[!(is.na(fire$dt_of_athrztn) == TRUE),]

## 추가 전처리

library(lubridate)

#승인일자 연도 dt_of_athrztn
fire$year_of_athrztn<-substr(fire$dt_of_athrztn,1,4); 
table(fire$year_of_athrztn)	#1.9e???  id=86599 75903

str(fire$year_of_athrztn)	#chr
fire$year_of_athrztn<-as.numeric(fire$year_of_athrztn)	#num 변환

#화재발생일자 연도 dt_of_fr
fire$year_of_fr<-year(fire$dt_of_fr);table(fire$year_of_fr)
str(fire$year_of_fr)	#numeric


#차이
fire$diff_in_year<-(fire$year_of_fr-fire$year_of_athrztn)
table(fire$diff_in_year);summary(fire$diff_in_year)
length(which(fire$year_of_fr<fire$year_of_athrztn))

fire[fire$diff_in_year<0|fire$diff_in_year>371,'diff_in_year']<-NA	#Na 처리
summary(fire$diff_in_year)	#확인

ggplot(data=fire, aes(x=diff_in_year, fill=fr_yn))+ geom_histogram(position='identity',colour='black') 
ggplot(data=fire, aes(x=diff_in_year, fill=fr_yn))+ geom_histogram(position='fill',colour='black') 

## 행정구역명 없는 행 삭제 ##

fire <- fire[!(fire$emd_nm == ""),]


### 건물승인일자 삭제, 토지면적 삭제 , rgnl2삭제 ###
fire <- fire[,-c(8,9,35)]

#### rgnl 레벨 축소 ####

library(dplyr)

summary(fire$rgnl_ar_nm)

fire$rgnl<-NA

fire$rgnl[fire$rgnl_ar_nm=="제1종일반주거지역"|fire$rgnl_ar_nm=="제2종일반주거지역"|fire$rgnl_ar_nm=="제3종일반주거지역"|
              fire$rgnl_ar_nm=="제1종전용주거지역"|fire$rgnl_ar_nm=="제2종전용주거지역"|fire$rgnl_ar_nm=="준주거지역"]<-"주거지역"
fire$rgnl[fire$rgnl_ar_nm=="중심상업지역"|fire$rgnl_ar_nm=="일반상업지역"|fire$rgnl_ar_nm=="근린상업지역"|
              fire$rgnl_ar_nm=="유통상업지역"]<-"상업지역"
fire$rgnl[fire$rgnl_ar_nm=="일반공업지역"|fire$rgnl_ar_nm=="준공업지역"]<-"공업지역"
fire$rgnl[fire$rgnl_ar_nm=="보전녹지지역"|fire$rgnl_ar_nm=="생산녹지지역"|fire$rgnl_ar_nm=="자연녹지지역"]<-"녹지지역"
fire$rgnl[fire$rgnl_ar_nm=="보전관리지역"]<-"보전관리지역"
fire$rgnl[fire$rgnl_ar_nm=="생산관리지역"]<-"생산관리지역"
fire$rgnl[fire$rgnl_ar_nm=="계획관리지역"]<-"계획관리지역"
fire$rgnl[fire$rgnl_ar_nm=="농림지역"]<-"농림지역"
fire$rgnl[fire$rgnl_ar_nm=="자연환경보전지역"]<-"자연환경보전지역"
fire$rgnl[fire$rgnl_ar_nm=="관리지역"]<-"계획관리지역"
fire$rgnl[fire$rgnl_ar_nm=="개발제한구역"]<-"개발제한구역"
fire$rgnl[fire$rgnl_ar_nm=="용도미지정"]<-"용도미지정"
fire$rgnl[fire$rgnl_ar_nm==""]<-"용도미지정"

fire$rgnl<-as.factor(fire$rgnl)
summary(fire$rgnl)

####lnd_us_sttn_nm => lnd_us 로 범주 줄이기
summary(fire$lnd_us_sttn_nm)

fire$lnd_us<-NA

fire$lnd_us[fire$lnd_us_sttn_nm=="단독"|fire$lnd_us_sttn_nm=="연립"|fire$lnd_us_sttn_nm=="다세대"|
                fire$lnd_us_sttn_nm=="아파트"|fire$lnd_us_sttn_nm=="주거기타"|fire$lnd_us_sttn_nm=="주거나지"]<-"주거용"
fire$lnd_us[fire$lnd_us_sttn_nm=="상업용"|fire$lnd_us_sttn_nm=="업무용"|fire$lnd_us_sttn_nm=="상업나지"|fire$lnd_us_sttn_nm=="상업기타"]<-"상업업무용"
fire$lnd_us[fire$lnd_us_sttn_nm=="주상용"|fire$lnd_us_sttn_nm=="주상나지"|fire$lnd_us_sttn_nm=="주상기타"]<-"주상복합용"
fire$lnd_us[fire$lnd_us_sttn_nm=="공업용"|fire$lnd_us_sttn_nm=="공업나지"|fire$lnd_us_sttn_nm=="공업기타"]<-"공업용"
fire$lnd_us[fire$lnd_us_sttn_nm=="전"|fire$lnd_us_sttn_nm=="과수원"|fire$lnd_us_sttn_nm=="전기타"]<-"전"
fire$lnd_us[fire$lnd_us_sttn_nm=="답"|fire$lnd_us_sttn_nm=="답기타"]<-"답"
fire$lnd_us[fire$lnd_us_sttn_nm=="조림"|fire$lnd_us_sttn_nm=="자연림"|fire$lnd_us_sttn_nm=="목장용지"|
                fire$lnd_us_sttn_nm=="임야기타"|fire$lnd_us_sttn_nm=="토지임야"]<-"임야"
fire$lnd_us[fire$lnd_us_sttn_nm=="공원묘지"|fire$lnd_us_sttn_nm=="골프장 회원제"|fire$lnd_us_sttn_nm=="골프장 대중제"|
                fire$lnd_us_sttn_nm=="여객자동차터미널"|fire$lnd_us_sttn_nm=="고속도로휴게소"]<-"특수토지"
fire$lnd_us[fire$lnd_us_sttn_nm=="도로등"|fire$lnd_us_sttn_nm=="공원등"|fire$lnd_us_sttn_nm=="주차장등"|
                fire$lnd_us_sttn_nm=="하천등"|fire$lnd_us_sttn_nm=="유해.혐오시설"|fire$lnd_us_sttn_nm=="운동장등"|
                fire$lnd_us_sttn_nm=="위험시설"|fire$lnd_us_sttn_nm=="기타"]<-"공공용지"

fire$lnd_us<-as.factor(fire$lnd_us)

summary(fire$lnd_us)

#NA 대체 => bldng_us 기준
summary(filter(fire,bldng_us=="단독주택")$lnd_us)
fire$lnd_us[fire$bldng_us=="단독주택" & is.na(fire$lnd_us)==TRUE]<-"주거용"
summary(filter(fire,bldng_us=="단독주택")$lnd_us)

summary(filter(fire,bldng_us=="동.식물 관련시설")$lnd_us)
fire$lnd_us[fire$bldng_us=="동.식물 관련시설" & is.na(fire$lnd_us)==TRUE]<-"주거용"
summary(filter(fire,bldng_us=="동.식물 관련시설")$lnd_us)

summary(filter(fire,bldng_us=="제1종근린생활시설")$lnd_us)
fire$lnd_us[fire$bldng_us=="제1종근린생활시설" & is.na(fire$lnd_us)==TRUE]<-"상업업무용"
summary(filter(fire,bldng_us=="제1종근린생활시설")$lnd_us)

summary(filter(fire,bldng_us=="제2종근린생활시설")$lnd_us)
fire$lnd_us[fire$bldng_us=="제2종근린생활시설" & is.na(fire$lnd_us)==TRUE]<-"상업업무용"
summary(filter(fire,bldng_us=="제2종근린생활시설")$lnd_us)

summary(filter(fire,bldng_us=="창고시설")$lnd_us)
fire$lnd_us[fire$bldng_us=="창고시설" & is.na(fire$lnd_us)==TRUE]<-"주거용"
summary(filter(fire,bldng_us=="창고시설")$lnd_us)

summary(fire$lnd_us)

filter(fire,is.na(lnd_us)==TRUE)$bldng_us

summary(filter(fire,bldng_us=="공동주택")$lnd_us)
fire$lnd_us[fire$bldng_us=="공동주택" & is.na(fire$lnd_us)==TRUE]<-"주거용"
summary(filter(fire,bldng_us=="공동주택")$lnd_us)

summary(filter(fire,bldng_us=="공장")$lnd_us)
fire$lnd_us[fire$bldng_us=="공장" & is.na(fire$lnd_us)==TRUE]<-"공업용"
summary(filter(fire,bldng_us=="공장")$lnd_us)

summary(filter(fire,bldng_us=="관광휴게시설")$lnd_us)
fire$lnd_us[fire$bldng_us=="관광휴게시설" & is.na(fire$lnd_us)==TRUE]<-"특수토지"
summary(filter(fire,bldng_us=="관광휴게시설")$lnd_us)

summary(filter(fire,bldng_us=="교육연구시설")$lnd_us)
fire$lnd_us[fire$bldng_us=="교육연구시설" & is.na(fire$lnd_us)==TRUE]<-"주거용"
summary(filter(fire,bldng_us=="교육연구시설")$lnd_us)

#가장 많은 그룹이 애매함 => 묘지관련시설은 원칙상 공공용지 또는 특수토지 => 특수토지 0 이라 공공용지로 넣음
summary(filter(fire,bldng_us=="묘지관련시설")$lnd_us)
fire$lnd_us[fire$bldng_us=="묘지관련시설" & is.na(fire$lnd_us)==TRUE]<-"공공용지"
summary(filter(fire,bldng_us=="묘지관련시설")$lnd_us)

summary(filter(fire,bldng_us=="문화및집회시설")$lnd_us)
fire$lnd_us[fire$bldng_us=="문화및집회시설" & is.na(fire$lnd_us)==TRUE]<-"주거용"
summary(filter(fire,bldng_us=="문화및집회시설")$lnd_us)

summary(filter(fire,bldng_us=="발전시설")$lnd_us)
fire$lnd_us[fire$bldng_us=="발전시설" & is.na(fire$lnd_us)==TRUE]<-"공업용"
summary(filter(fire,bldng_us=="발전시설")$lnd_us)

summary(filter(fire,bldng_us=="방송통신시설")$lnd_us)
fire$lnd_us[fire$bldng_us=="방송통신시설" & is.na(fire$lnd_us)==TRUE]<-"상업업무용"
summary(filter(fire,bldng_us=="방송통신시설")$lnd_us)

#가장 많은 그룹이 애매함 => 분뇨.쓰레기처리시설은 원칙상 공공용지 => 공공용지16,공업용15지만 사실 어디에 넣어도 전체적으로는 큰차이 없음=>공공용지로 넣음
summary(filter(fire,bldng_us=="분뇨.쓰레기처리시설")$lnd_us)
summary(fire$lnd_us)
fire$lnd_us[fire$bldng_us=="분뇨.쓰레기처리시설" & is.na(fire$lnd_us)==TRUE]<-"공공용지"
summary(filter(fire,bldng_us=="분뇨.쓰레기처리시설")$lnd_us)

summary(filter(fire,bldng_us=="자동차관련시설")$lnd_us)
fire$lnd_us[fire$bldng_us=="자동차관련시설" & is.na(fire$lnd_us)==TRUE]<-"상업업무용"
summary(filter(fire,bldng_us=="자동차관련시설")$lnd_us)

summary(filter(fire,bldng_us=="종교시설")$lnd_us)
fire$lnd_us[fire$bldng_us=="종교시설" & is.na(fire$lnd_us)==TRUE]<-"주거용"
summary(filter(fire,bldng_us=="종교시설")$lnd_us)

summary(filter(fire,bldng_us=="위험물저장및처리시설")$lnd_us)
fire$lnd_us[fire$bldng_us=="위험물저장및처리시설" & is.na(fire$lnd_us)==TRUE]<-"상업업무용"
summary(filter(fire,bldng_us=="위험물저장및처리시설")$lnd_us)

summary(filter(fire,bldng_us=="운동시설")$lnd_us)
fire$lnd_us[fire$bldng_us=="운동시설" & is.na(fire$lnd_us)==TRUE]<-"공공용지"
summary(filter(fire,bldng_us=="운동시설")$lnd_us)

summary(filter(fire,bldng_us=="숙박시설")$lnd_us)
fire$lnd_us[fire$bldng_us=="숙박시설" & is.na(fire$lnd_us)==TRUE]<-"상업업무용"
summary(filter(fire,bldng_us=="숙박시설")$lnd_us)

#가장 많은 그룹이 애매함 => 운수시설은 원칙상 특수토지 => 특수토지로 넣음
summary(filter(fire,bldng_us=="운수시설")$lnd_us)
fire$lnd_us[fire$bldng_us=="운수시설" & is.na(fire$lnd_us)==TRUE]<-"특수토지"
summary(filter(fire,bldng_us=="운수시설")$lnd_us)

summary(filter(fire,bldng_us=="위락시설")$lnd_us)
fire$lnd_us[fire$bldng_us=="위락시설" & is.na(fire$lnd_us)==TRUE]<-"상업업무용"
summary(filter(fire,bldng_us=="위락시설")$lnd_us)

filter(fire,is.na(lnd_us)==TRUE)$bldng_us

summary(filter(fire,bldng_us=="근린생활시설")$lnd_us)
fire$lnd_us[fire$bldng_us=="근린생활시설" & is.na(fire$lnd_us)==TRUE]<-"상업업무용"
summary(filter(fire,bldng_us=="근린생활시설")$lnd_us)

summary(filter(fire,bldng_us=="노유자시설")$lnd_us)
fire$lnd_us[fire$bldng_us=="노유자시설" & is.na(fire$lnd_us)==TRUE]<-"주거용"
summary(filter(fire,bldng_us=="노유자시설")$lnd_us)

summary(filter(fire,bldng_us=="업무시설")$lnd_us)
fire$lnd_us[fire$bldng_us=="업무시설" & is.na(fire$lnd_us)==TRUE]<-"상업업무용"
summary(filter(fire,bldng_us=="업무시설")$lnd_us)

summary(fire$lnd_us)

#lnd_us결측치 2개 => bldng_us 결측치 였던 것 3개 
filter(fire,is.na(lnd_us)==TRUE)
filter(fire,bldng_us=="")
summary(filter(fire,jmk=="잡")$bldng_us)

#bldng_us 결측치=>lnd_us결측치 아닌 것 1개 => 유해.혐오시설
summary(filter(fire,jmk=="잡",lnd_us_sttn_nm=="유해.혐오시설")$bldng_us)
fire$bldng_us[fire$bldng_us=="" & fire$jmk=="잡" & fire$lnd_us_sttn_nm=="유해.혐오시설"]<-"분뇨.쓰레기처리시설"
fire$lnd_us[fire$bldng_us=="분뇨.쓰레기처리시설" & is.na(fire$lnd_us)==TRUE]<-"공공용지"
summary(filter(fire,bldng_us=="분뇨.쓰레기처리시설")$lnd_us)

#bldng_us 결측치=>lnd_us결측치 2개=> jmk만 봤더나 모두 잡 => 동.식물 관련시설 로 대체
summary(filter(fire,jmk=="잡")$bldng_us)
fire$bldng_us[fire$bldng_us=="" & fire$jmk=="잡"]<-"동.식물 관련시설"
fire$lnd_us[fire$bldng_us=="동.식물 관련시설" & is.na(fire$lnd_us)==TRUE]<-"주거용"
summary(filter(fire,bldng_us=="동.식물 관련시설")$lnd_us)

summary(fire$lnd_us)
summary(fire$bldng_us)
#blank가 level에 껴있어서 제거함
fire$bldng_us<-as.character(fire$bldng_us)
fire$bldng_us<-as.factor(fire$bldng_us)
summary(fire$bldng_us)

#NA 0 인것 확인
filter(fire,bldng_us=="")
filter(fire,lnd_us=="")
filter(fire,rgnl=="")


##건물구조 NA 대체
filter(fire,bldng_archtctr=="")$id

#213    -->벽돌구조1804,철근콘크리트구조2010,블록구조836-->철근콘크리트구조
summary(filter(fire,code==213)$bldng_archtctr)
filter(fire,bldng_archtctr=="",code==213)$id #18개
fire$bldng_archtctr[fire$bldng_archtctr=="" & fire$code==213]<-"철근콘크리트구조"
summary(filter(fire,code==213)$bldng_archtctr)

#107
summary(filter(fire,code==107)$bldng_archtctr)
filter(fire,bldng_archtctr=="",code==107)$id #2개
fire$bldng_archtctr[fire$bldng_archtctr=="" & fire$code==107]<-"일반목구조"
summary(filter(fire,code==107)$bldng_archtctr)

#146
summary(filter(fire,code==146)$bldng_archtctr)
filter(fire,bldng_archtctr=="",code==146)$id #2개
fire$bldng_archtctr[fire$bldng_archtctr=="" & fire$code==146]<-"철근콘크리트구조"
summary(filter(fire,code==146)$bldng_archtctr)

#197   -->벽돌구조794,철근콘크리트구조623,일반목구조351-->벽돌구조
summary(filter(fire,code==197)$bldng_archtctr)
filter(fire,bldng_archtctr=="",code==197)$id #8개
fire$bldng_archtctr[fire$bldng_archtctr=="" & fire$code==197]<-"벽돌구조"
summary(filter(fire,code==197)$bldng_archtctr)

#233
summary(filter(fire,code==233)$bldng_archtctr)
filter(fire,bldng_archtctr=="",code==233)$id #1개
fire$bldng_archtctr[fire$bldng_archtctr=="" & fire$code==233]<-"철근콘크리트구조"
summary(filter(fire,code==233)$bldng_archtctr)

#36   ---> 블록구조44 일반목구조59 경량철골구조37--> 일반목구조
summary(filter(fire,code==36)$bldng_archtctr)
filter(fire,bldng_archtctr=="",code==36)$id #2개
fire$bldng_archtctr[fire$bldng_archtctr=="" & fire$code==36]<-"일반목구조"
summary(filter(fire,code==36)$bldng_archtctr)

#120   --> 경량철골구조5 일반철골구조3,벽돌구조2,...1...-->경량철골구조
summary(filter(fire,code==120)$bldng_archtctr)
filter(fire,bldng_archtctr=="",code==120)$id #1개
fire$bldng_archtctr[fire$bldng_archtctr=="" & fire$code==120]<-"경량철골구조"
summary(filter(fire,code==120)$bldng_archtctr)

#153
summary(filter(fire,code==153)$bldng_archtctr)
filter(fire,bldng_archtctr=="",code==153)$id #1개
fire$bldng_archtctr[fire$bldng_archtctr=="" & fire$code==153]<-"일반철골구조"
summary(filter(fire,code==153)$bldng_archtctr)

#5
summary(filter(fire,code==5)$bldng_archtctr)
filter(fire,bldng_archtctr=="",code==5)$id #2개
fire$bldng_archtctr[fire$bldng_archtctr=="" & fire$code==5]<-"벽돌구조"
summary(filter(fire,code==5)$bldng_archtctr)

#37
summary(filter(fire,code==37)$bldng_archtctr)
filter(fire,bldng_archtctr=="",code==37)$id #23개
fire$bldng_archtctr[fire$bldng_archtctr=="" & fire$code==37]<-"일반목구조"
summary(filter(fire,code==37)$bldng_archtctr)

#11   -->경량철골구조3,기타조적구조1, 일반철골구조2,철근콘크리트구조1-->경량철골구조
summary(filter(fire,code==11)$bldng_archtctr)
filter(fire,bldng_archtctr=="",code==11)$id #1개
fire$bldng_archtctr[fire$bldng_archtctr=="" & fire$code==11]<-"경량철골구조"
summary(filter(fire,code==11)$bldng_archtctr)

#169   -->경량철골구조19, 블록구조16,철근콘크리트구조14,일반철골구조12,벽돌구조11-->경량철골구조
summary(filter(fire,code==169)$bldng_archtctr)
filter(fire,bldng_archtctr=="",code==169)$id #1개
fire$bldng_archtctr[fire$bldng_archtctr=="" & fire$code==169]<-"경량철골구조"
summary(filter(fire,code==169)$bldng_archtctr)

#65    -->강파이프구조27,경량철골구조23,일반철골구조25-->강파이프구조
summary(filter(fire,code==65)$bldng_archtctr)
filter(fire,bldng_archtctr=="",code==65)$id #1개
fire$bldng_archtctr[fire$bldng_archtctr=="" & fire$code==65]<-"강파이프구조"
summary(filter(fire,code==65)$bldng_archtctr)

#130  --> 결양철골구조2,철근콘크리트구조1--> 코드130중 결측치1개가 운동시설,문교사회용..// 운동장등,주거기타 => 철근콘크리트구조
summary(filter(fire,code==130)$bldng_archtctr)
filter(fire,bldng_archtctr=="",code==130)$id #1개
summary(filter(fire,bldng_us=="운동시설")$bldng_archtctr) #=>철근콘크리트구조 가 가장 많음
fire$bldng_archtctr[fire$bldng_archtctr=="" & fire$code==130]<-"철근콘크리트구조"
summary(filter(fire,code==130)$bldng_archtctr)

#198  -->lnd_us_sttn_nm 고려함.--> 일반목구조2,블록구조2-->???????
summary(filter(fire,code==198)$bldng_archtctr)
filter(fire,bldng_archtctr=="",code==198)$id #1개
filter(fire,bldng_archtctr=="",code==198)
summary(filter(fire,bldng_us=="단독주택")$bldng_archtctr) #일반목구조,블록구조 중에 많은것=>일반목구조
fire$bldng_archtctr[fire$bldng_archtctr=="" & fire$code==198]<-"일반목구조"
summary(filter(fire,code==198)$bldng_archtctr)

#46  
summary(filter(fire,code==46)$bldng_archtctr)
filter(fire,bldng_archtctr=="",code==46)$id #5개
fire$bldng_archtctr[fire$bldng_archtctr=="" & fire$code==46]<-"일반철골구조"
summary(filter(fire,code==46)$bldng_archtctr)

#80  -->일반목구조181,벽돌구조125,경량철골구조99-->일반목구조
summary(filter(fire,code==80)$bldng_archtctr)
filter(fire,bldng_archtctr=="",code==80)$id #1개
fire$bldng_archtctr[fire$bldng_archtctr=="" & fire$code==80]<-"일반목구조"
summary(filter(fire,code==80)$bldng_archtctr)

#148
summary(filter(fire,code==148)$bldng_archtctr)
filter(fire,bldng_archtctr=="",code==148)$id #2개
fire$bldng_archtctr[fire$bldng_archtctr=="" & fire$code==148]<-"철근콘크리트구조"
summary(filter(fire,code==148)$bldng_archtctr)

#171   -->일반목구조18,블록구조14-->단독주택 고려--> 일반목구조가 많음
summary(filter(fire,code==171)$bldng_archtctr)
filter(fire,bldng_archtctr=="",code==171)$id #1개
summary(filter(fire,bldng_us=="단독주택")$bldng_archtctr)
fire$bldng_archtctr[fire$bldng_archtctr=="" & fire$code==171]<-"일반목구조"
summary(filter(fire,code==171)$bldng_archtctr)

#173   -->철근콬크리트구조6,일반철골구조5,벽돌구조4--> 철근콘크리트구조
summary(filter(fire,code==173)$bldng_archtctr)
filter(fire,bldng_archtctr=="",code==173)$id #1개
filter(fire,bldng_archtctr=="",code==173)
summary(filter(fire,bldng_us=="위험물저장및처리시설")$bldng_archtctr) #철근콘크리트구조가 많음
fire$bldng_archtctr[fire$bldng_archtctr=="" & fire$code==173]<-"철근콘크리트구조"
summary(filter(fire,code==173)$bldng_archtctr)

#160    --> 벽돌구조300,일반목구조294,철근콘크리특조233-->??????
summary(filter(fire,code==160)$bldng_archtctr)
filter(fire,bldng_archtctr=="",code==160)$id #1개
filter(fire,bldng_archtctr=="",code==160)
summary(filter(fire,bldng_us=="단독주택")$bldng_archtctr) #벽돌구조가 많음
fire$bldng_archtctr[fire$bldng_archtctr=="" & fire$code==160]<-"벽돌구조"
summary(filter(fire,code==160)$bldng_archtctr)

#121    --> 경량철골구조30,벽돌구조25,철근콘크리트구조28-->블록구조
summary(filter(fire,code==121)$bldng_archtctr)
filter(fire,bldng_archtctr=="",code==121)$id #1개
filter(fire,bldng_archtctr=="",code==121)
summary(filter(fire,bldng_us=="단독주택")$bldng_archtctr) #=> 벽돌구조가 많음
fire$bldng_archtctr[fire$bldng_archtctr=="" & fire$code==121]<-"벽돌구조"
summary(filter(fire,code==121)$bldng_archtctr)

#106     
summary(filter(fire,code==106)$bldng_archtctr)
filter(fire,bldng_archtctr=="",code==106)$id #1개
fire$bldng_archtctr[fire$bldng_archtctr=="" & fire$code==106]<-"경량철골구조"
summary(filter(fire,code==106)$bldng_archtctr)

#58
summary(filter(fire,code==58)$bldng_archtctr)
filter(fire,bldng_archtctr=="",code==58)$id #1개
fire$bldng_archtctr[fire$bldng_archtctr=="" & fire$code==58]<-"철근콘크리트구조"
summary(filter(fire,code==58)$bldng_archtctr)

#39      
summary(filter(fire,code==39)$bldng_archtctr)
filter(fire,bldng_archtctr=="",code==39)$id #1개
fire$bldng_archtctr[fire$bldng_archtctr=="" & fire$code==39]<-"경량철골구조"
summary(filter(fire,code==39)$bldng_archtctr)

#45   -->경량철골구조94,블록구조64,철근콘크리트구조51,일반철골구조43-->>경량철골구조
summary(filter(fire,code==45)$bldng_archtctr)
filter(fire,bldng_archtctr=="",code==45)$id #1개
fire$bldng_archtctr[fire$bldng_archtctr=="" & fire$code==45]<-"경량철골구조"
summary(filter(fire,code==45)$bldng_archtctr)

##bldng_us_clssfctn 결측치 대체
filter(fire,bldng_us_clssfctn=="")

summary(fire$bldng_us_clssfctn)

#교정및군사시설  => 결측데이터밖에 없음      
summary(filter(fire,bldng_us=="교정및군사시설")$bldng_us_clssfctn)
fire$bldng_us_clssfctn[fire$bldng_us=="교정및군사시설" & fire$bldng_us_clssfctn==""]<-"공공용"
summary(filter(fire,bldng_us=="교정및군사시설")$bldng_us_clssfctn)

#파출소  데이터 1개
summary(filter(fire,bldng_us=="파출소")$bldng_us_clssfctn)
fire$bldng_us_clssfctn[fire$bldng_us=="파출소" & fire$bldng_us_clssfctn==""]<-"공공용"
summary(filter(fire,bldng_us=="파출소")$bldng_us_clssfctn)

#판매및영업시설   데이터 1개
summary(filter(fire,bldng_us=="판매및영업시설")$bldng_us_clssfctn)
fire$bldng_us_clssfctn[fire$bldng_us=="판매및영업시설" & fire$bldng_us_clssfctn==""]<-"상업용"
summary(filter(fire,bldng_us=="판매및영업시설")$bldng_us_clssfctn)

#동.식물 관련시설=>농수산용
summary(filter(fire,bldng_us=="동.식물 관련시설")$bldng_us_clssfctn)
fire$bldng_us_clssfctn[fire$bldng_us=="동.식물 관련시설" & fire$bldng_us_clssfctn==""]<-"농수산용"
summary(filter(fire,bldng_us=="동.식물 관련시설")$bldng_us_clssfctn)

#분뇨.쓰레기처리시설 => 기타 
summary(filter(fire,bldng_us=="분뇨.쓰레기처리시설")$bldng_us_clssfctn)
fire$bldng_us_clssfctn[fire$bldng_us=="분뇨.쓰레기처리시설" & fire$bldng_us_clssfctn==""]<-"기타"
summary(filter(fire,bldng_us=="분뇨.쓰레기처리시설")$bldng_us_clssfctn)


filter(fire,bldng_us=="")
filter(fire,lnd_us=="")
filter(fire,rgnl=="")
filter(fire,bldng_archtctr=="")
filter(fire,bldng_us_clssfctn=="")
#####NA대체 완료


summary(fire$lnd_us)
summary(fire$bldng_us)
summary(fire$bldng_archtctr)
summary(fire$bldng_us_clssfctn)

#blank가 level에 껴있어서 제거함

fire$bldng_archtctr<-as.character(fire$bldng_archtctr)
fire$bldng_archtctr<-as.factor(fire$bldng_archtctr)
summary(fire$bldng_archtctr)


fire$bldng_us_clssfctn<-as.character(fire$bldng_us_clssfctn)
fire$bldng_us_clssfctn<-as.factor(fire$bldng_us_clssfctn)
summary(fire$bldng_us_clssfctn)


nrow(subset(fire,bldng_us == "")) # 건물용도 결측 0개
nrow(subset(fire,bldng_archtctr == "")) # 건물구조 결측 0개
nrow(subset(fire,bldng_us_clssfctn == "")) # 건물용도분류명 결측 0개

#####관할 소방서 인원 결측치 및 이상치 대체
######결측치는 지역, 년도로 분류한 그룹의 메디안값으로 대체
#######이상치는 위 분류대로 나눈 그룹에서 1개의 값만 다른 경우 메디안으로 대체(그룹내의 값은 사실상 모두 동일)
library(stringr)
filter(fire,str_count(fire$emd_nm, pattern = "거제시")==1,
       str_count(fire$dt_of_fr, pattern = "2014")==1,(is.na(fire$fr_mn_cnt)==TRUE))$id
fire$fr_mn_cnt[fire$id==100582]<-210

filter(fire,str_count(fire$emd_nm, pattern = "거제시")==1,
       str_count(fire$dt_of_fr, pattern = "2014")==1 )$fr_mn_cnt

filter(fire,str_count(fire$emd_nm, pattern = "거제시")==1,
       str_count(fire$dt_of_fr, pattern = "2018")==1,(is.na(fire$fr_mn_cnt)==TRUE))$id
filter(fire,str_count(fire$emd_nm, pattern = "거제시")==1,
       str_count(fire$dt_of_fr, pattern = "2018")==1)$fr_mn_cnt

fire$fr_mn_cnt[fire$id==104691]<-210

filter(fire,str_count(fire$emd_nm, pattern = "고성군")==1,
       str_count(fire$dt_of_fr, pattern = "2014")==1,(is.na(fire$fr_mn_cnt)==TRUE))$id
filter(fire,str_count(fire$emd_nm, pattern = "고성군")==1,
       str_count(fire$dt_of_fr, pattern = "2014")==1)$fr_mn_cnt

fire$fr_mn_cnt[fire$id==c(82059 ,130630, 101169)]<-112


filter(fire,str_count(fire$emd_nm, pattern = "고성군")==1,
       str_count(fire$dt_of_fr, pattern = "2015")==1,(is.na(fire$fr_mn_cnt)==TRUE))$id
filter(fire,str_count(fire$emd_nm, pattern = "고성군")==1,
       str_count(fire$dt_of_fr, pattern = "2015")==1)$fr_mn_cnt

fire$fr_mn_cnt[fire$id==125737]<-112
fire$fr_mn_cnt[fire$id==87486]<-112

filter(fire,str_count(fire$emd_nm, pattern = "남해군")==1,
       str_count(fire$dt_of_fr, pattern = "2016")==1,(is.na(fire$fr_mn_cnt)==TRUE))$id
filter(fire,str_count(fire$emd_nm, pattern = "남해군")==1,
       str_count(fire$dt_of_fr, pattern = "2016")==1)$fr_mn_cnt

fire$fr_mn_cnt[fire$id==83794]<-106

filter(fire,str_count(fire$emd_nm, pattern = "남해군")==1,
       str_count(fire$dt_of_fr, pattern = "2018")==1,(is.na(fire$fr_mn_cnt)==TRUE))$id
filter(fire,str_count(fire$emd_nm, pattern = "남해군")==1,
       str_count(fire$dt_of_fr, pattern = "2018")==1)$fr_mn_cnt
fire$fr_mn_cnt[fire$id== 106495]<-106

filter(fire,str_count(fire$emd_nm, pattern = "사천시")==1,
       str_count(fire$dt_of_fr, pattern = "2015")==1,(is.na(fire$fr_mn_cnt)==TRUE))$id
filter(fire,str_count(fire$emd_nm, pattern = "사천시")==1,
       str_count(fire$dt_of_fr, pattern = "2015")==1)$fr_mn_cnt

fire$fr_mn_cnt[fire$id==77843 ]<-164
fire$fr_mn_cnt[fire$id==131873 ]<-164
fire$fr_mn_cnt[fire$id==138036 ]<-164

filter(fire,str_count(fire$emd_nm, pattern = "사천시")==1,
       str_count(fire$dt_of_fr, pattern = "2018")==1,(is.na(fire$fr_mn_cnt)==TRUE))$id
filter(fire,str_count(fire$emd_nm, pattern = "사천시")==1,
       str_count(fire$dt_of_fr, pattern = "2018")==1)$fr_mn_cnt

fire$fr_mn_cnt[fire$id==113701]<-164

filter(fire,str_count(fire$emd_nm, pattern = "창원시")==1,
       str_count(fire$dt_of_fr, pattern = "2016")==1,(is.na(fire$fr_mn_cnt)==TRUE))$id
filter(fire,str_count(fire$emd_nm, pattern = "창원시")==1,
       str_count(fire$dt_of_fr, pattern = "2016")==1)$fr_mn_cnt

fire$fr_mn_cnt[fire$id==120648 ]<-176
fire$fr_mn_cnt[fire$id==118446 ]<-176
fire$fr_mn_cnt[fire$id==124502 ]<-176

filter(fire,str_count(fire$emd_nm, pattern = "창원시")==1,
       str_count(fire$dt_of_fr, pattern = "2017")==1,(is.na(fire$fr_mn_cnt)==TRUE))$id
filter(fire,str_count(fire$emd_nm, pattern = "창원시")==1,
       str_count(fire$dt_of_fr, pattern = "2017")==1)$fr_mn_cnt

fire$fr_mn_cnt[fire$id==137731  ]<-176
fire$fr_mn_cnt[fire$id==83355 ]<-176

filter(fire,str_count(fire$emd_nm, pattern = "창원시")==1,
       str_count(fire$dt_of_fr, pattern = "2018")==1,(is.na(fire$fr_mn_cnt)==TRUE))$id
mean(filter(fire,str_count(fire$emd_nm, pattern = "창원시")==1,
            str_count(fire$dt_of_fr, pattern = "2018")==1)$fr_mn_cnt,na.rm=TRUE)

fire$fr_mn_cnt[fire$id==96265  ]<-176
fire$fr_mn_cnt[fire$id==85666 ]<-176
fire$fr_mn_cnt[fire$id==131372 ]<-176

filter(fire,str_count(fire$emd_nm, pattern = "통영시")==1,
       +        str_count(fire$dt_of_fr, pattern = "2014")==1,(is.na(fire$fr_mn_cnt)==TRUE))$id
mean(filter(fire,str_count(fire$emd_nm, pattern = "통영시")==1,
            +             str_count(fire$dt_of_fr, pattern = "2014")==1)$fr_mn_cnt,na.rm=TRUE)
median(filter(fire,str_count(fire$emd_nm, pattern = "통영시")==1,
              +             str_count(fire$dt_of_fr, pattern = "2014")==1)$fr_mn_cnt,na.rm=TRUE)

fire$fr_mn_cnt[fire$id==136412|fire$id==101695|fire$id==82115|fire$id==118256|fire$id==94083]<-161

filter(fire,str_count(fire$emd_nm, pattern = "통영시")==1,
       +        str_count(fire$dt_of_fr, pattern = "2014")==1,(is.na(fire$fr_mn_cnt)==TRUE))$id

filter(fire,str_count(fire$emd_nm, pattern = "통영시")==1,
       str_count(fire$dt_of_fr, pattern = "2015")==1,(is.na(fire$fr_mn_cnt)==TRUE))$id
mean(filter(fire,str_count(fire$emd_nm, pattern = "통영시")==1,
            str_count(fire$dt_of_fr, pattern = "2015")==1)$fr_mn_cnt,na.rm=TRUE)

fire$fr_mn_cnt[fire$id==89508|fire$id==133356|fire$id==107602|fire$id==81738|
                   fire$id==107331|fire$id==118433]<-161

filter(fire,str_count(fire$emd_nm, pattern = "통영시")==1,
       str_count(fire$dt_of_fr, pattern = "2016")==1,(is.na(fire$fr_mn_cnt)==TRUE))$id
mean(filter(fire,str_count(fire$emd_nm, pattern = "통영시")==1,
            str_count(fire$dt_of_fr, pattern = "2016")==1)$fr_mn_cnt,na.rm=TRUE)

fire$fr_mn_cnt[fire$id==104796|fire$id==127172|fire$id==101396|fire$id==112015]<-161

filter(fire,str_count(fire$emd_nm, pattern = "통영시")==1,
       str_count(fire$dt_of_fr, pattern = "2017")==1,(is.na(fire$fr_mn_cnt)==TRUE))$id
mean(filter(fire,str_count(fire$emd_nm, pattern = "통영시")==1,
            str_count(fire$dt_of_fr, pattern = "2017")==1)$fr_mn_cnt,na.rm=TRUE)
filter(fire,str_count(fire$emd_nm, pattern = "통영시")==1,
       str_count(fire$dt_of_fr, pattern = "2017")==1,(fire$fr_mn_cnt==112))$id

fire$fr_mn_cnt[fire$id==133793|fire$id==74626|fire$id==70446|fire$id==102470|fire$id==130521]<-161

filter(fire,str_count(fire$emd_nm, pattern = "하동군")==1,
       str_count(fire$dt_of_fr, pattern = "2014")==1,(is.na(fire$fr_mn_cnt)==TRUE))$id
mean(filter(fire,str_count(fire$emd_nm, pattern = "하동군")==1,
            str_count(fire$dt_of_fr, pattern = "2014")==1)$fr_mn_cnt,na.rm=TRUE)
filter(fire,str_count(fire$emd_nm, pattern = "하동군")==1,
       str_count(fire$dt_of_fr, pattern = "2014")==1,(fire$fr_mn_cnt==164))$id


fire$fr_mn_cnt[fire$id==137744|fire$id==84302]<-118

colSums(is.na(fire[-c(18:137)]))

filter(fire,is.na(fire$fr_mn_cnt)==TRUE)$id
filter(fire,id==83794)

mean(filter(fire,str_count(fire$emd_nm, pattern = "남해군")==1,
            str_count(fire$dt_of_fr, pattern = "2016")==1)$fr_mn_cnt,na.rm=TRUE)
filter(fire,str_count(fire$emd_nm, pattern = "남해군")==1,
       str_count(fire$dt_of_fr, pattern = "2016")==1,(is.na(fire$fr_mn_cnt)==TRUE))$id

fire$fr_mn_cnt[fire$id==83794]<-106

## 인구수 결측치 대체 ##
----------------------------------------------------------------------------
    
    ##거제시##
    fire[fire$id==107509,159]<-7307
fire[fire$id==81931,159]<-7272
fire[fire$id==104691,159]<-27371
fire[fire$id==121535,159]<-3399
fire[fire$id==75238,159]<-3453
----------------------------------------------------------------------------
    
    ##고성군##
    fire[fire$id==109209,159]<-4570
fire[fire$id==101583,159]<-3851
fire[fire$id==124447,159]<-5113
----------------------------------------------------------------------------
    
    ##남해군##
    fire[fire$id==111932,159]<-5830
fire[fire$id==106495,159]<-13199
fire[fire$id==97235,159]<-3822	
fire[fire$id==120253,159]<-4305
----------------------------------------------------------------------------
    
    ##사천시#
    fire[fire$id==113578,159]<-5095
fire[fire$id==118226,159]<-5830
fire[fire$id==104635,159]<-5832
----------------------------------------------------------------------------
    
    ##양산시##
    fire[fire$id==81211,159]<-9572
----------------------------------------------------------------------------
    
    ##창녕군##
    fire[fire$id==112547,159]<-2785
fire[fire$id==133678,159]<-4082
fire[fire$id==128450,159]<-4020
---------------------------------------------------------------------------
    
    ##하동군##
    fire[fire$id==76630,159]<-6751
fire[fire$id==82742,159]<-3954
fire[fire$id==131214,159]<-4078		
fire[fire$id==97075,159]<-6698
----------------------------------------------------------------------------
    
    ###############진주시##############

#상대동
setwd("C:/Users/공다영/Documents/공다영/동국대학교/비교과/비어플/project/김해시 화재예측/data")
sangdae_set<-read.table("상대동인구.txt",header=T,sep="\t")

for(i in 2:length(sangdae_set)){
    sangdae_set[3,i]<-as.numeric(sangdae_set[3,i])
}
sangdae_set<-sangdae_set[3,]

__경상남도 진주시 상대동 & 인구수 결측인 경우__
sangdae<-which(fire$emd_nm=="경상남도 진주시 상대동" & is.na(fire$hm_cnt)==TRUE)

for(j in sangdae){
    if(year(fire[j,1])==2014 & month(fire[j,1])==1){
        fire[j,159]<-sangdae_set[1,2]
    }else if(year(fire[j,1])==2014 & month(fire[j,1])==2){
        fire[j,159]<-sangdae_set[1,3]  
    }else if(year(fire[j,1])==2014 & month(fire[j,1])==3){
        fire[j,159]<-sangdae_set[1,4] 
    }else if(year(fire[j,1])==2014 & month(fire[j,1])==4){
        fire[j,159]<-sangdae_set[1,5] 
    }else if(year(fire[j,1])==2014 & month(fire[j,1])==5){
        fire[j,159]<-sangdae_set[1,6] 
    }else if(year(fire[j,1])==2014 & month(fire[j,1])==6){
        fire[j,159]<-sangdae_set[1,7] 
    }else if(year(fire[j,1])==2014 & month(fire[j,1])==7){
        fire[j,159]<-sangdae_set[1,8] 
    }else if(year(fire[j,1])==2014 & month(fire[j,1])==8){
        fire[j,159]<-sangdae_set[1,9] 
    }else if(year(fire[j,1])==2014 & month(fire[j,1])==9){
        fire[j,159]<-sangdae_set[1,10] 
    }else if(year(fire[j,1])==2014 & month(fire[j,1])==10){
        fire[j,159]<-sangdae_set[1,11] 
    }else if(year(fire[j,1])==2014 & month(fire[j,1])==11){
        fire[j,159]<-sangdae_set[1,12] 
    }else if(year(fire[j,1])==2014 & month(fire[j,1])==12){
        fire[j,159]<-sangdae_set[1,13] 
    }else if(year(fire[j,1])==2015 & month(fire[j,1])==1){
        fire[j,159]<-sangdae_set[1,14] 
    }else if(year(fire[j,1])==2015 & month(fire[j,1])==2){
        fire[j,159]<-sangdae_set[1,15] 
    }else if(year(fire[j,1])==2015 & month(fire[j,1])==3){
        fire[j,159]<-sangdae_set[1,16] 
    }else if(year(fire[j,1])==2015 & month(fire[j,1])==4){
        fire[j,159]<-sangdae_set[1,17] 
    }else if(year(fire[j,1])==2015 & month(fire[j,1])==5){
        fire[j,159]<-sangdae_set[1,18] 
    }else if(year(fire[j,1])==2015 & month(fire[j,1])==6){
        fire[j,159]<-sangdae_set[1,19] 
    }else if(year(fire[j,1])==2015 & month(fire[j,1])==7){
        fire[j,159]<-sangdae_set[1,20] 
    }else if(year(fire[j,1])==2015 & month(fire[j,1])==8){
        fire[j,159]<-sangdae_set[1,21] 
    }else if(year(fire[j,1])==2015 & month(fire[j,1])==9){
        fire[j,159]<-sangdae_set[1,22] 
    }else if(year(fire[j,1])==2015 & month(fire[j,1])==10){
        fire[j,159]<-sangdae_set[1,23] 
    }else if(year(fire[j,1])==2015 & month(fire[j,1])==11){
        fire[j,159]<-sangdae_set[1,24] 
    }else if(year(fire[j,1])==2015 & month(fire[j,1])==12){
        fire[j,159]<-sangdae_set[1,25] 
    }else if(year(fire[j,1])==2016 & month(fire[j,1])==1){
        fire[j,159]<-sangdae_set[1,26] 
    }else if(year(fire[j,1])==2016 & month(fire[j,1])==2){
        fire[j,159]<-sangdae_set[1,27] 
    }else if(year(fire[j,1])==2016 & month(fire[j,1])==3){
        fire[j,159]<-sangdae_set[1,28] 
    }else if(year(fire[j,1])==2016 & month(fire[j,1])==4){
        fire[j,159]<-sangdae_set[1,29] 
    }else if(year(fire[j,1])==2016 & month(fire[j,1])==5){
        fire[j,159]<-sangdae_set[1,30] 
    }else if(year(fire[j,1])==2016 & month(fire[j,1])==6){
        fire[j,159]<-sangdae_set[1,31] 
    }else if(year(fire[j,1])==2016 & month(fire[j,1])==7){
        fire[j,159]<-sangdae_set[1,32] 
    }else if(year(fire[j,1])==2016 & month(fire[j,1])==8){
        fire[j,159]<-sangdae_set[1,33] 
    }else if(year(fire[j,1])==2016 & month(fire[j,1])==9){
        fire[j,159]<-sangdae_set[1,34] 
    }else if(year(fire[j,1])==2016 & month(fire[j,1])==10){
        fire[j,159]<-sangdae_set[1,35] 
    }else if(year(fire[j,1])==2016 & month(fire[j,1])==11){
        fire[j,159]<-sangdae_set[1,36] 
    }else if(year(fire[j,1])==2016 & month(fire[j,1])==12){
        fire[j,159]<-sangdae_set[1,37] 
    }
}



#상평동
fire[fire$id==101423,159]<-13525



#하대동
setwd("C:/Users/공다영/Documents/공다영/동국대학교/비교과/비어플/project/김해시 화재예측/data")
hadae_set<-read.table("하대동인구.txt",header=T,sep="\t")

for(i in 2:length(hadae_set)){
    hadae_set[3,i]<-as.numeric(hadae_set[3,i])
}
hadae_set<-hadae_set[3,]

__경상남도 진주시 하대동 & 인구수 결측인 경우__
hadae<-which(fire$emd_nm=="경상남도 진주시 하대동" & is.na(fire$hm_cnt)==TRUE)

for(j in hadae){
    if(year(fire[j,1])==2014 & month(fire[j,1])==1){
        fire[j,159]<-hadae_set[1,2]
    }else if(year(fire[j,1])==2014 & month(fire[j,1])==2){
        fire[j,159]<-hadae_set[1,3]  
    }else if(year(fire[j,1])==2014 & month(fire[j,1])==3){
        fire[j,159]<-hadae_set[1,4] 
    }else if(year(fire[j,1])==2014 & month(fire[j,1])==4){
        fire[j,159]<-hadae_set[1,5] 
    }else if(year(fire[j,1])==2014 & month(fire[j,1])==5){
        fire[j,159]<-hadae_set[1,6] 
    }else if(year(fire[j,1])==2014 & month(fire[j,1])==6){
        fire[j,159]<-hadae_set[1,7] 
    }else if(year(fire[j,1])==2014 & month(fire[j,1])==7){
        fire[j,159]<-hadae_set[1,8] 
    }else if(year(fire[j,1])==2014 & month(fire[j,1])==8){
        fire[j,159]<-hadae_set[1,9] 
    }else if(year(fire[j,1])==2014 & month(fire[j,1])==9){
        fire[j,159]<-hadae_set[1,10] 
    }else if(year(fire[j,1])==2014 & month(fire[j,1])==10){
        fire[j,159]<-hadae_set[1,11] 
    }else if(year(fire[j,1])==2014 & month(fire[j,1])==11){
        fire[j,159]<-hadae_set[1,12] 
    }else if(year(fire[j,1])==2014 & month(fire[j,1])==12){
        fire[j,159]<-hadae_set[1,13] 
    }else if(year(fire[j,1])==2015 & month(fire[j,1])==1){
        fire[j,159]<-hadae_set[1,14] 
    }else if(year(fire[j,1])==2015 & month(fire[j,1])==2){
        fire[j,159]<-hadae_set[1,15] 
    }else if(year(fire[j,1])==2015 & month(fire[j,1])==3){
        fire[j,159]<-hadae_set[1,16] 
    }else if(year(fire[j,1])==2015 & month(fire[j,1])==4){
        fire[j,159]<-hadae_set[1,17] 
    }else if(year(fire[j,1])==2015 & month(fire[j,1])==5){
        fire[j,159]<-hadae_set[1,18] 
    }else if(year(fire[j,1])==2015 & month(fire[j,1])==6){
        fire[j,159]<-hadae_set[1,19] 
    }else if(year(fire[j,1])==2015 & month(fire[j,1])==7){
        fire[j,159]<-hadae_set[1,20] 
    }else if(year(fire[j,1])==2015 & month(fire[j,1])==8){
        fire[j,159]<-hadae_set[1,21] 
    }else if(year(fire[j,1])==2015 & month(fire[j,1])==9){
        fire[j,159]<-hadae_set[1,22] 
    }else if(year(fire[j,1])==2015 & month(fire[j,1])==10){
        fire[j,159]<-hadae_set[1,23] 
    }else if(year(fire[j,1])==2015 & month(fire[j,1])==11){
        fire[j,159]<-hadae_set[1,24] 
    }else if(year(fire[j,1])==2015 & month(fire[j,1])==12){
        fire[j,159]<-hadae_set[1,25] 
    }else if(year(fire[j,1])==2016 & month(fire[j,1])==1){
        fire[j,159]<-hadae_set[1,26] 
    }else if(year(fire[j,1])==2016 & month(fire[j,1])==2){
        fire[j,159]<-hadae_set[1,27] 
    }else if(year(fire[j,1])==2016 & month(fire[j,1])==3){
        fire[j,159]<-hadae_set[1,28] 
    }else if(year(fire[j,1])==2016 & month(fire[j,1])==4){
        fire[j,159]<-hadae_set[1,29] 
    }else if(year(fire[j,1])==2016 & month(fire[j,1])==5){
        fire[j,159]<-hadae_set[1,30] 
    }else if(year(fire[j,1])==2016 & month(fire[j,1])==6){
        fire[j,159]<-hadae_set[1,31] 
    }else if(year(fire[j,1])==2016 & month(fire[j,1])==7){
        fire[j,159]<-hadae_set[1,32] 
    }else if(year(fire[j,1])==2016 & month(fire[j,1])==8){
        fire[j,159]<-hadae_set[1,33] 
    }else if(year(fire[j,1])==2016 & month(fire[j,1])==9){
        fire[j,159]<-hadae_set[1,34] 
    }else if(year(fire[j,1])==2016 & month(fire[j,1])==10){
        fire[j,159]<-hadae_set[1,35] 
    }else if(year(fire[j,1])==2016 & month(fire[j,1])==11){
        fire[j,159]<-hadae_set[1,36] 
    }else if(year(fire[j,1])==2016 & month(fire[j,1])==12){
        fire[j,159]<-hadae_set[1,37] 
    }else if(year(fire[j,1])==2017 & month(fire[j,1])==1){
        fire[j,159]<-hadae_set[1,38] 
    }else if(year(fire[j,1])==2017 & month(fire[j,1])==2){
        fire[j,159]<-hadae_set[1,39] 
    }else if(year(fire[j,1])==2017 & month(fire[j,1])==3){
        fire[j,159]<-hadae_set[1,40] 
    }else if(year(fire[j,1])==2017 & month(fire[j,1])==4){
        fire[j,159]<-hadae_set[1,41] 
    }else if(year(fire[j,1])==2017 & month(fire[j,1])==5){
        fire[j,159]<-hadae_set[1,42] 
    }else if(year(fire[j,1])==2017 & month(fire[j,1])==6){
        fire[j,159]<-hadae_set[1,43] 
    }else if(year(fire[j,1])==2017 & month(fire[j,1])==7){
        fire[j,159]<-hadae_set[1,44] 
    }else if(year(fire[j,1])==2017 & month(fire[j,1])==8){
        fire[j,159]<-hadae_set[1,45] 
    }else if(year(fire[j,1])==2017 & month(fire[j,1])==9){
        fire[j,159]<-hadae_set[1,46] 
    }else if(year(fire[j,1])==2017 & month(fire[j,1])==10){
        fire[j,159]<-hadae_set[1,47] 
    }else if(year(fire[j,1])==2017 & month(fire[j,1])==11){
        fire[j,159]<-hadae_set[1,48] 
    }else if(year(fire[j,1])==2017 & month(fire[j,1])==12){
        fire[j,159]<-hadae_set[1,49] 
    }
}

---------------------------------------------------------------------------
    
    ##############창원시##################

#마산합포구_반월동
fire[fire$id==112385,159]<-9245
fire[fire$id==118230,159]<-8742	
fire[fire$id==83916,159]<-9104
fire[fire$id==107394,159]<-9169
fire[fire$id==112398,159]<-9094
fire[fire$id==114445,159]<-9245
fire[fire$id==76615,159]<-9128
fire[fire$id==76181,159]<-9128
fire[fire$id==136143,159]<-9068
fire[fire$id==117646,159]<-9121


#마산합포구_오동동
fire[fire$id==83414,159]<-21681


#마산합포구_완월동
fire[fire$id==127531,159]<-10957


#마산합포구_월영동
fire[fire$id==82662,159]<-31288
fire[fire$id==100234,159]<-31187
fire[fire$id==78798,159]<-30531
fire[fire$id==121400,159]<-30445
fire[fire$id==70855,159]<-31288


#마산합포구_중앙동(1가+2가+3가 구분 안하고 합쳐서 입력함)
fire[fire$id==90849,159]<-8936
fire[fire$id==116272,159]<-8852
fire[fire$id==104620,159]<-8538
fire[fire$id==132799,159]<-8551
fire[fire$id==110909,159]<-9085
fire[fire$id==128442,159]<-8914
fire[fire$id==96149,159]<-9130
fire[fire$id==104521,159]<-9156
fire[fire$id==82523,159]<-8888
fire[fire$id==91624,159]<-9130
fire[fire$id==129740,159]<-9063
fire[fire$id==81514,159]<-8936
fire[fire$id==128055,159]<-9063
fire[fire$id==130642,159]<-9085
fire[fire$id==99510,159]<-8979
fire[fire$id==119087,159]<-8894
fire[fire$id==111004,159]<-8914
fire[fire$id==78983,159]<-8542



#마산합포구_진전면 
fire[fire$id==108929,159]<-4375

----------------------------------------------------------------------------
    
    #마산회원구_석전동
    __석전동 인구 데이터 불러오기__
setwd("C:/Users/공다영/Documents/공다영/동국대학교/비교과/비어플/project/김해시 화재예측/data")
suckjeon_set<-read.table("석전동인구.txt",header=T,sep="\t")

for(i in 2:length(suckjeon_set)){
    suckjeon_set[1,i]<-as.numeric(suckjeon_set[1,i])
    suckjeon_set[2,i]<-as.numeric(suckjeon_set[2,i])
}

for(i in 2:length(suckjeon_set)){
    suckjeon_set[3,i]<-suckjeon_set[1,i]+suckjeon_set[2,i]
}
suckjeon_set<-suckjeon_set[3,]

__경상남도 창원시 마산회원구 석전동 & 인구수 결측인 경우__
suckjeon<-which(fire$emd_nm=="경상남도 창원시 마산회원구 석전동" & is.na(fire$hm_cnt)==TRUE)

for(j in suckjeon){
    if(year(fire[j,1])==2014 & month(fire[j,1])==1){
        fire[j,159]<-suckjeon_set[1,2]
    }else if(year(fire[j,1])==2014 & month(fire[j,1])==2){
        fire[j,159]<-suckjeon_set[1,3]  
    }else if(year(fire[j,1])==2014 & month(fire[j,1])==3){
        fire[j,159]<-suckjeon_set[1,4] 
    }else if(year(fire[j,1])==2014 & month(fire[j,1])==4){
        fire[j,159]<-suckjeon_set[1,5] 
    }else if(year(fire[j,1])==2014 & month(fire[j,1])==5){
        fire[j,159]<-suckjeon_set[1,6] 
    }else if(year(fire[j,1])==2014 & month(fire[j,1])==6){
        fire[j,159]<-suckjeon_set[1,7] 
    }else if(year(fire[j,1])==2014 & month(fire[j,1])==7){
        fire[j,159]<-suckjeon_set[1,8] 
    }else if(year(fire[j,1])==2014 & month(fire[j,1])==8){
        fire[j,159]<-suckjeon_set[1,9] 
    }else if(year(fire[j,1])==2014 & month(fire[j,1])==9){
        fire[j,159]<-suckjeon_set[1,10] 
    }else if(year(fire[j,1])==2014 & month(fire[j,1])==10){
        fire[j,159]<-suckjeon_set[1,11] 
    }else if(year(fire[j,1])==2014 & month(fire[j,1])==11){
        fire[j,159]<-suckjeon_set[1,12] 
    }else if(year(fire[j,1])==2014 & month(fire[j,1])==12){
        fire[j,159]<-suckjeon_set[1,13] 
    }else if(year(fire[j,1])==2015 & month(fire[j,1])==1){
        fire[j,159]<-suckjeon_set[1,14] 
    }else if(year(fire[j,1])==2015 & month(fire[j,1])==2){
        fire[j,159]<-suckjeon_set[1,15] 
    }else if(year(fire[j,1])==2015 & month(fire[j,1])==3){
        fire[j,159]<-suckjeon_set[1,16] 
    }else if(year(fire[j,1])==2015 & month(fire[j,1])==4){
        fire[j,159]<-suckjeon_set[1,17] 
    }else if(year(fire[j,1])==2015 & month(fire[j,1])==5){
        fire[j,159]<-suckjeon_set[1,18] 
    }else if(year(fire[j,1])==2015 & month(fire[j,1])==6){
        fire[j,159]<-suckjeon_set[1,19] 
    }else if(year(fire[j,1])==2015 & month(fire[j,1])==7){
        fire[j,159]<-suckjeon_set[1,20] 
    }else if(year(fire[j,1])==2015 & month(fire[j,1])==8){
        fire[j,159]<-suckjeon_set[1,21] 
    }else if(year(fire[j,1])==2015 & month(fire[j,1])==9){
        fire[j,159]<-suckjeon_set[1,22] 
    }else if(year(fire[j,1])==2015 & month(fire[j,1])==10){
        fire[j,159]<-suckjeon_set[1,23] 
    }else if(year(fire[j,1])==2015 & month(fire[j,1])==11){
        fire[j,159]<-suckjeon_set[1,24] 
    }else if(year(fire[j,1])==2015 & month(fire[j,1])==12){
        fire[j,159]<-suckjeon_set[1,25] 
    }else if(year(fire[j,1])==2016 & month(fire[j,1])==1){
        fire[j,159]<-suckjeon_set[1,26] 
    }else if(year(fire[j,1])==2016 & month(fire[j,1])==2){
        fire[j,159]<-suckjeon_set[1,27] 
    }else if(year(fire[j,1])==2016 & month(fire[j,1])==3){
        fire[j,159]<-suckjeon_set[1,28] 
    }else if(year(fire[j,1])==2016 & month(fire[j,1])==4){
        fire[j,159]<-suckjeon_set[1,29] 
    }else if(year(fire[j,1])==2016 & month(fire[j,1])==5){
        fire[j,159]<-suckjeon_set[1,30] 
    }else if(year(fire[j,1])==2016 & month(fire[j,1])==6){
        fire[j,159]<-suckjeon_set[1,31] 
    }else if(year(fire[j,1])==2016 & month(fire[j,1])==7){
        fire[j,159]<-suckjeon_set[1,32] 
    }else if(year(fire[j,1])==2016 & month(fire[j,1])==8){
        fire[j,159]<-suckjeon_set[1,33] 
    }else if(year(fire[j,1])==2016 & month(fire[j,1])==9){
        fire[j,159]<-suckjeon_set[1,34] 
    }else if(year(fire[j,1])==2016 & month(fire[j,1])==10){
        fire[j,159]<-suckjeon_set[1,35] 
    }else if(year(fire[j,1])==2016 & month(fire[j,1])==11){
        fire[j,159]<-suckjeon_set[1,36] 
    }else if(year(fire[j,1])==2016 & month(fire[j,1])==12){
        fire[j,159]<-suckjeon_set[1,37] 
    }
}



#마산회원구_합성동
fire[fire$id==103967,159]<-18480
fire[fire$id==84983,159]<-18076
fire[fire$id==72186,159]<-19263
fire[fire$id==90142,159]<-17549



#마산회원구_회성동
fire[fire$id==135676,159]<-7584	
fire[fire$id==86916,159]<-7615
fire[fire$id==95728,159]<-7778
fire[fire$id==116143,159]<-7626
fire[fire$id==69831,159]<-7616

----------------------------------------------------------------------------
    
    #통영시 (*인구정보가 면,읍,동까지밖에 없어 '리'단위는 무시하고 넣음)
    fire[fire$id==132455,159]<-2075	
fire[fire$id==132181,159]<-2062
fire[fire$id==73157,159]<-13511
fire[fire$id==86412,159]<-5001	
fire[fire$id==97098,159]<-5001	
fire[fire$id==87879,159]<-2076
fire[fire$id==71671,159]<-13529
fire[fire$id==98417,159]<-2098	
fire[fire$id==122077,159]<-13237
fire[fire$id==112902,159]<-5232	
fire[fire$id==78349,159]<-5142
fire[fire$id==115615,159]<-2094
fire[fire$id==98353,159]<-5051
fire[fire$id==121615,159]<-5266	
fire[fire$id==77282,159]<-2061	
fire[fire$id==120133,159]<-3192


---------------------------------------------------------------------------
    ---------------------------------------------------------------------------
    
    
    
    #############데이터 없는 것들...################

#######진해구####### 
#가주동
fire[fire$id==91406,159]<-
    
    #남양동
    fire[fire$id==125121,159]<-
    
    #명동
    fire[fire$id==133405,159]<-
    
    #안골동
    fire[fire$id==75131,159]<-
    
    #연도동
    fire[fire$id==85606,159]<-
    fire[fire$id==120155,159]<-
    
    #용원동
    fire[fire$id==126369,159]<-
    fire[fire$id==78466,159]<-
    
    #행암동
    fire[fire$id==121913,159]<-
    
    ************가주동~ 행암동 인구 수가 나와있지 않음. 이것들을 조사된 진해구의 다른 '동'들의 평균값으로 대체할까요....???************
    
    
    
    ##########창원시#############
#마산합포구_월포동
fire[fire$id==75822,159]<-
    fire[fire$id==69477,159]<-
    fire[fire$id==105114,159]<-
    fire[fire$id==94593,159]<-
    fire[fire$id==85715,159]<-
    
    
    #마산합포구_장군동123
    fire[fire$id==75987,159]<-
    fire[fire$id==105965,159]<-
    fire[fire$id==107445,159]<-
    fire[fire$id==107132,159]<-
    fire[fire$id==94400,159]<-
    fire[fire$id==132668,159]<-
    fire[fire$id==89762,159]<-
    fire[fire$id==134600,159]<-
    fire[fire$id==135960,159]<-
    fire[fire$id==114059,159]<-
    fire[fire$id==96806,159]<-
    fire[fire$id==130842,159]<-
    fire[fire$id==75034,159]<-
    fire[fire$id==105514,159]<-
    
    
    #마산합포구_신월동 
    fire[fire$id==75583,159]<-
    fire[fire$id==86467,159]<-
    fire[fire$id==94643,159]<-
    fire[fire$id==112895,159]<-
    
    
    #마산합포구_신흥동
    fire[fire$id==89710,159]<-
    fire[fire$id==119606,159]<-
    fire[fire$id==85717,159]<-
    fire[fire$id==122429,159]<-
    fire[fire$id==69327,159]<-
    fire[fire$id==107417,159]<-
    
    
    #마산합포구-대성동1/2 
    fire[fire$id==104968,159]<-
    fire[fire$id==110572,159]<-
    fire[fire$id==116491,159]<-
    fire[fire$id==76871,159]<-
    fire[fire$id==110459,159]<-
    fire[fire$id==85732,159]<-
    
    *******위 마산합포구의 동들이 조사되지 않음. 조사된 마산합포구 다른 '동'들의 평균값으로 대체할까요..?************
    
    
    
    ##########사천시############
#실안동
fire[fire$id==92201,159]<-
    
    ********실안동이 조사되지 않음. 조사된 사천시의 다른 '동'들의 평균값으로 대체할까요..?************
    
    
    
    #########함안군#############
(함안군은 모두 2014년 칠원읍인데 칠원읍의 2014년 데이터 없음)

********함안군 2014년이 조사되지 않음. 함안군 칠원읍은 2015년부터 조사됨. 2014년이 조사된 함안군의 다른 읍들의 평균값으로 대체할까요*********
    
    
    
    ## code부여해 결측치 대체
    group <- aggregate(fire,by=list(fire$jmk,fire$rgnl_ar_nm),FUN=unique)
#aggregate(fire$dt_of_fr,by=list(fire$jmk,fire$rgnl_ar_nm),FUN=unique)

fire$code <- NA

for(i in 1:nrow(fire)){
    for(j in 1:nrow(group)){
        if((fire$jmk[i] == group[j, 1]) & (fire$rgnl_ar_nm[i] == group[j, 2])){
            fire$code[i] <- j
        }
    }
}
fire$code <- as.factor(fire$code)

sub<- select(fire, jmk, rgnl_ar_nm, code)

code_group <-  sub[-which(duplicated(sub$code)),]
length(code_group$code)
summary(group)

filter(fire,bldng_us=="")$code

#37
summary(filter(fire,code==37)$bldng_us)
filter(fire,bldng_us=="",code==37)$id #25개
fire$bldng_us[fire$bldng_us=="" & fire$code==37]<-"단독주택"
summary(filter(fire,code==37)$bldng_us)

#197
summary(filter(fire,code==197)$bldng_us)
filter(fire,bldng_us=="",code==197)$id #7개
fire$bldng_us[fire$bldng_us=="" & fire$code==197]<-"단독주택"
summary(filter(fire,code==197)$bldng_us)

#213
summary(filter(fire,code==213)$bldng_us)
filter(fire,bldng_us=="",code==213)$id #15개
fire$bldng_us[fire$bldng_us=="" & fire$code==213]<-"단독주택"
summary(filter(fire,code==213)$bldng_us)

#11 : 잡 & ""
#summary(filter(fire,code==11)$bldng_us)
#filter(fire,bldng_us=="",code==11)$id #2개
#fire$bldng_us[fire$bldng_us=="" & fire$code==11]<-""
#summary(filter(fire,code==11)$bldng_us)

filter(fire,bldng_us=="")$code

#171
summary(filter(fire,code==171)$bldng_us)
filter(fire,bldng_us=="",code==171)$id #2개
fire$bldng_us[fire$bldng_us=="" & fire$code==171]<-"단독주택"
summary(filter(fire,code==171)$bldng_us)

#169 잡	자연녹지지역 => 가장많은 건물용도 애매함 15 13 13...
#summary(filter(fire,code==169)$bldng_us)
#filter(fire,bldng_us=="",code==169)$id #1개
#fire$bldng_us[fire$bldng_us=="" & fire$code==169]<-""
#summary(filter(fire,code==169)$bldng_us)

#160
summary(filter(fire,code==160)$bldng_us)
filter(fire,bldng_us=="",code==160)$id #3개
fire$bldng_us[fire$bldng_us=="" & fire$code==160]<-"단독주택"
summary(filter(fire,code==160)$bldng_us)

filter(fire,bldng_us=="")$code

#65
summary(filter(fire,code==65)$bldng_us)
filter(fire,bldng_us=="",code==65)$id #1개
fire$bldng_us[fire$bldng_us=="" & fire$code==65]<-"동.식물 관련시설"
summary(filter(fire,code==65)$bldng_us)

#130
summary(filter(fire,code==130)$bldng_us)
filter(fire,bldng_us=="",code==130)$id #1개
fire$bldng_us[fire$bldng_us=="" & fire$code==130]<-"운동시설"
summary(filter(fire,code==130)$bldng_us)

filter(fire,bldng_us=="")$code

#198 
summary(filter(fire,code==198)$bldng_us)
filter(fire,bldng_us=="",code==198)$id #1개
fire$bldng_us[fire$bldng_us=="" & fire$code==198]<-"단독주택"
summary(filter(fire,code==198)$bldng_us)

#146 
summary(filter(fire,code==146)$bldng_us)
filter(fire,bldng_us=="",code==146)$id #4개
fire$bldng_us[fire$bldng_us=="" & fire$code==146]<-"단독주택"
summary(filter(fire,code==146)$bldng_us)

#141 
summary(filter(fire,code==141)$bldng_us)
filter(fire,bldng_us=="",code==141)$id #1개
fire$bldng_us[fire$bldng_us=="" & fire$code==141]<-"공장"
summary(filter(fire,code==141)$bldng_us)

#46  
summary(filter(fire,code==46)$bldng_us)
filter(fire,bldng_us=="",code==46)$id #11개
fire$bldng_us[fire$bldng_us=="" & fire$code==46]<-"공장"
summary(filter(fire,code==46)$bldng_us)

#4  
summary(filter(fire,code==4)$bldng_us)
filter(fire,bldng_us=="",code==4)$id #1개
fire$bldng_us[fire$bldng_us=="" & fire$code==4]<-"단독주택"
summary(filter(fire,code==4)$bldng_us)

#80  
summary(filter(fire,code==80)$bldng_us)
filter(fire,bldng_us=="",code==80)$id #1개
fire$bldng_us[fire$bldng_us=="" & fire$code==80]<-"단독주택"
summary(filter(fire,code==80)$bldng_us)

#148  
summary(filter(fire,code==148)$bldng_us)
filter(fire,bldng_us=="",code==148)$id #2개
fire$bldng_us[fire$id==122131& fire$code==148]<-"제1종근린생활시설"
fire$bldng_us[fire$id==130446& fire$code==148]<-"제1종근린생활시설"
summary(filter(fire,code==148)$bldng_us)

#36  
summary(filter(fire,code==36)$bldng_us)
filter(fire,bldng_us=="",code==36)$id #1개
fire$bldng_us[fire$bldng_us=="" & fire$code==36]<-"단독주택"
summary(filter(fire,code==36)$bldng_us)

#173
summary(filter(fire,code==173)$bldng_us)
filter(fire,bldng_us=="",code==173)$id #1개
fire$bldng_us[fire$bldng_us=="" & fire$code==173]<-"위험물저장및처리시설"
summary(filter(fire,code==173)$bldng_us)

#5   
summary(filter(fire,code==5)$bldng_us)
filter(fire,bldng_us=="",code==5)$id #1개
fire$bldng_us[fire$bldng_us=="" & fire$code==5]<-"단독주택"
summary(filter(fire,code==5)$bldng_us)

#121  
summary(filter(fire,code==121)$bldng_us)
filter(fire,bldng_us=="",code==121)$id #1개
fire$bldng_us[fire$bldng_us=="" & fire$code==121]<-"단독주택"
summary(filter(fire,code==121)$bldng_us)

#106 
summary(filter(fire,code==106)$bldng_us)
filter(fire,bldng_us=="",code==106)$id #1개
fire$bldng_us[fire$bldng_us=="" & fire$code==106]<-"동.식물 관련시설"
summary(filter(fire,code==106)$bldng_us)

#58  
summary(filter(fire,code==58)$bldng_us)
filter(fire,bldng_us=="",code==58)$id #1개
fire$bldng_us[fire$bldng_us=="" & fire$code==58]<-"단독주택"
summary(filter(fire,code==58)$bldng_us)

#39  
summary(filter(fire,code==39)$bldng_us)
filter(fire,bldng_us=="",code==39)$id #1개
fire$bldng_us[fire$bldng_us=="" & fire$code==39]<-"동.식물 관련시설"
summary(filter(fire,code==39)$bldng_us)

#45  
summary(filter(fire,code==45)$bldng_us)
filter(fire,bldng_us=="",code==45)$id #7개
fire$bldng_us[fire$bldng_us=="" & fire$code==45]<-"동.식물 관련시설"
summary(filter(fire,code==45)$bldng_us)


filter(fire,bldng_us=="")
#결측치 3개남음

######건물용도분류명 결측치
####건물용도명에 따라 분류명채우기

summary(fire$bldng_us_clssfctn)
#1762개 결측치
#공공용 공업용 기타 농수산용 문교사회용 상업용 주거용


#단독주택=>주거용
summary(filter(fire,bldng_us=="단독주택")$bldng_us_clssfctn)
fire$bldng_us_clssfctn[fire$bldng_us=="단독주택" & fire$bldng_us_clssfctn==""]<-"주거용"
summary(filter(fire,bldng_us=="단독주택")$bldng_us_clssfctn)

summary(fire$bldng_us[fire$bldng_us_clssfctn==""])


#제1종근린생활시설=>상업용
summary(filter(fire,bldng_us=="제1종근린생활시설")$bldng_us_clssfctn)
fire$bldng_us_clssfctn[fire$bldng_us=="제1종근린생활시설" & fire$bldng_us_clssfctn==""]<-"상업용"
summary(filter(fire,bldng_us=="제1종근린생활시설")$bldng_us_clssfctn)

#제2종근린생활시설=>상업용
summary(filter(fire,bldng_us=="제2종근린생활시설")$bldng_us_clssfctn)
fire$bldng_us_clssfctn[fire$bldng_us=="제2종근린생활시설" & fire$bldng_us_clssfctn==""]<-"상업용"
summary(filter(fire,bldng_us=="제2종근린생활시설")$bldng_us_clssfctn)


#동.식물 관련시설=>농수산용
summary(filter(fire,bldng_us=="동.식물 관련시설")$bldng_us_clssfctn)
fire$bldng_us_clssfctn[fire$bldng_us=="동.식물 관련시설" & fire$bldng_us_clssfctn==""]<-"농수산용"
summary(filter(fire,bldng_us=="동.식물 관련시설")$bldng_us_clssfctn)

#공장=>공업용
summary(filter(fire,bldng_us=="공장")$bldng_us_clssfctn)
fire$bldng_us_clssfctn[fire$bldng_us=="공장" & fire$bldng_us_clssfctn==""]<-"공업용"
summary(filter(fire,bldng_us=="공장")$bldng_us_clssfctn)

#창고시설=>기타
summary(filter(fire,bldng_us=="창고시설")$bldng_us_clssfctn)
fire$bldng_us_clssfctn[fire$bldng_us=="창고시설" & fire$bldng_us_clssfctn==""]<-"기타"
summary(filter(fire,bldng_us=="창고시설")$bldng_us_clssfctn)

summary(fire$bldng_us[fire$bldng_us_clssfctn==""])

#공공용시설 => 데이터 하나밖에 없는데 분류명이 결측이라 논리적 판단하에 공공용으로 넣음            
summary(filter(fire,bldng_us=="공공용시설")$bldng_us_clssfctn)
fire$bldng_us_clssfctn[fire$bldng_us=="공공용시설" & fire$bldng_us_clssfctn==""]<-"공공용"
summary(filter(fire,bldng_us=="공공용시설")$bldng_us_clssfctn)

#공동주택 =>주거용     
summary(filter(fire,bldng_us=="공동주택")$bldng_us_clssfctn)
fire$bldng_us_clssfctn[fire$bldng_us=="공동주택" & fire$bldng_us_clssfctn==""]<-"주거용"
summary(filter(fire,bldng_us=="공동주택")$bldng_us_clssfctn)

#관광휴게시설 => 문교사회용
summary(filter(fire,bldng_us=="관광휴게시설")$bldng_us_clssfctn)
fire$bldng_us_clssfctn[fire$bldng_us=="관광휴게시설" & fire$bldng_us_clssfctn==""]<-"문교사회용"
summary(filter(fire,bldng_us=="관광휴게시설")$bldng_us_clssfctn)

#교육연구및복지시설 => 문교사회용        
summary(filter(fire,bldng_us=="교육연구및복지시설")$bldng_us_clssfctn)
fire$bldng_us_clssfctn[fire$bldng_us=="교육연구및복지시설" & fire$bldng_us_clssfctn==""]<-"문교사회용"
summary(filter(fire,bldng_us=="교육연구및복지시설")$bldng_us_clssfctn)

#교육연구시설=> 문교사회용
summary(filter(fire,bldng_us=="교육연구시설")$bldng_us_clssfctn)
fire$bldng_us_clssfctn[fire$bldng_us=="교육연구시설" & fire$bldng_us_clssfctn==""]<-"문교사회용"
summary(filter(fire,bldng_us=="교육연구시설")$bldng_us_clssfctn)


#교정및군사시설  => 결측데이터밖에 없음      
#summary(filter(fire,bldng_us=="교정및군사시설")$bldng_us_clssfctn)
#fire$bldng_us_clssfctn[fire$bldng_us=="교정및군사시설" & fire$bldng_us_clssfctn==""]<-""
#summary(filter(fire,bldng_us=="교정및군사시설")$bldng_us_clssfctn)

#근린생활시설=>상업용
summary(filter(fire,bldng_us=="근린생활시설")$bldng_us_clssfctn)
fire$bldng_us_clssfctn[fire$bldng_us=="근린생활시설" & fire$bldng_us_clssfctn==""]<-"상업용"
summary(filter(fire,bldng_us=="근린생활시설")$bldng_us_clssfctn)

#노유자시설 => 문교사회용
summary(filter(fire,bldng_us=="노유자시설")$bldng_us_clssfctn)
fire$bldng_us_clssfctn[fire$bldng_us=="노유자시설" & fire$bldng_us_clssfctn==""]<-"문교사회용"
summary(filter(fire,bldng_us=="노유자시설")$bldng_us_clssfctn)

#setwd("C:/Users/me/Desktop/김해시 화재예측")
#write.csv(fire,file="fire_0204.csv",row.names=FALSE)

#fire<-read.csv("C:/Users/me/Desktop/김해시 화재예측/fire_0204.csv",encoding="utp-8")
#library(stats)
#library(dplyr)

#문화및집회시설 => 문교사회용
summary(filter(fire,bldng_us=="문화및집회시설")$bldng_us_clssfctn)
fire$bldng_us_clssfctn[fire$bldng_us=="문화및집회시설" & fire$bldng_us_clssfctn==""]<-"문교사회용"
summary(filter(fire,bldng_us=="문화및집회시설")$bldng_us_clssfctn)

#발전시설 => 공공용
summary(filter(fire,bldng_us=="발전시설")$bldng_us_clssfctn)
fire$bldng_us_clssfctn[fire$bldng_us=="발전시설" & fire$bldng_us_clssfctn==""]<-"공공용"
summary(filter(fire,bldng_us=="발전시설")$bldng_us_clssfctn)

#방송통신시설  =>공공용
summary(filter(fire,bldng_us=="방송통신시설")$bldng_us_clssfctn)
fire$bldng_us_clssfctn[fire$bldng_us=="방송통신시설" & fire$bldng_us_clssfctn==""]<-"공공용"
summary(filter(fire,bldng_us=="방송통신시설")$bldng_us_clssfctn)

#분뇨.쓰레기처리시설 => 기타           
summary(filter(fire,bldng_us=="분뇨.쓰레기처리시설")$bldng_us_clssfctn)
fire$bldng_us_clssfctn[fire$bldng_us=="분뇨.쓰레기처리시설" & fire$bldng_us_clssfctn==""]<-"기타"
summary(filter(fire,bldng_us=="분뇨.쓰레기처리시설")$bldng_us_clssfctn)

#수련시설 =>  문교사회용          
summary(filter(fire,bldng_us=="수련시설")$bldng_us_clssfctn)
fire$bldng_us_clssfctn[fire$bldng_us=="수련시설" & fire$bldng_us_clssfctn==""]<-"문교사회용"
summary(filter(fire,bldng_us=="수련시설")$bldng_us_clssfctn)

#숙박시설 =>상업용
summary(filter(fire,bldng_us=="숙박시설")$bldng_us_clssfctn)
fire$bldng_us_clssfctn[fire$bldng_us=="숙박시설" & fire$bldng_us_clssfctn==""]<-"상업용"
summary(filter(fire,bldng_us=="숙박시설")$bldng_us_clssfctn)

#업무시설 => 상업용
summary(filter(fire,bldng_us=="업무시설")$bldng_us_clssfctn)
fire$bldng_us_clssfctn[fire$bldng_us=="업무시설" & fire$bldng_us_clssfctn==""]<-"상업용"
summary(filter(fire,bldng_us=="업무시설")$bldng_us_clssfctn)

#운동시설 =>  문교사회용
summary(filter(fire,bldng_us=="운동시설")$bldng_us_clssfctn)
fire$bldng_us_clssfctn[fire$bldng_us=="운동시설" & fire$bldng_us_clssfctn==""]<-"문교사회용"
summary(filter(fire,bldng_us=="운동시설")$bldng_us_clssfctn)

#운수시설 => 상업용
summary(filter(fire,bldng_us=="운수시설")$bldng_us_clssfctn)
fire$bldng_us_clssfctn[fire$bldng_us=="운수시설" & fire$bldng_us_clssfctn==""]<-"상업용"
summary(filter(fire,bldng_us=="운수시설")$bldng_us_clssfctn)

#위락시설 => 상업용
summary(filter(fire,bldng_us=="위락시설")$bldng_us_clssfctn)
fire$bldng_us_clssfctn[fire$bldng_us=="위락시설" & fire$bldng_us_clssfctn==""]<-"상업용"
summary(filter(fire,bldng_us=="위락시설")$bldng_us_clssfctn)

#위험물저장및처리시설 => 상업용
summary(filter(fire,bldng_us=="위험물저장및처리시설")$bldng_us_clssfctn)
fire$bldng_us_clssfctn[fire$bldng_us=="위험물저장및처리시설" & fire$bldng_us_clssfctn==""]<-"상업용"
summary(filter(fire,bldng_us=="위험물저장및처리시설")$bldng_us_clssfctn)

#자동차관련시설 => 상업용
summary(filter(fire,bldng_us=="자동차관련시설")$bldng_us_clssfctn)
fire$bldng_us_clssfctn[fire$bldng_us=="자동차관련시설" & fire$bldng_us_clssfctn==""]<-"상업용"
summary(filter(fire,bldng_us=="자동차관련시설")$bldng_us_clssfctn)

#종교시설  => 문교사회용 
summary(filter(fire,bldng_us=="종교시설")$bldng_us_clssfctn)
fire$bldng_us_clssfctn[fire$bldng_us=="종교시설" & fire$bldng_us_clssfctn==""]<-"문교사회용"
summary(filter(fire,bldng_us=="종교시설")$bldng_us_clssfctn)

#파출소  데이터 1개
#summary(filter(fire,bldng_us=="파출소")$bldng_us_clssfctn)
#fire$bldng_us_clssfctn[fire$bldng_us=="파출소" & fire$bldng_us_clssfctn==""]<-""
#summary(filter(fire,bldng_us=="파출소")$bldng_us_clssfctn)

#판매및영업시설   데이터 1개
#summary(filter(fire,bldng_us=="판매및영업시설")$bldng_us_clssfctn)
#fire$bldng_us_clssfctn[fire$bldng_us=="판매및영업시설" & fire$bldng_us_clssfctn==""]<-""
#summary(filter(fire,bldng_us=="판매및영업시설")$bldng_us_clssfctn)


summary(fire$bldng_us_clssfctn)







group <- aggregate(fire,by=list(fire$bldng_us,fire$bldng_archtctr),FUN=unique)


fire$code2 <- NA

for(i in 1:nrow(fire)){
    for(j in 1:nrow(group)){
        if((fire$bldng_us[i] == group[j, 1]) & (fire$bldng_archtctr[i] == group[j, 2])){
            fire$code2[i] <- j
        }
    }
}
fire$code2 <- as.factor(fire$code2)
summary(fire$code2)

summary(group)


################################################################################################
## 모델 구축 _ 랜덤포레스트##

-------------------------------------
    ##### [1] 전체변수 포함한 모델 - rf1 #####
-------------------------------------
    
    # @@@@@@@@@ (1) 모델용 데이터프레임 생성 @@@@@@@@@ #
    
    
    
    ### 1. year_of_athrztn, year_of_fr의 변수는 삭제한다
    ### 이유 : 시간, 요일 등의 다른 날짜 파생변수는 매년 반복되지만 
    ### 2014년, 2015년과같은 년도는 한번 지나면 반복되는 변수가 아니기때문이다.
    ### id도 삭제!
    
    str(fire[,c(30)])
str(fire[,c(44,45)])

fire2 <- fire[,-c(30,44,45)] 

set.seed(123) 
indextrain2 <- createDataPartition(fire2$fr_yn, p = .7, list = F)
train<- fire2[ indextrain2, ]
test<- fire2[-indextrain2, ]

### test용 데이터셋은 타겟변수 fr_yn만을 포함한 test_y 
### 나머지 독립변수만을 포함한 test_x 로 분리한다.
test_x <- test[,c(2:62)]
test_y <- test[,c(1)]




# @@@@@@@@@ (2) 모델구축 @@@@@@@@@ #

set.seed(123)
rf1<-randomForest(fr_yn~.,data=train,na.action=na.omit,importance = TRUE)
rf1
## OOB : 19.91%
### OOB? : 부트스트랩을 통한 임의 중복추출 시 훈련 데이터 집합에 속하지 않는 
### 데이터의 실제 값과 각 트리로 부터 나온 예측 결과 사이의 오차
### 이 값이 낮을수록 좋음

# @@@@@@@@@ (3) 모델을 통한 predict + 수치평가 @@@@@@@@@ #


rf1_p <- predict(rf1, test_x, type = "class")

# Confusion Matrix
cf1<- confusionMatrix(rf1_p, test_y,positive = "Y")
cf1
### 화재발생여부 "Y"가 0이되도록 positive = "Y" 옵션 추가함 

###### f1 score 계산 
precision <- cf1$byClass['Pos Pred Value']

recall <- cf1$byClass['Sensitivity']

f_measure <- 2 * ((precision * recall) / (precision + recall))

f_measure 

### 정확도 80% / f1 score : 0.4543


# @@@@@@@@@ (4) 변수중요도 + 적합한 tree수 계산 @@@@@@@@@ #


importance(rf1)
varImpPlot(rf1, type=2, pch=19, col=1, cex=1, main="")

which.min(rf1$err.rate[, 1])  
### 적합한 트리수는 298개 ====> ntree = 298를 옵션으로 추가한 파생모델 rf1_2 생성


----------------------------------------------------------
    ##### [2] 전체변수 포함한 모델 - rf1_2 (ntree옵션 추가) #####
----------------------------------------------------------
    
    
    
    # @@@@@@@@@ (2) 모델구축 @@@@@@@@@ #
    
    set.seed(123)
rf1_2<-randomForest(fr_yn~.,data=train,na.action=na.omit,importance = TRUE, ntree=298)
rf1_2
## OOB : 19.89%
### rf1보다 OOB수치 감소함 


# @@@@@@@@@ (3) 모델을 통한 predict + 수치평가 @@@@@@@@@ #


rf1_p2 <- predict(rf1_2, test_x, type = "class")

# Confusion Matrix
cf2<- confusionMatrix(rf1_p2, test_y, positive = "Y")
cf2

###### f1 score 계산 
precision <- cf2$byClass['Pos Pred Value']

recall <- cf2$byClass['Sensitivity']

f_measure <- 2 * ((precision * recall) / (precision + recall))

f_measure 

### 정확도 79% / f1 score : 0.4531
### rf1 보다 정확도, f1 score 감소


# @@@@@@@@@ (4) 변수중요도 @@@@@@@@@ #


importance(rf1_2)
varImpPlot(rf1_2, type=2, pch=19, col=1, cex=1, main="")

imp_rf1_2 <-data.frame(importance(rf1_2))
imp_rf1_2 <- imp_rf1_2[c(order(imp_rf1_2$MeanDecreaseGini,decreasing=TRUE)),]
imp_rf1_2

quantile(imp_rf1_2$MeanDecreaseGini)



----------------------------------------------------------
    ##### [3] rf1_2에서 중요도 낮은 변수 삭제한 모델 - rf2 #####
----------------------------------------------------------
    ### rf2 : lw 변수전체, mlt_us_yn 삭제한 모델
    
    
    # @@@@@@@@@ (1) 모델용 데이터프레임 생성 @@@@@@@@@ #
    
    
    ### 변수중요도의 25% quantile인 4보다 작은 중요도를 가지는
    ### lw_, mlt_us_yn 변수 삭제
    
    str(fire2)
str(fire2[,c(15:28)])
str(fire2[,c(34)])

fire3 <- fire2[,-c(15:28,34)] 
str(fire3)

set.seed(123) 
indextrain3 <- createDataPartition(fire3$fr_yn, p = .7, list = F)
train2<- fire3[ indextrain3, ]
test2<- fire3[-indextrain3, ]

test_x2 <- test2[,c(2:47)]
test_y2 <- test2[,c(1)]


# @@@@@@@@@ (2) 모델구축 @@@@@@@@@ #

set.seed(123)
rf2<-randomForest(fr_yn~.,data=train2,na.action=na.omit,importance = TRUE)
rf2
## OOB : 20%
### rf1, rf1_2 보다 오류 큼

# @@@@@@@@@ (3) 모델을 통한 predict + 수치평가 @@@@@@@@@ #


rf2_p <- predict(rf2, test_x2, type = "class")

# Confusion Matrix
cf3<- confusionMatrix(rf2_p, test_y2,positive = "Y")
cf3

###### f1 score 계산 
precision <- cf3$byClass['Pos Pred Value']

recall <- cf3$byClass['Sensitivity']

f_measure <- 2 * ((precision * recall) / (precision + recall))

f_measure 

### 정확도 80% / f1 score : 0.4628
###### 오류수치가 가장 낮은건 아니지만 f1 score와 정확도가 가장 나음


# @@@@@@@@@ (4) 변수중요도 + 적합한 tree수 계산 @@@@@@@@@ #


importance(rf2)
varImpPlot(rf2, type=2, pch=19, col=1, cex=1, main="")

which.min(rf2$err.rate[, 1])  
### 적합한 트리수는 451개 ====> ntree = 451를 옵션으로 추가한 파생모델 rf2_2 생성


--------------------------------------------------------------------------
    ##### [4] rf2의 적합한 트리수 - rf2_2 (ntree옵션 추가) #####
--------------------------------------------------------------------------
    
    
    
    # @@@@@@@@@ (2) 모델구축 @@@@@@@@@ #
    
    set.seed(123)
rf2_2<-randomForest(fr_yn~.,data=train2,na.action=na.omit,importance = TRUE, ntree=451)
rf2_2
## OOB : 19.98% 



# @@@@@@@@@ (3) 모델을 통한 predict + 수치평가 @@@@@@@@@ #


rf2_p2 <- predict(rf2_2, test_x2, type = "class")

# Confusion Matrix
cf4<- confusionMatrix(rf2_p2, test_y2, positive = "Y")
cf4

###### f1 score 계산 
precision <- cf4$byClass['Pos Pred Value']

recall <- cf4$byClass['Sensitivity']

f_measure <- 2 * ((precision * recall) / (precision + recall))

f_measure 

### 정확도 80% / f1 score : 0.4642 
### 오류수치가 가장 낮은건 아니지만 지금까지 모델중에 f1 score가 가장 높음

# @@@@@@@@@ (4) 변수중요도 @@@@@@@@@ #


importance(rf2_2)
varImpPlot(rf2_2, type=2, pch=19, col=1, cex=1, main="")

imp_rf2_2 <-data.frame(importance(rf2_2))
imp_rf2_2 <- imp_rf2_2[c(order(imp_rf2_2$MeanDecreaseGini,decreasing=TRUE)),]
imp_rf2_2

quantile(imp_rf1_2$MeanDecreaseGini)

########################## rf2_2가 가장 좋은 모델 지금까지는 


----------------------------------------------------------
    ##### [5] rf2_2에서 중요도 낮은 변수 삭제한 모델 - rf3 #####
----------------------------------------------------------
    ### rf3 : rf2_2에서 gas/ele_all삭제
    
    
    # @@@@@@@@@ (1) 모델용 데이터프레임 생성 @@@@@@@@@ #
    
    
    ### gas/ele_all 변수 삭제
    
    
    str(fire3)
str(fire3[,c(28:37)])

fire4 <- fire3[,-c(28:37)]
str(fire4)


set.seed(123) 
indextrain4 <- createDataPartition(fire4$fr_yn, p = .7, list = F)
train3<- fire4[ indextrain4, ]
test3<- fire4[-indextrain4, ]

test_x3 <- test3[,c(2:37)]
test_y3 <- test3[,c(1)]


# @@@@@@@@@ (2) 모델구축 @@@@@@@@@ #

set.seed(123)
rf3<-randomForest(fr_yn~.,data=train3,na.action=na.omit,importance = TRUE)
rf3
## OOB : 20.09% (가장 큰 오류수치)

# @@@@@@@@@ (3) 모델을 통한 predict + 수치평가 @@@@@@@@@ #


rf3_p <- predict(rf3, test_x3, type = "class")

# Confusion Matrix
cf5<- confusionMatrix(rf3_p, test_y3,positive = "Y")
cf5

###### f1 score 계산 
precision <- cf5$byClass['Pos Pred Value']

recall <- cf5$byClass['Sensitivity']

f_measure <- 2 * ((precision * recall) / (precision + recall))

f_measure 

### 정확도 80% / f1 score : 0.4603
###### 오류수치가 가장 낮은건 아니지만 f1 score와 정확도가 가장 나음



# @@@@@@@@@ (4) 변수중요도 + 적합한 tree수 계산 @@@@@@@@@ #


importance(rf3)
varImpPlot(rf3, type=2, pch=19, col=1, cex=1, main="")

which.min(rf3$err.rate[, 1])  
### 적합한 트리수는 401개 ====> ntree = 401를 옵션으로 추가한 파생모델 rf3_2 생성



----------------------------------------------------------
    ##### [6] rf3에서 적합한 트리수 찾아서 - rf3_2 #####
----------------------------------------------------------
    
    
    
    # @@@@@@@@@ (2) 모델구축 @@@@@@@@@ #
    
    set.seed(123)
rf3_2<-randomForest(fr_yn~.,data=train3,na.action=na.omit,importance = TRUE, ntree=477)
rf3_2
## OOB : 20.0779



# @@@@@@@@@ (3) 모델을 통한 predict + 수치평가 @@@@@@@@@ #


rf3_p2 <- predict(rf3_2, test_x3, type = "class")

# Confusion Matrix
cf6<- confusionMatrix(rf3_p2, test_y3, positive = "Y")
cf6

###### f1 score 계산 
precision <- cf6$byClass['Pos Pred Value']

recall <- cf6$byClass['Sensitivity']

f_measure <- 2 * ((precision * recall) / (precision + recall))

f_measure 

### 정확도 80% / f1 score : 0.4596



--------------------------------------------------------------
    ################### 가장 좋은 모델: rf2_2 #######################
OOB : 19.98% / 정확도 : 80% / f1 score : 0.4642
--------------------------------------------------------------
    
    
    ## 모델 구축 _ GLM ##
    
    str(fire)
fire$month<-as.factor(fire$month) #월 변수는 factor형
str(fire)

#시간(hour1)은 num으로..?

#int형 : num으로 바꾸기
for(i in 1:65){
    if(class(fire[,i])=="integer"){fire[,i]<-as.numeric(fire[,i])}
}

str(fire)

#factor형 : 데이터 없는 레벨 지우기
for(i in 1:65){
    if(class(fire[,i])=="factor"){
        fire[,i]<-as.character(fire[,i])
        fire[,i]<-as.factor(fire[,i])
    }
}

#fr_yn을 0(N),1(Y)로 코딩
fire$fr_yn<-as.integer(fire$fr_yn)
fire$fr_yn[fire$fr_yn==1]<-0
fire$fr_yn[fire$fr_yn==2]<-1

str(fire)

#test,train set나누기
library(caret)
set.seed(123) 

indextrain <- createDataPartition(fire$fr_yn, p = .7, list = F)

train <- fire[ indextrain, ]
test  <- fire[-indextrain, ]

str(train)
colSums(is.na(train))

#####modeling전에 cor확인#####
###energy변수 선택
temp_fire<-fire[,-c(1,30)]
str(temp_fire[44:55])
energy_fire<-temp_fire[,c(44:55)]
str(energy_fire)

str(train[,c(46:57)])
energy_train<-train[,c(46:57)]
cor_energy1<-cor(energy_fire)

corrplot(cor_energy1, 
         method="shade", # 색 입힌 사각형
         addshade="all", # 상관관계 방향선 제시
         tl.col="red", # 라벨 색 지정
         tl.srt=30, # 위쪽 라벨 회전 각도
         diag=FALSE, # 대각선 값 미제시
         addCoef.col="black", # 상관계수 숫자 색
         order="FPC" # "FPC": First Principle Component
)

cor_energy2<-cor(energy_train)

corrplot(cor_energy2, method="shade", addshade="all", tl.col="red", tl.srt=30,
         diag=FALSE, addCoef.col="black", order="FPC" )


#####위 결과로 보아 energy변수끼리의 다중공선성이 예상됨
#####year로 정리한 5년치 변수는 서로 연관성이 매우 강해서(0.9이상인 것들도 반정도 되어서) all변수 두개를 쓰려고함(0.63)


####모델링####
##############
#id 제거 
##############
###1. hour1###
hour1_model1<-glm(fr_yn~bldng_us+bldng_archtctr+bldng_cnt+bldng_ar+ttl_ar+ttl_grnd_flr+ttl_dwn_flr+bldng_us_clssfctn
                  +tmprtr+prcpttn+wnd_spd+wnd_drctn+hmdt
                  +lw_13101010+lw_13101110+lw_13101210+lw_13101211+lw_13101310+lw_13101410+lw_13111010
                  +lw_13111110+lw_13121010+lw_13121011+lw_13131010+lw_13131110+lw_13141010+lw_13141011
                  +jmk+hm_cnt+fr_sttn_dstnc+fr_wthr_fclt_dstnc+fr_mn_cnt+mlt_us_yn
                  +cctv_dstnc+fr_wthr_fclt_in_100m+cctv_in_100m+tbc_rtl_str_dstnc+sft_emrgnc_bll_dstnc
                  +ahsm_dstnc+no_tbc_zn_dstnc+bldng_cnt_in_50m+year_of_athrztn+year_of_fr
                  +ele_engry_us_all+gas_engry_us_all+rgnl+lnd_us+emd_nm2+month+quarter+wday+hour1
                  ,family=binomial,data=train)
summary(hour1_model1)            
#AIC: 449312
hour1_model1$coefficients

#추정안되는 변수 제거 : lw_13101210 lw_13101211 lw_13121011 lw_13131110 lw_13141011 quarter
hour1_model2<-glm(fr_yn~bldng_us+bldng_archtctr+bldng_cnt+bldng_ar+ttl_ar+ttl_grnd_flr+ttl_dwn_flr+bldng_us_clssfctn
                  +tmprtr+prcpttn+wnd_spd+wnd_drctn+hmdt
                  +lw_13101010+lw_13101110+lw_13101310+lw_13101410+lw_13111010
                  +lw_13111110+lw_13121010+lw_13131010+lw_13141010
                  +jmk+hm_cnt+fr_sttn_dstnc+fr_wthr_fclt_dstnc+fr_mn_cnt+mlt_us_yn
                  +cctv_dstnc+fr_wthr_fclt_in_100m+cctv_in_100m+tbc_rtl_str_dstnc+sft_emrgnc_bll_dstnc
                  +ahsm_dstnc+no_tbc_zn_dstnc+bldng_cnt_in_50m+year_of_athrztn+year_of_fr
                  +ele_engry_us_all+gas_engry_us_all+rgnl+lnd_us+emd_nm2+month+wday+hour1
                  ,family=binomial,data=train)
summary(hour1_model2)
#AIC: 19594


#다중공선성 확인
library(car)
vif(hour1_model2)
#GVIF^(1/(2*Df)) >2 인 변수들 : bldng_ar ttl_ar bldng_us_clssfctn tmprtr 
#lw_13101010 lw_13101110 lw_13101310 lw_13101410 lw_13111010 lw_13111110 lw_13121010 lw_13131010
#fr_mn_cnt sft_emrgnc_bll_dstnc ahsm_dstnc  

#연속형 변수들 관계 확인
cor(fire$bldng_ar,fire$ttl_ar) #0.3315074
cor(train$bldng_ar,train$ttl_ar) #0.2502631

library(dplyr)
temp_fire2<-select(train,bldng_cnt,bldng_ar,ttl_ar,ttl_grnd_flr,ttl_dwn_flr
                   ,tmprtr,prcpttn,wnd_spd,wnd_drctn,hmdt,hm_cnt,fr_sttn_dstnc,fr_wthr_fclt_dstnc,fr_mn_cnt
                   ,cctv_dstnc,fr_wthr_fclt_in_100m,cctv_in_100m,tbc_rtl_str_dstnc,sft_emrgnc_bll_dstnc
                   ,ahsm_dstnc,no_tbc_zn_dstnc,bldng_cnt_in_50m,year_of_athrztn,year_of_fr
                   ,ele_engry_us_all,gas_engry_us_all,hour1)
cor_model2<-cor(temp_fire2)
corrplot(cor_model2, method =  "ellipse", addshade="all", tl.col="red", tl.srt=30,
         diag=FALSE, addCoef.col="black", order="FPC" )

##다중공선성 있는 변수들 중 0.5가 넘는 변수관계
#1. bldng_ar - 건물건축면적 : ele_energy_us_all
#2. sft_emrgnc_bll_dstnc - 안전 비상벨과의 최소 거리 : fr_mn_cnt - 관할 소방서 인원

##bldng_ar 제거
##bldng_us_clssfctn 은 bldng_us 의 상위 기준으로 두 변수 중 bldng_us 를 선택, bldng_us_clssfctn 제거
##tmprtr는 일단 유의미헤 보이는 변수이므로 제거하지 않겠음
##
##lw 변수들은 건축물 관련 성능 유지변수들이다. 따라서 서로 연관성이 높고 실제로도 다중공선성이 의심된다.
###그래서 그나마 GVIF^(1/(2*Df)) 값이 2 이하인 lw_13141010(지하층 피난구,피난계단의 성능 유지여부)를 남기고 모두 제거
###가능하다면 상관관계 plot첨부
###
##sft_emrgnc_bll_dstnc 와 fr_mn_cnt 중에 fr_mn_cnt의 GVIF^(1/(2*Df)) 값이 상대벅으로 매우 크므로 fr_mn_cnt 제거
##ahsm_dstnc(자동 심장 충격기와의 최소 거리) 은 다른변수와 크게 관련된 변수가 없었으나 그나마 fr_mn_cnt 와 관련성 있어보임
###위에서 이미 제거함

hour1_model3<-glm(fr_yn~bldng_us+bldng_archtctr+bldng_cnt+ttl_ar+ttl_grnd_flr+ttl_dwn_flr
                  +tmprtr+prcpttn+wnd_spd+wnd_drctn+hmdt+lw_13141010
                  +jmk+hm_cnt+fr_sttn_dstnc+fr_wthr_fclt_dstnc+mlt_us_yn
                  +cctv_dstnc+fr_wthr_fclt_in_100m+cctv_in_100m+tbc_rtl_str_dstnc+sft_emrgnc_bll_dstnc
                  +ahsm_dstnc+no_tbc_zn_dstnc+bldng_cnt_in_50m+year_of_athrztn+year_of_fr
                  +ele_engry_us_all+gas_engry_us_all+rgnl+lnd_us+emd_nm2+month+wday+hour1
                  ,family=binomial,data=train)
summary(hour1_model3)
#AIC: 19577

vif(hour1_model3)
#여전히 tmprtr sft_emrgnc_bll_dstnc ahsm_dstnc 는 2가 넘음

#tmprtr sft_emrgnc_bll_dstnc ahsm_dstnc 제거
hour1_model4<-glm(fr_yn~bldng_us+bldng_archtctr+bldng_cnt+ttl_ar+ttl_grnd_flr+ttl_dwn_flr
                  +prcpttn+wnd_spd+wnd_drctn+hmdt+lw_13141010
                  +jmk+hm_cnt+fr_sttn_dstnc+fr_wthr_fclt_dstnc+mlt_us_yn
                  +cctv_dstnc+fr_wthr_fclt_in_100m+cctv_in_100m+tbc_rtl_str_dstnc
                  +no_tbc_zn_dstnc+bldng_cnt_in_50m+year_of_athrztn+year_of_fr
                  +ele_engry_us_all+gas_engry_us_all+rgnl+lnd_us+emd_nm2+month+wday+hour1
                  ,family=binomial,data=train)
summary(hour1_model4)
#AIC: 19593


#jmk제거(level가장 많고 유의미한 레벨도 없어서) -> bldng_archtctr제거(한 레벨만 유의미해서), wday제거 (유의미한 레벨 없어서)
hour1_model4_1<-glm(fr_yn~bldng_us+bldng_cnt+ttl_ar+ttl_grnd_flr+ttl_dwn_flr
                    +prcpttn+wnd_spd+wnd_drctn+hmdt+lw_13141010
                    +hm_cnt+fr_sttn_dstnc+fr_wthr_fclt_dstnc+mlt_us_yn
                    +cctv_dstnc+fr_wthr_fclt_in_100m+cctv_in_100m+tbc_rtl_str_dstnc
                    +no_tbc_zn_dstnc+bldng_cnt_in_50m+year_of_athrztn+year_of_fr
                    +ele_engry_us_all+gas_engry_us_all+rgnl+lnd_us+emd_nm2+month+hour1
                    ,family=binomial,data=train)
summary(hour1_model4_1) #AIC: 19606

#->rgnl lw_13141010 lnd_us제거(한 레벨만 유의미해서) -> month 제거(한 레벨만 유의미해서) -> bldng_us emd_nm2 제거
hour1_model4_2<-glm(fr_yn~bldng_cnt+ttl_ar+ttl_grnd_flr+ttl_dwn_flr
                    +prcpttn+wnd_spd+wnd_drctn+hmdt+
                        +hm_cnt+fr_sttn_dstnc+fr_wthr_fclt_dstnc+mlt_us_yn
                    +cctv_dstnc+fr_wthr_fclt_in_100m+cctv_in_100m+tbc_rtl_str_dstnc
                    +no_tbc_zn_dstnc+bldng_cnt_in_50m+year_of_athrztn+year_of_fr
                    +ele_engry_us_all+gas_engry_us_all+hour1
                    ,family=binomial,data=train)
summary(hour1_model4_2) #AIC: 21093

#ttl_grnd_flr wnd_drctn hm_cnt fr_sttn_dstnc cctv_in_100m no_tbc_zn_dstnc bldng_cnt_in_50m year_of_fr 제거
hour1_model4_3<-glm(fr_yn~bldng_cnt+ttl_ar+ttl_dwn_flr+prcpttn+wnd_spd+hmdt++fr_wthr_fclt_dstnc+mlt_us_yn
                    +cctv_dstnc+fr_wthr_fclt_in_100m+tbc_rtl_str_dstnc+year_of_athrztn
                    +ele_engry_us_all+gas_engry_us_all+hour1 ,family=binomial,data=train)
summary(hour1_model4_3) #AIC: 21086

#model4에서 유의미하지 않은 변수들 모두 제거
hour1_model5<-glm(fr_yn~bldng_cnt+ttl_ar+ttl_dwn_flr+prcpttn+hmdt+fr_sttn_dstnc+mlt_us_yn+year_of_athrztn+hour1
                  ,family=binomial,data=train)
summary(hour1_model5)
#AIC: 22328

#유의미하지 않은 변수 ttl_dwn_flr 제거
hour1_model6<-glm(fr_yn~bldng_cnt+ttl_ar+prcpttn+hmdt+fr_sttn_dstnc+mlt_us_yn+year_of_athrztn+hour1
                  ,family=binomial,data=train)
summary(hour1_model6)
#AIC: 22326

###2. hour2###

hour2_model1<-glm(fr_yn~bldng_us+bldng_archtctr+bldng_cnt+bldng_ar+ttl_ar+ttl_grnd_flr+ttl_dwn_flr+bldng_us_clssfctn
                  +tmprtr+prcpttn+wnd_spd+wnd_drctn+hmdt
                  +lw_13101010+lw_13101110+lw_13101210+lw_13101211+lw_13101310+lw_13101410+lw_13111010
                  +lw_13111110+lw_13121010+lw_13121011+lw_13131010+lw_13131110+lw_13141010+lw_13141011
                  +jmk+hm_cnt+fr_sttn_dstnc+fr_wthr_fclt_dstnc+fr_mn_cnt+mlt_us_yn
                  +cctv_dstnc+fr_wthr_fclt_in_100m+cctv_in_100m+tbc_rtl_str_dstnc+sft_emrgnc_bll_dstnc
                  +ahsm_dstnc+no_tbc_zn_dstnc+bldng_cnt_in_50m+year_of_athrztn+year_of_fr
                  +ele_engry_us_all+gas_engry_us_all+rgnl+lnd_us+emd_nm2+month+quarter+wday+hour2
                  ,family=binomial,data=train)
summary(hour2_model1)            
#AIC: 547855
hour2_model1$coefficients

#추정안되는 변수 제거 : lw_13101210 lw_13101211 lw_13121011 lw_13131110 lw_13141011 quarter
hour2_model2<-glm(fr_yn~bldng_us+bldng_archtctr+bldng_cnt+bldng_ar+ttl_ar+ttl_grnd_flr+ttl_dwn_flr+bldng_us_clssfctn
                  +tmprtr+prcpttn+wnd_spd+wnd_drctn+hmdt
                  +lw_13101010+lw_13101110+lw_13101310+lw_13101410+lw_13111010
                  +lw_13111110+lw_13121010+lw_13131010+lw_13141010
                  +jmk+hm_cnt+fr_sttn_dstnc+fr_wthr_fclt_dstnc+fr_mn_cnt+mlt_us_yn
                  +cctv_dstnc+fr_wthr_fclt_in_100m+cctv_in_100m+tbc_rtl_str_dstnc+sft_emrgnc_bll_dstnc
                  +ahsm_dstnc+no_tbc_zn_dstnc+bldng_cnt_in_50m+year_of_athrztn+year_of_fr
                  +ele_engry_us_all+gas_engry_us_all+rgnl+lnd_us+emd_nm2+month+wday+hour2
                  ,family=binomial,data=train)
summary(hour2_model2)            
#AIC: 19528

#다중공선성 확인
vif(hour2_model2)
#GVIF^(1/(2*Df)) >2 인 변수들 : bldng_ar ttl_ar bldng_us_clssfctn tmprtr 
#lw_13101010 lw_13101110 lw_13101310 lw_13101410 lw_13111010 lw_13111110 lw_13121010 lw_13131010
#fr_mn_cnt sft_emrgnc_bll_dstnc ahsm_dstnc  

#hour1때와 같은 변수들 나옴. 같은처리
##다중공선성 있는 변수들 중 0.5가 넘는 변수관계
#1. bldng_ar - 건물건축면적 : ele_energy_us_all
#2. sft_emrgnc_bll_dstnc - 안전 비상벨과의 최소 거리 : fr_mn_cnt - 관할 소방서 인원

##bldng_ar 제거
##bldng_us_clssfctn 은 bldng_us 의 상위 기준으로 두 변수 중 bldng_us 를 선택, bldng_us_clssfctn 제거
##tmprtr는 일단 유의미헤 보이는 변수이므로 제거하지 않겠음
##
##lw 변수들은 건축물 관련 성능 유지변수들이다. 따라서 서로 연관성이 높고 실제로도 다중공선성이 의심된다.
###그래서 그나마 GVIF^(1/(2*Df)) 값이 2 이하인 lw_13141010(지하층 피난구,피난계단의 성능 유지여부)를 남기고 모두 제거
###가능하다면 상관관계 plot첨부
###
##sft_emrgnc_bll_dstnc 와 fr_mn_cnt 중에 fr_mn_cnt의 GVIF^(1/(2*Df)) 값이 상대벅으로 매우 크므로 fr_mn_cnt 제거
##ahsm_dstnc(자동 심장 충격기와의 최소 거리) 은 다른변수와 크게 관련된 변수가 없었으나 그나마 fr_mn_cnt 와 관련성 있어보임
###위에서 이미 제거함
hour2_model3<-glm(fr_yn~bldng_us+bldng_archtctr+bldng_cnt+ttl_ar+ttl_grnd_flr+ttl_dwn_flr
                  +tmprtr+prcpttn+wnd_spd+wnd_drctn+hmdt+lw_13141010
                  +jmk+hm_cnt+fr_sttn_dstnc+fr_wthr_fclt_dstnc+mlt_us_yn
                  +cctv_dstnc+fr_wthr_fclt_in_100m+cctv_in_100m+tbc_rtl_str_dstnc+sft_emrgnc_bll_dstnc
                  +ahsm_dstnc+no_tbc_zn_dstnc+bldng_cnt_in_50m+year_of_athrztn+year_of_fr
                  +ele_engry_us_all+gas_engry_us_all+rgnl+lnd_us+emd_nm2+month+wday+hour2
                  ,family=binomial,data=train)
summary(hour2_model3)            
#AIC: 19509
vif(hour2_model3)
#여전히 tmprtr sft_emrgnc_bll_dstnc ahsm_dstnc 는 2가 넘음

#tmprtr sft_emrgnc_bll_dstnc ahsm_dstnc 모두 제거
hour2_model4<-glm(fr_yn~bldng_us+bldng_archtctr+bldng_cnt+ttl_ar+ttl_grnd_flr+ttl_dwn_flr
                  +prcpttn+wnd_spd+wnd_drctn+hmdt+lw_13141010
                  +jmk+hm_cnt+fr_sttn_dstnc+fr_wthr_fclt_dstnc+mlt_us_yn
                  +cctv_dstnc+fr_wthr_fclt_in_100m+cctv_in_100m+tbc_rtl_str_dstnc
                  +no_tbc_zn_dstnc+bldng_cnt_in_50m+year_of_athrztn+year_of_fr
                  +ele_engry_us_all+gas_engry_us_all+rgnl+lnd_us+emd_nm2+month+wday+hour2
                  ,family=binomial,data=train)
summary(hour2_model4)  
#AIC: 19510

#jmk제거(level가장 많고 유의미한 레벨도 없어서) -> bldng_archtctr제거(한 레벨만 유의미해서), wday제거 (유의미한 레벨 없어서)
hour2_model4_1<-glm(fr_yn~bldng_us+bldng_cnt+ttl_ar+ttl_grnd_flr+ttl_dwn_flr
                    +prcpttn+wnd_spd+wnd_drctn+hmdt+lw_13141010
                    +hm_cnt+fr_sttn_dstnc+fr_wthr_fclt_dstnc+mlt_us_yn
                    +cctv_dstnc+fr_wthr_fclt_in_100m+cctv_in_100m+tbc_rtl_str_dstnc
                    +no_tbc_zn_dstnc+bldng_cnt_in_50m+year_of_athrztn+year_of_fr
                    +ele_engry_us_all+gas_engry_us_all+rgnl+lnd_us+emd_nm2+month+hour2
                    ,family=binomial,data=train)
summary(hour2_model4_1) #AIC: 19522

#->rgnl lw_13141010 lnd_us제거(한 레벨만 유의미) -> emd_nm2 제거(유의미한 레벨 비율 가장 낮음) -> bldng_us month 제거
hour2_model4_2<-glm(fr_yn~bldng_cnt+ttl_ar+ttl_grnd_flr+ttl_dwn_flr
                    +prcpttn+wnd_spd+wnd_drctn+hmdt
                    +hm_cnt+fr_sttn_dstnc+fr_wthr_fclt_dstnc+mlt_us_yn
                    +cctv_dstnc+fr_wthr_fclt_in_100m+cctv_in_100m+tbc_rtl_str_dstnc
                    +no_tbc_zn_dstnc+bldng_cnt_in_50m+year_of_athrztn+year_of_fr
                    +ele_engry_us_all+gas_engry_us_all+hour2
                    ,family=binomial,data=train)
summary(hour2_model4_2) #AIC: 21001

#ttl_grnd_flr prcpttn wnd_drctn hm_cnt fr_sttn_dstnc cctv_in_100m no_tbc_zn_dstnc bldng_cnt_in_50m year_of_fr 제거
hour2_model4_3<-glm(fr_yn~bldng_cnt+ttl_ar+ttl_dwn_flr+wnd_spd+hmdt+fr_wthr_fclt_dstnc+mlt_us_yn
                    +cctv_dstnc+fr_wthr_fclt_in_100m+tbc_rtl_str_dstnc+year_of_athrztn
                    +ele_engry_us_all+gas_engry_us_all+hour2,family=binomial,data=train)
summary(hour2_model4_3) #AIC: 20996


#model4에서 유의미하지 않은 변수들 모두 제거
hour2_model5<-glm(fr_yn~bldng_cnt+ttl_ar+ttl_dwn_flr+prcpttn+hmdt+fr_sttn_dstnc+mlt_us_yn+year_of_athrztn+hour2
                  ,family=binomial,data=train)
summary(hour2_model5)  
#AIC: 22219

#model5에서 유의미하지 않은 변수 ttl_dwn_flr 제거
hour2_model6<-glm(fr_yn~bldng_cnt+ttl_ar+prcpttn+hmdt+fr_sttn_dstnc+mlt_us_yn+year_of_athrztn+hour2
                  ,family=binomial,data=train)
summary(hour2_model6)  
#AIC: 22216


############f1 score############
library(ROCR)

f1score<-function(y_test_data,fire_pred,prob){
    pred<-ifelse(fire_pred > prob, 1, 0)
    confusion_matrix = table(y_test_data,pred)
    #cat(confusion_matrix,"\n","\n")
    
    accuracy = sum(diag(confusion_matrix))/sum(confusion_matrix)
    Precision = confusion_matrix[2,2]/sum(confusion_matrix[2,])
    Recall = confusion_matrix[2,2]/sum(confusion_matrix[,2])
    f1score = 2*(Precision*Recall)/(Precision+Recall)
    cat("accuracy: ",accuracy,"\nPrecision: ",Precision,"\nRecall: ",Recall,"\n")
    cat("F1-score: ",f1score,"\n")
    
    error_rate = 1 - accuracy
    cat("error_rate:",error_rate,"\n")
    
    auc = performance(prediction(pred,y_test_data),measure = "auc")
    cat("AUC: ", auc@y.values[[1]])   
}

y_true<-test$fr_yn
samplep<-2265/(7217+2265)

##########f1 score hour1_model6##########
y_pred<-predict(hour1_model6,test,type ="response")
#cut off => 표본비율
f1score(y_true,y_pred,samplep)
#0.4652318 

##########f1 score hour1_model5##########
y_pred2<-predict(hour1_model5,test,type ="response")
#cut off => 표본비율
f1score(y_true,y_pred2,samplep)
#0.4651317 

##########f1 score hour1_model4(다중공선성 까지만 처리)##########
hour1_model4$xlevels[["bldng_us"]] <- union(hour1_model4$xlevels[["bldng_us"]], levels(test$bldng_us))
hour1_model4$xlevels[["bldng_archtctr"]] <- union(hour1_model4$xlevels[["bldng_archtctr"]], levels(test$bldng_archtctr))
hour1_model4$xlevels[["jmk"]] <- union(hour1_model4$xlevels[["jmk"]], levels(test$jmk))

y_pred3<-predict(hour1_model4,test,type ="response")
#cut off => 표본비율
f1score(y_true,y_pred3,samplep)
#0.3701759  

##########f1 score hour1_model4_3##########
y_pred3_2<-predict(hour1_model4_3,test,type ="response")
#cut off => 표본비율
f1score(y_true,y_pred3_2,samplep)
#0.4928672 


##########f1 score hour2_model6##########
y_pred4<-predict(hour2_model6,test,type ="response")
#cut off => 표본비율
f1score(y_true,y_pred4,samplep)
#0.4666878 

##########f1 score hour2_model5##########
y_pred5<-predict(hour2_model5,test,type ="response")
#cut off => 표본비율
f1score(y_true,y_pred5,samplep)
#0.4671741  

##########f1 score hour2_model4##########
hour2_model4$xlevels[["bldng_us"]] <- union(hour2_model4$xlevels[["bldng_us"]], levels(test$bldng_us))
hour2_model4$xlevels[["bldng_archtctr"]] <- union(hour2_model4$xlevels[["bldng_archtctr"]], levels(test$bldng_archtctr))
hour2_model4$xlevels[["jmk"]] <- union(hour2_model4$xlevels[["jmk"]], levels(test$jmk))

y_pred6<-predict(hour2_model4,test,type ="response")
#cut off => 표본비율
f1score(y_true,y_pred6,samplep)
#0.3703639  

##########f1 score hour2_model4_3##########
y_pred6_2<-predict(hour2_model4_3,test,type ="response")
#cut off => 표본비율
f1score(y_true,y_pred6_2,samplep)
#0.4973188