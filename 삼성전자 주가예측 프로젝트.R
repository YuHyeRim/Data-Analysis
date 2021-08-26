library(lubridate)
library(httr)
library(rvest)
library(xts)

#################### 삼성전자 주가데이터 크롤링

tick = "005930"
url = paste0("https://fchart.stock.naver.com/sise.nhn?symbol="
             ,tick,"&timeframe=day&count=1000&requestType=0")



data = GET(url) %>%
    read_html %>%
    html_nodes("item") %>%
    html_attr("data") %>%
    strsplit("\\|")



data = lapply(data, function(x) {
    x[c(1, 5)] %>% t() %>% data.frame()
})

data = do.call(rbind, data)

data[,2] = as.numeric(as.character(data[,2]))
rownames(data) = ymd(data[,1]) %>% as.character
data[,1] = NULL
data = as.xts(data)

plot(data)



#################### 데이터 분석 수행

library(astsa)
library(fGarch)
library(lubridate)
library(MTS)
library(zoo)
library(rugarch)
library(FinTS)
library(ggplot2)

samsung <- read.csv('C:/Users/yhr63/Desktop/혦/2020년/학교/2학기/시계열해석/과제/프로젝트/데이터/samsung.csv')

samsung2<-zoo(samsung$price, as.Date(as.character(samsung$date),
                                     format = c("%Y-%m-%d")))

lnprice = log(samsung2)
return <- diff(lnprice)


### 이분산성 검정
ArchTest(return, lags=12, demean = FALSE)
# 1% 유의수준 하에서 이분산성이 존재함을 알 수 있다.

### 수익률 히스토그램
hist(return)

###### GARCH(1,1) 

# egarch , std , ARMA(1,1) + eGARCH(1,1), noint
garch1 <- ugarchspec(variance.model = list(model = 'eGARCH',garchOrder = c(1,1)),
                     mean.model = list(include.mean = FALSE),distribution = 'std')

garch1fit <- ugarchfit(spec = garch1, data = return)

garch1fit
# AIC = -5.3911 / 우도 = 2699.865
# Pearson Goodness test가 유의함

# egarch, std, ARMA(1,1)+eGARCH(1,1), int O
garch1_2 <- ugarchspec(variance.model = list(model = 'eGARCH',garchOrder = c(1,1)),
                       distribution = 'std')

garch1_2fit <- ugarchfit(spec = garch1_2, data = return)

garch1_2fit
# AIC = -5.3907 / 2700.642
# Pearson Goodness test가 유의함

# egarch, (0,0), noint
garch2 <- ugarchspec(variance.model = list(model = 'eGARCH',garchOrder = c(1,1)),
                     mean.model = list(armaOrder = c(0,0),include.mean = FALSE),distribution = 'std')

garch2fit <- ugarchfit(spec = garch2, data = return)

garch2fit
# AIC : -5.3849 / 3693.765
# Pearson Goodness test가 유의하지 않음

# egarch, (0,0), int O
garch2_2 <- ugarchspec(variance.model = list(model = 'eGARCH',garchOrder = c(1,1)),
                       mean.model = list(armaOrder = c(0,0)),distribution = 'std')

garch2_2fit <- ugarchfit(spec = garch2_2, data = return)

garch2_2fit
# AIC = -5.3942 / 2700.425
# Pearson Goodness test가 유의하다고 할 수 있음



###### ARMA(1,0) + GARCH(1,1) 

# eGARCH, (1,0), noint
garch3 <- ugarchspec(variance.model = list(model = 'eGARCH',garchOrder = c(1,1)),
                     mean.model = list(armaOrder = c(1,0),include.mean = FALSE),distribution = 'std')

garch3fit <- ugarchfit(spec = garch3, data = return)

garch3fit
# AIC =  -5.3930
# Pesrson Goodness test가 유의함


# eGARCH, (1,0), int O
garch3_2 <- ugarchspec(variance.model = list(model = 'eGARCH',garchOrder = c(1,1)),
                       mean.model = list(armaOrder = c(1,0)),distribution = 'std')

garch3_2fit <- ugarchfit(spec = garch3_2, data = return)

garch3_2fit
# AIC = -5.3926 
# Pearson Goodness test가 유의함


###### ARMA(0,1) + GARCH(1,1) 

# eGARCH, (0,1), noint
garch4 <- ugarchspec(variance.model = list(model = 'eGARCH',garchOrder = c(1,1)),
                     mean.model = list(armaOrder = c(0,1),include.mean = FALSE),distribution = 'std')

garch4fit <- ugarchfit(spec = garch4, data = return)

garch4fit

# AIC = -5.3930
# Pearson Goodness test 유의함

# eGARCH, (0,1), int O
garch4_2 <- ugarchspec(variance.model = list(model = 'eGARCH',garchOrder = c(1,1)),
                       mean.model = list(armaOrder = c(0,1)),distribution = 'std')

garch4_2fit <- ugarchfit(spec = garch4_2, data = return)

garch4_2fit

# AIC = -5.3926
# Pearson Goodness test가 유의함

###### ARMA(0,0)+GARCH(2,1) 

# eGARCH, (0,0), noint
garch5 <- ugarchspec(variance.model = list(model = 'eGARCH',garchOrder = c(2,1)),
                     mean.model = list(armaOrder = c(0,0),include.mean = FALSE),distribution = 'std')

garch5fit <- ugarchfit(spec = garch5, data = return)

garch5fit

# AIC =  -5.3911
# Pearson Goodness test가 유의하지않음

# eGARCH, (0,0), int O
garch5_2 <- ugarchspec(variance.model = list(model = 'eGARCH',garchOrder = c(2,1)),
                       mean.model = list(armaOrder = c(0,0)),distribution = 'std')

garch5_2fit <- ugarchfit(spec = garch5_2, data = return)

garch5_2fit

# AIC = -5.3908
# Pearson Goodness test 가 유의함



###### ARMA(1,0)+GARCH(2,1) 

# eGARCH, (1,0), noint
garch6 <- ugarchspec(variance.model = list(model = 'eGARCH',garchOrder = c(2,1)),
                     mean.model = list(armaOrder = c(1,0),include.mean = FALSE),distribution = 'std')

garch6fit <- ugarchfit(spec = garch6, data = return,out.sample = 20)

garch6fit

# AIC = -5.3879
# Pearson Goodness test가 유의함

# eGARCH, (1,0), int O
garch6_2 <- ugarchspec(variance.model = list(model = 'eGARCH',garchOrder = c(2,1)),
                       mean.model = list(armaOrder = c(1,0)),distribution = 'std')

garch6_2fit <- ugarchfit(spec = garch6_2, data = return,out.sample = 20)

garch6_2fit

# AIC = -5.3873
# Pearson Goodness test가 유의함

###### ARMA(0,1) + GARCH(2,1) 

# eGARCH, (0,1), noint
garch7 <- ugarchspec(variance.model = list(model = 'eGARCH',garchOrder = c(2,1)),
                     mean.model = list(armaOrder = c(0,1),include.mean = FALSE),distribution = 'std')

garch7fit <- ugarchfit(spec = garch7, data = return)

garch7fit

# AIC = -5.3894
# Pearson Goodness test가 유의함


# eGARCH, (0,1), int O
garch7_2 <- ugarchspec(variance.model = list(model = 'eGARCH',garchOrder = c(2,1)),
                       mean.model = list(armaOrder = c(0,1)),distribution = 'std')

garch7_2fit <- ugarchfit(spec = garch7_2, data = return)

garch7_2fit

# AIC = -5.3890
# Pearson Goodness test 유의함


###### ARMA(1,1) + GARCH(2,1) 

# eGARCH, (1,1), noint
garch8 <- ugarchspec(variance.model = list(model = 'eGARCH',garchOrder = c(2,1)),
                     mean.model = list(armaOrder = c(0,1),include.mean = FALSE),distribution = 'std')

garch8fit <- ugarchfit(spec = garch8, data = return)

garch8fit

# AIC = -5.3894
# Pearson Goodness test 유의함


# eGARCH, (1,1), int O
garch8_2 <- ugarchspec(variance.model = list(model = 'eGARCH',garchOrder = c(2,1)),
                       mean.model = list(armaOrder = c(0,1)),distribution = 'std')

garch8_2fit <- ugarchfit(spec = garch8_2, data = return)

garch8_2fit

# AIC = -5.3890
# Pearson Goodness test 유의함

###### ARMA(0,0) + GARCH(1,2)

# eGARCH, (0,0), noint
garch9 <- ugarchspec(variance.model = list(model = 'eGARCH',garchOrder = c(1,2)),
                     mean.model = list(armaOrder = c(0,0),include.mean = FALSE),distribution = 'std')

garch9fit <- ugarchfit(spec = garch9, data = return)

garch9fit

# AIC = -5.3931
# Pearson Goodness test 유의하지 않음

# eGARCH, (0,0), int O
garch9_2 <- ugarchspec(variance.model = list(model = 'eGARCH',garchOrder = c(1,2)),
                       mean.model = list(armaOrder = c(0,0)),distribution = 'std')

garch9_2fit <- ugarchfit(spec = garch9_2, data = return)

garch9_2fit

# AIC = -5.3931
# Pearson Goodness test 유의한편

###### ARMA(1,0) + GARCH(1,2)

# eGARCH, (1,0), noint
garch10 <- ugarchspec(variance.model = list(model = 'eGARCH',garchOrder = c(1,2)),
                      mean.model = list(armaOrder = c(1,0),include.mean = FALSE),distribution = 'std')

garch10fit <- ugarchfit(spec = garch10, data = return)

garch10fit

# AIC = -5.3919
# Pearson Goondess test 유의한편 

# eGARCH, (1,0), int O
garch10_2 <- ugarchspec(variance.model = list(model = 'eGARCH',garchOrder = c(1,2)),
                        mean.model = list(armaOrder = c(1,0)),distribution = 'std')

garch10_2fit <- ugarchfit(spec = garch10_2, data = return)

garch10_2fit

# AIC = -5.3917
# Pearson Goodness test 유의한편


###### ARMA(0,1) + GARCH(1,2)

# eGARCH, (0,1), noint
garch11 <- ugarchspec(variance.model = list(model = 'eGARCH',garchOrder = c(1,2)),
                      mean.model = list(armaOrder = c(0,1),include.mean = FALSE),distribution = 'std')

garch11fit <- ugarchfit(spec = garch11, data = return)

garch11fit

# AIC = -5.3919
# Pearson Goodness test 유의한편

# eGARCH, (0,1), int O
garch11_2 <- ugarchspec(variance.model = list(model = 'eGARCH',garchOrder = c(1,2)),
                        mean.model = list(armaOrder = c(0,1)),distribution = 'std')

garch11_2fit <- ugarchfit(spec = garch11_2, data = return)

garch11_2fit

# AIC = -5.3916
# Pearson Goodness test 유의한편


###### ARMA(1,1) + GARCH(1,2)

# eGARCH, (1,1), noint
garch12 <- ugarchspec(variance.model = list(model = 'eGARCH',garchOrder = c(1,2)),
                      mean.model = list(armaOrder = c(1,1),include.mean = FALSE),distribution = 'std')

garch12fit <- ugarchfit(spec = garch12, data = return)

garch12fit

# AIC = -5.3901
# Pearson Goodness test 유의한 편


# eGARCH, (1,1), int O

garch12_2 <- ugarchspec(variance.model = list(model = 'eGARCH', garchOrder = c(1,2)),
                        mean.model = list(armaOrder = c(1,1)),distribution = 'std')

garch12_2fit <- ugarchfit(spec = garch12_2, data = return)

garch12_2fit

# AIC = -5.3898
# Pearson Goodness test 유의함



###### ARMA(0,0) + GARCH(2,2)

# eGARCH, (0,0), noint
garch13 <- ugarchspec(variance.model = list(model = 'eGARCH',garchOrder = c(2,2)),
                      mean.model = list(armaOrder = c(0,0),include.mean = FALSE),distribution = 'std')

garch13fit <- ugarchfit(spec = garch13, data = return)

garch13fit

# AIC = -5.3912
# Pearson Goodness test 유의하지 않음

# eGARCH, (0,0) , int O
garch13_2 <- ugarchspec(variance.model = list(model = 'eGARCH', garchOrder = c(2,2)),
                        mean.model = list(armaOrder = c(0,0)),distribution = 'std')

garch13_2fit <- ugarchfit(spec = garch13_2, data = return)

garch13_2fit

# AIC = -5.3910
# Pearson Goodness test 유의함


###### ARMA(1,0) + GARCH(2,2)

# eGARCH, (1,0), noint
garch14 <- ugarchspec(variance.model = list(model = 'eGARCH',garchOrder = c(2,2)),
                      mean.model = list(armaOrder = c(1,0),include.mean = FALSE),distribution = 'std')

garch14fit <- ugarchfit(spec = garch14, data = return)

garch14fit

# AIC = -5.3898
# Pearson Goodness test 유의함

# eGARCH, (1,0), int O

garch14_2 <- ugarchspec(variance.model = list(model = 'eGARCH',garchOrder = c(2,2)),
                        mean.model = list(armaOrder = c(1,0)),distribution = 'std')

garch14_2fit <- ugarchfit(spec = garch14_2, data = return)

garch14_2fit

# AIC = -5.3894
# Pearson Goodness test 유의함


###### ARMA(0,1) + GARCH(2,2)

# eGARCH, (0,1), noint
garch15 <- ugarchspec(variance.model = list(model = 'eGARCH',garchOrder = c(2,2)),
                      mean.model = list(armaOrder = c(0,1),include.mean = FALSE),distribution = 'std')

garch15fit <- ugarchfit(spec = garch15, data = return)

garch15fit



# AIC = -5.3898
# Pearson Goodness test 유의함

# eGARCH, (0,1), int O
garch15_2 <- ugarchspec(variance.model = list(model = 'eGARCH',garchOrder = c(2,2)),
                        mean.model = list(armaOrder = c(0,1)),distribution = 'std')

garch15_2fit <- ugarchfit(spec = garch15_2, data = return)

garch15_2fit

# AIC = -5.3894
# Pearson Goodness test 유의함



###### ARMA(1,1) + GARCH(2,2)

# eGARCH, (1,1), noint
garch16 <- ugarchspec(variance.model = list(model = 'eGARCH',garchOrder = c(2,2)),
                      mean.model = list(armaOrder = c(1,1),include.mean = FALSE),distribution = 'std')

garch16fit <- ugarchfit(spec = garch16, data = return)

garch16fit

# AIC = -5.3879
# Pearson Goodness test 유의함

# eGARCH, (1,1), int O
garch16_2 <- ugarchspec(variance.model = list(model = 'eGARCH',garchOrder = c(2,2)),
                        mean.model = list(armaOrder = c(1,1)),distribution = 'std')

garch16_2fit <- ugarchfit(spec = garch16_2, data = return)

garch16_2fit

# AIC = -5.3874
# Pearson Goodness test 유의함

########################### 최종 선택모형 : 절편있는 ARMA(0,0) + EGARCH(1,1)

###### 모델 성능 확인
# 모델의 성능을 확인하는 방법은 히스토리컬 백테스트
# 리스크 모형 백테스트에서는 추정된 VaR과 실제 수익률을 비교함
# 실제 수익률이 VaR보다 더 낮으면 최대예상손실액 초과가 있음을 의미한다.


# garch2_2
garch2_2_roll <- ugarchroll(garch2_2,data = return, n.start = 120, refit.every = 1, refit.window='recursive')

report(garch2_2_roll, type = 'VaR', VaR.alpha = 0.01, conf.level = 0.99)
# 초과값들이 정확하며 독립이라는 귀무가설을 기각할 수 없음.





## plot 그려보기

return_var <- zoo(garch2_2_roll@forecast$VaR[,1])
return_actual <- zoo(garch2_2_roll@forecast$VaR[,2])


plot(return_actual, type = 'b', main = '99% 1 day VaR Backtesting',
     xlab = 'Date', ylab = 'Return/VaR in percent')
lines(return_var, col = 'red')
legend('topright', inset = 0.01, c('Return','VaR'), col = c('black','red'),
       lty = c(1,1))





##### 최종모형 plot
# plot(garch2_2fit)

##### 잔차 검정
## Ljung Box test
# 귀무가설 : 잔차가 백색잡음과정을 따른다.
# 결과 : 유의확률이 0.05보다 크므로 귀무가설을 기각하지 않으므로 잔차가 백색잡음과정을 따른다고 할 수 있다.

## 정규성 검정
# plot통해 확인



####### garch2_2로 검사
# VaR 예측

#garch2_2
garch2_2forecast <- ugarchforecast(garch2_2fit, n.head = 10)
garch2_2forecast


qt(0.01,10)* 0.01458
qt(0.01,10)* 0.01464
qt(0.01,10)* 0.01469
qt(0.01,10)* 0.01475
qt(0.01,10)* 0.01480
qt(0.01,10)* 0.01486
qt(0.01,10)* 0.01491
qt(0.01,10)* 0.01496
qt(0.01,10)* 0.01500
qt(0.01,10)* 0.01505

# t분포의 자유도는 10 (shape)
# 99%의 확률로 수익률은 -4% 이상





