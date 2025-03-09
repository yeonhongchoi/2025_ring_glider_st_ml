library(dlookr)
library(readxl)
library(dplyr)
library(tidyverse)
library(inspectdf)
library(caret)
library(rsample)
# 자판연습 충분
# 시험환경 연습 필요, 
# 시험 권장 코딩 도 해보기

#### 0. 탐색적 데이터 ####
rdata <- read_excel("고리비행기.xlsx")
str(rdata)
colnames(rdata)
summary(rdata)
is.na(rdata)
rdata %>% inspect_na() %>% show_plot()

#### 1. 데이터 정제, 전처리 #####

rdata<- rdata %>%  rename(고리간격=2, 고리재질=3, 고리폭앞=4, 고리폭뒤=5, 고리원주앞=6, 고리원주뒤=7, 클립개수앞=8, 클립개수뒤=9, 빨대지름=10, 
                                        빨대개수=11, 성능=26) %>% select(고리간격:빨대개수, 성능)



data<- rdata %>% rename(고리간격=2, 고리재질=3, 고리폭앞=4, 고리폭뒤=5, 고리원주앞=6, 고리원주뒤=7, 클립개수앞=8, 클립개수뒤=9, 빨대지름=10, 
                              빨대개수=11, 성능=26) %>% select(고리간격:빨대개수,성능) %>% filter(!is.na(성능))

#### 2.머신러닝 ####
#### 가. 데이터 나누기 ####
splits <- initial_split(data, 0.8, strata = 성능)
train <- training(splits)
test <- testing(splits)

#### 나. 머신러닝 모델 만들기 ####
fit_rf <- train(성능~., train, method = "rf")

#### 다. 학습결과 교차검증하기 ####
predict(fit_rf, test) %>% postResample(test$성능)
predict(fit_rf, test) %>% plot(test$성능)



#### 라. 변수 중요도 찾기 ####
varImp(fit_rf) %>% plot()



#### 3. 변수별 성능확인 ####
summary(data)
#library(dlookr)

#diagnose_category(as.factor(data$고리재질))
summary(as.factor(data$고리재질))


#### 가. 고리간격 ####

test %>% 
  mutate(#고리간격=15.50, 고리재질='A4 마분지', 고리폭앞=2.500, 고리폭뒤=3.000, 고리원주앞=15.00, 고리원주뒤=20.00, 클립개수앞=1.000, 클립개수뒤=:0.0000, 빨대지름=0.7000, 
빨대개수=1) %>% mutate(예측=predict(fit_rf,.)) %>% ggplot(aes(x=고리간격, y=예측))+geom_point()



#### 나 고리재질 ####

test %>% 
  mutate(고리간격=15.50, #고리재질='A4 마분지', 
             고리폭앞=2.500, 고리폭뒤=3.000, 고리원주앞=15.00, 고리원주뒤=20.00, 클립개수앞=1.000, 클립개수뒤=0.0000, 빨대지름=0.7000, 
    빨대개수=1) %>% mutate(예측=predict(fit_rf,.)) %>% ggplot(aes(x=고리재질, y=예측))+geom_point()



#### 다. 고리폭앞 ####

test %>% 
  mutate(고리간격=15.50, 고리재질='A4 마분지', 
             #고리폭앞=2.500, 
             고리폭뒤=3.000, 고리원주앞=15.00, 고리원주뒤=20.00, 클립개수앞=1.000, 클립개수뒤=0.0000, 빨대지름=0.7000, 빨대개수=1) %>% 
  mutate(예측=predict(fit_rf,.)) %>% ggplot(aes(x=고리폭앞, y=예측))+geom_point()


#### 라. 고리폭뒤 ####

test %>% 
  mutate(고리간격=15.50, 고리재질='A4 마분지', 
             고리폭앞=2.500, 
             #고리폭뒤=3.000, 
             고리원주앞=15.00, 고리원주뒤=20.00, 클립개수앞=1.000, 클립개수뒤=0.0000, 빨대지름=0.7000, 빨대개수=1) %>% 
  mutate(예측=predict(fit_rf,.)) %>% ggplot(aes(x=고리폭뒤, y=예측))+geom_point()




#### 마. 고리원주앞 ####

test %>% 
  mutate(고리간격=15.50, 고리재질='A4 마분지', 
             고리폭앞=2.500, 
             고리폭뒤=3.000, 
             #고리원주앞=15.00, 
             고리원주뒤=20.00, 클립개수앞=1.000, 클립개수뒤=0.0000, 빨대지름=0.7000, 빨대개수=1) %>% 
  mutate(예측=predict(fit_rf,.)) %>% ggplot(aes(x=고리원주앞, y=예측))+geom_point()



#### 바. 고리원주 뒤 ####


test %>% 
  mutate(고리간격=15.50, 고리재질='A4 마분지', 
             고리폭앞=2.500, 
             고리폭뒤=3.000, 
             고리원주앞=15.00, 
             #고리원주뒤=20.00, 
             클립개수앞=1.000, 클립개수뒤=0.0000, 빨대지름=0.7000, 빨대개수=1) %>% 
  mutate(예측=predict(fit_rf,.)) %>% ggplot(aes(x=고리원주뒤, y=예측))+geom_point()





#### 사. 클립개수 앞 ####

test %>% 
  mutate(고리간격=15.50, 고리재질='A4 마분지', 
             고리폭앞=2.500, 
             고리폭뒤=3.000, 
             고리원주앞=15.00, 
             고리원주뒤=20.00, 
             #클립개수앞=1.000, 
             클립개수뒤=0.0000, 빨대지름=0.7000, 빨대개수=1) %>% 
  mutate(예측=predict(fit_rf,.)) %>% ggplot(aes(x=클립개수앞, y=예측))+geom_point()





#### 아. 클립개수 뒤 ####


test %>% 
  mutate(고리간격=15.50, 고리재질='A4 마분지', 
             고리폭앞=2.500, 
             고리폭뒤=3.000, 
             고리원주앞=15.00, 
             고리원주뒤=20.00, 
             클립개수앞=1.000, 
             #클립개수뒤=0.0000, 
             빨대지름=0.7000, 빨대개수=1) %>% 
  mutate(예측=predict(fit_rf,.)) %>% ggplot(aes(x=클립개수뒤, y=예측))+geom_point()





#### 자. 빨대개수 ####



test %>% 
  mutate(고리간격=15.50, 고리재질='A4 마분지', 
             고리폭앞=2.500, 
             고리폭뒤=3.000, 
             고리원주앞=15.00, 
             고리원주뒤=20.00, 
             클립개수앞=1.000, 
             클립개수뒤=0.0000, 
             #빨대지름=0.7000, 
             빨대개수=1) %>% 
  mutate(예측=predict(fit_rf,.)) %>% ggplot(aes(x=빨대지름, y=예측))+geom_point()


















