##################################################################################
# An R script to predict the Covid-19 disease progression in India
# https://github.com/sourish-cmi/Covid19
#
# Author: Sourish Das
#         sourish@cmi.ac.in
#
# Created: Mar 24, 2020
# Copyright Sourish Das, 2020
#
# Published on: 07 Apr, 2020 (Version: 1.0)
#
# The script uses the Covid-19 Databases from John-Hopkins University
# Available from: https://github.com/CSSEGISandData/COVID-19
#
# This script is not guaranteed to be free of bugs and/or errors.
#
# This script can be freely used and shared as long as the author and
# copyright information in this header remain intact.
#
# In case John-Hopkins University changes the dataprotocol
# you may have to adopt the data procesing part according to JHU
##################################################################################





rm(list=ls())

## Data call from JHU repository

data<-read.csv(file='https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv',head=TRUE)

### Population data
nms<-c("China","India","US","Iran","SouthKorea","Japan","Italy","France","Germany","Spain")

population<-c(1401754280,1359772087,329448153,83279228,51780579,126010000,60243406,67064000,83149300,47100396)

names(population)<-nms

age<-c(37.4,28.1,38.1,30.3,41.8,47.3,45.5,41.4,47.1,42.7)

names(age)<-nms

## dates handling
dts1<-dts<-colnames(data)[-c(1:4)]
for(j in 1:length(dts))dts1[j]<-strsplit(dts[j],"X")[[1]][2]
dats<-as.Date(dts1,format = "%m.%d.%y")

### Data Processing
### Calculate Rate or cases per 100,000

## China
china_data<-data[data$Country.Region=='China',]
china_rate<-apply(china_data[,-c(1:4)],2,sum)/population['China']
china_rate<-china_rate*100000


#### US

US_data<-data[data$Country.Region=='US',]
US_rate<-apply(US_data[,-c(1:4)],2,sum)/population['US']
US_rate<-US_rate*100000
#US_rate<-t(US_rate)

#### Italy
Italy_data<-data[data$Country.Region=='Italy',]
Italy_rate<-Italy_data[,-c(1:4)]/population['Italy']
Italy_rate<-Italy_rate*100000
Italy_rate<-t(Italy_rate)

#### France
France_data<-data[data$Country.Region=='France',]
France_rate<-France_data[1,-c(1:4)]/population['France']
France_rate<-France_rate*100000
France_rate<-t(France_rate)

#### South Korea
SouthKorea_data<-data[data$Country.Region=='Korea, South',]
SouthKorea_rate<-SouthKorea_data[1,-c(1:4)]/population['SouthKorea']
SouthKorea_rate<-SouthKorea_rate*100000
SouthKorea_rate<-t(SouthKorea_rate)

#### Iran
Iran_data<-data[data$Country.Region=='Iran',]
Iran_rate<-Iran_data[1,-c(1:4)]/population['Iran']
Iran_rate<-Iran_rate*100000
Iran_rate<-t(Iran_rate)

#### Japan
Japan_data<-data[data$Country.Region=='Japan',]
Japan_rate<-Japan_data[1,-c(1:4)]/population['Japan']
Japan_rate<-Japan_rate*100000
Japan_rate<-t(Japan_rate)

#### Germany
Germany_data<-data[data$Country.Region=='Germany',]
Germany_rate<-Germany_data[1,-c(1:4)]/population['Germany']
Germany_rate<-Germany_rate*100000
Germany_rate<-t(Germany_rate)

#### Spain
Spain_data<-data[data$Country.Region=='Spain',]
Spain_rate<-Spain_data[1,-c(1:4)]/population['Spain']
Spain_rate<-Spain_rate*100000
Spain_rate<-t(Spain_rate)

#### India

India_data<-data[data$Country.Region=='India',]
India_rate<-India_data[1,-c(1:4)]/population['India']
India_rate<-India_rate*100000
India_rate<-t(India_rate)

###########################################
## Prepare the data for modeling
##########################################

############################################

India_data1<-cbind.data.frame(Date=dats,Lat=rep(as.numeric(India_data['Lat']),length(India_rate)),Rate=India_rate)
colnames(India_data1)<-c("Dates","Lat","Rate")
India_data1$Time<-1:length(India_rate)
India_data1$Country_name<-rep("India",length(India_rate))
India_data1$Age<-rep(age['India'],length(India_rate))


Italy_data1<-cbind.data.frame(Date=dats,Lat=rep(as.numeric(Italy_data['Lat']),length(Italy_rate)),Rate=Italy_rate)
colnames(Italy_data1)<-c("Dates","Lat","Rate")
Italy_data1$Time<-1:length(Italy_rate)
Italy_data1$Country_name<-rep("Italy",length(Italy_rate))
Italy_data1$Age<-rep(age['Italy'],length(Italy_rate))


Iran_data1<-cbind.data.frame(Date=dats,Lat=rep(as.numeric(Iran_data['Lat']),length(Iran_rate)),Rate=Iran_rate)
colnames(Iran_data1)<-c("Dates","Lat","Rate")
Iran_data1$Time<-1:length(Iran_rate)
Iran_data1$Country_name<-rep("Iran",length(Iran_rate))
Iran_data1$Age<-rep(age['Iran'],length(Iran_rate))

France_data1<-cbind.data.frame(Date=dats,Lat=rep(as.numeric(France_data[1,'Lat']),length(France_rate)),Rate=France_rate)
colnames(France_data1)<-c("Dates","Lat","Rate")
France_data1$Time<-1:length(France_rate)
France_data1$Country_name<-rep("France",length(France_rate))
France_data1$Age<-rep(age['France'],length(France_rate))

Spain_data1<-cbind.data.frame(Date=dats,Lat=rep(as.numeric(Spain_data['Lat']),length(Spain_rate)),Rate=Spain_rate)
colnames(Spain_data1)<-c("Dates","Lat","Rate")
Spain_data1$Time<-1:length(Spain_rate)
Spain_data1$Country_name<-rep("Spain",length(Spain_rate))
Spain_data1$Age<-rep(age['Spain'],length(Spain_rate))



Germany_data1<-cbind.data.frame(Date=dats,Lat=rep(as.numeric(Germany_data['Lat']),length(Germany_rate)),Rate=Germany_rate)
colnames(Germany_data1)<-c("Dates","Lat","Rate")
Germany_data1$Time<-1:length(Germany_rate)
Germany_data1$Country_name<-rep("Germany",length(Germany_rate))
Germany_data1$Age<-rep(age['Germany'],length(Germany_rate))


Japan_data1<-cbind.data.frame(Date=dats,Lat=rep(as.numeric(Japan_data['Lat']),length(Japan_rate)),Rate=Japan_rate)
colnames(Japan_data1)<-c("Dates","Lat","Rate")
Japan_data1$Time<-1:length(Japan_rate)
Japan_data1$Country_name<-rep("Japan",length(Japan_rate))
Japan_data1$Age<-rep(age['Japan'],length(Japan_rate))


SouthKorea_data1<-cbind.data.frame(Date=dats,Lat=rep(as.numeric(SouthKorea_data['Lat']),length(SouthKorea_rate)),Rate=SouthKorea_rate)
colnames(SouthKorea_data1)<-c("Dates","Lat","Rate")
SouthKorea_data1$Time<-1:length(SouthKorea_rate)
SouthKorea_data1$Country_name<-rep("South-Korea",length(SouthKorea_rate))
SouthKorea_data1$Age<-rep(age['SouthKorea'],length(SouthKorea_rate))


china_data1<-cbind.data.frame(Date=dats,Lat=rep(as.numeric(china_data[1,'Lat']),length(china_rate)),Rate=china_rate)
colnames(china_data1)<-c("Dates","Lat","Rate")
china_data1$Time<-1:length(china_rate)
china_data1$Country_name<-rep("China",length(china_rate))
china_data1$Age<-rep(age['China'],length(china_rate))

US_data1<-cbind.data.frame(Date=dats,Lat=rep(as.numeric(US_data[1,'Lat']),length(US_rate)),Rate=US_rate)
colnames(US_data1)<-c("Dates","Lat","Rate")
US_data1$Time<-1:length(US_rate)
US_data1$Country_name<-rep("US",length(US_rate))
US_data1$Age<-rep(age['US'],length(US_rate))

### Prepare the training dataset

data_train<-rbind.data.frame(china_data1,Italy_data1,Germany_data1,France_data1,Spain_data1,Japan_data1,SouthKorea_data1,Iran_data1,India_data1,US_data1)

### On 24th March the lock-down started
### So we took the data upto 24th March for training

data_train<-subset(data_train,Dates<="2020-03-24")

### Test data for 45 days ahead
n_ahead<-30

### prepare the test dataset for India

India_data1$cases<-as.numeric(t(India_data[1,-c(1:4)]))
India_data1<-subset(India_data1,Dates>="2020-03-03")

data_test<-data.frame(matrix(NA,nrow=(nrow(India_data1)+n_ahead),ncol=ncol(data_train)))
colnames(data_test)<-colnames(data_train)
data_test$Dates<-India_data1$Dates[1]+0:(n_ahead+nrow(India_data1)-1)
data_test$Lat<-rep(data_train$Lat[data_train$Country_name=='India'][1],(nrow(India_data1)+n_ahead))
data_test$Time<-(India_data1$Time[1]+0):(India_data1$Time[1]+nrow(data_test)-1)
data_test$Country_name<-rep('India',(nrow(India_data1)+n_ahead))
data_test$Age<-rep(India_data1$Age[1],(nrow(India_data1)+n_ahead))

data_full<-rbind.data.frame(data_train,data_test)

### Model training/ model fitting 
fit<-step(lm(log(Rate+1)~Time+I(Time^2)+I(Time^3)+I(Time^4)+I(Time^5)
             +Country_name
             +Time*Age
             +Time*Country_name
             +I(Time^2)*Country_name
             +I(Time^3)*Country_name
             +I(Time^4)*Country_name
             +I(Time^5)*Country_name
             ,data=data_train))
summary(fit)


pred<-predict(fit,newdata = data_test)
data_test$Rate_pred<- exp(pred)-1

## Confidence Interval
p<-length(fit$coefficients)
n<-nrow(data_train)
s<-sum(fit$residuals^2)/(n-p)
m1<-nrow(data_test)-(n_ahead+22)
s<-s*sqrt(1:(n_ahead+m1))
pred_lower<-pred[23:nrow(data_test)]-4*s
pred_upper<-pred[23:nrow(data_test)]+4*s


### Predict

data_test$Rate_pred_lower[23:nrow(data_test)]<-exp(pred_lower)-1
data_test$Rate_pred_lower[data_test$Rate_pred_lower<0]<-0
data_test$Rate_pred_upper[23:nrow(data_test)]<-exp(pred_upper)-1

data_test$Case<-NA
data_test$Case[1:22]<-India_data1$cases[1:22]


data_test$cases_pred<-round((data_test$Rate_pred/100000)*population['India'],2)
data_test$cases_pred_lower<-round((data_test$Rate_pred_lower/100000)*population['India'],2)
data_test$cases_pred_upper<-round((data_test$Rate_pred_upper/100000)*population['India'],2)


### Plot the training data (in black) 
### Plot prediction (in red) 
### Polot test data (in blue)

plot(data_test$Dates,data_test$Case,col="black" 
     ,type = "b" ,pch=20,ylab = "Cases",xlab = "India",ylim=c(0,5000))
points(data_test$Dates,data_test$cases_pred,type = "l",col="red",lwd=2)
points(data_test$Dates,data_test$cases_pred_lower,type = "l",col="red",lwd=2,lty=2)
points(data_test$Dates,data_test$cases_pred_upper,type = "l",col="red",lwd=2,lty=2)

abline(v=as.Date("2020-03-24"),lwd=2)
abline(v=as.Date("2020-04-01"),lwd=1,lty=2)
abline(v=as.Date("2020-04-15"),lwd=1,lty=2)
abline(v=as.Date("2020-05-01"),lwd=1,lty=2)

abline(h=5000,lwd=1,lty=2)
abline(h=15000,lwd=1,lty=2)
abline(h=25000,lwd=1,lty=2)
#grid(col = "grey",lwd=2)
points(as.Date("2020-03-24"),536,pch=20,col="blue")
points(as.Date("2020-03-25"),657,pch=20,col="blue")
points(as.Date("2020-03-26"),727,pch=20,col="blue")
points(as.Date("2020-03-27"),887,pch=20,col="blue")
points(as.Date("2020-03-28"),987,pch=20,col="blue")
points(as.Date("2020-03-29"),1024,pch=20,col="blue")
points(as.Date("2020-03-30"),1251,pch=20,col="blue")
points(as.Date("2020-03-31"),1397,pch=20,col="blue")
points(as.Date("2020-04-01"),1998,pch=20,col="blue")
points(as.Date("2020-04-02"),2543,pch=20,col="blue")
points(as.Date("2020-04-03"),2567,pch=20,col="blue")
points(as.Date("2020-04-04"),3082,pch=20,col="blue")
points(as.Date("2020-04-05"),3588,pch=20,col="blue")


#data_test_short<-data_test[,c("Dates","Case","cases_pred")]
#data_test_short[22:nrow(India_data1),"Case"]<-India_data1$cases[22:nrow(India_data1)]

library(xtable)
xtable(data_test_short)
#write.csv(data_test_short,file="prediction_20200331.csv",row.names = F)