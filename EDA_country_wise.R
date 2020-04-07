##################################################################################
# An R script to EDA of the Covid-19 disease progression in India
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
# In case John-Hopkins University changes the data protocol
# you may have to adopt the data procesing part according to JHU data protocol
##################################################################################


### Exploratory Data Analysis

#rm(list=ls())

data<-read.csv(file='https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv',head=TRUE)


### Population data
nms<-c("China","India","US","Iran","SouthKorea","Japan","Italy","France","Germany","Spain")

population<-c(1401754280,1359772087,329448153,83279228,51780579,126010000,60243406,67064000,83149300,47100396)
names(population)<-nms

## Median ages
age<-c(37.4,28.1,38.1,30.3,41.8,47.3,45.5,41.4,47.1,42.7)


names(age)<-nms
dts1<-dts<-colnames(data)[-c(1:4)]
for(j in 1:length(dts))dts1[j]<-strsplit(dts[j],"X")[[1]][2]
dats<-as.Date(dts1,format = "%m.%d.%y")




### Calculate Rate or cases per 100,000


#### US
n<-length(dats)
US_data<-data[data$Country.Region=='US',]
US_rate<-apply(US_data[,-c(1:4)],2,sum)/population['US']
US_rate<-US_rate*100000
plot(US_rate[(n-45):n]~dats[(n-45):n],type="l",lwd=2,col="black",ylab="Cases per 100,000",xlab="",ylim=c(0,300))

#### Italy
Italy_data<-data[data$Country.Region=='Italy',]
Italy_rate<-Italy_data[,-c(1:4)]/population['Italy']
Italy_rate<-Italy_rate*100000
Italy_rate<-t(Italy_rate)
points(Italy_rate[(n-45):n]~dats[(n-45):n],type="l",lwd=2,col="green",ylab="Cases per 100,000",xlab="Italy")

#### France
France_data<-data[data$Country.Region=='France',]
France_rate<-apply(France_data[,-c(1:4)],2,sum)/population['France']
France_rate<-France_rate*100000
points(France_rate[(n-45):n]~dats[(n-45):n],type="l",lwd=2,col="blue",ylab="Cases per 100,000",xlab="France")

#### Germany
Germany_data<-data[data$Country.Region=='Germany',]
Germany_rate<-Germany_data[1,-c(1:4)]/population['Germany']
Germany_rate<-Germany_rate*100000
Germany_rate<-t(Germany_rate)
points(Germany_rate[(n-45):n]~dats[(n-45):n],type="l",lwd=2,col="red",ylab="Cases per 100,000",xlab="Germany")


#### Iran
Iran_data<-data[data$Country.Region=='Iran',]
Iran_rate<-Iran_data[1,-c(1:4)]/population['Iran']
Iran_rate<-Iran_rate*100000
Iran_rate<-t(Iran_rate)
points(Iran_rate[(n-45):n]~dats[(n-45):n],type="l",lwd=2,col="darkgreen",ylab="Cases per 100,000",xlab="Iran")


#### Spain
Spain_data<-data[data$Country.Region=='Spain',]
Spain_rate<-Spain_data[1,-c(1:4)]/population['Spain']
Spain_rate<-Spain_rate*100000
Spain_rate<-t(Spain_rate)
points(Spain_rate[(n-45):n]~dats[(n-45):n],type="l",lwd=2,col="brown",ylab="Cases per 100,000",xlab="Spain")


text<-c("US","Italy","France","Germany","Iran","Spain")
legend("topleft",text,col=c("black","green","blue","red","darkgreen","brown"),lwd=c(2,2,2,2,2,2))

#### Japan
Japan_data<-data[data$Country.Region=='Japan',]
Japan_rate<-Japan_data[1,-c(1:4)]/population['Japan']
Japan_rate<-Japan_rate*100000
Japan_rate<-t(Japan_rate)
plot(Japan_rate~dats,type="l",lwd=2,col="red",ylab="Cases per 100,000",xlab="Japan")

#### South Korea
SouthKorea_data<-data[data$Country.Region=='Korea, South',]
SouthKorea_rate<-SouthKorea_data[1,-c(1:4)]/population['SouthKorea']
SouthKorea_rate<-SouthKorea_rate*100000
SouthKorea_rate<-t(SouthKorea_rate)
plot(SouthKorea_rate~dats,type="l",lwd=2,col="orange",ylab="Cases per 100,000",xlab="South Korea")

### China

china_data<-data[data$Country.Region=='China',]
china_rate<-apply(china_data[,-c(1:4)],2,sum)/population['China']
china_rate<-china_rate*100000

plot(china_rate~dats,type="l",lwd=2,col="red",ylim=c(0,8),ylab="Cases per 100,000",xlab="China")



#### India

India_data<-data[data$Country.Region=='India',]
India_rate<-India_data[1,-c(1:4)]/population['India']
India_rate<-India_rate*100000
India_rate<-t(India_rate)
plot(India_rate[(n-40):n]~dats[(n-40):n],type="l",lwd=2,col="skyblue",ylab="Cases per 100,000",xlab="India")
