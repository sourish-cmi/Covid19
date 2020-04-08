##################################################################################
# An R script to state-wise R0 estimation for Covid-19 disease 
# progression in India
#
# https://github.com/sourish-cmi/Covid19
#
# Author: Sourish Das
#         sourish@cmi.ac.in
#
# Created: April 04, 2020
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
#
# The code implement the Sherry Tower's  SIR implementation
# 
# The code uses population_india_census2011.csv and covid_19_india.csv downloaded from Kaggle Covid-19 India database from
# https://www.kaggle.com/sudalairajkumar/covid19-in-india
# 
##################################################################################



rm(list=ls())
######## R0 estimation
options(warn=-1)
library(R0)
source("Covid19/sir_func.R")
source("Covid19/R0_and_mean_shape_estimation.R")
data<-read.csv(file='https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv',head=TRUE)


dts1<-dts<-colnames(data)[-c(1:4)]
for(j in 1:length(dts))dts1[j]<-strsplit(dts[j],"X")[[1]][2]
dats<-as.Date(dts1,format = "%m.%d.%y")


china_data<-data[data$Country.Region=='China',]

china_rate<-apply(china_data[,-c(1:4)],2,sum)/population['China']
china_rate<-china_rate*100000

Hubei<-china_data[china_data$Province.State=='Hubei',-c(1:4)]

Hubei_incid<-diff(t(Hubei))
n<-length(Hubei_incid)  
  
Hubei_data1<-cbind.data.frame(Date=dats[2:length(dats)],Hubei_incid=Hubei_incid)

colnames(Hubei_data1)<-c("Dates","Incidence")
Hubei_data1<-Hubei_data1[1:23,]

Hubei_population<-57237740


Hubei_fit<-R0_and_mean_shape_estimation(incidence=Hubei_data1$Incidence
                             ,dates=Hubei_data1$Dates
                             ,N=70
                             ,mean_min=0.1
                             ,mean_max=10
                             ,shape_min=0.1
                             ,shape_max=10
                             ,npop = Hubei_population
                             ,I_0 = 444
                             ,R_0 = 0
                             ,gamma = 1/14 ## 14 days to recover        
)

Hubei_fit["mean"]
mean_hat<-Hubei_fit["mean"]
shape_hat<-Hubei_fit["shape"]
R0  <- Hubei_fit["R0"]    

gamma<-1/14   ## This is gamma parameter of the SIR model
beta  = R0*gamma  ## This is beta parameter of SIR model 

npop = Hubei_population
I_0 = 444
R_0 = 0
S_0 = npop-I_0-R_0

tbegin = 0
tend   = length(Hubei_data1$Incidence)
vt = seq(tbegin,tend,1)  

vparameters = c(gamma=gamma,beta=beta)
inits = c(S=S_0,I=I_0,R=R_0)

solved_model = as.data.frame(lsoda(inits, vt, derivative_calc_func, vparameters))

vI = solved_model$I

plot(Hubei_data1$Dates,Hubei_data1$Incidence,pch=20,xlab="Hubei : R0 = 2.53",ylab="Incidence")
points(Hubei_data1$Dates,vI[2:length(vI)],type="l",lwd=2,col="red")

plot(Hubei_data1$Dates,cumsum(Hubei_data1$Incidence),pch=20,xlab="Hubei : R0 = 2.53"
     ,ylab="Cumulative Cases")
points(Hubei_data1$Dates,cumsum(vI[2:length(vI)]),type="l",lwd=2,col="red")


#### China R0 estimation


china_data1<-cbind.data.frame(Date=dats,Cases=apply(china_data[,-c(1:4)],2,sum))

china_incid<-diff(china_data1$Cases)
n<-length(china_incid)  

china_data1<-cbind.data.frame(Date=dats[2:length(dats)],China_incid=china_incid)

colnames(china_data1)<-c("Dates","Incidence")
china_data1<-china_data1[1:23,]

China_population<-1427647786

China_fit<-R0_and_mean_shape_estimation(incidence=china_data1$Incidence
                                        ,dates=china_data1$Dates
                                        ,N=70
                                        ,mean_min=0.1
                                        ,mean_max=10
                                        ,shape_min=0.1
                                        ,shape_max=10
                                        ,npop = China_population
                                        ,I_0 = 548
                                        ,R_0 = 0
                                        ,gamma = 1/14 ## 14 days to recover        
)

mean_hat<-China_fit["mean"]
shape_hat<-China_fit["shape"]
R0  <- China_fit["R0"]    

gamma<-1/14   ## This is gamma parameter of the SIR model
beta  = R0*gamma  ## This is beta parameter of SIR model 

npop = China_population
I_0 = 548
R_0 = 0
S_0 = npop-I_0-R_0

tbegin = 0
tend   = length(china_data1$Incidence)
vt = seq(tbegin,tend,1)  

vparameters = c(gamma=gamma,beta=beta)
inits = c(S=S_0,I=I_0,R=R_0)

solved_model = as.data.frame(lsoda(inits, vt, derivative_calc_func, vparameters))

vI = solved_model$I


plot(china_data1$Dates,china_data1$Incidence,pch=20,xlab="China : R0 = 2.46",ylab="Incidence")
points(china_data1$Dates,vI[2:length(vI)],type="l",lwd=2,col="red")

plot(china_data1$Dates,cumsum(china_data1$Incidence),pch=20,xlab="China : R0 = 2.53"
     ,ylab="Cumulative Cases")
points(china_data1$Dates,cumsum(vI[2:length(vI)]),type="l",lwd=2,col="red")


#### India R0 estimation

India_data<-data[data$Country.Region=='India',]
India_data1<-cbind.data.frame(Date=dats,Cases=t(India_data[-c(1:4)]))
India_incid<-diff(India_data1$`132`)
n<-length(India_incid)  

India_data1<-cbind.data.frame(Date=dats[2:length(dats)],Hubei_incid=India_incid)
colnames(India_data1)<-c("Dates","Incidence")

India_population<-1359772087


India_data_full<-subset(India_data1,Dates>="2020-03-02")
#India_data1<-subset(India_data1,Dates<="2020-03-24")

India_fit_full<-R0_and_mean_shape_estimation(incidence=India_data_full$Incidence
                                        ,dates=India_data_full$Dates
                                        ,N=70
                                        ,mean_min=0.1
                                        ,mean_max=10
                                        ,shape_min=0.1
                                        ,shape_max=10
                                        ,npop = India_population
                                        ,I_0 = 5
                                        ,R_0 = 0
                                        ,gamma = 1/14 ## 14 days to recover        
)

India_data2<-subset(India_data1,Dates>="2020-03-02")
India_data2<-subset(India_data2,Dates<="2020-03-24")

India_fit_b4_lokdwn<-R0_and_mean_shape_estimation(incidence=India_data2$Incidence
                                             ,dates=India_data2$Dates
                                             ,N=70
                                             ,mean_min=0.1
                                             ,mean_max=10
                                             ,shape_min=0.1
                                             ,shape_max=10
                                             ,npop = India_population
                                             ,I_0 = 5
                                             ,R_0 = 0
                                             ,gamma = 1/14 ## 14 days to recover        
)


#India_data2<-subset(India_data1,Dates>="2020-03-02")
#India_data2<-subset(India_data2,Dates<="2020-03-24")


India_fit_with_jan_feb<-R0_and_mean_shape_estimation(incidence=India_data1$Incidence
                                                  ,dates=India_data1$Dates
                                                  ,N=70
                                                  ,mean_min=0.1
                                                  ,mean_max=10
                                                  ,shape_min=0.1
                                                  ,shape_max=10
                                                  ,npop = India_population
                                                  ,I_0 = 1
                                                  ,R_0 = 0
                                                  ,gamma = 1/14 ## 14 days to recover        
)


India_data2<-subset(India_data1,Dates<="2020-03-24")

India_fit_with_jan_feb_b4_lokdown<-R0_and_mean_shape_estimation(incidence=India_data2$Incidence
                                                     ,dates=India_data2$Dates
                                                     ,N=70
                                                     ,mean_min=0.1
                                                     ,mean_max=10
                                                     ,shape_min=0.1
                                                     ,shape_max=10
                                                     ,npop = India_population
                                                     ,I_0 = 1
                                                     ,R_0 = 0
                                                     ,gamma = 1/14 ## 14 days to recover        
)

Fit_summary<-rbind(Hubei_fit
                   ,China_fit
                   ,India_fit_with_jan_feb_b4_lokdown
                   ,India_fit_full
                   ,India_fit_with_jan_feb
                   ,India_fit_b4_lokdwn)

library(xtable)
xtable(round(Fit_summary[,c('R0','R0.lower','R0.upper')],2))

mean_hat<-India_fit["mean"]
shape_hat<-India_fit["shape"]
R0  <- India_fit["R0"]    

gamma<-1/14   ## This is gamma parameter of the SIR model
beta  = R0*gamma  ## This is beta parameter of SIR model 

npop = India_population
I_0 = 5
R_0 = 0
S_0 = npop-I_0-R_0

tbegin = 0
tend   = length(India_data1$Incidence)
vt = seq(tbegin,tend,1)  

vparameters = c(gamma=gamma,beta=beta)
inits = c(S=S_0,I=I_0,R=R_0)

solved_model = as.data.frame(lsoda(inits, vt, derivative_calc_func, vparameters))

vI = solved_model$I


plot(India_data1$Dates,India_data1$Incidence,pch=20,xlab="India : R0 = 2.52",ylab="Incidence")
points(India_data1$Dates,vI[2:length(vI)],type="l",lwd=2,col="red")

plot(India_data1$Dates,cumsum(India_data1$Incidence),pch=20,xlab="India : R0 = 2.52"
     ,ylab="Cumulative Cases")
points(India_data1$Dates,cumsum(vI[2:length(vI)]),type="l",lwd=2,col="red")



#### statewise R0 estimation
population_india<-read.csv(file="Covid19/covid19-in-india/population_india_census2011.csv",header = T,stringsAsFactors = F)
data_state<-read.csv(file="Covid19/covid19-in-india/covid_19_india.csv",header = T,stringsAsFactors = F)

data_state$State.UnionTerritory[data_state$State.UnionTerritory=='Chhattisgarh']<-'Chattisgarh'
data_state$State.UnionTerritory[data_state$State.UnionTerritory=='Pondicherry']<-'Puducherry'

data_state$Date<-as.Date(data_state$Date,format = "%d/%m/%y")

#data_state<-subset(data_state,Date>="2020-03-07")
#data_state_before<-subset(data_state,Date<="2020-03-24")

data_state<-subset(data_state,State.UnionTerritory!="Unassigned")

st_ut<-sort(unique(data_state$State.UnionTerritory))


R0_statewise<-data.frame(matrix(NA,nrow=length(st_ut),ncol=4))
colnames(R0_statewise)<-c("State/UT","R0","Lower","Upper")


for(i in 1:length(st_ut)){
  i<-24
  tryCatch(
    {
      data_sub<-subset(data_state,State.UnionTerritory==st_ut[i])
      R0_statewise[i,"State/UT"]<-st_ut[i]
      
      ### R0 Estimation 
      incid<-diff(data_sub$Confirmed)
      incid[incid<0]<-0
      n<-length(incid)
      cat(st_ut[i],n,"\n")
      t<-na.omit(as.character(data_sub$Date[2:nrow(data_sub)]))
      
      state_popln<- population_india[population_india$State...Union.Territory==st_ut[i],"Population"]
      rm(list = 'state_fit')
      state_fit<-R0_and_mean_shape_estimation(incidence=incid
                                              ,dates=t
                                              ,N=70
                                              ,mean_min=0.1
                                              ,mean_max=10
                                              ,shape_min=0.1
                                              ,shape_max=10
                                              ,npop = state_popln
                                              ,I_0 = 1
                                              ,R_0 = 0
                                              ,gamma = 1/14 ## 14 days to recover        
      )
      
      
      
      R0_statewise[i,"R0"]<-state_fit['R0']
      R0_statewise[i,"Lower"]<-state_fit['R0.lower']
      R0_statewise[i,"Upper"]<-state_fit['R0.upper']
      
      
    }, error=function(e){}
  )
   Sys.sleep(10)
}

apply(R0_statewise[,2:4],2,median,na.rm=T)  

final_R0_results<-list(Fit_summary=Fit_summary,R0_statewise=R0_statewise)

final_R0_results[['Fit_summary']]
final_R0_results[['R0_statewise']]

save(final_R0_results,file='Covid19/R0_results.RData')

xtable(na.omit(R0_statewise))



### Analysis for Punjab

data_sub<-subset(data_state,State.UnionTerritory=="Punjab")

plot(data_sub$Date, data_sub$Confirmed,pch=20,xlab="Punjab : R0 = 15.89",ylab = "Cases")
plot(data_sub$Date[2:nrow(data_sub)],diff(data_sub$Confirmed),pch=20,type = "b")

