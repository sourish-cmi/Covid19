##################################################################################
# An R script for R0 estimation 
#
# Source: https://github.com/sourish-cmi/Covid19
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
# The code implement the Sherry Tower's  SIR implementation
# 
# 
##################################################################################



R0_and_mean_shape_estimation<-function(incidence
                                       ,dates
                                       ,N=100
                                       ,mean_min=0.1
                                       ,mean_max=10
                                       ,shape_min=0.1
                                       ,shape_max=10
                                       ,npop = 100000
                                       ,I_0 = 1
                                       ,R_0 = 0
                                       ,gamma = 1/14 ## 14 days to recover        

                                       
){
  library("R0")
  mean.star<-seq(mean_min,mean_max,length.out = N)
  shape.star<-seq(shape_min,shape_max,length.out = N)
  R0.star<-matrix(NA,nrow=N^2,ncol=6)
  colnames(R0.star)<-c("mean","shape","R0","R0.lower","R0.upper","MSE")
  S_0 = npop-I_0-R_0
  
  tbegin = 0
  tend   = length(incidence)
  vt = seq(tbegin,tend,1)  
  
  k<-1
  for(i in 1:N){
    for(j in 1:N){
      tryCatch(
        {
          R0.star[k,"mean"]<-mean.star[i]
          R0.star[k,"shape"]<-shape.star[j]
          mGT<-generation.time("gamma"
                               ,c(mean.star[i]
                                  ,mean.star[j]))
          EG.R<-est.R0.EG(epid=incidence,t=dates ,GT=mGT)
          R0.star[k,"R0"]<-EG.R$R
          R0.star[k,"R0.lower"]<-EG.R$conf.int[1]
          R0.star[k,"R0.upper"]<-EG.R$conf.int[2]
          R0    = EG.R$R       
          beta  = R0*gamma  ## This is beta parameter of SIR model 
          
          vparameters = c(gamma=gamma,beta=beta)
          inits = c(S=S_0,I=I_0,R=R_0)
          
          solved_model = as.data.frame(lsoda(inits, vt, derivative_calc_func, vparameters))
          
          vI = solved_model$I
          R0.star[k,"MSE"]<-mean((vI-incidence)^2)
        }, error=function(e){}
      )
      rm(list = c("EG.R")) 
      if(k%%1000==0)cat("k = ",k,"\n")
      k<-k+1
    }
  }
  min.mse.indx<-which.min(R0.star[,"MSE"])
  result<-R0.star[min.mse.indx,]
  return(result)
  
}