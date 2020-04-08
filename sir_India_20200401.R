##################################################################################
# An R script to solve ODE's of a Susceptible Infected Recovered (SIR) model 
# http://www.sherrytowers.com/sir.R
#
# Author: Sherry Towers
#         smtowers@asu.edu
# Created: Dec 1st, 2012
#
# Copyright Sherry Towers, 2012
#
# This script is not guaranteed to be free of bugs and/or errors.
#
# This script can be freely used and shared as long as the author and
# copyright information in this header remain intact.
#
#
# You can edit script files (for example, this file)
# and either cut and paste lines from file into R command line
# (in Windows you can use ctrl-R to do this)
# or in the R command line type:
#
#    source("sir.R")
#
# You may need to use full path name in the filename, or alternatively in the R console
# window change to the directory containing the file sir.R by using the command:
#
#    setwd("<path of your directory>") 
#
# you will also need to have downloaded the file sir_func.R into that directory
# from http://www.sherrytowers.com/sir_func.R
##################################################################################

### Thanks to Sherry Tower's code
### I have modified it for India Covid-19 epidemic
### and came up with the SIR numbers for India.
### 
### Sourish Das

rm(list = ls(all = TRUE))  # resets R to fresh

##################################################################################
# The sfsmisc library includes a few nice plotting options, including
# the mult.fig() function which nicely divides up the plotting area
# If you don't already have this installed in R, type in the R command line:
#    install.packages("sfsmisc")
# and select a site geographically close to you for download
##################################################################################
require("sfsmisc")

##################################################################################
# sir_func.R contains a function derivative_calc_func that calculates the derivatives of
# S, I and R wrt time.  This function will get passed to functions in the R 
# deSolve library that will numerically solve the system of ODE's
##################################################################################
source("Covid19/sir_func.R")  

##################################################################################
# Let's set up some initial conditions at time t=0
# npop is the population
# I_0 is the inital number infected and infectious
# R_0 is the initial number recovered and immune
# S_0 is the initial number susceptible.  Note that npop=S_0+I_0+R_0
##################################################################################
npop = 1350674836
I_0 = 536
R_0 = 0
S_0 = npop-I_0-R_0

##################################################################################
# now the parameters of the model.  Note that in some posts on sherrytowers.com
# I refer to the recovery rate as k (here it is gamma), and the transmission
# rate as b (here is is beta).
#
# tbegin  is the begin time for the simulation (here we assume units of days)
# tend    is the time we want the simulation to end
#
# gamma=1/3   The recovery rate in units 1/days.  Note that 1/gamma is the
#             average recovery period.  For influenza,  this is quite short, but
#             for other diseases it can be quite long
# R0          This is the reproduction number of the disease.  For pandemic influenza
#             this has been found to be around 1.5
# beta        The transmission rate.  For the SIR model, mathematical analysis of
#             the model yields the relationship R0=beta/gamma
#             Thus if we know R0 and we know gamma, we can calculate beta
#
# vt          is the vector of time steps at which we want model estimates
##################################################################################
tbegin = 0
tend   = 180
vt = seq(tbegin,tend,1)  

gamma = 1/14        
R0    = 2.5       
beta  = R0*gamma   

##################################################################################
# fill named vectors with our parameter values and initial conditions
##################################################################################
vparameters = c(gamma=gamma,beta=beta)
inits = c(S=S_0,I=I_0,R=R_0)

##################################################################################
# this is where we call the lsoda function in the R deSolve library, which
# uses the 4th order Runge-Kutta method to solve the system of ODE's described
# in the derivative_calc_func() function in sir_func.R
##################################################################################
solved_model = as.data.frame(lsoda(inits, vt, derivative_calc_func, vparameters))

##################################################################################
# the lsoda method returns an object, with attributes being:
#  the time vector
#  the estimates of the values in the compartmentents of the model at each time
##################################################################################
cat("The item names in the solved_model object are:",names(solved_model),"\n")

##################################################################################
# let's fill some vectors with the results, just because I don't want to have to
# type "solved_model" over and over again....
##################################################################################
vS = solved_model$S
vI = solved_model$I
vR = solved_model$R
vtime = solved_model$time
vnpop = vS+vI+vR

##################################################################################
##################################################################################
##################################################################################
# now let's plot the results
# mult.fig(4) divides the plotting area into two up and two down
# It has an option that allows you to put a main title on the set of plots
##################################################################################
mult.fig(4,main="SIR model of pandemic influenza with R0=1.5") 

##################################################################################
# In the R plot() function:
#   type="l"  produces a line (the default is points)
#   lwd=XXX   produces a line of thickness XXX relative to the default line width
#   xlab and ylab and main add labels to the axes and title of the plot
#   ylim and xlim set the limits on the x and y axes (these default to automatically
#                 calculated values based on the data... but those don't necessarily
#                 produce the prettiest plots)
#   col=XXX   is the colour of the lines or points.  A list of R colours can be found
#             using colors() in R.  However, R also has a short list of numerically
#             coded colours that are a lot more convenient than typing "red" or "green",
#             etc.  These colour codes are
#             1=black
#             2=red
#             3=green
#             4=blue
#             5=cyan
#             6=magenta
#             7=yellow
#             8=gray
#
# Let's plot the prevalence first, which is the fraction infected at each
# point in time
# Here I calculate the ylimits on the plot to be 40% larger than the largest
# value of vI/npop
##################################################################################
ymax = 1.4*max(vI/npop)
plot(vtime,vI/npop,type="l",xlab="time",ylab="fraction infected",ylim=c(0,ymax),lwd=3,col=4,main="Infected")

##################################################################################
# Now let's overlay the incidence, which is the fraction of the population
# newly infected at each time step.  For the SIR model, this is -delta(S)/delta(t)
# the R lines() command overlays lines on the current plot
##################################################################################
n=length(vtime)
lines(vtime[2:n],-diff(vS)/(diff(vtime)*vnpop[1:(n-1)]),type="l",lwd=3,col=2)

##################################################################################
# The R legend function overlays a legend showing the lines
# Some options are:
#   bty="n" does not produce a box around the legend (the default is to do so)
#   legend  is the list of descriptions of the lines
#   col     is the list of line colours
#   lwd     is the list of line widths
# placement on the plot can be "topright" "topleft" "bottomright" or "bottomleft"
##################################################################################
legend("topright",legend=c("total infected (prevalence)","newly infected/day (incidence)"),bty="n",lwd=3,col=c(4,2))

##################################################################################
# Now plot the fraction susceptible at each time point
##################################################################################
ymin = 0.9*min(vS/vnpop)
plot(vtime,vS/vnpop,type="l",xlab="time",ylab="fraction susceptible",ylim=c(ymin,1),lwd=3,main="Susceptible")

##################################################################################
# Let's find the time at which S/N is closest to 1/R0.  Mathematical
# analysis of the SIR model indicates that this is where I/N should be
# maximal.
# Overlay a vertical line indicating this time point
##################################################################################
iind = which.min(abs(vS/vnpop-1/R0)) # find the index at which S/N is equal to 1/R0
lines(c(vtime[iind],vtime[iind]),c(-1000,1000),col=3,lwd=3)
legend("bottomleft",legend=c("time at which S=1/R0"),bty="n",lwd=3,col=c(3),cex=0.7)

##################################################################################
# Now let's plot the prevalence again, but this time the log of the prevalence.
# Mathematical analysis of the SIR model indicates that initially there is
# an exponential rise in prevalence.  Peak prevalence should occur at the
# time point where S=1/R0, so let's overlay that time point indicated
# by a vertical line to check whether our calculations in our simulation were
# correct
##################################################################################
plot(vtime,log(vI/vnpop),type="l",xlab="time",ylab="log(fraction infected)",lwd=3,col=4,main="log(Infected)")

##################################################################################
# the text(x,y,"my text") function overlays text on the plot
# at position x,y in the current plot coordinates
# cex=XXX makes the text size XXX relative to the default text size
##################################################################################
text(40,-14,"Initial\n exponential\n rise",cex=0.7)

lines(c(vtime[iind],vtime[iind]),c(-1000,1000),col=3,lwd=3)
legend("topleft",legend=c("time at which S=1/R0","log(Infected)"),bty="n",lwd=3,col=c(3,4),cex=0.7)

##################################################################################
# output the final size
# first solve for the final fraction of suscetibles at time=infinity
# using the final size relationship in 
# www.fields.utoronto.ca/programs/scientific/10-11/drugresistance/emergence/fred1.pdf 
#
# To numerically estimate this, start by filling a vector from 0 to 1 in tiny
# steps
##################################################################################
epsilon = 0.00001
vsinf = seq(0,1-epsilon,epsilon)
##################################################################################
# Using the vsinf vector, numerically solve -log(sinf)+log(s0) = R0*(1-sinf)
# so,
# LHS = -log(vsinf)+log(S_0/npop)
# RHS = R0*(1-vsinf)
##################################################################################
LHS = -log(vsinf)+log(S_0/npop)
RHS = R0*(1-vsinf)
iind = which.min(abs(RHS-LHS))
sinf_predicted = vsinf[iind]

cat("The final fraction of susceptibles at the end of the epidemic from the model simulation is ",min(vS/vnpop),"\n")
cat("The final fraction of susceptibles at the end of the epidemic predicted by the final size relation is ",sinf_predicted,"\n")

c(R0=R0, cases=cumsum(vI)[n])

cumsum(vR)[n]
