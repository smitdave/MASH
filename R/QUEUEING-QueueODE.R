# #################################################################
# #
# #   MASH
# #   R6-ified
# #   Generic Bite Queueing Algorithms for SimBite
# #   David Smith, Hector Sanchez, Sean Wu
# #   June 13, 2017
# #
# #################################################################
#
# require(deSolve)
# require(rootSolve)
#
# queueMG1.dy = function(t, y, par, foi, MX){
#     dy = 0*y
#     down =  par[1]*y[-1]*c(1:MX)
#     up = foi(t,par)*y[-MX]
#
#     dy[-1] = dy[-1] - down
#     dy[-MX] = dy[-MX] + down
#
#     dy[-MX] = dy[-MX] - up
#     dy[-1] = dy[-1]  + up
#
#     #i=5
#     #dyi = -par[1]*y[i] + par[1]*y[i+1] - foi(t,par)*y[i] + foi(t,par)*y[i-1]
#     #dyi-dy[i]
#     #plot(dy, type ="l")
#     #browser()
#     list(dy)
# }
#
# queueMG1.eq = function(par,foi,T){
#   yi = queueMG1.hr(par,foi,T)
#   parm = c(par[1], mean(foi(T,par)))
#   y.eq = steady(yi, time=0, func=queueMG1.dy, parms=parm, foi=foi.m, MX=length(yi))
#   par(mfrow = c(1,1))
#   y.eq$y
# }
#
# queueMG1.hr = function(par,foi,T){
#   mfoi = mean(foi(T,par))
#   mMOI = mfoi/par[1]
#   MXMOI = 10*mMOI
#   dpois(c(0:MXMOI),mMOI)
# }
#
# queueMG1.sim = function(par,foi,T=c(0:1825)){
#   yi = queueMG1.hr(par,foi,T)
#   lsode(yi,T,queueMG1.dy,par,foi=foi,MX=length(yi))
# }
#
#
#
# foi.m = function(t,p){p[2]}
#
# foi.sin = function(t,p){p[2]*(1+p[3]*sin(2*pi*t/365))}
#
#
# T=c(0:(365*10))
# par = c(r=1/200, a=1/200, b=0.9)
# out.hr = queueMG1.hr(par,foi.sin,T)
# out.eq = queueMG1.eq(par,foi.sin,T)
# out.sim = queueMG1.sim(par,foi.sin,T)
#
# y.t = out.sim[,-1]
# p.t = y.t[,-1]
# MOI = 1:dim(p.t)[2]
#
# dMOI.t = MOI*t(p.t)
# mMOI.t = colSums(dMOI.t)
# PR = 1-p.t[,1]
#
# par(mfrow = c(2,2))
# plot(T/365, foi.sin(T,par), type = "l", main = "FOI vs. Time")
# #plot(c(0,MOI), out.hr, type = "h", lwd=3, xlim = c(0,50))
# #lines(c(0,MOI), out.eq, col = "red", type = "h")
# plot(T/365, PR, type = "l", main = "Prevalence")
# plot(T/365, mMOI.t, type = "l", main = "MOI", xlab = "Time")
# ixo=c(1:730)
# plot(foi.sin(T[-ixo],par), PR[-ixo],  type = "l", xlab = "FOI", ylab = "PR")
# #plot(mMOI.t[-ixo], PR[-ixo],  type = "l", xlab = "FOI", ylab = "PR")
