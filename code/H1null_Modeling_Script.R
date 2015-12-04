library(statnet)

#fit endogenous parameters
net_temp <- net_imp
tempform<-net_temp ~ edges + mutual + twopath + ctriple+ gwidegree(0.25, fixed = TRUE)+
 gwodegree(.25,fixed=TRUE)+gwesp(0.25, fixed = TRUE) + nodecov("NUMRESP") + nodecov("NUMGROUPS") + 
nodecov("MEANYEARS")
modtemp<-ergm(tempform,control=control.ergm(
Step.MCMC.samplesize=10000,
main.method="Stepping", MCMLE.metric="lognormal",
Step.gridsize=1000,
Step.maxit=20))
H1n_imp = modtemp
gof_H1n_imp<-gof(H1n_imp,nsim=100,control=control.gof.ergm(MCMC.init.maxedges=2000))

#fit endogenous parameters
net_temp <- net_plan
tempform<-net_temp ~ edges + mutual + twopath + ctriple+ 
gwidegree(0.25, fixed = TRUE)+
  gwodegree(.25,fixed=TRUE)+gwesp(0.25, fixed = TRUE) + nodecov("NUMRESP") + nodecov("NUMGROUPS") + 
nodecov("MEANYEARS")
modtemp<-ergm(tempform,control=control.ergm(
Step.MCMC.samplesize=10000,
main.method="Stepping", MCMLE.metric="lognormal",
Step.gridsize=1000,
Step.maxit=20))
H1n_plan = modtemp
gof_H1n_plan<-gof(H1n_plan,nsim=100,control=control.gof.ergm(MCMC.init.maxedges=2000))

net_temp <- net_cons
tempform<-net_temp ~ edges + mutual + twopath + ctriple+ 
gwidegree(0.25, fixed = TRUE)+
  gwodegree(.25,fixed=TRUE)+gwesp(0.25, fixed = TRUE) + nodecov("NUMRESP") + nodecov("NUMGROUPS") + 
nodecov("MEANYEARS")
modtemp<-ergm(tempform,control=control.ergm(
Step.MCMC.samplesize=10000,
main.method="Stepping", MCMLE.metric="lognormal",
Step.gridsize=1000,
Step.maxit=20))
H1n_cons = modtemp
gof_H1n_cons<-gof(H1n_cons,nsim=100,control=control.gof.ergm(MCMC.init.maxedges=2000))

#fit endogenous parameters
net_temp <- net_uq
tempform<-net_temp ~ edges + mutual + twopath + ctriple+ 
gwidegree(0.25, fixed = TRUE)+
 gwodegree(.25,fixed=TRUE)+gwesp(0.25, fixed = TRUE) + nodecov("NUMRESP") + nodecov("NUMGROUPS") + 
nodecov("MEANYEARS")
modtemp<-ergm(tempform,control=control.ergm(
Step.MCMC.samplesize=10000,
main.method="Stepping", MCMLE.metric="lognormal",
Step.gridsize=1000,
Step.maxit=20))
H1n_uq = modtemp
gof_H1n_uq<-gof(H1n_uq,nsim=100,control=control.gof.ergm(MCMC.init.maxedges=2000))




