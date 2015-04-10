library(statnet)

#fit endogenous parameters
net_temp <- net_imp
tempform<-net_temp ~ edges + mutual + twopath + ctriple+ gwidegree(0.25, fixed = TRUE)
 + gwodegree(.25,fixed=TRUE)+gwesp(0.25, fixed = TRUE) + nodecov("NUMRESP") + nodecov("NUMGROUPS") + 
nodecov("MEANYEARS")+edgecov(npsp_group_order)
modtemp<-ergm(tempform,control=control.ergm(
Step.MCMC.samplesize=10000,
main.method="Stepping", MCMLE.metric="lognormal",
Step.gridsize=1000,
Step.maxit=20))
H2n_imp = modtemp

#fit endogenous parameters
net_temp <- net_plan
tempform<-net_temp ~ edges + mutual + twopath + ctriple+ gwidegree(0.25, fixed = TRUE)
 + gwodegree(.25,fixed=TRUE)+gwesp(0.25, fixed = TRUE) + nodecov("NUMRESP") + nodecov("NUMGROUPS") + 
nodecov("MEANYEARS")+edgecov(npsp_group_order)
modtemp<-ergm(tempform,control=control.ergm(
Step.MCMC.samplesize=10000,
main.method="Stepping", MCMLE.metric="lognormal",
Step.gridsize=1000,
Step.maxit=20))
H2n_plan = modtemp

net_temp <- net_cons
tempform<-net_temp ~ edges + mutual + twopath + ctriple+ gwidegree(0.25, fixed = TRUE)
 + gwodegree(.25,fixed=TRUE)+gwesp(0.25, fixed = TRUE) + nodecov("NUMRESP") + nodecov("NUMGROUPS") + 
nodecov("MEANYEARS")+edgecov(npsp_group_order)
modtemp<-ergm(tempform,control=control.ergm(
Step.MCMC.samplesize=10000,
main.method="Stepping", MCMLE.metric="lognormal",
Step.gridsize=1000,
Step.maxit=20))
H2n_cons = modtemp

#fit endogenous parameters
net_temp <- net_uq
tempform<-net_temp ~ edges + mutual + twopath + ctriple+ gwidegree(0.25, fixed = TRUE)
 + gwodegree(.25,fixed=TRUE)+gwesp(0.25, fixed = TRUE) + nodecov("NUMRESP") + nodecov("NUMGROUPS") + 
nodecov("MEANYEARS")+edgecov(npsp_group_order)
modtemp<-ergm(tempform,control=control.ergm(
Step.MCMC.samplesize=10000,
main.method="Stepping", MCMLE.metric="lognormal",
Step.gridsize=1000,
Step.maxit=20))
H2n_uq = modtemp


