library(statnet)


OVER<-(as.matrix(all_group))
OVER.PSP<-(as.matrix(psp_group))
OVER.NPSP<-as.matrix(npsp_group)


mod.settings<-control.ergm(
Step.MCMC.samplesize=10000,
main.method="Stepping", MCMLE.metric="lognormal",
Step.gridsize=1000,
Step.maxit=20,seed=24)


net_temp <- net_plan
tempform2<-net_temp ~ edges + mutual +twopath +isolates +gwidegree(0.25, fixed = TRUE)+
 gwesp(0.25, fixed = TRUE) + nodecov("NUMRESP") + nodecov("NPSP_N")+
edgecov(OVER.NPSP)
modtemp2<-ergm(tempform2,control=mod.settings)
mod_H2n_plan = modtemp2

net_temp <- net_cons
tempform2<-net_temp ~ edges + mutual +twopath +isolates +gwidegree(0.25, fixed = TRUE)+
 gwesp(0.25, fixed = TRUE) + nodecov("NUMRESP")  + nodecov("NPSP_N")+ edgecov(OVER.NPSP)
modtemp2<-ergm(tempform2,control=mod.settings)
mod_H2n_cons = modtemp2

net_temp <- net_imp
tempform2<-net_temp ~ edges + mutual +twopath +isolates +gwidegree(0.25, fixed = TRUE)+
 gwesp(0.25, fixed = TRUE) + nodecov("NUMRESP")  + nodecov("NPSP_N")+ edgecov(OVER.NPSP)
modtemp2<-ergm(tempform2,control=mod.settings)
mod_H2n_imp = modtemp2

net_temp <- net_uq
tempform2<-net_temp ~ edges + mutual +twopath +isolates +gwidegree(0.25, fixed = TRUE)+
 gwesp(0.25, fixed = TRUE) + nodecov("NUMRESP")  + nodecov("NPSP_N")+ edgecov(OVER.NPSP)
modtemp2<-ergm(tempform2,control=mod.settings)
mod_H2n_uq = modtemp2

net_temp <- net_plan
tempform2<-net_temp ~ edges + mutual +twopath +isolates +gwidegree(0.25, fixed = TRUE)+
 gwesp(0.25, fixed = TRUE) + nodecov("NUMRESP")  + nodecov("NPSP_N")+ edgecov(OVER.NPSP)+
edgecov(OVER.PSP)
modtemp2<-ergm(tempform2,control=mod.settings)
mod_H2a_plan = modtemp2

net_temp <- net_cons
tempform2<-net_temp ~ edges + mutual +twopath +isolates +gwidegree(0.25, fixed = TRUE)+
 gwesp(0.25, fixed = TRUE) + nodecov("NUMRESP")  + nodecov("NPSP_N")+ edgecov(OVER.NPSP)+
edgecov(OVER.PSP)
modtemp2<-ergm(tempform2,control=mod.settings)
mod_H2a_cons = modtemp2

net_temp <- net_imp
tempform2<-net_temp ~ edges + mutual +twopath +isolates +gwidegree(0.25, fixed = TRUE)+
 gwesp(0.25, fixed = TRUE) + nodecov("NUMRESP")  + nodecov("NPSP_N")+ edgecov(OVER.NPSP)+
edgecov(OVER.PSP)
modtemp2<-ergm(tempform2,control=mod.settings)
mod_H2a_imp = modtemp2


net_temp <- net_uq
tempform2<-net_temp ~ edges + mutual +twopath +isolates +gwidegree(0.25, fixed = TRUE)+
 gwesp(0.25, fixed = TRUE) + nodecov("NUMRESP")  + nodecov("NPSP_N")+ edgecov(OVER.NPSP)+
edgecov(OVER.PSP)
modtemp2<-ergm(tempform2,control=mod.settings)
mod_H2a_uq = modtemp2



net_temp <- net_uq
tempform2<-net_temp ~ edges + mutual +twopath +isolates +gwidegree(0.25, fixed = TRUE)+
 gwesp(0.25, fixed = TRUE) + nodecov("NUMRESP")  + nodecov("NPSP_N")+ edgecov(OVER.NPSP)+
edgecov(OVER.PSP)+edgecov(OVER.PSP*get.vertex.attribute(net_uq,"NPSP_N"))
modtemp2<-ergm(tempform2,control=mod.settings)
mod_H2a2_uq = modtemp2

net_temp <- net_uq
tempform2<-net_temp ~ edges + mutual +twopath +isolates +gwidegree(0.25, fixed = TRUE)+
 gwesp(0.25, fixed = TRUE) + nodecov("NUMRESP")  + nodecov("NPSP_N")+ edgecov(OVER.NPSP)+
edgecov(OVER.PSP)+edgecov(OVER.PSP*OVER.NPSP)
modtemp2<-ergm(tempform2,control=mod.settings)
mod_H2a3_uq = modtemp2



net_temp <- net_imp
tempform2<-net_temp ~ edges + mutual +twopath +isolates +gwidegree(0.25, fixed = TRUE)+
 gwesp(0.25, fixed = TRUE) + nodecov("NUMRESP")  + nodecov("NPSP_N")+ edgecov(OVER.NPSP)+
edgecov(OVER.PSP)+edgecov(OVER.PSP*get.vertex.attribute(net_uq,"NPSP_N"))
modtemp2<-ergm(tempform2,control=mod.settings)
mod_H2a2_imp = modtemp2



net_temp <- net_imp
tempform2<-net_temp ~ edges + mutual +twopath +isolates +gwidegree(0.25, fixed = TRUE)+
 gwesp(0.25, fixed = TRUE) + nodecov("NUMRESP")  + nodecov("NPSP_N")+ edgecov(OVER.NPSP)+
edgecov(OVER.PSP)+edgecov(OVER.PSP*OVER.NPSP)
modtemp2<-ergm(tempform2,control=mod.settings)
mod_H2a3_imp = modtemp2


net_temp <- net_plan
tempform2<-net_temp ~ edges + mutual +twopath +isolates +gwidegree(0.25, fixed = TRUE)+
 gwesp(0.25, fixed = TRUE) + nodecov("NUMRESP")  + nodecov("NPSP_N")+ edgecov(OVER.NPSP)+
edgecov(OVER.PSP)+edgecov(OVER.PSP*get.vertex.attribute(net_uq,"NPSP_N"))
modtemp2<-ergm(tempform2,control=mod.settings)
mod_H2a2_plan = modtemp2

net_temp <- net_plan
tempform2<-net_temp ~ edges + mutual +twopath +isolates +gwidegree(0.25, fixed = TRUE)+
 gwesp(0.25, fixed = TRUE) + nodecov("NUMRESP")  + nodecov("NPSP_N")+ edgecov(OVER.NPSP)+
edgecov(OVER.PSP)+edgecov(OVER.PSP*OVER.NPSP)
modtemp2<-ergm(tempform2,control=mod.settings)
mod_H2a3_plan = modtemp2


net_temp <- net_cons
tempform2<-net_temp ~ edges + mutual +twopath +isolates +gwidegree(0.25, fixed = TRUE)+
 gwesp(0.25, fixed = TRUE) + nodecov("NUMRESP")  + nodecov("NPSP_N")+ edgecov(OVER.NPSP)+
edgecov(OVER.PSP)+edgecov(OVER.PSP*get.vertex.attribute(net_uq,"NPSP_N"))
modtemp2<-ergm(tempform2,control=mod.settings)
mod_H2a2_cons = modtemp2

net_temp <- net_cons
tempform2<-net_temp ~ edges + mutual +twopath +isolates +gwidegree(0.25, fixed = TRUE)+
 gwesp(0.25, fixed = TRUE) + nodecov("NUMRESP")  + nodecov("NPSP_N")+ edgecov(OVER.NPSP)+
edgecov(OVER.PSP)+edgecov(OVER.PSP*OVER.NPSP)
modtemp2<-ergm(tempform2,control=mod.settings)
mod_H2a3_cons = modtemp2






