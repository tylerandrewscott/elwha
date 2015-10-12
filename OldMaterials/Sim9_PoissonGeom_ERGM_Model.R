load("H:/elwha/Poisson_ERGM_Work.RData")
library(statnet)
library(latentnet)
library(ergm.count)
setwd("H://elwha")
as.mcmc.default <- coda:::as.mcmc.default
as.mcmc.list.default <- coda:::as.mcmc.list.default

net<-net_temp

#Geometric:
m <- sum(net %e% "TVAL")/network.dyadcount(net) 
init.sum.geom <- log(1-1/(m+1))

#binomial model

form0 <-net~sum +nonzero
form1 <-net~sum +nonzero+mutual(form="min",threshold=.9)
form2 <-net~sum +nonzero+mutual(form="min",threshold=.9)+nodesqrtcovar(TRUE)
form3 <-net~sum +nonzero+mutual(form="min",threshold=.9)+nodesqrtcovar(TRUE)+
transitiveweights("min","max","min")
form4 <-net~sum +nonzero+mutual(form="min",threshold=.9)+nodesqrtcovar(TRUE)+
transitiveweights("min","max","min")+cyclicalweights(twopath="min",combine="max",affect="min")
form5 <-net~sum +nonzero+mutual(form="min",threshold=.9)+nodesqrtcovar(TRUE)+
transitiveweights("min","max","min")+nodecov("NUMRESP",form="sum")+
nodecov("NUMGROUPS",form="sum")+
nodecov("MEANYEARS",form="sum")+nodecov("USECONS",form="sum")+
nodecov("USEPLAN",form="sum")+
nodecov("USEWORK",form="sum")


geo.cont0<-control.ergm(seed=24,MCMLE.maxit=40,MCMC.runtime.traceplot=F,
MCMLE.density.guard=30,MCMLE.trustregion=500,
init=c(init.sum.geom,rep(0,1)))
mod0<-ergm(formula=form0,reference=~DiscUnif(0,3),response="TVAL",eval.loglik=F,
control=geo.cont0)

geo.cont1<-control.ergm(seed=24,MCMLE.maxit=40,MCMC.runtime.traceplot=F,
MCMLE.density.guard=30,MCMLE.trustregion=500,
init=c(init.sum.geom,rep(0,2)))
mod1<-ergm(formula=form1,reference=~DiscUnif(0,3),response="TVAL",eval.loglik=F,
control=geo.cont1)

geo.cont2<-control.ergm(seed=24,MCMLE.maxit=40,MCMC.runtime.traceplot=F,
MCMLE.density.guard=30,MCMLE.trustregion=500,
init=c(init.sum.geom,rep(0,3)))
mod2<-ergm(formula=form2,reference=~DiscUnif(0,3),response="TVAL",eval.loglik=F,
control=geo.cont2)

geo.cont3<-control.ergm(seed=24,MCMLE.maxit=40,MCMC.runtime.traceplot=F,
MCMLE.density.guard=30,MCMLE.trustregion=500,
init=c(init.sum.geom,rep(0,4)))
mod3<-ergm(formula=form3,reference=~DiscUnif(0,3),response="TVAL",eval.loglik=F,
control=geo.cont3)

geo.cont4<-control.ergm(seed=24,MCMLE.maxit=40,MCMC.runtime.traceplot=F,
MCMLE.density.guard=30,MCMLE.trustregion=500,
init=c(init.sum.geom,rep(0,5)))
mod4<-ergm(formula=form4,reference=~DiscUnif(0,3),response="TVAL",eval.loglik=F,
control=geo.cont4)

geo.cont5<-control.ergm(seed=24,MCMLE.maxit=40,MCMC.runtime.traceplot=F,
MCMLE.density.guard=30,MCMLE.trustregion=500,
init=c(init.sum.geom,rep(0,10)))
mod5<-ergm(formula=form5,reference=~DiscUnif(0,3),response="TVAL",eval.loglik=F,
control=geo.cont5)

m <- sum(net %e% "TCO")/network.dyadcount(net) 
init.sum.pois <- log(m)
pois.cont0<-control.ergm(seed=24,MCMLE.maxit=40,MCMC.runtime.traceplot=F,
MCMLE.density.guard=30,MCMLE.trustregion=500,MCMC.prop.weights="0inflated",
 MCMC.prop.args=list(p0=.5),
init=c(init.sum.pois,rep(0,1)))
mod0p<-ergm(formula=form0,reference=~Poisson,response="TCO",eval.loglik=F,
control=pois.cont0)

pois.cont1<-control.ergm(seed=24,MCMLE.maxit=40,MCMC.runtime.traceplot=F,
MCMLE.density.guard=30,MCMLE.trustregion=500,MCMC.prop.weights="0inflated",
 MCMC.prop.args=list(p0=.5),
init=c(init.sum.pois,rep(0,2)))
mod1p<-ergm(formula=form1,reference=~Poisson,response="TCO",eval.loglik=F,
control=pois.cont1)

pois.cont2<-control.ergm(seed=24,MCMLE.maxit=40,MCMC.runtime.traceplot=F,
MCMLE.density.guard=30,MCMLE.trustregion=500,MCMC.prop.weights="0inflated",
MCMC.prop.args=list(p0=.5),
init=c(init.sum.pois,rep(0,3)))
mod2p<-ergm(formula=form2,reference=~Poisson,response="TCO",eval.loglik=F,
control=pois.cont2)

pois.cont3<-control.ergm(seed=24,MCMLE.maxit=40,MCMC.runtime.traceplot=F,
MCMLE.density.guard=30,MCMLE.trustregion=500,MCMC.prop.weights="0inflated",
 MCMC.prop.args=list(p0=.5),
init=c(init.sum.pois,rep(0,4)))
mod3p<-ergm(formula=form3,reference=~Poisson,response="TCO",eval.loglik=F,
control=pois.cont3)

pois.cont4<-control.ergm(seed=24,MCMLE.maxit=40,MCMC.runtime.traceplot=F,
MCMLE.density.guard=30,MCMLE.trustregion=500,MCMC.prop.weights="0inflated",
 MCMC.prop.args=list(p0=.5),
init=c(init.sum.pois,rep(0,5)))
mod4p<-ergm(formula=form4,reference=~Poisson,response="TCO",eval.loglik=F,
control=pois.cont4)

pois.cont5<-control.ergm(seed=24,MCMLE.maxit=40,MCMLE.density.guard=30,MCMLE.trustregion=500,
MCMC.prop.weights="0inflated",MCMC.prop.args=list(p0=.5),init=c(init.sum.pois,rep(0,10)))
mod5p<-ergm(formula=form5,reference=~Poisson,response="TCO",eval.loglik=F,
control=pois.cont5)

mod0<-logLik(mod0, add=TRUE)
mod1<-logLik(mod1, add=TRUE)
mod2<-logLik(mod2, add=TRUE)
mod3<-logLik(mod3, add=TRUE)
mod4<-logLik(mod4, add=TRUE)
mod5<-logLik(mod5, add=TRUE)

mod0p<-logLik(mod0p, add=TRUE)
mod1p<-logLik(mod1p, add=TRUE)
mod2p<-logLik(mod2p, add=TRUE)
mod3p<-logLik(mod3p, add=TRUE)
mod4p<-logLik(mod4p, add=TRUE)
mod5p<-logLik(mod5p, add=TRUE)





summary(mod4p)
save.image("COUNT_SIM_RUNS")

library(stargazer)
ls()[grep("mod",ls())]
stargazer(mod4p,mod5p)
