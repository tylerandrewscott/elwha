#rm(list=ls())

library(statnet)
library(latentnet)
library(ergm.count)
as.mcmc.default <- coda:::as.mcmc.default
as.mcmc.list.default <- coda:::as.mcmc.list.default

net<-net_temp
net_temp


#Poisson:
m <- sum(net %e% "TCO")/network.dyadcount(net) 
init.sum.pois <- log(m)

mod0 <-
    ergm(net~sum,
         response="TCO", reference=~Poisson,
         control=control.ergm(init=c(init.sum.pois, rep(0, pr1-1)),
MCMC.prop.weights="0inflated",MCMLE.maxit=40,MCMC.runtime.traceplot=F,seed=24,
MCMLE.trustregion=1000,
           MCMC.prop.args=list(p0=0.5)),eval.loglik=F)
#mcmc.diagnostics(net.cmpois.nm)
summary(mod0)

# Simulate from model fit:
nonzero.sim <-
simulate(mod0, monitor=~nonzero, nsim = 1000, statsonly=TRUE,
             control=control.simulate.ergm(
               MCMC.prop.weights="0inflated",MCMLE.trustregion=1000,
               MCMC.prop.args=list(p0=0.5) # Should not be necessary in the next version.
               ))

#compute statistic for nonzero observed in the network
nonzero.obs<-summary(net~nonzero,response="TCO")
nonzero.obs
par(mar=c(5, 4, 4, 2) + 0.1)
# 2nd col. = nonzero
plot(density(nonzero.sim[,2]))
abline(v=nonzero.obs)

p.nonzero<-min(mean(nonzero.sim[,2]>nonzero.obs),mean(nonzero.sim[,2]<nonzero.obs))*2
p.nonzero


pr2 <- length(summary(net~sum+nonzero, 
response = "TCO"))

mod1 <-
    ergm(net~sum+nonzero,
         response="TCO", reference=~Poisson,
         control=control.ergm(init=c(init.sum.pois, rep(0, pr2-1)),
MCMC.prop.weights="0inflated",MCMLE.maxit=40,MCMC.runtime.traceplot=F,seed=24,
MCMLE.trustregion=1000,
           MCMC.prop.args=list(p0=0.5)),eval.loglik=F)
#mcmc.diagnostics(net.cmpois.nm)
summary(mod1)



# Simulate from model fit:
cmp.sim <-
simulate(mod1, monitor=~CMP, nsim = 1000, statsonly=TRUE,
             control=control.simulate.ergm(
               MCMC.prop.weights="0inflated",MCMLE.trustregion=1000,
               MCMC.prop.args=list(p0=0.5) # Should not be necessary in the next version.
               ))

#compute statistic for nonzero observed in the network
cmp.obs<-summary(net~CMP,response="TCO")

par(mar=c(5, 4, 4, 2) + 0.1)
# 3nd col. = CMP
plot(density(cmp.sim[,3]))
abline(v=cmp.obs)

p.cmp<-min(mean(cmp.sim[,3]>cmp.obs),mean(cmp.sim[,3]<cmp.obs))*2

# Simulate from model fit:
mutual.sim <-
simulate(mod1, monitor=~mutual(form="min"), nsim = 1000, statsonly=TRUE,
             control=control.simulate.ergm(
               MCMC.prop.weights="0inflated",MCMLE.trustregion=1000,
               MCMC.prop.args=list(p0=0.5) # Should not be necessary in the next version.
               ))

#compute statistic for nonzero observed in the network
mutual.obs<-summary(net~mutual,response="TCO")

par(mar=c(5, 4, 4, 2) + 0.1)
# 3nd col. = mutual
plot(density(mutual.sim[,3]))
abline(v=mutual.obs)

p.mutual<-min(mean(mutual.sim[,3]>mutual.obs),mean(mutual.sim[,3]<mutual.obs))*2

p.mutual

#select mutual, add into model
pr3 <- length(summary(net~sum+nonzero+mutual(form="min"), 
response = "TCO"))

mod2 <-
    ergm(net~sum+nonzero+mutual(form="min"),
         response="TCO", reference=~Poisson,
         control=control.ergm(init=c(init.sum.pois, rep(0, pr3-1)),
MCMC.prop.weights="0inflated",MCMLE.maxit=100,MCMC.runtime.traceplot=F,seed=24,
MCMLE.trustregion=1000,
           MCMC.prop.args=list(p0=0.5)),eval.loglik=F)
summary(mod2)


# Simulate from model fit:
transitiveweights.sim <-
simulate(mod2, monitor=~transitiveweights("min","max","min"), nsim = 1000, statsonly=TRUE,
             control=control.simulate.ergm(
               MCMC.prop.weights="0inflated",MCMLE.trustregion=1000,
               MCMC.prop.args=list(p0=0.5) # Should not be necessary in the next version.
               ))

#compute statistic for nonzero observed in the network
transitiveweights.obs<-summary(net~transitiveweights("min","max","min"),response="TCO")

par(mar=c(5, 4, 4, 2) + 0.1)
# 4th col. = transitiveweights("min","max","min")
plot(density(transitiveweights.sim[,4]))
abline(v=transitiveweights.obs)

p.transitiveweights<-min(mean(transitiveweights.sim[,3]>transitiveweights.obs),
mean(transitiveweights.sim[,3]<transitiveweights.obs))*2

#select transitiveweights, add to model
pr3 <- length(summary(net~sum+mutual(form="min")+ 
transitiveweights("geomean","sum","geomean")+
cyclicalweights(twopath="min",combine="max",affect="min"), 
response = "TCO"))

mod3 <-
    ergm(net~sum+mutual(form="min")+ transitiveweights("min","max","min")+
cyclicalweights(twopath="min",combine="max",affect="min"),
         response="TCO", reference=~Poisson,
         control=control.ergm(init=c(init.sum.pois, rep(0, pr3-1)),MCMLE.density.guard=10,MCMLE.density.guard.min=400,
MCMC.prop.weights="0inflated",MCMLE.maxit=100,MCMC.runtime.traceplot=F,seed=24,
MCMLE.trustregion=100,
           MCMC.prop.args=list(p0=0.5)),eval.loglik=F)
summary(mod3)


mod4 <-
    ergm(net~sum+mutual(form="min")+ transitiveweights("min","max","min")+
cyclicalweights(twopath="min",combine="max",affect="min")+nodecov("NUMRESP"),
         response="TCO", reference=~Poisson,
         control=control.ergm(init=c(init.sum.pois, rep(0, pr3)),MCMLE.density.guard=10,MCMLE.density.guard.min=400,
MCMC.prop.weights="0inflated",MCMLE.maxit=100,MCMC.runtime.traceplot=F,seed=24,
MCMLE.trustregion=100,
           MCMC.prop.args=list(p0=0.5)),eval.loglik=F)
summary(mod4)

mod5 <-
    ergm(net~sum+mutual(form="min")+ transitiveweights("min","max","min")+
cyclicalweights(twopath="min",combine="max",affect="min")+nodecov("NUMRESP")+
nodecov("MEANYEARS")+nodecov("NUMGROUPS"),
         response="TCO", reference=~Poisson,
         control=control.ergm(init=c(init.sum.pois, rep(0, pr3+2)),MCMLE.density.guard=10,MCMLE.density.guard.min=400,
MCMC.prop.weights="0inflated",MCMLE.maxit=100,MCMC.runtime.traceplot=F,seed=24,
MCMLE.trustregion=100,
           MCMC.prop.args=list(p0=0.5)),eval.loglik=F)
summary(mod5)

mod6 <-
    ergm(net~sum+mutual(form="min")+ transitiveweights("min","max","min")+
cyclicalweights(twopath="min",combine="max",affect="min")+CMP+nodecov("NUMRESP")+
nodecov("MEANYEARS")+nodecov("NUMGROUPS"),
         response="TCO", reference=~Poisson,
         control=control.ergm(init=c(init.sum.pois, rep(0, pr3+3)),MCMLE.density.guard=10,MCMLE.density.guard.min=400,
MCMC.prop.weights="0inflated",MCMLE.maxit=100,MCMC.runtime.traceplot=F,seed=24,
MCMLE.trustregion=100,
           MCMC.prop.args=list(p0=0.5)),eval.loglik=F)
summary(mod6)

mod7 <-
    ergm(net~sum(pow=1/2)+mutual(form="min")+ transitiveweights("min","max","min")+
cyclicalweights(twopath="min",combine="max",affect="min")+nodecov("NUMRESP")+
nodecov("MEANYEARS")+nodecov("NUMGROUPS"),
         response="TCO", reference=~Poisson,
         control=control.ergm(init=c(init.sum.pois, rep(0, pr3+2)),MCMLE.density.guard=10,MCMLE.density.guard.min=400,
MCMC.prop.weights="0inflated",MCMLE.maxit=100,MCMC.runtime.traceplot=F,seed=24,
MCMLE.trustregion=100,
           MCMC.prop.args=list(p0=0.5)),eval.loglik=F)
summary(mod7)

mod8 <-
    ergm(net~sum(pow=1/2)+mutual(form="min")+ transitiveweights("min","max","min")+
cyclicalweights(twopath="min",combine="max",affect="min")+
nodecov("NUMRESP")+
nodecov("MEANYEARS"),
         response="TCO", reference=~Poisson,
         control=control.ergm(init=c(init.sum.pois, rep(0, pr3+1)),MCMLE.density.guard=10,MCMLE.density.guard.min=400,
MCMC.prop.weights="0inflated",MCMLE.maxit=100,MCMC.runtime.traceplot=F,seed=24,
MCMLE.trustregion=1000,MCMLE.steplength=500,
           MCMC.prop.args=list(p0=0.5)),eval.loglik=F)
summary(mod8)

mod9 <-
    ergm(net~sum(pow=1/2)+mutual(form="min")+ transitiveweights("min","max","min")+
cyclicalweights(twopath="min",combine="max",affect="min")+
nodecov("NUMRESP")+
nodecov("NUMGROUPS"),
         response="TCO", reference=~Poisson,
         control=control.ergm(init=c(init.sum.pois, rep(0, pr3+1)),
MCMLE.density.guard=10,MCMLE.density.guard.min=400,
MCMC.prop.weights="0inflated",MCMLE.maxit=200,
MCMC.runtime.traceplot=F,seed=24,
MCMLE.trustregion=1000,
           MCMC.prop.args=list(p0=0.5)),eval.loglik=F)
summary(mod9)


mod10 <-
    ergm(net~sum(pow=1/2)+mutual(form="min")+ transitiveweights("min","max","min")+
cyclicalweights(twopath="min",combine="max",affect="min")+
nodecov("NUMRESP")+
nodecov("NUMGROUPS")+nodecov("MEANYEARS"),
         response="TCO", reference=~Poisson,
         control=control.ergm(init=c(init.sum.pois, rep(0, pr3+1)),
MCMLE.density.guard=10,MCMLE.density.guard.min=400,
MCMC.prop.weights="0inflated",MCMLE.maxit=200,
MCMC.runtime.traceplot=F,seed=24,
MCMLE.trustregion=1000,
           MCMC.prop.args=list(p0=0.5)),eval.loglik=F)
summary(mod10)
ls()[grep("mod", ls())]
summary(mod0)

 mod0<-logLik(mod0, add=TRUE)
 mod1<-logLik(mod1, add=TRUE)
 mod2<-logLik(mod2, add=TRUE)
 mod3<-logLik(mod3, add=TRUE)
 mod4<-logLik(mod4, add=TRUE)
 mod5<-logLik(mod5, add=TRUE)
 mod6<-logLik(mod6, add=TRUE)
 mod7<-logLik(mod7, add=TRUE)
 mod8<-logLik(mod8, add=TRUE)
 mod9<-logLik(mod9, add=TRUE)

mcmc.diagnostics(mod2,vars.per.page=4)
summary(mod2)


