

rm(list=ls())
load("Sim4_Poisson_Workspace.RData")  
library(statnet) 
library(latentnet) 
library(ergm.count) 
as.mcmc.default <- coda:::as.mcmc.default 
as.mcmc.list.default <- coda:::as.mcmc.list.default  
net<-net_temp   
#Poisson: 
m <- sum(net %e% "TCO")/network.dyadcount(net)  
init.sum.pois <- log(m)  



mod0 <-     
ergm(net~sum,  response="TCO", reference=~Poisson,
control=control.ergm(init=c(init.sum.pois, rep(0, 0)), 
MCMC.prop.weights="0inflated",MCMLE.maxit=40,MCMC.runtime.traceplot=F,seed=24, 
MCMLE.trustregion=1000,           
 MCMC.prop.args=list(p0=0.5)),eval.loglik=T)

mod1 <-
    ergm(net~sum+mutual(form="min"),
         response="TCO", reference=~Poisson,
         control=control.ergm(init=c(init.sum.pois, rep(0, 1)),
MCMC.prop.weights="0inflated",MCMLE.maxit=40,MCMC.runtime.traceplot=F,seed=24,
MCMLE.trustregion=1000,
           MCMC.prop.args=list(p0=0.5)),eval.loglik=T) 
mutual.sim <-
simulate(mod0, monitor=~mutual(form="min"), nsim = 1000, statsonly=TRUE,
             control=control.simulate.ergm(
               MCMC.prop.weights="0inflated",MCMLE.trustregion=1000,
               MCMC.prop.args=list(p0=0.5) # Should not be necessary in the next version.
               ))        

mutual.obs<-summary(net~mutual(form="min"),response="TCO")
par(mar=c(5, 4, 4, 2) + 0.1)
# 2nd col. = mutual
plot(density(mutual.sim[,2]))
abline(v=mutual.obs)
p.mutual<-min(mean(mutual.sim[,2]>mutual.obs),mean(mutual.sim[,2]<mutual.obs))*2
   

mod2 <-
    ergm(net~sum+mutual(form="min")+transitiveweights("min","max","min"),
         response="TCO", reference=~Poisson,
         control=control.ergm(init=c(init.sum.pois, rep(0, 2)),
MCMC.prop.weights="0inflated",MCMLE.maxit=40,MCMC.runtime.traceplot=F,seed=24,
MCMLE.trustregion=1000,
           MCMC.prop.args=list(p0=0.5)),eval.loglik=T)   
transitive.sim <-
simulate(mod1, monitor=~transitiveweights("min","max","min"), nsim = 1000, statsonly=TRUE,
             control=control.simulate.ergm(
               MCMC.prop.weights="0inflated",MCMLE.trustregion=1000,
               MCMC.prop.args=list(p0=0.5) # Should not be necessary in the next version.
               ))         

transitive.obs<-summary(net~transitiveweights("min","max","min"),response="TCO")
par(mar=c(5, 4, 4, 2) + 0.1)
# 3rd col. = transitive
plot(density(transitive.sim[,3]))
abline(v=transitive.obs)
p.transitive<-min(mean(transitive.sim[,3]>transitive.obs),mean(transitive.sim[,3]<transitive.obs))*2
   

mod3 <-
    ergm(net~sum+mutual(form="min")+transitiveweights("min","max","min")+
    cyclicalweights("min","max","min"),
         response="TCO", reference=~Poisson,
         control=control.ergm(init=c(init.sum.pois, rep(0, 3)),
MCMC.prop.weights="0inflated",MCMLE.maxit=40,MCMC.runtime.traceplot=F,seed=24,
MCMLE.trustregion=1000,
           MCMC.prop.args=list(p0=0.5)),eval.loglik=T)  
cyclical.sim <-
simulate(mod2, monitor=~cyclicalweights("min","max","min"), nsim = 1000, statsonly=TRUE,
             control=control.simulate.ergm(
               MCMC.prop.weights="0inflated",MCMLE.trustregion=1000,
               MCMC.prop.args=list(p0=0.5) # Should not be necessary in the next version.
               ))    

cyclical.obs<-summary(net~cyclicalweights("min","max","min"),response="TCO")
par(mar=c(5, 4, 4, 2) + 0.1)
# 4th col. = cyclical
plot(density(cyclical.sim[,4]))
abline(v=cyclical.obs)
p.cyclical<-min(mean(cyclical.sim[,4]>cyclical.obs),mean(cyclical.sim[,2]<cyclical.obs))*2
   
mod4 <-
    ergm(net~sum+mutual(form="min")+transitiveweights("min","max","min")+
    cyclicalweights("min","max","min")+nodecov("NUMRESP"),
         response="TCO", reference=~Poisson,
         control=control.ergm(init=c(init.sum.pois, rep(0, 4)),
MCMC.prop.weights="0inflated",MCMLE.maxit=40,MCMC.runtime.traceplot=F,seed=24,
MCMLE.trustregion=1000,
           MCMC.prop.args=list(p0=0.5)),eval.loglik=T)             
numresp.sim <-
simulate(mod3, monitor=~nodecov("NUMRESP"), nsim = 1000, statsonly=TRUE,
             control=control.simulate.ergm(
               MCMC.prop.weights="0inflated",MCMLE.trustregion=1000,
               MCMC.prop.args=list(p0=0.5) # Should not be necessary in the next version.
               ))

numresp.obs<-summary(net~nodecov("NUMRESP"),response="TCO")
par(mar=c(5, 4, 4, 2) + 0.1)
# 5th col. = numresp
plot(density(numresp.sim[,5]))
abline(v=numresp.obs)
p.numresp<-min(mean(numresp.sim[,5]>numresp.obs),mean(numresp.sim[,5]<numresp.obs))*2

#######################################
########################################
###################################

net<-net_temp

all_group

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


head(as.matrix(all_group))
pr2 <- length(summary(net~sum+nonzero, 
response = "TCO"))

mod1 <-
    ergm(net~sum+mutual(form="min")+nodecov("NUMRESP")+
edgecov(as.matrix(all_group)),
         response="TCO", reference=~Poisson,
         control=control.ergm(init=c(init.sum.pois, rep(0, 3)),
MCMC.prop.weights="0inflated",MCMLE.maxit=200,MCMC.runtime.traceplot=F,seed=24,
MCMLE.trustregion=1000,
           MCMC.prop.args=list(p0=0.5)),eval.loglik=F)

mod2 <-
    ergm(net~sum+mutual(form="min")+transitiveweights("min","max","min")+
nodecov("NUMRESP")+
edgecov(as.matrix(all_group)),
         response="TCO", reference=~Poisson,
         control=control.ergm(init=c(init.sum.pois, rep(0, 4)),
MCMC.prop.weights="0inflated",MCMC.burnin=10000,MCMLE.maxit=200,MCMC.runtime.traceplot=F,seed=24,
MCMLE.trustregion=1000,
           MCMC.prop.args=list(p0=0.5)),eval.loglik=F
)

mod3 <-
    ergm(net~sum+mutual(form="min")+transitiveweights("min","max","min")+
nodesqrtcovar(TRUE)+
nodecov("NUMRESP")+
edgecov(as.matrix(all_group)),
         response="TCO", reference=~Poisson,
         control=control.ergm(init=c(init.sum.pois, rep(0, 5)),
MCMC.max.maxedges=5000,MCMLE.steplength=.2,
MCMC.prop.weights="0inflated",MCMC.burnin=10000,MCMLE.maxit=100,MCMC.runtime.traceplot=F,seed=24,
MCMLE.trustregion=1000,
           MCMC.prop.args=list(p0=0.5)),eval.loglik=F
)
mod3<-logLik(mod3,add=T)
library(stargazer)
stargazer(mod3)
mod4 <-
    ergm(net~sum+mutual(form="min")+transitiveweights("min","max","min")+
cyclicalweights("min","max","min")+
nodecov("NUMRESP")+
edgecov(as.matrix(all_group)),
         response="TCO", reference=~Poisson,
         control=control.ergm(init=c(init.sum.pois, rep(0, 5)),
MCMC.prop.weights="0inflated",MCMC.burnin=10000,MCMLE.maxit=100,MCMLE.steplength=.2,
MCMC.runtime.traceplot=F,seed=24,MCMC.max.maxedges=5000,
MCMLE.trustregion=1000,
           MCMC.prop.args=list(p0=0.5)),eval.loglik=F
)



 mod1<-logLik(mod1, add=TRUE)
mod2<-logLik(mod2, add=TRUE)
 mod4<-logLik(mod4, add=TRUE)



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
