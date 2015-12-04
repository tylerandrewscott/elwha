#rm(list=ls())
load("H:/elwha/Poisson_ERGM_Work.RData")
library(statnet)
library(latentnet)
library(ergm.count)
as.mcmc.default <- coda:::as.mcmc.default
as.mcmc.list.default <- coda:::as.mcmc.list.default
summary(mod2)
net<-net_temp
net_temp
setwd("H:/elwha")

psp_group = read.csv(file="Group.Overlap.Matrix.PSP.csv",row.names=1)
all_group = read.csv(file="Group.Overlap.Matrix.csv",row.names=1)
npsp_group = read.csv(file="Group.Overlap.Matrix.NPSP.csv",row.names=1)
fina_up_group=read.csv(file="Group.Fina.Up.Matrix.csv",row.names=1)
fina_down_group=read.csv(file="Group.Fina.Down.Matrix.csv",row.names=1)
huma_up_group=read.csv(file="Group.Huma.Up.Matrix.csv",row.names=1)
huma_down_group=read.csv(file="Group.Huma.Down.Matrix.csv",row.names=1)
valu_up_group=read.csv(file="Group.Valu.Up.Matrix.csv",row.names=1)
valu_down_group=read.csv(file="Group.Value.Down.Matrix.csv",row.names=1)
lang_up_group=read.csv(file="Group.Lang.Up.Matrix.csv",row.names=1)
lang_down_group=read.csv(file="Group.Lang.Down.Matrix.csv",row.names=1)
scie_up_group=read.csv(file="Group.Scie.Up.Matrix.csv",row.names=1)
scie_down_group=read.csv(file="Group.Scie.Down.Matrix.csv",row.names=1)
face_up_group=read.csv(file="Group.Face.Up.Matrix.csv",row.names=1)
face_down_group=read.csv(file="Group.Face.Down.Matrix.csv",row.names=1)

#conway-maxwell-poisson model
pr1 <- length(summary(net~sum, 
response = "TCO"))

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



mod3 <-
    ergm(net~sum+mutual(form="min")+
 transitiveweights("min","max","min"),
         response="TCO", reference=~Poisson,
         control=control.ergm(init=c(init.sum.pois, rep(0, pr3-2)),
MCMLE.density.guard=10,MCMLE.density.guard.min=400,
MCMC.prop.weights="0inflated",MCMLE.maxit=200,MCMC.runtime.traceplot=F,
seed=24,
MCMLE.trustregion=400,
           MCMC.prop.args=list(p0=0.5)),eval.loglik=F)
summary(mod3)

mod4 <-
    ergm(net~sum+mutual(form="min")+
 transitiveweights("min","max","min")+nodecov("NUMRESP"),
         response="TCO", reference=~Poisson,
         control=control.ergm(init=c(init.sum.pois, rep(0, pr3-1)),
MCMLE.density.guard=10,MCMLE.density.guard.min=400,
MCMC.prop.weights="0inflated",MCMLE.maxit=200,MCMC.runtime.traceplot=F,
seed=24,
MCMLE.trustregion=400,
           MCMC.prop.args=list(p0=0.5)),eval.loglik=F)
summary(mod4)


mod5 <-
    ergm(net~sum+mutual(form="min")+
 transitiveweights("min","max","min")+nodecov("NUMRESP")+nodecov("MEANYEARS"),
         response="TCO", reference=~Poisson,
         control=control.ergm(init=c(init.sum.pois, rep(0, pr3)),
MCMLE.density.guard=10,MCMLE.density.guard.min=400,
MCMC.prop.weights="0inflated",MCMLE.maxit=200,MCMC.runtime.traceplot=F,
seed=24,
MCMLE.trustregion=400,
           MCMC.prop.args=list(p0=0.5)),eval.loglik=F)
summary(mod5)

mod6 <-
    ergm(net~sum+mutual(form="min")+
 transitiveweights("min","max","min")+nodecov("NUMRESP")+nodecov("MEANYEARS")+
nodecov("NUMGROUPS"),
         response="TCO", reference=~Poisson,
         control=control.ergm(init=c(init.sum.pois, rep(0, pr3+1)),
MCMLE.density.guard=10,MCMLE.density.guard.min=400,
MCMC.prop.weights="0inflated",MCMLE.maxit=200,MCMC.runtime.traceplot=F,
seed=24,
MCMLE.trustregion=400,
           MCMC.prop.args=list(p0=0.5)),eval.loglik=F)
summary(mod6)

mod7 <-
    ergm(net~sum+mutual(form="min")+
 transitiveweights("min","max","min")+
cyclicalweights("min","max","min")+
nodecov("NUMRESP")+nodecov("MEANYEARS")+
nodecov("NUMGROUPS"),
         response="TCO", reference=~Poisson,
         control=control.ergm(init=c(init.sum.pois, rep(0, pr3+2)),
MCMLE.density.guard=10,MCMLE.density.guard.min=400,
MCMC.prop.weights="0inflated",MCMLE.maxit=200,MCMC.runtime.traceplot=F,
seed=24,
MCMLE.trustregion=400,
           MCMC.prop.args=list(p0=0.5)),eval.loglik=F)
summary(mod7)

# Simulate from model fit:
mutual.sim <-
simulate(mod1, monitor=~mutual(form="min"), nsim = 1000, statsonly=TRUE,
             control=control.simulate.ergm(
               MCMC.prop.weights="0inflated",MCMLE.trustregion=1000,
               MCMC.prop.args=list(p0=0.5) # Should not be necessary in the next version.
               ))

transitive.sim <-
simulate(mod2, monitor=~transitiveweights("min","max","min"), nsim = 1000, statsonly=TRUE,
             control=control.simulate.ergm(
               MCMC.prop.weights="0inflated",MCMLE.trustregion=1000,
               MCMC.prop.args=list(p0=0.5) # Should not be necessary in the next version.
               ))

numresp.sim <-
simulate(mod3, monitor=~nodecov("NUMRESP"), nsim = 1000, statsonly=TRUE,
             control=control.simulate.ergm(
               MCMC.prop.weights="0inflated",MCMLE.trustregion=1000,
               MCMC.prop.args=list(p0=0.5) # Should not be necessary in the next version.
                ))

meanyears.sim <-
simulate(mod4, monitor=~nodecov("MEANYEARS"), nsim = 1000, statsonly=TRUE,
             control=control.simulate.ergm(
               MCMC.prop.weights="0inflated",MCMLE.trustregion=1000,
               MCMC.prop.args=list(p0=0.5) # Should not be necessary in the next version.
               ))  
numgroups.sim <-
simulate(mod5, monitor=~nodecov("NUMGROUPS"), nsim = 1000, statsonly=TRUE,
             control=control.simulate.ergm(
               MCMC.prop.weights="0inflated",MCMLE.trustregion=1000,
               MCMC.prop.args=list(p0=0.5) # Should not be necessary in the next version.
               ))

cyclical.sim <-
simulate(mod6, monitor=~cyclicalweights("min","max","min"), nsim = 1000, statsonly=TRUE,
             control=control.simulate.ergm(
               MCMC.prop.weights="0inflated",MCMLE.trustregion=1000,
               MCMC.prop.args=list(p0=0.5) # Should not be necessary in the next version.
               ))
head(cyclical.sim)
mod8 <-
    ergm(net~sum+mutual(form="min")+
 transitiveweights("min","max","min")+
cyclicalweights("min","max","min")+
nodecov("NUMRESP")+nodecov("MEANYEARS")+
nodecov("NUMGROUPS")+nodecov("USECONS")+nodecov("USEPLAN")+
nodecov("USEWORK"),
         response="TCO", reference=~Poisson,
         control=control.ergm(init=c(init.sum.pois, rep(0, 9)),
MCMLE.density.guard=10,MCMLE.density.guard.min=400,
MCMC.prop.weights="0inflated",MCMLE.maxit=200,MCMC.runtime.traceplot=F,
seed=24,
MCMLE.trustregion=400,
           MCMC.prop.args=list(p0=0.5)),eval.loglik=F)
summary(mod8)


mod9 <-
    ergm(net~sum+mutual(form="min")+
 transitiveweights("min","max","min")+
cyclicalweights("min","max","min")+
nodecov("NUMRESP")+nodecov("MEANYEARS"),
         response="TCO", reference=~Poisson,
         control=control.ergm(init=c(init.sum.pois, rep(0, 5)),
MCMLE.density.guard=10,MCMLE.density.guard.min=400,
MCMC.prop.weights="0inflated",MCMLE.maxit=200,MCMC.runtime.traceplot=F,
seed=24,
MCMLE.trustregion=400,
           MCMC.prop.args=list(p0=0.5)),eval.loglik=F)
summary(mod9)

mod10 <-
    ergm(net~sum+mutual(form="min")+
 transitiveweights("min","max","min")+
cyclicalweights("min","max","min")+
nodecov("NUMRESP")+nodecov("NUMGROUPS"),
         response="TCO", reference=~Poisson,
         control=control.ergm(init=c(init.sum.pois, rep(0, 5)),
MCMLE.density.guard=10,MCMLE.density.guard.min=400,
MCMC.prop.weights="0inflated",MCMLE.maxit=200,MCMC.runtime.traceplot=F,
seed=24,
MCMLE.trustregion=400,
           MCMC.prop.args=list(p0=0.5)),eval.loglik=F)
summary(mod10)

mod11 <-
    ergm(net~sum+mutual(form="min")+
 transitiveweights("min","max","min")+
cyclicalweights("min","max","min")+
nodecov("NUMRESP")+nodecov("USECONS"),
         response="TCO", reference=~Poisson,
         control=control.ergm(init=c(init.sum.pois, rep(0, 5)),
MCMLE.density.guard=10,MCMLE.density.guard.min=400,
MCMC.prop.weights="0inflated",MCMLE.maxit=200,MCMC.runtime.traceplot=F,
seed=24,
MCMLE.trustregion=400,
           MCMC.prop.args=list(p0=0.5)),eval.loglik=F)
summary(mod11)


mod12 <-
    ergm(net~sum+mutual(form="min")+
 transitiveweights("min","max","min")+
cyclicalweights("min","max","min")+
nodecov("NUMRESP")+nodecov("USEPLAN"),
         response="TCO", reference=~Poisson,
         control=control.ergm(init=c(init.sum.pois, rep(0, 5)),
MCMLE.density.guard=10,MCMLE.density.guard.min=400,
MCMC.prop.weights="0inflated",MCMLE.maxit=200,MCMC.runtime.traceplot=F,
seed=24,
MCMLE.trustregion=400,
           MCMC.prop.args=list(p0=0.5)),eval.loglik=F)
summary(mod12)

mod13 <-
    ergm(net~sum+mutual(form="min")+
 transitiveweights("min","max","min")+
cyclicalweights("min","max","min")+
nodecov("NUMRESP")+nodecov("USEWORK"),
         response="TCO", reference=~Poisson,
         control=control.ergm(init=c(init.sum.pois, rep(0, 5)),
MCMLE.density.guard=10,MCMLE.density.guard.min=400,
MCMC.prop.weights="0inflated",MCMLE.maxit=200,MCMC.runtime.traceplot=F,
seed=24,
MCMLE.trustregion=400,
           MCMC.prop.args=list(p0=0.5)),eval.loglik=F)
summary(mod13)

mod14 <-
    ergm(net~sum+mutual(form="min")+
 transitiveweights("min","max","min")+
cyclicalweights("min","max","min")+
nodecov("NUMRESP")+edgecov(as.matrix(all_group)),
         response="TCO", reference=~Poisson,
         control=control.ergm(init=c(init.sum.pois, rep(0, 5)),
MCMLE.density.guard=10,MCMLE.density.guard.min=400,
MCMC.prop.weights="0inflated",MCMLE.maxit=200,MCMC.runtime.traceplot=F,
seed=24,
MCMLE.trustregion=1000,
           MCMC.prop.args=list(p0=0.5)),eval.loglik=F)
summary(mod14)

mod15 <-
    ergm(net~sum+mutual(form="min")+
 transitiveweights("min","max","min")+
cyclicalweights("min","max","min")+
nodecov("NUMRESP")+edgecov(as.matrix(npsp_group)),
         response="TCO", reference=~Poisson,
         control=control.ergm(init=c(init.sum.pois, rep(0, 5)),
MCMLE.density.guard=10,MCMLE.density.guard.min=400,
MCMC.prop.weights="0inflated",MCMLE.maxit=200,MCMC.runtime.traceplot=F,
seed=24,
MCMLE.trustregion=1000,
           MCMC.prop.args=list(p0=0.5)),eval.loglik=F)
summary(mod15)


#mod16 is really good
mod16 <-
    ergm(net~sum+mutual(form="min")+
 transitiveweights("min","max","min")+
cyclicalweights("min","max","min")+
nodecov("NUMRESP")+edgecov(as.matrix(psp_group)),
         response="TCO", reference=~Poisson,
         control=control.ergm(init=c(init.sum.pois, rep(0, 5)),
MCMLE.density.guard=10,MCMLE.density.guard.min=400,
MCMC.prop.weights="0inflated",MCMLE.maxit=200,MCMC.runtime.traceplot=F,
seed=24,
MCMLE.trustregion=1000,MCMLE.steplength=.25,
           MCMC.prop.args=list(p0=0.5)),eval.loglik=F)
summary(mod16)

mod17 <-
    ergm(net~sum+mutual(form="min")+
 transitiveweights("min","max","min")+
cyclicalweights("min","max","min")+
nodecov("NUMRESP")+nodecov("USEPLAN")+nodecov("USEWORK")+nodecov("USECONS")+
edgecov(as.matrix(all_group)),
         response="TCO", reference=~Poisson,
         control=control.ergm(init=c(init.sum.pois, rep(0, 8)),
MCMLE.density.guard=10,MCMLE.density.guard.min=400,
MCMC.prop.weights="0inflated",MCMLE.maxit=250,MCMC.runtime.traceplot=F,
seed=24,
MCMLE.trustregion=1000,MCMLE.steplength=.25,
           MCMC.prop.args=list(p0=0.5)),eval.loglik=F)
summary(mod17)

mod18 <-
    ergm(net~sum+mutual(form="min")+
 transitiveweights("min","max","min")+
cyclicalweights("min","max","min")+
nodecov("NUMRESP")+nodecov("USEPLAN")+nodecov("USEWORK")+nodecov("USECONS"),
         response="TCO", reference=~Poisson,
         control=control.ergm(init=c(init.sum.pois, rep(0, 7)),
MCMLE.density.guard=10,MCMLE.density.guard.min=400,
MCMC.prop.weights="0inflated",MCMLE.maxit=200,MCMC.runtime.traceplot=F,
seed=24,
MCMLE.trustregion=1000,MCMLE.steplength=.25,
           MCMC.prop.args=list(p0=0.5)),eval.loglik=F)
summary(mod18)



control.ergm

summary(mod11)
summary(mod12)
summary(mod13)
summary(mod14)
summary(mod15)
summary(mod16)


mod1<-logLik(mod1, add=TRUE)
mod2<-logLik(mod2, add=TRUE)
mod3<-logLik(mod3, add=TRUE)
mod4<-logLik(mod4, add=TRUE)
mod5<-logLik(mod5, add=TRUE)
mod6<-logLik(mod6, add=TRUE)
mod7<-logLik(mod7, add=TRUE)
mod8<-logLik(mod8, add=TRUE)
mod9<-logLik(mod9, add=TRUE)
mod10<-logLik(mod10, add=TRUE)
mod11<-logLik(mod11, add=TRUE)
mod12<-logLik(mod12, add=TRUE)
mod13<-logLik(mod13, add=TRUE)
mod14<-logLik(mod14, add=TRUE)
mod15<-logLik(mod15, add=TRUE)
mod16<-logLik(mod16, add=TRUE)
mod17<-logLik(mod17, add=TRUE)
mod18<-logLik(mod18, add=TRUE)





