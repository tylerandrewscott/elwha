
modB <-
    ergm(net~sum+mutual(form="min")+
transitiveweights("min","max","min")+
nodecov("NUMRESP")+nodecov("NUMGROUPS")+nodecov("IPn"),
         response="TVAL", reference=~Binomial(3, exp(-1)/(1+exp(-1))),
         control=control.ergm(init=c(geo.init, rep(0, 5)),
MCMLE.maxit=200,MCMC.runtime.traceplot=F,seed=24,
MCMLE.trustregion=1000,MCMC.addto.se=T,parallel.type="SOCK",
MPLE.max.dyad.types=1e+7,parallel=8,MCMC.samplesize=10000,
MCMC.burnin=100000,MCMC.interval=10000,MCMLE.steplength=.25,
           MCMC.prop.args=list(p0=0.5)),eval.loglik=T)

modF <-
    ergm(net~sum+mutual(form="min")+
transitiveweights("min","max","min")+
nodecov("NUMRESP")+nodecov("NUMGROUPS")+edgecov(dpn,form="sum")+edgecov(dppsp,form="sum"),
         response="TVAL", reference=~
Binomial(3, exp(-1)/(1+exp(-1))),
         control=control.ergm(init=c(geo.init, rep(0, 6)),
MCMLE.maxit=200,MCMC.runtime.traceplot=F,seed=24,
MCMLE.trustregion=1000,MCMC.addto.se=T,parallel.type="SOCK",
MPLE.max.dyad.types=1e+7,parallel=8,MCMC.samplesize=10000,
MCMC.burnin=100000,MCMC.interval=10000,MCMLE.steplength=.25,
           MCMC.prop.args=list(p0=0.5)),eval.loglik=T)

modH <-
    ergm(net~sum+mutual(form="min")+
transitiveweights("min","max","min")+
nodecov("NUMRESP")+nodecov("NUMGROUPS")+edgecov(spn,form="sum")+edgecov(sppsp,form="sum"),
         response="TVAL", reference=~
Binomial(3, exp(-1)/(1+exp(-1))),
         control=control.ergm(init=c(geo.init, rep(0, 6)),
MCMLE.maxit=200,MCMC.runtime.traceplot=F,seed=24,
MCMLE.trustregion=1000,MCMC.addto.se=T,parallel.type="SOCK",
MPLE.max.dyad.types=1e+7,parallel=8,MCMC.samplesize=10000,
MCMC.burnin=100000,MCMC.interval=10000,MCMLE.steplength=.25,
           MCMC.prop.args=list(p0=0.5)),eval.loglik=T)

modG <-
    ergm(net~sum+mutual(form="min")+
transitiveweights("min","max","min")+
nodecov("NUMRESP")+nodecov("NUMGROUPS")+edgecov(spn,form="sum"),
         response="TVAL", reference=~
Binomial(3, exp(-1)/(1+exp(-1))),
         control=control.ergm(init=c(geo.init, rep(0, 5)),
MCMLE.maxit=200,MCMC.runtime.traceplot=F,seed=24,
MCMLE.trustregion=1000,MCMC.addto.se=T,parallel.type="SOCK",
MPLE.max.dyad.types=1e+7,parallel=8,MCMC.samplesize=10000,
MCMC.burnin=100000,MCMC.interval=10000,MCMLE.steplength=.25,
           MCMC.prop.args=list(p0=0.5)),eval.loglik=T)
