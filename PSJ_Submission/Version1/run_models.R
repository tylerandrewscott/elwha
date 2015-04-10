rm(list=ls())
require(statnet)
require(Rmpi)
#setwd('H:/passtosim')
setwd("//Users/TScott/Google Drive/elwha")
load('NetworkReady.RData')
g <- sum(net %e% "TVAL")/network.dyadcount(net)
init.geo<-log(1 - 1/(g+1))


temp = c(as.sociomatrix(net))


geodists = (geodist(net)[[2]])
dists = c(geodists)


sum(as.sociomatrix(net,'TVAL')==1)
sum(as.sociomatrix(net,'TVAL')==2)
sum(as.sociomatrix(net,'TVAL')==3)

sum(as.sociomatrix(net))/network.dyadcount(net)
sum(as.sociomatrix(net,'TVAL')==1)/network.dyadcount(net)
sum(as.sociomatrix(net,'TVAL')==2)/network.dyadcount(net)
sum(as.sociomatrix(net,'TVAL')==3)/network.dyadcount(net)


deg = degree(net,gmode='digraph')
deg2 = degree(net,gmode='digraph',cmode='indegree')
deg3 = degree(net,gmode='digraph',cmode='outdegree')

summary(deg3)
summary(deg2)

bet = betweenness(net,gmode='graph')
clo = closeness(net,gmode='graph')


# network.density(net)
# network.dyadcount(net)
# network.edgecount(net)
# network.size(net)
# 


test2 = ((degree(net,cmode = 'indegree',gmode='digraph')))
test = (degree(net,cmode = 'outdegree'))
test3 = degree(net,cmode='freeman',gmode='graph')

as.mcmc.default <- coda:::as.mcmc.default
as.mcmc.list.default <- coda:::as.mcmc.list.default

dpx <- dppsp * dpn
spx <- sppsp * spn
spn7 <- spn/7
spx7 <- spx/7
sppsp7 <- sppsp/7
dpn7 <- dpn/7
dpx7 <- dpx/7
dppsp7 <- dppsp/7
ipn_pt<-get.vertex.attribute(net,'IPn7')*as.sociomatrix(net,"PRIOR_TIE")
ippsp_pt<-get.vertex.attribute(net,'IPpsp7')*as.sociomatrix(net,"PRIOR_TIE")
ipx_pt<-get.vertex.attribute(net,'IPx7')*as.sociomatrix(net,"PRIOR_TIE")
dpn_pt<-dpn7*as.sociomatrix(net,"PRIOR_TIE")
dppsp_pt<-dppsp7*as.sociomatrix(net,"PRIOR_TIE")
dpx_pt<-dpx7*as.sociomatrix(net,"PRIOR_TIE")
spn_pt<-spn7*as.sociomatrix(net,"PRIOR_TIE")
sppsp_pt<-sppsp7*as.sociomatrix(net,"PRIOR_TIE")
spx_pt<-spx7*as.sociomatrix(net,"PRIOR_TIE")
all_sp_quad<-((spn7+sppsp7)^2)
all_sp_pt<-((spn7+sppsp7)*as.sociomatrix(net,"PRIOR_TIE"))
all_sp<-(spn7+sppsp7)
install.packages("Rmpi", type="source")
install.packages(file.choose(), repos = NULL, type = "source")
mod_base <-
  ergm(net~sum+mutual(form="min")+
         transitiveweights("min","max","min")+nodecov("NUMRESP")+
         nodecov("NUMGROUPS")+nodecov("MEANYEARS")+nodematch("ORGTYPE",form='sum'),
         #nodecov('allpartdiv7'),
       response="TVAL", reference=~DiscUnif(0,3),
       control=control.ergm(init=c(init.geo, rep(0, 6)),
                            MCMLE.maxit=30,MCMC.runtime.traceplot=F,seed=24,
                            MCMLE.trustregion=1000,MCMC.addto.se=T,parallel.type="SOCK",
                            MPLE.max.dyad.types=1e+7,parallel=8,MCMC.samplesize=40000,
                            MCMC.burnin=15000,MCMC.interval=2000,MCMLE.steplength=.25,
                            MCMC.prop.args=list(p0=0.5)),eval.loglik=T)

mod_base2 <-
  ergm(net~sum+mutual(form="min")+
         transitiveweights("min","max","min")+nodecov("NUMRESP")+
         nodecov("NUMGROUPS")+nodecov("MEANYEARS")+nodematch("ORGTYPE",form='sum'),
       #nodecov('allpartdiv7'),
       response="TVAL", reference=~DiscUnif(0,3),
       control=control.ergm(init=c(init.geo, rep(0, 6)),
                            MCMLE.maxit=30,MCMC.runtime.traceplot=F,seed=24,
                            MCMLE.trustregion=1000,MCMC.addto.se=T,parallel.type="SOCK",
                            MPLE.max.dyad.types=1e+7,parallel=8,MCMC.samplesize=100000,
                            MCMC.burnin=20000,MCMC.interval=2000,MCMLE.steplength=.25,
                            MCMC.prop.args=list(p0=0.5)),eval.loglik=T)



mod_allpart <-
  ergm(net~sum+mutual(form="min")+
         transitiveweights("min","max","min")+nodecov("NUMRESP")+
         nodecov("NUMGROUPS")+nodecov("MEANYEARS")+nodematch("ORGTYPE",form='sum')+
         nodecov('allpartdiv7'),
       response="TVAL", reference=~DiscUnif(0,3),
       control=control.ergm(init=c(geo.init, rep(0, 7)),
                            MCMLE.maxit=30,MCMC.runtime.traceplot=F,seed=24,
                            MCMLE.trustregion=1000,MCMC.addto.se=T,parallel.type="SOCK",
                            MPLE.max.dyad.types=1e+7,parallel=8,MCMC.samplesize=16000,
                            MCMC.burnin=12000,MCMC.interval=1000,MCMLE.steplength=.25,
                            MCMC.prop.args=list(p0=0.5)),eval.loglik=T)

mod_allpartquad <-
  ergm(net~sum+mutual(form="min")+
         transitiveweights("min","max","min")+nodecov("NUMRESP")+
         nodecov("NUMGROUPS")+nodecov("MEANYEARS")+nodematch("ORGTYPE",form='sum')+
         nodecov('allpartdiv7')+nodecov('allpartdiv7quad'),
       response="TVAL", reference=~DiscUnif(0,3),
       control=control.ergm(init=c(geo.init, rep(0, 8)),
                            MCMLE.maxit=30,MCMC.runtime.traceplot=F,seed=24,
                            MCMLE.trustregion=1000,MCMC.addto.se=T,parallel.type="SOCK",
                            MPLE.max.dyad.types=1e+7,parallel=8,MCMC.samplesize=16000,
                            MCMC.burnin=12000,MCMC.interval=1000,MCMLE.steplength=.25,
                            MCMC.prop.args=list(p0=0.5)),eval.loglik=T)

mod_indpart1 <-
  ergm(net~sum+mutual(form="min")+
         transitiveweights("min","max","min")+nodecov("NUMRESP")+
         nodecov("NUMGROUPS")+nodecov("MEANYEARS")+nodematch("ORGTYPE",form='sum')+
         nodecov('IPn7'),
       response="TVAL", reference=~DiscUnif(0,3),
       control=control.ergm(init=c(geo.init, rep(0, 7)),
                            MCMLE.maxit=30,MCMC.runtime.traceplot=F,seed=24,
                            MCMLE.trustregion=1000,MCMC.addto.se=T,parallel.type="SOCK",
                            MPLE.max.dyad.types=1e+7,parallel=4,MCMC.samplesize=16000,
                            MCMC.burnin=10000,MCMC.interval=1000,MCMLE.steplength=.25,
                            MCMC.prop.args=list(p0=0.5)),eval.loglik=T)

mod_indpart2 <-
  ergm(net~sum+mutual(form="min")+
         transitiveweights("min","max","min")+nodecov("NUMRESP")+
         nodecov("NUMGROUPS")+nodecov("MEANYEARS")+nodematch("ORGTYPE",form='sum')+
         nodecov('IPn7')+nodecov('IPpsp7'),
       response="TVAL", reference=~DiscUnif(0,3),
       control=control.ergm(init=c(geo.init, rep(0, 8)),
                            MCMLE.maxit=30,MCMC.runtime.traceplot=F,seed=24,
                            MCMLE.trustregion=1000,MCMC.addto.se=T,parallel.type="SOCK",
                            MPLE.max.dyad.types=1e+7,parallel=4,MCMC.samplesize=16000,
                            MCMC.burnin=10000,MCMC.interval=1000,MCMLE.steplength=.25,
                            MCMC.prop.args=list(p0=0.5)),eval.loglik=T)

mod_indpart3<-
  ergm(net~sum+mutual(form="min")+
         transitiveweights("min","max","min")+nodecov("NUMRESP")+
         nodecov("NUMGROUPS")+nodecov("MEANYEARS")+nodematch("ORGTYPE",form='sum')+
         nodecov('IPn7')+nodecov('IPpsp7')+nodecov('IPx7'),
       response="TVAL", reference=~DiscUnif(0,3),
       control=control.ergm(init=c(geo.init, rep(0, 9)),
                            MCMLE.maxit=30,MCMC.runtime.traceplot=F,seed=24,
                            MCMLE.trustregion=1000,MCMC.addto.se=T,parallel.type="SOCK",
                            MPLE.max.dyad.types=1e+7,parallel=4,MCMC.samplesize=16000,
                            MCMC.burnin=10000,MCMC.interval=1000,MCMLE.steplength=.25,
                            MCMC.prop.args=list(p0=0.5)),eval.loglik=T)

mod_indpart<-mod_indpart3

mod_dirpart <-
    ergm(net~sum+mutual(form="min")+
transitiveweights("min","max","min")+nodecov("NUMRESP")+
nodecov("NUMGROUPS")+nodecov("MEANYEARS")+nodematch("ORGTYPE",form='sum')+
edgecov(dpn7,form='sum')+edgecov(dppsp7,form='sum')+edgecov(dpx7,form='sum'),
         response="TVAL", reference=~DiscUnif(0,3),
         control=control.ergm(init=c(geo.init, rep(0, 9)),
MCMLE.maxit=50,MCMC.runtime.traceplot=F,seed=24,
MCMLE.trustregion=1000,MCMC.addto.se=T,parallel.type="SOCK",
MPLE.max.dyad.types=1e+7,parallel=4,MCMC.samplesize=16000,
MCMC.burnin=10000,MCMC.interval=1000,MCMLE.steplength=.25,
           MCMC.prop.args=list(p0=0.5)),eval.loglik=T)

mod_shapart <-
  ergm(net~sum+mutual(form="min")+
         transitiveweights("min","max","min")+nodecov("NUMRESP")+
         nodecov("NUMGROUPS")+nodecov("MEANYEARS")+nodematch("ORGTYPE",form='sum')+
         edgecov(spn7,form='sum')+edgecov(sppsp7,form='sum')+edgecov(spx7,form='sum'),
       response="TVAL", reference=~DiscUnif(0,3),
       control=control.ergm(init=c(geo.init, rep(0, 9)),
                            MCMLE.maxit=50,MCMC.runtime.traceplot=F,seed=24,
                            MCMLE.trustregion=1000,MCMC.addto.se=T,parallel.type="SOCK",
                            MPLE.max.dyad.types=1e+7,parallel=4,MCMC.samplesize=16000,
                            MCMC.burnin=10000,MCMC.interval=1000,MCMLE.steplength=.25,
                            MCMC.prop.args=list(p0=0.5)),eval.loglik=T)



modpast_base7 <-
  ergm(net~sum+mutual(form="min")+
         transitiveweights("min","max","min")+nodecov("NUMRESP")+
         nodecov("NUMGROUPS")+nodecov("MEANYEARS")+nodematch("ORGTYPE",form='sum')+
         edgecov(net,"PRIOR_TIE",form='sum')+nodecov('IPn7',form='sum')+nodecov('IPpsp7',form='sum')+
         nodecov('IPx7',form='sum'),
       response="TVAL", reference=~DiscUnif(0,3),
       control=control.ergm(init=c(init.geo, rep(0, 10)),
                            MCMLE.maxit=30,MCMC.runtime.traceplot=F,seed=24,
                            MCMLE.trustregion=1000,MCMC.addto.se=T,parallel.type="SOCK",
                            MPLE.max.dyad.types=1e+7,parallel=8,MCMC.samplesize=16000,
                            MCMC.burnin=12000,MCMC.interval=1000,MCMLE.steplength=.25,
                            MCMC.prop.args=list(p0=0.5)),eval.loglik=T)

modpast_allpart7 <-
  ergm(net~sum+mutual(form="min")+
         transitiveweights("min","max","min")+nodecov("NUMRESP")+
         nodecov("NUMGROUPS")+nodecov("MEANYEARS")+nodematch("ORGTYPE",form='sum')+
         edgecov(net,"PRIOR_TIE",form='sum')+edgecov(all_sp,form='sum')+
         edgecov(all_sp_quad,form='sum') +
         edgecov(all_sp_pt,form='sum'),
       response="TVAL", reference=~DiscUnif(0,3),
       control=control.ergm(init=c(init.geo, rep(0, 10)),
                            MCMLE.maxit=70,MCMC.runtime.traceplot=F,seed=24,
                            MCMLE.trustregion=1000,MCMC.addto.se=T,parallel.type="SOCK",
                            MPLE.max.dyad.types=1e+7,parallel=8,MCMC.samplesize=16000,
                            MCMC.burnin=12000,MCMC.interval=1000,MCMLE.steplength=.25,
                            MCMC.prop.args=list(p0=0.5)),eval.loglik=T)


modpast_ip7 <-
  ergm(net~sum+mutual(form="min")+
         transitiveweights("min","max","min")+nodecov("NUMRESP")+
         nodecov("NUMGROUPS")+nodecov("MEANYEARS")+nodematch("ORGTYPE",form='sum')+
         edgecov(net,"PRIOR_TIE",form='sum')+nodecov('IPn7',form='sum')+nodecov('IPpsp7',form='sum')+
         nodecov('IPx7',form='sum')+
         edgecov(ipn_pt,form='sum')+
         edgecov(ippsp_pt,form='sum')+
         edgecov(ipx_pt,form='sum'),
       response="TVAL", reference=~DiscUnif(0,3),
       control=control.ergm(init=c(init.geo, rep(0, 13)),
                            MCMLE.maxit=70,MCMC.runtime.traceplot=F,seed=24,
                            MCMLE.trustregion=1000,MCMC.addto.se=T,parallel.type="SOCK",
                            MPLE.max.dyad.types=1e+7,parallel=8,MCMC.samplesize=16000,
                            MCMC.burnin=12000,MCMC.interval=1000,MCMLE.steplength=.25,
                            MCMC.prop.args=list(p0=0.5)),eval.loglik=T)

modpast_dp7 <-
  ergm(net~sum+mutual(form="min")+
         transitiveweights("min","max","min")+nodecov("NUMRESP")+
         nodecov("NUMGROUPS")+nodecov("MEANYEARS")+nodematch("ORGTYPE",form='sum')+
         edgecov(net,"PRIOR_TIE",form='sum')+
         edgecov(dpn7,form='sum')+edgecov(dppsp7,form='sum')+edgecov(dpx7,form='sum')+
         edgecov(dpn_pt,form='sum')+
         edgecov(dppsp_pt,form='sum')+
         edgecov(dpx_pt,form='sum'),
       response="TVAL", reference=~DiscUnif(0,3),
       control=control.ergm(init=c(init.geo, rep(0, 13)),
                            MCMLE.maxit=70,MCMC.runtime.traceplot=F,seed=24,
                            MCMLE.trustregion=1000,MCMC.addto.se=T,parallel.type="SOCK",
                            MPLE.max.dyad.types=1e+7,parallel=8,MCMC.samplesize=16000,
                            MCMC.burnin=12000,MCMC.interval=1000,MCMLE.steplength=.25,
                            MCMC.prop.args=list(p0=0.5)),eval.loglik=T)

modpast_sp7 <-
  ergm(net~sum+mutual(form="min")+
         transitiveweights("min","max","min")+nodecov("NUMRESP")+
         nodecov("NUMGROUPS")+nodecov("MEANYEARS")+nodematch("ORGTYPE",form='sum')+
         edgecov(net,"PRIOR_TIE",form='sum')+
         edgecov(spn7,form='sum')+edgecov(sppsp7,form='sum')+edgecov(spx7,form='sum')+
         edgecov(spn_pt,form='sum')+
         edgecov(sppsp_pt,form='sum')+
         edgecov(spx_pt,form='sum'),
       response="TVAL", reference=~DiscUnif(0,3),
       control=control.ergm(init=c(init.geo, rep(0, 13)),
                            MCMLE.maxit=70,MCMC.runtime.traceplot=F,seed=24,
                            MCMLE.trustregion=1000,MCMC.addto.se=T,parallel.type="SOCK",
                            MPLE.max.dyad.types=1e+7,parallel=8,MCMC.samplesize=16000,
                            MCMC.burnin=12000,MCMC.interval=1000,MCMLE.steplength=.25,
                            MCMC.prop.args=list(p0=0.5)),eval.loglik=T)

detach(file:NetworkReady.RData)
save.image('model_results.RData')
rm(list=ls())
