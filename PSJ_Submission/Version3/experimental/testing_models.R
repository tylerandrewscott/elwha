#rm(list=ls())
#rm(list=ls())

require(statnet)
library(snow)
library(rlecuyer)

#setwd('H:/passtosim')
#setwd("//Users/TScott/Google Drive/elwha")
#setwd('H:/elwha/Dissert_Scripts/')

setwd('/homes/tscott1/win/user/elwha/PSJ_Submission/Version3/experimental/')
load('NetworkReady.RData')
#load('Ready_to_ERGM.RData')
g <- sum(net %e% "TVAL")/network.dyadcount(net)
init.geo<-log(1 - 1/(g+1))
geo.init = init.geo

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
central.actors = c('Federal_Agency','State_Agency','Regional_Commission')
set.vertex.attribute(net,attrname = 'Central',
                     value = ifelse(get.vertex.attribute(net,'ORGTYPE') %in% central.actors,1,0))



search.ergmTerms(net = net,keyword='valued')


form_base1 = net~sum+
  mutual(form="geometric") +
  transitiveweights(twopath="min",combine="max",affect="min")+
  nodecov("NUMRESP",form='sum')+
  nodecov("NUMGROUPS",form='sum')+
  nodecov("MEANYEARS",form='sum')+
  nodefactor('Central')+
  nodematch("ORGTYPE",form='sum')

form_base2 = net~sum+
  mutual(form="min") +
  transitiveweights(twopath="min",combine="max",affect="min")+
  nodecov("NUMRESP",form='sum')+
  nodecov("NUMGROUPS",form='sum')+
  nodecov("MEANYEARS",form='sum')+
  nodefactor('Central')+
  nodematch("ORGTYPE",form='sum')

form_base2 = net~sum+
  mutual(form="min") +
  transitiveweights(twopath="min",combine="max",affect="min")+
  nodecov("NUMRESP",form='sum')+
  nodecov("NUMGROUPS",form='sum')+
  nodecov("MEANYEARS",form='sum')+
  nodefactor('Central')+
  nodematch("ORGTYPE",form='sum')


# 
# form3 = net~sum+
#   mutual(form="min")+
#   transitiveweights(twopath="min",combine="max",affect="min")+
#   cyclicalweights(twopath="min",combine="max",affect="min") + 
#  nodesqrtcovar(TRUE)
#   
# form4 = net~sum+
#   mutual(form="min")+
#   transitiveweights(twopath="min",combine="max",affect="min")+
#   cyclicalweights(twopath="min",combine="max",affect="min")+
#   nodesqrtcovar(TRUE)
# 
# form5 = net~sum+
#   mutual(form="min")+
#   transitiveweights(twopath="min",combine="max",affect="min")+
#   nodesqrtcovar(TRUE)
# 
# form6 = net~sum+
#   mutual(form="min")+
#   transitiveweights(twopath="min",combine="max",affect="min")+
#   cyclicalweights(twopath="min",combine="max",affect="min")+
#   nodesqrtcovar(TRUE)+
# nodecov("NUMRESP",form='sum')+
#   nodecov("NUMGROUPS",form='sum')+
#   nodecov("MEANYEARS",form='sum')
# 
# form7 = net~sum+
#   mutual(form="min")+
#   transitiveweights(twopath="min",combine="max",affect="min")+
#   cyclicalweights(twopath="min",combine="max",affect="min")+
#   nodesqrtcovar(TRUE)+
#   nodecov("NUMRESP",form='sum')+
#   nodecov("NUMGROUPS",form='sum')+
#   nodecov("MEANYEARS",form='sum')+
# nodefactor('Central')
# 
# form8 = net~sum(pow=.5)+
#   mutual(form="min")+
#   transitiveweights(twopath="min",combine="max",affect="min")+
#   nodesqrtcovar(TRUE)
# 
# form9 = net~sum(pow=.5)+
#   mutual(form="min")+
#   transitiveweights(twopath="min",combine="max",affect="min")

# npar1 = length(summary(form1,response='TVAL'))-1
# npar2 = length(summary(form2,response='TVAL'))-1
# npar3 = length(summary(form3,response='TVAL'))-1
# npar4 = length(summary(form4,response='TVAL'))-1
# npar5 = length(summary(form5,response='TVAL'))-1
# npar6 = length(summary(form6,response='TVAL'))-1
# npar7 = length(summary(form7,response='TVAL'))-1
# npar8 = length(summary(form8,response='TVAL'))-1
# npar9 = length(summary(form9,response='TVAL'))-1

npar = length(summary(form_unrestricted,response='TVAL'))-1


mod_ur <-
  ergm(form_unrestricted,
       response="TVAL", reference=~DiscUnif(0,3),
       control=control.ergm(init=c(geo.init, rep(0, npar)),
                            MCMLE.maxit=30,MCMC.runtime.traceplot=F,seed=24,
                            MCMLE.trustregion=1000,MCMC.addto.se=T,
                            parallel.type="SOCK",parallel=8,
                            MPLE.max.dyad.types=1e+7,MCMC.samplesize=50000,
                            MCMC.burnin=15000,MCMC.interval=1500,
                            MCMLE.steplength=.25,
                            MCMC.prop.args=list(p0=0.5)),eval.loglik=F,verbose=T)

save.image('results_unrestricted_parallel.RData')
# 
# mod2 <-
#   ergm(form2,
#        response="TVAL", reference=~DiscUnif(0,3),
#        control=control.ergm(init=c(geo.init, rep(0, npar2)),
#                             MCMLE.maxit=30,MCMC.runtime.traceplot=F,seed=24,
#                             MCMLE.trustregion=1000,MCMC.addto.se=T,
#                             #  parallel.type="SOCK",parallel=8,
#                             MPLE.max.dyad.types=1e+7,MCMC.samplesize=10000,
#                             MCMC.burnin=10000,MCMC.interval=1000,
#                             MCMLE.steplength=.25,
#                             MCMC.prop.args=list(p0=0.5)),eval.loglik=F,verbose=T)
# 
# save.image('results_modtesting.RData')
# 
# mod3 <-
#   ergm(form3,
#        response="TVAL", reference=~DiscUnif(0,3),
#        control=control.ergm(init=c(geo.init, rep(0, npar3)),
#                             MCMLE.maxit=30,MCMC.runtime.traceplot=F,seed=24,
#                             MCMLE.trustregion=1000,MCMC.addto.se=T,
#                             #  parallel.type="SOCK",parallel=8,
#                             MPLE.max.dyad.types=1e+7,MCMC.samplesize=10000,
#                             MCMC.burnin=10000,MCMC.interval=1000,
#                             MCMLE.steplength=.25,
#                             MCMC.prop.args=list(p0=0.5)),eval.loglik=F,verbose=T)
# 
# save.image('results_modtesting.RData')
# 
# mod4 <-
#   ergm(form4,
#        response="TVAL", reference=~DiscUnif(0,3),
#        control=control.ergm(init=c(geo.init, rep(0, npar4)),
#                             MCMLE.maxit=30,MCMC.runtime.traceplot=F,seed=24,
#                             MCMLE.trustregion=1000,MCMC.addto.se=T,
#                             #  parallel.type="SOCK",parallel=8,
#                             MPLE.max.dyad.types=1e+7,MCMC.samplesize=10000,
#                             MCMC.burnin=10000,MCMC.interval=1000,
#                             MCMLE.steplength=.25,
#                             MCMC.prop.args=list(p0=0.5)),eval.loglik=F,verbose=T)
# 
# save.image('results_modtesting.RData')
# mod5 <-
#   ergm(form5,
#        response="TVAL", reference=~DiscUnif(0,3),
#        control=control.ergm(init=c(geo.init, rep(0, npar5)),
#                             MCMLE.maxit=30,MCMC.runtime.traceplot=F,seed=24,
#                             MCMLE.trustregion=1000,MCMC.addto.se=T,
#                             #  parallel.type="SOCK",parallel=8,
#                             MPLE.max.dyad.types=1e+7,MCMC.samplesize=10000,
#                             MCMC.burnin=10000,MCMC.interval=1000,
#                             MCMLE.steplength=.25,
#                             MCMC.prop.args=list(p0=0.5)),eval.loglik=F,verbose=T)
# 
# mod6 <-
#   ergm(form6,
#        response="TVAL", reference=~DiscUnif(0,3),
#        control=control.ergm(init=c(geo.init, rep(0, npar6)),
#                             MCMLE.maxit=30,MCMC.runtime.traceplot=F,seed=24,
#                             MCMLE.trustregion=1000,MCMC.addto.se=T,
#                             #  parallel.type="SOCK",parallel=8,
#                             MPLE.max.dyad.types=1e+7,MCMC.samplesize=10000,
#                             MCMC.burnin=10000,MCMC.interval=1000,
#                             MCMLE.steplength=.25,
#                             MCMC.prop.args=list(p0=0.5)),eval.loglik=F,verbose=T)
# 
# mod7 <-
#   ergm(form7,
#        response="TVAL", reference=~DiscUnif(0,3),
#        control=control.ergm(init=c(geo.init, rep(0, npar7)),
#                             MCMLE.maxit=30,MCMC.runtime.traceplot=F,seed=24,
#                             MCMLE.trustregion=1000,MCMC.addto.se=T,
#                             #  parallel.type="SOCK",parallel=8,
#                             MPLE.max.dyad.types=1e+7,MCMC.samplesize=10000,
#                             MCMC.burnin=10000,MCMC.interval=1000,
#                             MCMLE.steplength=.25,
#                             MCMC.prop.args=list(p0=0.5)),eval.loglik=F,verbose=T)
# 
# mod8 <-
#   ergm(form8,
#        response="TVAL", reference=~DiscUnif(0,3),
#        control=control.ergm(init=c(geo.init, rep(0, npar8)),
#                             MCMLE.maxit=30,MCMC.runtime.traceplot=F,seed=24,
#                             MCMLE.trustregion=1000,MCMC.addto.se=T,
#                             #  parallel.type="SOCK",parallel=8,
#                             MPLE.max.dyad.types=1e+7,MCMC.samplesize=10000,
#                             MCMC.burnin=10000,MCMC.interval=1000,
#                             MCMLE.steplength=.25,
#                             MCMC.prop.args=list(p0=0.5)),eval.loglik=F,verbose=T)
# 
# mod9 <-
#   ergm(form9,
#        response="TVAL", reference=~DiscUnif(0,3),
#        control=control.ergm(init=c(geo.init, rep(0, npar9)),
#                             MCMLE.maxit=30,MCMC.runtime.traceplot=F,seed=24,
#                             MCMLE.trustregion=1000,MCMC.addto.se=T,
#                             #  parallel.type="SOCK",parallel=8,
#                             MPLE.max.dyad.types=1e+7,MCMC.samplesize=10000,
#                             MCMC.burnin=10000,MCMC.interval=1000,
#                             MCMLE.steplength=.25,
#                             MCMC.prop.args=list(p0=0.5)),eval.loglik=F,verbose=T)



# 
# npar = length(summary(form,response='TVAL'))-1
# 
# mod_base <-
#   ergm(form,
#        response="TVAL", reference=~DiscUnif(0,3),
#        control=control.ergm(init=c(geo.init, rep(0, npar)),
#                             MCMLE.maxit=30,MCMC.runtime.traceplot=F,seed=24,
#                             MCMLE.trustregion=1000,MCMC.addto.se=T,
#                             parallel.type="SOCK",parallel=8,
#                             MPLE.max.dyad.types=1e+7,MCMC.samplesize=50000,
#                             MCMC.burnin=15000,MCMC.interval=1500,MCMLE.steplength=.25,
#                             MCMC.prop.args=list(p0=0.5)),eval.loglik=F,verbose=T)
# 



# mod_indpart3<-
#   ergm(net~sum+mutual(form="min")+
#          transitiveweights("min","max","min")+nodecov("NUMRESP")+
#          nodecov("NUMGROUPS")+nodecov("MEANYEARS")+nodematch("ORGTYPE",form='sum')+
#          nodecov('IPn7')+nodecov('IPpsp7')+nodecov('IPx7'),
#        response="TVAL", reference=~DiscUnif(0,3),
#        control=control.ergm(init=c(geo.init, rep(0, 9)),
#                             MCMLE.maxit=30,MCMC.runtime.traceplot=F,seed=24,
#                             MCMLE.trustregion=1000,MCMC.addto.se=T,parallel.type="SOCK",
#                             MPLE.max.dyad.types=1e+7,parallel=4,MCMC.samplesize=16000,
#                             MCMC.burnin=10000,MCMC.interval=1000,MCMLE.steplength=.25,
#                             MCMC.prop.args=list(p0=0.5)),eval.loglik=F)
# 
# mod_indpart<-mod_indpart3
# 
# mod_dirpart <-
#     ergm(net~sum+mutual(form="min")+
# transitiveweights("min","max","min")+nodecov("NUMRESP")+
# nodecov("NUMGROUPS")+nodecov("MEANYEARS")+nodematch("ORGTYPE")+
# edgecov(dpn7,form='sum')+edgecov(dppsp7,form='sum')+edgecov(dpx7,form='sum'),
#          response="TVAL", reference=~DiscUnif(0,3),
#          control=control.ergm(init=c(geo.init, rep(0, 9)),
#                               MCMLE.maxit=30,MCMC.runtime.traceplot=F,seed=24,
#                               MCMLE.trustregion=1000,MCMC.addto.se=T,
#                               parallel.type="MPI",parallel=8,
#                               MPLE.max.dyad.types=1e+7,MCMC.samplesize=50000,
#                               MCMC.burnin=15000,MCMC.interval=1500,MCMLE.steplength=.25,
#                               MCMC.prop.args=list(p0=0.5)),eval.loglik=F)
# 
# mod_shapart <-
#   ergm(net~sum+mutual(form="min")+
#          transitiveweights("min","max","min")+nodecov("NUMRESP")+
#          nodecov("NUMGROUPS")+nodecov("MEANYEARS")+nodematch("ORGTYPE",form='sum')+
#          edgecov(spn7,form='sum')+edgecov(sppsp7,form='sum')+edgecov(spx7,form='sum'),
#        response="TVAL", reference=~DiscUnif(0,3),
#        control=control.ergm(init=c(geo.init, rep(0, 9)),
#                             MCMLE.maxit=30,MCMC.runtime.traceplot=F,seed=24,
#                             MCMLE.trustregion=1000,MCMC.addto.se=T,
#                             parallel.type="MPI",parallel=8,
#                             MPLE.max.dyad.types=1e+7,MCMC.samplesize=50000,
#                             MCMC.burnin=15000,MCMC.interval=1500,MCMLE.steplength=.25,
#                             MCMC.prop.args=list(p0=0.5)),eval.loglik=F)
# 
# detach(file:NetworkReady.RData)
# 
# rm(
# list=ls()[intersect(grep('shapart',ls(),invert=T),grep('dirpart',ls(),
#                                                        invert=T))])

rm(list=ls())
