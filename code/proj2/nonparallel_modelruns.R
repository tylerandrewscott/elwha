#rm(list=ls())
#rm(list=ls())
require(statnet)
library(snow)
library(rlecuyer)
library(Rmpi)
#setwd('H:/passtosim')
#setwd("//Users/TScott/Google Drive/elwha")
#setwd('H:/elwha/Dissert_Scripts/')

setwd('/homes/tscott1/win/user/elwha/PSJ_Submission/Version3/')
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



mod_base_np <-
  ergm(net~sum+mutual(form="geometric")+
         transitiveweights("geomean","sum","geomean")+
         nodecov("NUMRESP",form='sum')+
         nodecov("NUMGROUPS",form='sum')+nodecov("MEANYEARS",form='sum')+
         nodematch("ORGTYPE",form='sum',diff=FALSE),
       response="TVAL", reference=~DiscUnif(0,3),
       control=control.ergm(init=c(geo.init, rep(0, 6)),
                            MCMLE.maxit=30,MCMC.runtime.traceplot=F,seed=24,
                            MCMLE.trustregion=1000,MCMC.addto.se=T,
                           # parallel.type="SOCK",parallel=8,
                            MPLE.max.dyad.types=1e+7,MCMC.samplesize=50000,
                            MCMC.burnin=1000,MCMC.interval=1000,MCMLE.steplength=.25,
                            MCMC.prop.args=list(p0=0.5)),eval.loglik=T,verbose=T)

save.image('result_base_np.RData')