rm(list=ls())

install.packages('Rglpk')
library(ergm)
library(coda)
library(mail)
setwd('/homes/tscott1/win/user/elwha/PSJ_Submission/Version3/min_versions/')
load('result_allpartmod.RData')
load('result_allpartmodquad.RData')
load('result_base.RData')
load('result_dirpart.RData')
load('result_indpart.RData')
load('result_shapart.RData')
load('result_pasttie.RData')

small.control = control.logLik.ergm(
  nsteps=4,
  MCMC.burnin=100,
  MCMC.interval=100,
  MCMC.samplesize=1000,
  obs.MCMC.samplesize=1000,
  obs.MCMC.interval=100,
  obs.MCMC.burnin=100,
  MCMC.prop.args=list(p0=0.5),
  warn.dyads=TRUE,
  MCMC.init.maxedges=NULL,
  MCMC.packagenames=NULL,
  seed=24)


mod_base = logLik(object = mod_base,
                  add = TRUE,verbose=T,control=small.control)

mod_allpart = logLik.ergm(object = mod_allpart,
                          add = TRUE,verbose=T,
                          control=small.control)

mod_allpartquad = logLik.ergm(object = mod_allpartquad,
                              add = TRUE,verbose=T,control=small.control)

mod_dirpart = logLik.ergm(object = mod_dirpart,
                          add = TRUE,verbose=T,control=small.control)

mod_indpart = logLik.ergm(object = mod_indpart,
                          add = TRUE,verbose=T,control=small.control)


mod_shapart = logLik.ergm(object = mod_shapart,
                          add = TRUE,verbose=T,control=small.control)

modpast_pasttie = logLik.ergm(object = modpast_pasttie,
                              add = TRUE,verbose=T,control=small.control)

save.image('small_logliks.RData')
sendmail(recipient = 'tyler.andrew.scott@gmail.com','test loglik values added','add loglik finished')
# test <- lapply(1:100,function(x) rnorm(10000))
# system.time(x <- lapply(test,function(x) loess.smooth(x,x)))
# system.time(x <- mclapply(test,function(x) loess.smooth(x,x), mc.cores=7))
