rm(list=ls())
setwd('/homes/tscott1/win/user/elwha/PSJ_Submission/Version3/min_versions/')


library(ergm)
library(coda)

mod.names = c('mod_base','mod_allpart','mod_allpartquad',
              'mod_dirpart','mod_indpart','mod_shapart','modpast_allpart7')

load('result_base.RData')

load('result_allpartmod.RData')
load('result_indpart.RData')
load('result_dirpart.RData')
load('result_shapart.RData')
rm(list=ls()[ls() %in% mod.names==FALSE])
load('result_pasttie.RData')


mod.names = c('mod_base','mod_allpart','mod_allpartquad',
              'mod_dirpart','mod_indpart','mod_shapart','modpast_allpart7')

# control.settings = control.logLik.ergm(nsteps=5,
#                                        MCMC.burnin=1000,
#                                        MCMC.interval=500,
#                                        MCMC.samplesize=10000,
#                                        obs.MCMC.samplesize=10000,
#                                        obs.MCMC.interval=500,
#                                        obs.MCMC.burnin=1000,
#                                        MCMC.prop.weights="random",
#                                        MCMC.prop.args=NULL,
#                                        warn.dyads=TRUE,
#                                        MCMC.init.maxedges=10000,
#                                        seed=24)

mod.list = list(mod_base,mod_allpart,mod_allpartquad,
                mod_dirpart,mod_indpart,mod_shapart,modpast_allpart7)

#test = logLik.ergm(mod.list[[1]],control.logLik.ergm=control.settings,add=TRUE,verbose=T)


mod_base = logLik(object = mod_base,
                       add = TRUE,verbose=T)


mod_allpart = logLik.ergm(object = mod_allpart,
                       add = TRUE,verbose=T)

mod_allpartquad = logLik.ergm(object = mod_allpartquad,
                          add = TRUE,verbose=T)

mod_dirpart = logLik.ergm(object = mod_dirpart,
                          add = TRUE,verbose=T)

mod_indpart = logLik.ergm(object = mod_indpart,
                          add = TRUE,verbose=T)

mod_shapart = logLik.ergm(object = mod_shapart,
                          add = TRUE,verbose=T)

modpast_allpart7= logLik.ergm(object = modpast_allpart7,
                          add = TRUE,verbose=T)


save.image('only_model_results.RData')
# 
# test <- lapply(1:100,function(x) rnorm(10000))
# system.time(x <- lapply(test,function(x) loess.smooth(x,x)))
# system.time(x <- mclapply(test,function(x) loess.smooth(x,x), mc.cores=7))

rm(list=ls())