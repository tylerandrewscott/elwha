rm(list=ls())

install.packages('Rglpk')
library(ergm)
library(coda)
library(mail)
setwd('/homes/tscott1/win/')

load('result_allpartmod.RData')
mod_allpart = logLik.ergm(object = mod_allpart,
                          add = TRUE,verbose=T)
save.image('result_allpartmod_wll.RData')


load('result_allpartmodquad.RData')
mod_allpartquad = logLik.ergm(object = mod_allpartquad,
                          add = TRUE,verbose=T)
save.image('result_allpartmodquad_wll.RData')



load('result_base.RData')
mod_base = logLik(object = mod_base,
                  add = TRUE,verbose=T)
save.image('result_base_wll.RData')



load('result_dirpart.RData')
mod_dirpart = logLik.ergm(object = mod_dirpart,
                          add = TRUE,verbose=T)
save.image('result_dirpart_wll.RData')



load('result_indpart.RData')
mod_indpart = logLik.ergm(object = mod_indpart,
                          add = TRUE,verbose=T)
save.image('result_indpart_wll.RData')


load('result_shapart.RData')
mod_shapart = logLik.ergm(object = mod_shapart,
                          add = TRUE,verbose=T)
save.image('result_shapart_wll.RData')


load('result_pasttie.RData')
mod_pasttie = logLik.ergm(object = mod_pasttie,
                          add = TRUE,verbose=T)
save.image('result_pasttie_wll.RData',)


sendmail(recipient = 'tyler.andrew.scott@gmail.com',ls()[grep('mod',ls())][1],'add loglik finished')
# test <- lapply(1:100,function(x) rnorm(10000))
# system.time(x <- lapply(test,function(x) loess.smooth(x,x)))
# system.time(x <- mclapply(test,function(x) loess.smooth(x,x), mc.cores=7))
