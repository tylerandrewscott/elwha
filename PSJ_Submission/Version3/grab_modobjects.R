rm(list=ls())
setwd('/homes/tscott1/win/user/elwha/Dissert_Scripts/')

mod.names = c('mod_base','mod_allpart','mod_allpartquad',
              'mod_dirpart','mod_indpart','mod_shapart','modpast_allpart7')

load('result_base.RData')
load('result_allpart.RData')
load('result_indpart.RData')
load('result_dirpart.RData')
load('result_shapart.RData')
load('result_pasttie.RData')

rm(list=ls()[ls() %in% mod.names==FALSE])

mod_base <- logLik(mod_base,add=TRUE)
mod_allpart <- logLik(mod_allpart,add=TRUE)
mod_allpartquad <- logLik(mod_allpartquad,add=TRUE)
mod_dirpart <- logLik(mod_dirpart,add=TRUE)
mod_indpart <- logLik(mod_indpart,add=TRUE)
mod_shapart <- logLik(mod_shapart,add=TRUE)
modpast_allpart7 <- logLik(modpast_allpart7,add=TRUE)

save.image('only_model_results.RData')

rm(list=ls())