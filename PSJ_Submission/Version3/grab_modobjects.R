rm(list=ls())
setwd('/homes/tscott1/win/user/elwha/PSJ_Submission/Version3/')
library(statnet)

mod.names = c('mod_base','mod_allpart','mod_allpartquad',
              'mod_dirpart','mod_indpart','mod_shapart','modpast_allpart7')

load('result_base.RData')
mod_base <- logLik.ergm(mod_base,add=TRUE)
rm(list=ls()[ls() %in% mod.names==FALSE])

load('result_allpartmod.RData')
mod_allpart <- logLik.ergm(mod_allpart,add=TRUE)
mod_allpartquad <- logLik.ergm(mod_allpartquad,add=TRUE)
rm(list=ls()[ls() %in% mod.names==FALSE])

load('result_indpart.RData')
mod_indpart <- logLik.ergm(mod_indpart,add=TRUE)
rm(list=ls()[ls() %in% mod.names==FALSE])

load('result_dirpart.RData')
mod_dirpart <- logLik.ergm(mod_dirpart,add=TRUE)
rm(list=ls()[ls() %in% mod.names==FALSE])

load('result_shapart.RData')
mod_shapart <- logLik.ergm(mod_shapart,add=TRUE)
rm(list=ls()[ls() %in% mod.names==FALSE])

load('result_pasttie.RData')
modpast_allpart7 <- logLik.ergm(modpast_allpart7,add=TRUE)
rm(list=ls()[ls() %in% mod.names==FALSE])

save.image('only_model_results.RData')

rm(list=ls())