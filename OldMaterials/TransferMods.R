
attach("//Users/TScott/Google Drive/PSYCH548/CodeandRelated/jmfuns.rda")

summary(modGtry7)

ls()
setwd("//Users/TScott/Google Drive/elwha")
library(statnet)
list.files()[grep("Sim",list.files())]

load("//Users/TScott/Google Drive/elwha/Sim9_PoissonGeom_Workspace.RData")
modAdu<-modA
modBdu<-modB
modCdu<-modC
modDdu<-modD
modEdu<-modE
modFdu<-modF
modGdu<-modG
modHdu<-modH

mod_base<-modAtry
mod_allpart<-mod0try7
mod_allpartquad<-mod0btry7

save(mod_base,mod_allpart,mod_allpartquad,mod_indpart,mod_dirpart,mod_shapart, file="//Users/TScott/Google Drive/elwha/SOCKmods.RData")

mod_dirpart<-modGtry7

mod_indpart1<-modBtry7
mod_indpart2<-modCtry7
mod_indpart3<-modDtry7
ls(search()[2])
attach('modsim7fulldirect.RData')
list.files()
ls()

mod_pasttie_iprestricted<-modpastip7
mod_pasttie_ip<-modpastip7b
mod_pasttie_dp<-modpastdp7b
mod_pasttie_sp<-modpastsp7b


summary(modpastsp7b)
summary(modpastip7b)
search()
detach(pos=3)
rm(list=ls())
list.files()
attach("//Users/TScott/Google Drive/elwha/SOCKmods.RData",pos=2)
load("//Users/TScott/Google Drive/elwha/Sim7_Poisson_Workspace.RData")
modAp<-modA
modBp<-modB
modCp<-modC
modDp<-modD
modEp<-modE
modFp<-modF
modGp<-modG
modHp<-modH

move(mod_, to = "//Users/TScott/Google Drive/elwha/SOCKmods.RData")

move(mod_dirpart, to = "//Users/TScott/Google Drive/elwha/NetworkChapter/SOCKmods.RData")



move(mod_indpart3, to = "//Users/TScott/Google Drive/elwha/SOCKmods.RData")

mod_pasttie_iprestricted<-modpastip7
mod_pasttie_ip<-modpastip7b
mod_pasttie_dp<-modpastdp7b
mod_pasttie_sp<-modpastsp7b

move(mod_pasttie_iprestricted, to = "//Users/TScott/Google Drive/elwha/SOCKmods.RData")
move(mod_pasttie_ip, to = "//Users/TScott/Google Drive/elwha/SOCKmods.RData")
move(mod_pasttie_dp, to = "//Users/TScott/Google Drive/elwha/SOCKmods.RData")
move(mod_pasttie_sp, to = "//Users/TScott/Google Drive/elwha/SOCKmods.RData")


move(modDp, to = "//Users/TScott/Google Drive/elwha/SOCKmods.RData")
move(modEp, to = "//Users/TScott/Google Drive/elwha/SOCKmods.RData")
move(modFp, to = "//Users/TScott/Google Drive/elwha/SOCKmods.RData")
move(modGp, to = "//Users/TScott/Google Drive/elwha/SOCKmods.RData")
move(modHp, to = "//Users/TScott/Google Drive/elwha/SOCKmods.RData")
rm(list=ls())
load("//Users/TScott/Google Drive/elwha/Sim5_Geom_Workspace.RData")
modAb<-modA
modBb<-modB
modCb<-modC
modDb<-modD
modEb<-modE
modFb<-modF
modGb<-modG
modHb<-modH
mcmc.diagnostics(modHb)
move(modAb, to = "//Users/TScott/Google Drive/elwha/SOCKmods.RData")
move(modBb, to = "//Users/TScott/Google Drive/elwha/SOCKmods.RData")
move(modCb, to = "//Users/TScott/Google Drive/elwha/SOCKmods.RData")
move(modDb, to = "//Users/TScott/Google Drive/elwha/SOCKmods.RData")
move(modEb, to = "//Users/TScott/Google Drive/elwha/SOCKmods.RData")
move(modFb, to = "//Users/TScott/Google Drive/elwha/SOCKmods.RData")
move(modGb, to = "//Users/TScott/Google Drive/elwha/SOCKmods.RData")
move(modHb, to = "//Users/TScott/Google Drive/elwha/SOCKmods.RData")