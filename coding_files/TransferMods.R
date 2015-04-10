require(statnet)
attach("//Users/TScott/Google Drive/PSYCH548/CodeandRelated/jmfuns.rda")

setwd('//Users/TScott/Google Drive/elwha')
list.files()
attach('NetworkReady.RData')
detach(pos=2)
search()
net<-net
rm(list=ls())
net
load('modsim7pasttiemods.RData')
load('model_results.RData')


search()
require(statnet)
summary(modpastdp7b)
summary(modpastip7)
summary(modpastip7b)

list.files()
load('')
list.files()
load('modsim7lasttwomods.RData')

mod_pastdirpart<-summary(modpastdp7b)
mod_pastshapart<-summary(modpastsp7b)

summary(modpastip7b)
ls()


mod_pasttie_dp<-modpast_dp7
mod_pasttie_ip<-modpast_ip7
mod_pasttie_sp<-modpast_sp7
mod_pasttie_base<-modpast_base7
mod_pasttie_allgroups<-modpast_allpart7

attach('modsim5use7 (1).RData')
detach('modsim5use7 (1).RData')
search()
detach(pos=2)
summary(modpastdp7b)

load('modsim7fulldirect (1).RData')

net

ls(search()[2])
mod_pastdirpart<-modpastdp7b
mod_pasttiebase<-modpastip7 
mod_pastshapart<-modpastsp7b
mod_pastindpart<-modpastip7b

detach(pos=2)
setwd("//Users/TScott/Google Drive/elwha/NetworkChapter")
attach('SOCKmods.RData')


save.image('model_results.RData')
ls(search()[2])
list.files()

library(statnet)
list.files()[grep("Sim",list.files())]

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

rm(net)

save(mod_base,mod_allpart,mod_allpartquad,mod_indpart,mod_dirpart,mod_shapart, file="//Users/TScott/Google Drive/elwha/SOCKmods.RData")
rm(mod_pasttiebase)


load('')
load('model_results.RData')
detach(pos=2)
rm(list=ls())

detach(pos=2)
search()

mod_pasttie_dp<-modpast_dp7
mod_pasttie_ip<-modpast_ip7
mod_pasttie_sp<-modpast_sp7
mod_pasttie_base<-modpast_base7
mod_pasttie_allgroups<-modpast_allpart7
list.files()
move(mod_pasttie_dp,to='model_results.RData')
move(mod_pasttie_ip,to='model_results.RData')
move(mod_pasttie_sp,to='model_results.RData')
move(mod_pasttie_base,to='model_results.RData')
move(mod_pasttie_allgroups,to='model_results.RData')



net
mod_dirpart<-modGtry7

mod_indpart1<-modBtry7
mod_indpart2<-modCtry7
mod_indpart3<-modDtry7
ls(search()[2])
attach('modsim7fulldirect.RData')
list.files()
ls()
rm(mod_pastdirpart)



mod_pasttie_iprestricted<-modpastip7
mod_pasttie_ip<-modpastip7b
mod_pasttie_dp<-modpastdp7b
mod_pasttie_sp<-modpastsp7b

load()
summary(modpastsp7b)
summary(modpastip7b)
search()
detach(pos=3)
rm(list=ls())
list.files()
attach("//Users/TScott/Google Drive/elwha/SOCKmods.RData",pos=2)
detach("//Users/TScott/Google Drive/elwha/SOCKmods.RData")
load("//Users/TScott/Google Drive/elwha/Sim7_Poisson_Workspace.RData")
modAp<-modA
modBp<-modB
modCp<-modC
modDp<-modD
modEp<-modE
modFp<-modF
modGp<-modG
modHp<-modH
ls(search()[15])

rm(mod_pasttie_sp)


move(dpn7, to = "SOCKmods.RData")
move(dppsp7, to = "SOCKmods.RData")
move(dpx7, to = "SOCKmods.RData")
move(spn7, to = "SOCKmods.RData")
move(sppsp7, to = "SOCKmods.RData")
move(spx7, to = "SOCKmods.RData")

move(mod_pastdirpart, to = "SOCKmods.RData")
move(mod_pastshapart, to = "SOCKmods.RData")
rm(list=ls())
rm(mod_pastdirpart)
search()
setwd('//Users/TScott/Google Drive/elwha')
load('SOCKmods.RData')
list.files()
attach('modsim7lasttwomods.RData')

detach(pos=2)
move(mod_pastindpart, to = "SOCKmods.RData")
move(mod_pasttiebase, to = "SOCKmods.RData")

detach(pos=2)
ls()
search()

getwd()
save.image('model_results.RData')
mod_pastdirpart<-modpastdp7b
mod_pasttiebase<-modpastip7 
mod_pasttie_sp<-modpastsp7b
mod_pasttie_dp<-modpastdp7b

mod_past<-modpastip7b
search()
move(net, to = "SOCKmods.RData")

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