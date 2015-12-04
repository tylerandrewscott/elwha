library(statnet)

OVER<-as.matrix(all_group)
OVER.PSP<-as.matrix(psp_group)
OVER.NPSP<-as.matrix(npsp_group)
FINA.UP<-as.matrix(fina_up_group)
FINA.DOWN<-as.matrix(fina_down_group)
HUMA.UP<-as.matrix(huma_up_group)
HUMA.DOWN<-as.matrix(huma_down_group)
LANG.UP<-as.matrix(lang_up_group)
LANG.DOWN<-as.matrix(lang_down_group)
SCIE.UP<-as.matrix(scie_up_group)
SCIE.DOWN<-as.matrix(scie_down_group)
VALU.UP<-as.matrix(valu_up_group)
VALU.DOWN<-as.matrix(valu_down_group)
FACE.UP<-as.matrix(face_up_group)
FACE.DOWN<-as.matrix(face_down_group)

PE.ALL.UP <- LANG.UP+VALU.UP+FACE.UP
JA.ALL.UP <- FINA.UP+SCIE.UP+HUMA.UP



mod.settings<-control.ergm(
Step.MCMC.samplesize=10000,
main.method="Stepping", MCMLE.metric="lognormal",
Step.gridsize=1000,
Step.maxit=20,seed=24)


cor(c(VALU.UP),c(LANG.UP))
cor(c(VALU.UP),c(FACE.UP))
cor(c(FACE.UP),c(LANG.UP))
cor(c(FINA.UP),c(HUMA.UP))
cor(c(FINA.UP),c(SCIE.UP))
cor(c(HUMA.UP),c(SCIE.UP))


c(VALU.UP)

net_temp <- net_uq
tempform2<-net_temp ~ edges + mutual +twopath +isolates +gwidegree(0.25, fixed = TRUE)+
 gwesp(0.25, fixed = TRUE) + nodecov("NUMRESP") + nodecov("NUMGROUPS")+
edgecov(OVER)+
edgecov(FINA.UP)
modtemp2<-ergm(tempform2,control=mod.settings)
mod_H3_fina_up = modtemp2

net_temp <- net_uq
tempform2<-net_temp ~ edges + mutual +twopath +isolates +gwidegree(0.25, fixed = TRUE)+
 gwesp(0.25, fixed = TRUE) + nodecov("NUMRESP") + nodecov("NUMGROUPS") +
 edgecov(OVER)+
edgecov(HUMA.UP)
modtemp2<-ergm(tempform2,control=mod.settings)
mod_H3_huma_up = modtemp2

net_temp <- net_uq
tempform2<-net_temp ~ edges + mutual +twopath +isolates +gwidegree(0.25, fixed = TRUE)+
 gwesp(0.25, fixed = TRUE) + nodecov("NUMRESP") + nodecov("NUMGROUPS")+edgecov(OVER)+
edgecov(SCIE.UP)
modtemp2<-ergm(tempform2,control=mod.settings)
mod_H3_scie_up = modtemp2

net_temp <- net_uq
tempform2<-net_temp ~ edges + mutual +twopath +isolates +gwidegree(0.25, fixed = TRUE)+
 gwesp(0.25, fixed = TRUE) + nodecov("NUMRESP") + nodecov("NUMGROUPS")+edgecov(OVER)+
edgecov(FACE.UP)
modtemp2<-ergm(tempform2,control=mod.settings)
mod_H3_face_up = modtemp2

tempform2<-net_temp ~ edges + mutual +twopath +isolates +gwidegree(0.25, fixed = TRUE)+
 gwesp(0.25, fixed = TRUE) + nodecov("NUMRESP") + nodecov("NUMGROUPS")+edgecov(OVER)+
edgecov(LANG.UP)
modtemp2<-ergm(tempform2,control=mod.settings)
mod_H3_lang_up = modtemp2

tempform2<-net_temp ~ edges + mutual +twopath +isolates +gwidegree(0.25, fixed = TRUE)+
 gwesp(0.25, fixed = TRUE) + nodecov("NUMRESP") + nodecov("NUMGROUPS")+edgecov(OVER)+
edgecov(VALU.UP)
modtemp2<-ergm(tempform2,control=mod.settings)
mod_H3_valu_up = modtemp2


net_temp <- net_uq
tempform2<-net_temp ~ edges + mutual +twopath +isolates +gwidegree(0.25, fixed = TRUE)+
 gwesp(0.25, fixed = TRUE) + nodecov("NUMRESP") + nodecov("NUMGROUPS")+
edgecov(OVER)+
edgecov(FINA.DOWN)
modtemp2<-ergm(tempform2,control=mod.settings)
mod_H3_fina_down = modtemp2

net_temp <- net_uq
tempform2<-net_temp ~ edges + mutual +twopath +isolates +gwidegree(0.25, fixed = TRUE)+
 gwesp(0.25, fixed = TRUE) + nodecov("NUMRESP") + nodecov("NUMGROUPS") +
 edgecov(OVER)+
edgecov(HUMA.DOWN)
modtemp2<-ergm(tempform2,control=mod.settings)
mod_H3_huma_down = modtemp2

net_temp <- net_uq
tempform2<-net_temp ~ edges + mutual +twopath +isolates +gwidegree(0.25, fixed = TRUE)+
 gwesp(0.25, fixed = TRUE) + nodecov("NUMRESP") + nodecov("NUMGROUPS")+edgecov(OVER)+
edgecov(SCIE.DOWN)
modtemp2<-ergm(tempform2,control=mod.settings)
mod_H3_scie_down = modtemp2

net_temp <- net_uq
tempform2<-net_temp ~ edges + mutual +twopath +isolates +gwidegree(0.25, fixed = TRUE)+
 gwesp(0.25, fixed = TRUE) + nodecov("NUMRESP") + nodecov("NUMGROUPS")+edgecov(OVER)+
edgecov(FACE.DOWN)
modtemp2<-ergm(tempform2,control=mod.settings)
mod_H3_face_down = modtemp2

tempform2<-net_temp ~ edges + mutual +twopath +isolates +gwidegree(0.25, fixed = TRUE)+
 gwesp(0.25, fixed = TRUE) + nodecov("NUMRESP") + nodecov("NUMGROUPS")+edgecov(OVER)+
edgecov(LANG.DOWN)
modtemp2<-ergm(tempform2,control=mod.settings)
mod_H3_lang_down = modtemp2

tempform2<-net_temp ~ edges + mutual +twopath +isolates +gwidegree(0.25, fixed = TRUE)+
 gwesp(0.25, fixed = TRUE) + nodecov("NUMRESP") + nodecov("NUMGROUPS")+edgecov(OVER)+
edgecov(VALU.DOWN)
modtemp2<-ergm(tempform2,control=mod.settings)
mod_H3_valu_down = modtemp2



tempform2<-net_temp ~ edges + mutual +twopath +isolates +gwidegree(0.25, fixed = TRUE)+
 gwesp(0.25, fixed = TRUE) + nodecov("NUMRESP") + nodecov("NUMGROUPS")+
edgecov(OVER)+
edgecov(VALU.UP)+edgecov(LANG.UP)+edgecov(FACE.UP)
modtemp2<-ergm(tempform2,control=mod.settings)
mod_H3_all_pe = modtemp2

tempform2<-net_temp ~ edges + mutual +twopath +isolates +gwidegree(0.25, fixed = TRUE)+
 gwesp(0.25, fixed = TRUE) + nodecov("NUMRESP") + nodecov("NUMGROUPS")+edgecov(OVER)+
edgecov(HUMA.UP)+edgecov(SCIE.UP)+edgecov(FINA.UP)
modtemp2<-ergm(tempform2,control=mod.settings)
mod_H3_all_ic = modtemp2

tempform2<-net_temp ~ edges + mutual +twopath +isolates +gwidegree(0.25, fixed = TRUE)+
 gwesp(0.25, fixed = TRUE) + nodecov("NUMRESP") + nodecov("NUMGROUPS") +
edgecov(OVER)+
edgecov(PE.ALL.UP)
modtemp2<-ergm(tempform2,control=mod.settings)
mod_H3_pe = modtemp2


tempform2<-net_temp ~ edges + mutual +twopath +isolates +gwidegree(0.25, fixed = TRUE)+
 gwesp(0.25, fixed = TRUE) + nodecov("NUMRESP") + nodecov("NUMGROUPS")+edgecov(OVER)+
edgecov(JA.ALL.UP)
modtemp2<-ergm(tempform2,control=mod.settings)
mod_H3_ic = modtemp2


summary(mod_H3_pe)
summary(mod_H3_ic)
tempform2<-net_temp ~ edges + mutual +twopath +isolates +gwidegree(0.25, fixed = TRUE)+
 gwesp(0.25, fixed = TRUE) + nodecov("NUMRESP") + nodecov("NUMGROUPS")+edgecov(OVER)+
edgecov(COMB_SUM)
modtemp2<-ergm(tempform2,control=mod.settings)
mod_H3_bothsum = modtemp2

tempform2<-net_temp ~ edges + mutual +twopath +isolates +gwidegree(0.25, fixed = TRUE)+
 gwesp(0.25, fixed = TRUE) + nodecov("NUMRESP") + nodecov("NUMGROUPS")+edgecov(OVER)+
edgecov(COMB_MULT)
modtemp2<-ergm(tempform2,control=mod.settings)
mod_H3_bothmult = modtemp2

summary(mod_H3_bothmult)
summary(mod_H3_pe)

mean(FACE)


summary(mod_H3_valu_up)
summary(mod_H3_face_up)
summary(mod_H3_lang_up)


summary(mod_H3_scie_up)
summary(mod_H3_huma_up)
summary(mod_H3_fina_up)


cor(c(FACE),c(VALU))
cor(c(FACE),c(LANG))
cor(c(LANG),c(VALU))

cor(c(HUMA),c(FINA))
cor(c(HUMA),c(SCIE))
cor(c(SCIE),c(FINA))




