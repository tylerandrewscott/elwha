rm(list=ls())

install.packages('Rglpk')
require(statnet)
library(ergm)
library(RCurl)
library(plyr)
library(dplyr)
library(snow)
library(rlecuyer)
library(reshape2)
library(ggplot2)


####PREPARE DATA#############
dat_all <- read.csv(text = getURL("https://raw.githubusercontent.com/tylerascott/elwha/master/data_files/edgelist_all.csv"),row.names=1)

# Read a csv file into a data frame

#dat_all=read.csv(file="edgelist_all.csv",row.names=1)
temp<-dat_all[as.character(dat_all$ORG)!=as.character(dat_all$Contact),]

temp$paste<-paste(temp$ORG,temp$Contact)

temptab<-data.frame(table(temp$paste))
colnames(temptab) = c('paste','Freq')

temp$count = temptab$Freq[match(temp$paste,temptab$paste)]

t<-temp[order(temp$TType,decreasing=T),]

tt<-t[!duplicated(t$paste),]

resp.dat=read.csv(text = getURL('https://raw.githubusercontent.com/tylerascott/elwha/master/data_files/Response.Used.csv')
   ,row.names=1)
                           

allorgs<-sort(unique(c(as.character(resp.dat$ORG),as.character(tt$ORG),as.character(tt$Contact))))

tt$INSURV<-tt$Contact %in% resp.dat$ORG

t1<-tt[tt$INSURV==TRUE,]

net_temp = network.initialize(length(unique(resp.dat$ORG)),directed=TRUE,loops=FALSE)
vertex_attributes  = data.frame(sort(unique(resp.dat$ORG)))
colnames(vertex_attributes) = "NAME"
network.vertex.names(net_temp)<-as.character(vertex_attributes$NAME)

TAIL_ID = match(t1$ORG,network.vertex.names(net_temp))
HEAD_ID = match(t1$Contact,network.vertex.names(net_temp))

for (i in 1:length(TAIL_ID))
{
  net_temp[TAIL_ID[i],HEAD_ID[i]]<-1
  net_temp[TAIL_ID[i],HEAD_ID[i],"TCO"]<-t1$count[i]
  net_temp[TAIL_ID[i],HEAD_ID[i],"TVAL"]<-ifelse(t1$TType[i]=="WT",
                                                 3,ifelse(t1$TType[i]=="PT",2,1))
  net_temp[TAIL_ID[i],HEAD_ID[i],"WIN5_dk1"]<-ifelse(is.na(t1$WIN5[i]),1,(ifelse(t1$WIN5[i]>0,1,t1$WIN5)))
  net_temp[TAIL_ID[i],HEAD_ID[i],"WIN5_dk0"]<-ifelse(is.na(t1$WIN5[i]),0,(ifelse(t1$WIN5[i]==1,1,0)))
  net_temp[TAIL_ID[i],HEAD_ID[i],"PRIOR_TIE"]<-ifelse(is.na(t1$WIN5[i]),0,(ifelse(t1$WIN5[i]>0,0,1)))
}


vertex_attributes$TOTALYEARS = resp.dat$total.years[match(vertex_attributes$NAME,resp.dat$ORG)]

vertex_attributes$NUMGROUPS = data.frame(resp.dat %>% group_by(ORG) %>% summarise(mean(NumGroups)))[,2][
  match(vertex_attributes$NAME, data.frame(resp.dat %>% group_by(ORG) %>% summarise(mean(NumGroups)))[,1])]

vertex_attributes$NUMRESP = data.frame(resp.dat %>% group_by(ORG) %>% summarise(mean(Numres)))[,2][
  match(vertex_attributes$NAME, data.frame(resp.dat %>% group_by(ORG) %>% summarise(mean(Numres)))[,1])]

vertex_attributes$MEANYEARS= data.frame(resp.dat %>% group_by(ORG) %>% summarise(mean(Years)))[,2][
  match(vertex_attributes$NAME, data.frame(resp.dat %>% group_by(ORG) %>% summarise(mean(Years)))[,1])]

vertex_attributes$ORGTYPE = resp.dat$ORGType[match(vertex_attributes$NAME,resp.dat$ORG)]

vertex_attributes$USEPLAN = resp.dat$useful_plan[match(vertex_attributes$NAME,resp.dat$ORG)]
vertex_attributes$USEWORK = resp.dat$useful_work[match(vertex_attributes$NAME,resp.dat$ORG)]
vertex_attributes$USECONS = resp.dat$useful_cons[match(vertex_attributes$NAME,resp.dat$ORG)]


temp = as.data.frame(tapply(resp.dat$npsp,resp.dat$ORG,mean))
temp$ORG = rownames(temp)
colnames(temp)[1] = 'npsp.mean'
resp.dat$npsp = as.vector(temp$npsp.mean[match(resp.dat$ORG,temp$ORG)])

temp = as.data.frame(tapply(resp.dat$psp,resp.dat$ORG,mean))
temp$ORG = rownames(temp)
colnames(temp)[1] = 'psp.mean'
resp.dat$psp = as.vector(temp$psp.mean[match(resp.dat$ORG,temp$ORG)])

vertex_attributes$PSP_N = as.vector(
  tapply(resp.dat$psp,resp.dat$ORG,mean)[match(vertex_attributes$NAME, rownames(tapply(resp.dat$psp,resp.dat$ORG,mean)))])

vertex_attributes$NPSP_N = as.vector(
  tapply(resp.dat$psp,resp.dat$ORG,mean)[match(vertex_attributes$NAME, rownames(tapply(resp.dat$npsp,resp.dat$ORG,mean)))])


network.vertex.names(net_temp) = as.character(vertex_attributes$NAME)

set.vertex.attribute(net_temp,"ORGTYPE",value=as.character(vertex_attributes$ORGTYPE))
set.vertex.attribute(net_temp,"TOTALYEARS",value=vertex_attributes$TOTALYEARS)
set.vertex.attribute(net_temp,"NUMGROUPS",value=vertex_attributes$NUMGROUPS)
set.vertex.attribute(net_temp,"NUMRESP",value=vertex_attributes$NUMRESP)
set.vertex.attribute(net_temp,"MEANYEARS",value=vertex_attributes$MEANYEARS)
set.vertex.attribute(net_temp,"PSP_N",    value=vertex_attributes$PSP_N)
set.vertex.attribute(net_temp,"NPSP_N",value=vertex_attributes$NPSP_N)
set.vertex.attribute(net_temp,"USEWORK",value=ifelse(is.na(as.numeric(vertex_attributes$USEWORK)),0,as.numeric(vertex_attributes$USEWORK)))
set.vertex.attribute(net_temp,"USEPLAN",value=ifelse(is.na(as.numeric(vertex_attributes$USEPLAN)),0,as.numeric(vertex_attributes$USEPLAN)))
set.vertex.attribute(net_temp,"USECONS",value=ifelse(is.na(as.numeric(vertex_attributes$USECONS)),0,as.numeric(vertex_attributes$USECONS)))


net<-net_temp

psp_group <-read.csv(text = getURL("https://raw.githubusercontent.com/tylerascott/elwha/master/data_files/Group.Overlap.Matrix.PSP.csv"),row.names=1)
all_group <-read.csv(text = getURL("https://raw.githubusercontent.com/tylerascott/elwha/master/data_files/Group.Overlap.Matrix.csv"),row.names=1)
npsp_group <-read.csv(text = getURL("https://raw.githubusercontent.com/tylerascott/elwha/master/data_files/Group.Overlap.Matrix.NPSP.csv"),row.names=1)
fina_up_group<-read.csv(text = getURL("https://raw.githubusercontent.com/tylerascott/elwha/master/data_files/Group.Fina.Up.Matrix.csv"),row.names=1)
fina_down_group<-read.csv(text = getURL("https://raw.githubusercontent.com/tylerascott/elwha/master/data_files/Group.Fina.Down.Matrix.csv"),row.names=1)
huma_up_group<-read.csv(text = getURL("https://raw.githubusercontent.com/tylerascott/elwha/master/data_files/Group.Huma.Up.Matrix.csv"),row.names=1)
huma_down_group<-read.csv(text = getURL("https://raw.githubusercontent.com/tylerascott/elwha/master/data_files/Group.Huma.Down.Matrix.csv"),row.names=1)
valu_up_group<-read.csv(text = getURL("https://raw.githubusercontent.com/tylerascott/elwha/master/data_files/Group.Valu.Up.Matrix.csv"),row.names=1)
valu_down_group<-read.csv(text = getURL("https://raw.githubusercontent.com/tylerascott/elwha/master/data_files/Group.Value.Down.Matrix.csv"),row.names=1)
lang_up_group<-read.csv(text = getURL("https://raw.githubusercontent.com/tylerascott/elwha/master/data_files/Group.Lang.Up.Matrix.csv"),row.names=1)
lang_down_group<-read.csv(text = getURL("https://raw.githubusercontent.com/tylerascott/elwha/master/data_files/Group.Lang.Down.Matrix.csv"),row.names=1)
scie_up_group<-read.csv(text = getURL("https://raw.githubusercontent.com/tylerascott/elwha/master/data_files/Group.Scie.Up.Matrix.csv"),row.names=1)
scie_down_group<-read.csv(text = getURL("https://raw.githubusercontent.com/tylerascott/elwha/master/data_files/Group.Scie.Down.Matrix.csv"),row.names=1)
face_up_group<-read.csv(text = getURL("https://raw.githubusercontent.com/tylerascott/elwha/master/data_files/Group.Face.Up.Matrix.csv"),row.names=1)
face_down_group<-read.csv(text = getURL("https://raw.githubusercontent.com/tylerascott/elwha/master/data_files/Group.Face.Down.Matrix.csv"),row.names=1)

sppsp<-read.csv(text = getURL("https://raw.githubusercontent.com/tylerascott/elwha/master/data_files/SharedParticipationMatrixPSP.csv"),row.names=1)
spn<-read.csv(text = getURL("https://raw.githubusercontent.com/tylerascott/elwha/master/data_files/SharedParticipationMatrixN.csv"),row.names=1)
dppsp<-read.csv(text = getURL("https://raw.githubusercontent.com/tylerascott/elwha/master/data_files/DirectParticipationMatrixPSP.csv"),row.names=1)
dpn<-read.csv(text = getURL("https://raw.githubusercontent.com/tylerascott/elwha/master/data_files/DirectParticipationMatrixN.csv"),row.names=1)
ip<-read.csv(text = getURL("https://raw.githubusercontent.com/tylerascott/elwha/master/data_files/IndirectParticipation.csv"),row.names=1)

tname<-data.frame(network.vertex.names(net))
colnames(tname)<-"Name"
ww<-merge(tname,ip,by.x="Name",by.y="ORG",all.x=T)
ww$TotPartpsp<-ifelse(is.na(ww$TotPartpsp),0,ww$TotPartpsp)
ww$TotPartn<-ifelse(is.na(ww$TotPartn),0,ww$TotPartn)

set.vertex.attribute(net,"IPn",ww$TotPartn)
set.vertex.attribute(net,"IPpsp",ww$TotPartpsp)
set.vertex.attribute(net,"IPx",ww$TotPartn*ww$TotPartpsp)

set.vertex.attribute(x=net, attrname='allpart',
                     value=get.vertex.attribute(net,'IPn')+get.vertex.attribute(net,'IPpsp'))
set.vertex.attribute(net,'allpartdiv7',get.vertex.attribute(net,'allpart')/7) 
set.vertex.attribute(net,'allpartdiv7quad',get.vertex.attribute(net,'allpartdiv7')^2) 
set.vertex.attribute(net,'IPn7',get.vertex.attribute(net,'IPn')/7) 
set.vertex.attribute(net,'IPpsp7',get.vertex.attribute(net,'IPpsp')/7) 
set.vertex.attribute(net,'IPx7',get.vertex.attribute(net,'IPn7')*get.vertex.attribute(net,'IPpsp7')) 

colnames(spn)<-rownames(spn)
colnames(sppsp)<-rownames(sppsp)
colnames(dpn)<-rownames(dpn)
colnames(dppsp)<-rownames(dppsp)

emp<-matrix(0,nrow=(network.size(net)),ncol=network.size(net))
colnames(emp)<-network.vertex.names(net)
rownames(emp)<-network.vertex.names(net)

fullmatrix<-function(netx,fm)
{
  emp<-matrix(0,nrow=(network.size(netx)),ncol=network.size(netx))
  colnames(emp)<-network.vertex.names(netx)
  rownames(emp)<-network.vertex.names(netx)
  for (i in 1:nrow(fm))
  {
    for (j in 1:ncol(fm))
    {
      r<-which(rownames(emp)==colnames(fm)[i])
      c<-which(colnames(emp)==colnames(fm)[j])
      emp[r,c]<-fm[i,j]
    }}
  new<-emp
  return(new)
}

dpn<-fullmatrix(net,dpn)
dppsp<-fullmatrix(net,dppsp)
sppsp<-fullmatrix(net,sppsp)
spn<-fullmatrix(net,spn)

g <- sum(net %e% "TVAL")/network.dyadcount(net)
geo.init = log(1 - 1/(g+1))

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

##########RUN MODEL###############
form_base = net~sum+
  mutual(form="min")+
  transitiveweights(twopath="min",combine="max",affect="min")+
  nodecov("NUMRESP")+
  nodecov("NUMGROUPS")+
  nodecov("MEANYEARS")+
  nodematch("ORGTYPE",diff=FALSE)
  
npar = length(summary(form_base,response='TVAL'))-1

mod_base <-
  ergm(form_base,
       response="TVAL", reference=~DiscUnif(0,3),
       control=control.ergm(init=c(geo.init, rep(0, npar)),
                            MCMLE.maxit=30,MCMC.runtime.traceplot=F,seed=24,
                            MCMLE.trustregion=1000,MCMC.addto.se=T,
                            parallel='cluster',
                            MPLE.max.dyad.types=1e+7,MCMC.samplesize=50000,
                            MCMC.burnin=10000,MCMC.interval=1500,
                            MCMC.prop.args=list(p0=0.5)),eval.loglik=T,verbose=T)

mod_dirpart <-
  ergm(net~sum+
         mutual(form="min")+
         transitiveweights(twopath="min",combine="max",affect="min")+
         nodecov("NUMRESP",form='sum')+
         nodecov("NUMGROUPS",form='sum')+nodecov("MEANYEARS",form='sum')+
         nodematch("ORGTYPE",form='sum',diff=FALSE)+
         edgecov(dpn7,form='sum')+edgecov(dppsp7,form='sum')+edgecov(dpx7,form='sum'),
       response="TVAL", reference=~DiscUnif(0,3),
       control=control.ergm(init=c(geo.init, rep(0, npar+3)),
                            MCMLE.maxit=30,MCMC.runtime.traceplot=F,seed=24,
                            MCMLE.trustregion=1000,MCMC.addto.se=T,
                            parallel='cluster',
                            MPLE.max.dyad.types=1e+7,MCMC.samplesize=50000,
                            MCMC.burnin=10000,MCMC.interval=1500,
                            MCMC.prop.args=list(p0=0.5)),eval.loglik=T,verbose=T)



mod_indpart<-
  ergm(net~sum+
         mutual(form="min")+
         transitiveweights(twopath="min",combine="max",affect="min")+
         nodecov("NUMRESP",form='sum')+
         nodecov("NUMGROUPS",form='sum')+nodecov("MEANYEARS",form='sum')+
         nodematch("ORGTYPE",form='sum',diff=FALSE)+
         nodecov('IPn7')+nodecov('IPpsp7')+nodecov('IPx7'),
       response="TVAL", reference=~DiscUnif(0,3),
       control=control.ergm(init=c(geo.init, rep(0, npar+3)),
                            MCMLE.maxit=30,MCMC.runtime.traceplot=F,seed=24,
                            MCMLE.trustregion=1000,MCMC.addto.se=T,
                            parallel='cluster',
                            MPLE.max.dyad.types=1e+7,MCMC.samplesize=50000,
                            MCMC.burnin=10000,MCMC.interval=1500,
                            MCMC.prop.args=list(p0=0.5)),eval.loglik=T,verbose=T)



mod_shapart <-
  ergm(net~sum+
         mutual(form="min")+
         transitiveweights(twopath="min",combine="max",affect="min")+
         nodecov("NUMRESP",form='sum')+
         nodecov("NUMGROUPS",form='sum')+nodecov("MEANYEARS",form='sum')+
         nodematch("ORGTYPE",form='sum',diff=FALSE)+
         edgecov(spn7,form='sum')+edgecov(sppsp7,form='sum')+edgecov(spx7,form='sum'),
       response="TVAL", reference=~DiscUnif(0,3),
       control=control.ergm(init=c(geo.init, rep(0, npar+3)),
                            MCMLE.maxit=30,MCMC.runtime.traceplot=F,seed=24,
                            MCMLE.trustregion=1000,MCMC.addto.se=T,
                            parallel='cluster',
                            MPLE.max.dyad.types=1e+7,MCMC.samplesize=50000,
                            MCMC.burnin=10000,MCMC.interval=1500,
                            MCMC.prop.args=list(p0=0.5)),eval.loglik=T,verbose=T)

############EVALUATE RESULTS############
# sim_base = simulate.ergm(mod_base,nsim = 200000,statsonly=TRUE,response="TVAL", reference=~DiscUnif(0,3),
#                     control=control.simulate.ergm(MCMC.burnin=15000,
#                                                   MCMC.interval=1500,
#                                                   MCMC.prop.args=list(p0=0.5),
#                                                   MCMC.runtime.traceplot=FALSE,
#                                                   parallel=8,
#                                                   parallel.type='PSOCK'))
# 
# 
# sim_base.dat = melt(sim_base,id.vars=rownames(sim_base))
# 
# 
# net.obs <- data.frame(summary(mod_base$formula,
#                               response="TVAL",reference=~DiscUnif(0,3)))
# 
# names(net.obs)[1] = 'Observed.Value'
# net.obs[,2] = rownames(net.obs)
# names(net.obs)[2] = 'Var2'
# 
# sim.dat= join(sim.dat,net.obs)
# 
# 
# sim.plot = ggplot(sim.dat) + geom_density(aes(x=value),adjust=1.5,colour='black',trim=TRUE) + facet_wrap(~Var2,scales='free') + 
#   geom_vline(aes(xintercept=Observed.Value,linetype = "expected"),show_guide = TRUE,lwd=1.25)+
#   theme_bw()  + ylab('Density') + xlab('Simulated Value') + 
#   theme(panel.background=element_blank(),
#         panel.grid = element_blank(),
#         axis.title = element_text(size=12),
#         strip.text = element_text(size=12),
#         axis.text = element_text(size=10),
#         legend.position=c(.7,.2),
#         legend.key.height = unit(2,'cm'),
#         # legend.title = element_blank(),
#         legend.text = element_text(size=12))+
#   scale_linetype_manual(name='',values = 2,labels='Observed value')
# plot(sim.plot)
# 
# 
# ggdudens = ggplot(data=temp2,aes(colour=as.character(Chain),x=value))+
#   geom_density(aes(),size=.5,trim=TRUE,adjust=2)+ 
#   facet_wrap(~Parameter,scales="free")+
#   scale_colour_brewer(type="qual",palette='Set1',
#                       guide=guide_legend(title="MCMC Chain", label.position="bottom",nrow=2,
#                                          override.aes=list(size=.5,alpha=1)))+
#   scale_x_continuous("Parameter Value",expand=c(0,0))+
#   scale_y_continuous("Density") +
#   theme_bw()+
#   theme(axis.text=element_text(size=10),
#         axis.title = element_text(size=12),
#         legend.title = element_text(size=12),
#         legend.text = element_text(size=10),
#         legend.position=c(.85,.15),legend.text.align=.5, 
#         legend.title.align=.5, legend.key.size=unit(.5,'cm'),
#         axis.text=element_text(size=6),
#         legend.background = element_rect(colour = "black"))
# plot(ggdudens)

save.image('result_fourmods.RData')

sendmail("tyler.andrew.scott@gmail.com", "R notice: runmod_base", "runmod_base finished.")
