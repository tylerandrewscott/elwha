rm(list=ls())
#install.packages('statnet',dependencies=TRUE,repos='http://cran.us.r-project.org')
setwd("win/user/elwha/data_files/")
require(statnet)

dat_all=read.csv(file="edgelist_all.csv",row.names=1)
temp<-dat_all[as.character(dat_all$ORG)!=as.character(dat_all$Contact),]

temp$paste<-paste(temp$ORG,temp$Contact)

temptab<-data.frame(table(temp$paste))
colnames(temptab) = c('paste','Freq')

temp$count = temptab$Freq[match(temp$paste,temptab$paste)]

t<-temp[order(temp$TType,decreasing=T),]

tt<-t[!duplicated(t$paste),]

resp.dat=read.csv(file="Response.Used.csv",row.names=1)

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


library(plyr)
library(dplyr)
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



org.npsp.mean = data.frame(tapply(resp.dat$npsp,resp.dat$ORG,mean))
resp.dat$npsp = org.npsp.mean$mean.npsp.[match(resp.dat$ORG,org.npsp.mean$ORG)]

org.psp.mean = data.frame(tapply(resp.dat$psp,resp.dat$ORG,mean))
resp.dat$psp = org.psp.mean$mean.psp.[match(resp.dat$ORG,org.psp.mean$ORG)]


vertex_attributes$PSP_N = tapply(resp.dat$psp,resp.dat$ORG,mean)[match(vertex_attributes$NAME, rownames(tapply(resp.dat$psp,resp.dat$ORG,mean)))]

vertex_attributes$NPSP_N = tapply(resp.dat$psp,resp.dat$ORG,mean)[match(vertex_attributes$NAME, rownames(tapply(resp.dat$npsp,resp.dat$ORG,mean)))]


network.vertex.names(net_temp) = as.character(vertex_attributes$NAME)
set.vertex.attribute(net_temp,"ORGTYPE",value=vertex_attributes$ORGTYPE)
set.vertex.attribute(net_temp,"TOTALYEARS",value=vertex_attributes$TOTALYEARS)
set.vertex.attribute(net_temp,"NUMGROUPS",value=vertex_attributes$NUMGROUPS)
set.vertex.attribute(net_temp,"NUMRESP",value=vertex_attributes$NUMRESP)
set.vertex.attribute(net_temp,"MEANYEARS",value=vertex_attributes$MEANYEARS)
set.vertex.attribute(net_temp,"PSP_N",value=vertex_attributes$PSP_N)
set.vertex.attribute(net_temp,"NPSP_N",value=vertex_attributes$NPSP_N)
set.vertex.attribute(net_temp,"USEWORK",value=ifelse(is.na(as.numeric(vertex_attributes$USEWORK)),0,as.numeric(vertex_attributes$USEWORK)))
set.vertex.attribute(net_temp,"USEPLAN",value=ifelse(is.na(as.numeric(vertex_attributes$USEPLAN)),0,as.numeric(vertex_attributes$USEPLAN)))
set.vertex.attribute(net_temp,"USECONS",value=ifelse(is.na(as.numeric(vertex_attributes$USECONS)),0,as.numeric(vertex_attributes$USECONS)))


net<-net_temp


psp_group = read.csv(file="Group.Overlap.Matrix.PSP.csv",row.names=1)
all_group = read.csv(file="Group.Overlap.Matrix.csv",row.names=1)
npsp_group = read.csv(file="Group.Overlap.Matrix.NPSP.csv",row.names=1)
fina_up_group=read.csv(file="Group.Fina.Up.Matrix.csv",row.names=1)
fina_down_group=read.csv(file="Group.Fina.Down.Matrix.csv",row.names=1)
huma_up_group=read.csv(file="Group.Huma.Up.Matrix.csv",row.names=1)
huma_down_group=read.csv(file="Group.Huma.Down.Matrix.csv",row.names=1)
valu_up_group=read.csv(file="Group.Valu.Up.Matrix.csv",row.names=1)
valu_down_group=read.csv(file="Group.Value.Down.Matrix.csv",row.names=1)
lang_up_group=read.csv(file="Group.Lang.Up.Matrix.csv",row.names=1)
lang_down_group=read.csv(file="Group.Lang.Down.Matrix.csv",row.names=1)
scie_up_group=read.csv(file="Group.Scie.Up.Matrix.csv",row.names=1)
scie_down_group=read.csv(file="Group.Scie.Down.Matrix.csv",row.names=1)
face_up_group=read.csv(file="Group.Face.Up.Matrix.csv",row.names=1)
face_down_group=read.csv(file="Group.Face.Down.Matrix.csv",row.names=1)

sppsp<-read.csv("SharedParticipationMatrixPSP.csv",row.names=1)
spn<-read.csv("SharedParticipationMatrixN.csv",row.names=1)
dppsp<-read.csv("DirectParticipationMatrixPSP.csv",row.names=1)
dpn<-read.csv("DirectParticipationMatrixN.csv",row.names=1)
ip<-read.csv("IndirectParticipation.csv",row.names=1)
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

# 
# 
# set.vertex.attribute(x=net, attrname='allpartquad', value = get.vertex.attribute(net,'allpart')^2)
# set.vertex.attribute(net,'allpartmc',get.vertex.attribute(net,'allpart') - 
#                        mean(get.vertex.attribute(net,'allpart')))


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

save.image('NetworkReady.RData')
