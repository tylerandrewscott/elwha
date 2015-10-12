rm(list=ls())
install.packages('statnet',dependencies=TRUE,repos='http://cran.us.r-project.org')
library(statnet)

dat_all=read.csv(file="edgelist_all.csv",row.names=1)
temp<-dat_all[as.character(dat_all$ORG)!=as.character(dat_all$Contact),]
temp1<-paste(temp$ORG,temp$Contact)
temp$paste<-temp1

temptab<-table(temp$paste)
for (i in 1:nrow(temp))
{
	loc<-which(names(table(temp$paste))==temp$paste[i])
	temp$count[i]<-temptab[loc]
}

t<-temp[order(temp$TType,decreasing=T),]
t$dup<-duplicated(t$paste)
tt<-t[duplicated(t$paste)!=TRUE,]


resp.dat=read.csv(file="Response.Used.csv",row.names=1)

allorgs<-sort(unique(c(as.character(resp.dat$ORG),as.character(tt$ORG),as.character(tt$Contact))))

tt$INSURV<-tt$Contact %in% resp.dat$ORG

t1<-tt[tt$INSURV==TRUE,]

net_temp = network.initialize(length(unique(resp.dat$ORG)),directed=TRUE,loops=FALSE)
vertex_attributes  = data.frame(sort(unique(resp.dat$ORG)))
colnames(vertex_attributes) = "NAME"
network.vertex.names(net_temp)<-as.character(vertex_attributes$NAME)

TAIL_ID = rep(0,nrow(t1))
HEAD_ID = rep(0,nrow(t1))


for (i in 1:nrow(t1))
{
	TAIL_ID[i] = (which(network.vertex.names(net_temp)==t1$ORG[i]))
	HEAD_ID[i] = (which(network.vertex.names(net_temp)==t1$Contact[i]))
}

for (i in 1:length(TAIL_ID))
{
	net_temp[TAIL_ID[i],HEAD_ID[i]]<-1
	net_temp[TAIL_ID[i],HEAD_ID[i],"TCO"]<-t1$count[i]
	net_temp[TAIL_ID[i],HEAD_ID[i],"TVAL"]<-ifelse(t1$TType[i]=="WT",
	3,ifelse(t1$TType[i]=="PT",2,1))
	net_temp[TAIL_ID[i],HEAD_ID[i],"WIN5_dk1"]<-ifelse(is.na(t1$WIN5[i]),1,(ifelse(t1$WIN5[i]>0,1,t1$WIN5)))
	net_temp[TAIL_ID[i],HEAD_ID[i],"WIN5_dk0"]<-ifelse(is.na(t1$WIN5[i]),0,(ifelse(t1$WIN5[i]==1,1,0)))
}


TOTALYEARS = rep(0,length(vertex_attributes$NAME))
for (i in 1:length(TOTALYEARS))
{
	TOTALYEARS[i]=resp.dat$total.years[which(resp.dat$ORG==vertex_attributes$NAME[i])[1]]
}
vertex_attributes$TOTALYEARS = TOTALYEARS


NUMGROUPS = rep(0,length(vertex_attributes$NAME))
for (i in 1:length(NUMGROUPS))
{
	NUMGROUPS[i]=mean(resp.dat$NumGroups[which(resp.dat$ORG==vertex_attributes$NAME[i])])
}
vertex_attributes$NUMGROUPS = NUMGROUPS

NUMRESP = rep(0,length(vertex_attributes$NAME))
for (i in 1:length(NUMRESP))
{
	NUMRESP[i]=mean(resp.dat$Numres[which(resp.dat$ORG==vertex_attributes$NAME[i])])
}
vertex_attributes$NUMRESP = NUMRESP

MEANYEARS = rep(0,length(vertex_attributes$NAME))
for (i in 1:length(MEANYEARS))
{
	MEANYEARS[i]=mean(resp.dat$Years[which(resp.dat$ORG==vertex_attributes$NAME[i])])
}
vertex_attributes$MEANYEARS = MEANYEARS

ORGTYPE = rep(0,length(vertex_attributes$NAME))
for (i in 1:length(ORGTYPE))
{
	ORGTYPE[i]=as.character(resp.dat$ORGType[which(resp.dat$ORG==vertex_attributes$NAME[i])[1]])
}
vertex_attributes$ORGTYPE = ORGTYPE

USEPLAN = rep(0,length(vertex_attributes$NAME))
for (i in 1:length(USEPLAN))
{
	USEPLAN[i]=as.character(resp.dat$useful_plan[which(resp.dat$ORG==vertex_attributes$NAME[i])[1]])
}
vertex_attributes$USEPLAN = USEPLAN

USEWORK = rep(0,length(vertex_attributes$NAME))
for (i in 1:length(USEWORK))
{
	USEWORK[i]=as.character(resp.dat$useful_work[which(resp.dat$ORG==vertex_attributes$NAME[i])[1]])
}
vertex_attributes$USEWORK = USEWORK

USECONS = rep(0,length(vertex_attributes$NAME))
for (i in 1:length(USECONS))
{
	USECONS[i]=as.character(resp.dat$useful_cons[which(resp.dat$ORG==vertex_attributes$NAME[i])[1]])
}
vertex_attributes$USECONS = USECONS


for (i in 1:nrow(resp.dat))
{resp.dat$npsp[i]<-tapply(resp.dat$npsp,resp.dat$ORG,mean)[which(sort(unique(resp.dat$ORG))==resp.dat$ORG[i])]}


PSP_N = rep(0,length(vertex_attributes$NAME))
for (i in 1:length(PSP_N))
{
	{
	PSP_N[i]=tapply(resp.dat$psp,resp.dat$ORG,mean)[which(rownames(tapply(resp.dat$psp,resp.dat$ORG,mean))==vertex_attributes$NAME[i])[1]]
}
}
vertex_attributes$PSP_N = PSP_N

NPSP_N = rep(0,length(vertex_attributes$NAME))
for (i in 1:length(PSP_N))
{
	NPSP_N[i]=tapply(resp.dat$npsp,resp.dat$ORG,mean)[which(rownames(tapply(resp.dat$npsp,resp.dat$ORG,mean))==vertex_attributes$NAME[i])[1]]
}
vertex_attributes$NPSP_N = NPSP_N


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

as.mcmc.default <- coda:::as.mcmc.default
as.mcmc.list.default <- coda:::as.mcmc.list.default

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
#Poisson:
m <- sum(net %e% "TVAL")/network.dyadcount(net)
geo.init<-log(1-1/(m+1))
modAtry <-
    ergm(net~sum+mutual(form="min")+
transitiveweights("min","max","min")+
nodecov("NUMRESP")+nodecov("NUMGROUPS")+nodecov("MEANYEARS")+nodematch("ORGTYPE",form='sum'),
         response="TVAL", reference=~Binomial(3, exp(-1)/(1+exp(-1))),
         control=control.ergm(init=c(geo.init, rep(0, 6)),
MCMLE.maxit=200,MCMC.runtime.traceplot=F,seed=24,
MCMLE.trustregion=1000,MCMC.addto.se=T,parallel.type="SOCK",
MPLE.max.dyad.types=1e+7,parallel=8,MCMC.samplesize=10000,
MCMC.burnin=10000,MCMC.interval=1000,MCMLE.steplength=.25,
           MCMC.prop.args=list(p0=0.5)),eval.loglik=T)
save.image()