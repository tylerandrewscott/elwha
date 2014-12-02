rm(list=ls())
setwd("//Users/TScott/Google Drive/elwha")
library(statnet)

#dat_imp=read.csv(file="edgelist_implement.csv",row.names=1)
#dat_plan=read.csv(file="edgelist_plan.csv",row.names=1)
#dat_cons=read.csv(file="edgelist_consult.csv",row.names=1)
dat_all=read.csv(file="edgelist_all.csv",row.names=1)
dat_all$combi = paste(dat_all$ORG,dat_all$Contact)
dat_imp = dat_all[dat_all$TType=='WT',]
dat_plan = dat_all[dat_all$TType=='PT',]
dat_cons = dat_all[dat_all$TType=='CT',]

#sum(dat_imp$combi %in% dat_plan$combi)
#sum(dat_imp$combi %in% dat_cons$combi)
#sum(dat_cons$combi %in% dat_plan$combi)
resp.dat=read.csv(file="Response.Used.csv",row.names=1)
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

test<-(read.csv(file="Group.Overlap.Matrix.PSP.csv",row.names=1))


#sort out only those who responded to survey
uq = resp.dat[unique(resp.dat$ORG),]
isolate_vertices = unique(resp.dat$ORG)[unique(resp.dat$ORG) %in% unique(dat_all$ORG)==FALSE]
nonisolate_vertices = sort(unique(dat_all$ORG))
all_vertices = c(as.character(isolate_vertices),as.character(nonisolate_vertices))
all_vertices = sort(as.factor(all_vertices))
respondent_edges = dat_all[dat_all$Contact %in% all_vertices == TRUE,]

dat_imp = dat_imp[dat_imp$Contact %in% all_vertices==TRUE,]
dat_plan = dat_plan[dat_plan$Contact %in% all_vertices==TRUE,]
dat_cons = dat_cons[dat_cons$Contact %in% all_vertices==TRUE,]


dat0 = rbind(dat_imp,dat_plan,dat_cons)
dat1 = dat0[as.character(dat0$ORG) != as.character(dat0$Contact),]
edgename = paste(dat1$ORG,"@@@",dat1$Contact)
unique_dat = dat1[ifelse(duplicated(edgename),0,1)==1,]

dat0 = dat_imp
dat1 = dat0[as.character(dat0$ORG) != as.character(dat0$Contact),]
edgename = paste(dat1$ORG,"@@@",dat1$Contact)
dat_imp = dat1[ifelse(duplicated(edgename),0,1)==1,]

dat0 = dat_cons
dat1 = dat0[as.character(dat0$ORG) != as.character(dat0$Contact),]
edgename = paste(dat1$ORG,"@@@",dat1$Contact)
dat_cons = dat1[ifelse(duplicated(edgename),0,1)==1,]

dat0 = dat_plan
dat1 = dat0[as.character(dat0$ORG) != as.character(dat0$Contact),]
edgename = paste(dat1$ORG,"@@@",dat1$Contact)
dat_plan = dat1[ifelse(duplicated(edgename),0,1)==1,]

#combine all three types of ties
dat_imp$TYPE = "IMP"
dat_plan$TYPE = "PLAN"
dat_cons$TYPE = "CONS"
temp = rbind(dat_imp,dat_plan,dat_cons)
dat_combined = temp[temp$Contact %in% all_vertices==TRUE,]

colnames(npsp_group)<-rownames(npsp_group)
colnames(psp_group)<-rownames(psp_group)
colnames(all_group)<-rownames(all_group)

net_temp = network.initialize(length(all_vertices),directed=TRUE,loops=FALSE)

vertex_attributes  = data.frame(sort(all_vertices))
colnames(vertex_attributes) = "NAME"

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

TAIL_ID = rep(0,nrow(respondent_edges))
HEAD_ID = rep(0,nrow(respondent_edges))

IMP_TAIL_ID = rep(0,nrow(dat_imp))
IMP_HEAD_ID = rep(0,nrow(dat_imp))

net_imp = net_temp
for (i in 1:nrow(dat_imp))
{
	IMP_TAIL_ID[i] = (which(network.vertex.names(net_imp)==dat_imp$ORG[i]))
	IMP_HEAD_ID[i] = (which(network.vertex.names(net_imp)==dat_imp$Contact[i]))
}

for (i in 1:length(IMP_TAIL_ID))
{
	net_imp[IMP_TAIL_ID[i],IMP_HEAD_ID[i]]<-1
}


CONS_TAIL_ID = rep(0,nrow(dat_cons))
CONS_HEAD_ID = rep(0,nrow(dat_cons))
net_cons = net_temp

for (i in 1:nrow(dat_cons))
{
	CONS_TAIL_ID[i] = (which(network.vertex.names(net_cons)==dat_cons$ORG[i]))
	CONS_HEAD_ID[i] = (which(network.vertex.names(net_cons)==dat_cons$Contact[i]))
}

for (i in 1:length(CONS_TAIL_ID))
{
	net_cons[CONS_TAIL_ID[i],CONS_HEAD_ID[i]]<-1
}


PLAN_TAIL_ID = rep(0,nrow(dat_plan))
PLAN_HEAD_ID = rep(0,nrow(dat_plan))

net_plan = net_temp

for (i in 1:nrow(dat_plan))
{
	PLAN_TAIL_ID[i] = (which(network.vertex.names(net_plan)==dat_plan$ORG[i]))
	PLAN_HEAD_ID[i] = (which(network.vertex.names(net_plan)==dat_plan$Contact[i]))
}

for (i in 1:length(PLAN_TAIL_ID))
{
	net_plan[PLAN_TAIL_ID[i],PLAN_HEAD_ID[i]]<-1
}


PLAN_TAIL_ID = rep(0,nrow(unique_dat))
PLAN_HEAD_ID = rep(0,nrow(unique_dat))

net_uq = net_temp

for (i in 1:nrow(unique_dat))
{
	PLAN_TAIL_ID[i] = (which(network.vertex.names(net_uq)==unique_dat$ORG[i]))
	PLAN_HEAD_ID[i] = (which(network.vertex.names(net_uq)==unique_dat$Contact[i]))
}

for (i in 1:length(PLAN_TAIL_ID))
{
	net_uq[PLAN_TAIL_ID[i],PLAN_HEAD_ID[i]]<-1
}

net_temp_p <- net_plan
net_temp_i <- net_imp
net_temp_c <- net_cons
net_temp_c$gal[6]=TRUE
set.edge.value(net_temp_p,"T_VALUE",value=2)
set.edge.value(net_temp_i,"T_VALUE",value=3)
set.edge.value(net_temp_c,"T_VALUE",value=1)

net_temp_all <-net_temp_c


add.edges(net_temp_all,tail=as.edgelist(net_temp_i)[,1],head=as.edgelist(net_temp_i)[,2],names.eval="T_VALUE",vals.eval=3)
add.edges(net_temp_all,tail=as.edgelist(net_temp_p)[,1],head=as.edgelist(net_temp_p)[,2],names.eval="T_VALUE",vals.eval=2)


#set.edge.attribute(net_temp_all,"WIN5_dk1",value=)
#set.edge.attribute(net_temp_all,"WIN5_dk0",value=)

for (i in 1:nrow(dat_imp))
{
	t<-which(network.vertex.names(net_temp_i)==dat_imp$ORG[i])
	h<-which(network.vertex.names(net_temp_i)==dat_imp$Contact[i])
	if (h>0)
	{
		set.edge.attribute(net_temp_i,"WIN5_dk1",value=ifelse(dat_imp$WIN5>0,1,dat_imp$WIN5))
		set.edge.attribute(net_temp_i,"WIN5_dk0",value=ifelse(dat_imp$WIN5==1,1,0))
	}
}

for (i in 1:nrow(dat_cons))
{
	t<-which(network.vertex.names(net_temp_c)==dat_cons$ORG[i])
	h<-which(network.vertex.names(net_temp_c)==dat_cons$Contact[i])
	if (h>0)
	{
		set.edge.attribute(net_temp_c,"WIN5_dk1",value=ifelse(dat_cons$WIN5>0,1,dat_cons$WIN5))
		set.edge.attribute(net_temp_c,"WIN5_dk0",value=ifelse(dat_cons$WIN5==1,1,0))
	}
}

for (i in 1:nrow(dat_plan))
{
	t<-which(network.vertex.names(net_temp_p)==dat_plan$ORG[i])
	h<-which(network.vertex.names(net_temp_p)==dat_plan$Contact[i])
	if (h>0)
	{
		set.edge.attribute(net_temp_p,"WIN5_dk1",value=ifelse(dat_plan$WIN5>0,1,dat_plan$WIN5))
		set.edge.attribute(net_temp_p,"WIN5_dk0",value=ifelse(dat_plan$WIN5==1,1,0))
	}
}

#for (i in 1:nrow(dat_all))
#{
#	t<-which(network.vertex.names(net_temp_all)==dat_all$ORG[i])
#	h<-which(network.vertex.names(net_temp_all)==dat_all$Contact[i])
#	if (h>0)
#	{
#		set.edge.attribute(net_temp_all,"WIN5_dk1",value=ifelse(dat_all$WIN5>0,1,dat_all$WIN5))
#		set.edge.attribute(net_temp_all,"WIN5_dk0",value=ifelse(dat_all$WIN5==1,1,0))
#	}
#}


dat_plan2 = dat_plan[dat_plan$combi %in% dat_imp$combi == FALSE,]

dat_cons2 = dat_cons[dat_cons$combi %in% dat_imp$combi | dat_cons$combi %in% dat_plan$combi == FALSE,]

full_temp = rbind(dat_imp,dat_plan2,dat_cons2)


net_temp = network.initialize(length(all_vertices),directed=TRUE,loops=FALSE)
network.vertex.names(net_temp) = as.character(all_vertices)

TEMP_TAIL_ID = rep(0,nrow(full_temp))
TEMP_HEAD_ID = rep(0,nrow(full_temp))

for (i in 1:nrow(full_temp))
{
  TEMP_TAIL_ID[i] = (which(network.vertex.names(net_temp)==full_temp$ORG[i]))
  TEMP_HEAD_ID[i] = (which(network.vertex.names(net_temp)==full_temp$Contact[i]))
}

add.edges(net_temp,tail=TEMP_TAIL_ID,head=TEMP_HEAD_ID)

summary(degree(net_temp,gmode='digraph',cmode='indegree'))
sd(degree(net_temp,gmode='digraph',cmode='indegree'))
summary(degree(net_temp,gmode='digraph',cmode='outdegree'))
sd(degree(net_temp,gmode='digraph',cmode='outdegree'))



save.image(file="Ready_to_ERGM.RData")



