rm(list=ls())

library(statnet)

dat_imp=read.csv(file="//Users/TScott/Google Drive/PSP_Project/PS_Data/edgelist_implement.csv",row.names=1)
dat_plan=read.csv(file="//Users/TScott/Google Drive/PSP_Project/PS_Data/edgelist_plan.csv",row.names=1)
dat_cons=read.csv(file="//Users/TScott/Google Drive/PSP_Project/PS_Data/edgelist_consult.csv",row.names=1)
dat_all=read.csv(file="//Users/TScott/Google Drive/PSP_Project/PS_Data/edgelist_all.csv",row.names=1)
resp.dat=read.csv(file="//Users/TScott/Google Drive/PSP_Project/PS_Data/Response.Contact.Dat.csv",row.names=1)

psp_group = read.csv(file="//Users/TScott/Google Drive/PSP_Project/PS_Data/Group.Overlap.Matrix.PSP.csv",row.names=1)
all_group = read.csv(file="//Users/TScott/Google Drive/PSP_Project/PS_Data/Group.Overlap.Matrix.csv",row.names=1)
npsp_group = read.csv(file="//Users/TScott/Google Drive/PSP_Project/PS_Data/Group.Overlap.Matrix.NPSP.csv",row.names=1)





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


#combine all three types of ties
dat_imp$TYPE = "IMP"
dat_plan$TYPE = "PLAN"
dat_cons$TYPE = "CONS"
temp = rbind(dat_imp,dat_plan,dat_cons)
dat_combined = temp[temp$Contact %in% all_vertices==TRUE,]

#reorder weighted matrices alphabetically by org name
temp = psp_group
temp1 = matrix(0,ncol=ncol(temp),nrow=nrow(temp))
for (i in 1:ncol(temp))
{
temp1[,i] = temp[i][order(rownames(temp)),]
}
rownames(temp1) = sort(rownames(temp))
colnames(temp1) = colnames(temp)
temp2 = matrix(0,ncol=ncol(temp),nrow=nrow(temp))
for (i in 1:ncol(temp))
{
	temp2[i,] = temp1[i,][sort(as.character(colnames(temp1)))]
}
rownames(temp2) = rownames(temp1)
colnames(temp2) = rownames(temp1)
psp_group_order = temp2

temp = npsp_group
temp1 = matrix(0,ncol=ncol(temp),nrow=nrow(temp))
for (i in 1:ncol(temp))
{
temp1[,i] = temp[i][order(rownames(temp)),]
}
rownames(temp1) = sort(rownames(temp))
colnames(temp1) = colnames(temp)
temp2 = matrix(0,ncol=ncol(temp),nrow=nrow(temp))
for (i in 1:ncol(temp))
{
	temp2[i,] = temp1[i,][sort(as.character(colnames(temp1)))]
}
rownames(temp2) = rownames(temp1)
colnames(temp2) = rownames(temp1)
npsp_group_order = temp2


temp = all_group
temp1 = matrix(0,ncol=ncol(temp),nrow=nrow(temp))
for (i in 1:ncol(temp))
{
temp1[,i] = temp[i][order(rownames(temp)),]
}
rownames(temp1) = sort(rownames(temp))
colnames(temp1) = colnames(temp)
temp2 = matrix(0,ncol=ncol(temp),nrow=nrow(temp))
for (i in 1:ncol(temp))
{
	temp2[i,] = temp1[i,][sort(as.character(colnames(temp1)))]
}
rownames(temp2) = rownames(temp1)
colnames(temp2) = rownames(temp1)
all_group_order = temp2






net_temp = network.initialize(length(all_vertices),directed=TRUE,loops=TRUE)

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


network.vertex.names(net_temp) = as.character(vertex_attributes$NAME)
set.vertex.attribute(net_temp,"ORGTYPE",value=vertex_attributes$ORGTYPE)
set.vertex.attribute(net_temp,"TOTALYEARS",value=vertex_attributes$TOTALYEARS)
set.vertex.attribute(net_temp,"NUMGROUPS",value=vertex_attributes$NUMGROUPS)
set.vertex.attribute(net_temp,"NUMRESP",value=vertex_attributes$NUMRESP)
set.vertex.attribute(net_temp,"MEANYEARS",value=vertex_attributes$MEANYEARS)


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

net_temp_p
net_temp_c
net_temp_i
net_temp_all
net_temp_p$mel[[]]$inl
net_temp_p$mel[[]]$outl
net_temp_p$mel[[]]$T_VALUE

get.edge.attribute(net_temp_all,"T_VALUE")











net_imp


net_plan
net_cons
net_imp

t = dat_combined[dat_combined$TYPE=="IMP",]
tt = (t[order(t$ORG),])

tt[as.character(tt$ORG)==as.character(tt$Contact),]

dat_combined$TYPE=="PLAN"
head(dat_combined)


dim(dat_combined)

net_temp[head(TAIL_ID)[1],head(HEAD_ID)[1]]
head(respondent_edges)


network.vertic




respondent_edges[respondent_edges$Contact =="WDOE",]
head(respondent_edges)


small = dat_combined[1:10,]
add.edges(net_temp,tail=as.character(small$ORG),head=small$Contact, names.eval="TYPE", vals.eval=small$TYPE)


add.edges(net_temp,tail=dat_combined$ORG[1:10],head=dat_combined$Contact[1:10],names.eval="TYPE",vals.eval=dat_combined$TYPE[1:10])


head(dat_combined,10)

respondent_edges$Contact %in% network.vertex.names(net_temp)

as.character(respondent_edges$ORG[1])

names(vertex_attributes)
seq_len(network.size(net_temp))
vertex_attributes$ORGTYPE[63] = "Local_Commission"

warnings()
unique(vertex_attributes$ORGTYPE)

all_net = network.initialize(dim(vertex_attributes)[1],directed=TRUE)
network.vertex.names(all_net) = as.character(vertex_attributes$NAME)
set.vertex.attribute(all_net,attrname=colnames(vertex_attributes)[2],value=vertex_attributes[,2])
set.vertex.attribute(all_net,attrname=colnames(vertex_attributes)[-1],value=vertex_attributes[,3])
set.vertex.attribute(all_net,attrname=colnames(vertex_attributes)[3],value=vertex_attributes[,3])
set.vertex.attribute(all_net,attrname=colnames(vertex_attributes)[4],value=vertex_attributes[,4])
set.vertex.attribute(all_net,attrname=colnames(vertex_attributes)[5],value=vertex_attributes[,5])
set.vertex.attribute(all_net,attrname=colnames(vertex_attributes)[6],value=vertex_attributes[,6])


set.edge.attribute(all_net,"PSP_OVERLAP",value=psp_group)
set.edge.attribute(all_net,"NPSP_OVERLAP",value=npsp_group)
set.edge.attribute(all_net,"OVERLAP",value=all_group)

add.edge(all_net,tail=as.character(dat_all_resp$ORG),head=as.character(dat_all_resp$Contact))


test_net = as.network.matrix(dat_all_resp,matrix.type="edgelist",directed=TRUE)
add.vertices(test_net,length(isolate_vertices))


length(unique(c(as.character(dat_all_resp$ORG),as.character(dat_all_resp$Contact))))


network.vertex.names(test_net)[201:222] = as.character(isolate_vertices)
network.vertex.names(test_net)



isolate_vertices
?add.vertices
as.character(dat_all_resp$ORG) %in% network.vertex.names(all_net)

?add.isolates
dat_all_resp = dat_all[dat_all$Contact %in% network.vertex.names(all_net)==TRUE,]

?add.edges

dat_imp_resp = dat_imp[dat_imp$Contact %in% network.vertex.names(all_net)==TRUE,]
dat_cons_resp = dat_cons[dat_cons$Contact %in% network.vertex.names(all_net)==TRUE,]
dat_plan_resp = dat_plan[dat_plan$Contact %in% network.vertex.names(all_net)==TRUE,]




dim(dat_all)


head(dat_all)

?add.edge
temp = psp_group[order(colnames(psp_group)&colnames(psp_group))][1:10,1:10]
temp2 = temp[order(rownames(temp))]

temp
temp2



list.vertex.attributes(all_net)

head(vertex_attributes)

imp_net = network.initialize(dim(vertex_attributes)[1],directed=TRUE)
plan_net = network.initialize(dim(vertex_attributes)[1],directed=TRUE)
cons_net = network.initialize(dim(vertex_attributes)[1],directed=TRUE)



temp = read.csv(file="//Users/TScott/Google Drive/PSP_Project/PS_Data/Group.Overlap.Matrix.csv",row.names=1)

dim(temp)
dim(vertex_attributes)

rownames(temp)[rownames(temp) %in% vertex_attributes$NAME==FALSE]

resp.dat$ORG[resp.dat$ORG=="Snohomish County Surface Water Management"]

vertex_attributes$NAME


temp[0:10,0:10]



head(sort(dat_all$Contact),20)
vertexes.all<-sort(unique(c(as.character(dat_all[,1]),subset(as.character(dat_all[,2]),dat_all[,2]!=""&dat_all[,2]!=" "))))

vertexes.all

observed.vertices<-(resp.dat[duplicated(resp.dat$ORG)==FALSE,])

pull = rep(0,length(observed.vertices$ORG))
for (i in 1:length(observed.vertices$ORG))
{
	pull[i] = which(vertexes.all == observed.vertices$ORG[i])
}

observed.vertexes = sort(vertexes.all[pull])







