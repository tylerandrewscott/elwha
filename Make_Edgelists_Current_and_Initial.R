#begin script 5

Wide.Data<-read.csv("//Users/TScott/Google Drive/PSP_Project/PS_Data/Final_Verbose.csv",header=T,sep=",")
Short.Data<-read.csv("//Users/TScott/Google Drive/PSP_Project/PS_Data/Final_Scrubbed.csv",header=T)
SD<-Short.Data
SD.fullorgs<-subset(SD,SD[,20]!="")
SD.fullorgs$Last.page.saved<-as.numeric(gsub("P","",SD.fullorgs$Last.page.saved))
SD.answers<-subset(SD.fullorgs,SD.fullorgs$Last.page.saved>=15)
dat2<-SD.answers
WD<-Wide.Data
WD.fullorgs<-subset(WD,WD[,91]!="")
WD.fullorgs$Last.page.saved<-as.numeric(gsub("P","",WD.fullorgs$Last.page.saved))
WD.answers<-subset(WD.fullorgs,WD.fullorgs$Last.page.saved>=15)
dat1<-WD.answers


dat4<-read.csv("//Users/TScott/Google Drive/PSP_Project/PS_Data/Group.Member.Dat.csv",row.names=1,header=T)
temp1<-dat4[,-1]
names(temp1)
temp1$G.OTHER.1<-ifelse(temp1$G.OTHER.1=="",0,1)
temp1$G.OTHER.2<-ifelse(temp1$G.OTHER.2=="",0,1)
temp1$G.OTHER.3<-ifelse(temp1$G.OTHER.3=="",0,1)
temp1$G.OTHER.4<-ifelse(temp1$G.OTHER.4=="",0,1)
temp1$G.OTHER.5<-ifelse(temp1$G.OTHER.5=="",0,1)
Num.Groups1<-rowSums(temp1)
dat4$Num.Groups<-Num.Groups1
dat2<-SD.answers
ng<-data.frame(cbind(as.character(dat1$ID),dat4$NumGroups));colnames(ng)<-c("ID","NumGroups")

dat2<-merge(dat2,ng,by.x="Response.ID",by.y="ID",all.x=TRUE,all.y=FALSE)



aa<-(c(
grep("routinely.implement",colnames(dat2)),
grep("work.on.joint.projects.or.programs.with.these.organizations.begin.within.the.past.five.years",colnames(dat2)),
grep("routinely.coordinate.plans",colnames(dat2)),
grep("coordinated.planning.or.strategizing.with.these.organizations.begin.within.the.last.five.years",colnames(dat2)),
grep("do.you.informally.consult",colnames(dat2)),
grep("informal.consultation.with.these.organizations.begin.within.the.last.five.years",colnames(dat2))
))



net1.calls<-cbind(
grep("routinely.implement",colnames(dat2)),
grep("routinely.coordinate.plans",colnames(dat2)),
grep("do.you.informally.consult",colnames(dat2)))

net0ind.calls<-cbind(
grep("work.on.joint.projects.or.programs.with.these.organizations.begin.within.the.past.five.years",colnames(dat2)),
grep("coordinated.planning.or.strategizing.with.these.organizations.begin.within.the.last.five.years",colnames(dat2)),
grep("informal.consultation.with.these.organizations.begin.within.the.last.five.years",colnames(dat2)))

colnames(net1.calls)<-c("Work1","Plan1","Cons1")
colnames(net0ind.calls)<-c("Work0","Plan0","Cons0")
rownames(net1.calls)<-c("T1","T2","T3","T4","T5")
rownames(net0ind.calls)<-c("T1","T2","T3","T4","T5")

#Network 1
Network1<-data.frame(dat2[,1],dat2[,net1.calls])
head(Network1)

#network indicator
#temp<-Network1[,-1]
#temp2<-temp
#for (i in 1:nrow(temp))
#{
#	for (i in 1:ncol(temp))
#	{
#		if (temp[i,j]==""){temp2[i,j]==0}
#		else {temp2[i,j]==1}
#	}
#}


#old tie indicator
Nettie<-data.frame(dat2[,1],dat2[,net0ind.calls])
Numtie<-Nettie


for (i in 1:nrow(Numtie))
{
	for (j in 1:ncol(Numtie))
	{
		Numtie[i,j]<-ifelse(is.na(Nettie[i,j])==TRUE,NA,Nettie[i,j])
	}
}


Network0<-Network1

#build network 0
for (i in 1:nrow(Network1))
{
	for (j in 2:ncol(Network1))
	{
		if (Numtie[i,j]==1){Network0[i,j]<-Network1[i,j]}
		else if (Numtie[i,j]==0){Network0[i,j]<-0}
		else if (Numtie[i,j]==-99){Network0[i,j]<--99}	
		else if (Numtie[i,j]==0.5){Network0[i,j]<-Network1[i,j]}
	}
}



#build edgelist
#########################
#########################

temp<-merge(dat2,dat4,by.x="Response.ID",by.y="ID")

Org.Rep<-data.frame(cbind(as.character(dat2$Response.ID),as.character(dat2$What.organization.do.you.primarily.represent..work.for..volunteer.for..or.are.otherwise.affiliated.with..on.the.collaborative.group.s..that.you.previously.identified..Organization),as.character(dat2$ExtraOrg),as.character(dat2$OrgType),(dat2$How.many.years.have.you.worked.at.or.volunteered.for.the.organization.that.you.identified.above..Years),(dat4$Num.Groups)))

colnames(Org.Rep)
colnames(Org.Rep)<-c("ID","ORG","Details","ORGType","Years","NumGroups")

colnames(Network1)[1]<-c("ID")

egolist<-Org.Rep$ORG
egolist2<-ifelse(egolist=="","nonegiven",as.character(egolist))
egolist3<-na.omit(egolist2)
rescount<-table(egolist3)
rescountn<-as.vector(rescount)
newvar<-rep(0,length(egolist3))
for (i in 1:length(egolist3))
{
	pullv<-grep(egolist3[i],names(rescount))
	newvar[i]<-rescountn[pullv]
}

Org.Rep<-data.frame(cbind(Org.Rep,newvar))


colnames(Org.Rep)[ncol(Org.Rep)]<-c("Numres")

OR<-Org.Rep


temp<-merge(OR,Network1)

EMatrix<-data.frame(temp)
E<-EMatrix
for (i in 1:ncol(EMatrix))
{
	E[,i]<-as.vector(as.character(EMatrix[,i]))
}

total.years<-rep(0,nrow(EMatrix))

for (i in 1:nrow(EMatrix))
{
	total.years[i]<-sum(as.numeric(EMatrix$Years[subset(grep(EMatrix$ORG[i],EMatrix$ORG),grep(EMatrix$ORG[i],EMatrix$ORG,value=TRUE)==EMatrix$ORG[i])]))
}

total.years

tempdat<-data.frame(EMatrix,total.years)

write.csv(tempdat,file="//Users/TScott/Google Drive/PSP_Project/PS_Data/Response.Contact.Dat.csv")

E<-tempdat

colnames(E)[8:22]<-c("T1","T2","T3","T4","T5","T6","T7","T8","T9","T10","T11","T12","T13","T14","T15")
ee<-as.matrix(E)
ee1<-c(ee[,8],ee[,9],ee[,10],ee[,11],ee[,12],ee[,13],ee[,14],ee[,15],ee[,16],ee[,17],ee[,18],ee[,19],ee[,20],ee[,21],ee[,22])

E.long<-data.frame(cbind(rep(as.character(E$ID),15),rep(as.character(E$ORG),15),rep(as.character(E$Details),15),rep(as.character(E$ORGType),15),rep(E$Years,15),rep(E$NumGroups,15),rep(E$Numres,15),rep(E$total.years,15),ee1,rep(c(rep("WT",5),rep("PT",5),rep("CT",5)),nrow(E))))

colnames(E.long)<-c("ID","ORG","Details","ORGType","Years","NumGroups","Numres","total.years","Contact","TType")


ww<-subset(E.long,E.long$ORG!="")
vv<-subset(ww,ww$Contact!="")
Edgelist.Dat<-vv
head(sort(Edgelist.Dat$Contact),20)

write.csv(Edgelist.Dat,file="//Users/TScott/Google Drive/PSP_Project/PS_Data/Edgelist.Dat.Good.csv")


temp<-merge(OR,Network0)

EMatrix<-data.frame(temp)
E<-EMatrix
for (i in 1:ncol(EMatrix))
{
	E[,i]<-as.vector(as.character(EMatrix[,i]))
}

total.years<-rep(0,nrow(EMatrix))

for (i in 1:nrow(EMatrix))
{
	total.years[i]<-sum(as.numeric(EMatrix$Years[subset(grep(EMatrix$ORG[i],EMatrix$ORG),grep(EMatrix$ORG[i],EMatrix$ORG,value=TRUE)==EMatrix$ORG[i])]))
}

total.years

tempdat<-data.frame(EMatrix,total.years)

write.csv(tempdat,file="//Users/TScott/Google Drive/PSP_Project/PS_Data/Response.Contact.Dat_initnet.csv")

E<-tempdat

colnames(E)[8:22]<-c("T1","T2","T3","T4","T5","T6","T7","T8","T9","T10","T11","T12","T13","T14","T15")
ee<-as.matrix(E)
ee1<-c(ee[,8],ee[,9],ee[,10],ee[,11],ee[,12],ee[,13],ee[,14],ee[,15],ee[,16],ee[,17],ee[,18],ee[,19],ee[,20],ee[,21],ee[,22])

E.long<-data.frame(cbind(rep(as.character(E$ID),15),rep(as.character(E$ORG),15),rep(as.character(E$Details),15),rep(as.character(E$ORGType),15),rep(E$Years,15),rep(E$NumGroups,15),rep(E$Numres,15),rep(E$total.years,15),ee1,rep(c(rep("WT",5),rep("PT",5),rep("CT",5)),nrow(E))))

colnames(E.long)<-c("ID","ORG","Details","ORGType","Years","NumGroups","Numres","total.years","Contact","TType")


ww<-subset(E.long,E.long$ORG!="")
vv<-subset(ww,ww$Contact!="")
Edgelist.Dat<-vv


write.csv(Edgelist.Dat,file="//Users/TScott/Google Drive/PSP_Project/PS_Data/Edgelist.Dat.Good_initnet.csv")

