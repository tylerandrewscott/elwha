rm(list=ls())
setwd("//Users/TScott/Google Drive/elwha")

Long.Data1<-read.csv("Final_Verbose.csv",header=T,sep=",")
Short.Data1<-read.csv("Final_Scrubbed.csv",header=T)

Long.Data1<-Long.Data1[Long.Data1$Response.ID %in% Short.Data1$Response.ID==TRUE,]
SD<-Short.Data1
SD.fullorgs<-subset(SD,SD[,20]!="")
SD.fullorgs$Last.page.saved<-as.numeric(gsub("P","",SD.fullorgs$Last.page.saved))
SD.answers<-subset(SD.fullorgs,SD.fullorgs$Last.page.saved>=15)
data2<-SD.answers
WD<-Long.Data1

WD[,91]<-SD[,20]

WD.fullorgs<-subset(WD,WD[,91]!="")
WD.fullorgs$Last.page.saved<-as.numeric(gsub("P","",WD.fullorgs$Last.page.saved))
WD.answers<-subset(WD.fullorgs,WD.fullorgs$Last.page.saved>=15)
dat1<-WD.answers
#Long.Data<-dat1
#Short.Data<-data2

Long.Data<-dat1
Short.Data<-data2


colnames(Short.Data)[256]<-"Please.describe.your.regular.level.of.involvement.in.the.following.group.activities.for.the.collaborative.groups.in.which.you.participate..For.each.activity.and.group.you.participate.in..please.check.the.box.if.you.regularly.engage.in.the.specified.action..Regularly.participate.in.the.following.activties.Other.1"
group.name.vec<-grep("Leadership.Council",colnames(Long.Data))
group.name.vec
group.name.vec<-gsub("Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..","",colnames(Short.Data)[104:177])
group.names<-group.name.vec[c(-1*grep("One.or.more",group.name.vec,invert=FALSE),-1*grep("None",group.name.vec,invert=FALSE))]




Group.Membership.Dat<-read.csv("//Users/TScott/Google Drive/elwha/Group.Membership.Dat.csv",row.names=1)

Gmem.dat<-read.csv("Group.Member.Dat.csv",row.names=1,header=T)
Grea.dat<-read.csv("Group.Reason.Dat.csv",row.names=1,header=T)
Gpar.dat<-read.csv("Group.Part.Dat.csv",row.names=1,header=T)

Org.Reps.dat<-read.csv("Org.Dat.csv",row.names=1,header=T)





nn.s<-length(grep(group.names[2],colnames(Short.Data)))
nn.l<-length(grep(group.names[2],colnames(Long.Data)))

group.pulls.long<-data.frame(matrix(rep(0,nn.l),ncol=nn.l))
group.pulls.short<-data.frame(matrix(rep(0,nn.s),ncol=nn.s))
length(grep(group.names[2],colnames(Long.Data)))
for (i in 1:length(group.names))
{
	for (j in 1:nn.s)
	{
	group.pulls.short[i,1:nn.s]<-grep(group.names[i],colnames(Short.Data))
	group.pulls.short[i,nn.s+1]<-group.names[i]
	}
}

for (i in 1:length(group.names))
{
	for (j in 1:nn.l)
	{
	group.pulls.long[i,1:nn.l]<-grep(group.names[i],colnames(Long.Data))
	group.pulls.long[i,(nn.l+1)]<-group.names[i]
	}
}


colnames(group.pulls.long)<-c(gsub("The.PSP.Leadership.Council","",colnames(Long.Data)[c(as.numeric(group.pulls.long[1,1:(ncol(group.pulls.long)-1)]))]),"GroupID")
colnames(group.pulls.short)<-c(gsub("The.PSP.Leadership.Council","",colnames(Short.Data)[c(as.numeric(group.pulls.short[1,1:(ncol(group.pulls.short)-1)]))]),"GroupID")



group.effects<-c("face.to.face","values","language","scientific","human.resources","financial.resources")





gname<-Gmem.dat
gind<-gname

gind[,65]<-ifelse((as.numeric(gind[,65])-1)==0,0,1)
gind[,66]<-ifelse((as.numeric(gind[,66])-1)==0,0,1)
gind[,67]<-ifelse((as.numeric(gind[,67])-1)==0,0,1)
gind[,68]<-ifelse((as.numeric(gind[,68])-1)==0,0,1)
gind[,69]<-ifelse((as.numeric(gind[,69])-1)==0,0,1)

gtemp<-gind
#gtemp<-cbind(gtemp[,1:64],as.vector(gtemp[,65]),as.vector(gtemp[,66]),as.vector(gtemp[,67]),as.vector(gtemp[,68]),as.vector(gtemp[,69]))


G.reorder<-data.frame(cbind(gind[,2:ncol(Gmem.dat)],gind[,1]))
colnames(G.reorder)[ncol(G.reorder)]<-"ID"

temp<-cbind(
as.numeric(c(335:339)),
as.numeric(c(414:418)),
as.numeric(c(493:497)),
as.numeric(c(572:576)),
as.numeric(c(651:655)),
as.numeric(c(730:734)),
c("Other.1","Other.2","Other.3","Other.4","Other.5"))
colnames(temp)<-colnames(group.pulls.short)[(ncol(group.pulls.short)-6):ncol(group.pulls.short)]

temp1<-data.frame(rbind(
group.pulls.short[,(ncol(group.pulls.short)-6):ncol(group.pulls.short)],temp))
group.eff.pulls<-temp1
head(group.eff.pulls)
temp.pull<-group.eff.pulls[,-7]

colnames(temp.pull)<-seq(1,6,1)

pull.vec<-data.frame(matrix(rep(0,6*sum(G.reorder[,-ncol(G.reorder)])),ncol=6))

#colnames(Short.Data)[1]<-"ID"
#new.index<-merge(Short.Data,G.reorder,by="ID")
#Short.Data<-new.index[,1:805]

tempp<-data.frame(pull.vec)
p<-cumsum(t(G.reorder[1:nrow(G.reorder),-ncol(G.reorder)]))
place<-(matrix(p,ncol=68,byrow=TRUE))


for (i in 1:nrow(Short.Data))
{for (j in 1:nrow(temp.pull))
	{if (G.reorder[i,j]==1)
		{
				tempp[place[i,j],1]<-
				(Short.Data[i,(as.numeric(group.eff.pulls[j,1]))]);
				tempp[place[i,j],2]<-
				(Short.Data[i,(as.numeric(group.eff.pulls[j,2]))]);
				tempp[place[i,j],3]<-
				(Short.Data[i,(as.numeric(group.eff.pulls[j,3]))]);
				tempp[place[i,j],4]<-
				(Short.Data[i,(as.numeric(group.eff.pulls[j,4]))]);
				tempp[place[i,j],5]<-
				(Short.Data[i,(as.numeric(group.eff.pulls[j,5]))]);
				tempp[place[i,j],6]<-
				(Short.Data[i,(as.numeric(group.eff.pulls[j,6]))])
				tempp[place[i,j],7]<-
				(as.character(Short.Data[i,1]))
				tempp[place[i,j],8]<-
				(as.character(colnames(G.reorder)[j]))
		}
	}
}

group.effects<-c("face.to.face","values","language","scientific","human.resources","financial.resources","ID","GroupID")
colnames(tempp)<-group.effects
Group.Effects.Dat<-tempp


Group.All<-merge(Group.Membership.Dat,Group.Effects.Dat,by=c("ID","GroupID"))


write.csv(Group.All,file="Group.All.Dat.csv")

