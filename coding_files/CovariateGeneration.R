


rm(list=ls())
setwd("//Users/TScott/Google Drive/elwha")
temp<-read.csv("Group.All.Dat.csv")
temp<-temp[grep("G.OTHER",temp$GroupID,invert=TRUE),]
count<-stack(table(temp$ORG));colnames(count)<-c("Count","ID")
temp$TotPart<-rowSums(cbind(temp$email,temp$meet,temp$event,temp$proj,temp$read,temp$write,temp$other))

PSPcreate<-rep(0,nrow(temp))
PSPcreate[grep("G.ECO",temp$GroupID)]<-1
PSPcreate[grep("G.LIO",temp$GroupID)]<-1
PSPcreate[grep("G.LC",temp$GroupID)]<-1
PSPcreate[grep("G.ECB",temp$GroupID)]<-1
PSPcreate[grep("G.SCIENCE",temp$GroupID)]<-1
PSPcreate[grep("G.SOCSCI",temp$GroupID)]<-1
temp$PSPcreate<-PSPcreate

tempn<-temp[temp$PSPcreate==0,]
temppsp<-temp[temp$PSPcreate==1,]
partpsp<-data.frame(as.character(temppsp$ORG),temppsp$TotPart);colnames(partpsp)<-c("ORG","Part")
partn<-data.frame(as.character(tempn$ORG),tempn$TotPart);colnames(partn)<-c("ORG","Part")

ttpsp<-data.frame(tapply(partpsp$Part,partpsp$ORG,sum))
ttpsp$ORG<-rownames(ttpsp)
rownames(ttpsp)<-seq(1,nrow(ttpsp),1)
colnames(ttpsp)[1]<-"TotPartpsp"

ttn<-data.frame(tapply(partn$Part,partn$ORG,sum))
ttn$ORG<-rownames(ttn)
rownames(ttn)<-seq(1,nrow(ttn),1)
colnames(ttn)[1]<-"TotPartn"

tt<-merge(ttpsp,ttn,by="ORG",all.x=T,all.y=T)

write.csv(tt,"//Users/TScott/Google Drive/elwha/IndirectParticipation.csv")


rm(list=ls())
setwd("//Users/TScott/Google Drive/elwha")
temp<-read.csv("Group.All.Dat.csv")
temp$TotPart<-rowSums(cbind(temp$email,temp$meet,temp$event,temp$proj,temp$read,temp$write,temp$other))

PSPcreate<-rep(0,nrow(temp))
PSPcreate[grep("G.ECO",temp$GroupID)]<-1
PSPcreate[grep("G.LIO",temp$GroupID)]<-1
PSPcreate[grep("G.LC",temp$GroupID)]<-1
PSPcreate[grep("G.ECB",temp$GroupID)]<-1
PSPcreate[grep("G.SCIENCE",temp$GroupID)]<-1
PSPcreate[grep("G.SOCSCI",temp$GroupID)]<-1
temp$PSPcreate<-PSPcreate
temp1<-temp[grep("G.OTHER",temp$GroupID,invert=TRUE),]

temp1<-temp1[temp1$PSPcreate==1,]
t<-data.frame(tapply(temp1$TotPart,paste(temp1$ORG,temp1$GroupID),sum))
t$orggroup<-rownames(t)
colnames(t)<-c("orgrouppart","orggroup")

temp1$orggroup<-paste(temp1$ORG,temp1$GroupID)

temp2<-temp1[duplicated(temp1$orggroup)==FALSE,]

temp3<-merge(t,temp2,by="orggroup")

empty<-matrix(0,nrow=length(unique(temp3$ORG)),ncol=length(unique(temp3$ORG)))
colnames(empty)<-sort(unique(temp3$ORG))
rownames(empty)<-sort(unique(temp3$ORG))

for (i in 1:length(unique(temp3$GroupID)))
{
	group<-unique(temp3$GroupID)[1];print("ok")
	tem<-temp3[temp3$GroupID==group,];print("ok")
	n<-length(unique(tem$ORG));print("ok")
	m<-matrix(0,ncol=n,nrow=n);print("ok")
	colnames(m)<-c(as.character(sort(unique(tem$ORG))));print("ok")
	rownames(m)<-c(as.character(sort(unique(tem$ORG))));print("ok")
	for (j in 1:nrow(tem))
	{
		m[which(rownames(m)==tem$ORG[j]),]<-tem$orgrouppart[j]
	}
		print("ok")
	for (k in 1:nrow(m))
	{

empty[which(rownames(m)[k]==rownames(empty)),colnames(empty) %in% colnames(m)]<-
empty[which(rownames(m)[k]==rownames(empty)),colnames(empty) %in% colnames(m)]+
m[k,]
	}
	;print("ok")
}

write.csv(empty,"//Users/TScott/Google Drive/elwha/DirectParticipationMatrixPSP.csv")



rm(list=ls())
setwd("//Users/TScott/Google Drive/elwha")
temp<-read.csv("Group.All.Dat.csv")
temp$TotPart<-rowSums(cbind(temp$email,temp$meet,temp$event,temp$proj,temp$read,temp$write,temp$other))

PSPcreate<-rep(0,nrow(temp))
PSPcreate[grep("G.ECO",temp$GroupID)]<-1
PSPcreate[grep("G.LIO",temp$GroupID)]<-1
PSPcreate[grep("G.LC",temp$GroupID)]<-1
PSPcreate[grep("G.ECB",temp$GroupID)]<-1
PSPcreate[grep("G.SCIENCE",temp$GroupID)]<-1
PSPcreate[grep("G.SOCSCI",temp$GroupID)]<-1
temp$PSPcreate<-PSPcreate
temp1<-temp[grep("G.OTHER",temp$GroupID,invert=TRUE),]
temp1<-temp1[temp1$PSPcreate==0,]
t<-data.frame(tapply(temp1$TotPart,paste(temp1$ORG,temp1$GroupID),sum))
t$orggroup<-rownames(t)
colnames(t)<-c("orgrouppart","orggroup")

temp1$orggroup<-paste(temp1$ORG,temp1$GroupID)

temp2<-temp1[duplicated(temp1$orggroup)==FALSE,]

temp3<-merge(t,temp2,by="orggroup")

empty<-matrix(0,nrow=length(unique(temp3$ORG)),ncol=length(unique(temp3$ORG)))
colnames(empty)<-sort(unique(temp3$ORG))
rownames(empty)<-sort(unique(temp3$ORG))

for (i in 1:length(unique(temp3$GroupID)))
{
	group<-unique(temp3$GroupID)[1];print("ok")
	tem<-temp3[temp3$GroupID==group,];print("ok")
	n<-length(unique(tem$ORG));print("ok")
	m<-matrix(0,ncol=n,nrow=n);print("ok")
	colnames(m)<-c(as.character(sort(unique(tem$ORG))));print("ok")
	rownames(m)<-c(as.character(sort(unique(tem$ORG))));print("ok")
	for (j in 1:nrow(tem))
	{
		m[which(rownames(m)==tem$ORG[j]),]<-tem$orgrouppart[j]
	}
		print("ok")
	for (k in 1:nrow(m))
	{

empty[which(rownames(m)[k]==rownames(empty)),colnames(empty) %in% colnames(m)]<-
empty[which(rownames(m)[k]==rownames(empty)),colnames(empty) %in% colnames(m)]+
m[k,]
	}
	;print("ok")
}
write.csv(empty,"//Users/TScott/Google Drive/elwha/DirectParticipationMatrixN.csv")




rm(list=ls())
setwd("//Users/TScott/Google Drive/elwha")
temp<-read.csv("Group.All.Dat.csv")
temp$TotPart<-rowSums(cbind(temp$email,temp$meet,temp$event,temp$proj,temp$read,temp$write,temp$other))

PSPcreate<-rep(0,nrow(temp))
PSPcreate[grep("G.ECO",temp$GroupID)]<-1
PSPcreate[grep("G.LIO",temp$GroupID)]<-1
PSPcreate[grep("G.LC",temp$GroupID)]<-1
PSPcreate[grep("G.ECB",temp$GroupID)]<-1
PSPcreate[grep("G.SCIENCE",temp$GroupID)]<-1
PSPcreate[grep("G.SOCSCI",temp$GroupID)]<-1
temp$PSPcreate<-PSPcreate

temp1<-temp[grep("G.OTHER",temp$GroupID,invert=TRUE),]
temp1<-temp1[temp1$PSPcreate==0,]


t<-data.frame(tapply(temp1$TotPart,paste(temp1$ORG,temp1$GroupID),sum))
t$orggroup<-rownames(t)
colnames(t)<-c("orgrouppart","orggroup")

temp1$orggroup<-paste(temp1$ORG,temp1$GroupID)

temp2<-temp1[duplicated(temp1$orggroup)==FALSE,]

temp3<-merge(t,temp2,by="orggroup")

empty<-matrix(0,nrow=length(unique(temp3$ORG)),ncol=length(unique(temp3$ORG)))
colnames(empty)<-sort(unique(temp3$ORG))
rownames(empty)<-sort(unique(temp3$ORG))

for (i in 1:length(unique(temp3$GroupID)))
{
	group<-unique(temp3$GroupID)[1]
	tem<-temp3[temp3$GroupID==group,]
	n<-length(unique(tem$ORG))
	m<-matrix(0,ncol=n,nrow=n)
	colnames(m)<-c(as.character(sort(unique(tem$ORG))));print("ok")
	rownames(m)<-c(as.character(sort(unique(tem$ORG))));print("ok")
	for (j in 1:nrow(tem))
	{
		m[which(rownames(m)==tem$ORG[j]),]<-tem$orgrouppart[j]
	}
	for (h in 1:nrow(m))
	{
		for (g in 1:ncol(m))
		{
			m[g,h]<-min(m[g,h],m[h,g])
		}
	}
	for (k in 1:nrow(m))
	{

empty[which(rownames(m)[k]==rownames(empty)),colnames(empty) %in% colnames(m)]<-
empty[which(rownames(m)[k]==rownames(empty)),colnames(empty) %in% colnames(m)]+
m[k,]
	}
}

write.csv(empty,"//Users/TScott/Google Drive/elwha/SharedParticipationMatrixN.csv")


rm(list=ls())
setwd("//Users/TScott/Google Drive/elwha")
temp<-read.csv("Group.All.Dat.csv")
temp$TotPart<-rowSums(cbind(temp$email,temp$meet,temp$event,temp$proj,temp$read,temp$write,temp$other))

PSPcreate<-rep(0,nrow(temp))
PSPcreate[grep("G.ECO",temp$GroupID)]<-1
PSPcreate[grep("G.LIO",temp$GroupID)]<-1
PSPcreate[grep("G.LC",temp$GroupID)]<-1
PSPcreate[grep("G.ECB",temp$GroupID)]<-1
PSPcreate[grep("G.SCIENCE",temp$GroupID)]<-1
PSPcreate[grep("G.SOCSCI",temp$GroupID)]<-1
temp$PSPcreate<-PSPcreate

temp1<-temp[grep("G.OTHER",temp$GroupID,invert=TRUE),]
temp1<-temp1[temp1$PSPcreate==1,]

t<-data.frame(tapply(temp1$TotPart,paste(temp1$ORG,temp1$GroupID),sum))
t$orggroup<-rownames(t)
colnames(t)<-c("orgrouppart","orggroup")

temp1$orggroup<-paste(temp1$ORG,temp1$GroupID)

temp2<-temp1[duplicated(temp1$orggroup)==FALSE,]

temp3<-merge(t,temp2,by="orggroup")

empty<-matrix(0,nrow=length(unique(temp3$ORG)),ncol=length(unique(temp3$ORG)))
colnames(empty)<-sort(unique(temp3$ORG))
rownames(empty)<-sort(unique(temp3$ORG))

for (i in 1:length(unique(temp3$GroupID)))
{
	group<-unique(temp3$GroupID)[1]
	tem<-temp3[temp3$GroupID==group,]
	n<-length(unique(tem$ORG))
	m<-matrix(0,ncol=n,nrow=n)
	colnames(m)<-c(as.character(sort(unique(tem$ORG))));print("ok")
	rownames(m)<-c(as.character(sort(unique(tem$ORG))));print("ok")
	for (j in 1:nrow(tem))
	{
		m[which(rownames(m)==tem$ORG[j]),]<-tem$orgrouppart[j]
	}
	for (h in 1:nrow(m))
	{
		for (g in 1:ncol(m))
		{
			m[g,h]<-min(m[g,h],m[h,g])
		}
	}
	for (k in 1:nrow(m))
	{

empty[which(rownames(m)[k]==rownames(empty)),colnames(empty) %in% colnames(m)]<-
empty[which(rownames(m)[k]==rownames(empty)),colnames(empty) %in% colnames(m)]+
m[k,]
	}
}

write.csv(empty,"//Users/TScott/Google Drive/elwha/SharedParticipationMatrixPSP.csv")







