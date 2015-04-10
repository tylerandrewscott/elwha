


rm(list=ls())
setwd("//Users/TScott/Google Drive/elwha")
temp<-read.csv("Group.All.Dat.csv")
temp$TotPart<-rowSums(cbind(temp$email,temp$meet,temp$event,temp$proj,temp$read,temp$write,temp$other))

temp1<-temp[grep("G.OTHER",temp$GroupID,invert=TRUE),]
temp1$uq<-paste(temp1$ORG,temp1$GroupID)

head(temp1)
uq.email<-paste(temp1$ORG,temp1$GroupID,temp1$email)
uq.meet<-paste(temp1$ORG,temp1$GroupID,temp1$meet)
uq.event<-paste(temp1$ORG,temp1$GroupID,temp1$event)
uq.proj<-paste(temp1$ORG,temp1$GroupID,temp1$proj)
uq.read<-paste(temp1$ORG,temp1$GroupID,temp1$read)
uq.write<-paste(temp1$ORG,temp1$GroupID,temp1$write)
uq.other<-paste(temp1$ORG,temp1$GroupID,temp1$other)

head(uq.other)
email.sum<-tapply(temp1$email,paste(temp1$ORG,temp1$GroupID),sum)
email.sum.df<-data.frame(email.sum)
meet.sum<-tapply(temp1$meet,paste(temp1$ORG,temp1$GroupID),sum)
meet.sum.df<-data.frame(meet.sum)
event.sum<-tapply(temp1$event,paste(temp1$ORG,temp1$GroupID),sum)
event.sum.df<-data.frame(event.sum)
proj.sum<-tapply(temp1$proj,paste(temp1$ORG,temp1$GroupID),sum)
proj.sum.df<-data.frame(proj.sum)
read.sum<-tapply(temp1$read,paste(temp1$ORG,temp1$GroupID),sum)
read.sum.df<-data.frame(read.sum)
write.sum<-tapply(temp1$write,paste(temp1$ORG,temp1$GroupID),sum)
write.sum.df<-data.frame(write.sum)
other.sum<-tapply(temp1$other,paste(temp1$ORG,temp1$GroupID),sum)
other.sum.df<-data.frame(other.sum)

test<-merge(email.sum.df,meet.sum.df,by.x=rownames(email.sum.df),by.y=rownames(meet.sum.df))


emat<-matrix(0,nrow=length(unique(temp1$ORG)),ncol=length(unique(temp1$ORG)))
colnames(emat)<-sort(unique(temp1$ORG))
rownames(emat)<-sort(unique(temp1$ORG))

for (i in 1:length(unique(temp1$ORG)))
{
	tem<-temp1[temp1$ORG==sort(unique(temp1$ORG))[i],]
	{
		for (j in 1:nrow(tem$GroupID))
		{
			gr<-temp1[temp1$GroupID==tem$GroupID[j],]
			{
				for (k in 1:nrow(gr))
				{
					c<-which(colnames(emat)==gr$ORG[k])
					r<-which(colnames(emta)==temp1$ORG[i])
					gr$email[k]
					
				}
				
			}
		}
		
	}
}
for ()




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
	group<-unique(temp3$GroupID)[i]
	tem<-temp3[temp3$GroupID==group,]
	n<-length(unique(tem$ORG))
	m<-matrix(0,ncol=n,nrow=n)
	colnames(m)<-c(as.character(sort(unique(tem$ORG))))
	rownames(m)<-c(as.character(sort(unique(tem$ORG))))
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
	group<-unique(temp3$GroupID)[i]
	tem<-temp3[temp3$GroupID==group,]
	n<-length(unique(tem$ORG))
	m<-matrix(0,ncol=n,nrow=n)
	colnames(m)<-c(as.character(sort(unique(tem$ORG))))
	rownames(m)<-c(as.character(sort(unique(tem$ORG))))
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

write.csv(empty,"//Users/TScott/Google Drive/elwha/ShPartSameMatrix.csv")









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
	group<-unique(temp3$GroupID)[i]
	tem<-temp3[temp3$GroupID==group,]
	n<-length(unique(tem$ORG))
	m<-matrix(0,ncol=n,nrow=n)
	colnames(m)<-c(as.character(sort(unique(tem$ORG))))
	rownames(m)<-c(as.character(sort(unique(tem$ORG))))
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



