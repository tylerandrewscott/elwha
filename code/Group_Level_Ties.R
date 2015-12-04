setwd("/Users/TScott/Google Drive/elwha/")
library(RSiena)
library(statnet)
list.files()[grep(".csv",list.files())]


test<-merge(read.csv("Group.Member.Dat.csv",row.names=1),read.csv("Response.Contact.Dat.csv",row.names=1),by="ID")

test2<-cbind(test$ORG,test[,2:64])
colnames(test2)[1]<-"ORG"

temp<-matrix(data=NA,nrow=length(unique(test2$ORG)),ncol=(ncol(test2)))
temp[,1]<-sort(unique(as.character(test2$ORG)))
temp<-data.frame(temp)
for (i in 2:(ncol(test2)))
{
temp[,i]<-tapply(test2[,i],test2$ORG,sum)
}
colnames(temp)<-colnames(test2)
temp[,2:ncol(temp)]<-as.numeric(temp[,2:ncol(temp)])

for (i in 2:ncol(temp))
{
	for (j in 1:nrow(temp))
	{
		if (temp[j,i]>0){temp[j,i]==1}
	}
}

common_members<-data.frame(matrix(0,ncol=ncol(temp)-1,nrow=ncol(temp)-1))
colnames(common_members)<-colnames(temp)[-1]
rownames(common_members)<-colnames(temp)[-1]

for (j in 2:ncol(temp))
	{print(j/(ncol(temp)-1));for (k in 2:ncol(temp)){for (i in 1:nrow(temp)){if (temp[i,j]>0&temp[i,k]>0){common_members[j-1,k-1]<-common_members[j-1,k-1]+1}}}}


tt<-read.csv("Group.Membership.Dat.csv",row.names=1)
t1_bin<-matrix(data=0,nrow=length(unique(tt$ORG)),ncol=length(unique(tt$GroupID)))
t1_val<-matrix(data=0,nrow=length(unique(tt$ORG)),ncol=length(unique(tt$GroupID)))
colnames(t1)<-unique(tt$GroupID)
rownames(t1)<-unique(tt$ORG)
for (i in 1:nrow(tt))
{
	print(paste(round(i/nrow(tt),3)*100,"%",sep=""))
	j<-grep(tt$ORG[i],rownames(t1))
	k<-grep(tt$GroupID[i],colnames(t1))
	t1_val[j,k]<-t1_val[j,k]+1
	t1_bin[j,k]<-1
}








head(t1_bin)
head(tt)
length(unique(tt$Group.ID))
tt$GroupID
t1<-tt[,grep("G.",colnames(tt))]
t2<-data.frame((t1[,1:63]))
rownames(t2)<-tt$ORG
write.table(common_members,file="")





head(common_members)

	
head(common_members)	
	head(temp)
	
warnings()
		
		for (k in 1:nrow(temp))
		{
			if (j!=k&temp[j,i]>1&temp[k,i]>1)
			{
				common_members[]<-(common_members$SHARED[i-1]+1)
			}
		}
		}
}
head(common_members)
temp[temp[,5]==1,]


head(test2[,-1])
length(test2$ORG)
nrow(test2)
temp<-read.csv("Group.Member.Dat.csv",row.names=1)
head(temp)


tapply(test2[,i],test2$ORG,sum)