
##################################
#begin script 4
rm(list=ls())
setwd("//Users/TScott/Google Drive/elwha")
Gmem.dat<-read.csv("Group.Member.Dat.csv",row.names=1,header=T)
Grea.dat<-read.csv("Group.Reason.Dat.csv",row.names=1,header=T)
Gpar.dat<-read.csv("Group.Part.Dat.csv",row.names=1,header=T)


Org.Reps.dat<-read.csv("Org.Dat.csv",row.names=1,header=T)

gname<-Gmem.dat
gind<-gname

gind[,65]<-ifelse((as.numeric(gind[,65])-1)==0,0,1)
gind[,66]<-ifelse((as.numeric(gind[,66])-1)==0,0,1)
gind[,67]<-ifelse((as.numeric(gind[,67])-1)==0,0,1)
gind[,68]<-ifelse((as.numeric(gind[,68])-1)==0,0,1)
gind[,69]<-ifelse((as.numeric(gind[,69])-1)==0,0,1)

gtemp<-gind
#gtemp<-cbind(gtemp[,1:64],as.vector(gtemp[,65]),as.vector(gtemp[,66]),as.vector(gtemp[,67]),as.vector(gtemp[,68]),as.vector(gtemp[,69]))

gtemp2<-gtemp

#insert group names in place of "1" binary membership indicator
for (i in 2:(ncol(gind)-5))
{
	for (j in 1:nrow(gind))
	{
		if (gind[j,i]==1)
		{gtemp2[j,i]<-colnames(gtemp)[i]}
	}
}
for (i in (ncol(gind)-5):ncol(gind))
{
	for (j in 1:nrow(gind))
	{
		if (gind[j,i]==1)
		{gtemp2[j,i]<-as.character(gname[j,i])}
	}
}

gtemp3<-gtemp2



temp1<-data.frame((cbind(rep(as.character(gtemp3[,1]),63),(stack(gtemp3[,2:64])))))

colnames(temp1)<-c("ID","Group","GroupID")

temp2<-data.frame(cbind(rep(as.character(gtemp3[,1]),5),(c(as.character(gtemp3[,65]),as.character(gtemp3[,66]),as.character(gtemp3[,67]),as.character(gtemp3[,68]),as.character(gtemp3[,69]))),c(rep(colnames(gtemp3)[65],nrow(gtemp3)),rep(colnames(gtemp3)[66],nrow(gtemp3)),rep(colnames(gtemp3)[67],nrow(gtemp3)),rep(colnames(gtemp3)[68],nrow(gtemp3)),rep(colnames(gtemp3)[69],nrow(gtemp3)))))
colnames(temp2)<-c("ID","Group","GroupID")

temp3<-rbind(temp1,temp2)
temp4<-(subset(temp3,temp3$Group!=0))


Group.Edges<-temp4


#######################################
#create reason for group membership data


gtemp<-data.frame(as.vector(Grea.dat))
colnames(gtemp)<-colnames(Gmem.dat)
gtemp1<-lapply(gtemp,as.vector)
gtemp2<-cbind(rep(as.character(gtemp[,1]),68),stack(gtemp1[2:69]))
colnames(gtemp2)<-c("ID","Reason","GroupID")
gtemp2$Reason<-ifelse(gtemp2$Reason=="",0,gtemp2$Reason)
gtemp3<-na.omit(gtemp2)

Group.Reason<-gtemp3


#########################
#create participation data

#create binary vector or groupmembership
grpnames<-colnames(Gmem.dat)
tempgrp<-data.frame(matrix(rep(0,(ncol(Gmem.dat)*nrow(Gmem.dat))),ncol=ncol(Gmem.dat)))
for (i in 2:ncol(tempgrp))
{for (j in 1:nrow(tempgrp)){tempgrp[j,i]<-ifelse(Gmem.dat[j,i]==""|Gmem.dat[j,i]==0,0,1)}}
colnames(tempgrp)<-colnames(Gmem.dat)

tempgrp[,1]<-(Gmem.dat[,1])

#tempgrp<-data.frame((merge(Org.Reps.dat,tempgrp)))

#callvecs<-matrix(rep(0,7*68),ncol=7)
#for (i in 2:length(colnames(tempgrp)))
#{
#	callvecs[i,]<-grep(colnames(tempgrp)[i],colnames(Gpar.dat))	
#}
#rownames(callvecs)<-colnames(tempgrp)
#fill<-data.frame(matrix(rep(0,(7*(nrow(tempgrp)))),ncol=7));fill


#fill1<-matrix(rep(0,(sum(tempgrp[,c(-1,-2,-3)]*7))),ncol=7);fill1
fill1<-data.frame(matrix(rep(0,(sum(tempgrp[,-1]*7))),ncol=7))

#temp<-rep(0,nrow(fill1));temp
#fill1<-data.frame(cbind(fill1,temp));fill1
for (i in 1:nrow(tempgrp))
{
	for (j in 2:ncol(tempgrp))
	{
		if (tempgrp[i,j]==1)
		{
			temp<-c(grep(colnames(tempgrp)[j],colnames(Gpar.dat)))
				for (k in 1:length(temp))
			{
					fill1[(sum(tempgrp[(1:i),(2:ncol(tempgrp))])-ifelse(j==ncol(tempgrp),0,sum(tempgrp[i,(j+1):ncol(tempgrp)])))
					,k]<-Gpar.dat[i,temp[k]]
			}
					fill1[(sum(tempgrp[(1:i),(2:ncol(tempgrp))])-ifelse(j==ncol(tempgrp),0,sum(tempgrp[i,(j+1):ncol(tempgrp)])))
					,8]<-as.character(Gpar.dat[i,1])
					fill1[(sum(tempgrp[(1:i),(2:ncol(tempgrp))])-ifelse(j==ncol(tempgrp),0,sum(tempgrp[i,(j+1):ncol(tempgrp)])))
					,9]<-as.character(colnames(tempgrp)[j])
					
					fill1[(sum(tempgrp[(1:i),(2:ncol(tempgrp))])-ifelse(j==ncol(tempgrp),0,sum(tempgrp[i,(j+1):ncol(tempgrp)])))
					,10]<-as.character(Org.Reps.dat$ORG[i])
					fill1[(sum(tempgrp[(1:i),(2:ncol(tempgrp))])-ifelse(j==ncol(tempgrp),0,sum(tempgrp[i,(j+1):ncol(tempgrp)])))
					,11]<-as.character(Org.Reps.dat$Detail[i])
			}
			}
			}
			
			
colnames(fill1)<-c("email","meet","event","proj","read","write","other","ID","GroupID","ORG","Detail")	
Group.Membership.Dat<-fill1


write.csv(Group.Membership.Dat,file="Group.Membership.Dat.csv")
