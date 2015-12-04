##########################################

rm(list=ls())
#turn by-response group overlap into organizational group overlap
part.byresp.mat<-read.csv("//Users/TScott/Google Drive/PSP_Project/PS_Data/Shared.Groups.Weighted.csv")
part.byresp.mat.psp<-read.csv("//Users/TScott/Google Drive/PSP_Project/PS_Data/Shared.Groups.Weighted.Psp.csv")
part.byresp.mat.npsp<-read.csv("//Users/TScott/Google Drive/PSP_Project/PS_Data/Shared.Groups.Weighted.Npsp.csv")


edge.dat<-read.csv('//Users/TScott/Google Drive/PSP_Project/PS_Data/Edgelist.Dat.Good.csv',row.names=1)
resp.dat<-read.csv('//Users/TScott/Google Drive/PSP_Project/PS_Data/Response.Contact.Dat.csv',row.names=1)
group.dat.byedge<-read.csv("//Users/TScott/Google Drive/PSP_Project/PS_Data/Group.Membership.Dat.csv",row.names=1,header=T)
group.dat.byedge<-read.csv("//Users/TScott/Google Drive/PSP_Project/PS_Data/Group.Membership.Dat.csv",row.names=1,header=T)
group.dat.byresp<-read.csv("//Users/TScott/Google Drive/PSP_Project/PS_Data/Group.Member.Dat.csv",row.names=1,header=T)
resp.bin.mat<-read.csv("//Users/TScott/Google Drive/PSP_Project/PS_Data/Shared.Groups.csv")
resp.bin.matnpsp<-read.csv("//Users/TScott/Google Drive/PSP_Project/PS_Data/Shared.Groups.Npsp.csv")
resp.bin.mat.psp<-read.csv("//Users/TScott/Google Drive/PSP_Project/PS_Data/Shared.Groups.Psp.csv")


resp.dat.full<-resp.dat
part.byresp.mat<-read.csv("//Users/TScott/Google Drive/PSP_Project/PS_Data/Shared.Groups.Weighted.csv")
part.byresp.mat<-part.byresp.mat[,-1]
part.byresp.mat.psp<-read.csv("//Users/TScott/Google Drive/PSP_Project/PS_Data/Shared.Groups.Weighted.Psp.csv")
part.byresp.mat.psp<-part.byresp.mat.psp[,-1]
part.byresp.mat.npsp<-read.csv("//Users/TScott/Google Drive/PSP_Project/PS_Data/Shared.Groups.Weighted.Npsp.csv")
part.byresp.mat.npsp<-part.byresp.mat.npsp[,-1]



try<-part.byresp.mat
colnames(try)<-resp.dat.full$ORG
try.psp<-part.byresp.mat.psp
colnames(try.psp)<-resp.dat.full$ORG
try.npsp<-part.byresp.mat.npsp
colnames(try.npsp)<-resp.dat.full$ORG
dp.list<-resp.dat.full$ORG
uq.list<-unique(resp.dat.full$ORG)

empty.mat<-matrix(0,ncol=ncol(try),nrow=1)
empty.df<-as.data.frame(empty.mat)
empty.mat2<-matrix(0,nrow=ncol(try),ncol=1)
empty.df2<-as.data.frame(empty.mat2)

empty.mat.psp<-matrix(0,ncol=ncol(try.psp),nrow=1)
empty.df.psp<-as.data.frame(empty.mat.psp)
empty.mat2.psp<-matrix(0,nrow=ncol(try.psp),ncol=1)
empty.df2.psp<-as.data.frame(empty.mat2.psp)
empty.mat.npsp<-matrix(0,ncol=ncol(try.npsp),nrow=1)
empty.df.npsp<-as.data.frame(empty.mat.npsp)
empty.mat2.npsp<-matrix(0,nrow=ncol(try.npsp),ncol=1)
empty.df2.npsp<-as.data.frame(empty.mat2.npsp)

for (i in 1:length(uq.list))
{
	dup.vec<-which(dp.list==uq.list[i])	
	empty.df[i,]<-colSums(part.byresp.mat[dup.vec,])
	rownames(empty.df)[i]<-as.character(uq.list[i])
}
for (i in 1:length(uq.list))
{
	dup.vec<-which(dp.list==uq.list[i])	
	empty.df.psp[i,]<-colSums(part.byresp.mat.psp[dup.vec,])
	rownames(empty.df.psp)[i]<-as.character(uq.list[i])
}
for (i in 1:length(uq.list))
{
	dup.vec<-which(dp.list==uq.list[i])	
	empty.df.npsp[i,]<-colSums(part.byresp.mat.npsp[dup.vec,])
	rownames(empty.df.npsp)[i]<-as.character(uq.list[i])
}

for (j in 1:ncol(empty.df))
{
colnames(empty.df)[j]<-colnames(part.byresp.mat)[j]
}
for (j in 1:ncol(empty.df.psp))
{
colnames(empty.df.psp)[j]<-colnames(part.byresp.mat.psp)[j]
}
for (j in 1:ncol(empty.df.npsp))
{
colnames(empty.df.npsp)[j]<-colnames(part.byresp.mat.npsp)[j]
}


write.csv(empty.df,file="//Users/TScott/Google Drive/PSP_Project/PS_Data/Intermediate.csv")
write.csv(empty.df.psp,file="//Users/TScott/Google Drive/PSP_Project/PS_Data/Intermediate.Psp.csv")
write.csv(empty.df.npsp,file="//Users/TScott/Google Drive/PSP_Project/PS_Data/Intermediate.Npsp.csv")

###stopped fixing here
empty.df2<-empty.df[,1:nrow(empty.df)]

for (j in 1:length(uq.list))
{
	dup.vec2<-which(dp.list==uq.list[j])
	if (length(dup.vec2)>1)
	{
	empty.df2[,j]<-rowSums(empty.df[,dup.vec2])
	}
	else {empty.df2[,j]<-empty.df[,dup.vec2]}
	colnames(empty.df2)[j]<-as.character(uq.list[j])
}

empty.df2.psp<-empty.df.psp[,1:nrow(empty.df.psp)]

for (j in 1:length(uq.list))
{
	dup.vec2<-which(dp.list==uq.list[j])
	if (length(dup.vec2)>1)
	{
	empty.df2.psp[,j]<-rowSums(empty.df.psp[,dup.vec2])
	}
	else {empty.df2.psp[,j]<-empty.df.psp[,dup.vec2]}
	colnames(empty.df2.psp)[j]<-as.character(uq.list[j])
}

empty.df2.npsp<-empty.df.npsp[,1:nrow(empty.df.npsp)]

for (j in 1:length(uq.list))
{
	dup.vec2<-which(dp.list==uq.list[j])
	if (length(dup.vec2)>1)
	{
	empty.df2.npsp[,j]<-rowSums(empty.df.npsp[,dup.vec2])
	}
	else {empty.df2.npsp[,j]<-empty.df.npsp[,dup.vec2]}
	colnames(empty.df2.npsp)[j]<-as.character(uq.list[j])
}



#write.csv(empty.df2,file="//Users/TScott/Google Drive/PSP_Project/PS_Data/Test.csv")
write.csv(empty.df2,file="//Users/TScott/Google Drive/PSP_Project/PS_Data/Group.Overlap.Matrix.csv")
write.csv(empty.df2.psp,file="//Users/TScott/Google Drive/PSP_Project/PS_Data/Group.Overlap.Matrix.PSP.csv")
write.csv(empty.df2.npsp,file="//Users/TScott/Google Drive/PSP_Project/PS_Data/Group.Overlap.Matrix.NPSP.csv")
############################################