


rm(list=ls())
###################################
#begin script 6

edge.dat<-read.csv('//Users/TScott/Google Drive/elwha/Edgelist.Dat.Good.csv',row.names=1)
resp.dat<-read.csv('//Users/TScott/Google Drive/elwha/Response.Contact.Dat.csv',row.names=1)

#group.dat.byedge<-read.csv("//Users/TScott/Google Drive/PSP_Project/PS_Data/Group.Membership.Dat.csv",row.names=1,header=T)
group.dat.byedge<-read.csv("//Users/TScott/Google Drive/elwha/Group.All.Dat.csv",row.names=1,header=T)
group.dat.byresp<-read.csv("//Users/TScott/Google Drive/elwha/Group.Member.Dat.csv",row.names=1,header=T)



PSP.Created<-rep(0,nrow(group.dat.byedge))
PSP.Created[grep("G.ECO",group.dat.byedge$GroupID)]<-1
PSP.Created[grep("G.LIO",group.dat.byedge$GroupID)]<-1
PSP.Created[grep("G.LC",group.dat.byedge$GroupID)]<-1
PSP.Created[grep("G.ECB",group.dat.byedge$GroupID)]<-1
PSP.Created[grep("G.SCIENCE",group.dat.byedge$GroupID)]<-1
PSP.Created[grep("G.SOCSCI",group.dat.byedge$GroupID)]<-1
group.dat.byedge$PSP.Created<-PSP.Created

group.dat.byedge.psp<-subset(group.dat.byedge,group.dat.byedge$PSP.Created==1)
group.dat.byedge.npsp<-subset(group.dat.byedge,group.dat.byedge$PSP.Created==0)

group.dat.byresp.psp<-group.dat.byresp[,c(1,c(grep("G.ECO",colnames(group.dat.byresp)),
grep("G.LIO",colnames(group.dat.byresp)),
grep("G.LC",colnames(group.dat.byresp)),
grep("G.ECB",colnames(group.dat.byresp)),
grep("G.SCIENCE",colnames(group.dat.byresp)),
grep("G.SOCSCI",colnames(group.dat.byresp))),65:ncol(group.dat.byresp))]

group.dat.byresp.npsp<-group.dat.byresp[,-c(grep("G.ECO",colnames(group.dat.byresp)),
grep("G.LIO",colnames(group.dat.byresp)),
grep("G.LC",colnames(group.dat.byresp)),
grep("G.ECB",colnames(group.dat.byresp)),
grep("G.SCIENCE",colnames(group.dat.byresp)),
grep("G.SOCSCI",colnames(group.dat.byresp)))]


temp<-merge(group.dat.byresp,resp.dat)
bigdat<-temp[with(temp,order(ORG,ID)),]
temp<-merge(group.dat.byresp.psp,resp.dat)
bigdat.psp<-temp[with(temp,order(ORG,ID)),]
temp<-merge(group.dat.byresp.npsp,resp.dat)
bigdat.npsp<-temp[with(temp,order(ORG,ID)),]
##################

#do similar thing, but weight for participation
#turn group.dat into sociomatrix, then apply to actual sociomatrix

library(Matrix)

group.byresp.full<-bigdat
group.byresp.full.psp<-bigdat.psp
group.byresp.full.npsp<-bigdat.npsp
#turned off so doesn't reset
resp.bin.mat<-matrix(data=0,nrow=nrow(group.byresp.full),ncol=nrow(group.byresp.full))
colnames(resp.bin.mat)<-group.byresp.full$ORG
rownames(resp.bin.mat)<-group.byresp.full$ORG
resp.bin.mat.psp<-matrix(data=0,nrow=nrow(group.byresp.full.psp),ncol=nrow(group.byresp.full.psp))
colnames(resp.bin.mat.psp)<-group.byresp.full$ORG
rownames(resp.bin.mat.psp)<-group.byresp.full$ORG
resp.bin.mat.npsp<-matrix(data=0,nrow=nrow(group.byresp.full.npsp),ncol=nrow(group.byresp.full.npsp))
colnames(resp.bin.mat.npsp)<-group.byresp.full$ORG
rownames(resp.bin.mat.npsp)<-group.byresp.full$ORG


pull.partic<-c(which(colnames(group.dat.byedge)=="email"):which(colnames(group.dat.byedge)=="other"))
group.dat.byedge$part.sum<-rep(0,nrow(group.dat.byedge))
group.dat.byedge.psp$part.sum<-rep(0,nrow(group.dat.byedge.psp))
group.dat.byedge.npsp$part.sum<-rep(0,nrow(group.dat.byedge.npsp))

for (i in 1:nrow(group.dat.byedge))
{
	group.dat.byedge$part.sum[i]<-sum(group.dat.byedge[i,pull.partic])
}

for (i in 1:nrow(group.dat.byedge.psp))
{
	group.dat.byedge.psp$part.sum[i]<-sum(group.dat.byedge.psp[i,pull.partic])
}
for (i in 1:nrow(group.dat.byedge.npsp))
{
	group.dat.byedge.npsp$part.sum[i]<-sum(group.dat.byedge.npsp[i,pull.partic])
}

last.col<-which(colnames(group.byresp.full)=="G.OTHER.1")-1
last.col.psp<-which(colnames(group.byresp.full.psp)=="G.OTHER.1")-1
last.col.npsp<-which(colnames(group.byresp.full.npsp)=="G.OTHER.1")-1


overlap.score <- rep(0,400*400)
ex.grid<-expand.grid(1:400,1:400)
for (i in 1:(nrow(ex.grid)*.5))
{
	overlap.score[i]<-sum(group.byresp.full[ex.grid[i,1],2:last.col]*
	group.byresp.full[ex.grid[i,2],2:last.col])
	print(i)
}

ex.grid[,3]<-c(overlap.score[1:80000],rep(0,80000))
for (i in 1:(nrow(ex.grid)*.5))
{
	resp.bin.mat[ex.grid[i,1],ex.grid[i,2]]<-ex.grid[i,3]
	resp.bin.mat[ex.grid[i,2],ex.grid[i,1]]<-ex.grid[i,3]
	print(i)
}

#non-psp groups
overlap.score.npsp <- rep(0,400*400)
ex.grid.npsp<-expand.grid(1:400,1:400)
for (i in 1:(nrow(ex.grid.npsp)*.5))
{
	overlap.score.npsp[i]<-sum(group.byresp.full.npsp[ex.grid.npsp[i,1],2:last.col.npsp]*
	group.byresp.full.npsp[ex.grid.npsp[i,2],2:last.col.npsp])
}

ex.grid.npsp[,3]<-c(overlap.score.npsp[1:80000],rep(0,80000))
for (i in 1:(nrow(ex.grid.npsp)*.5))
{
	resp.bin.mat.npsp[ex.grid.npsp[i,1],ex.grid.npsp[i,2]]<-ex.grid.npsp[i,3]
	resp.bin.mat.npsp[ex.grid.npsp[i,2],ex.grid.npsp[i,1]]<-ex.grid.npsp[i,3]
}

#psp groups
overlap.score.psp <- rep(0,400*400)
ex.grid.psp<-expand.grid(1:400,1:400)
for (i in 1:(nrow(ex.grid.npsp)*.5))
{
	overlap.score.psp[i]<-sum(group.byresp.full.psp[ex.grid.psp[i,1],2:last.col.psp]*
	group.byresp.full.psp[ex.grid.psp[i,2],2:last.col.psp])
}

ex.grid.psp[,3]<-c(overlap.score.psp[1:80000],rep(0,80000))
for (i in 1:(nrow(ex.grid.psp)*.5))
{
	resp.bin.mat.psp[ex.grid.psp[i,1],ex.grid.psp[i,2]]<-ex.grid.psp[i,3]
	resp.bin.mat.psp[ex.grid.psp[i,2],ex.grid.psp[i,1]]<-ex.grid.psp[i,3]
}



write.csv(resp.bin.mat,file="//Users/TScott/Google Drive/elwha/Shared.Groups.csv")
write.csv(resp.bin.mat.psp,file="//Users/TScott/Google Drive/elwha/Shared.Groups.Psp.csv")
write.csv(resp.bin.mat.npsp,file="//Users/TScott/Google Drive/elwha/Shared.Groups.Npsp.csv")

#weighted indicator for each organization in sociomatrix




resp.weight.mat<-matrix(data=0,nrow=nrow(group.byresp.full),ncol=nrow(group.byresp.full))
colnames(resp.weight.mat)<-group.byresp.full$ORG
rownames(resp.weight.mat)<-group.byresp.full$ORG
grp.name.ref<-colnames(group.byresp.full)[2:(which(colnames(group.byresp.full)=="G.OTHER.1")-1)]


resp.weight.mat.psp<-matrix(data=0,nrow=nrow(group.byresp.full.psp),ncol=nrow(group.byresp.full.psp))
colnames(resp.weight.mat.psp)<-group.byresp.full.psp$ORG
rownames(resp.weight.mat.psp)<-group.byresp.full.psp$ORG
grp.name.ref.psp<-colnames(group.byresp.full.psp)[2:(which(colnames(group.byresp.full.psp)=="G.OTHER.1")-1)]

resp.weight.mat.npsp<-matrix(data=0,nrow=nrow(group.byresp.full.npsp),ncol=nrow(group.byresp.full.npsp))
colnames(resp.weight.mat.npsp)<-group.byresp.full.npsp$ORG
rownames(resp.weight.mat.npsp)<-group.byresp.full.npsp$ORG
grp.name.ref.npsp<-colnames(group.byresp.full.npsp)[2:(which(colnames(group.byresp.full.npsp)=="G.OTHER.1")-1)]





group.byresp.full.wt <- group.byresp.full
for (i in 1:100)
{
	for (j in (1:last.col))
	{
		if (group.byresp.full[i,j]==1)
		{
group.byresp.full.wt[i,j]<-group.dat.byedge$part.sum[as.character(group.dat.byedge$ID)== (as.character(group.byresp.full$ID[i]))&group.dat.byedge$GroupID==colnames(group.byresp.full)[j]]
}}}

group.byresp.full.npsp.wt <- group.byresp.full.npsp
for (i in 1:100)
{
	for (j in (1:last.col.npsp))
	{
		if (group.byresp.full.npsp[i,j]==1)
		{
group.byresp.full.npsp.wt[i,j]<-group.dat.byedge$part.sum[as.character(group.dat.byedge$ID)== (as.character(group.byresp.full.npsp$ID[i]))&group.dat.byedge$GroupID==colnames(group.byresp.full.npsp)[j]]
}}}

group.byresp.full.psp.wt <- group.byresp.full.psp
for (i in 1:100)
{
	for (j in (1:last.col.psp))
	{
		if (group.byresp.full.psp[i,j]==1)
		{
group.byresp.full.psp.wt[i,j]<-group.dat.byedge$part.sum[as.character(group.dat.byedge$ID)== (as.character(group.byresp.full.psp$ID[i]))&group.dat.byedge$GroupID==colnames(group.byresp.full.psp)[j]]
}}}





part.overlap.score <- rep(0,nrow(group.byresp.full)*nrow(group.byresp.full))
part.ex.grid<-expand.grid(1:length((group.byresp.full$ORG)),1:length((group.byresp.full$ORG)))


for (i in 1:(nrow(part.ex.grid)*.5))
{
	part.overlap.score[i]<-sum(apply((rbind(group.byresp.full.wt[ex.grid[i,1],2:last.col],
	group.byresp.full.wt[ex.grid[i,2],2:last.col])),2,min))
	print(i)
}
part.ex.grid[,3]<-c(part.overlap.score[1:80000],rep(0,80000))

for (i in 1:(nrow(part.ex.grid)*.5))
{
	resp.weight.mat[part.ex.grid[i,1],part.ex.grid[i,2]]<-part.ex.grid[i,3]
	resp.weight.mat[part.ex.grid[i,2],part.ex.grid[i,1]]<-part.ex.grid[i,3]
	print(i)
}


org.overlap.mat<-matrix(rep(0,length(unique(colnames(resp.weight.mat)))*length(unique(colnames(resp.weight.mat)))),ncol=(length(unique(colnames(resp.weight.mat)))))
org.overlap.score <- length(unique(colnames(resp.weight.mat)))*length(unique(colnames(resp.weight.mat)))
colnames(org.overlap.mat)<-sort(unique(colnames(resp.weight.mat)))
rownames(org.overlap.mat)<-sort(unique(colnames(resp.weight.mat)))
org.ex.grid <- expand.grid(1:length(unique(colnames(resp.weight.mat))),1:length(unique(colnames(resp.weight.mat))))
head(org.ex.grid)
for (i in 1:nrow(org.ex.grid))
{
	org.overlap.score<-(sum(apply(rbind(apply(group.byresp.full.wt[group.byresp.full.wt$ORG == colnames(org.overlap.mat)[org.ex.grid[i,1]],2:last.col],2,max),
	apply(group.byresp.full.wt[group.byresp.full.wt$ORG == colnames(org.overlap.mat)[org.ex.grid[i,2]],2:last.col],2,max)),2,min)))
print(i)
}
org.ex.grid[,3]<-org.overlap.score

for (i in 1:nrow(org.ex.grid))
{
	org.overlap.mat[org.ex.grid[i,1],org.ex.grid[i,2]]<-org.ex.grid[i,3]
	org.overlap.mat[org.ex.grid[i,2],org.ex.grid[i,1]]<-org.ex.grid[i,3]
	print(i)
}

write.csv(org.overlap.mat,file="//Users/TScott/Google Drive/elwha/Shared.Groups.Weighted.csv")




part.overlap.score.npsp <- rep(0,nrow(group.byresp.full.npsp)*nrow(group.byresp.full.npsp))
part.ex.grid.npsp<-expand.grid(1:length((group.byresp.full.npsp$ORG)),1:length((group.byresp.full.npsp$ORG)))

for (i in 1:(nrow(part.ex.grid.npsp)*.5))
{
	part.overlap.score.npsp[i]<-sum(apply((rbind(group.byresp.full.npsp.wt[ex.grid.npsp[i,1],2:last.col.npsp],
	group.byresp.full.npsp.wt[ex.grid.npsp[i,2],2:last.col.npsp])),2,min))
	print(i)
}

part.ex.grid.npsp[,3]<-c(part.overlap.score.npsp[1:80000],rep(0,80000))
for (i in 1:(nrow(part.ex.grid.npsp)*.5))
{
	resp.weight.mat.npsp[part.ex.grid.npsp[i,1],part.ex.grid.npsp[i,2]]<-part.ex.grid.npsp[i,3]
	resp.weight.mat.npsp[part.ex.grid.npsp[i,2],part.ex.grid.npsp[i,1]]<-part.ex.grid.npsp[i,3]
	print(i)
}


org.overlap.mat.npsp<-matrix(rep(0,length(unique(colnames(resp.weight.mat.npsp)))*length(unique(colnames(resp.weight.mat.npsp)))),ncol=(length(unique(colnames(resp.weight.mat.npsp)))))
org.overlap.score.npsp <- length(unique(colnames(resp.weight.mat.npsp)))*length(unique(colnames(resp.weight.mat.npsp)))
colnames(org.overlap.mat.npsp)<-sort(unique(colnames(resp.weight.mat.npsp)))
rownames(org.overlap.mat.npsp)<-sort(unique(colnames(resp.weight.mat.npsp)))
org.ex.grid.npsp <- expand.grid(1:length(unique(colnames(resp.weight.mat.npsp))),1:length(unique(colnames(resp.weight.mat.npsp))))

for (i in 1:nrow(org.ex.grid.npsp))
{
	org.overlap.score.npsp<-(sum(apply(rbind(apply(group.byresp.full.npsp.wt[group.byresp.full.npsp.wt$ORG == colnames(org.overlap.mat.npsp)[org.ex.grid.npsp[i,1]],2:last.col.npsp],2,max),
	apply(group.byresp.full.npsp.wt[group.byresp.full.npsp.wt$ORG == colnames(org.overlap.mat.npsp)[org.ex.grid.npsp[i,2]],2:last.col.npsp],2,max)),2,min)))
print(i)
}
org.ex.grid.npsp[,3]<-org.overlap.score.npsp

for (i in 1:nrow(org.ex.grid.npsp))
{
	org.overlap.mat.npsp[org.ex.grid.npsp[i,1],org.ex.grid.npsp[i,2]]<-org.ex.grid.npsp[i,3]
	org.overlap.mat.npsp[org.ex.grid.npsp[i,2],org.ex.grid.npsp[i,1]]<-org.ex.grid.npsp[i,3]
	print(i)
}

write.csv(org.overlap.mat.npsp,file="//Users/TScott/Google Drive/elwha/Shared.Groups.Weighted.NPSP.csv")




part.overlap.score.psp <- rep(0,nrow(group.byresp.full.psp)*nrow(group.byresp.full.psp))
part.ex.grid.psp<-expand.grid(1:length((group.byresp.full.psp$ORG)),1:length((group.byresp.full.psp$ORG)))


for (i in 1:(nrow(part.ex.grid.psp)*.5))
{
	part.overlap.score.psp[i]<-sum(apply((rbind(group.byresp.full.psp.wt[ex.grid.psp[i,1],2:last.col.psp],
	group.byresp.full.psp.wt[ex.grid.psp[i,2],2:last.col.psp])),2,min))
	print(i)
}

part.ex.grid.psp[,3]<-c(part.overlap.score.psp[1:80000],rep(0,80000))
for (i in 1:(nrow(part.ex.grid.psp)*.5))
{
	resp.weight.mat.psp[part.ex.grid.psp[i,1],part.ex.grid.psp[i,2]]<-part.ex.grid.psp[i,3]
	resp.weight.mat.psp[part.ex.grid.psp[i,2],part.ex.grid.psp[i,1]]<-part.ex.grid.psp[i,3]
	print(i)
}



org.overlap.mat.psp<-matrix(rep(0,length(unique(colnames(resp.weight.mat.psp)))*length(unique(colnames(resp.weight.mat.psp)))),ncol=(length(unique(colnames(resp.weight.mat.psp)))))
org.overlap.score.psp <- length(unique(colnames(resp.weight.mat.psp)))*length(unique(colnames(resp.weight.mat.psp)))
colnames(org.overlap.mat.psp)<-sort(unique(colnames(resp.weight.mat.psp)))
rownames(org.overlap.mat.psp)<-sort(unique(colnames(resp.weight.mat.psp)))
org.ex.grid <- expand.grid(1:length(unique(colnames(resp.weight.mat.psp))),1:length(unique(colnames(resp.weight.mat.psp))))

for (i in 1:nrow(org.ex.grid.psp))
{
	org.overlap.score.psp<-(sum(apply(rbind(apply(group.byresp.full.psp.wt[group.byresp.full.psp.wt$ORG == colnames(org.overlap.mat.psp)[org.ex.grid.psp[i,1]],2:last.col.psp],2,max),
	apply(group.byresp.full.psp.wt[group.byresp.full.psp.wt$ORG == colnames(org.overlap.mat.psp)[org.ex.grid.psp[i,2]],2:last.col.psp],2,max)),2,min)))
print(i)
}
org.ex.grid.psp[,3]<-org.overlap.score.psp

for (i in 1:nrow(org.ex.grid.psp))
{
	org.overlap.mat.psp[org.ex.grid.psp[i,1],org.ex.grid.psp[i,2]]<-org.ex.grid.psp[i,3]
	org.overlap.mat.psp[org.ex.grid.psp[i,2],org.ex.grid.psp[i,1]]<-org.ex.grid.psp[i,3]
	print(i)
}

write.csv(org.overlap.mat.psp,file="//Users/TScott/Google Drive/elwha/Shared.Groups.Weighted.PSP.csv")







#for (i in 1:nrow(resp.weight.mat))
for (i in 1:nrow(resp.weight.mat))
{
#print("i");print(i);
(vec.i<-(as.numeric(group.byresp.full[i,2:(which(colnames(group.byresp.full)=="G.OTHER.1")-1)])))
#print("vec.i before"); print(vec.i)
(for (k in 1:length(vec.i))
			{
				#print("k") ; print(k)
				if (vec.i[k]>0)
				{id.pull1<-group.byresp.full$ID[i]
					#print("id.pull1");print(id.pull1)
				groupn.pull1<-grp.name.ref[k]
				#print("groupn.pull1");print(groupn.pull1)
				specific.group1<-subset(group.dat.byedge,group.dat.byedge$ID==as.character(id.pull1))	
				#print("specific.group1");print(specific.group1)				
				specific.group2<-subset(specific.group1,specific.group1$GroupID==as.character(groupn.pull1))
				#print("specific.group2");print(specific.group2)
				if (nrow(specific.group2)!=0)
					{part.sum<-specific.group2$part.sum[1]
					part.rat<-part.sum/7 
					#print("part.rat");print(part.rat)
					vec.i[k]<-part.rat 
					#print("vec.i[k]");print(vec.i[k])
}}})
#print("vec.i after"); print(vec.i)	
for (j in 1:nrow(resp.weight.mat))
{
	#print("j");print(j);
(vec.j<-(as.numeric(group.byresp.full[j,2:(which(colnames(group.byresp.full)=="G.OTHER.1")-1)])))
#print("vec.j before");print(vec.j)
(for (l in 1:length(vec.j))
	{if (vec.j[l]>0)
			{id.pull2<-group.byresp.full$ID[j]
			#print("id.pull2");print(id.pull2)
			groupn.pull2<-grp.name.ref[l]
			#print("groupn.pull2");print(groupn.pull2)
			specific.group3<-subset(group.dat.byedge,group.dat.byedge$ID==as.character(id.pull2))
			#print("specific.group3");print(specific.group3)							
			specific.group4<-subset(specific.group3,specific.group3$GroupID==as.character(groupn.pull2))
			#print("specific.group4");print(specific.group4)
			if (nrow(specific.group4)!=0)
			{part.sum<-specific.group4$part.sum[1]
			part.rat<-part.sum/7 
			#;print(part.rat);print("part.rat")
			vec.j[l]<-part.rat 
			#;print(vec.j[l]);print("vec.j[l]")
			}}})
#print("vec.j after");print(vec.j)	
			
	
	min.combs<-pmin(vec.i,vec.j)
	#print("min.combs");print(min.combs)
	min.vec<-min.combs  
	#print("min.vec");print(min.vec)
	resp.weight.mat[i,j]<-sum(min.vec)
	#print("sum(min.vec)");print(sum(min.vec))	
	#print(resp.weight.mat[i,j]);print("resp.weight.mat[i,j]")	
}
print("iteration i done");print(i);print("#####")
}

write.csv(resp.weight.mat,file="//Users/TScott/Google Drive/elwha/Shared.Groups.Weighted.csv")

for (i in 1:nrow(resp.weight.mat.psp))
{
#print("i");print(i);
(vec.i<-(as.numeric(group.byresp.full.psp[i,2:(which(colnames(group.byresp.full.psp)=="G.OTHER.1")-1)])))
#print("vec.i before"); print(vec.i)
(for (k in 1:length(vec.i))
			{
				#print("k") ; print(k)
				if (vec.i[k]>0)
				{id.pull1<-group.byresp.full.psp$ID[i]
					#print("id.pull1");print(id.pull1)
				groupn.pull1<-grp.name.ref.psp[k]
				#print("groupn.pull1");print(groupn.pull1)
				specific.group1<-subset(group.dat.byedge.psp,group.dat.byedge.psp$ID==as.character(id.pull1))	
				#print("specific.group1");print(specific.group1)				
				specific.group2<-subset(specific.group1,specific.group1$GroupID==as.character(groupn.pull1))
				#print("specific.group2");print(specific.group2)
				if (nrow(specific.group2)!=0)
					{part.sum<-specific.group2$part.sum[1]
					part.rat<-part.sum/7 
					#print("part.rat");print(part.rat)
					vec.i[k]<-part.rat 
					#print("vec.i[k]");print(vec.i[k])
}}})
#print("vec.i after"); print(vec.i)	
for (j in 1:nrow(resp.weight.mat.psp))
{
	#print("j");print(j);
(vec.j<-(as.numeric(group.byresp.full.psp[j,2:(which(colnames(group.byresp.full.psp)=="G.OTHER.1")-1)])))
#print("vec.j before");print(vec.j)
(for (l in 1:length(vec.j))
	{if (vec.j[l]>0)
			{id.pull2<-group.byresp.full.psp$ID[j]
			#print("id.pull2");print(id.pull2)
			groupn.pull2<-grp.name.ref.psp[l]
			#print("groupn.pull2");print(groupn.pull2)
			specific.group3<-subset(group.dat.byedge.psp,group.dat.byedge.psp$ID==as.character(id.pull2))
			#print("specific.group3");print(specific.group3)							
			specific.group4<-subset(specific.group3,specific.group3$GroupID==as.character(groupn.pull2))
			#print("specific.group4");print(specific.group4)
			if (nrow(specific.group4)!=0)
			{part.sum<-specific.group4$part.sum[1]
			part.rat<-part.sum/7 
			#;print(part.rat);print("part.rat")
			vec.j[l]<-part.rat 
			#;print(vec.j[l]);print("vec.j[l]")
			}}})
#print("vec.j after");print(vec.j)	
			
	
	min.combs<-pmin(vec.i,vec.j)
	#print("min.combs");print(min.combs)
	min.vec<-min.combs  
	#print("min.vec");print(min.vec)
	resp.weight.mat.psp[i,j]<-sum(min.vec)
	#print("sum(min.vec)");print(sum(min.vec))	
	#print(resp.weight.mat[i,j]);print("resp.weight.mat[i,j]")	
}
print("iteration i done");print(i);print("#####")
}

write.csv(resp.weight.mat.psp,file="//Users/TScott/Google Drive/elwha/Shared.Groups.Weighted.Psp.csv")


for (i in 1:nrow(resp.weight.mat.npsp))
{
#print("i");print(i);
(vec.i<-(as.numeric(group.byresp.full.npsp[i,2:(which(colnames(group.byresp.full.npsp)=="G.OTHER.1")-1)])))
#print("vec.i before"); print(vec.i)
(for (k in 1:length(vec.i))
			{
				#print("k") ; print(k)
				if (vec.i[k]>0)
				{id.pull1<-group.byresp.full.npsp$ID[i]
					#print("id.pull1");print(id.pull1)
				groupn.pull1<-grp.name.ref.npsp[k]
				#print("groupn.pull1");print(groupn.pull1)
				specific.group1<-subset(group.dat.byedge.npsp,group.dat.byedge.npsp$ID==as.character(id.pull1))	
				#print("specific.group1");print(specific.group1)				
				specific.group2<-subset(specific.group1,specific.group1$GroupID==as.character(groupn.pull1))
				#print("specific.group2");print(specific.group2)
				if (nrow(specific.group2)!=0)
					{part.sum<-specific.group2$part.sum[1]
					part.rat<-part.sum/7 
					#print("part.rat");print(part.rat)
					vec.i[k]<-part.rat 
					#print("vec.i[k]");print(vec.i[k])
}}})
#print("vec.i after"); print(vec.i)	
for (j in 1:nrow(resp.weight.mat.npsp))
{
	#print("j");print(j);
(vec.j<-(as.numeric(group.byresp.full.npsp[j,2:(which(colnames(group.byresp.full.npsp)=="G.OTHER.1")-1)])))
#print("vec.j before");print(vec.j)
(for (l in 1:length(vec.j))
	{if (vec.j[l]>0)
			{id.pull2<-group.byresp.full.npsp$ID[j]
			#print("id.pull2");print(id.pull2)
			groupn.pull2<-grp.name.ref.npsp[l]
			#print("groupn.pull2");print(groupn.pull2)
			specific.group3<-subset(group.dat.byedge.npsp,group.dat.byedge.npsp$ID==as.character(id.pull2))
			#print("specific.group3");print(specific.group3)							
			specific.group4<-subset(specific.group3,specific.group3$GroupID==as.character(groupn.pull2))
			#print("specific.group4");print(specific.group4)
			if (nrow(specific.group4)!=0)
			{part.sum<-specific.group4$part.sum[1]
			part.rat<-part.sum/7 
			#;print(part.rat);print("part.rat")
			vec.j[l]<-part.rat 
			#;print(vec.j[l]);print("vec.j[l]")
			}}})
#print("vec.j after");print(vec.j)	
			
	
	min.combs<-pmin(vec.i,vec.j)
	#print("min.combs");print(min.combs)
	min.vec<-min.combs  
	#print("min.vec");print(min.vec)
	resp.weight.mat.npsp[i,j]<-sum(min.vec)
	#print("sum(min.vec)");print(sum(min.vec))	
	#print(resp.weight.mat[i,j]);print("resp.weight.mat[i,j]")	
}
print("iteration i done");print(i);print("#####")
}

write.csv(resp.weight.mat.npsp,file="//Users/TScott/Google Drive/elwha/Shared.Groups.Weighted.Npsp.csv")



