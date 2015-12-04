

rm(list=ls())
setwd("//Users/TScott/Google Drive/elwha")
edge.dat<-read.csv('Edgelist.Dat.Good.csv',row.names=1)
resp.dat<-read.csv('Response.Used.csv',row.names=1)

fina_up_group=read.csv(file="Group.Financial.Resources.Up.Dat.csv",row.names=1)
fina_down_group=read.csv(file="Group.Financial.Resources.Down.Dat.csv",row.names=1)
huma_up_group=read.csv(file="Group.Human.Resources.Up.Dat.csv",row.names=1)
huma_down_group=read.csv(file="Group.Human.Resources.Down.Dat.csv",row.names=1)
valu_up_group=read.csv(file="Group.Values.Up.Dat.csv",row.names=1)
valu_down_group=read.csv(file="Group.Values.Down.Dat.csv",row.names=1)
lang_up_group=read.csv(file="Group.Language.Up.Dat.csv",row.names=1)
lang_down_group=read.csv(file="Group.Language.Down.Dat.csv",row.names=1)
scie_up_group=read.csv(file="Group.Scientific.Up.Dat.csv",row.names=1)
scie_down_group=read.csv(file="Group.Scientific.Down.Dat.csv",row.names=1)
face_up_group=read.csv(file="Group.Face.Up.Dat.csv",row.names=1)
face_down_group=read.csv(file="Group.Face.Down.Dat.csv",row.names=1)

temp.init = face_up_group
colnames(temp.init)[1]<-"ID"
temp<-merge(temp.init,resp.dat)
bigdat<-temp[with(temp,order(ORG,ID)),]
group.temp.full<-bigdat
tmp.group.full<-group.temp.full
last.col <- grep("North.Central.Action",colnames(group.temp.full))


resp.temp.mat<-matrix(data=0,nrow=nrow(tmp.group.full),ncol=nrow(tmp.group.full))
colnames(resp.temp.mat)<-tmp.group.full$ORG
rownames(resp.temp.mat)<-tmp.group.full$ORG
grp.name.ref<-colnames(tmp.group.full)[2:last.col]
 
group.tmp.full.wt <- tmp.group.full

tpart.overlap.score <- rep(0,nrow(tmp.group.full)*nrow(tmp.group.full))
tpart.ex.grid<-expand.grid(1:length((tmp.group.full$ORG)),1:length((tmp.group.full$ORG)))


for (i in 1:(nrow(tpart.ex.grid)*.5))
{
	tpart.overlap.score[i]<-sum(apply((rbind(group.tmp.full.wt[tpart.ex.grid[i,1],2:last.col],
	group.tmp.full.wt[tpart.ex.grid[i,2],2:last.col])),2,min))
	print(i)
}
tpart.ex.grid[,3]<-c(tpart.overlap.score[1:80000],rep(0,80000))

for (i in 1:(nrow(tpart.ex.grid)*.5))
{
	resp.temp.mat[tpart.ex.grid[i,1],tpart.ex.grid[i,2]]<-tpart.ex.grid[i,3]
	resp.temp.mat[tpart.ex.grid[i,2],tpart.ex.grid[i,1]]<-tpart.ex.grid[i,3]
	print(i)
}


org.tmp.mat<-matrix(rep(0,length(unique(colnames(resp.temp.mat)))*length(unique(colnames(resp.temp.mat)))),ncol=(length(unique(colnames(resp.temp.mat)))))
org.temp.score <- rep(0,length(unique(colnames(resp.temp.mat)))*length(unique(colnames(resp.temp.mat))))
colnames(org.tmp.mat)<-sort(unique(colnames(resp.temp.mat)))
rownames(org.tmp.mat)<-sort(unique(colnames(resp.temp.mat)))
torg.ex.grid <- expand.grid(1:length(unique(colnames(resp.temp.mat))),1:length(unique(colnames(resp.temp.mat))))


for (i in 1:nrow(torg.ex.grid))
{
	org.temp.score[i]<-(sum(apply(rbind(apply(group.tmp.full.wt[group.tmp.full.wt$ORG == colnames(org.tmp.mat)[torg.ex.grid[i,1]],2:last.col],2,max),
	apply(group.tmp.full.wt[group.tmp.full.wt$ORG == colnames(org.tmp.mat)[torg.ex.grid[i,2]],2:last.col],2,max)),2,min)))
print(i)
}

torg.ex.grid[,3]<-org.temp.score


for (i in 1:nrow(torg.ex.grid))
{
	org.tmp.mat[torg.ex.grid[i,1],torg.ex.grid[i,2]]<-torg.ex.grid[i,3]
	org.tmp.mat[torg.ex.grid[i,2],torg.ex.grid[i,1]]<-torg.ex.grid[i,3]
	print(i)
}

org.tmp.mat[1:20,1:20]

write.csv(org.tmp.mat,file="Group.Face.Up.Matrix.csv")




temp.init = face_down_group
colnames(temp.init)[1]<-"ID"
temp<-merge(temp.init,resp.dat)
bigdat<-temp[with(temp,order(ORG,ID)),]
group.temp.full<-bigdat
tmp.group.full<-group.temp.full
last.col <- grep("North.Central.Action",colnames(group.temp.full))


resp.temp.mat<-matrix(data=0,nrow=nrow(tmp.group.full),ncol=nrow(tmp.group.full))
colnames(resp.temp.mat)<-tmp.group.full$ORG
rownames(resp.temp.mat)<-tmp.group.full$ORG
grp.name.ref<-colnames(tmp.group.full)[2:last.col]
 
group.tmp.full.wt <- tmp.group.full

tpart.overlap.score <- rep(0,nrow(tmp.group.full)*nrow(tmp.group.full))
tpart.ex.grid<-expand.grid(1:length((tmp.group.full$ORG)),1:length((tmp.group.full$ORG)))


for (i in 1:(nrow(tpart.ex.grid)*.5))
{
	tpart.overlap.score[i]<-sum(apply((rbind(group.tmp.full.wt[tpart.ex.grid[i,1],2:last.col],
	group.tmp.full.wt[tpart.ex.grid[i,2],2:last.col])),2,min))
	print(i)
}
tpart.ex.grid[,3]<-c(tpart.overlap.score[1:80000],rep(0,80000))

for (i in 1:(nrow(tpart.ex.grid)*.5))
{
	resp.temp.mat[tpart.ex.grid[i,1],tpart.ex.grid[i,2]]<-tpart.ex.grid[i,3]
	resp.temp.mat[tpart.ex.grid[i,2],tpart.ex.grid[i,1]]<-tpart.ex.grid[i,3]
	print(i)
}


org.tmp.mat<-matrix(rep(0,length(unique(colnames(resp.temp.mat)))*length(unique(colnames(resp.temp.mat)))),ncol=(length(unique(colnames(resp.temp.mat)))))
org.temp.score <- rep(0,length(unique(colnames(resp.temp.mat)))*length(unique(colnames(resp.temp.mat))))
colnames(org.tmp.mat)<-sort(unique(colnames(resp.temp.mat)))
rownames(org.tmp.mat)<-sort(unique(colnames(resp.temp.mat)))
torg.ex.grid <- expand.grid(1:length(unique(colnames(resp.temp.mat))),1:length(unique(colnames(resp.temp.mat))))


for (i in 1:nrow(torg.ex.grid))
{
	org.temp.score[i]<-(sum(apply(rbind(apply(group.tmp.full.wt[group.tmp.full.wt$ORG == colnames(org.tmp.mat)[torg.ex.grid[i,1]],2:last.col],2,max),
	apply(group.tmp.full.wt[group.tmp.full.wt$ORG == colnames(org.tmp.mat)[torg.ex.grid[i,2]],2:last.col],2,max)),2,min)))
print(i)
}

torg.ex.grid[,3]<-org.temp.score


for (i in 1:nrow(torg.ex.grid))
{
	org.tmp.mat[torg.ex.grid[i,1],torg.ex.grid[i,2]]<-torg.ex.grid[i,3]
	org.tmp.mat[torg.ex.grid[i,2],torg.ex.grid[i,1]]<-torg.ex.grid[i,3]
	print(i)
}

org.tmp.mat[1:20,1:20]

write.csv(org.tmp.mat,file="Group.Face.Down.Matrix.csv")




temp.init = fina_up_group
colnames(temp.init)[1]<-"ID"
temp<-merge(temp.init,resp.dat)
bigdat<-temp[with(temp,order(ORG,ID)),]
group.temp.full<-bigdat
tmp.group.full<-group.temp.full
last.col <- grep("North.Central.Action",colnames(group.temp.full))


resp.temp.mat<-matrix(data=0,nrow=nrow(tmp.group.full),ncol=nrow(tmp.group.full))
colnames(resp.temp.mat)<-tmp.group.full$ORG
rownames(resp.temp.mat)<-tmp.group.full$ORG
grp.name.ref<-colnames(tmp.group.full)[2:last.col]
 
group.tmp.full.wt <- tmp.group.full

tpart.overlap.score <- rep(0,nrow(tmp.group.full)*nrow(tmp.group.full))
tpart.ex.grid<-expand.grid(1:length((tmp.group.full$ORG)),1:length((tmp.group.full$ORG)))


for (i in 1:(nrow(tpart.ex.grid)*.5))
{
	tpart.overlap.score[i]<-sum(apply((rbind(group.tmp.full.wt[tpart.ex.grid[i,1],2:last.col],
	group.tmp.full.wt[tpart.ex.grid[i,2],2:last.col])),2,min))
	print(i)
}
tpart.ex.grid[,3]<-c(tpart.overlap.score[1:80000],rep(0,80000))

for (i in 1:(nrow(tpart.ex.grid)*.5))
{
	resp.temp.mat[tpart.ex.grid[i,1],tpart.ex.grid[i,2]]<-tpart.ex.grid[i,3]
	resp.temp.mat[tpart.ex.grid[i,2],tpart.ex.grid[i,1]]<-tpart.ex.grid[i,3]
	print(i)
}


org.tmp.mat<-matrix(rep(0,length(unique(colnames(resp.temp.mat)))*length(unique(colnames(resp.temp.mat)))),ncol=(length(unique(colnames(resp.temp.mat)))))
org.temp.score <- rep(0,length(unique(colnames(resp.temp.mat)))*length(unique(colnames(resp.temp.mat))))
colnames(org.tmp.mat)<-sort(unique(colnames(resp.temp.mat)))
rownames(org.tmp.mat)<-sort(unique(colnames(resp.temp.mat)))
torg.ex.grid <- expand.grid(1:length(unique(colnames(resp.temp.mat))),1:length(unique(colnames(resp.temp.mat))))


for (i in 1:nrow(torg.ex.grid))
{
	org.temp.score[i]<-(sum(apply(rbind(apply(group.tmp.full.wt[group.tmp.full.wt$ORG == colnames(org.tmp.mat)[torg.ex.grid[i,1]],2:last.col],2,max),
	apply(group.tmp.full.wt[group.tmp.full.wt$ORG == colnames(org.tmp.mat)[torg.ex.grid[i,2]],2:last.col],2,max)),2,min)))
print(i)
}

torg.ex.grid[,3]<-org.temp.score


for (i in 1:nrow(torg.ex.grid))
{
	org.tmp.mat[torg.ex.grid[i,1],torg.ex.grid[i,2]]<-torg.ex.grid[i,3]
	org.tmp.mat[torg.ex.grid[i,2],torg.ex.grid[i,1]]<-torg.ex.grid[i,3]
	print(i)
}

org.tmp.mat[1:20,1:20]

write.csv(org.tmp.mat,file="Group.Fina.Up.Matrix.csv")




temp.init = fina_down_group
colnames(temp.init)[1]<-"ID"
temp<-merge(temp.init,resp.dat)
bigdat<-temp[with(temp,order(ORG,ID)),]
group.temp.full<-bigdat
tmp.group.full<-group.temp.full
last.col <- grep("North.Central.Action",colnames(group.temp.full))


resp.temp.mat<-matrix(data=0,nrow=nrow(tmp.group.full),ncol=nrow(tmp.group.full))
colnames(resp.temp.mat)<-tmp.group.full$ORG
rownames(resp.temp.mat)<-tmp.group.full$ORG
grp.name.ref<-colnames(tmp.group.full)[2:last.col]
 
group.tmp.full.wt <- tmp.group.full

tpart.overlap.score <- rep(0,nrow(tmp.group.full)*nrow(tmp.group.full))
tpart.ex.grid<-expand.grid(1:length((tmp.group.full$ORG)),1:length((tmp.group.full$ORG)))


for (i in 1:(nrow(tpart.ex.grid)*.5))
{
	tpart.overlap.score[i]<-sum(apply((rbind(group.tmp.full.wt[tpart.ex.grid[i,1],2:last.col],
	group.tmp.full.wt[tpart.ex.grid[i,2],2:last.col])),2,min))
	print(i)
}
tpart.ex.grid[,3]<-c(tpart.overlap.score[1:80000],rep(0,80000))

for (i in 1:(nrow(tpart.ex.grid)*.5))
{
	resp.temp.mat[tpart.ex.grid[i,1],tpart.ex.grid[i,2]]<-tpart.ex.grid[i,3]
	resp.temp.mat[tpart.ex.grid[i,2],tpart.ex.grid[i,1]]<-tpart.ex.grid[i,3]
	print(i)
}


org.tmp.mat<-matrix(rep(0,length(unique(colnames(resp.temp.mat)))*length(unique(colnames(resp.temp.mat)))),ncol=(length(unique(colnames(resp.temp.mat)))))
org.temp.score <- rep(0,length(unique(colnames(resp.temp.mat)))*length(unique(colnames(resp.temp.mat))))
colnames(org.tmp.mat)<-sort(unique(colnames(resp.temp.mat)))
rownames(org.tmp.mat)<-sort(unique(colnames(resp.temp.mat)))
torg.ex.grid <- expand.grid(1:length(unique(colnames(resp.temp.mat))),1:length(unique(colnames(resp.temp.mat))))


for (i in 1:nrow(torg.ex.grid))
{
	org.temp.score[i]<-(sum(apply(rbind(apply(group.tmp.full.wt[group.tmp.full.wt$ORG == colnames(org.tmp.mat)[torg.ex.grid[i,1]],2:last.col],2,max),
	apply(group.tmp.full.wt[group.tmp.full.wt$ORG == colnames(org.tmp.mat)[torg.ex.grid[i,2]],2:last.col],2,max)),2,min)))
print(i)
}

torg.ex.grid[,3]<-org.temp.score


for (i in 1:nrow(torg.ex.grid))
{
	org.tmp.mat[torg.ex.grid[i,1],torg.ex.grid[i,2]]<-torg.ex.grid[i,3]
	org.tmp.mat[torg.ex.grid[i,2],torg.ex.grid[i,1]]<-torg.ex.grid[i,3]
	print(i)
}

org.tmp.mat[1:20,1:20]

write.csv(org.tmp.mat,file="Group.Fina.Down.Matrix.csv")





temp.init = huma_up_group
colnames(temp.init)[1]<-"ID"
temp<-merge(temp.init,resp.dat)
bigdat<-temp[with(temp,order(ORG,ID)),]
group.temp.full<-bigdat
tmp.group.full<-group.temp.full
last.col <- grep("North.Central.Action",colnames(group.temp.full))


resp.temp.mat<-matrix(data=0,nrow=nrow(tmp.group.full),ncol=nrow(tmp.group.full))
colnames(resp.temp.mat)<-tmp.group.full$ORG
rownames(resp.temp.mat)<-tmp.group.full$ORG
grp.name.ref<-colnames(tmp.group.full)[2:last.col]
 
group.tmp.full.wt <- tmp.group.full

tpart.overlap.score <- rep(0,nrow(tmp.group.full)*nrow(tmp.group.full))
tpart.ex.grid<-expand.grid(1:length((tmp.group.full$ORG)),1:length((tmp.group.full$ORG)))


for (i in 1:(nrow(tpart.ex.grid)*.5))
{
	tpart.overlap.score[i]<-sum(apply((rbind(group.tmp.full.wt[tpart.ex.grid[i,1],2:last.col],
	group.tmp.full.wt[tpart.ex.grid[i,2],2:last.col])),2,min))
	print(i)
}
tpart.ex.grid[,3]<-c(tpart.overlap.score[1:80000],rep(0,80000))

for (i in 1:(nrow(tpart.ex.grid)*.5))
{
	resp.temp.mat[tpart.ex.grid[i,1],tpart.ex.grid[i,2]]<-tpart.ex.grid[i,3]
	resp.temp.mat[tpart.ex.grid[i,2],tpart.ex.grid[i,1]]<-tpart.ex.grid[i,3]
	print(i)
}


org.tmp.mat<-matrix(rep(0,length(unique(colnames(resp.temp.mat)))*length(unique(colnames(resp.temp.mat)))),ncol=(length(unique(colnames(resp.temp.mat)))))
org.temp.score <- rep(0,length(unique(colnames(resp.temp.mat)))*length(unique(colnames(resp.temp.mat))))
colnames(org.tmp.mat)<-sort(unique(colnames(resp.temp.mat)))
rownames(org.tmp.mat)<-sort(unique(colnames(resp.temp.mat)))
torg.ex.grid <- expand.grid(1:length(unique(colnames(resp.temp.mat))),1:length(unique(colnames(resp.temp.mat))))


for (i in 1:nrow(torg.ex.grid))
{
	org.temp.score[i]<-(sum(apply(rbind(apply(group.tmp.full.wt[group.tmp.full.wt$ORG == colnames(org.tmp.mat)[torg.ex.grid[i,1]],2:last.col],2,max),
	apply(group.tmp.full.wt[group.tmp.full.wt$ORG == colnames(org.tmp.mat)[torg.ex.grid[i,2]],2:last.col],2,max)),2,min)))
print(i)
}

torg.ex.grid[,3]<-org.temp.score


for (i in 1:nrow(torg.ex.grid))
{
	org.tmp.mat[torg.ex.grid[i,1],torg.ex.grid[i,2]]<-torg.ex.grid[i,3]
	org.tmp.mat[torg.ex.grid[i,2],torg.ex.grid[i,1]]<-torg.ex.grid[i,3]
	print(i)
}

org.tmp.mat[1:20,1:20]

write.csv(org.tmp.mat,file="Group.Huma.Up.Matrix.csv")




temp.init = huma_down_group
colnames(temp.init)[1]<-"ID"
temp<-merge(temp.init,resp.dat)
bigdat<-temp[with(temp,order(ORG,ID)),]
group.temp.full<-bigdat
tmp.group.full<-group.temp.full
last.col <- grep("North.Central.Action",colnames(group.temp.full))


resp.temp.mat<-matrix(data=0,nrow=nrow(tmp.group.full),ncol=nrow(tmp.group.full))
colnames(resp.temp.mat)<-tmp.group.full$ORG
rownames(resp.temp.mat)<-tmp.group.full$ORG
grp.name.ref<-colnames(tmp.group.full)[2:last.col]
 
group.tmp.full.wt <- tmp.group.full

tpart.overlap.score <- rep(0,nrow(tmp.group.full)*nrow(tmp.group.full))
tpart.ex.grid<-expand.grid(1:length((tmp.group.full$ORG)),1:length((tmp.group.full$ORG)))


for (i in 1:(nrow(tpart.ex.grid)*.5))
{
	tpart.overlap.score[i]<-sum(apply((rbind(group.tmp.full.wt[tpart.ex.grid[i,1],2:last.col],
	group.tmp.full.wt[tpart.ex.grid[i,2],2:last.col])),2,min))
	print(i)
}
tpart.ex.grid[,3]<-c(tpart.overlap.score[1:80000],rep(0,80000))

for (i in 1:(nrow(tpart.ex.grid)*.5))
{
	resp.temp.mat[tpart.ex.grid[i,1],tpart.ex.grid[i,2]]<-tpart.ex.grid[i,3]
	resp.temp.mat[tpart.ex.grid[i,2],tpart.ex.grid[i,1]]<-tpart.ex.grid[i,3]
	print(i)
}


org.tmp.mat<-matrix(rep(0,length(unique(colnames(resp.temp.mat)))*length(unique(colnames(resp.temp.mat)))),ncol=(length(unique(colnames(resp.temp.mat)))))
org.temp.score <- rep(0,length(unique(colnames(resp.temp.mat)))*length(unique(colnames(resp.temp.mat))))
colnames(org.tmp.mat)<-sort(unique(colnames(resp.temp.mat)))
rownames(org.tmp.mat)<-sort(unique(colnames(resp.temp.mat)))
torg.ex.grid <- expand.grid(1:length(unique(colnames(resp.temp.mat))),1:length(unique(colnames(resp.temp.mat))))


for (i in 1:nrow(torg.ex.grid))
{
	org.temp.score[i]<-(sum(apply(rbind(apply(group.tmp.full.wt[group.tmp.full.wt$ORG == colnames(org.tmp.mat)[torg.ex.grid[i,1]],2:last.col],2,max),
	apply(group.tmp.full.wt[group.tmp.full.wt$ORG == colnames(org.tmp.mat)[torg.ex.grid[i,2]],2:last.col],2,max)),2,min)))
print(i)
}

torg.ex.grid[,3]<-org.temp.score


for (i in 1:nrow(torg.ex.grid))
{
	org.tmp.mat[torg.ex.grid[i,1],torg.ex.grid[i,2]]<-torg.ex.grid[i,3]
	org.tmp.mat[torg.ex.grid[i,2],torg.ex.grid[i,1]]<-torg.ex.grid[i,3]
	print(i)
}

org.tmp.mat[1:20,1:20]

write.csv(org.tmp.mat,file="Group.Huma.Down.Matrix.csv")




temp.init = lang_up_group
colnames(temp.init)[1]<-"ID"
temp<-merge(temp.init,resp.dat)
bigdat<-temp[with(temp,order(ORG,ID)),]
group.temp.full<-bigdat
tmp.group.full<-group.temp.full
last.col <- grep("North.Central.Action",colnames(group.temp.full))


resp.temp.mat<-matrix(data=0,nrow=nrow(tmp.group.full),ncol=nrow(tmp.group.full))
colnames(resp.temp.mat)<-tmp.group.full$ORG
rownames(resp.temp.mat)<-tmp.group.full$ORG
grp.name.ref<-colnames(tmp.group.full)[2:last.col]
 
group.tmp.full.wt <- tmp.group.full

tpart.overlap.score <- rep(0,nrow(tmp.group.full)*nrow(tmp.group.full))
tpart.ex.grid<-expand.grid(1:length((tmp.group.full$ORG)),1:length((tmp.group.full$ORG)))


for (i in 1:(nrow(tpart.ex.grid)*.5))
{
	tpart.overlap.score[i]<-sum(apply((rbind(group.tmp.full.wt[tpart.ex.grid[i,1],2:last.col],
	group.tmp.full.wt[tpart.ex.grid[i,2],2:last.col])),2,min))
	print(i)
}
tpart.ex.grid[,3]<-c(tpart.overlap.score[1:80000],rep(0,80000))

for (i in 1:(nrow(tpart.ex.grid)*.5))
{
	resp.temp.mat[tpart.ex.grid[i,1],tpart.ex.grid[i,2]]<-tpart.ex.grid[i,3]
	resp.temp.mat[tpart.ex.grid[i,2],tpart.ex.grid[i,1]]<-tpart.ex.grid[i,3]
	print(i)
}


org.tmp.mat<-matrix(rep(0,length(unique(colnames(resp.temp.mat)))*length(unique(colnames(resp.temp.mat)))),ncol=(length(unique(colnames(resp.temp.mat)))))
org.temp.score <- rep(0,length(unique(colnames(resp.temp.mat)))*length(unique(colnames(resp.temp.mat))))
colnames(org.tmp.mat)<-sort(unique(colnames(resp.temp.mat)))
rownames(org.tmp.mat)<-sort(unique(colnames(resp.temp.mat)))
torg.ex.grid <- expand.grid(1:length(unique(colnames(resp.temp.mat))),1:length(unique(colnames(resp.temp.mat))))


for (i in 1:nrow(torg.ex.grid))
{
	org.temp.score[i]<-(sum(apply(rbind(apply(group.tmp.full.wt[group.tmp.full.wt$ORG == colnames(org.tmp.mat)[torg.ex.grid[i,1]],2:last.col],2,max),
	apply(group.tmp.full.wt[group.tmp.full.wt$ORG == colnames(org.tmp.mat)[torg.ex.grid[i,2]],2:last.col],2,max)),2,min)))
print(i)
}

torg.ex.grid[,3]<-org.temp.score


for (i in 1:nrow(torg.ex.grid))
{
	org.tmp.mat[torg.ex.grid[i,1],torg.ex.grid[i,2]]<-torg.ex.grid[i,3]
	org.tmp.mat[torg.ex.grid[i,2],torg.ex.grid[i,1]]<-torg.ex.grid[i,3]
	print(i)
}

org.tmp.mat[1:20,1:20]

write.csv(org.tmp.mat,file="Group.Lang.Up.Matrix.csv")




temp.init = lang_down_group
colnames(temp.init)[1]<-"ID"
temp<-merge(temp.init,resp.dat)
bigdat<-temp[with(temp,order(ORG,ID)),]
group.temp.full<-bigdat
tmp.group.full<-group.temp.full
last.col <- grep("North.Central.Action",colnames(group.temp.full))


resp.temp.mat<-matrix(data=0,nrow=nrow(tmp.group.full),ncol=nrow(tmp.group.full))
colnames(resp.temp.mat)<-tmp.group.full$ORG
rownames(resp.temp.mat)<-tmp.group.full$ORG
grp.name.ref<-colnames(tmp.group.full)[2:last.col]
 
group.tmp.full.wt <- tmp.group.full

tpart.overlap.score <- rep(0,nrow(tmp.group.full)*nrow(tmp.group.full))
tpart.ex.grid<-expand.grid(1:length((tmp.group.full$ORG)),1:length((tmp.group.full$ORG)))


for (i in 1:(nrow(tpart.ex.grid)*.5))
{
	tpart.overlap.score[i]<-sum(apply((rbind(group.tmp.full.wt[tpart.ex.grid[i,1],2:last.col],
	group.tmp.full.wt[tpart.ex.grid[i,2],2:last.col])),2,min))
	print(i)
}
tpart.ex.grid[,3]<-c(tpart.overlap.score[1:80000],rep(0,80000))

for (i in 1:(nrow(tpart.ex.grid)*.5))
{
	resp.temp.mat[tpart.ex.grid[i,1],tpart.ex.grid[i,2]]<-tpart.ex.grid[i,3]
	resp.temp.mat[tpart.ex.grid[i,2],tpart.ex.grid[i,1]]<-tpart.ex.grid[i,3]
	print(i)
}


org.tmp.mat<-matrix(rep(0,length(unique(colnames(resp.temp.mat)))*length(unique(colnames(resp.temp.mat)))),ncol=(length(unique(colnames(resp.temp.mat)))))
org.temp.score <- rep(0,length(unique(colnames(resp.temp.mat)))*length(unique(colnames(resp.temp.mat))))
colnames(org.tmp.mat)<-sort(unique(colnames(resp.temp.mat)))
rownames(org.tmp.mat)<-sort(unique(colnames(resp.temp.mat)))
torg.ex.grid <- expand.grid(1:length(unique(colnames(resp.temp.mat))),1:length(unique(colnames(resp.temp.mat))))


for (i in 1:nrow(torg.ex.grid))
{
	org.temp.score[i]<-(sum(apply(rbind(apply(group.tmp.full.wt[group.tmp.full.wt$ORG == colnames(org.tmp.mat)[torg.ex.grid[i,1]],2:last.col],2,max),
	apply(group.tmp.full.wt[group.tmp.full.wt$ORG == colnames(org.tmp.mat)[torg.ex.grid[i,2]],2:last.col],2,max)),2,min)))
print(i)
}

torg.ex.grid[,3]<-org.temp.score


for (i in 1:nrow(torg.ex.grid))
{
	org.tmp.mat[torg.ex.grid[i,1],torg.ex.grid[i,2]]<-torg.ex.grid[i,3]
	org.tmp.mat[torg.ex.grid[i,2],torg.ex.grid[i,1]]<-torg.ex.grid[i,3]
	print(i)
}

org.tmp.mat[1:20,1:20]

write.csv(org.tmp.mat,file="Group.Lang.Down.Matrix.csv")




temp.init = scie_up_group
colnames(temp.init)[1]<-"ID"
temp<-merge(temp.init,resp.dat)
bigdat<-temp[with(temp,order(ORG,ID)),]
group.temp.full<-bigdat
tmp.group.full<-group.temp.full
last.col <- grep("North.Central.Action",colnames(group.temp.full))


resp.temp.mat<-matrix(data=0,nrow=nrow(tmp.group.full),ncol=nrow(tmp.group.full))
colnames(resp.temp.mat)<-tmp.group.full$ORG
rownames(resp.temp.mat)<-tmp.group.full$ORG
grp.name.ref<-colnames(tmp.group.full)[2:last.col]
 
group.tmp.full.wt <- tmp.group.full

tpart.overlap.score <- rep(0,nrow(tmp.group.full)*nrow(tmp.group.full))
tpart.ex.grid<-expand.grid(1:length((tmp.group.full$ORG)),1:length((tmp.group.full$ORG)))


for (i in 1:(nrow(tpart.ex.grid)*.5))
{
	tpart.overlap.score[i]<-sum(apply((rbind(group.tmp.full.wt[tpart.ex.grid[i,1],2:last.col],
	group.tmp.full.wt[tpart.ex.grid[i,2],2:last.col])),2,min))
	print(i)
}
tpart.ex.grid[,3]<-c(tpart.overlap.score[1:80000],rep(0,80000))

for (i in 1:(nrow(tpart.ex.grid)*.5))
{
	resp.temp.mat[tpart.ex.grid[i,1],tpart.ex.grid[i,2]]<-tpart.ex.grid[i,3]
	resp.temp.mat[tpart.ex.grid[i,2],tpart.ex.grid[i,1]]<-tpart.ex.grid[i,3]
	print(i)
}


org.tmp.mat<-matrix(rep(0,length(unique(colnames(resp.temp.mat)))*length(unique(colnames(resp.temp.mat)))),ncol=(length(unique(colnames(resp.temp.mat)))))
org.temp.score <- rep(0,length(unique(colnames(resp.temp.mat)))*length(unique(colnames(resp.temp.mat))))
colnames(org.tmp.mat)<-sort(unique(colnames(resp.temp.mat)))
rownames(org.tmp.mat)<-sort(unique(colnames(resp.temp.mat)))
torg.ex.grid <- expand.grid(1:length(unique(colnames(resp.temp.mat))),1:length(unique(colnames(resp.temp.mat))))


for (i in 1:nrow(torg.ex.grid))
{
	org.temp.score[i]<-(sum(apply(rbind(apply(group.tmp.full.wt[group.tmp.full.wt$ORG == colnames(org.tmp.mat)[torg.ex.grid[i,1]],2:last.col],2,max),
	apply(group.tmp.full.wt[group.tmp.full.wt$ORG == colnames(org.tmp.mat)[torg.ex.grid[i,2]],2:last.col],2,max)),2,min)))
print(i)
}

torg.ex.grid[,3]<-org.temp.score


for (i in 1:nrow(torg.ex.grid))
{
	org.tmp.mat[torg.ex.grid[i,1],torg.ex.grid[i,2]]<-torg.ex.grid[i,3]
	org.tmp.mat[torg.ex.grid[i,2],torg.ex.grid[i,1]]<-torg.ex.grid[i,3]
	print(i)
}

org.tmp.mat[1:20,1:20]

write.csv(org.tmp.mat,file="Group.Scie.Up.Matrix.csv")





temp.init = scie_down_group
colnames(temp.init)[1]<-"ID"
temp<-merge(temp.init,resp.dat)
bigdat<-temp[with(temp,order(ORG,ID)),]
group.temp.full<-bigdat
tmp.group.full<-group.temp.full
last.col <- grep("North.Central.Action",colnames(group.temp.full))


resp.temp.mat<-matrix(data=0,nrow=nrow(tmp.group.full),ncol=nrow(tmp.group.full))
colnames(resp.temp.mat)<-tmp.group.full$ORG
rownames(resp.temp.mat)<-tmp.group.full$ORG
grp.name.ref<-colnames(tmp.group.full)[2:last.col]
 
group.tmp.full.wt <- tmp.group.full

tpart.overlap.score <- rep(0,nrow(tmp.group.full)*nrow(tmp.group.full))
tpart.ex.grid<-expand.grid(1:length((tmp.group.full$ORG)),1:length((tmp.group.full$ORG)))


for (i in 1:(nrow(tpart.ex.grid)*.5))
{
	tpart.overlap.score[i]<-sum(apply((rbind(group.tmp.full.wt[tpart.ex.grid[i,1],2:last.col],
	group.tmp.full.wt[tpart.ex.grid[i,2],2:last.col])),2,min))
	print(i)
}
tpart.ex.grid[,3]<-c(tpart.overlap.score[1:80000],rep(0,80000))

for (i in 1:(nrow(tpart.ex.grid)*.5))
{
	resp.temp.mat[tpart.ex.grid[i,1],tpart.ex.grid[i,2]]<-tpart.ex.grid[i,3]
	resp.temp.mat[tpart.ex.grid[i,2],tpart.ex.grid[i,1]]<-tpart.ex.grid[i,3]
	print(i)
}


org.tmp.mat<-matrix(rep(0,length(unique(colnames(resp.temp.mat)))*length(unique(colnames(resp.temp.mat)))),ncol=(length(unique(colnames(resp.temp.mat)))))
org.temp.score <- rep(0,length(unique(colnames(resp.temp.mat)))*length(unique(colnames(resp.temp.mat))))
colnames(org.tmp.mat)<-sort(unique(colnames(resp.temp.mat)))
rownames(org.tmp.mat)<-sort(unique(colnames(resp.temp.mat)))
torg.ex.grid <- expand.grid(1:length(unique(colnames(resp.temp.mat))),1:length(unique(colnames(resp.temp.mat))))


for (i in 1:nrow(torg.ex.grid))
{
	org.temp.score[i]<-(sum(apply(rbind(apply(group.tmp.full.wt[group.tmp.full.wt$ORG == colnames(org.tmp.mat)[torg.ex.grid[i,1]],2:last.col],2,max),
	apply(group.tmp.full.wt[group.tmp.full.wt$ORG == colnames(org.tmp.mat)[torg.ex.grid[i,2]],2:last.col],2,max)),2,min)))
print(i)
}

torg.ex.grid[,3]<-org.temp.score


for (i in 1:nrow(torg.ex.grid))
{
	org.tmp.mat[torg.ex.grid[i,1],torg.ex.grid[i,2]]<-torg.ex.grid[i,3]
	org.tmp.mat[torg.ex.grid[i,2],torg.ex.grid[i,1]]<-torg.ex.grid[i,3]
	print(i)
}

org.tmp.mat[1:20,1:20]

write.csv(org.tmp.mat,file="Group.Scie.Down.Matrix.csv")





temp.init = valu_up_group
colnames(temp.init)[1]<-"ID"
temp<-merge(temp.init,resp.dat)
bigdat<-temp[with(temp,order(ORG,ID)),]
group.temp.full<-bigdat
tmp.group.full<-group.temp.full
last.col <- grep("North.Central.Action",colnames(group.temp.full))


resp.temp.mat<-matrix(data=0,nrow=nrow(tmp.group.full),ncol=nrow(tmp.group.full))
colnames(resp.temp.mat)<-tmp.group.full$ORG
rownames(resp.temp.mat)<-tmp.group.full$ORG
grp.name.ref<-colnames(tmp.group.full)[2:last.col]
 
group.tmp.full.wt <- tmp.group.full

tpart.overlap.score <- rep(0,nrow(tmp.group.full)*nrow(tmp.group.full))
tpart.ex.grid<-expand.grid(1:length((tmp.group.full$ORG)),1:length((tmp.group.full$ORG)))


for (i in 1:(nrow(tpart.ex.grid)*.5))
{
	tpart.overlap.score[i]<-sum(apply((rbind(group.tmp.full.wt[tpart.ex.grid[i,1],2:last.col],
	group.tmp.full.wt[tpart.ex.grid[i,2],2:last.col])),2,min))
	print(i)
}
tpart.ex.grid[,3]<-c(tpart.overlap.score[1:80000],rep(0,80000))

for (i in 1:(nrow(tpart.ex.grid)*.5))
{
	resp.temp.mat[tpart.ex.grid[i,1],tpart.ex.grid[i,2]]<-tpart.ex.grid[i,3]
	resp.temp.mat[tpart.ex.grid[i,2],tpart.ex.grid[i,1]]<-tpart.ex.grid[i,3]
	print(i)
}


org.tmp.mat<-matrix(rep(0,length(unique(colnames(resp.temp.mat)))*length(unique(colnames(resp.temp.mat)))),ncol=(length(unique(colnames(resp.temp.mat)))))
org.temp.score <- rep(0,length(unique(colnames(resp.temp.mat)))*length(unique(colnames(resp.temp.mat))))
colnames(org.tmp.mat)<-sort(unique(colnames(resp.temp.mat)))
rownames(org.tmp.mat)<-sort(unique(colnames(resp.temp.mat)))
torg.ex.grid <- expand.grid(1:length(unique(colnames(resp.temp.mat))),1:length(unique(colnames(resp.temp.mat))))


for (i in 1:nrow(torg.ex.grid))
{
	org.temp.score[i]<-(sum(apply(rbind(apply(group.tmp.full.wt[group.tmp.full.wt$ORG == colnames(org.tmp.mat)[torg.ex.grid[i,1]],2:last.col],2,max),
	apply(group.tmp.full.wt[group.tmp.full.wt$ORG == colnames(org.tmp.mat)[torg.ex.grid[i,2]],2:last.col],2,max)),2,min)))
print(i)
}

torg.ex.grid[,3]<-org.temp.score


for (i in 1:nrow(torg.ex.grid))
{
	org.tmp.mat[torg.ex.grid[i,1],torg.ex.grid[i,2]]<-torg.ex.grid[i,3]
	org.tmp.mat[torg.ex.grid[i,2],torg.ex.grid[i,1]]<-torg.ex.grid[i,3]
	print(i)
}

org.tmp.mat[1:20,1:20]

write.csv(org.tmp.mat,file="Group.Valu.Up.Matrix.csv")





temp.init = valu_down_group
colnames(temp.init)[1]<-"ID"
temp<-merge(temp.init,resp.dat)
bigdat<-temp[with(temp,order(ORG,ID)),]
group.temp.full<-bigdat
tmp.group.full<-group.temp.full
last.col <- grep("North.Central.Action",colnames(group.temp.full))


resp.temp.mat<-matrix(data=0,nrow=nrow(tmp.group.full),ncol=nrow(tmp.group.full))
colnames(resp.temp.mat)<-tmp.group.full$ORG
rownames(resp.temp.mat)<-tmp.group.full$ORG
grp.name.ref<-colnames(tmp.group.full)[2:last.col]
 
group.tmp.full.wt <- tmp.group.full

tpart.overlap.score <- rep(0,nrow(tmp.group.full)*nrow(tmp.group.full))
tpart.ex.grid<-expand.grid(1:length((tmp.group.full$ORG)),1:length((tmp.group.full$ORG)))


for (i in 1:(nrow(tpart.ex.grid)*.5))
{
	tpart.overlap.score[i]<-sum(apply((rbind(group.tmp.full.wt[tpart.ex.grid[i,1],2:last.col],
	group.tmp.full.wt[tpart.ex.grid[i,2],2:last.col])),2,min))
	print(i)
}
tpart.ex.grid[,3]<-c(tpart.overlap.score[1:80000],rep(0,80000))

for (i in 1:(nrow(tpart.ex.grid)*.5))
{
	resp.temp.mat[tpart.ex.grid[i,1],tpart.ex.grid[i,2]]<-tpart.ex.grid[i,3]
	resp.temp.mat[tpart.ex.grid[i,2],tpart.ex.grid[i,1]]<-tpart.ex.grid[i,3]
	print(i)
}


org.tmp.mat<-matrix(rep(0,length(unique(colnames(resp.temp.mat)))*length(unique(colnames(resp.temp.mat)))),ncol=(length(unique(colnames(resp.temp.mat)))))
org.temp.score <- rep(0,length(unique(colnames(resp.temp.mat)))*length(unique(colnames(resp.temp.mat))))
colnames(org.tmp.mat)<-sort(unique(colnames(resp.temp.mat)))
rownames(org.tmp.mat)<-sort(unique(colnames(resp.temp.mat)))
torg.ex.grid <- expand.grid(1:length(unique(colnames(resp.temp.mat))),1:length(unique(colnames(resp.temp.mat))))


for (i in 1:nrow(torg.ex.grid))
{
	org.temp.score[i]<-(sum(apply(rbind(apply(group.tmp.full.wt[group.tmp.full.wt$ORG == colnames(org.tmp.mat)[torg.ex.grid[i,1]],2:last.col],2,max),
	apply(group.tmp.full.wt[group.tmp.full.wt$ORG == colnames(org.tmp.mat)[torg.ex.grid[i,2]],2:last.col],2,max)),2,min)))
print(i)
}

torg.ex.grid[,3]<-org.temp.score


for (i in 1:nrow(torg.ex.grid))
{
	org.tmp.mat[torg.ex.grid[i,1],torg.ex.grid[i,2]]<-torg.ex.grid[i,3]
	org.tmp.mat[torg.ex.grid[i,2],torg.ex.grid[i,1]]<-torg.ex.grid[i,3]
	print(i)
}

org.tmp.mat[1:20,1:20]

write.csv(org.tmp.mat,file="Group.Value.Down.Matrix.csv")