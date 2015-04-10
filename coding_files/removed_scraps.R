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

group.overlap.dat<-read.csv("Group.Overlap.Matrix.csv",row.names=1,header=T)
group.overlap.dat.psp<-read.csv("Group.Overlap.Matrix.PSP.csv",row.names=1,header=T)
group.overlap.dat.npsp<-read.csv("Group.Overlap.Matrix.NPSP.csv",row.names=1,header=T)

temp<-data.frame(resp.dat$ORG,resp.dat$ID);colnames(temp)<-c("ORG","ID")
group.dat.byresp<-merge(group.dat.byresp,temp,by="ID")
group.dat.byresp.psp<-merge(group.dat.byresp.psp,temp,by="ID")
group.dat.byresp.npsp<-merge(group.dat.byresp.npsp,temp,by="ID")
group.byresp.full<-group.dat.byresp
group.byresp.full.psp<-group.dat.byresp.psp
group.byresp.full.npsp<-group.dat.byresp.npsp


alter.list<-edge.dat$Contact

fill1<-rep(0,length(alter.list))

for (i in 1:length(alter.list))
	{
		tem<-grep(as.character(alter.list[i]),edge.dat$ORG,fixed=TRUE)
		#fill[i]<-egolist3[tem[1]]
		fill1[i]<-ifelse(length(tem)>0,1,0)
	}
	
	

inter.dat<-cbind(edge.dat,fill1)


inter.dat<-cbind(edge.dat,fill1)

#colnames(inter.dat)<-c(colnames(edge.dat),"is.resp")
#tt<-subset(inter.dat,inter.dat$is.resp==1)

tt1<-inter.dat


tt<-tt1

bb<-data.frame(tt[2],tt[9],tt[c(-2,-9)])

final.edgelist<-bb
edgelist_consult = final.edgelist[final.edgelist$TType=="CT",]
edgelist_plan = final.edgelist[final.edgelist$TType=="PT",]
edgelist_implement = final.edgelist[final.edgelist$TType=="WT",]
edgelist_all = final.edgelist



fe<-edgelist_all
alter.list2<-fe$Contact
fill2<-rep(0,length(alter.list2))

for (i in 1:length(alter.list2))
	{
		tem<-grep(as.character(alter.list2[i]),fe$ORG,fixed=TRUE)
		#fill[i]<-egolist3[tem[1]]
		fill2[i]<-ifelse(length(tem)>0,1,0)
	}

fe$fill2<-fill2

fe1.1<-subset(fe,fe$fill2==1)
fe0.1<-subset(fe,fe$fill2==0)


fe0<-subset(fe0.1,(as.character(fe0.1$ORG)==as.character(fe0.1$Contact))==FALSE)
fe1<-subset(fe1.1,(as.character(fe1.1$ORG)==as.character(fe1.1$Contact))==FALSE)

fe1uq<-duplicated(c(as.character(fe1$ORG),as.character(fe1$Contact),as.character(fe0$ORG),as.character(fe0$Contact)))

everyname<-c(as.character(fe1$ORG),as.character(fe1$Contact),as.character(fe0$ORG),as.character(fe0$Contact))
uniquename<-subset(everyname,duplicated(c(as.character(fe1$ORG),as.character(fe1$Contact),as.character(fe0$ORG),as.character(fe0$Contact)))==FALSE)

temp<-resp.dat[,grep("routine",colnames(resp.dat))]
for (i in 1:ncol(temp))
{{temp[,i]<-as.character(temp[,i])}}

test<-data.frame(rep(resp.dat$ID,15),rep(resp.dat$ORG,15),stack(temp))
colnames(test)<-c("ID","ORG","CONTACT","TIEPOS")


all.edges<-test
colnames(all.edges)<-c("ORG","Contact")
all.edges1<-subset(all.edges,as.character(all.edges$ORG)!=as.character(all.edges$CONTACT))
all.edges<-subset(all.edges1,as.character(all.edges1$CONTACT)!='')


fe<-edgelist_plan
alter.list2<-fe$Contact
fill2<-rep(0,length(alter.list2))

for (i in 1:length(alter.list2))
	{
		tem<-grep(as.character(alter.list2[i]),fe$ORG,fixed=TRUE)
		#fill[i]<-egolist3[tem[1]]
		fill2[i]<-ifelse(length(tem)>0,1,0)
	}

fe$fill2<-fill2

fe1.1<-subset(fe,fe$fill2==1)
fe0.1<-subset(fe,fe$fill2==0)


fe0<-subset(fe0.1,(as.character(fe0.1$ORG)==as.character(fe0.1$Contact))==FALSE)
fe1<-subset(fe1.1,(as.character(fe1.1$ORG)==as.character(fe1.1$Contact))==FALSE)

fe1uq<-duplicated(c(as.character(fe1$ORG),as.character(fe1$Contact),as.character(fe0$ORG),as.character(fe0$Contact)))

everyname<-c(as.character(fe1$ORG),as.character(fe1$Contact),as.character(fe0$ORG),as.character(fe0$Contact))
uniquename<-subset(everyname,duplicated(c(as.character(fe1$ORG),as.character(fe1$Contact),as.character(fe0$ORG),as.character(fe0$Contact)))==FALSE)


all.edges<-data.frame(cbind(rep(as.character(resp.dat[,2]),15),c(as.vector(resp.dat[,8]),as.vector(resp.dat[,9]),as.vector(resp.dat[,10]),as.vector(resp.dat[,11]),as.vector(resp.dat[,12]),as.vector(resp.dat[,13]),as.vector(resp.dat[,14]),as.vector(resp.dat[,15]),as.vector(resp.dat[,16]),as.vector(resp.dat[,17]),as.vector(resp.dat[,18]),as.vector(resp.dat[,19]),as.vector(resp.dat[,20]),as.vector(resp.dat[,21]),as.vector(resp.dat[,22]))))
colnames(resp.dat)
colnames(all.edges)<-c("ORG","Contact")

all.edges1<-subset(all.edges,as.character(all.edges[,1])!=as.character(all.edges[,2]))
plan.edges<-subset(all.edges1,as.character(all.edges1[,2])!='')

fe<-edgelist_consult
alter.list2<-fe$Contact
fill2<-rep(0,length(alter.list2))

for (i in 1:length(alter.list2))
	{
		tem<-grep(as.character(alter.list2[i]),fe$ORG,fixed=TRUE)
		#fill[i]<-egolist3[tem[1]]
		fill2[i]<-ifelse(length(tem)>0,1,0)
	}

fe$fill2<-fill2

fe1.1<-subset(fe,fe$fill2==1)
fe0.1<-subset(fe,fe$fill2==0)


fe0<-subset(fe0.1,(as.character(fe0.1$ORG)==as.character(fe0.1$Contact))==FALSE)
fe1<-subset(fe1.1,(as.character(fe1.1$ORG)==as.character(fe1.1$Contact))==FALSE)

fe1uq<-duplicated(c(as.character(fe1$ORG),as.character(fe1$Contact),as.character(fe0$ORG),as.character(fe0$Contact)))

everyname<-c(as.character(fe1$ORG),as.character(fe1$Contact),as.character(fe0$ORG),as.character(fe0$Contact))
uniquename<-subset(everyname,duplicated(c(as.character(fe1$ORG),as.character(fe1$Contact),as.character(fe0$ORG),as.character(fe0$Contact)))==FALSE)


all.edges<-data.frame(cbind(rep(as.character(resp.dat[,2]),15),c(as.vector(resp.dat[,8]),as.vector(resp.dat[,9]),as.vector(resp.dat[,10]),as.vector(resp.dat[,11]),as.vector(resp.dat[,12]),as.vector(resp.dat[,13]),as.vector(resp.dat[,14]),as.vector(resp.dat[,15]),as.vector(resp.dat[,16]),as.vector(resp.dat[,17]),as.vector(resp.dat[,18]),as.vector(resp.dat[,19]),as.vector(resp.dat[,20]),as.vector(resp.dat[,21]),as.vector(resp.dat[,22]))))

colnames(all.edges)<-c("ORG","Contact")

all.edges1<-subset(all.edges,as.character(all.edges[,1])!=as.character(all.edges[,2]))
consult.edges<-subset(all.edges1,as.character(all.edges1[,2])!='')



fe<-edgelist_implement
alter.list2<-fe$Contact
fill2<-rep(0,length(alter.list2))

for (i in 1:length(alter.list2))
	{
		tem<-grep(as.character(alter.list2[i]),fe$ORG,fixed=TRUE)
		#fill[i]<-egolist3[tem[1]]
		fill2[i]<-ifelse(length(tem)>0,1,0)
	}

fe$fill2<-fill2

fe1.1<-subset(fe,fe$fill2==1)
fe0.1<-subset(fe,fe$fill2==0)


fe0<-subset(fe0.1,(as.character(fe0.1$ORG)==as.character(fe0.1$Contact))==FALSE)
fe1<-subset(fe1.1,(as.character(fe1.1$ORG)==as.character(fe1.1$Contact))==FALSE)

fe1uq<-duplicated(c(as.character(fe1$ORG),as.character(fe1$Contact),as.character(fe0$ORG),as.character(fe0$Contact)))

everyname<-c(as.character(fe1$ORG),as.character(fe1$Contact),as.character(fe0$ORG),as.character(fe0$Contact))
uniquename<-subset(everyname,duplicated(c(as.character(fe1$ORG),as.character(fe1$Contact),as.character(fe0$ORG),as.character(fe0$Contact)))==FALSE)


all.edges<-data.frame(cbind(rep(as.character(resp.dat[,2]),15),c(as.vector(resp.dat[,8]),as.vector(resp.dat[,9]),as.vector(resp.dat[,10]),as.vector(resp.dat[,11]),as.vector(resp.dat[,12]),as.vector(resp.dat[,13]),as.vector(resp.dat[,14]),as.vector(resp.dat[,15]),as.vector(resp.dat[,16]),as.vector(resp.dat[,17]),as.vector(resp.dat[,18]),as.vector(resp.dat[,19]),as.vector(resp.dat[,20]),as.vector(resp.dat[,21]),as.vector(resp.dat[,22]))))

colnames(all.edges)<-c("ORG","Contact")

all.edges1<-subset(all.edges,as.character(all.edges[,1])!=as.character(all.edges[,2]))


implement.edges<-subset(all.edges1,as.character(all.edges1[,2])!='')


imp.edges<-data.frame(cbind(rep(as.character(resp.dat[,2]),5),c(as.vector(resp.dat[,8]),as.vector(resp.dat[,9]),as.vector(resp.dat[,10]),as.vector(resp.dat[,11]),as.vector(resp.dat[,12])),rep("IMP",400*5)))
colnames(imp.edges)<-c("ORG","Contact","TYPE")
imp.edges<-imp.edges[is.na(imp.edges$Contact)==FALSE,]

plan.edges<-data.frame(cbind(rep(as.character(resp.dat[,2]),5),c(as.vector(resp.dat[,13]),as.vector(resp.dat[,14]),as.vector(resp.dat[,15]),as.vector(resp.dat[,16]),as.vector(resp.dat[,17])),rep("PLAN",400*5)))
colnames(plan.edges)<-c("ORG","Contact","TYPE")
plan.edges<-plan.edges[is.na(plan.edges$Contact)==FALSE,]

cons.edges<-data.frame(cbind(rep(as.character(resp.dat[,2]),5),c(as.vector(resp.dat[,18]),as.vector(resp.dat[,19]),as.vector(resp.dat[,20]),as.vector(resp.dat[,21]),as.vector(resp.dat[,22])),rep("CONS",400*5)))
colnames(cons.edges)<-c("ORG","Contact","TYPE")
cons.edges<-cons.edges[is.na(cons.edges$Contact)==FALSE,]