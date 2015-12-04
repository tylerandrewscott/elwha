rm(list=ls())
library(statnet)


setwd("//Users/TScott/Google Drive/elwha")
edge.dat<-read.csv('Edgelist.Dat.Good.csv',row.names=1)

resp.dat<-read.csv('Response.Used.csv',row.names=1)
#group.dat.byedge<-read.csv("//Users/TScott/Google Drive/PSP_Project/PS_Data/Group.Membership.Dat.csv",row.names=1,header=T)
group.dat.byedge<-read.csv("Group.All.Dat.csv",row.names=1,header=T)
group.dat.byresp<-read.csv("Group.Member.Dat.csv",row.names=1,header=T)


temp<-data.frame(edge.dat$ORG,edge.dat$Contact,edge.dat$TType,edge.dat$WIN5)
colnames(temp)<-c("ORG","Contact","TType","WIN5")
temp<-temp[temp$TType=="WT",]
write.csv(temp,file="edgelist_implement.csv")

temp1<-data.frame(edge.dat$ORG,edge.dat$Contact,edge.dat$TType,edge.dat$WIN5)
colnames(temp1)<-c("ORG","Contact","TType","WIN5")
temp1<-temp1[temp1$TType=="PT",]
write.csv(temp1,file="edgelist_plan.csv")

temp2<-data.frame(edge.dat$ORG,edge.dat$Contact,edge.dat$TType,edge.dat$WIN5)
colnames(temp2)<-c("ORG","Contact","TType","WIN5")
temp<-temp2[temp2$TType=="CT",]
write.csv(temp2,file="edgelist_consult.csv")

temp3<-data.frame(edge.dat$ORG,edge.dat$Contact,edge.dat$TType,edge.dat$WIN5)
colnames(temp3)<-c("ORG","Contact","TType","WIN5")
write.csv(temp3,file="edgelist_all.csv")

