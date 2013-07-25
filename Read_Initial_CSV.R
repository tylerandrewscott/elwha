Short.Data<-read.csv("//Users/TScott/Google Drive/PSP_Project/PS_Data/Final_Scrubbed.csv",header=T)

dat2<-Short.Data
colnames(dat2)<-c("ID","Date0","Date1","Time","Lpage","Fname","Lname","Groups")


Base.Dat<-data.frame(dat2$ID,dat2$Date0,dat2$Date1,dat2$Time,dat2$Lpage,dat2$Fname,dat2$Lname)
colnames(Base.Dat)<-c("ID","Date0","Date1","Time","Lpage","Fname","Lname")


completion.page<-as.numeric(gsub("P","",Short.Data[,5]))

Base.Dat<-subset(Base.Dat,completion.page>=15)


write.csv(Base.Dat,file="//Users/TScott/Google Drive/PSP_Project/PS_Data/Base.Dat.csv")

