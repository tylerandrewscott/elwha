Short.Data<-read.csv("//Users/TScott/Google Drive/PSP_Project/PS_Data/Final_Scrubbed.csv",header=T)
SD<-Short.Data
SD.fullorgs<-subset(SD,SD[,20]!="")
SD.fullorgs$Last.page.saved<-as.numeric(gsub("P","",SD.fullorgs$Last.page.saved))
SD.answers<-subset(SD.fullorgs,SD.fullorgs$Last.page.saved>=15)
data2<-SD.answers
Org.Dat<-data.frame(cbind(as.character(data2[,1]),as.character(data2[,20]),as.character(data2[,21])));colnames(Org.Dat)<-c("ID","ORG","Detail")
write.csv(Org.Dat,file="//Users/TScott/Google Drive/PSP_Project/PS_Data/Org.Dat.csv")