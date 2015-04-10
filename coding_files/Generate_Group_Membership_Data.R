rm(list=ls())
setwd("//Users/TScott/Google Drive/elwha")
Long.Data<-read.csv("Final_Verbose.csv",header=T,sep=",")
Short.Data<-read.csv("Final_Scrubbed.csv",header=T,sep=",")
SD<-Short.Data
SD.fullorgs<-subset(SD,SD[,20]!="")
SD.fullorgs$Last.page.saved<-as.numeric(gsub("P","",SD.fullorgs$Last.page.saved))
SD.answers<-subset(SD.fullorgs,SD.fullorgs$Last.page.saved>=15)
data2<-SD.answers


WD<-Long.Data

WD<-WD[WD$Response.ID %in% SD$Response.ID==TRUE,]


WD.fullorgs<-subset(WD,WD[,91]!="")
WD.fullorgs$Last.page.saved<-as.numeric(gsub("P","",WD.fullorgs$Last.page.saved))
WD.answers<-subset(WD.fullorgs,WD.fullorgs$Last.page.saved>=15)
dat1<-WD.answers
off<--1
#build group indicators dataframe

ID<-as.character(dat1[,1])
G.LC<-dat1$"Have.you.regularly.participated.in.any.of.the.following.collaborative.groups.working.on.Puget.Sound.recovery.in.the.past.five.years..Please.check.all.that.apply..The.PSP.Leadership.Council"
G.LC.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..The.PSP.Leadership.Council"
G.LC.part<-data.frame(dat1[,(505+off):(511+off)]);colnames(G.LC.part)<-c("email","meet","event","proj","read","prod","other")
 
 

temp<-colnames(data2)[grepl("My.participation.in.the.following.collaborative.group.",colnames(data2))==TRUE&
grepl("One.or.more.",colnames(data2))==FALSE&grepl("financial.resources.",colnames(data2))==TRUE &grepl("None.of.the.above",colnames(data2))==FALSE]
tmp <-as.matrix(data2[,temp])
tmp2<-ifelse(is.na(tmp)==TRUE,0,ifelse(tmp<=3,0,ifelse(tmp==4,1,2)))
tmp3<-ifelse(is.na(tmp)==TRUE,0,ifelse(tmp>=3,0,ifelse(tmp==2,1,2)))
tmp4<-data.frame(data2$Response.ID,tmp2)
tmp5<-data.frame(data2$Response.ID,tmp3)
write.csv(tmp4,file="Group.Financial.Resources.Up.Dat.csv")
write.csv(tmp5,file="Group.Financial.Resources.Down.Dat.csv")

temp<-colnames(data2)[grepl("My.participation.in.the.following.collaborative.group.",colnames(data2))==TRUE&
grepl("One.or.more.",colnames(data2))==FALSE&grepl("human.resources.",colnames(data2))==TRUE &grepl("None.of.the.above",colnames(data2))==FALSE]
tmp <-as.matrix(data2[,temp])
tmp2<-ifelse(is.na(tmp)==TRUE,0,ifelse(tmp<=3,0,ifelse(tmp==4,1,2)))
tmp3<-ifelse(is.na(tmp)==TRUE,0,ifelse(tmp>=3,0,ifelse(tmp==2,1,2)))
tmp4<-data.frame(data2$Response.ID,tmp2)
tmp5<-data.frame(data2$Response.ID,tmp3)
write.csv(tmp4,file="Group.Human.Resources.Up.Dat.csv")
write.csv(tmp5,file="Group.Human.Resources.Down.Dat.csv")

temp<-colnames(data2)[grepl("My.participation.in.the.following.collaborative.group.",colnames(data2))==TRUE&
grepl("One.or.more.",colnames(data2))==FALSE&grepl("scientific..technical",colnames(data2))==TRUE &grepl("None.of.the.above",colnames(data2))==FALSE]
tmp <-as.matrix(data2[,temp])
tmp2<-ifelse(is.na(tmp)==TRUE,0,ifelse(tmp<=3,0,ifelse(tmp==4,1,2)))
tmp3<-ifelse(is.na(tmp)==TRUE,0,ifelse(tmp>=3,0,ifelse(tmp==2,1,2)))
tmp4<-data.frame(data2$Response.ID,tmp2)
tmp5<-data.frame(data2$Response.ID,tmp3)
write.csv(tmp4,file="Group.Scientific.Up.Dat.csv")
write.csv(tmp5,file="Group.Scientific.Down.Dat.csv")
temp<-colnames(data2)[grepl("My.participation.in.the.following.collaborative.group.",colnames(data2))==TRUE&
grepl("One.or.more.",colnames(data2))==FALSE&grepl("face.to.face.",colnames(data2))==TRUE &grepl("None.of.the.above",colnames(data2))==FALSE]
tmp <-as.matrix(data2[,temp])
tmp2<-ifelse(is.na(tmp)==TRUE,0,ifelse(tmp<=3,0,ifelse(tmp==4,1,2)))
tmp3<-ifelse(is.na(tmp)==TRUE,0,ifelse(tmp>=3,0,ifelse(tmp==2,1,2)))
tmp4<-data.frame(data2$Response.ID,tmp2)
tmp5<-data.frame(data2$Response.ID,tmp3)
write.csv(tmp4,file="Group.Face.Up.Dat.csv")
write.csv(tmp5,file="Group.Face.Down.Dat.csv")

temp<-colnames(data2)[grepl("My.participation.in.the.following.collaborative.group.",colnames(data2))==TRUE&
grepl("One.or.more.",colnames(data2))==FALSE&grepl("used.language.",colnames(data2))==TRUE &grepl("None.of.the.above",colnames(data2))==FALSE]
tmp <-as.matrix(data2[,temp])
tmp2<-ifelse(is.na(tmp)==TRUE,0,ifelse(tmp<=3,0,ifelse(tmp==4,1,2)))
tmp3<-ifelse(is.na(tmp)==TRUE,0,ifelse(tmp>=3,0,ifelse(tmp==2,1,2)))
tmp4<-data.frame(data2$Response.ID,tmp2)
tmp5<-data.frame(data2$Response.ID,tmp3)
write.csv(tmp4,file="Group.Language.Up.Dat.csv")
write.csv(tmp5,file="Group.Language.Down.Dat.csv")

temp<-colnames(data2)[grepl("My.participation.in.the.following.collaborative.group.",colnames(data2))==TRUE&
grepl("One.or.more.",colnames(data2))==FALSE&grepl("interests.and.values.",colnames(data2))==TRUE &grepl("None.of.the.above",colnames(data2))==FALSE]
tmp <-as.matrix(data2[,temp])
tmp2<-ifelse(is.na(tmp)==TRUE,0,ifelse(tmp<=3,0,ifelse(tmp==4,1,2)))
tmp3<-ifelse(is.na(tmp)==TRUE,0,ifelse(tmp>=3,0,ifelse(tmp==2,1,2)))
tmp4<-data.frame(data2$Response.ID,tmp2)
tmp5<-data.frame(data2$Response.ID,tmp3)
write.csv(tmp4,file="Group.Values.Up.Dat.csv")
write.csv(tmp5,file="Group.Values.Down.Dat.csv")

   
G.ECB<-dat1$"Have.you.regularly.participated.in.any.of.the.following.collaborative.groups.working.on.Puget.Sound.recovery.in.the.past.five.years..Please.check.all.that.apply..The.PSP.Ecosystem.Coordination.Board" 
G.ECB.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..The.PSP.Ecosystem.Coordination.Board"
G.ECB.part<-data.frame(dat1[,(512+off):(518+off)]);colnames(G.ECB.part)<-c("email","meet","event","proj","read","prod","other")
        


G.SCIENCE<-dat1$"Have.you.regularly.participated.in.any.of.the.following.collaborative.groups.working.on.Puget.Sound.recovery.in.the.past.five.years..Please.check.all.that.apply..The.PSP.Science.Panel"
G.SCIENCE.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..The.PSP.Science.Panel"
G.SCIENCE.part<-data.frame(dat1[,(519+off):(525+off)]);colnames(G.SCIENCE.part)<-c("email","meet","event","proj","read","prod","other")

G.SOCSCI<-dat1$"Have.you.regularly.participated.in.any.of.the.following.collaborative.groups.working.on.Puget.Sound.recovery.in.the.past.five.years..Please.check.all.that.apply..The.PSP.Social.Science.Social.Strategies.Committee" 
G.SOCSCI.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..The.PSP.Social.Science.Social.Strategies.Committee"
G.SOCSCI.part<-data.frame(dat1[,(526+off):(532+off)]);colnames(G.SOCSCI.part)<-c("email","meet","event","proj","read","prod","other")
G.SRC<-dat1$"Have.you.regularly.participated.in.any.of.the.following.collaborative.groups.working.on.Puget.Sound.recovery.in.the.past.five.years..Please.check.all.that.apply..The.Puget.Sound.Salmon.Recovery.Council"
G.SRC.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..The.Puget.Sound.Salmon.Recovery.Council"
G.SRC.part<-data.frame(dat1[,(533+off):(539+off)]);colnames(G.SRC.part)<-c("email","meet","event","proj","read","prod","other")     
G.RITT<-dat1$"Have.you.regularly.participated.in.any.of.the.following.collaborative.groups.working.on.Puget.Sound.recovery.in.the.past.five.years..Please.check.all.that.apply..The.Puget.Sound.Recovery.Implementation.Technical.Team"
 G.RITT.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..The.Puget.Sound.Recovery.Implementation.Technical.Team"
G.RITT.part<-data.frame(dat1[,(540+off):(546+off)]);colnames(G.RITT.part)<-c("email","meet","event","proj","read","prod","other")     

G.WLEADS<-dat1$"Have.you.regularly.participated.in.any.of.the.following.collaborative.groups.working.on.Puget.Sound.recovery.in.the.past.five.years..Please.check.all.that.apply..The.Puget.Sound.Watershed.Leads.Group" 
 G.WLEADS.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..The.Puget.Sound.Watershed.Leads.Group"
G.WLEADS.part<-data.frame(dat1[,(547+off):(553+off)]);colnames(G.WLEADS.part)<-c("email","meet","event","proj","read","prod","other")     
G.FED<-dat1$"Have.you.regularly.participated.in.any.of.the.following.collaborative.groups.working.on.Puget.Sound.recovery.in.the.past.five.years..Please.check.all.that.apply..The.Puget.Sound.Federal.Caucus"  
G.FED.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..The.Puget.Sound.Federal.Caucus"
G.FED.part<-data.frame(dat1[,(561+off):(567+off)]);colnames(G.FED.part)<-c("email","meet","event","proj","read","prod","other")    
G.ENVIRO<-dat1$"Have.you.regularly.participated.in.any.of.the.following.collaborative.groups.working.on.Puget.Sound.recovery.in.the.past.five.years..Please.check.all.that.apply..The.Puget.Sound.Environmental.Caucus"
 G.ENVIRO.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..The.Puget.Sound.Environmental.Caucus"
G.ENVIRO.part<-data.frame(dat1[,(568+off):(574+off)]);colnames(G.ENVIRO.part)<-c("email","meet","event","proj","read","prod","other")    

G.PSEMP<-dat1$"Have.you.regularly.participated.in.any.of.the.following.collaborative.groups.working.on.Puget.Sound.recovery.in.the.past.five.years..Please.check.all.that.apply..The.Puget.Sound.Ecosystem.Monitoring.Program..PSEMP..Steering.Committee"
 G.PSEMP.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..The.Puget.Sound.Ecosystem.Monitoring.Program..PSEMP..Steering.Committee"
G.PSEMP.part<-data.frame(dat1[,(575+off):(581+off)]);colnames(G.PSEMP.part)<-c("email","meet","event","proj","read","prod","other")
G.PSI<-dat1$"Have.you.regularly.participated.in.any.of.the.following.collaborative.groups.working.on.Puget.Sound.recovery.in.the.past.five.years..Please.check.all.that.apply..The.Puget.Sound.Institute" 
G.PSI.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..The.Puget.Sound.Institute"
G.PSI.part<-data.frame(dat1[,(589+off):(595+off)]);colnames(G.PSI.part)<-c("email","meet","event","proj","read","prod","other")

G.PSNERP<-dat1$ "Have.you.regularly.participated.in.any.of.the.following.collaborative.groups.working.on.Puget.Sound.recovery.in.the.past.five.years..Please.check.all.that.apply..The.Puget.Sound.Nearshore.Restoration.Project..PSNERP."
G.PSNERP.reason<-data2$ "Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..The.Puget.Sound.Nearshore.Restoration.Project..PSNERP."                    
G.PSNERP.part<-data.frame(dat1[,(596+off):(602+off)]);colnames(G.PSNERP.part)<-c("email","meet","event","proj","read","prod","other")
                
G.ECO.HOOD<-dat1$"Which.local.ECO.Net.group.s..have.you.participated.in.within.the.last.five.years...Please.check.all.that.apply...Hood.Canal.ECO.Net"                         
G.ECO.HOOD.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..Hood.Canal.ECO.Net"    
G.ECO.HOOD.part<-data.frame(dat1[,(617+off):(623+off)]);colnames(G.ECO.HOOD.part)<-c("email","meet","event","proj","read","prod","other")                                                                                
G.ECO.KING<-dat1$"Which.local.ECO.Net.group.s..have.you.participated.in.within.the.last.five.years...Please.check.all.that.apply...King.ECO.Net"            
G.ECO.KING.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..King.ECO.Net"   
G.ECO.KING.part<-data.frame(dat1[,(624+off):(630+off)]);colnames(G.ECO.KING.part)<-c("email","meet","event","proj","read","prod","other")     
G.ECO.KITSAP<-dat1$"Which.local.ECO.Net.group.s..have.you.participated.in.within.the.last.five.years...Please.check.all.that.apply...Kitsap.ECO.Net"  
G.ECO.KITSAP.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..Kitsap.ECO.Net"   
G.ECO.KITSAP.part<-data.frame(dat1[,(631+off):(637+off)]);colnames(G.ECO.KITSAP.part)<-c("email","meet","event","proj","read","prod","other")     
G.ECO.MASON<-dat1$"Which.local.ECO.Net.group.s..have.you.participated.in.within.the.last.five.years...Please.check.all.that.apply...Mason.ECO.net"  
G.ECO.MASON.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..Mason.ECO.net"  
G.ECO.MASON.part<-data.frame(dat1[,(638+off):(644+off)]);colnames(G.ECO.MASON.part)<-c("email","meet","event","proj","read","prod","other")       
G.ECO.PIERCE<-dat1$"Which.local.ECO.Net.group.s..have.you.participated.in.within.the.last.five.years...Please.check.all.that.apply...Pierce.ECO.Net"  
G.ECO.PIERCE.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..Pierce.ECO.Net"   
G.ECO.PIERCE.part<-data.frame(dat1[,(645+off):(651+off)]);colnames(G.ECO.PIERCE.part)<-c("email","meet","event","proj","read","prod","other")     
G.ECO.SJ<-dat1$"Which.local.ECO.Net.group.s..have.you.participated.in.within.the.last.five.years...Please.check.all.that.apply...San.Juan.ECO.Net"
G.ECO.SJ.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..San.Juan.ECO.Net"   
G.ECO.SJ.part<-data.frame(dat1[,(652+off):(658+off)]);colnames(G.ECO.SJ.part)<-c("email","meet","event","proj","read","prod","other")     
G.ECO.SKAGIT<-dat1$"Which.local.ECO.Net.group.s..have.you.participated.in.within.the.last.five.years...Please.check.all.that.apply...Skagit.ECO.Net" 
G.ECO.SKAGIT.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..Skagit.ECO.Net"   
G.ECO.SKAGIT.part<-data.frame(dat1[,(659+off):(665+off)]);colnames(G.ECO.SKAGIT.part)<-c("email","meet","event","proj","read","prod","other")     
G.ECO.SNOHO<-dat1$"Which.local.ECO.Net.group.s..have.you.participated.in.within.the.last.five.years...Please.check.all.that.apply...Snohomish.Camano.ECO.Net"
G.ECO.SNOHO.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..Snohomish.Camano.ECO.Net"  
G.ECO.SNOHO.part<-data.frame(dat1[,(666+off):(672+off)]);colnames(G.ECO.SNOHO.part)<-c("email","meet","event","proj","read","prod","other")      
G.ECO.STRAIT<-dat1$"Which.local.ECO.Net.group.s..have.you.participated.in.within.the.last.five.years...Please.check.all.that.apply...Strait.ECO.Net"   
G.ECO.STRAIT.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..Strait.ECO.Net"    
G.ECO.STRAIT.part<-data.frame(dat1[,(673+off):(679+off)]);colnames(G.ECO.STRAIT.part)<-c("email","meet","event","proj","read","prod","other")           
G.ECO.THURST<-dat1$"Which.local.ECO.Net.group.s..have.you.participated.in.within.the.last.five.years...Please.check.all.that.apply...Thurston.ECO.Net"
G.ECO.THURST.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..Thurston.ECO.Net"   
G.ECO.THURST.part<-data.frame(dat1[,(680+off):(686+off)]);colnames(G.ECO.THURST.part)<-c("email","meet","event","proj","read","prod","other")             
G.ECO.WHAT<-dat1$"Which.local.ECO.Net.group.s..have.you.participated.in.within.the.last.five.years...Please.check.all.that.apply...Whatcom.ECO.Net"    
G.ECO.WHAT.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..Whatcom.ECO.Net"   
G.ECO.WHAT.part<-data.frame(dat1[,(687+off):(693+off)]);colnames(G.ECO.WHAT.part)<-c("email","meet","event","proj","read","prod","other")          
G.ECO.WHID<-dat1$"Which.local.ECO.Net.group.s..have.you.participated.in.within.the.last.five.years...Please.check.all.that.apply...Whidbey.ECO.Net"  
G.ECO.WHID.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..Whidbey.ECO.Net"   
G.ECO.WHID.part<-data.frame(dat1[,(694+off):(700+off)]);colnames(G.ECO.WHID.part)<-c("email","meet","event","proj","read","prod","other")   
        
G.ECM.BIRD<-dat1$"Which.Puget.Sound.Ecosystem.Monitoring.Program..PSEMP..work.group.s..have.you.participated.in.within.the.last.five.years..Please.check.all.that.apply..ECM.Birds.and.Mammals"    
G.ECM.BIRD.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..ECM.Birds.and.Mammals" 
G.ECM.BIRD.part<-data.frame(dat1[,(708+off):(714+off)]);colnames(G.ECM.BIRD.part)<-c("email","meet","event","proj","read","prod","other")   

G.ECM.FRESH<-dat1$"Which.Puget.Sound.Ecosystem.Monitoring.Program..PSEMP..work.group.s..have.you.participated.in.within.the.last.five.years..Please.check.all.that.apply..ECM.Freshwater"  
G.ECM.FRESH.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..ECM.Freshwater"     
G.ECM.FRESH.part<-data.frame(dat1[,(715+off):(721+off)]);colnames(G.ECM.FRESH.part)<-c("email","meet","event","proj","read","prod","other")        
G.ECM.FOOD<-dat1$ "Which.Puget.Sound.Ecosystem.Monitoring.Program..PSEMP..work.group.s..have.you.participated.in.within.the.last.five.years..Please.check.all.that.apply..ECM.Forage.Fish.Food.Webs"
G.ECM.FOOD.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..ECM.Forage.Fish.Food.Webs" 
G.ECM.FOOD.part<-data.frame(dat1[,(722+off):(728+off)]);colnames(G.ECM.FOOD.part)<-c("email","meet","event","proj","read","prod","other")   
G.ECM.MARINE<-dat1$"Which.Puget.Sound.Ecosystem.Monitoring.Program..PSEMP..work.group.s..have.you.participated.in.within.the.last.five.years..Please.check.all.that.apply..ECM.Marine.Waters"  
G.ECM.MARINE.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..ECM.Marine.Waters" 
G.ECM.MARINE.part<-data.frame(dat1[,(729+off):(735+off)]);colnames(G.ECM.MARINE.part)<-c("email","meet","event","proj","read","prod","other")        
G.ECM.MODEL<-dat1$"Which.Puget.Sound.Ecosystem.Monitoring.Program..PSEMP..work.group.s..have.you.participated.in.within.the.last.five.years..Please.check.all.that.apply..ECM.Modeling"   
G.ECM.MODEL.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..ECM.Modeling"  
G.ECM.MODEL.part<-data.frame(dat1[,(736+off):(742+off)]);colnames(G.ECM.MODEL.part)<-c("email","meet","event","proj","read","prod","other")   
G.ECM.NEAR<-dat1$"Which.Puget.Sound.Ecosystem.Monitoring.Program..PSEMP..work.group.s..have.you.participated.in.within.the.last.five.years..Please.check.all.that.apply..ECM.Nearshore"   
G.ECM.NEAR.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..ECM.Nearshore" 
G.ECM.NEAR.part<-data.frame(dat1[,(743+off):(749+off)]);colnames(G.ECM.NEAR.part)<-c("email","meet","event","proj","read","prod","other")   
G.ECM.SALM<-dat1$"Which.Puget.Sound.Ecosystem.Monitoring.Program..PSEMP..work.group.s..have.you.participated.in.within.the.last.five.years..Please.check.all.that.apply..ECM.Salmon"  
G.ECM.SALM.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..ECM.Salmon"    
G.ECM.SALM.part<-data.frame(dat1[,(750+off):(756+off)]);colnames(G.ECM.SALM.part)<-c("email","meet","event","proj","read","prod","other")    
G.ECM.STORM<-dat1$"Which.Puget.Sound.Ecosystem.Monitoring.Program..PSEMP..work.group.s..have.you.participated.in.within.the.last.five.years..Please.check.all.that.apply..ECM.Stormwater" 
G.ECM.STORM.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..ECM.Stormwater"  
G.ECM.STORM.part<-data.frame(dat1[,(757+off):(763+off)]);colnames(G.ECM.STORM.part)<-c("email","meet","event","proj","read","prod","other")   
G.ECM.TOXIC<-dat1$"Which.Puget.Sound.Ecosystem.Monitoring.Program..PSEMP..work.group.s..have.you.participated.in.within.the.last.five.years..Please.check.all.that.apply..ECM.Toxics" 
G.ECM.TOXIC.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..ECM.Toxics"
G.ECM.TOXIC.part<-data.frame(dat1[,(764+off):(770+off)]);colnames(G.ECM.TOXIC.part)<-c("email","meet","event","proj","read","prod","other")   


G.MRC.CLALLAM<-dat1$"Which.Marine.Resource.Committee.s..have.you.participated.in.within.the.last.five.years..Please.check.all.that.apply..Clallam.County.Marine.Resource.Committee"   
G.MRC.CLALLAM.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..Clallam.County.Marine.Resource.Committee"
G.MRC.CLALLAM.part<-data.frame(dat1[,(778+off):(784+off)]);colnames(G.MRC.CLALLAM.part)<-c("email","meet","event","proj","read","prod","other") 
        
G.MRC.ISLAND<-dat1$"Which.Marine.Resource.Committee.s..have.you.participated.in.within.the.last.five.years..Please.check.all.that.apply..Island.County.Marine.Resource.Committee"      
G.MRC.ISLAND.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..Island.County.Marine.Resource.Committee"  
G.MRC.ISLAND.part<-data.frame(dat1[,(785+off):(791+off)]);colnames(G.MRC.ISLAND.part)<-c("email","meet","event","proj","read","prod","other") 
    
G.MRC.JEFF<-dat1$"Which.Marine.Resource.Committee.s..have.you.participated.in.within.the.last.five.years..Please.check.all.that.apply..Jefferson.County.Marine.Resource.Committee"   
G.MRC.JEFF.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..Jefferson.County.Marine.Resource.Committee"  
G.MRC.JEFF.part<-data.frame(dat1[,(792+off):(798+off)]);colnames(G.MRC.JEFF.part)<-c("email","meet","event","proj","read","prod","other") 
    
G.MRC.SJ<-dat1$"Which.Marine.Resource.Committee.s..have.you.participated.in.within.the.last.five.years..Please.check.all.that.apply..San.Juan.County.Marine.Resource.Committee" 
G.MRC.SJ.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..San.Juan.County.Marine.Resource.Committee"
G.MRC.SJ.part<-data.frame(dat1[,(799+off):(805+off)]);colnames(G.MRC.SJ.part)<-c("email","meet","event","proj","read","prod","other") 

G.MRC.SKAGIT<-dat1$"Which.Marine.Resource.Committee.s..have.you.participated.in.within.the.last.five.years..Please.check.all.that.apply..Skagit.County.Marine.Resource.Committee"   
G.MRC.SKAGIT.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..Skagit.County.Marine.Resource.Committee"
G.MRC.SKAGIT.part<-data.frame(dat1[,(806+off):(812+off)]);colnames(G.MRC.SKAGIT.part)<-c("email","meet","event","proj","read","prod","other") 

G.MRC.SNOHO<-dat1$"Which.Marine.Resource.Committee.s..have.you.participated.in.within.the.last.five.years..Please.check.all.that.apply..Snohomish.County.Marine.Resource.Committee"
G.MRC.SNOHO.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..Snohomish.County.Marine.Resource.Committee"
G.MRC.SNOHO.part<-data.frame(dat1[,(813+off):(819+off)]);colnames(G.MRC.SNOHO.part)<-c("email","meet","event","proj","read","prod","other") 

G.MRC.WHAT<-dat1$"Which.Marine.Resource.Committee.s..have.you.participated.in.within.the.last.five.years..Please.check.all.that.apply..Whatcom.County.Marine.Resource.Committee"  
G.MRC.WHAT.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..Whatcom.County.Marine.Resource.Committee"
G.MRC.WHAT.part<-data.frame(dat1[,(820+off):(826+off)]);colnames(G.MRC.WHAT.part)<-c("email","meet","event","proj","read","prod","other") 
  
G.LOC.GREEN<-dat1$"Which.local.salmon.recovery.or.watershed.group.s..have.you.participated.in.within.the.last.five.years..Please.check.all.that.apply..Green.Duwamish.Watershed"   
G.LOC.GREEN.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..Green.Duwamish.Watershed"
G.LOC.GREEN.part<-data.frame(dat1[,(834+off):(840+off)]);colnames(G.LOC.GREEN.part)<-c("email","meet","event","proj","read","prod","other") 
G.LOC.HOOD<-dat1$"Which.local.salmon.recovery.or.watershed.group.s..have.you.participated.in.within.the.last.five.years..Please.check.all.that.apply..Hood.Canal.Watershed" 
G.LOC.HOOD.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..Hood.Canal.Watershed"   
G.LOC.HOOD.part<-data.frame(dat1[,(841+off):(847+off)]);colnames(G.LOC.HOOD.part)<-c("email","meet","event","proj","read","prod","other")                  
G.LOC.ISLAND<-dat1$"Which.local.salmon.recovery.or.watershed.group.s..have.you.participated.in.within.the.last.five.years..Please.check.all.that.apply..Island.Watershed"    
G.LOC.ISLAND.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..Island.Watershed"     
G.LOC.ISLAND.part<-data.frame(dat1[,(848+off):(854+off)]);colnames(G.LOC.ISLAND.part)<-c("email","meet","event","proj","read","prod","other")                 
G.LOC.CEDAR<-dat1$ "Which.local.salmon.recovery.or.watershed.group.s..have.you.participated.in.within.the.last.five.years..Please.check.all.that.apply..Lake.Washington.Cedar.Sammamish.Watershed"
G.LOC.CEDAR.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..Lake.Washington.Cedar.Sammamish.Watershed"
G.LOC.CEDAR.part<-data.frame(dat1[,(855+off):(861+off)]);colnames(G.LOC.CEDAR.part)<-c("email","meet","event","proj","read","prod","other") 
G.LOC.NISQ<-dat1$"Which.local.salmon.recovery.or.watershed.group.s..have.you.participated.in.within.the.last.five.years..Please.check.all.that.apply..Nisqually.River.Watershed"  
G.LOC.NISQ.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..Nisqually.River.Watershed"       
G.LOC.NISQ.part<-data.frame(dat1[,(862+off):(868+off)]);colnames(G.LOC.NISQ.part)<-c("email","meet","event","proj","read","prod","other")         
G.LOC.NOOK<-dat1$"Which.local.salmon.recovery.or.watershed.group.s..have.you.participated.in.within.the.last.five.years..Please.check.all.that.apply..Nooksack.Watershed"
G.LOC.NOOK.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..Nooksack.Watershed" 
G.LOC.NOOK.part<-data.frame(dat1[,(869+off):(875+off)]);colnames(G.LOC.NOOK.part)<-c("email","meet","event","proj","read","prod","other")     

G.LOC.NOLY<-dat1$"Which.local.salmon.recovery.or.watershed.group.s..have.you.participated.in.within.the.last.five.years..Please.check.all.that.apply..North.Olympic.Peninsula.Watershed"         
G.LOC.NOLY.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..North.Olympic.Peninsula.Watershed"    
G.LOC.NOLY.part<-data.frame(dat1[,(876+off):(882+off)]);colnames(G.LOC.NOLY.part)<-c("email","meet","event","proj","read","prod","other") 
G.LOC.PUY<-dat1$"Which.local.salmon.recovery.or.watershed.group.s..have.you.participated.in.within.the.last.five.years..Please.check.all.that.apply..Puyallup.White...Chambers.Clover.Watersheds"
G.LOC.PUY.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..Puyallup.White...Chambers.Clover.Watersheds"
G.LOC.PUY.part<-data.frame(dat1[,(883+off):(889+off)]);colnames(G.LOC.PUY.part)<-c("email","meet","event","proj","read","prod","other") 
G.LOC.SJ<-dat1$"Which.local.salmon.recovery.or.watershed.group.s..have.you.participated.in.within.the.last.five.years..Please.check.all.that.apply..San.Juan.Watershed"   
G.LOC.SJ.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..San.Juan.Watershed" 
G.LOC.SJ.part<-data.frame(dat1[,(890+off):(896+off)]);colnames(G.LOC.SJ.part)<-c("email","meet","event","proj","read","prod","other")                       
G.LOC.SKAGIT<-dat1$"Which.local.salmon.recovery.or.watershed.group.s..have.you.participated.in.within.the.last.five.years..Please.check.all.that.apply..Skagit.Watershed"      
G.LOC.SKAGIT.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..Skagit.Watershed"    
G.LOC.SKAGIT.part<-data.frame(dat1[,(897+off):(903+off)]);colnames(G.LOC.SKAGIT.part)<-c("email","meet","event","proj","read","prod","other")                   
G.LOC.SNOHO<-dat1$"Which.local.salmon.recovery.or.watershed.group.s..have.you.participated.in.within.the.last.five.years..Please.check.all.that.apply..Snohomish.Watershed" 
G.LOC.SNOHO.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..Snohomish.Watershed" 
 G.LOC.SNOHO.part<-data.frame(dat1[,(904+off):(910+off)]);colnames(G.LOC.SNOHO.part)<-c("email","meet","event","proj","read","prod","other") 
G.LOC.SOUTH<-dat1$"Which.local.salmon.recovery.or.watershed.group.s..have.you.participated.in.within.the.last.five.years..Please.check.all.that.apply..South.Sound.Watersheds"   
G.LOC.SOUTH.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..South.Sound.Watersheds" 
G.LOC.SOUTH.part<-data.frame(dat1[,(911+off):(917+off)]);colnames(G.LOC.SOUTH.part)<-c("email","meet","event","proj","read","prod","other") 
G.LOC.STILL<-dat1$"Which.local.salmon.recovery.or.watershed.group.s..have.you.participated.in.within.the.last.five.years..Please.check.all.that.apply..Stillaguamish.Watershed"  
G.LOC.STILL.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..Stillaguamish.Watershed" 
G.LOC.STILL.part<-data.frame(dat1[,(918+off):(924+off)]);colnames(G.LOC.STILL.part)<-c("email","meet","event","proj","read","prod","other") 
G.LOC.WEST<-dat1$"Which.local.salmon.recovery.or.watershed.group.s..have.you.participated.in.within.the.last.five.years..Please.check.all.that.apply..West.Sound.Watersheds"  
G.LOC.WEST.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..West.Sound.Watersheds"   
G.LOC.WEST.part<-data.frame(dat1[,(925+off):(931+off)]);colnames(G.LOC.WEST.part)<-c("email","meet","event","proj","read","prod","other")

G.LIO.SJWATER<-dat1$"Which.Local.Integrating.Organization.s...LIO..have.you.participated.in.within.the.last.five.years..Please.check.all.that.apply..San.Juan.County.Watershed.LIO"
 G.LIO.SJWATER.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..San.Juan.County.Watershed.LIO"   
 G.LIO.SJWATER.part<-data.frame(dat1[,(939+off):(945+off)]);colnames(G.LIO.SJWATER.part)<-c("email","meet","event","proj","read","prod","other")
        
 G.LIO.NOOKWATER<-dat1$"Which.Local.Integrating.Organization.s...LIO..have.you.participated.in.within.the.last.five.years..Please.check.all.that.apply..Whatcom.County.Nooksack.Watershed.LIO"  
 G.LIO.NOOKWATER.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..Whatcom.County.Nooksack.Watershed.LIO"  
 G.LIO.NOOKWATER.part<-data.frame(dat1[,(946+off):(952+off)]);colnames(G.LIO.NOOKWATER.part)<-c("email","meet","event","proj","read","prod","other")         
G.LIO.ISLANDWATER<-dat1$"Which.Local.Integrating.Organization.s...LIO..have.you.participated.in.within.the.last.five.years..Please.check.all.that.apply..Island.County.Watershed.LIO"
 G.LIO.ISLANDWATER.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..Island.County.Watershed.LIO"  
 G.LIO.ISLANDWATER.part<-data.frame(dat1[,(953+off):(959+off)]);colnames(G.LIO.ISLANDWATER.part)<-c("email","meet","event","proj","read","prod","other")                      
G.LIO.SNOHOSTILL<-dat1$ "Which.Local.Integrating.Organization.s...LIO..have.you.participated.in.within.the.last.five.years..Please.check.all.that.apply..Snohomish.and.Stillaguamish.Watersheds.LIO"    
G.LIO.SNOHOSTILL.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..Snohomish.and.Stillaguamish.Watersheds.LIO" 
G.LIO.SNOHOSTILL.part<-data.frame(dat1[,(960+off):(966+off)]);colnames(G.LIO.SNOHOSTILL.part)<-c("email","meet","event","proj","read","prod","other")       
G.LIO.SOUTHCENTRAL<-dat1$"Which.Local.Integrating.Organization.s...LIO..have.you.participated.in.within.the.last.five.years..Please.check.all.that.apply..South.Central.Action.Area..WRIA.8..9..10..LIO"    
G.LIO.SOUTHCENTRAL.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..South.Central.Action.Area..WRIA.8..9..10..LIO"  
G.LIO.SOUTHCENTRAL.part<-data.frame(dat1[,(967+off):(973+off)]);colnames(G.LIO.SOUTHCENTRAL.part)<-c("email","meet","event","proj","read","prod","other")
G.LIO.SOUTHSOUND<-dat1$"Which.Local.Integrating.Organization.s...LIO..have.you.participated.in.within.the.last.five.years..Please.check.all.that.apply..South.Sound.Action.Area..WRIA.11..12..13..14..LIO"
G.LIO.SOUTHSOUND.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..South.Sound.Action.Area..WRIA.11..12..13..14..LIO"  
G.LIO.SOUTHSOUND.part<-data.frame(dat1[,(974+off):(980+off)]);colnames(G.LIO.SOUTHSOUND.part)<-c("email","meet","event","proj","read","prod","other")
G.LIO.HOOD<-dat1$"Which.Local.Integrating.Organization.s...LIO..have.you.participated.in.within.the.last.five.years..Please.check.all.that.apply..Hood.Canal.Action.Area.LIO"    
G.LIO.HOOD.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..Hood.Canal.Action.Area.LIO"   
G.LIO.HOOD.part<-data.frame(dat1[,(981+off):(987+off)]);colnames(G.LIO.HOOD.part)<-c("email","meet","event","proj","read","prod","other")                                                
G.LIO.STRAIT<-dat1$"Which.Local.Integrating.Organization.s...LIO..have.you.participated.in.within.the.last.five.years..Please.check.all.that.apply..Strait.Action.Area.LIO"   
G.LIO.STRAIT.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..Strait.Action.Area.LIO"    
G.LIO.STRAIT.part<-data.frame(dat1[,(988+off):(994+off)]);colnames(G.LIO.STRAIT.part)<-c("email","meet","event","proj","read","prod","other")                                                    
G.LIO.NORTHCENTRAL<-dat1$"Which.Local.Integrating.Organization.s...LIO..have.you.participated.in.within.the.last.five.years..Please.check.all.that.apply..North.Central.Action.Area..WRIA.15..LIO"  
G.LIO.NORTHCENTRAL.reason<-data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..North.Central.Action.Area..WRIA.15..LIO"    
G.LIO.NORTHCENTRAL.part<-data.frame(dat1[,(995+off):(1001+off)]);colnames(G.LIO.NORTHCENTRAL.part)<-c("email","meet","event","proj","read","prod","other")  

  
              

G.OTHER<-dat1$"Have.you.regularly.participated.in.any.other.collaborative.group.s..focused.on.environmental.recovery.in.the.Puget.Sound.region.within.the.last.five.years.other.than.those.that.you.previously.identified...Yes"   

G.OTHER.1<-dat1$"Please.specify.the.collaborative.groups.focused.on.environmental.recovery.in.the.Puget.Sound.region.in.which.you.have.regularly.participated.within.the.last.five.years..other.than.those.that.you.previously.identified..You.may.list.up.to.ten.."  
G.OTHER.1.reason<-
data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..1"
G.OTHER.1.part<-data.frame(dat1[,(1009+off):(1015+off)]);colnames(G.OTHER.1.part)<-c("email","meet","event","proj","read","prod","other")  

G.OTHER.2<-dat1$"Please.specify.the.collaborative.groups.focused.on.environmental.recovery.in.the.Puget.Sound.region.in.which.you.have.regularly.participated.within.the.last.five.years..other.than.those.that.you.previously.identified..You.may.list.up.to.ten...1"
G.OTHER.2.reason<-
data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in."
G.OTHER.2.part<-data.frame(dat1[,(1016+off):(1022+off)]);colnames(G.OTHER.2.part)<-c("email","meet","event","proj","read","prod","other")  

G.OTHER.3<-dat1$"Please.specify.the.collaborative.groups.focused.on.environmental.recovery.in.the.Puget.Sound.region.in.which.you.have.regularly.participated.within.the.last.five.years..other.than.those.that.you.previously.identified..You.may.list.up.to.ten...2"
G.OTHER.3.reason<-
data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..2"
G.OTHER.3.part<-data.frame(dat1[,(1023+off):(1029+off)]);colnames(G.OTHER.3.part)<-c("email","meet","event","proj","read","prod","other")  

G.OTHER.4<-dat1$"Please.specify.the.collaborative.groups.focused.on.environmental.recovery.in.the.Puget.Sound.region.in.which.you.have.regularly.participated.within.the.last.five.years..other.than.those.that.you.previously.identified..You.may.list.up.to.ten...3"
G.OTHER.4.reason<-
data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..3"
G.OTHER.4.part<-data.frame(dat1[,(1030+off):(1036+off)]);colnames(G.OTHER.4.part)<-c("email","meet","event","proj","read","prod","other")  

G.OTHER.5<-dat1$"Please.specify.the.collaborative.groups.focused.on.environmental.recovery.in.the.Puget.Sound.region.in.which.you.have.regularly.participated.within.the.last.five.years..other.than.those.that.you.previously.identified..You.may.list.up.to.ten...4"
G.OTHER.5.reason<-
data2$"Please.describe.the.reason.s..that.you.have.been.involved.in.the.collaborative.group.s..that.you.previously.identified..Please.select.one.answer.for.each.group.you.have.participated.in..4"
G.OTHER.5.part<-data.frame(dat1[,(1037+off):(1043+off)]);colnames(G.OTHER.5.part)<-c("email","meet","event","proj","read","prod","other")  

colnames(dat1)[(995+off):(1001+off)]
Group.Member.Dat<-data.frame(cbind(ID,
G.LC,
G.ECB,
G.SCIENCE,
G.SOCSCI,
G.SRC,
G.RITT,
G.WLEADS,
G.FED,
G.ENVIRO,
G.PSEMP,
G.PSI,
G.PSNERP,
G.ECO.HOOD,
G.ECO.KING,
G.ECO.KITSAP,
G.ECO.MASON,
G.ECO.PIERCE,
G.ECO.SJ,
G.ECO.SKAGIT,
G.ECO.SNOHO,
G.ECO.STRAIT,
G.ECO.THURST,
G.ECO.WHAT,
G.ECO.WHID,
G.ECM.BIRD,
G.ECM.FRESH,
G.ECM.FOOD,
G.ECM.MARINE,
G.ECM.MODEL,
G.ECM.NEAR,
G.ECM.SALM,
G.ECM.STORM,
G.ECM.TOXIC,
G.MRC.CLALLAM,
G.MRC.ISLAND,
G.MRC.JEFF,
G.MRC.SJ,
G.MRC.SKAGIT,
G.MRC.SNOHO,
G.MRC.WHAT,
G.LOC.GREEN,
G.LOC.HOOD,
G.LOC.ISLAND,
G.LOC.CEDAR,
G.LOC.NISQ,
G.LOC.NOOK,
G.LOC.NOLY,
G.LOC.PUY,
G.LOC.SJ,
G.LOC.SKAGIT,
G.LOC.SNOHO,
G.LOC.SOUTH,
G.LOC.STILL,
G.LOC.WEST,
G.LIO.SJWATER,
G.LIO.NOOKWATER,
G.LIO.ISLANDWATER,
G.LIO.SNOHOSTILL,
G.LIO.SOUTHCENTRAL,
G.LIO.SOUTHSOUND,
G.LIO.HOOD,
G.LIO.STRAIT,
G.LIO.NORTHCENTRAL,
as.character(G.OTHER.1),
as.character(G.OTHER.2),
as.character(G.OTHER.3),
as.character(G.OTHER.4),
as.character(G.OTHER.5)))


Group.Reason.Dat<-data.frame(cbind(ID,
as.character(G.LC.reason),
as.character(G.ECB.reason),
as.character(G.SCIENCE.reason),
as.character(G.SOCSCI.reason),
as.character(G.SRC.reason),
as.character(G.RITT.reason),
as.character(G.WLEADS.reason),
as.character(G.FED.reason),
as.character(G.ENVIRO.reason),
as.character(G.PSEMP.reason),
as.character(G.PSI.reason),
as.character(G.PSNERP.reason),
as.character(G.ECO.HOOD.reason),
as.character(G.ECO.KING.reason),
as.character(G.ECO.KITSAP.reason),
as.character(G.ECO.MASON.reason),
as.character(G.ECO.PIERCE.reason),
as.character(G.ECO.SJ.reason),
as.character(G.ECO.SKAGIT.reason),
as.character(G.ECO.SNOHO.reason),
as.character(G.ECO.STRAIT.reason),
as.character(G.ECO.THURST.reason),
as.character(G.ECO.WHAT.reason),
as.character(G.ECO.WHID.reason),
as.character(G.ECM.BIRD.reason),
as.character(G.ECM.FRESH.reason),
as.character(G.ECM.FOOD.reason),
as.character(G.ECM.MARINE.reason),
as.character(G.ECM.MODEL.reason),
as.character(G.ECM.NEAR.reason),
as.character(G.ECM.SALM.reason),
as.character(G.ECM.STORM.reason),
as.character(G.ECM.TOXIC.reason),
as.character(G.MRC.CLALLAM.reason),
as.character(G.MRC.ISLAND.reason),
as.character(G.MRC.JEFF.reason),
as.character(G.MRC.SJ.reason),
as.character(G.MRC.SKAGIT.reason),
as.character(G.MRC.SNOHO.reason),
as.character(G.MRC.WHAT.reason),
as.character(G.LOC.GREEN.reason),
as.character(G.LOC.HOOD.reason),
as.character(G.LOC.ISLAND.reason),
as.character(G.LOC.CEDAR.reason),
as.character(G.LOC.NISQ.reason),
as.character(G.LOC.NOOK.reason),
as.character(G.LOC.NOLY.reason),
as.character(G.LOC.PUY.reason),
as.character(G.LOC.SJ.reason),
as.character(G.LOC.SKAGIT.reason),
as.character(G.LOC.SNOHO.reason),
as.character(G.LOC.SOUTH.reason),
as.character(G.LOC.STILL.reason),
as.character(G.LOC.WEST.reason),
as.character(G.LIO.SJWATER.reason),
as.character(G.LIO.NOOKWATER.reason),
as.character(G.LIO.ISLANDWATER.reason),
as.character(G.LIO.SNOHOSTILL.reason),
as.character(G.LIO.SOUTHCENTRAL.reason),
as.character(G.LIO.SOUTHSOUND.reason),
as.character(G.LIO.HOOD.reason),
as.character(G.LIO.STRAIT.reason),
as.character(G.LIO.NORTHCENTRAL.reason),
as.character(G.OTHER.1.reason),
as.character(G.OTHER.2.reason),
as.character(G.OTHER.3.reason),
as.character(G.OTHER.4.reason),
as.character(G.OTHER.5.reason)))



Group.Part.Dat<-data.frame(cbind(ID,
G.LC.part,
G.ECB.part,
G.SCIENCE.part,
G.SOCSCI.part,
G.SRC.part,
G.RITT.part,
G.WLEADS.part,
G.FED.part,
G.ENVIRO.part,
G.PSEMP.part,
G.PSI.part,
G.PSNERP.part,
G.ECO.HOOD.part,
G.ECO.KING.part,
G.ECO.KITSAP.part,
G.ECO.MASON.part,
G.ECO.PIERCE.part,
G.ECO.SJ.part,
G.ECO.SKAGIT.part,
G.ECO.SNOHO.part,
G.ECO.STRAIT.part,
G.ECO.THURST.part,
G.ECO.WHAT.part,
G.ECO.WHID.part,
G.ECM.BIRD.part,
G.ECM.FRESH.part,
G.ECM.FOOD.part,
G.ECM.MARINE.part,
G.ECM.MODEL.part,
G.ECM.NEAR.part,
G.ECM.SALM.part,
G.ECM.STORM.part,
G.ECM.TOXIC.part,
G.MRC.CLALLAM.part,
G.MRC.ISLAND.part,
G.MRC.JEFF.part,
G.MRC.SJ.part,
G.MRC.SKAGIT.part,
G.MRC.SNOHO.part,
G.MRC.WHAT.part,
G.LOC.GREEN.part,
G.LOC.HOOD.part,
G.LOC.ISLAND.part,
G.LOC.CEDAR.part,
G.LOC.NISQ.part,
G.LOC.NOOK.part,
G.LOC.NOLY.part,
G.LOC.PUY.part,
G.LOC.SJ.part,
G.LOC.SKAGIT.part,
G.LOC.SNOHO.part,
G.LOC.SOUTH.part,
G.LOC.STILL.part,
G.LOC.WEST.part,
G.LIO.SJWATER.part,
G.LIO.NOOKWATER.part,
G.LIO.ISLANDWATER.part,
G.LIO.SNOHOSTILL.part,
G.LIO.SOUTHCENTRAL.part,
G.LIO.SOUTHSOUND.part,
G.LIO.HOOD.part,
G.LIO.STRAIT.part,
G.LIO.NORTHCENTRAL.part,
(G.OTHER.1.part),
(G.OTHER.2.part),
(G.OTHER.3.part),
(G.OTHER.4.part),
(G.OTHER.5.part)))

head(Group.Part.Dat)


colnames(Group.Member.Dat)<-c("ID",
"G.LC",
"G.ECB",
"G.SCIENCE",
"G.SOCSCI",
"G.SRC",
"G.RITT",
"G.WLEADS",
"G.FED",
"G.ENVIRO",
"G.PSEMP",
"G.PSI",
"G.PSNERP",
"G.ECO.HOOD",
"G.ECO.KING",
"G.ECO.KITSAP",
"G.ECO.MASON",
"G.ECO.PIERCE",
"G.ECO.SJ",
"G.ECO.SKAGIT",
"G.ECO.SNOHO",
"G.ECO.STRAIT",
"G.ECO.THURST",
"G.ECO.WHAT",
"G.ECO.WHID",
"G.ECM.BIRD",
"G.ECM.FRESH",
"G.ECM.FOOD",
"G.ECM.MARINE",
"G.ECM.MODEL",
"G.ECM.NEAR",
"G.ECM.SALM",
"G.ECM.STORM",
"G.ECM.TOXIC",
"G.MRC.CLALLAM",
"G.MRC.ISLAND",
"G.MRC.JEFF",
"G.MRC.SJ",
"G.MRC.SKAGIT",
"G.MRC.SNOHO",
"G.MRC.WHAT",
"G.LOC.GREEN",
"G.LOC.HOOD",
"G.LOC.ISLAND",
"G.LOC.CEDAR",
"G.LOC.NISQ",
"G.LOC.NOOK",
"G.LOC.NOLY",
"G.LOC.PUY",
"G.LOC.SJ",
"G.LOC.SKAGIT",
"G.LOC.SNOHO",
"G.LOC.SOUTH",
"G.LOC.STILL",
"G.LOC.WEST",
"G.LIO.SJWATER",
"G.LIO.NOOKWATER",
"G.LIO.ISLANDWATER",
"G.LIO.SNOHOSTILL",
"G.LIO.SOUTHCENTRAL",
"G.LIO.SOUTHSOUND",
"G.LIO.HOOD",
"G.LIO.STRAIT",
"G.LIO.NORTHCENTRAL",
"G.OTHER.1",
"G.OTHER.2",
"G.OTHER.3",
"G.OTHER.4",
"G.OTHER.5")

colnames(Group.Reason.Dat)<-c("ID",
"G.LC.reason",
"G.ECB.reason",
"G.SCIENCE.reason",
"G.SOCSCI.reason",
"G.SRC.reason",
"G.RITT.reason",
"G.WLEADS.reason",
"G.FED.reason",
"G.ENVIRO.reason",
"G.PSEMP.reason",
"G.PSI.reason",
"G.PSNERP.reason",
"G.ECO.HOOD.reason",
"G.ECO.KING.reason",
"G.ECO.KITSAP.reason",
"G.ECO.MASON.reason",
"G.ECO.PIERCE.reason",
"G.ECO.SJ.reason",
"G.ECO.SKAGIT.reason",
"G.ECO.SNOHO.reason",
"G.ECO.STRAIT.reason",
"G.ECO.THURST.reason",
"G.ECO.WHAT.reason",
"G.ECO.WHID.reason",
"G.ECM.BIRD.reason",
"G.ECM.FRESH.reason",
"G.ECM.FOOD.reason",
"G.ECM.MARINE.reason",
"G.ECM.MODEL.reason",
"G.ECM.NEAR.reason",
"G.ECM.SALM.reason",
"G.ECM.STORM.reason",
"G.ECM.TOXIC.reason",
"G.MRC.CLALLAM.reason",
"G.MRC.ISLAND.reason",
"G.MRC.JEFF.reason",
"G.MRC.SJ.reason",
"G.MRC.SKAGIT.reason",
"G.MRC.SNOHO.reason",
"G.MRC.WHAT.reason",
"G.LOC.GREEN.reason",
"G.LOC.HOOD.reason",
"G.LOC.ISLAND.reason",
"G.LOC.CEDAR.reason",
"G.LOC.NISQ.reason",
"G.LOC.NOOK.reason",
"G.LOC.NOLY.reason",
"G.LOC.PUY.reason",
"G.LOC.SJ.reason",
"G.LOC.SKAGIT.reason",
"G.LOC.SNOHO.reason",
"G.LOC.SOUTH.reason",
"G.LOC.STILL.reason",
"G.LOC.WEST.reason",
"G.LIO.SJWATER.reason",
"G.LIO.NOOKWATER.reason",
"G.LIO.ISLANDWATER.reason",
"G.LIO.SNOHOSTILL.reason",
"G.LIO.SOUTHCENTRAL.reason",
"G.LIO.SOUTHSOUND.reason",
"G.LIO.HOOD.reason",
"G.LIO.STRAIT.reason",
"G.LIO.NORTHCENTRAL.reason",
"G.OTHER.1.reason",
"G.OTHER.2.reason",
"G.OTHER.3.reason",
"G.OTHER.4.reason",
"G.OTHER.5.reason")

colnames(Group.Part.Dat)<-(c("ID",
rep("G.LC.part",7),
rep("G.ECB.part",7),
rep("G.SCIENCE.part",7),
rep("G.SOCSCI.part",7),
rep("G.SRC.part",7),
rep("G.RITT.part",7),
rep("G.WLEADS.part",7),
rep("G.FED.part",7),
rep("G.ENVIRO.part",7),
rep("G.PSEMP.part",7),
rep("G.PSI.part",7),
rep("G.PSNERP.part",7),
rep("G.ECO.HOOD.part",7),
rep("G.ECO.KING.part",7),
rep("G.ECO.KITSAP.part",7),
rep("G.ECO.MASON.part",7),
rep("G.ECO.PIERCE.part",7),
rep("G.ECO.SJ.part",7),
rep("G.ECO.SKAGIT.part",7),
rep("G.ECO.SNOHO.part",7),
rep("G.ECO.STRAIT.part",7),
rep("G.ECO.THURST.part",7),
rep("G.ECO.WHAT.part",7),
rep("G.ECO.WHID.part",7),
rep("G.ECM.BIRD.part",7),
rep("G.ECM.FRESH.part",7),
rep("G.ECM.FOOD.part",7),
rep("G.ECM.MARINE.part",7),
rep("G.ECM.MODEL.part",7),
rep("G.ECM.NEAR.part",7),
rep("G.ECM.SALM.part",7),
rep("G.ECM.STORM.part",7),
rep("G.ECM.TOXIC.part",7),
rep("G.MRC.CLALLAM.part",7),
rep("G.MRC.ISLAND.part",7),
rep("G.MRC.JEFF.part",7),
rep("G.MRC.SJ.part",7),
rep("G.MRC.SKAGIT.part",7),
rep("G.MRC.SNOHO.part",7),
rep("G.MRC.WHAT.part",7),
rep("G.LOC.GREEN.part",7),
rep("G.LOC.HOOD.part",7),
rep("G.LOC.ISLAND.part",7),
rep("G.LOC.CEDAR.part",7),
rep("G.LOC.NISQ.part",7),
rep("G.LOC.NOOK.part",7),
rep("G.LOC.NOLY.part",7),
rep("G.LOC.PUY.part",7),
rep("G.LOC.SJ.part",7),
rep("G.LOC.SKAGIT.part",7),
rep("G.LOC.SNOHO.part",7),
rep("G.LOC.SOUTH.part",7),
rep("G.LOC.STILL.part",7),
rep("G.LOC.WEST.part",7),
rep("G.LIO.SJWATER.part",7),
rep("G.LIO.NOOKWATER.part",7),
rep("G.LIO.ISLANDWATER.part",7),
rep("G.LIO.SNOHOSTILL.part",7),
rep("G.LIO.SOUTHCENTRAL.part",7),
rep("G.LIO.SOUTHSOUND.part",7),
rep("G.LIO.HOOD.part",7),
rep("G.LIO.STRAIT.part",7),
rep("G.LIO.NORTHCENTRAL.part",7),
rep("G.OTHER.1.part",7),
rep("G.OTHER.2.part",7),
rep("G.OTHER.3.part",7),
rep("G.OTHER.4.part",7),
rep("G.OTHER.5.part",7)
))


write.csv(Group.Member.Dat,file="Group.Member.Dat.csv")
write.csv(Group.Reason.Dat,file="Group.Reason.Dat.csv")
write.csv(Group.Part.Dat,file="Group.Part.Dat.csv")
