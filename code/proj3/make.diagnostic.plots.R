
rm(list=ls())
load('code/proj3/modtemp3.RData')
#load('code/proj3/modtemp2.RData')


library(broom)
library(statnet)
library(scales)
library(ggthemes)
library(plyr);library(dplyr)
library(texreg)
library(ggplot2)



###### MAKE COEF PLOT ######
resource = rep(c('fin','tech','hum'),each=2)
model = rep(1:2,3)
modlist = list(mod.fin1,mod.fin2,mod.tech1,mod.tech2,
               mod.hum1,mod.hum2)

temp = lapply(modlist, function(x) data.frame(confint_tidy(x),name=names(x$coef),coef=x$coef))
for (i in 1:length(temp))
{temp[[i]]$resource = resource[i]
temp[[i]]$model = model[i]}

coef = join_all(temp,type='full')
coef = coef %>% filter(name!='edges')
coef = coef %>% filter(name != 'edgecov.nonmembership.matrix')
coef$linet = ifelse(coef$model==1,'solid','dotdash')


coef.labels = data.frame(name = sort(as.character(unique(coef$name))),
                         order = c(3,4,8,6,7,2,5,9,1),
                         label = c('Actor 2-star','Forum 2-star', 'Brokerage', 'Num. Groups',
                                   'Group Members','Participation','Principled Eng.','Ties to Members',
                                   'Years at Org.'),
                         hyp = c('H1A/B','H2A/B','H5','H4A','H4B','','H3','H6',''))
coef = join(coef,coef.labels)

coef.order = coef %>% arrange(order)
# Or use character vectors as lookup tables:
label_var <- c(
  '1' = "Years at Org.",
  '2' = "Participation",
  '3' = "Actor 2-star (H1A/B)",
  '4' = "Forum 2-star (H2/A/B)",
  '5' = "Principled Eng. (H3)",
  '6' = "Num. Groups (H4A)",
  '7' = "Group Members (H4B)",
  '8' = "Brokerage (H5)",
  '9' = "Ties to Members (H6)")

ggplot(coef) + geom_hline(aes(yintercept=0),lty=2,colour="grey") +
  geom_linerange(aes(ymax=conf.high,ymin=conf.low,x=as.factor(model),
                     colour=resource),lwd=8,
                 position = position_dodge(width = 0.5)) + 
  facet_wrap(~order,labeller=labeller(order = label_var)) + 
  theme_tufte() + coord_flip() + 
  scale_x_discrete(name='Model',labels = c('w/ Actor 2-star','w/ Forum 2-star'))+
  scale_colour_colorblind(name='Resource Type',labels=c('Financial','Human','Technical'))+
  theme(legend.position = c(0.9,0.15), axis.ticks.y = element_blank(),
        legend.background = element_rect(fill = alpha('grey80',0.5)),
        strip.text=element_text(size=24),axis.text= element_text(size=24),
        axis.title=element_text(size=24),legend.title = element_text(size=24),
        legend.text=element_text(size=24))



##### Make Table #####


coef.groups = list(" " = 1:3,"H1A/B" = 4,"H2A/B" = 5,"H3" = 6, "H4" = 7:8,"H5" = 9, "H6" = 10)

htmlreg(list(mod.fin1,mod.fin2,mod.hum1,mod.hum2,mod.tech1,mod.tech2),
          reorder.coef = c(1,3,2,4,10,5:9),ci.force =TRUE,stars = numeric(0),
          omit.coef='Non.mem',align.center=TRUE,ci.test=NULL,
          digits=2,leading.zero = TRUE,#groups = coef.groups,
          custom.columns = list("Hypothesis" = c(' ',' ',' ',"H1A/B","H2A/B","H3","H4A","H4B",'H5',"H6")),custom.col.pos = c(2),
        single.row = TRUE,
          no.margin=TRUE,include.aic=FALSE,include.bic = FALSE,include.loglik=FALSE,sideways=TRUE,
          custom.coef.names = c("Edges","Participation","Years at Org.","Actor 2-star","Principled Eng.", "Num. Groups","Group Members","Brokerage","Ties to Members",'Non.mem',"Forum 2-star"),
         custom.model.names = rep(c('w/ Actor 2-star','w/ Forum 2-star'),3))




##### Grab Proc data ####


pfin0 = data.frame(Model = 'M0',
                   true.positive.rate = unlist(gf.fin0$`Tie prediction`$pr.rgraph@x.values),
                   positive.predictive.rate =  unlist(gf.fin0$`Tie prediction`$pr.rgraph@y.values))
pfin1 = data.frame(Model = 'M1',
                   true.positive.rate = unlist(gf.fin1$`Tie prediction`$pr.rgraph@x.values),
                   positive.predictive.rate =  unlist(gf.fin1$`Tie prediction`$pr.rgraph@y.values))
pfin2 = data.frame(Model = 'M2',
                   true.positive.rate = unlist(gf.fin2$`Tie prediction`$pr.rgraph@x.values),
                   positive.predictive.rate =  unlist(gf.fin2$`Tie prediction`$pr.rgraph@y.values))
pfins = join_all(list(pfin0,pfin1,pfin2),type='full')


ptech0 = data.frame(Model = 'M0',
                    true.positive.rate = unlist(gf.tech0$`Tie prediction`$pr.rgraph@x.values),
                    positive.predictive.rate =  unlist(gf.tech0$`Tie prediction`$pr.rgraph@y.values))
ptech1 = data.frame(Model = 'M1',
                    true.positive.rate = unlist(gf.tech1$`Tie prediction`$pr.rgraph@x.values),
                    positive.predictive.rate =  unlist(gf.tech1$`Tie prediction`$pr.rgraph@y.values))
ptech2 = data.frame(Model = 'M2',
                    true.positive.rate = unlist(gf.tech2$`Tie prediction`$pr.rgraph@x.values),
                    positive.predictive.rate =  unlist(gf.tech2$`Tie prediction`$pr.rgraph@y.values))

ptechs = join_all(list(ptech0,ptech1,ptech2),type='full')


phum0 = data.frame(Model = 'M0',
                   true.positive.rate = unlist(gf.hum0$`Tie prediction`$pr.rgraph@x.values),
                   positive.predictive.rate =  unlist(gf.hum0$`Tie prediction`$pr.rgraph@y.values))
phum1 = data.frame(Model = 'M1',
                   true.positive.rate = unlist(gf.hum1$`Tie prediction`$pr.rgraph@x.values),
                   positive.predictive.rate =  unlist(gf.hum1$`Tie prediction`$pr.rgraph@y.values))
phum2 = data.frame(Model = 'M2',
                   true.positive.rate = unlist(gf.hum2$`Tie prediction`$pr.rgraph@x.values),
                   positive.predictive.rate =  unlist(gf.hum2$`Tie prediction`$pr.rgraph@y.values))
phums = join_all(list(phum0,phum1,phum2),type='full')

phums$Resource = 'human'
ptechs$Resource = 'tech'
pfins$Resource = 'fin'
prall = join_all(list(phums,ptechs,pfins),type='full')
prall$uq = paste(prall$Model,prall$Resource)

##### Plot Proc Data #####


gg.proc.tech = ggplot(data=ptechs) + geom_line(aes(x=true.positive.rate,y=positive.predictive.rate,colour=Model)) +
  theme_bw() + scale_color_colorblind() +
  scale_x_continuous(expand=c(0,0))


gg.proc.fin = ggplot(data=pfins) + geom_line(aes(x=true.positive.rate,y=positive.predictive.rate,colour=Model)) +
  theme_bw() + scale_color_colorblind() +
  scale_x_continuous(expand=c(0,0))



gg.recall = ggplot(data=prall,aes(x=true.positive.rate,y=positive.predictive.rate,
                                  linetype=Model,colour=Resource,group=uq)) + 
  #geom_line(aes(x=true.positive.rate,y=positive.predictive.rate,colour=Resource,linetype=Model)) +
  geom_line(lwd=.5) +
  theme_bw() + 
  # scale_linetype_manual(values = c(rep(c("dotted","twodash","dashed","dotdash","solid"),3))) +
  # scale_color_manual(values = c(rep('green', 5),rep('red',5),rep('blue', 5))) +
  scale_colour_colorblind(labels=c('Financial','Human','Technical')) +
  scale_linetype(name='Specification') + 
  scale_x_continuous(expand=c(0,0)) +
  theme(legend.position = c(0.8,.15),
        legend.direction = 'horizontal',
        legend.background = element_rect(fill = alpha('grey90',.50)),
        axis.text = element_text(size=16),axis.title(size=18), legend.text = element_text(size=16), legend.title=element_text(size=16))+
  ylab('Positive Predictive Rate') + xlab('True Positive Rate')



library(dplyr)
auc.pr <- c(gf.fin0$`Tie prediction`$auc.pr, gf.fin1$`Tie prediction`$auc.pr, gf.fin2$`Tie prediction`$auc.pr)
temp.pr = data.frame(auc.pr  = auc.pr,model = 0:2,resource = 'Financial')
auc.pr.df = temp.pr

auc.pr <- c(gf.tech0$`Tie prediction`$auc.pr, gf.tech1$`Tie prediction`$auc.pr, gf.tech2$`Tie prediction`$auc.pr)
temp.pr = data.frame(auc.pr  = auc.pr,model = 0:2,resource = 'Technical')
auc.pr.df = full_join(auc.pr.df,temp.pr)

auc.pr <- c(gf.hum0$`Tie prediction`$auc.pr, gf.hum1$`Tie prediction`$auc.pr, gf.hum2$`Tie prediction`$auc.pr)
temp.pr = data.frame(auc.pr  = auc.pr,model = 0:2,resource = 'Human')
auc.pr.df = full_join(auc.pr.df,temp.pr)

gg.auc = ggplot(auc.pr.df,aes(y=auc.pr,x=model)) + 
  #geom_point(aes(colour = as.factor(model)),size=2) + 
  geom_bar(stat='identity',position = 'dodge') + 
  facet_grid(~ resource) +theme_bw() + theme_tufte(ticks=F) +
  ylab('Area under precision-recall curve') + xlab('Model Specification')+
  theme(strip.text = element_text(size=16),axis.text = element_text(size=14),
        axis.title = element_text(size=16)) +
  scale_y_continuous(expand=c(0,0),limits=c(0,1)) + scale_x_continuous(breaks=c(0:2),labels=c('Edges \nonly','Actor \n2-stars','Forum \n2-stars'))

gg.auc




########## Traceplots ###########

traceplot.theme = theme(panel.background=element_blank(),
                        panel.grid = element_blank(),
                        axis.title = element_text(size=14),
                        strip.text = element_text(size=12),
                        legend.title=element_text(size=14),
                        axis.text = element_text(size=10),
                        legend.position=c(.85,.25),legend.text.align=.5, 
                        legend.title.align=.5, legend.key.size=unit(.5,'cm'),
                        # legend.title = element_blank(),
                        legend.text = element_text(size=12))
plot.width = 6
plot.height=(553/943) *plot.width

mcmc.fin1$Resource = 'Financial'
mcmc.fin2$Resource = 'Financial'
mcmc.hum1$Resource = 'Human'
mcmc.hum2$Resource = 'Human'
mcmc.tech1$Resource = 'Technical'
mcmc.tech2$Resource = 'Technical'
mcmc.tech1$Model = 1
mcmc.tech2$Model = 2
mcmc.hum1$Model = 1
mcmc.hum2$Model = 2
mcmc.fin1$Model = 1
mcmc.fin2$Model = 2

library(texreg)
screenreg(list(mod.fin2,mod.hum2,mod.tech2))



mcmc.mod1 = join_all(list(as.data.frame(mcmc.fin1),as.data.frame(mcmc.hum1),as.data.frame(mcmc.tech1)),type='full')
mcmc.mod2 = join_all(list(as.data.frame(mcmc.fin2),as.data.frame(mcmc.hum2),as.data.frame(mcmc.tech2)),type='full')

mcmc.mod1  = mcmc.mod1 %>% filter(Parameter != 'edgecov.nonmembership.matrix')
mcmc.mod2 = mcmc.mod2 %>% filter(Parameter != 'edgecov.nonmembership.matrix')


trace.order.mod1 = data.frame(Parameter = c('edges','edgecov.year.at.org','edgecov.participation.matrix','b1star2','edgecov.principled.engagement','edgecov.number.of.groups','edgecov.number.of.members',
                                       'edgecov.brokerage.matrix','edgecov.ties.to.members.matrix'), 
                         Pretty.Name = c("Edges","Years at Org.","Participation", "Actor 2-star (H1A/B)", "Principled Eng. (H3)","Num. Groups (H4A)","Group Members (H4B)", "Brokerage (H5)","Ties to Members (H6)"),
                         plot.order = c(1,2,3,4,5,6,7,8,9))

trace.order.mod2 = data.frame(Parameter = c('edges','edgecov.year.at.org','edgecov.participation.matrix','b2star2','edgecov.principled.engagement','edgecov.number.of.groups','edgecov.number.of.members',
                                            'edgecov.brokerage.matrix','edgecov.ties.to.members.matrix'), 
                              Pretty.Name = c("Edges","Years at Org.","Participation", "Forum 2-star (H2/A/B)", "Principled Eng. (H3)","Num. Groups (H4A)","Group Members (H4B)", "Brokerage (H5)","Ties to Members (H6)"),
                              plot.order = c(1,2,3,4,5,6,7,8,9))


mcmc.mod1 = full_join(mcmc.mod1,trace.order.mod1)
mcmc.mod2 = full_join(mcmc.mod2,trace.order.mod2)

label_trace = as.character(trace.order$Pretty.Name)
names(label_trace) = as.character(as.vector(trace.order$plot.order))
label_trace.mod1 = label_trace[label_trace!='Forum 2-star (H2/A/B)']
label_trace.mod2 = label_trace[label_trace!='Actor 2-star (H1A/B)']


ggplot(data=mcmc.mod1,aes(colour=as.character(Resource),x=Iteration,y=value,alpha = Model))+
  geom_line(aes(),size=.05,alpha=.6) +
  facet_wrap(~plot.order,scales="free",labeller=labeller(plot.order =label_trace.mod1)) + 
  # scale_colour_grey(guide=guide_legend(title="MCMC Chain", label.position="bottom",nrow=2,override.aes=list(size=1.5,alpha=1)))+
  scale_colour_colorblind(guide=guide_legend(title="Resource", label.position="bottom",nrow=1,override.aes=list(size=1.5,alpha=1))) +
  scale_x_continuous("Iteration",expand=c(0,0),
                     labels = c('50k','100k','150k','200k'),
                     breaks = c(50,100,150,200)*1000) + 
  #breaks=c(40000,120000),labels=c('40k','120k'))+
  scale_y_continuous("Parameter Value")+
  theme_bw()+ 
  theme(panel.background=element_blank(),
        panel.grid = element_blank(),
        axis.title = element_text(size=16),
        axis.ticks = element_blank(),
        strip.text = element_text(size=12),
        legend.title=element_text(size=14),
        axis.text = element_text(size=12),
        legend.position='bottom',legend.text.align=.5, 
        legend.title.align=.5, legend.key.size=unit(.5,'cm'),
        # legend.title = element_blank(),
        legend.text = element_text(size=12))



ggplot(data=mcmc.hum1,aes(colour=as.character(Resource),x=Iteration,y=value,alpha = Model))+
  geom_line(aes(),size=.05,alpha=.6) +
  facet_wrap(~Parameter,scales="free") + 
  # scale_colour_grey(guide=guide_legend(title="MCMC Chain", label.position="bottom",nrow=2,override.aes=list(size=1.5,alpha=1)))+
  scale_colour_colorblind(guide=guide_legend(title="Resource", label.position="bottom",nrow=1,override.aes=list(size=1.5,alpha=1))) +
  scale_x_continuous("Iteration",expand=c(0,0),
                     labels = c('50k','100k','150k','200k'),
                     breaks = c(50,100,150,200)*1000) + 
  #breaks=c(40000,120000),labels=c('40k','120k'))+
  scale_y_continuous("Parameter Value")+
  theme_bw()+ 
  theme(panel.background=element_blank(),
        panel.grid = element_blank(),
        axis.title = element_text(size=16),
        axis.ticks = element_blank(),
        strip.text = element_text(size=12),
        legend.title=element_text(size=14),
        axis.text = element_text(size=12),
        legend.position='bottom',legend.text.align=.5, 
        legend.title.align=.5, legend.key.size=unit(.5,'cm'),
        # legend.title = element_blank(),
        legend.text = element_text(size=12))




ggplot(data=mcmc.mod2,aes(colour=as.character(Resource),x=Iteration,y=value,alpha = Model))+
  geom_line(aes(),size=.05,alpha=.6) +
  facet_wrap(~plot.order,scales="free",labeller=labeller(plot.order =label_trace.mod2)) + 
  # scale_colour_grey(guide=guide_legend(title="MCMC Chain", label.position="bottom",nrow=2,override.aes=list(size=1.5,alpha=1)))+
  scale_colour_colorblind(guide=guide_legend(title="Resource", label.position="bottom",nrow=1,override.aes=list(size=1.5,alpha=1))) +
  scale_x_continuous("Iteration",expand=c(0,0),
                     labels = c('50k','100k','150k','200k'),
                     breaks = c(50,100,150,200)*1000) + 
  #breaks=c(40000,120000),labels=c('40k','120k'))+
  scale_y_continuous("Parameter Value")+
  theme_bw()+ 
  theme(panel.background=element_blank(),
        panel.grid = element_blank(),
        axis.title = element_text(size=16),
        axis.ticks = element_blank(),
        strip.text = element_text(size=12),
        legend.title=element_text(size=14),
        axis.text = element_text(size=12),
        legend.position='bottom',legend.text.align=.5, 
        legend.title.align=.5, legend.key.size=unit(.5,'cm'),
        # legend.title = element_blank(),
        legend.text = element_text(size=12))



ggplot(data=mcmc.mod1,aes(colour=as.character(Resource),x=value),alpha=0.75)+
  #geom_density(aes(),size=.5,trim=TRUE,adjust=1.75)+ 
  geom_density(aes(),size=.5,trim=TRUE)+ 
  facet_wrap(~plot.order,scales="free",labeller=labeller(plot.order =label_trace.mod1))+
  #scale_colour_grey(guide=guide_legend(title="MCMC Chain", label.position="bottom",nrow=2,override.aes=list(size=1.5,alpha=1)))+
  scale_colour_colorblind(guide=guide_legend(title="Resource", label.position="bottom",nrow=1))+
  scale_x_continuous("Parameter Value",expand=c(0,0),breaks=pretty_breaks(n=3))+
  scale_y_continuous("Density") +
  theme_bw()+
  theme(panel.background=element_blank(),
        panel.grid = element_blank(),
        axis.title = element_text(size=16),
        axis.ticks = element_blank(),
        strip.text = element_text(size=12),
        legend.title=element_text(size=14),
        axis.text = element_text(size=12),
        legend.position='bottom',legend.text.align=.5, 
        legend.title.align=.5, legend.key.size=unit(.5,'cm'),
        legend.text = element_text(size=12))

head(mcmc.mod2)
mcmc.mod2 %>% group_by(Resource,Parameter) %>% summarise(mean(value))

ggplot(data=mcmc.mod2,aes(colour=as.character(Resource),x=value),alpha=0.75)+
  #geom_density(aes(),size=.5,trim=TRUE,adjust=1.75)+ 
  geom_density(aes(),size=.5,trim=TRUE)+ 
  facet_wrap(~plot.order,scales="free",labeller=labeller(plot.order =label_trace.mod2))+
  #scale_colour_grey(guide=guide_legend(title="MCMC Chain", label.position="bottom",nrow=2,override.aes=list(size=1.5,alpha=1)))+
  scale_colour_colorblind(guide=guide_legend(title="Resource", label.position="bottom",nrow=1))+
  scale_x_continuous("Parameter Value",expand=c(0,0),breaks=pretty_breaks(n=3))+
  scale_y_continuous("Density") +
  theme_bw()+
  theme(panel.background=element_blank(),
        panel.grid = element_blank(),
        axis.title = element_text(size=16),
        axis.ticks = element_blank(),
        strip.text = element_text(size=12),
        legend.title=element_text(size=14),
        axis.text = element_text(size=12),
        legend.position='bottom',legend.text.align=.5, 
        legend.title.align=.5, legend.key.size=unit(.5,'cm'),
        legend.text = element_text(size=12))

























ggplot(data=mcmc.mod2,aes(colour=as.character(Resource),x=Iteration,y=value,alpha = Model))+
  geom_line(aes(),size=.05,alpha=.6) +
  facet_wrap(~plot.order,scales="free",labeller=labeller(plot.order = label_trace[label_trace!='b2star2'])) + 
  # scale_colour_grey(guide=guide_legend(title="MCMC Chain", label.position="bottom",nrow=2,override.aes=list(size=1.5,alpha=1)))+
  scale_colour_colorblind(guide=guide_legend(title="Resource Type", label.position="bottom",nrow=2,override.aes=list(size=1.5,alpha=1))) +
  scale_x_continuous("Iteration",expand=c(0,0)) + #breaks=c(40000,120000),labels=c('40k','120k'))+
  scale_y_continuous("Parameter Value")+
  theme_bw()+
  traceplot.theme


head(mcmc.fin1)

mcmc.fin1$Chain


test = mcmc.mod1 %>% filter(Resource == 'Financial',Parameter == 'b1star2')

head(test)

plot(test$value)














library(lme4)

head(uq.org.group.resource)
temp = as.data.frame(principled.engagement)
temp$uq.ID = rownames(temp)
temp = gather(temp,Group,PE,-uq.ID)

uq.org.group.resource$principled.engagement = temp$PE[match(paste(uq.org.group.resource$uq.ID,uq.org.group.resource$Group),paste(temp$uq.ID,temp$Group))]


temp = as.data.frame(number.of.members)
temp$uq.ID = rownames(temp)
temp = gather(temp,Group,num.members,-uq.ID)
uq.org.group.resource$num.members = temp$num.members[match(paste(uq.org.group.resource$uq.ID,uq.org.group.resource$Group),paste(temp$uq.ID,temp$Group))]


mod.glm.fin = glmer(Increase ~ Participation + years.at.org + principled.engagement + Number.Of.Groups + num.members + 
                      brokerage.score + ties.to.other.members + (1|uq.ID) + (1|Group),
                    data=uq.org.group.resource[uq.org.group.resource$Short=='financial',],
      family='binomial')

mod.glm.hum = glmer(Increase ~ Participation + years.at.org + principled.engagement + Number.Of.Groups + num.members + 
                      sqrt(brokerage.score) + ties.to.other.members +  (1|Group),
                    data=uq.org.group.resource[uq.org.group.resource$Short=='human',],
                    family=binomial)

uq.org.group.resource$brokerage.score



brokerage.matrix = sqrt(brokerage.matrix)
bimod.brokerage.matrix = sqrt(bimod.brokerage.matrix)
principled.engagement = face.to.face.matrix + awareness.matrix + understanding.matrix



uq.org.group.resource[uq.org.group.resource$group.resource=='financial',]
uq.org.group.resource$group.resource\


head(uq.org.group.resource)
glmer(Increase ~ Participation + years.at.org + principled.engagement + Number.Of.Groups + )



summary(mod.fin1)
head(uq.org.group.resource)

library(data.table) ## v >= 1.9.6
library(stargazer)

temp$CI = paste(round(temp$conf.low,2),round(temp$conf.high,2),sep=', ')
coef.table = dcast(setDT(temp),order + label + hyp ~ resource + model,value.var = c('CI'))

coef.table

coef.table %>% arrange(order) %>% select(-order) %>% stargazer(.,summary=FALSE,digits=2,column.labels = rep(c('w/ Actor 2-star','w/ Forum 2-star'),3),type='html')


stargazer(mod.fin1,ci=TRUE)

dcast(setDT(df), month ~ student, value.var = c("A", "B")) 
  
  
sdt%>% unite(Prod_Count, Product,Country) %>% spread(Prod_Count, value)%>% head()


?unite
unique(coef$label)
   
coefplot(mod.fin1)



screenreg(list(mod.fin1,mod.fin2,mod.hum1,mod.hum2,mod.tech1,mod.tech2))
         
)


coef.labels %>% arrange(order) %>% .$name



library(statnet)
library(coefplot)
coefplot::coefplot(mod.fin1)

mod.fin1$coef
names(mod.fin1$coef)[-length(mod.fin1$coef)]

as.data.frame(confint(mod.fin1))


ggplot()

library(xergm)







tab1<-data.frame("Vars"=row.names(noav_logits[[1]]$model$summary.fixed),noav_logits[[1]]$model$summary.fixed,"NOAV"=0)
tab2<-data.frame("Vars"=row.names(noav_logits[[2]]$model$summary.fixed),noav_logits[[2]]$model$summary.fixed,"NOAV"=1)
noavtab<-rbind(tab1,tab2)
row.names(noavtab)<-1:nrow(noavtab)


levels(noavtab$Vars)<-c("BMP Count","Complaint Distance","Dehydrator Units","Drilling Pits","Flare","Form 2a","Gas Volume","Hydraullic Fracturing","Collaboration BMPs","Multi-Well Pits","Oil Tanks","Oil Volume","Pop Density","Producing","Wildlife Area","Horizontal Drilling","200PC Poverty","Water Distance","Water Pipeline","Water Tanks","Water Volume")
noavtab$NOAV<-as.factor(noavtab$NOAV)
library(ggplot2)
library(ggthemes)
ggplot(noavtab)+geom_hline(aes(yintercept=1),lty=2,colour="grey")+geom_linerange(aes(x=NOAV,ymin=exp(X0.025quant),ymax=exp(X0.975quant),colour=as.ordered(NOAV)),size=4)+facet_wrap(~Vars,nrow=5)+theme_pander()+coord_flip(ylim=c(0,3))+scale_colour_tableau(name="Complaint NOAV Status,\nCompared to\nNo Complaint",labels=c("No Notice","Notice"))+theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())+xlab("")+ylab("Odds Ratio")





  scale_x_continuous(labels=paste0('M',0:4))

scale_x_discrete(name = 'Specification',expand=c(0,0),labels=as.character(0:4)) 

  scale_fill_colorblind(name='Specification') + 

  scale_y_continuous(name= 'Area Under Precision-Recall Curve',expand=c(0,0),limits=c(0,1)) + theme_bw() 



  theme(axis.ticks.x=element_blank(),axis.title.x=element_blank(),
        legend.direction = 'horizontal',legend.background = element_rect(fill = alpha('grey60',.8),colour=NA),
        legend.position = c(0.8,.1),legend.key=element_blank())



barplot(auc.pr, col = c("#000000", "#666666", "#AAAAAA", "#CCCCCC", "#EEEEEE"),
        ylim = c(0, 1), names = round(auc.pr, 3),
        main = "Area under the precision‐recall curve")


ggplot(temp) + geom_bar(aes(x=model,y=auc.pr),stat='identity')


barplot(auc.pr, col = c("#000000", "#666666", "#AAAAAA", "#CCCCCC", "#EEEEEE"),
        ylim = c(0, 0.65), names = round(auc.pr, 2),
        main = "Area under the precision‐recall curve")


barplot(auc.pr, col = c("#000000", "#666666", "#AAAAAA", "#CCCCCC", "#EEEEEE"),
        ylim = c(0, 0.65), names = round(auc.pr, 2),
        main = "Area under the precision‐recall curve")



legend("topright", legend = c("Model 1", "Model 2", "Model 3",
                              "Edges and two‐stars (coalitions)", "Random graph with same density"), 
       col = c("#000000", "#666666", "#AAAAAA", "#CCCCCC", "#EEEEEE"), pch = 15)



  

ggplot(mort3, aes(x = year, y = BCmort, col = State, linetype = State)) +
  geom_line(lwd = 1) +
  scale_linetype_manual(values = c(rep("solid", 10), rep("dashed", 6))) +
  scale_color_manual(values = c(brewer.pal(10, "Set3"), brewer.pal(6, "Set3"))) +
  opts(title = "BC mortality") +
  theme_bw()
  
  scale_colour_manual(values=c('red','blue','green'))


  
  geom_line(aes(x=true.positive.rate,y=positive.predictive.rate,colour=Resource,group=uq),linetype=0) +
    geom_line(aes(x=true.positive.rate,y=positive.predictive.rate,linetype=Model,group=uq),linetype=0) +
+
  guides(linetype=guide_legend(show=FALSE))
 


class(temp$roc)



gf.fin1$`Tie prediction`$auc.pr.rgraph                                                                            
                                                                                                                                                                "Edges and two‐stars (coalitions)", "Random graph with same density"), col = c("#000000", "#666666", "#AAAAAA", "#CCCCCC", "#EEEEEE"), pch = 15)
dev.off()
gf.fin1$`Tie prediction`$auc.pr.rgraph


plot(gf.fin1$`Tie prediction`, boxplot = FALSE, roc = FALSE, pr = TRUE,
     pr.col = "#000000", pr.random = TRUE, pr.random.col = "#EEEEEE") 

summary(mod.)


plot(gf.2, boxplot = FALSE, roc = FALSE, pr = TRUE, rocpr.add = TRUE,
                                                                           pr.col = "#666666")
plot(gf.3, boxplot = FALSE, roc = FALSE, pr = TRUE, rocpr.add = TRUE,
     pr.col = "#AAAAAA")
plot(gf.endogenous, boxplot = FALSE, roc = FALSE, pr = TRUE, rocpr.add = TRUE,
     pr.col = "#CCCCCC")



  plot(gf.fin1, boxplot = FALSE, roc = FALSE, pr = TRUE,
         pr.random = TRUE, rocpr.add = TRUE) # add PR curve



remote = FALSE
if (remote)
{load('../../modtemp.RData')
}

if (!remote)
{load('modtemp.RData')
}


library(xergm)
library(btergm)
test = btergm::rocprgof(mod.fin1,obs=finNA)

test = btergm::rocpr(mod.fin1,obs=finNA)
library(Matrix)
test = btergm::gof(mod.fin1, target = finNA, 
    nsim = 100, MCMC.interval = 1000, MCMC.burnin = 10000,
    parallel = 'no', ncpus = 1,
    statistics = c(dsp,deg, rocpr), verbose = TRUE)

summary(test)
test[[3]]

temp = test[[3]]
temp2 = temp[6]
temp3 = temp2['roc']
as.data.frame(temp3)

plot(temp2['roc'][[1]])

# simulate 100 networks from t = 3 and compare to t = 4
gof3 <- test

# display goodness of fit
plot(gof3, roc = FALSE, pr = FALSE) # predictive fit (boxplots)
gof3 # display goodness of fit tables
pdf("rocpr.pdf")
plot(gof3, boxplot = FALSE, pr = FALSE, roc = TRUE,
     roc.random = TRUE, pr.random = FALSE, ylab = "TPR/PPV",
     xlab = "FPR/TPR", roc.main = "ROC and PR curves") # ROC curve
plot(gof3, boxplot = FALSE, roc = FALSE, pr = TRUE,
     pr.random = TRUE, rocpr.add = TRUE) # add PR curve



roc.perf = performance(pred, measure = &quot;tpr&quot;, x.measure = &quot;fpr&quot;)
plot(roc.perf)
abline(a=0, b= 1)


class(test)
class()
unlist(temp)
test$`Tie prediction`

nsim=1000
test <- gof(mod.fin1, nsim = nsim, target = finNA,  
               #coef = mod.fin1$coef[names(mod.fin1$coef)!='edgecov.nonmembership.matrix'],
               rocprgof = compute.rocprgof, checkdegeneracy = FALSE)



auc.pr <‐ c(gf.1$auc.pr, gf.2$auc.pr, gf.3$auc.pr,
            gf.endogenous$auc.pr, gf.3$rgraph.auc.pr)


require(statnet)
require(ggplot2)
require(ggmcmc)
require(grid)
library(ergm)
library(reshape2)
library(plyr)
library(dplyr)
library(scales)
library(texreg)
library(stargazer)
library(texreg)
library(gdata)
library(car)
#rm(list=ls())


#WA.static.ergm<-logLik(WA.static.ergm, add=TRUE)
#MO.static.ergm<-logLik(MO.static.ergm, add=TRUE)
#MO.static.ergm<-logLik(GA.static.ergm, add=TRUE)



library(ggplot2)
library(scales)
library(ggthemes)

ggplot(coef) + geom_hline(aes(yintercept=1),lty=2,colour="grey") +
geom_linerange(aes(ymax=conf.high,ymin=conf.low,x=as.factor(-order),
                    colour=resource,alpha = as.factor(model)),lwd=2,
               position = position_dodge(width = 0.7)) + facet_wrap(as.factor(order)~) 


  coord_flip() + geom_text(aes(y = -0.45,x = as.factor(-order),label = hyp)) + 
  scale_x_discrete(name='Variable',breaks=-c(1:max(coef$order)),labels=unique(coef.order$label))+ 
  scale_y_continuous(limits=c(-.5,1.5))+
scale_alpha_discrete(name = '',labels=c('w/ actor 2-stars','w/ forum 2-stars'),range=c(.5,1))+
  theme_bw()  + 
  scale_colour_colorblind(name='',labels=c('Financial','Human','Technical'))+
theme(legend.direction='horizontal',legend.position = c(0.7,.2),axis.title.y=element_blank(),
      axis.title.x = element_blank(),
       axis.ticks.y = element_blank(),legend.background=element_rect(fill = alpha('grey80',.25)))


library(ggplot2)
library(ggthemes)
ggplot(noavtab)+geom_hline(aes(yintercept=0),lty=2,colour="grey")+geom_linerange(aes(x=NOAV,ymin=exp(X0.025quant),ymax=exp(X0.975quant),colour=as.ordered(NOAV)),size=4)+
  facet_wrap(~Vars,nrow=5)+theme_pander()+coord_flip(ylim=c(0,3))+scale_colour_tableau(name="Complaint NOAV Status,\nCompared to\nNo Complaint",labels=c("No Notice","Notice"))+theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())+xlab("")+ylab("Odds Ratio")










facet_grid(vore ~ conservation, labeller = labeller(
  .default = capitalize,
  conservation = conservation_status
))



mf_labeller <- function(var, value){
  value <- as.character(value)
  if (var=="sex") { 
    value[value=="Female"] <- "Woman"
    value[value=="Male"]   <- "Man"
  }
  return(value)
}

coef.labels



## Source: http://en.wikipedia.org/wiki/Wikipedia:Conservation_status

p2 + facet_grid(vore ~ conservation, labeller = labeller(
  .default = capitalize,
  conservation = conservation_status
))



var_labeller(coef$order)

coef$order



ggplot(coef) + 
  geom_segmen
  
  
  
  geom_linerange(aes(ymax=conf.high,ymin=conf.low,x=1,
                     colour=resource,alpha = as.factor(model)),lwd=2,
                 position = position_dodge(width = 0.7)) + facet_wrap(~as.factor(-order)) + 
  coord_flip() + geom_text(aes(y = -0.45,x = as.factor(-order),label = hyp)) + 
  scale_x_discrete(name='Variable',breaks=-c(1:max(coef$order)),labels=unique(coef.order$label))+ 
  scale_y_continuous(limits=c(-.5,1.5))+
  scale_alpha_discrete(name = '',labels=c('w/ actor 2-stars','w/ forum 2-stars'),range=c(.5,1))+
  theme_bw()  + 
  scale_colour_colorblind(name='',labels=c('Financial','Human','Technical'))+
  theme(legend.direction='horizontal',legend.position = c(0.7,.2),axis.title.y=element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.y = element_blank(),legend.background=element_rect(fill = alpha('grey80',.25)))




library(arm)  


library(texreg)
linetype_pal(1:2)
?scale_linetype
?geom_pointrange
test = ergm::logLik.ergm(mod.fin1,add=TRUE)
test


library(ggthemes)
ggplot(coef,aes(ymin=conf.low,ymax=conf.high,y=coef,x=name))  + 
  geom_linerange(aes(colour=resource),
                 position = position_dodge(width = 1/2)) +
  coord_flip() + theme_bw() + 
  facet_wrap(~model)+
  theme(axis.text=element_blank(),axis.title=element_blank(),legend.position = c(0.8,.2))



zp1 <- ggplot(allModelFrame, aes(colour = modelName))
zp1 <- zp1 + geom_hline(yintercept = 0, colour = gray(1/2), lty = 2)
zp1 <- zp1 + geom_linerange(aes(x = Variable, ymin = Coefficient - SE*interval1,
                                ymax = Coefficient + SE*interval1),
                            lwd = 1, position = position_dodge(width = 1/2))
zp1 <- zp1 + geom_pointrange(aes(x = Variable, y = Coefficient, ymin = Coefficient - SE*interval2,
                                 ymax = Coefficient + SE*interval2),
                             lwd = 1/2, position = position_dodge(width = 1/2),
                             shape = 21, fill = "WHITE")
zp1 <- zp1 + coord_flip() + theme_bw()
zp1 <- zp1 + ggtitle("Comparing several models")
print(zp1)  # The trick to these is position_dodge().



?betwrg
coef$resource
head(coef)
  
?confint_tidy

broom::confint_tidy(mod.fin1)
head(gf.fin1$)

gf.fin1$psim.dist
gf.fin1$pobs.dist

plot(
  gf.fin1$
     
     ~gf.fin1$pobs.dist)
plot(gf.fin1, boxplot = FALSE, roc = FALSE, pr = TRUE,
     pr.col = "#000000", pr.random = TRUE, pr.random.col = "#EEEEEE")

htmlreg(l=list(mod.fin1,mod.fin2,mod.fin3,mod.fin4),
       #file=paste0(extension.add,'model.output.H1.table.html'),
      #  custom.model.names = c('Washington','Missouri','Georgia'),
       leading.zero = TRUE,digits = 2)

htmlreg(l=list(mod.hum1,mod.hum2,mod.hum3,mod.hum4),
        #file=paste0(extension.add,'model.output.H1.table.html'),
        #  custom.model.names = c('Washington','Missouri','Georgia'),
        leading.zero = TRUE,digits = 2)

htmlreg(l=list(mod.tech1,mod.tech2,mod.tech3,mod.tech4),
        #file=paste0(extension.add,'model.output.H1.table.html'),
        #  custom.model.names = c('Washington','Missouri','Georgia'),
        leading.zero = TRUE,digits = 2)


#RENAME PARAMETERS
levels(temp.WA.H1$Parameter) = c('Sum','Meetings')
levels(temp.WA.H2$Parameter) = c('Sum','Non-zero','Meetings')
levels(temp.WA.H34$Parameter) = c('Sum','Non-zero','Mutuality','Transitivity','Meetings')

levels(temp.MO.H1$Parameter) = c('Sum','Meetings')
levels(temp.MO.H2$Parameter) = c('Sum','Non-zero','Meetings')
levels(temp.MO.H34$Parameter) = c('Sum','Non-zero','Mutuality','Transitivity','Meetings')

levels(temp.GA.H1$Parameter) = c('Sum','Meetings')
levels(temp.GA.H2$Parameter) = c('Sum','Non-zero','Meetings')
levels(temp.GA.H34$Parameter) = c('Sum','Non-zero','Mutuality','Transitivity','Meetings')


# compute observed network statistics
net.obs.MO.H1 <- data.frame(summary(MO.static.ergm.H1$formula,
                                    response="edge.weight",reference=~Poisson))
net.obs.MO.H2 <- data.frame(summary(MO.static.ergm.H2$formula,
                                    response="edge.weight",reference=~Poisson))
net.obs.MO.H34 <- data.frame(summary(MO.static.ergm.H34$formula,
                                     response="edge.weight",reference=~Poisson))

net.obs.WA.H1 <- data.frame(summary(WA.static.ergm.H1$formula,
                                 response="edge.weight",reference=~Poisson))
net.obs.WA.H2 <- data.frame(summary(WA.static.ergm.H2$formula,
                                 response="edge.weight",reference=~Poisson))
net.obs.WA.H34 <- data.frame(summary(WA.static.ergm.H34$formula,
                                 response="edge.weight",reference=~Poisson))

net.obs.GA.H1 <- data.frame(summary(GA.static.ergm.H1$formula,
                                    response="edge.weight",reference=~Poisson))
net.obs.GA.H2 <- data.frame(summary(GA.static.ergm.H2$formula,
                                    response="edge.weight",reference=~Poisson))
net.obs.GA.H34 <- data.frame(summary(GA.static.ergm.H34$formula,
                                     response="edge.weight",reference=~Poisson))


net.obs.WA.H1$Var2 <- levels(temp.WA.H1$Parameter)
net.obs.WA.H2$Var2 <- levels(temp.WA.H2$Parameter)
net.obs.WA.H34$Var2 <- levels(temp.WA.H34$Parameter)

net.obs.MO.H1$Var2 <- levels(temp.MO.H1$Parameter)
net.obs.MO.H2$Var2 <- levels(temp.MO.H2$Parameter)
net.obs.MO.H34$Var2 <- levels(temp.MO.H34$Parameter)

net.obs.GA.H1$Var2 <- levels(temp.GA.H1$Parameter)
net.obs.GA.H2$Var2 <- levels(temp.GA.H2$Parameter)
net.obs.GA.H34$Var2 <- levels(temp.GA.H34$Parameter)


names(net.obs.MO.H1)[1] = 'Observed.Value'
net.obs.MO.H1[,2] = rownames(net.obs.MO.H1)
names(net.obs.MO.H1)[2] = 'Var2'
net.obs.MO.H1$Var2 = levels(temp.MO.H1$Parameter)

names(net.obs.MO.H2)[1] = 'Observed.Value'
net.obs.MO.H2[,2] = rownames(net.obs.MO.H2)
names(net.obs.MO.H2)[2] = 'Var2'
net.obs.MO.H2$Var2 = levels(temp.MO.H2$Parameter)

names(net.obs.MO.H34)[1] = 'Observed.Value'
net.obs.MO.H34[,2] = rownames(net.obs.MO.H34)
names(net.obs.MO.H34)[2] = 'Var2'
net.obs.MO.H34$Var2 = levels(temp.MO.H34$Parameter)

names(net.obs.WA.H1)[1] = 'Observed.Value'
net.obs.WA.H1[,2] = rownames(net.obs.WA.H1)
names(net.obs.WA.H1)[2] = 'Var2'
net.obs.WA.H1$Var2 = levels(temp.WA.H1$Parameter)

names(net.obs.WA.H2)[1] = 'Observed.Value'
net.obs.WA.H2[,2] = rownames(net.obs.WA.H2)
names(net.obs.WA.H2)[2] = 'Var2'
net.obs.WA.H2$Var2 = levels(temp.WA.H2$Parameter)

names(net.obs.WA.H34)[1] = 'Observed.Value'
net.obs.WA.H34[,2] = rownames(net.obs.WA.H34)
names(net.obs.WA.H34)[2] = 'Var2'
net.obs.WA.H34$Var2 = levels(temp.WA.H34$Parameter)

names(net.obs.GA.H1)[1] = 'Observed.Value'
net.obs.GA.H1[,2] = rownames(net.obs.GA.H1)
names(net.obs.GA.H1)[2] = 'Var2'
net.obs.GA.H1$Var2 = levels(temp.GA.H1$Parameter)

names(net.obs.GA.H2)[1] = 'Observed.Value'
net.obs.GA.H2[,2] = rownames(net.obs.GA.H2)
names(net.obs.GA.H2)[2] = 'Var2'
net.obs.GA.H2$Var2 = levels(temp.GA.H2$Parameter)

names(net.obs.GA.H34)[1] = 'Observed.Value'
net.obs.GA.H34[,2] = rownames(net.obs.GA.H34)
names(net.obs.GA.H34)[2] = 'Var2'
net.obs.GA.H34$Var2 = levels(temp.GA.H34$Parameter)


levels(sim.dat.WA.H1$Var2) = levels(temp.WA.H1$Parameter)
levels(sim.dat.WA.H2$Var2) = levels(temp.WA.H2$Parameter)
levels(sim.dat.WA.H34$Var2) = levels(temp.WA.H34$Parameter)
levels(sim.dat.MO.H1$Var2) = levels(temp.MO.H1$Parameter)
levels(sim.dat.MO.H2$Var2) = levels(temp.MO.H2$Parameter)
levels(sim.dat.MO.H34$Var2) = levels(temp.MO.H34$Parameter)
levels(sim.dat.GA.H1$Var2) = levels(temp.GA.H1$Parameter)
levels(sim.dat.GA.H2$Var2) = levels(temp.GA.H2$Parameter)
levels(sim.dat.GA.H34$Var2) = levels(temp.GA.H34$Parameter)

# precision‐recall curves
pdf("pr‐curve.pdf")
plot(gf.fin1, boxplot = FALSE, roc = FALSE, pr = TRUE,
     pr.col = "#000000", pr.random = TRUE, pr.random.col = "#EEEEEE")
dev.off()


legend("topright", legend = c("Model 1", "Model 2", "Model 3",
                              "Edges and two‐stars (coalitions)", "Random graph with same density"), col = c("#000000", "#666666", "#AAAAAA", "#CCCCCC", "#EEEEEE"),
       lty = 1, lwd = 3)


plot(gf.2, boxplot = FALSE, roc = FALSE, pr = TRUE, rocpr.add = TRUE,
                                                                           pr.col = "#666666")
pdf("auc‐pr‐barplot.pdf")
auc.pr <‐ c(gf.1$auc.pr, gf.2$auc.pr, gf.3$auc.pr,
            gf.endogenous$auc.pr, gf.3$rgraph.auc.pr)
barplot(auc.pr, col = c("#000000", "#666666", "#AAAAAA", "#CCCCCC", "#EEEEEE"),
        ylim = c(0, 0.65), names = round(auc.pr, 2),
        main = "Area under the precision‐recall curve") legend("topright", legend = c("Model 1", "Model 2", "Model 3",
                                                                                      "Edges and two‐stars (coalitions)", "Random graph with same density"), col = c("#000000", "#666666", "#AAAAAA", "#CCCCCC", "#EEEEEE"), pch = 15)
dev.off()



#Match simulated values to observed statistics

sim.values.WA.H1= join(sim.dat.WA.H1,net.obs.WA.H1)
sim.values.WA.H2= join(sim.dat.WA.H2,net.obs.WA.H2)
sim.values.WA.H34= join(sim.dat.WA.H34,net.obs.WA.H34)

sim.values.MO.H1= join(sim.dat.MO.H1,net.obs.MO.H1)
sim.values.MO.H2= join(sim.dat.MO.H2,net.obs.MO.H2)
sim.values.MO.H34= join(sim.dat.MO.H34,net.obs.MO.H34)

sim.values.GA.H1= join(sim.dat.GA.H1,net.obs.GA.H1)
sim.values.GA.H2= join(sim.dat.GA.H2,net.obs.GA.H2)
sim.values.GA.H34= join(sim.dat.GA.H34,net.obs.GA.H34)

traceplot.theme = theme(panel.background=element_blank(),
                        panel.grid = element_blank(),
                        axis.title = element_text(size=14),
                        strip.text = element_text(size=12),
                        legend.title=element_text(size=14),
                        axis.text = element_text(size=10),
                        legend.position=c(.85,.25),legend.text.align=.5, 
                        legend.title.align=.5, legend.key.size=unit(.5,'cm'),
                        # legend.title = element_blank(),
                        legend.text = element_text(size=12))
plot.width = 6
plot.height=(553/943) *plot.width




#MCMC TRACEPLOTS
ggplot(data=mcmc.fin1,aes(colour=as.character(Chain),x=Iteration,y=value),alpha=0.75)+
  geom_line(aes(),size=.05,alpha=.6)+
  facet_wrap(~Parameter,scales="free")+
  # scale_colour_grey(guide=guide_legend(title="MCMC Chain", label.position="bottom",nrow=2,override.aes=list(size=1.5,alpha=1)))+
  scale_colour_brewer(type="qual",palette='Set1',guide=guide_legend(title="MCMC Chain", label.position="bottom",nrow=2,override.aes=list(size=1.5,alpha=1)))+
  scale_x_continuous("Iteration",expand=c(0,0)) + #breaks=c(40000,120000),labels=c('40k','120k'))+
  scale_y_continuous("Parameter Value")+
  theme_bw()+
  traceplot.theme




#DENSITY PLOTS
plot.width = 6
plot.height=(555/945) *plot.width





# 
# 
# sim = simulate.ergm(WA.static.ergm,nsim = 50000,statsonly=TRUE,control=control.simulate.ergm(MCMC.burnin=15000,
#                                                                                         MCMC.interval=1500,
#                                                                                         MCMC.prop.args=list(p0=0.5),
#                                                                                         MCMC.runtime.traceplot=FALSE,
#                                                                                         parallel=8,
#                                                                                    parallel.type='SOCK'))
# sim.dat = melt(sim,id.vars=nrow(sim))

# sim = simulate.ergm(WA.static.ergm,nsim = 50000,statsonly=TRUE,control=control.simulate.ergm(MCMC.burnin=15000,
#                                                                                         MCMC.interval=1500,
#                                                                                         MCMC.prop.args=list(p0=0.5),
#                                                                                         MCMC.runtime.traceplot=FALSE,
#                                                                                         parallel=8,
#                                                                                    parallel.type='SOCK'))
# sim.dat = melt(sim,id.vars=nrow(sim))

# sim = simulate.ergm(WA.static.ergm,nsim = 50000,statsonly=TRUE,control=control.simulate.ergm(MCMC.burnin=15000,
#                                                                                         MCMC.interval=1500,
#                                                                                         MCMC.prop.args=list(p0=0.5),
#                                                                                         MCMC.runtime.traceplot=FALSE,
#                                                                                         parallel=8,
#                                                                                    parallel.type='SOCK'))
# sim.dat = melt(sim,id.vars=nrow(sim))



#levels(sim.dat.MO$Var2) = c('Sum','Mutual','Transitive Weights','Number Resp.','Number Groups','Mean Years','Org. Type')

simplot.theme =   theme(panel.background=element_blank(),
                        panel.grid = element_blank(),
                        axis.title = element_text(size=14),
                        strip.text = element_text(size=12),
                        axis.text = element_text(size=10),
                        legend.position=c(.85,.25),
                        legend.title=element_text(size=14),
                        legend.key.height = unit(2,'cm'),
                        # legend.title = element_blank(),
                        legend.text = element_text(size=12))

plot.width = 6
plot.height=(555/945) *plot.width




sim.plot.MO.H1 = ggplot(sim.values.MO.H1) + 
  #geom_density(aes(x=value),adjust=1.5,colour='black',trim=TRUE) + 
  geom_line(aes(x=value),stat='density',adjust=1.5,colour='black',trim=TRUE) + 
  facet_wrap(~Var2,scales='free') + 
  geom_vline(aes(xintercept=Observed.Value,linetype = "expected"),show_guide = TRUE,lwd=1.25)+
  theme_bw()  + ylab('Density') + xlab('Simulated Value') + 
  scale_x_continuous(breaks=pretty_breaks(n=2))+
  scale_linetype_manual(name='',values=2,labels=c('Observed statistic')) + 
  # scale_x_continuous(breaks = c(min(df$x), 0, max(df$x)))
  theme_bw() + simplot.theme
ggsave(paste0(extension.add,'mcmc.simplot.MO.H1.png'),sim.plot.MO.H1,width=plot.width,height= plot.height,units='in')
