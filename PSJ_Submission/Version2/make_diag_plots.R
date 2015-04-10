#rm(list=ls())

require(statnet)
library(reshape2)
library(ggplot2)
require(ggmcmc)
require(grid)
#setwd('H:/passtosim')
#setwd("//Users/TScott/Google Drive/elwha")
setwd('//Users/TScott/Google Drive/elwha/PSJ_Submission/Version2/')

load('Hyak.Run2.RData')
library(plyr)
library(dplyr)
library(ggthemes)
temp2<-ggs(mod_base$sample)
temp2<-(merge(temp2,mod_base$target))

levels(temp2$Parameter)<-c("Sum","Mutual","Transitive Weights",
                           "Number Resp.","Number Groups", 'Mean Years', 'Org. Type')
vline.dat.du <- data.frame(z=unique(temp2$y), vl=levels(temp2$Parameter))

ggdu = ggplot(data=temp2,aes(colour=as.character(Chain),x=Iteration,y=value))+geom_line(aes(),size=.10,alpha=.75)+
  facet_wrap(~Parameter,scales="free")+
  scale_colour_brewer(type="qual",palette='Set1',guide=guide_legend(title="MCMC Chain", label.position="bottom",nrow=2,override.aes=list(size=1.5,alpha=1)))+
  theme_tufte(ticks=F)+
  theme(legend.position=c(.85,.15),legend.text.align=.5,
        axis.text=element_text(size=8),
        axis.title=element_text(size=12),
        legend.text = element_text(size=10),
        legend.title = element_text(size=12),
        strip.text = element_text(size=12),
        legend.title.align=.5, legend.key.size=unit(.5,'cm'),
        legend.background = element_rect(colour = "black"))+
  scale_x_continuous("Iteration",expand=c(0,0))+
  scale_y_continuous("Parameter Value",expand=c(0,0))
ggsave(ggdu,filename="traceplotdu.png")
rm(ggdu)

ggdudens = ggplot(data=temp2,aes(colour=as.character(Chain),x=value))+
  geom_density(aes(),size=.5,trim=TRUE,adjust=2.5)+ 
  facet_wrap(~Parameter,scales="free")+
  scale_colour_brewer(type="qual",palette='Set1',
                      guide=guide_legend(title="MCMC Chain", label.position="bottom",nrow=2,
                                         override.aes=list(size=.5,alpha=1)))+
  theme_tufte(ticks=F)+
  theme(legend.position=c(.85,.150),legend.text.align=.5, 
        legend.title.align=.5, legend.key.size=unit(.5,'cm'),
        axis.text=element_text(size=8),
        axis.title=element_text(size=12),
        legend.text = element_text(size=10),
        legend.title = element_text(size=12),
        strip.text = element_text(size=12),
        legend.background = element_rect(colour = "black"))+
  scale_x_continuous("Iteration",expand=c(0,0))+
  scale_y_continuous("p(X)",expand=c(0,0))
ggsave(ggdudens,filename="densityplotdu.png")
rm(ggdudens)
detach(file:Hyak.Run2.RData)
rm(list=ls())
