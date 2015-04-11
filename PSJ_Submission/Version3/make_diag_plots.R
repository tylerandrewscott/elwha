require(statnet)
require(ggplot2)
require(ggmcmc)
require(grid)

rm(list=ls())
setwd('/homes/tscott1/win/user/elwha/PSJ_Submission/Version3/')

load('result_base.RData')

temp2<-ggs(mod_base$sample)
temp2<-(merge(temp2,mod_base$target.stats))


levels(temp2$Parameter)<-c("Sum","Mutual","Transitive Weights",
                           "Number Resp.","Number Groups", 'Mean Years', 'Org. Type')
vline.dat.du <- data.frame(z=unique(temp2$y), vl=levels(temp2$Parameter))
levels(temp2$Parameter)

# 
# testdu<-simulate.ergm(mod_base,nsim=1000,seed=24,response="TVAL",
#                       reference=~DiscUnif(0,3),
#                       statsonly=TRUE,
#                       monitor=~nonzero+transitiveties,control=
#                         control.simulate.ergm(
#                                               MCMC.interval=1000,MCMC.burnin=10000,parallel=4,parallel.version.check=TRUE))

ggdu = ggplot(data=temp2,aes(colour=as.character(Chain),x=Iteration,y=value))+geom_line(aes(),size=.10,alpha=.75)+
  facet_wrap(~Parameter,scales="free")+
  scale_colour_brewer(type="qual",palette='Set1',guide=guide_legend(title="MCMC Chain", label.position="bottom",nrow=2,override.aes=list(size=1.5,alpha=1)))+
  scale_x_continuous("Iteration",expand=c(0,0))+
  scale_y_continuous("Parameter Value",expand=c(0,0))+
  theme_bw()+
  theme(axis.text=element_text(size=10),
        axis.title = element_text(size=12),
        legend.title = element_text(size=12),
        legend.text = element_text(size=10),
        legend.position=c(.85,.15),legend.text.align=.5,
        axis.text=element_text(size=6),legend.title.align=.5, legend.key.size=unit(.5,'cm'),
        legend.background = element_rect(colour = "black"))
ggsave(ggdu,filename="traceplotdu.png")
rm(ggdu)


ggdudens = ggplot(data=temp2,aes(colour=as.character(Chain),x=value))+
  geom_density(aes(),size=.5,trim=TRUE,adjust=2.5)+ 
  facet_wrap(~Parameter,scales="free")+
  scale_colour_brewer(type="qual",palette='Set1',
                      guide=guide_legend(title="MCMC Chain", label.position="bottom",nrow=2,
                                         override.aes=list(size=.5,alpha=1)))+
  scale_x_continuous("Iteration",expand=c(0,0))+
  scale_y_continuous("p(X)",expand=c(0,0)) +
  theme_bw()+
  theme(axis.text=element_text(size=10),
        axis.title = element_text(size=12),
        legend.title = element_text(size=12),
        legend.text = element_text(size=10),
        legend.position=c(.85,.25),legend.text.align=.5, 
        legend.title.align=.5, legend.key.size=unit(.5,'cm'),
        axis.text=element_text(size=6),
        legend.background = element_rect(colour = "black"))
ggsave(ggdudens,filename="densityplotdu.png")
rm(ggdudens)

