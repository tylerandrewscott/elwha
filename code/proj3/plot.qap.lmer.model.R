load('code/proj3/temp.qap.results.RData')

library(INLA)
library(ggplot2)

library(plyr)
library(dplyr)
library(tidyr)

plot.df = left_join(perm.mean.ests,param.ests)  

plot.df$uq = paste(plot.df$dep.var,plot.df$param)
plot.df$Greater.than.est = plot.df$mean>plot.df$ests

temp.q.lower = data.frame(tapply(plot.df$mean,plot.df$uq,quantile,0.025))
names(temp.q.lower) = 'lower.q.bound'
temp.q.lower$uq = rownames(temp.q.lower)

temp.q.upper = data.frame(tapply(plot.df$mean,plot.df$uq,quantile,0.975))
names(temp.q.upper) = 'upper.q.bound'
temp.q.upper$uq = rownames(temp.q.upper)

plot.df = left_join(plot.df,full_join(temp.q.lower,temp.q.upper))

for (i in unique(plot.df$uq))
{
  filt = plot.df %>% filter(uq == i) 
  plot.df$perc.rank[plot.df$uq==i] = 1-sum(filt$Greater.than.est)/nrow(filt)
}

strip.labels = c('Face-to-Face Communication','Awareness of Goals/Interests','Common Understanding',
                 'Technical Resources','Human Resources','Financial Resources')

levels(plot.df$dep.var) = strip.labels

#Participation -- sanity check
p.participation = ggplot(plot.df[plot.df$param=='Participation',]) +
  geom_line(aes(x=mean),lty=2,stat='density',trim=TRUE) + 
  facet_wrap(~dep.var,scales='free_x',labeller = ) + 
  geom_vline((aes(xintercept=ests)),col='red') + theme_bw() +
  ylab('') + xlab('Permuted Estimates') + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

## H1: higher brokerage --> higher benefits
p.h1 = ggplot(plot.df[plot.df$param=='brokerage.score',]) +
  geom_line(aes(x=mean),lty=2,stat='density',trim=TRUE) + 
  facet_wrap(~dep.var,scales='free_x') + 
  geom_vline((aes(xintercept=ests)),col='red') + theme_bw() +
  ylab('') + 
  xlab('Permuted Estimates:  Resource ~ Brokerage Score') + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

p.h2 = ggplot(plot.df[plot.df$param=='bimodal.brokerage.score',]) +
  geom_line(aes(x=mean),lty=2,stat='density',trim=TRUE) + 
  facet_wrap(~dep.var,scales='free_x') + 
  geom_vline((aes(xintercept=ests)),col='red') + theme_bw() +
  ylab('') + 
  xlab('Permuted Estimates:  Resource ~ Bimodal Brokerage Score') + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

p.h3 = ggplot(plot.df[plot.df$param=='Number.Of.Groups',]) +
  geom_line(aes(x=mean),lty=2,stat='density',trim=TRUE) + 
  facet_wrap(~dep.var,scales='free_x') + 
  geom_vline((aes(xintercept=ests)),col='red') + theme_bw() +
  ylab('') +
  xlab('Permuted Estimates:  Resource ~ Number of Groups') + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())


p.h5 = ggplot(plot.df[plot.df$param=='ties.to.other.members',]) +
  geom_line(aes(x=mean),lty=2,stat='density',trim=TRUE) + 
  facet_wrap(~dep.var,scales='free_x') + 
  geom_vline((aes(xintercept=ests)),col='red') + theme_bw() +
  ylab('') + 
  xlab('Permuted Estimates:  Resource ~ Ties To Other Group Members') + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())



p = 
  head(plot.df)
ggplot(plot.df[plot.df$param=='diff.from.group.brok.median',]) +
  geom_line(aes(x=mean),lty=1,stat='density',trim=TRUE) + 
  facet_wrap(~dep.var,scales='free_x') + 
  geom_vline(aes(xintercept=lower.q.bound),col='black',lty=2) +
  geom_vline(aes(xintercept=upper.q.bound),col='black',lty=2)+
  geom_vline(aes(xintercept=ests),col='red') + theme_bw()+
  scale_colour_manual(name='',values=c('red','black','black'),
                      labels= 'Posterior Mean','Lower','Upper')+
  xlab('Permuted Estimates:  Resource ~ Difference From Median Group Brokerage Score') + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  ylab('') 


?scale_colour_manual


guides(colour=guide_legend(title = '',))


?guides



direct.label(p)
direct.label(qplot(score,data=loci,
                   color=type,geom="density"))


install.packages('directlabels')    
library(directlabels)
?direct.label
plot.df[plot.df$param=='diff.from.group.brok.median'&!duplicated(paste(plot.df$param,plot.df$dep.var)), ], 
aes(label = round(ests,2),x = ests*1.1,y=1,group=dep.var), hjust = 0.7,
vjust = 1)+
  
  
  
  
  
  
  
  
  
  test[test$dep.var=='face.to.face',]


param.ests[param.ests$dep.var=='face.to.face',]

rm(list=ls())
save.image('temp.qap.results.RData')








# 
# library(ggplot2)
# ggplot() + geom_histogram(aes(x=mean),
#             data=perm.mean.ests[perm.mean.ests$dep.var=='financial',],bins = 100) + 
#   facet_wrap(~param,scales = 'free')

