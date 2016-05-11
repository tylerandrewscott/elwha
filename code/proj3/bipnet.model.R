rm(list=ls())
library(plyr)
library(dplyr)
library(tidyr)
library(data.table)
library(stringr)
library(knitr)
library(magrittr)
library(scales)
library(ggplot2)
library(parallel)
library(gdata)
require(ggmcmc)
library("network") # tested with version 1.11.3
library("statnet") # tested with version 2014.2.0
library("xergm") # tested with version 1.4.13
library("texreg")# tested with version 1.35.2


remote=TRUE
#read in raw data
if(remote)
{uq.org.group.resource = read.csv('../../input/proj3/resource.access.long.file.csv',row.names=1)}
if(!remote)
{uq.org.group.resource = read.csv('input/proj3/resource.access.long.file.csv',row.names=1)}


uq.org = uq.org.group.resource %>% 
  select(-Group,-Short,-Increase,-Response.Option,-diff.from.group.brok.median,-bimodal.brokerage.score,-ties.to.other.members,-brokerage.score,-Participation,-group.resource) %>% .[!duplicated(.),]

uq.org.group = uq.org.group.resource %>% select(-Short,-Increase,-Response.Option,-group.resource) %>% .[!duplicated(.),]

b1.names = sort(unique(uq.org$uq.ID))
b2.names = sort(unique(uq.org.group$Group))
nb1 = length(b1.names)
nb2 = length(b2.names)

base.matrix = matrix(NA,ncol=nb2,nrow=nb1,
                     dimnames = list(b1.names,b2.names))

#Create matrix of group membership status
membership.matrix = base.matrix
for (i in 1:nrow(uq.org.group)){
membership.matrix[match(uq.org.group$uq.ID[i],rownames(membership.matrix)),match(uq.org.group$Group[i],colnames(membership.matrix))] <- 1
}
membership.matrix[is.na(membership.matrix)] <- 0
#invert to create non-membership matrix
nonmembership.matrix = abs(membership.matrix - 1)

#Create matrix for participation level
participation.matrix = base.matrix
for (i in 1:nrow(uq.org.group)){
  participation.matrix[match(uq.org.group$uq.ID[i],rownames(participation.matrix)),match(uq.org.group$Group[i],colnames(participation.matrix))] <- uq.org.group$Participation[i]
}
participation.matrix[is.na(participation.matrix)] <- 0

#Create matrix for brokerage level (for each group in which participates)
brokerage.matrix = base.matrix
for (i in 1:nrow(uq.org.group)){
  brokerage.matrix[match(uq.org.group$uq.ID[i],rownames(brokerage.matrix)),match(uq.org.group$Group[i],colnames(brokerage.matrix))] <- uq.org.group$brokerage.score[i]
}
brokerage.matrix[is.na(brokerage.matrix)] <- 0

#Create matrix for bimodal bimod.brokerage level (for each group in which participates)
bimod.brokerage.matrix = base.matrix
for (i in 1:nrow(uq.org.group)){
  bimod.brokerage.matrix[match(uq.org.group$uq.ID[i],rownames(bimod.brokerage.matrix)),match(uq.org.group$Group[i],colnames(bimod.brokerage.matrix))] <- uq.org.group$bimodal.brokerage.score[i]
}
bimod.brokerage.matrix[is.na(bimod.brokerage.matrix)] <- 0

#Create matrix for ties to other group members
ties.to.members.matrix = base.matrix
for (i in 1:nrow(uq.org.group)){
  ties.to.members.matrix[match(uq.org.group$uq.ID[i],rownames(ties.to.members.matrix)),match(uq.org.group$Group[i],colnames(ties.to.members.matrix))] <- uq.org.group$ties.to.other.members[i]
}
ties.to.members.matrix[is.na(ties.to.members.matrix)] <- 0

#Create matrix for diff from group brok median
diff.from.brok.med.matrix = base.matrix
for (i in 1:nrow(uq.org.group)){
  diff.from.brok.med.matrix[match(uq.org.group$uq.ID[i],rownames(diff.from.brok.med.matrix)),match(uq.org.group$Group[i],colnames(diff.from.brok.med.matrix))] <- uq.org.group$diff.from.group.brok.median[i]
}
diff.from.brok.med.matrix[is.na(diff.from.brok.med.matrix)] <- 0

#Create matrix for length of time at org
year.at.org = base.matrix
for (i in 1:nrow(year.at.org))
{
  year.at.org[i,] <- uq.org$years.at.org[match(rownames(year.at.org)[i],uq.org$uq.ID)]
}

#Create matrix for reason for participation
reason.for.membership = base.matrix
for (i in 1:nrow(reason.for.membership))
{
  reason.for.membership[i,] <- as.character(uq.org$Reason[match(rownames(reason.for.membership)[i],uq.org$uq.ID)])
}


number.of.groups = base.matrix
for (i in 1:nrow(number.of.groups))
{
  number.of.groups[i,] <- uq.org$Number.Of.Groups[match(rownames(year.at.org)[i],uq.org$uq.ID)]
}

number.of.members = base.matrix
for (i in 1:ncol(number.of.members))
{
  number.of.members[,i] = colSums(membership.matrix)[i]
}

#Create matrix for prop. of members that report having access
prop.group.access = data.frame(tapply(uq.org.group.resource$Increase,paste(uq.org.group.resource$Group,uq.org.group.resource$Short),mean))
vars = c('financial','human','technical')
temp.group.prop.access = replicate(length(vars),base.matrix)
dimnames(temp.group.prop.access)[[3]] <- vars
resource.prop = uq.org.group.resource %>% group_by(Group,Short) %>% summarise(Prop.Increase = mean(Increase))
group.res.prop.array = replicate(length(vars),base.matrix)
dimnames(group.res.prop.array)[[3]] = vars
for (v in vars)
{
  temp = filter(resource.prop,Short == v)
  temp.mat = matrix(rep(temp$Prop.Increase[match(colnames(base.matrix), temp$Group)],each = nb1),byrow=F,ncol=nb2)
  group.res.prop.array[,,v] = temp.mat
}


ties.to.members.matrix = base.matrix
for (i in 1:nrow(ties.to.members.matrix))
  {
  ties.to.members.matrix[match(uq.org.group$uq.ID[i],rownames(ties.to.members.matrix)),match(uq.org.group$Group[i],colnames(ties.to.members.matrix))] <- uq.org.group$ties.to.other.members[i]
}
ties.to.members.matrix[is.na(ties.to.members.matrix)] <- 0


# base.df$Response.Factor = base.df$Response.Option
# base.df$Response.Factor[base.df$Response.Factor=="na"] = NA
# base.df$Response.Factor[base.df$Response.Factor=="don't know"] = 'neither agree nor disagree'
# response.levels = c("strongly disagree","disagree","neither agree nor disagree",
#               "agree","strongly agree")



face.to.face.matrix = base.matrix
temp = uq.org.group.resource %>% filter(Short == 'face.to.face')
  for (i in 1:nrow(temp))
  {
    face.to.face.matrix[match(temp$uq.ID[i],rownames(face.to.face.matrix)),match(temp$Group[i],colnames(face.to.face.matrix))] <- temp$Increase[i]
  }
face.to.face.matrix[is.na(face.to.face.matrix)] <- 0


understanding.matrix = base.matrix
temp = uq.org.group.resource %>% filter(Short == 'understanding')
for (i in 1:nrow(temp))
{
  understanding.matrix[match(temp$uq.ID[i],rownames(understanding.matrix)),match(temp$Group[i],colnames(understanding.matrix))] <- temp$Increase[i]
}
understanding.matrix[is.na(understanding.matrix)] <- 0

awareness.matrix = base.matrix
temp = uq.org.group.resource %>% filter(Short == 'awareness')
for (i in 1:nrow(temp))
{
  awareness.matrix[match(temp$uq.ID[i],rownames(awareness.matrix)),match(temp$Group[i],colnames(awareness.matrix))] <- temp$Increase[i]
}
awareness.matrix[is.na(awareness.matrix)] <- 0

#registerDoParallel(cores=16)

resource.matrix.array = replicate(length(vars),base.matrix)
dimnames(resource.matrix.array)[[3]] <- as.character(vars)

for (v in unlist(dimnames(resource.matrix.array)[[3]]))
{
  temp = filter(uq.org.group.resource,Short == v)
  temp.mat = base.matrix
  for (i in 1:nrow(temp))
  {temp.mat[match(temp$uq.ID[i],rownames(base.matrix)),match(temp$Group[i],colnames(base.matrix))] <- temp$Increase[i]}
  temp.mat[is.na(temp.mat)] <- 0
  resource.matrix.array[,,unlist(dimnames(resource.matrix.array)[3])==v] <- temp.mat
}

network.list = list()
for (v in unlist(dimnames(resource.matrix.array)[3]))
{
  network.list[[v]] = as.network.matrix(resource.matrix.array[,,unlist(dimnames(resource.matrix.array)[3])==v],matrix.type='adjacency',bipartite=TRUE,directed=FALSE)
}


group.prop.financial = group.res.prop.array[,,'financial']
group.prop.human = group.res.prop.array[,,'human']
group.prop.technical = group.res.prop.array[,,'technical']

network.financial = network.list['financial'][[1]]
network.human = network.list['human'][[1]]
network.technical = network.list['technical'][[1]]



#Participation + Number.Of.Groups + diff.from.group.brok.median + brokerage.score + bimodal.brokerage.score + ties.to.other.members + f(uq.ID,model='iid') + f(Group,model='iid'),

#set blockdiagonal dyads = 1, 0 elsewhere, and by constraining the coefficient corresponding to this covariate to be infinitely small.

burnin <- 10000    # MCMC burnin
sampsize <- 50000  # MCMC sample size
maxit <- 200       # number of MCMC MLE iterations
nsim <- 100000       # number of simulated networks for the GOF assessment
seed <- 24 # random seed for exact reproducibility
interval = 1500
set.seed(seed)

# endog.form =  x ~ edges +    
#   #b1star(2) +    
#   #b2star(2) +  
#   gwb1degree(0.25,fixed=TRUE) +
#   gwb2degree(0.25,fixed=TRUE) +
#   edgecov(participation.matrix) +
#   edgecov(brokerage.matrix) +
#   edgecov(bimod.brokerage.matrix) +
#   edgecov(diff.from.brok.med.matrix)+
#   edgecov(ties.to.members.matrix)
#   edgecov(group.res.prop.array[,,])
#   offset(edgecov(nonmembership.matrix)) 

custom.control = control.ergm(MCMC.burnin = burnin, MCMC.interval = interval, MCMLE.trustregion = 100,
                                MCMC.samplesize = sampsize, init = c(rep(0,9),-Inf),
                              seed = seed, MCMLE.maxit = maxit)#,parallel = 4)


library(Bergm)
test = Bergm::bergm(network.financial ~ edges + 
                      edgecov(participation.matrix) + edgecov(year.at.org) +
                      #H1
                      b1star(2) +
                      #H3
                      edgecov(principled.engagement) +
#H4a
edgecov(number.of.groups) +
  #H4b
  edgecov(number.of.members) +
  #H5
  edgecov(brokerage.matrix) +
  #H6
  edgecov(ties.to.members.matrix) +
  offset(edgecov(nonmembership.matrix)), offset.coef=-Inf,cust)






brokerage.matrix = sqrt(brokerage.matrix)
bimod.brokerage.matrix = sqrt(bimod.brokerage.matrix)
principled.engagement = face.to.face.matrix + awareness.matrix + understanding.matrix




control.ergm

form.0 = '
edges + offset(edgecov(nonmembership.matrix))'

form.1 = '
edges + 
edgecov(participation.matrix) + edgecov(year.at.org) +
#H1
b1star(2) +
#H3
edgecov(principled.engagement) +
#H4a
edgecov(number.of.groups) +
#H4b
edgecov(number.of.members) +
#H5
edgecov(brokerage.matrix) +
#H6
edgecov(ties.to.members.matrix) +
offset(edgecov(nonmembership.matrix))'


form.2 = '
edges + edgecov(participation.matrix) + edgecov(year.at.org) +
#H2
b2star(2) + 
#H3
edgecov(principled.engagement) +
#H4a
edgecov(number.of.groups) +
#H4b
edgecov(number.of.members) +
#H5
edgecov(brokerage.matrix) +
#H6
edgecov(ties.to.members.matrix) +
offset(edgecov(nonmembership.matrix))'

verb = FALSE
getloglik = TRUE

#mod.tech0 = ergm(as.formula(paste0('network.technical ~', form.0)),eval.loglik=getloglik,offset.coef=-Inf,control = custom.control,verbose=verb)
mod.tech1 = ergm(as.formula(paste0('network.technical ~', form.1)),eval.loglik=getloglik,offset.coef=-Inf,control = custom.control,verbose=verb)
#mod.tech2 = ergm(as.formula(paste0('network.technical ~', form.2)),eval.loglik=getloglik,offset.coef=-Inf,control = custom.control,verbose=verb)
#mod.tech3 = ergm(as.formula(paste0('network.technical ~', form.3)),eval.loglik=getloglik,offset.coef=-Inf,control = custom.control,verbose=verb)
#mod.tech4 = ergm(as.formula(paste0('network.technical ~', form.4)),eval.loglik=getloglik,offset.coef=-Inf,control = custom.control,verbose=verb)
#mod.tech5 = ergm(as.formula(paste0('network.technical ~', form.5)),eval.loglik=getloglik,offset.coef=-Inf,control = custom.control,verbose=verb)

#mod.fin0 = ergm(as.formula(paste0('network.financial ~', form.0)),eval.loglik=getloglik,offset.coef=-Inf,control = custom.control,verbose=verb)
mod.fin1 = ergm(as.formula(paste0('network.financial ~', form.1)),eval.loglik=getloglik,offset.coef=-Inf,control = custom.control,verbose=verb)
#mod.fin2 = ergm(as.formula(paste0('network.financial ~', form.2)),eval.loglik=getloglik,offset.coef=-Inf,control = custom.control,verbose=verb)
#mod.fin3 = ergm(as.formula(paste0('network.financial ~', form.3)),eval.loglik=getloglik,offset.coef=-Inf,control = custom.control,verbose=verb)
#mod.fin4 = ergm(as.formula(paste0('network.financial ~', form.4)),eval.loglik=getloglik,offset.coef=-Inf,control = custom.control,verbose=verb)
#mod.fin5 = ergm(as.formula(paste0('network.financial ~', form.5)),eval.loglik=getloglik,offset.coef=-Inf,control = custom.control,verbose=verb)

#mod.hum0 = ergm(as.formula(paste0('network.human ~', form.0)),eval.loglik=getloglik,offset.coef=-Inf,control = custom.control,verbose=verb)
mod.hum1 = ergm(as.formula(paste0('network.human ~', form.1)),eval.loglik=getloglik,offset.coef=-Inf,control = custom.control,verbose=verb)
#mod.hum2 = ergm(as.formula(paste0('network.human ~', form.2)),eval.loglik=getloglik,offset.coef=-Inf,control = custom.control,verbose=verb)
#mod.hum3 = ergm(as.formula(paste0('network.human ~', form.3)),eval.loglik=getloglik,offset.coef=-Inf,control = custom.control,verbose=verb)
#mod.hum4 = ergm(as.formula(paste0('network.human ~', form.4)),eval.loglik=getloglik,offset.coef=-Inf,control = custom.control,verbose=verb)
#mod.hum5 = ergm(as.formula(paste0('network.human ~', form.5)),eval.loglik=getloglik,offset.coef=-Inf,control = custom.control,verbose=verb)





#mcmc.tech0 = ggs(mod.tech0$sample)
mcmc.tech1 = ggs(mod.tech1$sample)
#mcmc.tech2 = ggs(mod.tech2$sample)
#mcmc.tech3 = ggs(mod.tech3$sample)
#mcmc.tech4 = ggs(mod.tech4$sample)
#mcmc.tech5 = ggs(mod.tech5$sample)

#mcmc.fin0 = ggs(mod.fin0$sample)
mcmc.fin1 = ggs(mod.fin1$sample)
#mcmc.fin2 = ggs(mod.fin2$sample)
#mcmc.fin3 = ggs(mod.fin3$sample)
#mcmc.fin4 = ggs(mod.fin4$sample)
#mcmc.fin5 = ggs(mod.fin5$sample)

#mcmc.hum0 = ggs(mod.hum0$sample)
mcmc.hum1 = ggs(mod.hum1$sample)
#mcmc.hum2 = ggs(mod.hum2$sample)
#mcmc.hum3 = ggs(mod.hum3$sample)
#mcmc.hum4 = ggs(mod.hum4$sample)
#mcmc.hum5 = ggs(mod.hum5$sample)


finNA <- as.matrix(network.financial)
finNA[membership.matrix == 0] <-NA

humNA <- as.matrix(network.human)
humNA[membership.matrix == 0] <-NA

techNA <- as.matrix(network.technical)
techNA[membership.matrix == 0] <-NA

compute.rocprgof = TRUE

gf.fin0 <- gof(mod.fin0, nsim = nsim, target = finNA,  statistics = 'ideg',
                coef = mod.fin0$coef[names(mod.fin0$coef)!='edgecov.nonmembership.matrix'],
                rocprgof = compute.rocprgof, checkdegeneracy = FALSE)
 gf.fin1 <- gof(mod.fin1, nsim = nsim, target = finNA, statistics = 'ideg',
                coef = mod.fin1$coef[names(mod.fin1$coef)!='edgecov.nonmembership.matrix'],
             rocprgof = compute.rocprgof, checkdegeneracy = FALSE)
 gf.fin2 <- gof(mod.fin2, nsim = nsim, target = finNA,statistics = 'ideg',
                coef = mod.fin2$coef[names(mod.fin2$coef)!='edgecov.nonmembership.matrix'],
               rocprgof = compute.rocprgof, checkdegeneracy = FALSE)
 # gf.fin3 <- gof(mod.fin3, nsim = nsim, target = finNA,  GOF = ~ degree,
 #                coef = mod.fin3$coef[names(mod.fin3$coef)!='edgecov.nonmembership.matrix'],
 #               rocprgof = compute.rocprgof, checkdegeneracy = FALSE)
 # gf.fin4 <- gof(mod.fin4, nsim = nsim, target = finNA,  
 #                coef = mod.fin4$coef[names(mod.fin4$coef)!='edgecov.nonmembership.matrix'],
 #               rocprgof = compute.rocprgof, checkdegeneracy = FALSE)

 gf.tech0 <- gof(mod.tech0, nsim = nsim, target = techNA,statistics = 'ideg',
                 coef = mod.tech0$coef[names(mod.tech0$coef)!='edgecov.nonmembership.matrix'],
                 rocprgof = compute.rocprgof, checkdegeneracy = FALSE)
 gf.tech1 <- gof(mod.tech1, nsim = nsim, target = techNA, statistics = 'ideg',
                coef = mod.tech1$coef[names(mod.tech1$coef)!='edgecov.nonmembership.matrix'],
                rocprgof = compute.rocprgof, checkdegeneracy = FALSE)
 gf.tech2 <- gof(mod.tech2, nsim = nsim, target = techNA,statistics = 'ideg',
                coef = mod.tech2$coef[names(mod.tech2$coef)!='edgecov.nonmembership.matrix'],
                rocprgof = compute.rocprgof, checkdegeneracy = FALSE)
 # gf.tech3 <- gof(mod.tech3, nsim = nsim, target = techNA,  
 #                coef = mod.tech3$coef[names(mod.tech3$coef)!='edgecov.nonmembership.matrix'],
 #                rocprgof = compute.rocprgof, checkdegeneracy = FALSE)
 # gf.tech4 <- gof(mod.tech4, nsim = nsim, target = techNA,  
 #                coef = mod.tech4$coef[names(mod.tech4$coef)!='edgecov.nonmembership.matrix'],
 #                rocprgof = compute.rocprgof, checkdegeneracy = FALSE)

 gf.hum0 <- gof(mod.hum0, nsim = nsim, target = humNA,statistics = 'ideg',
                coef = mod.hum0$coef[names(mod.hum0$coef)!='edgecov.nonmembership.matrix'],
                rocprgof = compute.rocprgof, checkdegeneracy = FALSE)
 gf.hum1 <- gof(mod.hum1, nsim = nsim, target = humNA,  statistics = 'ideg',
                coef = mod.hum1$coef[names(mod.hum1$coef)!='edgecov.nonmembership.matrix'],
                rocprgof = compute.rocprgof, checkdegeneracy = FALSE)
 gf.hum2 <- gof(mod.hum2, nsim = nsim, target = humNA,statistics = 'ideg',
                coef = mod.hum2$coef[names(mod.hum2$coef)!='edgecov.nonmembership.matrix'],
                rocprgof = compute.rocprgof, checkdegeneracy = FALSE)
 # gf.hum3 <- gof(mod.hum3, nsim = nsim, target = humNA,  
 #                coef = mod.hum3$coef[names(mod.hum3$coef)!='edgecov.nonmembership.matrix'],
 #                rocprgof = compute.rocprgof, checkdegeneracy = FALSE)
 # gf.hum4 <- gof(mod.hum4, nsim = nsim, target = humNA,  
 #                coef = mod.hum4$coef[names(mod.hum4$coef)!='edgecov.nonmembership.matrix'],
 #                rocprgof = compute.rocprgof, checkdegeneracy = FALSE)

save.image('modtemp4.RData')


