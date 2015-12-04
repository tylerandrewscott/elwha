library(statnet)


OVER<-(as.matrix(all_group))
OVER.PSP<-(as.matrix(psp_group))
OVER.NPSP<-as.matrix(npsp_group)
#fit endogenous parameters
net_temp <- net_uq
tempform2<-net_temp ~ edges + mutual +twopath +isolates +gwidegree(0.25, fixed = TRUE)+
 gwesp(0.25, fixed = TRUE) + nodecov("NUMRESP") 
modtemp2<-ergm(tempform2,control=control.ergm(
Step.MCMC.samplesize=10000,
main.method="Stepping", MCMLE.metric="lognormal",
Step.gridsize=1000,
Step.maxit=20),seed=24)
mod_H1n = modtemp2

net_temp <- net_uq
tempform2<-net_temp ~ edges + mutual +twopath +isolates +gwidegree(0.25, fixed = TRUE)+
 gwesp(0.25, fixed = TRUE) + nodecov("NUMRESP") + edgecov(OVER)
modtemp2<-ergm(tempform2,control=control.ergm(
Step.MCMC.samplesize=10000,
main.method="Stepping", MCMLE.metric="lognormal",
Step.gridsize=1000,
Step.maxit=20),seed=24)
mod_H1a = modtemp2

net_temp <- net_imp
tempform2<-net_temp ~ edges + mutual +twopath +isolates +gwidegree(0.25, fixed = TRUE)+
 gwesp(0.25, fixed = TRUE) + nodecov("NUMRESP") + edgecov(OVER)
modtemp2<-ergm(tempform2,control=control.ergm(
Step.MCMC.samplesize=10000,
main.method="Stepping", MCMLE.metric="lognormal",
Step.gridsize=1000,
Step.maxit=20),seed=24)
mod_H1a_imp = modtemp2


net_temp <- net_imp
tempform2<-net_temp ~ edges + mutual +twopath +isolates +gwidegree(0.25, fixed = TRUE)+
 gwesp(0.25, fixed = TRUE) + nodecov("NUMRESP") + 
edgecov(net_cons)+edgecov(net_plan)+edgecov(OVER)
modtemp2<-ergm(tempform2,control=control.ergm(
Step.MCMC.samplesize=10000,
main.method="Stepping", MCMLE.metric="lognormal",
Step.gridsize=1000,
Step.maxit=20),seed=24)
mod_H1a_alltest_imp = modtemp2


net_temp <- net_plan
tempform2<-net_temp ~ edges + mutual +twopath +isolates +gwidegree(0.25, fixed = TRUE)+
 gwesp(0.25, fixed = TRUE) + nodecov("NUMRESP") + 
edgecov(net_imp)+edgecov(net_cons)+edgecov(OVER)
modtemp2<-ergm(tempform2,control=control.ergm(
Step.MCMC.samplesize=10000,
main.method="Stepping", MCMLE.metric="lognormal",
Step.gridsize=1000,
Step.maxit=20),seed=24)
mod_H1a_alltest_plan = modtemp2

summary(mod_H1a_alltest_imp)

summary(mod_H1a_alltest)

net_temp <- net_imp
tempform2<-net_temp ~ edges + mutual +twopath +isolates +gwidegree(0.25, fixed = TRUE)+
 gwesp(0.25, fixed = TRUE) + nodecov("NUMRESP")
 modtemp2<-ergm(tempform2,control=control.ergm(
Step.MCMC.samplesize=10000,
main.method="Stepping", MCMLE.metric="lognormal",
Step.gridsize=1000,
Step.maxit=20),seed=24)
mod_H1n_imp = modtemp2

net_temp <- net_plan
tempform2<-net_temp ~ edges + mutual +twopath +isolates +gwidegree(0.25, fixed = TRUE)+
 gwesp(0.25, fixed = TRUE) + nodecov("NUMRESP") + edgecov(OVER)
modtemp2<-ergm(tempform2,control=control.ergm(
Step.MCMC.samplesize=10000,
main.method="Stepping", MCMLE.metric="lognormal",
Step.gridsize=1000,
Step.maxit=20),seed=24)
mod_H1a_plan = modtemp2

net_temp <- net_plan
tempform2<-net_temp ~ edges + mutual +twopath +isolates +gwidegree(0.25, fixed = TRUE)+
 gwesp(0.25, fixed = TRUE) + nodecov("NUMRESP") 
modtemp2<-ergm(tempform2,control=control.ergm(
Step.MCMC.samplesize=10000,
main.method="Stepping", MCMLE.metric="lognormal",
Step.gridsize=1000,
Step.maxit=20),seed=24)
mod_H1n_plan = modtemp2

net_temp <- net_cons
tempform2<-net_temp ~ edges + mutual +twopath +isolates +gwidegree(0.25, fixed = TRUE)+
 gwesp(0.25, fixed = TRUE) + nodecov("NUMRESP") + edgecov(OVER)
modtemp2<-ergm(tempform2,control=control.ergm(
Step.MCMC.samplesize=10000,
main.method="Stepping", MCMLE.metric="lognormal",
Step.gridsize=1000,
Step.maxit=20),seed=24)
mod_H1a_cons = modtemp2

net_temp <- net_cons
tempform2<-net_temp ~ edges + mutual +twopath +isolates +gwidegree(0.25, fixed = TRUE)+
 gwesp(0.25, fixed = TRUE) + nodecov("NUMRESP")
modtemp2<-ergm(tempform2,control=control.ergm(
Step.MCMC.samplesize=10000,
main.method="Stepping", MCMLE.metric="lognormal",
Step.gridsize=1000,
Step.maxit=20),seed=24)
mod_H1n_cons = modtemp2



gof_H1n<-gof(mod_H1n,nsim=1000,control=control.gof.ergm(MCMC.init.maxedges=2000))
gof_H1n_imp<-gof(mod_H1n_imp,nsim=1000,control=control.gof.ergm(MCMC.init.maxedges=2000))
library(ggplot2)
gof_H1n_cons<-gof(mod_H1n_cons,nsim=1000,control=control.gof.ergm(MCMC.init.maxedges=2000))
gof_H1n_plan<-gof(mod_H1n_plan,nsim=1000,control=control.gof.ergm(MCMC.init.maxedges=2000))

dev.off()
par(mfrow=c(2,2),mar=c(2.5,2,1,1.5))
plot.gofobject2(gof_H1n,main="",cex.main=.8)



sim_H1n<-simulate.ergm(mod_H1n, burnin = 1e+5, interval = 1e+5,
nsim = 1000, verbose = TRUE,statsonly=TRUE,monitor=~triangle+edges)
sim_H1n_imp<-simulate.ergm(mod_H1n_imp, burnin = 1e+5, interval = 1e+5,
nsim = 1000, verbose = TRUE,statsonly=TRUE,monitor=~triangle+edges)
sim_H1n_plan<-simulate.ergm(mod_H1n_plan, burnin = 1e+5, interval = 1e+5,
nsim = 1000, verbose = TRUE,statsonly=TRUE,monitor=~triangle+edges)
sim_H1n_cons<-simulate.ergm(mod_H1n_cons, burnin = 1e+5, interval = 1e+5,
nsim = 1000, verbose = TRUE,statsonly=TRUE,monitor=~triangle+edges)




sim_H1n[[1]]~triangle
head(sim_H1n)

edge.count<-rep(0,1000)
for (i in 1:100)
{
edge.count[i]<-summary(sim_H1n[[i]]~edges)
}

triangle.count.imp<-rep(0,1000)
for (i in 1:1000)
{
triangle.count.imp[i]<-summary(sim_H1n_imp[[i]]~triangle)
}

edge.count.imp<-rep(0,1000)
for (i in 1:100)
{
edge.count.imp[i]<-summary(sim_H1n_imp[[i]]~edges)
}

triangle.count.plan<-rep(0,1000)
for (i in 1:1000)
{
triangle.count.plan[i]<-summary(sim_H1n_plan[[i]]~triangle)
}

edge.count.plan<-rep(0,1000)
for (i in 1:100)
{
edge.count.plan[i]<-summary(sim_H1n_plan[[i]]~edges)
}


triangle.count.cons<-rep(0,1000)
for (i in 1:1000)
{
triangle.count.cons[i]<-summary(sim_H1n_cons[[i]]~triangle)
}

triangle.count
summary(sim_H1n[[400]]~triangle)

edge.count.cons<-rep(0,1000)
for (i in 1:100)
{
edge.count.cons[i]<-summary(sim_H1n_cons[[i]]~edges)
}

edge.count.cons

library(ggplot2)
length(triangle.count)
length(edge.count)
sim.count<-data.frame(triangle.count,edge.count)
sim.stack<-stack(sim.count)
#sim.stack[,3]<-rep(c(rep("sim",1000),"obs"),2)

sim.stack$values
sim_H1n
sim.stack<-cbind(sim_H1n$edges,sim_H1n$triangle)
#colnames(sim.stack)[3]<-"which"



sim<-sim_H1n
sim.stack<-data.frame(c(sim[,1]))
colnames(sim.stack)<-"count"
sim.stack$structure<-c(rep("edges",length(sim[,1])))
sim.plot<-ggplot(sim.stack,aes(count,fill=structure))+
geom_bar(position="dodge",binwidth=10)+
labs(title="All Ties")+
xlab("Edge Count")+ylab("Number of Networks")+
scale_fill_discrete(name  ="Structure",
                            breaks=c("edge.count", "triangle.count"),
                            labels=c("Edges", "Two-Paths"))+
geom_segment(x=1045,xend=1045,y=0,yend=500,lty=2,lwd=2)

sim<-sim_H1n_imp
sim.stack<-data.frame(c(sim[,1]))
colnames(sim.stack)<-"count"
sim.stack$structure<-c(rep("edges",length(sim[,1])))
sim.plot.imp<-ggplot(sim.stack,aes(count,fill=structure))+
geom_bar(position="dodge",binwidth=10)+
labs(title="Implementation Ties")+
xlab("Edge Count")+ylab("Number of Networks")+
scale_fill_discrete(name  ="Structure",
                            breaks=c("edge.count", "triangle.count"),
                            labels=c("Edges", "Two-Paths"))+
geom_segment(x=615,xend=615,y=0,yend=500,lty=2,lwd=2)

sim<-sim_H1n_plan
sim.stack<-data.frame(c(sim[,1]))
colnames(sim.stack)<-"count"
sim.stack$structure<-c(rep("edges",length(sim[,1])))
sim.plot.plan<-ggplot(sim.stack,aes(count,fill=structure))+
geom_bar(position="dodge",binwidth=10)+
labs(title="Planning Ties")+
xlab("Edge Count")+ylab("Number of Networks")+
scale_fill_discrete(name  ="Structure",
                            breaks=c("edge.count", "triangle.count"),
                            labels=c("Edges", "Two-Paths"))+
geom_segment(x=463,xend=463,y=0,yend=500,lty=2,lwd=2)

sim<-sim_H1n_cons
sim.stack<-data.frame(c(sim[,1]))
colnames(sim.stack)<-"count"
sim.stack$structure<-c(rep("edges",length(sim[,1])))
sim.plot.cons<-ggplot(sim.stack,aes(count,fill=structure))+
geom_bar(position="dodge",binwidth=10)+
labs(title="Consultation Ties")+
xlab("Edge Count")+ylab("Number of Networks")+
scale_fill_discrete(name  ="Structure",
                            breaks=c("edge.count", "triangle.count"),
                            labels=c("Edges", "Two-Paths"))+
geom_segment(x=436,xend=436,y=0,yend=500,lty=2,lwd=2)



require(grid)
vp.layout <- function(x, y) viewport(layout.pos.row=x, layout.pos.col=y)
arrange_ggplot2 <- function(..., nrow=NULL, ncol=NULL, as.table=FALSE) {
	dots <- list(...)
	n <- length(dots)
	if(is.null(nrow) & is.null(ncol)) { nrow = floor(n/2) ; ncol = ceiling(n/nrow)}
	if(is.null(nrow)) { nrow = ceiling(n/ncol)}
	if(is.null(ncol)) { ncol = ceiling(n/nrow)}
        ## NOTE see n2mfrow in grDevices for possible alternative
grid.newpage()
pushViewport(viewport(layout=grid.layout(nrow,ncol) ) )
	ii.p <- 1
	for(ii.row in seq(1, nrow)){
	ii.table.row <- ii.row	
	if(as.table) {ii.table.row <- nrow - ii.table.row + 1}
		for(ii.col in seq(1, ncol)){
			ii.table <- ii.p
			if(ii.p > n) break
			print(dots[[ii.table]], vp=vp.layout(ii.table.row, ii.col))
			ii.p <- ii.p + 1
		}
	}
}



plot.all<-arrange_ggplot2(sim.plot,sim.plot.imp,sim.plot.plan,sim.plot.cons)
print(plot.all)
ggsave("H:/GitHub/elwha/ggplot edge sim count.png", arrange_ggplot2(sim.plot,sim.plot.imp,sim.plot.plan,sim.plot.cons)
, h = 9/2, w = 9/2, type = "cairo-png")


par(mfrow=c(2,2))
print(sim.plot.cons)
summary(net_uq~twopath)

net_plan
sim.stack$structure
summary(net_uq~triangles)



