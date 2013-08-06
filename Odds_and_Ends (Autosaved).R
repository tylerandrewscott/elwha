dat = read.csv("/Users/TScott/Google Drive/PSP_Project/PS_Data/Group.All.Dat.csv")

data = subset(dat,dat$GroupID!="G.OTHER"&dat$GroupID!="G.OTHER.1"&dat$GroupID!="G.OTHER.2"&
dat$GroupID!="G.OTHER.3"&dat$GroupID!="G.OTHER.4"&dat$GroupID!="G.OTHER.5")

plot(0, xlim=c(0,1), ylim=c(0,1), type="n", axes=FALSE, xlab="", ylab="")

factor.to.int <- function(f) {
  (as.integer(f) - 1) / (length(levels(f)) - 1)
}

segments(factor.to.int(data$ORG), 0, factor.to.int(data$GroupID), 1, col=data$ORG)
axis(1, at = seq(0, 1, by = 10 / (length(levels(data$ORG)) - 1)), labels = sample(levels(dat$ORG),22),size=.9)
axis(3, at = seq(0, 1, by = 10 / (length(levels(data$GroupID)[-(55:59)]) - 1)), labels = sample(levels(data$GroupID)[-(55:59)],7),size=.9)
title(main="Groups",sub="Organizations",outer=TRUE)


orgsamp = sample(data$ORG,10)
grpsamp = sample(data$GroupID,7)
print(grpsamp)

"Skagit MRC","PSP Science Panel","Stillaguamish Watershed","King County ECO Net","Nisqually"
unique(dat$GroupID)


data$GroupID!="G.OTHER.5"

data$GroupID=="G.OTHER.5"

levels(data$GroupID)[-(55:59)]
data$GroupID

