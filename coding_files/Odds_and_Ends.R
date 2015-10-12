dat = read.csv("/Users/TScott/Google Drive/PSP_Project/PS_Data/Group.All.Dat.csv")

data = subset(dat,dat$GroupID!="G.OTHER"&dat$GroupID!="G.OTHER.1"&dat$GroupID!="G.OTHER.2"&
dat$GroupID!="G.OTHER.3"&dat$GroupID!="G.OTHER.4"&dat$GroupID!="G.OTHER.5")

plot(0, xlim=c(0,1), ylim=c(0,1), type="n", axes=FALSE, xlab="", ylab="",main="Organizations and Collaborative Groups")

factor.to.int <- function(f) {
  (as.integer(f) - 1) / (length(levels(f)) - 1)
}

segments(factor.to.int(data$ORG), 0, factor.to.int(data$GroupID), 1, col=data$ORG)
axis(1, at = seq(0, 1, by = 1 / (length(levels(data$ORG)) - 1)), labels = levels(data$ORG))
axis(3, at = seq(0, 1, by = 1 / (length(levels(data$GroupID)[-(55:59)]) - 1)), labels = levels(data$GroupID)[-(55:59)])



unique(dat$GroupID)


data$GroupID!="G.OTHER.5"

data$GroupID=="G.OTHER.5"

levels(data$GroupID)[-(55:59)]
data$GroupID


 geom_rect(data = TextFrame, aes(xmin = X - w/2, xmax = X + w/2, 
    ymin = Y - h/2, ymax = Y + h/2), fill = "grey80") +
  geom_text(data = TextFrame,aes(x = X, y = Y, label = LAB), size = 4)
