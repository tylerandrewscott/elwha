rm(list=ls())
resp.bin.mat<-read.csv("//Users/TScott/Google Drive/elwha/Shared.Groups.csv")
resp.bin.mat.psp<-read.csv("//Users/TScott/Google Drive/elwha/Shared.Groups.Psp.csv")
resp.bin.mat.npsp<-read.csv("//Users/TScott/Google Drive/elwha/Shared.Groups.Npsp.csv")


sum(read.csv("//Users/TScott/Google Drive/elwha/Shared.Groups.Psp.csv")[,-1])
sum(read.csv("//Users/TScott/Google Drive/elwha/Shared.Groups.Npsp.csv")[,-1])
sum(read.csv("//Users/TScott/Google Drive/elwha/Shared.Groups.csv")[,-1])

#weighted indicator for each organization in sociomatrix

resp.weight.mat<-matrix(data=0,nrow=nrow(group.byresp.full),ncol=nrow(group.byresp.full))
colnames(resp.weight.mat)<-group.byresp.full$ORG
rownames(resp.weight.mat)<-group.byresp.full$ORG
grp.name.ref<-colnames(group.byresp.full)[2:(which(colnames(group.byresp.full)=="G.OTHER.1")-1)]

colnames(group.byresp.full)[2:(which(colnames(group.byresp.full)=="G.OTHER.1")-1)]

colnames(group.byresp.full)

resp.weight.mat.psp<-matrix(data=0,nrow=nrow(group.byresp.full.psp),ncol=nrow(group.byresp.full.psp))
colnames(resp.weight.mat.psp)<-group.byresp.full.psp$ORG
rownames(resp.weight.mat.psp)<-group.byresp.full.psp$ORG
grp.name.ref.psp<-colnames(group.byresp.full.psp)[2:(which(colnames(group.byresp.full.psp)=="G.OTHER.1")-1)]

resp.weight.mat.npsp<-matrix(data=0,nrow=nrow(group.byresp.full.npsp),ncol=nrow(group.byresp.full.npsp))
colnames(resp.weight.mat.npsp)<-group.byresp.full.npsp$ORG
rownames(resp.weight.mat.npsp)<-group.byresp.full.npsp$ORG
grp.name.ref.npsp<-colnames(group.byresp.full.npsp)[2:(which(colnames(group.byresp.full.npsp)=="G.OTHER.1")-1)]

#turn on test
#test1<-resp.weight.mat[1:2,1:2];test1
#test2<-group.byresp.full[1:2,];test2
#resp.weight.mat<-test1
#group.byresp.full<-test2

###
#reset group.byresp.full to real values

#for (i in 1:nrow(resp.weight.mat))
for (i in 1:nrow(resp.weight.mat))
{
#print("i");print(i);
(vec.i<-(as.numeric(group.byresp.full[i,2:(which(colnames(group.byresp.full)=="G.OTHER.1")-1)])))
#print("vec.i before"); print(vec.i)
(for (k in 1:length(vec.i))
			{
				#print("k") ; print(k)
				if (vec.i[k]>0)
				{id.pull1<-group.byresp.full$ID[i]
					#print("id.pull1");print(id.pull1)
				groupn.pull1<-grp.name.ref[k]
				#print("groupn.pull1");print(groupn.pull1)
				specific.group1<-subset(group.dat.byedge,group.dat.byedge$ID==as.character(id.pull1))	
				#print("specific.group1");print(specific.group1)				
				specific.group2<-subset(specific.group1,specific.group1$GroupID==as.character(groupn.pull1))
				#print("specific.group2");print(specific.group2)
				if (nrow(specific.group2)!=0)
					{part.sum<-specific.group2$part.sum[1]
					part.rat<-part.sum/7 
					#print("part.rat");print(part.rat)
					vec.i[k]<-part.rat 
					#print("vec.i[k]");print(vec.i[k])
}}})
#print("vec.i after"); print(vec.i)	
for (j in 1:nrow(resp.weight.mat))
{
	#print("j");print(j);
(vec.j<-(as.numeric(group.byresp.full[j,2:(which(colnames(group.byresp.full)=="G.OTHER.1")-1)])))
#print("vec.j before");print(vec.j)
(for (l in 1:length(vec.j))
	{if (vec.j[l]>0)
			{id.pull2<-group.byresp.full$ID[j]
			#print("id.pull2");print(id.pull2)
			groupn.pull2<-grp.name.ref[l]
			#print("groupn.pull2");print(groupn.pull2)
			specific.group3<-subset(group.dat.byedge,group.dat.byedge$ID==as.character(id.pull2))
			#print("specific.group3");print(specific.group3)							
			specific.group4<-subset(specific.group3,specific.group3$GroupID==as.character(groupn.pull2))
			#print("specific.group4");print(specific.group4)
			if (nrow(specific.group4)!=0)
			{part.sum<-specific.group4$part.sum[1]
			part.rat<-part.sum/7 
			#;print(part.rat);print("part.rat")
			vec.j[l]<-part.rat 
			#;print(vec.j[l]);print("vec.j[l]")
			}}})
#print("vec.j after");print(vec.j)	
			
	
	min.combs<-pmin(vec.i,vec.j)
	#print("min.combs");print(min.combs)
	min.vec<-min.combs  
	#print("min.vec");print(min.vec)
	resp.weight.mat[i,j]<-sum(min.vec)
	#print("sum(min.vec)");print(sum(min.vec))	
	#print(resp.weight.mat[i,j]);print("resp.weight.mat[i,j]")	
}
#print("iteration i done");print(i);print("#####")
}

write.csv(resp.weight.mat,file="//Users/TScott/Google Drive/elwha/Shared.Groups.Weighted.csv")

for (i in 1:nrow(resp.weight.mat.psp))
{
#print("i");print(i);
(vec.i<-(as.numeric(group.byresp.full.psp[i,2:(which(colnames(group.byresp.full.psp)=="G.OTHER.1")-1)])))
#print("vec.i before"); print(vec.i)
(for (k in 1:length(vec.i))
			{
				#print("k") ; print(k)
				if (vec.i[k]>0)
				{id.pull1<-group.byresp.full.psp$ID[i]
					#print("id.pull1");print(id.pull1)
				groupn.pull1<-grp.name.ref.psp[k]
				#print("groupn.pull1");print(groupn.pull1)
				specific.group1<-subset(group.dat.byedge.psp,group.dat.byedge.psp$ID==as.character(id.pull1))	
				#print("specific.group1");print(specific.group1)				
				specific.group2<-subset(specific.group1,specific.group1$GroupID==as.character(groupn.pull1))
				#print("specific.group2");print(specific.group2)
				if (nrow(specific.group2)!=0)
					{part.sum<-specific.group2$part.sum[1]
					part.rat<-part.sum/7 
					#print("part.rat");print(part.rat)
					vec.i[k]<-part.rat 
					#print("vec.i[k]");print(vec.i[k])
}}})
#print("vec.i after"); print(vec.i)	
for (j in 1:nrow(resp.weight.mat.psp))
{
	#print("j");print(j);
(vec.j<-(as.numeric(group.byresp.full.psp[j,2:(which(colnames(group.byresp.full.psp)=="G.OTHER.1")-1)])))
#print("vec.j before");print(vec.j)
(for (l in 1:length(vec.j))
	{if (vec.j[l]>0)
			{id.pull2<-group.byresp.full.psp$ID[j]
			#print("id.pull2");print(id.pull2)
			groupn.pull2<-grp.name.ref.psp[l]
			#print("groupn.pull2");print(groupn.pull2)
			specific.group3<-subset(group.dat.byedge.psp,group.dat.byedge.psp$ID==as.character(id.pull2))
			#print("specific.group3");print(specific.group3)							
			specific.group4<-subset(specific.group3,specific.group3$GroupID==as.character(groupn.pull2))
			#print("specific.group4");print(specific.group4)
			if (nrow(specific.group4)!=0)
			{part.sum<-specific.group4$part.sum[1]
			part.rat<-part.sum/7 
			#;print(part.rat);print("part.rat")
			vec.j[l]<-part.rat 
			#;print(vec.j[l]);print("vec.j[l]")
			}}})
#print("vec.j after");print(vec.j)	
			
	
	min.combs<-pmin(vec.i,vec.j)
	#print("min.combs");print(min.combs)
	min.vec<-min.combs  
	#print("min.vec");print(min.vec)
	resp.weight.mat.psp[i,j]<-sum(min.vec)
	#print("sum(min.vec)");print(sum(min.vec))	
	#print(resp.weight.mat[i,j]);print("resp.weight.mat[i,j]")	
}
print("iteration i done");print(i);print("#####")
}

write.csv(resp.weight.mat.psp,file="//Users/TScott/Google Drive/elwha/Shared.Groups.Weighted.Psp.csv")


for (i in 1:nrow(resp.weight.mat.npsp))
{
#print("i");print(i);
(vec.i<-(as.numeric(group.byresp.full.npsp[i,2:(which(colnames(group.byresp.full.npsp)=="G.OTHER.1")-1)])))
#print("vec.i before"); print(vec.i)
(for (k in 1:length(vec.i))
			{
				#print("k") ; print(k)
				if (vec.i[k]>0)
				{id.pull1<-group.byresp.full.npsp$ID[i]
					#print("id.pull1");print(id.pull1)
				groupn.pull1<-grp.name.ref.npsp[k]
				#print("groupn.pull1");print(groupn.pull1)
				specific.group1<-subset(group.dat.byedge.npsp,group.dat.byedge.npsp$ID==as.character(id.pull1))	
				#print("specific.group1");print(specific.group1)				
				specific.group2<-subset(specific.group1,specific.group1$GroupID==as.character(groupn.pull1))
				#print("specific.group2");print(specific.group2)
				if (nrow(specific.group2)!=0)
					{part.sum<-specific.group2$part.sum[1]
					part.rat<-part.sum/7 
					#print("part.rat");print(part.rat)
					vec.i[k]<-part.rat 
					#print("vec.i[k]");print(vec.i[k])
}}})
#print("vec.i after"); print(vec.i)	
for (j in 1:nrow(resp.weight.mat.npsp))
{
	#print("j");print(j);
(vec.j<-(as.numeric(group.byresp.full.npsp[j,2:(which(colnames(group.byresp.full.npsp)=="G.OTHER.1")-1)])))
#print("vec.j before");print(vec.j)
(for (l in 1:length(vec.j))
	{if (vec.j[l]>0)
			{id.pull2<-group.byresp.full.npsp$ID[j]
			#print("id.pull2");print(id.pull2)
			groupn.pull2<-grp.name.ref.npsp[l]
			#print("groupn.pull2");print(groupn.pull2)
			specific.group3<-subset(group.dat.byedge.npsp,group.dat.byedge.npsp$ID==as.character(id.pull2))
			#print("specific.group3");print(specific.group3)							
			specific.group4<-subset(specific.group3,specific.group3$GroupID==as.character(groupn.pull2))
			#print("specific.group4");print(specific.group4)
			if (nrow(specific.group4)!=0)
			{part.sum<-specific.group4$part.sum[1]
			part.rat<-part.sum/7 
			#;print(part.rat);print("part.rat")
			vec.j[l]<-part.rat 
			#;print(vec.j[l]);print("vec.j[l]")
			}}})
#print("vec.j after");print(vec.j)	
			
	
	min.combs<-pmin(vec.i,vec.j)
	#print("min.combs");print(min.combs)
	min.vec<-min.combs  
	#print("min.vec");print(min.vec)
	resp.weight.mat.npsp[i,j]<-sum(min.vec)
	#print("sum(min.vec)");print(sum(min.vec))	
	#print(resp.weight.mat[i,j]);print("resp.weight.mat[i,j]")	
}
print("iteration i done");print(i);print("#####")
}

write.csv(resp.weight.mat.npsp,file="//Users/TScott/Google Drive/elwha/Shared.Groups.Weighted.Npsp.csv")

