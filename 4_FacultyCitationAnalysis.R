install.packages("statnet")
install.packages("foreign")
library(statnet)
library(foreign)


faculty<-read.csv("http://michaelbrown.work/wp-content/uploads/2016/06/Faculty-Adjacency-Matrix.csv", header=T, sep=",")
#Right now we have an adjacency matrix

#if you already have the network in a network data file format like UCINET or Pajek you can read it in like:
#faculty<-read.paj("CSHPE Extracted Co-Authorship Network.net")

#Get some basic information about the data
summary(faculty)

#Well....that wasn't useful 
#Let's make this an actual network
faculty<-as.network(faculty)
summary(faculty)

plot.network(faculty, displaylabels=F)
attributes(faculty)
network.dyadcount(faculty)                                    # How many dyads in nflo?
#10506 dyads
network.edgecount(faculty)                                    # How many edges are present?
#486 edges
network.size(faculty)    
#102 authors
gplot(faculty)

nodeInfo<-read.csv("http://michaelbrown.work/wp-content/uploads/2016/06/nodeInfo.csv", header=T, sep=",")

sum(nodeInfo$number_of_authored_works)
nodeInfo$works<-nodeInfo$number_of_authored_works^2/255

pdf("Faculty Collaboration Network 5.18.16.pdf")
plot.network(faculty, displaylabels=FALSE, displayisolates=TRUE, arrowhead.cex=.5, 
             label.cex=.5, vertex.cex=nodeInfo$works, edge.col=8, label.col=(1), vertex.col=9, 
             label.border=1, vertex.border=1, mode="fruchtermanreingold",  sub="Faculty Collaboration Network")
dev.off()

#Density
gden(faculty)
#2.3% denstiy

nnet<-faculty

#Degree Centrality
degree(nnet)
deg <- degree(nnet)  
ideg <- degree(nnet, cmode="indegree")                    # Indegree 
odeg <- degree(nnet, cmode="outdegree")                   # Outdegree 
summary(ideg)

# This should be true, as the sum of all indegrees and outdegrees over the network equals the number of degree
centralization(nnet, degree, cmode="indegree")   		#network level measure
centralization(nnet, degree, cmode="outdegree") 		#network level measure

plot(ideg,odeg, type="p", col="red") # Plot ideg by odeg

par(mfrow=c(2,2))                                             # Set up a 2x2 display
pdf("Faculty Degree Distribution")
hist(ideg, xlab="Indegree", main="Indegree Distribution", prob=TRUE)
hist(odeg, xlab="Outdegree", main="Outdegree Distribution", prob=TRUE)
hist(ideg+odeg, xlab="Total Degree", main="Total Degree Distribution", prob=TRUE)
dev.off()
#par(mfrow=c(2,1))    										# Restore display
#pdf("Total Faculty Degree Distribution")
#hist(ideg+odeg, xlab="Total Degree", main="Total Degree Distribution", prob=FALSE)
#hist(ideg, xlab="Total In Degree", main="Total Degree Distribution", prob=FALSE)
#dev.off()
pdf("CSHPE Reciprocal-Graph NO LABELS.pdf")
gplot(nnet, vertex.cex=nodeInfo$number_of_authored_works^2/100, vertex.sides=50, edge.col = 8, label.cex=0.4,arrowhead.cex=.5, 
      label=nodeInfo$X.1, displaylabels=T, displayisolates=TRUE, usecurv=TRUE, mode = "fruchtermanreingold")
dev.off()

#Finding Cliques
summary(nnet)
clique.census(nnet)  
clique.census(nnet, tabulate.by.vertex=FALSE, enumerate=FALSE) # Find maximal cliques of varying sizes
is.connected(nnet) 
reachability(nnet) 

#everybody is connected
gplot(nnet,vertex.col=2+cutpoints(nnet,mode="graph", return.indicator=T))
gplot(nnet,displaylabels=T,vertex.col=2+cutpoints(nnet,mode="graph", return.indicator=T))


FacMat<-as.matrix(faculty)
FacMat<-as.data.frame(FacMat)
