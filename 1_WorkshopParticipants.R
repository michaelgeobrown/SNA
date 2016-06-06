install.packages("statnet")
install.packages("foreign")
library(statnet)
library(foreign)

#Our Edge List

workshop.edgelist<-read.csv("https://raw.githubusercontent.com/michaelgeobrown/SNA/master/WorkshopEdgelist%20-%20Edgelist.csv", header=T, sep=",")

#let's look at it
summary(workshop.edgelist)

#Ok, that's not super helpful

#Let's transform this into a network
work.net<-as.network(workshop.edgelist, directed=T)

#Now let's look at it as a network
summary(work.net)

nodeInfo<-read.csv("https://raw.githubusercontent.com/michaelgeobrown/SNA/master/3_nodeInfo", header=T, sep=",")

plot(work.net)

network.dyadcount(work.net)                                    # How many dyads?

network.edgecount(work.net)                                    # How many edges are present?

network.size(work.net)    
#
#How dense is the network?
gden(work.net)

#Degree centrality
degree(work.net)

deg <- degree(work.net)  
ideg <- degree(work.net, cmode="indegree")                    # Indegree 
odeg <- degree(work.net, cmode="outdegree")                   # Outdegree 
summary(ideg)

# This should be true, as the sum of all indegrees and outdegrees over the network equals the number of degree
centralization(work.net, degree, cmode="indegree")   		#network level measure
centralization(work.net, degree, cmode="outdegree") 		#network level measure

#Is everybody who is sending a tie receiving a tie?
plot(ideg,odeg, type="p", col="red") # Plot ideg by odeg

#Finding Cliques
summary(work.net)
clique.census(work.net)  
clique.census(work.net, tabulate.by.vertex=FALSE, enumerate=FALSE) # Find maximal cliques of varying sizes
is.connected(work.net) 
reachability(work.net) 

#A real basic network visualization
gplot(work.net)

#Plotting a Network

netviz.fr<-plot.network(work.net, displaylabels=TRUE, displayisolates=TRUE, arrowhead.cex=.5, 
             label.cex=.5, vertex.cex=nodeInfo$Name, edge.col=8, label.col=(1), vertex.col=9, 
             label.border=1, vertex.border=1, mode="fruchtermanreingold",  sub="Fruchterman Reingold")
netviz.kk<-plot.network(work.net, displaylabels=TRUE, displayisolates=TRUE, arrowhead.cex=.5, 
                       label.cex=.5, vertex.cex=nodeInfo$Name, edge.col=8, label.col=(1), vertex.col=9, 
                       label.border=1, vertex.border=1, mode="kamadakawai",  sub="Kamada–Kawai")
netviz.circle<-plot.network(work.net, displaylabels=TRUE, displayisolates=TRUE, arrowhead.cex=.5, 
                            label.cex=.5, vertex.cex=nodeInfo$Name, edge.col=8, label.col=(1), vertex.col=9, 
                            label.border=1, vertex.border=1, mode="circle",  sub="Circle")



setwd("~/Downloads")
#Put that network graph in a PDF in the downloads folder
par(mfrow=c(1,3))  #This places all three graphs into one document
pdf("Workshop Visualization.pdf")
plot.network(work.net, displaylabels=FALSE, displayisolates=TRUE, arrowhead.cex=.5, 
                        label.cex=.5, edge.col=8, label.col=(1), vertex.col=9, 
                        label.border=1, vertex.border=1, mode="fruchtermanreingold",  sub="Fruchterman Reingold")
plot.network(work.net, displaylabels=FALSE, displayisolates=TRUE, arrowhead.cex=.5, 
                        label.cex=.5, edge.col=8, label.col=(1), vertex.col=9, 
                        label.border=1, vertex.border=1, mode="kamadakawai",  sub="Kamada–Kawai")
plot.network(work.net, displaylabels=FALSE, displayisolates=TRUE, arrowhead.cex=.5, 
             label.cex=.5, edge.col=8, label.col=(1), vertex.col=9, 
             label.border=1, vertex.border=1, mode="circle",  sub="Circle")
dev.off()
#Creating a network with some labels and identifhing cutpoints by color
gplot(work.net,displaylabels=T,vertex.col=2+cutpoints(work.net,mode="graph", return.indicator=T))

