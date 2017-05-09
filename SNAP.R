#SNAP
setwd("~/Documents/MSiA2017Spring/Social_network/SNAP")

#load packages
library(magrittr)
library(httr)
library(data.table)
library(igraph)
library(dplyr)

#load data:
network_table <- read.csv("pol_agg.csv")

#Fake news
network_table1 = network_table[,c(1,2,5)]
network_table1= network_table1[network_table1$FALSE.>0,]
names(network_table1)[3]<-"CoOccurrences"
x1<-as.data.frame(network_table1$Source)
x2<-as.data.frame(network_table1$website)
names(x1)<-"nodes"
names(x2)<-"nodes"
total_table<-rbind(x1,x2)
nrow(total_table) #2728
total_table<-unique(total_table) #
nrow(total_table) #856

# Create a graph
# Data is stored in a data frame

#g_fake = graph_from_data_frame(d = network_table1,directed = T)
am_fake_valued = as_adj(g_fake)

g_valued <- graph_from_data_frame(d = network_table1,directed = TRUE,vertices = total_table)

# Count the number of nodes/edges in all of your graphs
numVertices <- vcount(g_valued)
numVertices #856
numEdges <- ecount(g_valued)
numEdges #1364

# Repeat for other 4 networks

# What is the density of your networks? 
graphDensity <- graph.density(g_valued)
graphDensity #0.001863694

## Sanity check - do these values equal 2 * numEdges / (numVertices * (numVertices-1)) ?? numEdges / (numVertices * (numVertices-1) / 2 )

######################################################################################
#
# Part II: Visualize your networks
#
######################################################################################

colbar = rainbow(numVertices) ## we are selecting different colors to correspond to each 
V(g_valued)$color = colbar


## We can set the width of the edges to be proportionate to the number of cooccurences
E(g_valued)$width = 5*network_table1$CoOccurrences/max(network_table1$CoOccurrences)

##FR layout and Spring layout are the best for visualization
# Set layout here 
L1 = layout_with_fr(g_valued)  # Fruchterman Reingold

# Plot graph with labels (messy)
#plot(g_valued,vertex.color=V(g_valued)$color, layout = L, vertex.size=6, main="Network by co-occurence value") 

#Plot graphs without labels:
plot(g_valued,vertex.color=V(g_valued)$color, layout = L1, vertex.size=6, vertex.label=NA, main="Network by co-occurence value") 

## You can change the layout by picking one of the other options
#L = layout_with_dh(g_valued) ## Davidson and Harel
#plot(g_valued,vertex.color=V(g_valued)$color, layout = L, vertex.size=6, vertex.label=NA, main="Network by co-occurence value") 

#L = layout_with_drl(g_valued) ## Force-directed
#plot(g_valued,vertex.color=V(g_valued)$color, layout = L, vertex.size=6, vertex.label=NA, main="Network by co-occurence value") 

L2 = layout_with_kk(g_valued) ## Spring
plot(g_valued,vertex.color=V(g_valued)$color, layout = L2, vertex.size=6, vertex.label=NA, main="Kamada-Kawai layout Network by co-occurence value") 

# Experiment with other layouts by setting L to a different setting
L3 = layout_on_sphere(g_valued) ## Sphere
plot(g_valued,vertex.color=V(g_valued)$color, layout = L3, vertex.size=6, vertex.label=NA,main="Layout on Sphere Network by co-occurence value") 

# You can play with the vertex sizes and/or edge sizes to get a better vizualization for the graphs below
plot(g_valued,vertex.color=V(g_valued)$color, layout = L, edge.width=E(g_valued)$width,vertex.label=NA,vertex.size=4)  # include values for edge weights



######################################################################################
#
# Part III: Independent Metrices
#
######################################################################################


# Centrality calculations
# ---------------------------------------

# Compute centralities for valued network 
totalDegree <- degree(g_valued,mode="all")
sort(totalDegree,decreasing=TRUE)[1:20] #top 20 of degree centrality

b <- betweenness(g_valued,directed=TRUE)
sort(b,decreasing=TRUE)[1:20] #top 20 of betweenness centrality

c <- closeness(g_valued)
sort(c,decreasing=TRUE)[1:20] #top 20 of closeness centrality 

eigc <- eigen_centrality(g_valued,directed=TRUE)
sort(eigc$vector,decreasing=TRUE)[1:20] #top 20 of eigen centrality

# For undirected matrices the adjacency matrix is symmetric and the hub scores are the same as authority scores

a <- authority_score(g_valued, scale = TRUE)
sort(a$vector,decreasing=TRUE)[1:20]

# For undirected matrices the adjacency matrix is symmetric and the hub scores are the same as authority scores
u <- hub_score(g_valued, scale = TRUE)
sort(u$vector,decreasing=TRUE)[1:20] 


# Centrality Visualization
# ---------------------------------------

## Plot based on the centrality of mean-valued netowrk 
g2 <- g_valued
V(g2)$size <- totalDegree*3 #can adjust the number
plot(g2, layout = L1, vertex.label=NA, main="valued network centrality visualization plot")

# What is the betweenness centrality score for each vertex? 
g4 <- g_valued
V(g4)$size <- b*3  #can adjust the number
plot(g4, layout = L1,vertex.label=NA, main="valued network betweenness centrality visualization plot")

# What is the closeness centrality score for each vertex? 
g5 <- g_valued
V(g5)$size <- c*5000  #can adjust the number
plot(g5, layout = L1, vertex.label=NA, main="valued network closeness centrality visualization plot")

# What is the eigenvector centrality score for each vertex? 
g6 <- g_valued
V(g6)$size <- eigc$vector*50 #can adjust the number
plot(g6, layout = L1, vertex.label=NA, main="valued network eigenvector centrality visualization plot")

# What is the authority score for each vertex? 
g7 <- g_valued
V(g7)$size <- a$vector*50 #can adjust the number
plot(g7, layout = L1,vertex.label=NA, main="valued network authority score visualization plot")

# What is the hub score for each vertex? 
g8 <- g_valued
V(g8)$size <- u$vector*50 # can adjust the number
plot(g8, layout = L1,vertex.label=NA,main="valued network hub score visualization plot")



#######Same set of centrality graphs for valued network

# Weights coming out of each vertex
graph.strength(g_valued,weights = E(g_valued)$CoOccurrences)
sort(graph.strength(g_valued,weights = E(g_valued)$CoOccurrences), 
     decreasing = TRUE)

# Weighted betweenness centrality, normalized
betweenness(g_valued,directed = FALSE,weights = E(g_valued)$CoOccurrences,normalized = TRUE)
sort(betweenness(g_valued,directed = FALSE,weights = E(g_valued)$CoOccurrences,normalized = TRUE), 
     decreasing = TRUE)

# Weighted closeness centrality, normalized
closeness(g_valued,mode = "all",weights = E(g_valued)$CoOccurrences,normalized = TRUE)
sort(closeness(g_valued,mode = "all",weights = E(g_valued)$CoOccurrences,normalized = TRUE), 
     decreasing = TRUE)

# Weighted eigenvector centrality, normalized
evcent(g_valued,directed = FALSE,weights = E(g_valued)$CoOccurrences)
sort(evcent(g_valued,directed = FALSE,weights = E(g_valued)$CoOccurrences)$vector, 
     decreasing = TRUE)

#valued network centrality:
sum(totalDegree, na.rm = TRUE) #degree network centrality
sum(b, na.rm = TRUE) #betweenness network centrality
sum(c, na.rm = TRUE) #closeness network centrality
sum(eigc$vector, na.rm = TRUE) #eigenvector network centrality

#mean valued network centrality:

######################################################################################
#
# Part IV: Valued Network Centrality and Centralization
#
######################################################################################

###For network split at 50th percentile###

# Plot the number of clusters in the graph and their size
# there are also other algorithms for this you may want to explore
cluster <- cluster_walktrap(g_valued)


# Find the number of clusters
membership(cluster)   # affiliation list 

length(sizes(cluster)) # number of clusters is 96

# Find the size the each cluster 
# Note that communities with one node are isolates, or have only a single tie
sizes(cluster) 

# Set layout here 
L = layout_with_fr(g_valued)  # Fruchterman Reingold

# Visualize clusters - this puts colored circles around the nodes in a community
plot(cluster, g_valued, col = V(g_valued)$color, layout = L, vertex.size=6, vertex.label=NA,main="Clustered network split at 50th percentile")

# Degree centralization of valued graph
centralization.degree(g_valued,normalized = TRUE) #0.08888342

# Betweenness centralization of split graphs
centralization.betweenness(g_valued,normalized = TRUE) #0.03434465

# Closeness centralization of split graphs
centralization.closeness(g_valued,normalized = TRUE) #0.0008192955


# Eigenvector centralization of split graphs
centralization.evcent(g_valued,normalized = TRUE) #0.9766616

# Calculate degree distribution
deg <- degree(g_valued,v=V(g_valued), mode="all") 

# Degree distribution is the cumulative frequency of nodes with a given degree
deg_distr <-degree.distribution(g_valued, cumulative=T, mode="all")

# Fit a power law to the degree distribution
# The output of the power.law.fit() function tells us what the exponent of the power law is ($alpha)
# and the log-likelihood of the parameters used to fit the power law distribution ($logLik)
# Also, it performs a Kolmogov-Smirnov test to test whether the given degree distribution could have
# been drawn from the fitted power law distribution.
# The function thus gives us the test statistic ($KS.stat) and p-vaule ($KS.p) for that test
power <- power.law.fit(deg_distr)
power

power
power$KS.p #p-value, if <0.05, reject the hypothesis that the original data could have been drawn from the fitted power-law distribution
#here power$KS.p is 0.874 > 0.05. Fail to reject tested network fits into power-law
#aka, tested network fits into power-law distribution

# Plot the degree distribution (power law)
plot(deg_distr, log="xy", ylim=c(.001,1), bg="black",pch=21, xlab="Degree", ylab="Cumulative Frequency",
     main="Degree distribution plot of valued network")


# Average clustering coefficient (ACC)
transitivity(g_valued, type = c("average")) #0.07016435

# Characteristic path length (CPL)
average.path.length(g_valued) #4.016522

# Generate 1 random networks & compute ACC & CPL
g <- erdos.renyi.game(500, 350, type = "gnm")
transitivity(g, type = c("average")) 
average.path.length(g)

###generate 100 random network & compute ACC & CPL
listOfGraphs <- list()
listOfAvePathLength <- list()
listOfAveClusteringCoef <- list()
for (i in 1:100) {
  numVertices <- vcount(g_valued)
  numEdges <- ecount(g_valued)
  listOfGraphs[[i]]<-erdos.renyi.game(numVertices, numEdges, type = "gnm")
  listOfAvePathLength[i]<-average.path.length(listOfGraphs[[i]])
  listOfAveClusteringCoef[i] <-transitivity(listOfGraphs[[i]], type = c("average"))
}
#student-t distribution stats of CPL and ACC out of 100 random graphs:
#here degree of freedom = 100 = 100 randomly geneared observations (graphs)
meanCPL<-mean(sapply(listOfAvePathLength,mean))
meanCPL #5.833257
sdCPL<-sd(unlist(listOfAvePathLength))
sdCPL #0.05074506
meanACC<-mean(sapply(listOfAveClusteringCoef,mean))
meanACC #0.003446509
sdACC<-sd(unlist(listOfAveClusteringCoef))
sdACC #0.00198854

# Average clustering coefficient (ACC)
g_ACC<-transitivity(g_valued, type = c("average")) #0.07016435

# Characteristic path length (CPL)
g_CPL<-average.path.length(g_valued) #4.016522

#calculated 1-sided t test statistics for both network ACC and network CPL 
#hypothese test 1 null: network ACC = meanACC (observed fake-news network is random netowrk)
#hypothese test 1 alt: network ACC > meanACC (observed fake-news network is small-world network)
t_stat1<-(g_ACC-meanACC)/sdACC
p_value1<-pt(t_stat1,100,lower.tail=FALSE) #one-side test for P[X > x]
p_value1 #p_value1 close to zero, reject null hyphothesis, aka fake-news network is small-world network
#hypothese test 2 null: network CPL = meanCPL (observed fake-news network is random netowrk)
#hypothese test 2 alt: network CPL < meanCPL (observed fake-news network is small-world network)
t_stat2<-(g_CPL-meanCPL)/sdCPL
p_value2<-pt(t_stat2,100,lower.tail=TRUE) #one-sided test for P[X <= x]
p_value2 #p_value2 close to zero, reject null hyphothesis, aka fake-news network is small-world network
