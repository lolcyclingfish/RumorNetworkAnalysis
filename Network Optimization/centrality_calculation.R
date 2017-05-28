##############
#load packages
##############
library(igraph)

#load data
netRumor = read.csv('netRumor.csv')
netTruth = read.csv('netTruth.csv')

#make graph
gRumor = graph_from_data_frame(netRumor[1:2])
gTruth = graph_from_data_frame(netTruth[1:2])

#betweeness
betweenness(gRumor)['www.facebook.com']
betweenness(gTruth)['www.facebook.com']

#closeness
#closeness(gRumor)['www.facebook.com']
#closeness(gTruth)['www.facebook.com']

#eigen_centrality
eigen_centrality(gRumor,weights = netRumor[['FALSE.']])$vector[['www.facebook.com']]
eigen_centrality(gTruth,weights = netTruth[['TRUE.']])$vector[['www.facebook.com']]
