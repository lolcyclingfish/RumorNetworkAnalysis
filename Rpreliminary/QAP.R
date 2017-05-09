######################################################################################
#
# Import the necessary libraries
#
######################################################################################
#install.packages('magrittr')
#install.packages('igraph')
#install.packages('httr')
#install.packages('data.table')
#install.packages('dplyr')
library(magrittr)
library(httr)
library(data.table)
library(igraph)

######################
#load data and create network
######################
pol = read.csv('../data/pol_agg.csv',stringsAsFactors = FALSE)
pol = pol[pol$Source!=pol$website,]


#fake news
#g_valued <- graph_from_data_frame(d = network_table[,1:3,with=FALSE],directed = FALSE,vertices = total_table)
g_fake_valued = graph_from_data_frame(d = pol[,c(1,2,5)],directed = T)

graph.density(g_fake_valued)
L = layout_with_fr(g_fake_valued)  # Fruchterman Reingold
plot(g_fake_valued,vertex.color=V(g_fake_valued)$color, layout = L, vertex.label=NA,vertex.size=6) 

#convert into network
library('network')
library('sna')

