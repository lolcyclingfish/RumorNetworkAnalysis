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
pol_fake = pol[,c(1,2,5)]
pol_fake = pol_fake[pol_fake$FALSE.>0,]
g_fake_valued = graph_from_data_frame(d = pol_fake,directed = T)
#am_fake_valued = as_adj(g_fake_valued)
#m_fake_valued = as.matrix(am_fake_valued)

#real news
pol_true = pol[,c(1,2,4)]
pol_true = pol_true[pol_true$TRUE.>0,]
g_real_valued = graph_from_data_frame(d = pol_true,directed = T)
#am_real_valued = as_adj(g_real_valued)
#m_real_valued = as.matrix(am_real_valued)

#graph.density(g_fake_valued)
L = layout_with_fr(g_fake_valued)  # Fruchterman Reingold
plot(g_fake_valued,vertex.color=V(g_fake_valued)$color, layout = L, vertex.label=NA,vertex.size=6) 



#############
#QAP
#############

#for QAP, rerun the 

#fake news
#g_valued <- graph_from_data_frame(d = network_table[,1:3,with=FALSE],directed = FALSE,vertices = total_table)
pol_fake = pol[,c(1,2,5)]
#pol_fake = pol_fake[pol_fake$FALSE.>0,]
g_fake_valued = graph_from_data_frame(d = pol_fake,directed = T)
am_fake_valued = as_adj(g_fake_valued)
m_fake_valued = as.matrix(am_fake_valued)

#real news
pol_true = pol[,c(1,2,4)]
#pol_true = pol_true[pol_true$TRUE.>0,]
g_real_valued = graph_from_data_frame(d = pol_true,directed = T)
am_real_valued = as_adj(g_real_valued)
m_real_valued = as.matrix(am_real_valued)

detach(package:igraph) 
library('sna')

g_fake_binary_net = network(m_fake_valued,directed = T)
g_real_binary_net = network(m_real_valued,directed = T)

qapoutput = qaptest(list(g_fake_binary_net,g_real_binary_net),gcor,g1=1,g2=2,reps=100) 
qapoutput$testval
