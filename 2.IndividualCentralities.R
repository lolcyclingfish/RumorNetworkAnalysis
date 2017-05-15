##Local
#LY
rm(list=ls())
library(magrittr)
library(httr)
library(data.table)
library(igraph)
detach(package:sna)

######################
#load data and create network
######################
setwd("~/Documents/RumorNetworkAnalysis")
pol = read.csv('data/pol_agg.csv',stringsAsFactors = FALSE)
pol = pol[pol$Source!=pol$website,] #Remove self-loop

#fake news
#g_valued <- graph_from_data_frame(d = network_table[,1:3,with=FALSE],directed = FALSE,vertices = total_table)
pol_fake = pol[,c(1,2,5)]
pol_fake = pol_fake[pol_fake$FALSE.>0,]
g_fake_valued = graph_from_data_frame(d = pol_fake,directed = T)
am_fake_valued = as_adjacency_matrix(g_fake_valued)

#real news
pol_true = pol[,c(1,2,4)]
pol_true = pol_true[pol_true$TRUE.>0,]
g_real_valued = graph_from_data_frame(d = pol_true,directed = T)
am_real_valued = as_adjacency_matrix(g_real_valued)

centr_degree(g_fake_valued,mode='all',loops = FALSE,normalized = TRUE)
centr_degree(g_real_valued,mode='all',loops = FALSE,normalized = TRUE)

####################################
ob<-g_real_valued
E(ob)$width =  5*(pol_true$TRUE.+1/max(pol_true$TRUE.))
wd<-graph.strength(ob,weights = E(ob)$width*0.8)
wb<-betweenness(ob,directed = TRUE, weights = E(ob)$width*0.8,normalized = TRUE)
wc<-closeness(ob,mode = "all",
              weights = E(ob)$width*0.8,normalized = TRUE)
we<-evcent(ob,directed = FALSE,
           weights = E(ob)$width*0.8)
real<-data.frame(wd)
for (i in c('wb','wc','wd')){
  v<-as.data.frame(get(i))
  real<-cbind(real,v)
}
write.csv(real,'real_localcentralities.csv')

####################################
ob<-g_fake_valued
E(ob)$width =  5*(pol_fake$TRUE.+1/max(pol_fake$TRUE.))
wd<-graph.strength(ob,weights = E(ob)$width*0.8)
wb<-betweenness(ob,directed = TRUE, weights = E(ob)$width*0.8,normalized = TRUE)
wc<-closeness(ob,mode = "all",
              weights = E(ob)$width*0.8,normalized = TRUE)
we<-evcent(ob,directed = FALSE,
           weights = E(ob)$width*0.8)
fake<-data.frame(wd)
for (i in c('wb','wc','wd')){
  v<-as.data.frame(get(i))
  fake<-cbind(fake,v)
}
write.csv(fake,'fake_localcentralities.csv')