#I think I'm going to run it now with the same 116th Congress list, 
#but change the edge list from 2018-->2020. So we can see the nodematch change.

library(statnet)
install.packages("tergm")
library(ergm)
library(dplyr)
library(igraph)
setwd("~/Documents/Working papers/Caucus Paper/Data")

links115 <- read.csv("candidates_115edges.csv", header=T, as.is = T )
nodes116 <- read.csv("H116_members.csv", header = T, as.is = T)
links116 <- read.csv("H116NEWMEMBERS_edges.csv", header=T, as.is = T )

nodes116[is.na(nodes116)] <- 0

#making the two networks

vertice1_115 <- c(links115$out)
vertice2_115 <- c(links115$to)
weight_115 <- c(links115$weight)

vertice1_116 <- c(links116$out)
vertice2_116 <- c(links116$to)
weight_116 <- c(links116$weight)

data_115 <- cbind(vertice1_115, vertice2_115)
data_116 <- cbind(vertice1_116, vertice2_116)

#115
all.net_115 <- graph.empty()
all.net_115 <- add.vertices(all.net_115, length(unique(c(vertice1_115, vertice2_115))),
                            name=as.character(unique(c(vertice1_115, vertice2_115))))
all.net_115 <- add.edges(all.net_115, t(data_115))

E(all.net_115)$sender_name <-vertice1_115
E(all.net_115)$receiver_name <- vertice2_115
E(all.net_115)$weight <- weight_115

#116
all.net_116 <- graph.empty()
all.net_116 <- add.vertices(all.net_116, length(unique(c(vertice1_116, vertice2_116))),
                            name=as.character(unique(c(vertice1_116, vertice2_116))))
all.net_116 <- add.edges(all.net_116, t(data_116))

E(all.net_116)$sender_name <-vertice1_116
E(all.net_116)$receiver_name <- vertice2_116
E(all.net_116)$weight <- weight_116

#creating a function to limit the networks to the nodes that are in it (ie remove those who aren't connected)
Process_Node_and_Relational_Data <- function(node_level_data,
                                             relational_data,
                                             node_id_column = 1
){
  #get our node ids
  node_ids <- node_level_data[,node_id_column]
  #remove any missing or blank entries
  to_remove <- which(node_ids == "" | is.na(node_ids))
  if(length(to_remove) > 0){
    node_ids <- node_ids[-to_remove]
    node_level_data <- node_level_data[-to_remove,]
  }
  # Allocate a blank edgelist to return
  edgelist <- NULL
  # Loop over rows to check them
  for(i in 1:length(relational_data[,1])){
    # Check to see if the sender is in the dataset
    if(length(which(node_ids == relational_data[i,1]) > 0)){
      #' If we have a valid sender, check to see if there is a valid reciever 
      #' and add them to the dataset if they are valid as well for each 
      #' receiver
      for(j in 2:ncol(relational_data)){
        if(!is.na(relational_data[i,j])){
          if(length(which(node_ids == relational_data[i,j]) > 0)){
            edge <- c(relational_data[i,1],relational_data[i,j])
            edgelist <- rbind(edgelist,edge)
          }
        }
      }
    }
  }
  # Give column names to edgelist
  colnames(edgelist) <- c("sender","receiver")
  # Return cleaned data as a list object
  return(list(node_level_data = node_level_data, 
              edgelist = edgelist,
              num_nodes = length(node_level_data[,1]),
              node_names = node_ids))
}

#applying to data
head(nodes116)
head(links115)
Clean_Data115 <- Process_Node_and_Relational_Data(node_level_data = nodes116, 
                                               relational_data = links115,
                                               node_id_column = 1)


net115can <- network.initialize(Clean_Data115$num_nodes)

#116
Clean_Data116 <- Process_Node_and_Relational_Data(node_level_data = nodes116, 
                                                  relational_data = links116,
                                                  node_id_column = 1)


net116can <- network.initialize(Clean_Data116$num_nodes)

  
#adding nodenames
#115 first
network.vertex.names(net115can) <- Clean_Data115$node_names
net115can[as.matrix(Clean_Data115$edgelist)] <- 1
lablename <- as.character(nodes116$bioname)
head(lablename)


#116
network.vertex.names(net116can) <- Clean_Data116$node_names
net116can[as.matrix(Clean_Data116$edgelist)] <- 1
#label name is the same :) 

#adding vertex attributes back in for both networks
detach(package:igraph)
detach(package:threejs)

set.vertex.attribute(net115can, "Party", Clean_Data115$node_level_data$party_code)
set.vertex.attribute(net116can, "Party", Clean_Data116$node_level_data$party_code)

set.vertex.attribute(net115can,"State",Clean_Data115$node_level_data$state_abbrev)
set.vertex.attribute(net116can,"State",Clean_Data116$node_level_data$state_abbrev)

set.vertex.attribute(net115can,"RSC",Clean_Data115$node_level_data$RSC)
set.vertex.attribute(net116can,"RSC",Clean_Data116$node_level_data$RSC)

set.vertex.attribute(net115can,"GOP Leader",Clean_Data115$node_level_data$GOPLeadership)
set.vertex.attribute(net116can,"GOP Leader",Clean_Data116$node_level_data$GOPLeadership)

set.vertex.attribute(net115can,"Committee Chair",Clean_Data115$node_level_data$CommitteeChair)
set.vertex.attribute(net116can,"Committee Chair",Clean_Data116$node_level_data$CommitteeChair)

set.vertex.attribute(net115can,"Dem Leader",Clean_Data115$node_level_data$DemLeadership)
set.vertex.attribute(net116can,"Dem Leader",Clean_Data116$node_level_data$DemLeadership)

set.vertex.attribute(net115can,"Freedom",Clean_Data115$node_level_data$Freedom)
set.vertex.attribute(net116can,"Freedom",Clean_Data116$node_level_data$Freedom)

set.vertex.attribute(net115can,"Tuesday",Clean_Data115$node_level_data$Tuesday)
set.vertex.attribute(net116can,"Tuesday",Clean_Data116$node_level_data$Tuesday)

set.vertex.attribute(net115can,"Progressive",Clean_Data115$node_level_data$Progressive)
set.vertex.attribute(net116can,"Progressive",Clean_Data116$node_level_data$Progressive)

set.vertex.attribute(net115can,"Problem Solver",Clean_Data115$node_level_data$ProblemSolvers)
set.vertex.attribute(net116can,"Problem Solver",Clean_Data116$node_level_data$ProblemSolvers)

set.vertex.attribute(net115can,"Blue Dog",Clean_Data115$node_level_data$BlueDog)
set.vertex.attribute(net116can,"Blue Dog",Clean_Data116$node_level_data$BlueDog)

set.vertex.attribute(net115can,"New Dems",Clean_Data115$node_level_data$NewDemocrat)
set.vertex.attribute(net116can,"New Dems",Clean_Data116$node_level_data$NewDemocrat)

set.vertex.attribute(net115can,"New member",Clean_Data115$node_level_data$NewMember)
set.vertex.attribute(net116can,"New member",Clean_Data116$node_level_data$NewMember)

set.vertex.attribute(net115can,"Liberty",Clean_Data115$node_level_data$Liberty)
set.vertex.attribute(net116can,"Liberty",Clean_Data116$node_level_data$Liberty)

set.vertex.attribute(net115can, "bionames", Clean_Data115$node_level_data$bioname)
set.vertex.attribute(net116can, "bionames", Clean_Data116$node_level_data$bioname)

#analysis
#115 first
summary.network(net115can,print.adj = FALSE)
library(ergm)
newmembers115 <- ergm(net115can ~ edges + nodeocov("Dem Leader")+
                             nodeocov("GOP Leader")+
                             nodematch("Freedom", diff=F)+
                             nodematch("Liberty", diff=F)+
                             nodematch("RSC", diff=F)+
                             nodematch("Tuesday", diff=F)+
                             nodematch("Problem Solver", diff=F)+
                             nodematch("Blue Dog", diff=F)+
                             nodematch("New Dems", diff=F)+
                             nodematch("Progressive", diff=F)+
                             nodematch("State", diff=F))

summary(newmembers115)
#116
newmembers116 <- ergm(net116can ~ edges + nodeocov("Dem Leader")+
                        nodeocov("GOP Leader")+
                        nodematch("Freedom", diff=F)+
                        nodematch("Liberty", diff=F)+
                        nodematch("RSC", diff=F)+
                        nodematch("Tuesday", diff=F)+
                        nodematch("Problem Solver", diff=F)+
                        nodematch("Blue Dog", diff=F)+
                        nodematch("New Dems", diff=F)+
                        nodematch("Progressive", diff=F)+
                        nodematch("State", diff=F))
summary(newmembers116)

library(stargazer)
stargazer(newmembers115, newmembers116, type="text")
#brilliant--GOP members shift their giving patters once they join a caucus

#now let's graph it.
#116
library(ggraph)
library(tidygraph)
library(RColorBrewer)
library(ggplot2)
library(ggnetwork)
library(igraph)
library(dplyr)

tidy_graph116 <-as_tbl_graph(net116can)
mcnames <- as.character(net116can$bionames)
tidy_graph116<- tidy_graph116 %>%  
  # Remove loops
  activate(edges) %>%
  filter(!edge_is_loop()) %>%
  # Remove isolated nodes
  activate(nodes) %>%
  filter(!node_is_isolated()) %>%
  mutate(outdegree=centrality_degree(mode = 'out'), 
         indegree=centrality_degree(mode = 'in')) 

ggraph(tidy_graph116, layout="fr") +
  geom_edge_link(color="gray", alpha=.3) +
  geom_node_label(aes(label=bionames, filter = outdegree > 45, 
                      size=log(outdegree), alpha=log(outdegree+1)))+
  geom_node_point(aes(color="New member", size=log(indegree+1),
                      alpha=log(indegree+1))) +
  theme_blank() +
  scale_color_manual(name="", values=c("forest green", "black"), labels=c("New Members")) +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.title=element_blank()) +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.title=element_text(size=12)) +
  labs(title="Leadership PAC Donations to New Members, 116th Congress", 
       x="", y="") + 
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.title=element_text(size=12), 
        legend.position="bottom") +
  guides(size=FALSE, alpha=FALSE)


#115 congress
#tragically, need to pull in the #115 node list because of the retired members
nodes115 <- read.csv("cananalysis_115nodes.csv", header = T, as.is = T)
Clean_Data115update <- Process_Node_and_Relational_Data(node_level_data = nodes115, 
                                               relational_data = links115,
                                               node_id_column = 1)
net115update <- network.initialize(Clean_Data115update$num_nodes)
network.vertex.names(net115update) <- Clean_Data115update$node_names
net115update[as.matrix(Clean_Data115update$edgelist)] <- 1
lablename115update <- as.character(nodes115$bioname)

#adding in relevant attributes
detach(package:igraph)
set.vertex.attribute(net115update, "Party", Clean_Data115update$node_level_data$party_code)
set.vertex.attribute(net115update,"New member",Clean_Data115update$node_level_data$NewMember)
set.vertex.attribute(net115update, "bionames", Clean_Data115update$node_level_data$bioname)

tidy_graph115 <-as_tbl_graph(net115update)
mcnames <- as.character(net115update$bionames)
tidy_graph115<- tidy_graph115 %>%  
  # Remove loops
  activate(edges) %>%
  filter(!edge_is_loop()) %>%
  # Remove isolated nodes
  activate(nodes) %>%
  filter(!node_is_isolated()) %>%
  mutate(outdegree=centrality_degree(mode = 'out'), 
         indegree=centrality_degree(mode = 'in')) 

ggraph(tidy_graph115, layout="fr") +
  geom_edge_link(color="gray", alpha=.3) +
  geom_node_label(aes(label=bionames, filter = outdegree > 50, 
                      size=log(outdegree), alpha=log(outdegree+1)))+
  geom_node_point(aes(color="New member", size=log(indegree+1),
                      alpha=log(indegree+1))) +
  theme_blank() +
  scale_color_manual(name="", values=c("forest green", "black"), labels=c("New Members")) +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.title=element_blank()) +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.title=element_text(size=12)) +
  labs(title="Leadership PAC Donations to Successful Candidates, 115th Congress", 
       x="", y="") + 
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.title=element_text(size=12), 
        legend.position="bottom") +
  guides(size=FALSE, alpha=FALSE)
