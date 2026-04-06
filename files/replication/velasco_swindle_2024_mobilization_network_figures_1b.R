### July 5, 2023
### Network Analysis: First generating OrganizationxOrganization matrix from OrganizationxConference network

# Clear the workspace and do a gabarbage collection
rm(list=ls())
gc()

install.packages("dplyr")
install.packages("igraph")
install.packages("ggplot2")
install.packages("GGally")
install.packages("network")
install.packages("sna")
install.packages("intergraph")
install.packages("MetBrewer")
install.packages("gridExtra")  
install.packages("reshape2")

library(gridExtra)
library(network)
library(sna)
library(ggplot2)
library(intergraph)
library(dplyr)
library(igraph)
library(stringi)
library(reshape2)
library(ggplot2)
library(GGally)
library(MetBrewer)


options(stringsAsFactors = F)

# Importing the 2020 matrix to get eigenvector values
wcf_all <- read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/Research Projects/WCF Network/Datasets/WCF Presenters_20230705.xlsx", 
                         +     sheet = "Network Format")

#Making master network analysis
      # Removing Node Names to Make a Matrix Object
      orgs = array(wcf_all$Organization)
      country = array(wcf_all$Organization_Country)
      region = array(wcf_all$Organization_Region)
      NetData = as.matrix(wcf_all[4:57])
      matrix_all= as.matrix(NetData, header=TRUE)
      row.names(matrix_all) <- orgs
      
      #Converting from a Two-Mode to a One-Mode Matrix
      matrix_all <- matrix_all%*%t(matrix_all)
      
      #Getting rid of self-loops
      diag(matrix_all)<-0
      
      #Creating a graph object so that we can calculate centrality and other scores
      g_all <-graph_from_adjacency_matrix(matrix_all, mode="undirected", weighted=TRUE)
      
      #Adding a couple attributes back in
      #Name
      V(g_all)$name <- orgs
      
      #Country
      V(g_all)$country <- country
      
      #Region
      V(g_all)$region <- region   
      
      #Getting Betweenness Centrality Scores
      V(g_all)$betweenness <- betweenness(g_all) 
      
      centralityall <- data.frame(orgs = V(g_all)$name,
                                   country = V(g_all)$country,
                                   betweenness = V(g_all)$betweenness)
      

# Subsetting the matrix to relevant conferences based on time:
    #1997-2001
    wcf_1997 <- subset(wcf_all, select=c("Organization_Region","Organization_Country", "Organization", "C1", "C2", "R1999A", "R1999B", "R2000A", "R2000B", "R2001A", "R2001B", "R2001C"))
    
    #2002-2006
    wcf_2002 <- subset(wcf_all, select=c("Organization_Region","Organization_Country", "Organization", "C3", "R2002A", "R2006A"))
    
    #2007-2015
    wcf_2007 <- subset(wcf_all, select=c("Organization_Region","Organization_Country", "Organization", "C4", "C5", "C6", "C7", "C9" , "R2009B" , "R2010D", "R2011D", "R2011E", "R2011F", "R2011G", "R2013C", "R2013D", "R2013E", "R2014C", "R2014D", "R2014E", "R2014F", "R2015B" , "R2015D", "R2015E", "R2015F" , "R2015G", "R2015H"))
    
    #2016-2022
    wcf_2016 <- subset(wcf_all, select=c("Organization_Region","Organization_Country", "Organization","C10", "C11" ,"C12", "C13", "C14", "R2016B", "R2016C", "R2016D", "R2017A", "R2017B" , "R2017C", "R2017D", "R2017E", "R2018C", "R2018D", "R2018E", "R2018F", "R2019B"))
    
#### Making Each Sub-Network Graph Objects
# 1997-2001  
     # Removing Node Names to Make a Matrix Object
        orgs = array(wcf_1997$Organization)
        country = array(wcf_1997$Organization_Country)
        region = array(wcf_1997$Organization_Region)
        NetData = as.matrix(wcf_1997[4:12])
        matrix_1997 = as.matrix(NetData, header=TRUE)
        row.names(matrix_1997) <- orgs
    
    #Converting from a Two-Mode to a One-Mode Matrix
        matrix_1997 <- matrix_1997%*%t(matrix_1997)
        
    #Getting rid of self-loops
        diag(matrix_1997)<-0
    
    #Creating a graph object so that we can calculate centrality and other scores
       g_1997 <-graph_from_adjacency_matrix(matrix_1997, mode="undirected", weighted=TRUE)
        
    #Adding a couple attributes back in
        #Name
        V(g_1997)$name <- orgs
        
        #Country
        V(g_1997)$country <- country
        
        #Region
        V(g_1997)$region <- region
    
    
# 2002-2006    
    # Removing Node Names to Make a Matrix Object
        orgs = array(wcf_2002$Organization)
        country = array(wcf_2002$Organization_Country)
        region = array(wcf_2002$Organization_Region)
        NetData = as.matrix(wcf_2002[4:6])
        matrix_2002= as.matrix(NetData, header=TRUE)
        row.names(matrix_2002) <- orgs
    
    #Converting from a Two-Mode to a One-Mode Matrix
       matrix_2002 <- matrix_2002%*%t(matrix_2002)
    
    #Getting rid of self-loops
       diag(matrix_2002)<-0
    
    #Creating a graph object so that we can calculate centrality and other scores
       g_2002 <-graph_from_adjacency_matrix(matrix_2002, mode="undirected", weighted=TRUE)
    
    #Adding a couple attributes back in
     #Name
       V(g_2002)$name <- orgs
    
     #Country
       V(g_2002)$country <- country
    
     #Region
       V(g_2002)$region <- region   

# 2007-2015           
       # Removing Node Names to Make a Matrix Object
       orgs = array(wcf_2007$Organization)
       country = array(wcf_2007$Organization_Country)
       region = array(wcf_2007$Organization_Region)
       NetData = as.matrix(wcf_2007[4:27])
       matrix_2007= as.matrix(NetData, header=TRUE)
       row.names(matrix_2007) <- orgs
       
       #Converting from a Two-Mode to a One-Mode Matrix
       matrix_2007 <- matrix_2007%*%t(matrix_2007)
       
       #Getting rid of self-loops
       diag(matrix_2007)<-0
       
       #Creating a graph object so that we can calculate centrality and other scores
       g_2007 <-graph_from_adjacency_matrix(matrix_2007, mode="undirected", weighted=TRUE)
       
       #Adding a couple attributes back in
       #Name
       V(g_2007)$name <- orgs
       
       #Country
       V(g_2007)$country <- country
       
       #Region
       V(g_2007)$region <- region   

# 2016-2022
       # Removing Node Names to Make a Matrix Object
       orgs = array(wcf_2016$Organization)
       country = array(wcf_2016$Organization_Country)
       region = array(wcf_2016$Organization_Region)
       NetData = as.matrix(wcf_2016[4:21])
       matrix_2016= as.matrix(NetData, header=TRUE)
       row.names(matrix_2016) <- orgs
       
       #Converting from a Two-Mode to a One-Mode Matrix
       matrix_2016 <- matrix_2016%*%t(matrix_2016)
       
       #Getting rid of self-loops
       diag(matrix_2016)<-0
       
       #Creating a graph object so that we can calculate centrality and other scores
       g_2016 <-graph_from_adjacency_matrix(matrix_2016, mode="undirected", weighted=TRUE)
       
       #Adding a couple attributes back in
       #Name
       V(g_2016)$name <- orgs
       
       #Country
       V(g_2016)$country <- country
       
       #Region
       V(g_2016)$region <- region   
       
       
#Graphs    
      ## For graphing purposes I'm going to drop isolates 
       g_1997_isolates <- delete.vertices(g_1997, V(g_1997)[ degree(g_1997)==0 ]) 
       g_2002_isolates <- delete.vertices(g_2002, V(g_2002)[ degree(g_2002)==0 ]) 
       g_2007_isolates <- delete.vertices(g_2007, V(g_2007)[ degree(g_2007)==0 ]) 
       g_2016_isolates <- delete.vertices(g_2016, V(g_2016)[ degree(g_2016)==0 ]) 
       
#1997-2001       
network1997 <-  ggnet2(g_1997_isolates, color = "region", palette=c("Post-Communist" = "#ffbb44",
                                                                    "Latin America/Caribbean" = "#ee8577",
                                                                    "Asia/Pacific" = "#ce4441",
                                                                    "Africa" = "#859b6c",
                                                                    "The West" = "#004f63"),
                  size = 2, size.min = 1, size.cut = 4,  node.alpha = .8, segment.alpha = .8, edge.color = c("color", "grey70"), edge.alpha = .5, layout.exp = .05,
                  color.legend = "Region") + guides(size = "none")
    
network1997 <- network1997 + ggtitle("1997-2001")  +
  theme(plot.title = element_text(hjust = 0.5))

#2002-2006      
network2002 <-  ggnet2(g_2002_isolates, color = "region", palette=c("Post-Communist" = "#ffbb44",
                                                                    "Latin America/Caribbean" = "#ee8577",
                                                                    "Asia/Pacific" = "#ce4441",
                                                                    "Africa" = "#859b6c",
                                                                    "The West" = "#004f63"),
           size = 2, size.min = 1, size.cut = 4,  node.alpha = .8, segment.alpha = .8, edge.color = c("color", "grey70"), edge.alpha = .5, layout.exp = .05,
           color.legend = "Region") + guides(size = "none")
    
network2002 <- network2002 + ggtitle("2002-2006")  +
  theme(plot.title = element_text(hjust = 0.5))

#2007-2015      
network2007 <- ggnet2(g_2007_isolates, color = "region", palette=c("Post-Communist" = "#ffbb44",
                                                                   "Latin America/Caribbean" = "#ee8577",
                                                                   "Asia/Pacific" = "#ce4441",
                                                                   "Africa" = "#859b6c",
                                                                   "The West" = "#004f63"),
           size = 2, size.min = 1, size.cut = 4,  node.alpha = .8, segment.alpha = .8, edge.color = c("color", "grey70"), edge.alpha = .5, layout.exp = .05,
           color.legend = "Region") + guides(size = "none")

network2007 <- network2007 + ggtitle("2007-2015")   +
  theme(plot.title = element_text(hjust = 0.5))

#2016-2022    
network2016 <- ggnet2(g_2016_isolates, color = "region", palette=c("Post-Communist" = "#ffbb44",
                                                        "Latin America/Caribbean" = "#ee8577",
                                                        "Asia/Pacific" = "#ce4441",
                                                        "Africa" = "#859b6c",
                                                        "The West" = "#004f63"),
           size = 2, size.min = 1, size.cut = 4,  node.alpha = .8, segment.alpha = .8, edge.color = c("color", "grey70"), edge.alpha = .5, layout.exp = .05,
           color.legend = "Region") + guides(size = "none")
    
network2016 <- network2016 + ggtitle("2016-2022") +
  theme(plot.title = element_text(hjust = 0.5)) 


grid.arrange(network1997, network2002, network2007, network2016,
                        ncol = 2, nrow = 2)


## Getting Betweenness Scores
V(g_1997)$betweenness <- betweenness(g_1997) 

centrality1997 <- data.frame(orgs = V(g_1997)$name,
                             country = V(g_1997)$country,
                             betweenness = V(g_1997)$betweenness)

V(g_2002)$betweenness <- betweenness(g_2002) 

centrality2002 <- data.frame(orgs = V(g_2002)$name,
                             country = V(g_2002)$country,
                             betweenness = V(g_2002)$betweenness)


V(g_2007)$betweenness <- betweenness(g_2007) 

centrality2007 <- data.frame(orgs = V(g_2007)$name,
                             country = V(g_2007)$country,
                             betweenness = V(g_2007)$betweenness)

V(g_2016)$betweenness <- betweenness(g_2016) 

centrality2016 <- data.frame(orgs = V(g_2016)$name,
                             country = V(g_2016)$country,
                             betweenness = V(g_2016)$betweenness)