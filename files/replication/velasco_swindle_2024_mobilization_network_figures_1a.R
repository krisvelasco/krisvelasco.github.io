### May 3, 2022
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
wcf_all <- read_excel("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/Research Projects/WCF Network/Datasets/WCF Presenters.xlsx", 
                      sheet = "Network Format")

# Subsetting the matrix to relevant conferences based on time:
    #1997-2001
    wcf_1997 <- subset(wcf_all, select=c("Region","Country", "Organization", "C1", "C2", "R1", "R2", "R3", "R4"))
    
    #2002-2006
    wcf_2002 <- subset(wcf_all, select=c("Region","Country", "Organization",  "R5", "C3", "R6"))
    
    #2007-2011
    wcf_2007 <- subset(wcf_all, select=c("Region","Country", "Organization",  "C4", "C5", "R7", "R8", "R9", "R10", "R11", "R12"))
    
    #2012-2015
    wcf_2012 <- subset(wcf_all, select=c("Region","Country", "Organization",  "C6", "R13", "C7", "R14", "R15", "R16", "R17", "R18", "C9","R19", "R20"))
    
    #2016-2021
    wcf_2016 <- subset(wcf_all, select=c("Region","Country", "Organization", "R21","R22", "R23", "R24", "C9", "R25", "R26", "R27", "R28", "C10", "C11", "C12", "C13", "R29", "R30", "R31", "R32", "R33", "R34", "R35", "R36"))
    
# Making Overall Network
    # Removing Node Names to Make a Matrix Object
    orgs = array(wcf_all$Organization)
    country = array(wcf_all$Country)
    region = array(wcf_all$Region)
    NetData = as.matrix(wcf_all[4:51])
    matrix_all = as.matrix(NetData, header=TRUE)
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
    
    
# Removing Node Names to Make a Matrix Object
    orgs = array(wcf_1997$Organization)
    country = array(wcf_1997$Country)
    region = array(wcf_1997$Region)
    NetData = as.matrix(wcf_1997[4:9])
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

 # Removing Node Names to Make a Matrix Object
    orgs = array(wcf_2002$Organization)
    country = array(wcf_2002$Country)
    region = array(wcf_2002$Region)
    NetData = as.matrix(wcf_2002[4:6])
    matrix_2002 = as.matrix(NetData, header=TRUE)
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
    
 # Removing Node Names to Make a Matrix Object
    orgs = array(wcf_2007$Organization)
    country = array(wcf_2007$Country)
    region = array(wcf_2007$Region)
    NetData = as.matrix(wcf_2007[4:11])
    matrix_2007 = as.matrix(NetData, header=TRUE)
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
    

# Removing Node Names to Make a Matrix Object
    orgs = array(wcf_2012$Organization)
    country = array(wcf_2012$Country)
    region = array(wcf_2012$Region)
    NetData = as.matrix(wcf_2012[4:14])
    matrix_2012 = as.matrix(NetData, header=TRUE)
    row.names(matrix_2012) <- orgs
    
    #Converting from a Two-Mode to a One-Mode Matrix
    matrix_2012 <- matrix_2012%*%t(matrix_2012)
    
    #Getting rid of self-loops
    diag(matrix_2012)<-0
    
    #Creating a graph object so that we can calculate centrality and other scores
    g_2012 <-graph_from_adjacency_matrix(matrix_2012, mode="undirected", weighted=TRUE)
    
    #Adding a couple attributes back in
    #Name
    V(g_2012)$name <- orgs
    
    #Country
    V(g_2012)$country <- country
    
    #Region
    V(g_2012)$region <- region   
    

# Removing Node Names to Make a Matrix Object
    orgs = array(wcf_2016$Organization)
    country = array(wcf_2016$Country)
    region = array(wcf_2016$Region)
    NetData = as.matrix(wcf_2016[4:24])
    matrix_2016 = as.matrix(NetData, header=TRUE)
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
    ggnet2(g_all, color = "region", palette=c("Eastern Europe & Central Asia" = "#ffbb44",
                                                 "Latin America and the Caribbean" = "#62929a",
                                                  "Middle East & North Africa" = "#c969a1",
                                                  "Oceania" = "#004f63",
                                                  "South/East Asia" = "#ce4441",
                                                  "Sub-Saharan Africa" = "#859b6c",
                                                  "Western Europe & North America" = "#ee8577"),
                  size = 3, size.min = 1, size.cut = 4,  node.alpha = .9, segment.alpha = .8, edge.color = c("color", "grey90"), layout.exp = .05,
                  color.legend = "Region") + 
      guides(size = "none")
    