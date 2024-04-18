#packages
library(tidyverse)
library(igraph)


#contri network----

load("Q:/Projekte/DFG_ENOC/R/ENOC_data_setup/r-lib-info-network-Tsed.Rdata" )
load("Q:/Projekte/DFG_ENOC/R/ENOC_data_setup/r-lib-repos.Rdata")


#drop na
final_info <- drop_na(final_info)

nodes <- data.frame((unique(final_info$temp_login)))


#time work conversion
r_lib$owner_date_crea <- as.POSIXlt(r_lib$owner_date_crea)
d3 <- as.POSIXlt("2024-04-18")

r_lib$age <- d3 - r_lib$owner_date_crea
r_lib$age <- as.numeric(as.character(r_lib$age))
#r_lib$age <- r_lib$age/max(r_lib$age)


final_info$temp_date <- as.POSIXlt(final_info$temp_date)
final_info$age <- d3 - final_info$temp_date
final_info$age <- as.numeric(as.character(final_info$age))

#create age groups

r_lib$age_group <- 0
final_info$age_group <- 0

#one year bracket
cutoffs <- seq(from=max(final_info$age), to = (min(final_info$age)-365), by = -365)

#age groups
for(cuts in 1:length(cutoffs)){
  
  if(cuts > 1){
    r_lib$age_group <- if_else(r_lib$age >= cutoffs[cuts] & r_lib$age < cutoffs[cuts-1], cuts, r_lib$age_group)
  } else {
    r_lib$age_group <- if_else(r_lib$age >= cutoffs[cuts], cuts, r_lib$age_group)
  }
  
}

for(cuts in 1:length(cutoffs)){
  
  if(cuts > 1){
    final_info$age_group <- if_else(final_info$age >= cutoffs[cuts] & final_info$age < cutoffs[cuts-1], cuts, final_info$age_group)
  } else {
    final_info$age_group <- if_else(final_info$age >= cutoffs[cuts], cuts, final_info$age_group)
  }
  
}


counts_repos <- final_info %>%
  group_by(age_group) %>% 
  count(repo)

counts_parts <- final_info %>%
  group_by(age_group) %>% 
  count(temp_login)

count_table <- data.frame()
for(AG in 1:length(cutoffs)){
  
  counts_parts_repos <- final_info %>%
    subset(age_group == AG) %>% 
    group_by(repo) %>% 
    count(temp_login)
  
  output <- cbind(counts_parts_repos, AG)
  colnames(output)[4] <- "age_group"
  
  count_table <- rbind(count_table,output)
}




#loop contris for timegroups-----

edges_by_time <- list()
nodes_by_time <- list()
count_by_time <- list()


for(groupies in 1:length(cutoffs)){
  
  
  final_info_age_group <- subset(final_info, age_group == groupies)
  
  
  age_nodes <- data.frame(unique(final_info_age_group$temp_login))
  
  edges_start <- final_info_age_group %>%
    group_by(temp_login) %>%
    count(repo)
  
  edges_time <- data.frame()
  
  
  for (noddy in 1:nrow(age_nodes)){
    
    node <- age_nodes[noddy,1]
    
    #list of parts in node
    parts_on <- subset(edges_start, temp_login == node)
    
    #list of some parts in other games (nodes)
    parts_of <- subset(edges_start, repo %in% parts_on$repo)
    
    if(nrow(parts_on) < nrow(parts_of)){
      
      #just the other nodes
      parts_ofc <- unique(parts_of$temp_login)
      parts_ofc <- parts_ofc[! parts_ofc %in% node]
      
      tie <- data.frame()
      
      for (luca in 1:length(parts_ofc)){
        
        tie_s <- nrow(subset(parts_of, temp_login == parts_ofc[luca]))
        tie <- rbind(tie, tie_s)
        
      }
      
      edges_id <- data.frame(cbind(node, parts_ofc))
      
      edges_id$temp <- apply(edges_id, 1, function(x) paste(sort(x), collapse= ""))
      
      edges_id$tie_s <- tie[,1]
      names(edges_id)[4] <- "tie_share_repo"
      
      tie <- data.frame()
      for(share in 1:nrow(edges_id)){
        find <- subset(edges_start, temp_login == node | temp_login == edges_id$parts_ofc[share])
        
        counts <- find %>% group_by(repo) %>% count()
        dupes <- subset(counts, n > 1)
        DF2 <- find %>% subset(repo %in% dupes$repo)
        tie_s <- sum(as.numeric(DF2$n))
        tie <- rbind(tie, tie_s)
        
      }
      
      edges_id$tie_s <- tie[,1]
     
      names(edges_id)[5] <- "tie_intensity_repo"
      
      edges_time <- rbind(edges_time, edges_id)
      
      print(noddy)
      
    }
    
    
  }
  
  
  a <- edges_time[!duplicated(edges_time$temp),]
  
  
  if(nrow(a) != 0){
    a <- a[,c(1:2,4:5)]
    names(a)[1] <- "from"
    names(a)[2] <- "to"
    names(a)[3] <- "strength_repo"
    names(a)[4] <- "strength_share"
  } else {
    a <- data.frame(matrix(ncol = 4, nrow = 0))
    
    colnames(a)[1:4] <- c("from","to", "strength_repo","strength_share")
    
  }

  
  nam_e <- paste("Edges_Group", groupies, sep = "")
  nam_n <- paste("Nodes_Group", groupies, sep = "")
  nam_g <- paste("Count_Group", groupies, sep = "")
  
  assign(paste0(nam_e), a)
  assign(paste0(nam_n), age_nodes)
  assign(paste0(nam_g), edges_start)
  
  
  
  edges_by_time <- c(edges_by_time, a)
  nodes_by_time <- c(nodes_by_time, age_nodes)
  count_by_time <- c(count_by_time, edges_start)
  
  print(nam_e)
}

#save(edges_by_time, file = "Q:/Projekte/DFG_ENOC/R/ENOC_data_setup/edges_by_time.Rdata" )
#save(nodes_by_time, file = "Q:/Projekte/DFG_ENOC/R/ENOC_data_setup/nodes_by_time.Rdata" )
#save(count_by_time, file = "Q:/Projekte/DFG_ENOC/R/ENOC_data_setup/count_by_time.Rdata" )




#calculate network props-------

load("Q:/Projekte/DFG_ENOC/R/ENOC_data_setup/r-lib-info-network-Tsed.Rdata" )
load("Q:/Projekte/DFG_ENOC/R/ENOC_data_setup/r-lib-repos.Rdata")

load("Q:/Projekte/DFG_ENOC/R/ENOC_data_setup/edges_by_time.Rdata" )
load("Q:/Projekte/DFG_ENOC/R/ENOC_data_setup/nodes_by_time.Rdata" )
load("Q:/Projekte/DFG_ENOC/R/ENOC_data_setup/count_by_time.Rdata" )

final_info <- drop_na(final_info)

#create whole level network properties of and calculate


props_WN_contris <- data.frame()
indi_res_time <- list()

for(groupies in 1:length(nodes_by_time)){
  
  seq <- seq(from = 1, to = (4*15), by=4)
  
  edges_simple <- data.frame(edges_by_time[(seq[groupies]):(seq[groupies]+1)])
  
  a <- data.frame(edges_by_time[(seq[groupies]):(seq[groupies]+2)])
  edges_repo_share <- a[rep(seq.int(1,nrow(a)), a$strength_repo), 1:2]
  
  a <- data.frame(edges_by_time[c((seq[groupies]):(seq[groupies]+1),(seq[groupies]+3))])
  edges_repo_intensity <- a[rep(seq.int(1,nrow(a)), a$strength_share), 1:2]
  
  
  nam_e <- paste("Time_group_", groupies, sep = "")
  
  routes_igraph <- graph_from_data_frame(d = edges_simple,
                                         vertices = nodes_by_time[groupies+2],
                                         directed = FALSE)
  
  

  
  #indi measures
  #centrality
  eigen <- eigen_centrality(routes_igraph)
  ESD <- eigen[1]$vector
  
  #coreness
  coreness <- data.frame(graph.coreness(routes_igraph))
  
    #constraint
  constraint <- data.frame(igraph::constraint(routes_igraph))
  
  #cluster
  #clu <- cluster_fast_greedy(routes_igraph)
  #clu2 <- cluster.distribution(routes_igraph)
  #clu3 <- cluster_edge_betweenness(routes_igraph)
  
  #network measures
  ED <- edge_density(routes_igraph, loops=F)
  TR <- transitivity(routes_igraph, type="global")
  DI <- diameter(routes_igraph, directed=F)
  MD <- mean_distance(routes_igraph, directed=F)
  #M_cor <- max(coreness[1])
  #SD_cor <- sd(coreness[1]$graph.coreness.routes_igraph.)
  #SD_con <- sd(constraint[1]$igraph..constraint.routes_igraph.)
  
  
  #combine
  
  net_results <- data.frame(nodes_by_time[groupies+2]$unique.final_info_age_group.temp_login.,
                            ESD, coreness, constraint)
  indi_res_time <- c(indi_res_time, net_results)
  
  net_results_WN <- data.frame(nam_e, ED, TR, DI, MD)
  
  props_WN_contris <- rbind(props_WN_contris, net_results_WN)
  print(nam_e)
}

test <- data.frame(indi_res_time[21:24])

#for tomorrow: other strengths, repo dynamics, big dataframe for regression