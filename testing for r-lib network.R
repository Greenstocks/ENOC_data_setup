
#necessary packages-------------------------------------------
#install.packages("gh")
library(gh)

#install.packages("httpuv")
library(httpuv)
#install.packages("httr")
library(httr)

#install.packages("allcontributors")
library(allcontributors)

library(tidyverse)

library(igraph)

#token generation-------------------------------------------------

# Can be github, linkedin etc depending on application
oauth_endpoints("github")

# Change based on what you 
myapp <- oauth_app(appname = "Research_LMU",
                   key = "4f63b516efce7e548487",
                   secret = "d7893b49abef164efffc6c89e9d3d0a67eaec461")

# Get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

# Use API
gtoken <- config(token = github_token)

#get data-----------------

my_owner_reps <- gh("GET /orgs/{org}/repos", org = "r-lib", per_page = 100, page = 1)
owner_reps <- vapply(my_owner_reps, "[[", "", "name")
owner_stars <- sapply(my_owner_reps, function(x){as.numeric(x[78])})
owner_date_crea  <- vapply(my_owner_reps, "[[", "", "created_at")
owner_date_update  <- vapply(my_owner_reps, "[[", "", "updated_at")
owner_forks <- sapply(my_owner_reps, function(x){as.numeric(x[76])})


if (length(owner_reps) == 100){
  my_owner_reps <- gh("GET /orgs/{org}/repos", org = "r-lib", per_page = 100, page = 2)
  owner_reps  <- c(owner_reps, vapply(my_owner_reps, "[[", "", "name"))
  owner_stars <- c(owner_stars, sapply(my_owner_reps, function(x){as.numeric(x[78])}))
  owner_date_crea  <- c(owner_date_crea, vapply(my_owner_reps, "[[", "", "created_at"))
  owner_date_update  <- c(owner_date_update, vapply(my_owner_reps, "[[", "", "updated_at"))
  owner_forks <- c(owner_forks, sapply(my_owner_reps, function(x){as.numeric(x[76])}))
}

r_lib <- data.frame(owner_reps, owner_stars,owner_date_crea,owner_date_update, owner_forks )


#save(r_lib, file = "Q:/Projekte/DFG_ENOC/R/ENOC_data_setup/r-lib-repos.Rdata" )

result <- data.frame()
#get all contributors of repos


for(id in 1:nrow(r_lib)){
  my_contris <- gh("GET /repos/{owner}/{repo}/contributors", owner = "r-lib", repo =r_lib[id,1], per_page = 100)
  contris <- vapply(my_contris, "[[", "", "login")
  contris_amount <- sapply(my_contris, function(x){as.numeric(x[19])})
  
  if (length(my_contris) == 100){
    my_contris <- gh("GET /repos/{owner}/{repo}/contributors", owner = "r-lib", repo =r_lib[id,1], per_page = 100, page = 2)
    contris  <- c(contris, vapply(my_contris, "[[", "", "login"))
    contris_amount <- c(contris_amount, sapply(my_contris, function(x){as.numeric(x[19])}))
    }
  
  temp <- data.frame(r_lib[id,1], contris,contris_amount)
  result <- rbind(result, temp)
  Sys.sleep(2)
}

#save(result, file = "Q:/Projekte/DFG_ENOC/R/ENOC_data_setup/r-lib-list.Rdata" )


#create adjacency matrix from object for rep network---------

load("Q:/Projekte/DFG_ENOC/R/ENOC_data_setup/r-lib-repos.Rdata" )
load("Q:/Projekte/DFG_ENOC/R/ENOC_data_setup/r-lib-list.Rdata")

colnames(result)[1] <- "repos"

nodes <- data.frame((unique(result$repos)))

edges <- data.frame()


for (noddy in 1:nrow(nodes)){
  
  node <- nodes[noddy,1]
  
  #list of parts in node
  parts_on <- subset(result, repos == node)
  
  #list of some parts in other games (nodes)
  parts_of <- subset(result, contris %in% parts_on$contris)
  
  if(nrow(parts_on) < nrow(parts_of)){
    
    #just the other nodes
    parts_ofc <- unique(parts_of$repos)
    parts_ofc <- parts_ofc[! parts_ofc %in% node]
    
    tie <- data.frame()
    
    for (luca in 1:length(parts_ofc)){
      
      tie_s <- nrow(subset(parts_of, repos == parts_ofc[luca]))
      tie <- rbind(tie, tie_s)
      
    }
    
    edges_id <- data.frame(cbind(node, parts_ofc))
    
    edges_id$temp <- apply(edges_id, 1, function(x) paste(sort(x), collapse= ""))
    
    edges_id$tie_s <- tie[,1]
    names(edges_id)[4] <- "tie_s"
    
    edges <- rbind(edges, edges_id)
    
    print(noddy)
    
  }
  
  
}

a <- edges[!duplicated(edges$temp),]
a <- a[,c(1:2,4)]

names(a)[1] <- "from"
names(a)[2] <- "to"
names(a)[3] <- "strength"

edges_new <- a[rep(seq.int(1,nrow(a)), a$strength), 1:2]



#create strength csv--------

total <- data.frame()
for(test in 1:nrow(nodes)){
  needle <- nodes[test,1]
  
  haystack <- subset(edges_new, from == needle | to == needle)
  
  temp <- nrow(haystack)
  total <- rbind(total, temp)
  
}

nodes <- cbind(nodes, total)
colnames(nodes)[1] <- "repos"
colnames(nodes)[2] <- "strength"

write.csv(nodes, file = "Q:/Projekte/DFG_ENOC/R/ENOC_data_setup/nodes.csv" )

#plot graphs---------

routes_igraph <- graph_from_data_frame(d = edges_new,
                                       vertices = nodes,
                                       directed = FALSE)



V(routes_igraph)$size <- 8

V(routes_igraph)$frame.color <- "white"

V(routes_igraph)$color <- "orange"

V(routes_igraph)$label <- "" 

E(routes_igraph)$arrow.mode <- 0

l <- layout_with_fr(routes_igraph)

plot(routes_igraph, layout=l)


#test to remove isolates and create some fun graphs with time----------
colnames(nodes)[1] <- "contris"

nodes2 <- subset(nodes, contris %in% edges_new$from | contris %in% edges_new$to)



routes_igraph <- graph_from_data_frame(d = edges_new,
                                       vertices = nodes2,
                                       directed = FALSE)



V(routes_igraph)$size <- 6

V(routes_igraph)$frame.color <- "white"

V(routes_igraph)$color <- "orange"

V(routes_igraph)$label <- "" 

E(routes_igraph)$arrow.mode <- 0

l <- layout_with_fr(routes_igraph)

plot(routes_igraph, layout=l)


eigen <- eigen_centrality(routes_igraph)
coreness <- data.frame(graph.coreness(routes_igraph))
constraint <- igraph::constraint(routes_igraph)

#cluster <- cluster_edge_betweenness(routes_igraph)



net_results <- data.frame(eigen$vector, coreness[1], constraint, r_lib$owner_stars, r_lib$owner_date_crea)
Sys.timezone()



net_results$r_lib.owner_date_crea <- as.POSIXlt(net_results$r_lib.owner_date_crea)

net_results <- cbind(newColName = rownames(net_results), net_results)
rownames(net_results) <- 1:nrow(net_results)

net_results <- subset(net_results, newColName %in% edges_new$from | newColName %in% edges_new$to)



#get time
d3 <- as.POSIXlt("2024-04-11")
net_results$age <- d3 - net_results$r_lib.owner_date_crea
net_results$age <- as.numeric(as.character(net_results$age))
net_results$age <- net_results$age/max(net_results$age)


#first plot
install.packages("viridis")  # Install
library("viridis")           # Load


sn_colorrange <- colorRampPalette(c("yellow", "orange", "red", "darkred"))
sn_color <- sn_colorrange(net_results$age)

n_bins <- 5
sn_color <- sn_colorrange(n_bins)


V(routes_igraph)$size <- 6

V(routes_igraph)$frame.color <- "white"

V(routes_igraph)$color <- sn_color[cut(net_results$age, breaks=n_bins )]

V(routes_igraph)$label <- "" 

E(routes_igraph)$arrow.mode <- 0

l <- layout_with_fr(routes_igraph)

plot(routes_igraph, layout=l)



#try plotting with qgraph-----------
#install.packages("qgraph")
library(qgraph)

print("run")

g <- routes_igraph

png("plot1.png", height=6, width=12, units="in", res=250)
par(mfrow=c(1, 3))

plot(g,layout=layout_with_fr,vertex.size=4,vertex.label=NA)
mtext("layout_with_fr", side=1)

e <- get.edgelist(g,names=FALSE)
l <- qgraph.layout.fruchtermanreingold(e,vcount=vcount(g))
plot(g,layout=l,vertex.size=4,vertex.label=NA)
mtext("qgraph.layout.fruchtermanreingold default", side=1)

l <- qgraph.layout.fruchtermanreingold(e,vcount=vcount(g),
                                       area=8*(vcount(g)^2),repulse.rad=(vcount(g)^3.1))
plot(g,layout=l,vertex.size=4,vertex.label=NA)

mtext("qgraph.layout.fruchtermanreingold modified", side=1)




#try get timpestamped commits----


n <- 9999
final_info <- data.frame()
info <- data.frame()

for(run_rep in 1:nrow(r_lib)){
repo_run <- r_lib[run_rep,1]


for(pages_xy in 1:n){
  my_repos <- gh("GET /repos/{owner}/{repo}/commits", owner = "r-lib", repo = repo_run,  per_page = 100, page = pages_xy)
  print(repo_run)
  
  if(is.null(my_repos) == TRUE){
    break} else {
      print(pages_xy)
    }
  
  for(list in 1:length(my_repos)){
    
    if(is.null(my_repos[[list]]$commit$author$date) == FALSE){
      temp_date <-my_repos[[list]]$commit$author$date
    } else {
      temp_date <- NA
    }
    
    if(is.null(my_repos[[list]]$commit$message) == FALSE){
      temp_message <- my_repos[[list]]$commit$message
    } else {
      temp_message <- NA
    }
    
    if(is.null(my_repos[[list]]$author$login) == FALSE){
      temp_login <- my_repos[[list]]$author$login
    } else {
      temp_login <- NA
    }
    
    if(is.null(my_repos[[list]]$author$type) == FALSE){
      temp_type <- my_repos[[list]]$author$type
    } else {
      temp_type <- NA
    }
    
    temp_data <- cbind(temp_login, temp_date, temp_type, temp_message)
    
    info <- rbind(info, temp_data)
  }
  
  if(length(my_repos) != 100){
      break} else {
        print(pages_xy)
      }
  
}


info$repo <- repo_run
final_info <- rbind(final_info, info)
info <- data.frame()
}

#save(final_info, file = "Q:/Projekte/DFG_ENOC/R/ENOC_data_setup/r-lib-info-network-Tsed.Rdata" )

#get comments--------
my_repos_com <- gh("GET /repos/{owner}/{repo}/pulls/comments", owner = "r-lib", repo ="httr")

my_repos_com[[1]]$body

my_repos_com_com <- gh("GET /repos/{owner}/{repo}/pulls/comments/{comment_id}", owner = "r-lib", repo ="xmlparsedata",
                       comment_id = 313814067)

#make timestamped network----


load("Q:/Projekte/DFG_ENOC/R/ENOC_data_setup/r-lib-info-network-Tsed.Rdata" )
load("Q:/Projekte/DFG_ENOC/R/ENOC_data_setup/r-lib-repos.Rdata")


#drop na
final_info <- drop_na(final_info)

nodes <- data.frame((unique(final_info$repo)))


#time work conversion
r_lib$owner_date_crea <- as.POSIXlt(r_lib$owner_date_crea)
d3 <- as.POSIXlt("2024-04-15")

r_lib$age <- d3 - r_lib$owner_date_crea
r_lib$age <- as.numeric(as.character(r_lib$age))
#r_lib$age <- r_lib$age/max(r_lib$age)


final_info$temp_date <- as.POSIXlt(final_info$temp_date)
final_info$age <- d3 - final_info$temp_date
final_info$age <- as.numeric(as.character(final_info$age))

#create age groups
cutoffs <- quantile(r_lib$age, probs = seq(0.1,1,length=10))
r_lib$age_group <- 0

for(cuts in 1:10){
  
  if(cuts > 1){
    r_lib$age_group <- if_else(r_lib$age <= cutoffs[cuts] & r_lib$age > cutoffs[cuts-1], cuts, r_lib$age_group)
  } else {
    r_lib$age_group <- if_else(r_lib$age <= cutoffs[cuts], cuts, r_lib$age_group)
  }
 
}

#loop groups-----

for(groupies in 1:length(cutoffs)){
  
  if(groupies == 10){
    final_info_age_group <- final_info
  } else {
    final_info_age_group <- subset(final_info, age > cutoffs[10-groupies])
  }

  age_nodes <- data.frame(unique(final_info_age_group$repo))
  
  edges_start <- final_info_age_group %>%
    group_by(temp_login) %>% # or: group_by_at(vars(-score))
    count(repo)
  
  edges_time <- data.frame()
  
  
  for (noddy in 1:nrow(age_nodes)){
    
    node <- age_nodes[noddy,1]
    
    #list of parts in node
    parts_on <- subset(edges_start, repo == node)
    
    #list of some parts in other games (nodes)
    parts_of <- subset(edges_start, temp_login %in% parts_on$temp_login)
    
    if(nrow(parts_on) < nrow(parts_of)){
      
      #just the other nodes
      parts_ofc <- unique(parts_of$repo)
      parts_ofc <- parts_ofc[! parts_ofc %in% node]
      
      tie <- data.frame()
      
      for (luca in 1:length(parts_ofc)){
        
        tie_s <- nrow(subset(parts_of, repo == parts_ofc[luca]))
        tie <- rbind(tie, tie_s)
        
      }
      
      edges_id <- data.frame(cbind(node, parts_ofc))
      
      edges_id$temp <- apply(edges_id, 1, function(x) paste(sort(x), collapse= ""))
      
      edges_id$tie_s <- tie[,1]
      names(edges_id)[4] <- "tie_s"
      
      edges_time <- rbind(edges_time, edges_id)
      
      print(noddy)
      
    }
    
    
  }
  
  a <- edges_time[!duplicated(edges_time$temp),]
  a <- a[,c(1:2,4)]
  
  names(a)[1] <- "from"
  names(a)[2] <- "to"
  names(a)[3] <- "strength"
  
  edges_new <- a[rep(seq.int(1,nrow(a)), a$strength), 1:2]
  
  
  nam_e <- paste("Network_E", groupies, sep = "")
  nam_n <- paste("Network_N", groupies, sep = "")
  
  assign(paste0(nam_e), edges_new)
  assign(paste0(nam_n), age_nodes)
  
  print(nam_e)
}

edges_by_time <- list(Network_E1, Network_E2, Network_E3, Network_E4, Network_E5, Network_E6, Network_E7, Network_E8, 
                            Network_E9, Network_E10)


nodes_by_time <- list(Network_N1, Network_N2, Network_N3, Network_N4, Network_N5, Network_N6, Network_N7, Network_N8, 
                      Network_N9, Network_N10)

#create network and calculate properties

props <- data.frame()
for(groupies in 1:length(edges_by_time)){
  nam_e <- paste("Time_group_", groupies, sep = "")
  
  routes_igraph <- graph_from_data_frame(d = edges_by_time[groupies],
                                         vertices = nodes_by_time[groupies],
                                         directed = FALSE)
  
  
  ED <- edge_density(routes_igraph, loops=F)
  TR <- transitivity(routes_igraph, type="global")
  DI <- diameter(routes_igraph, directed=F)
  
  #degree(routes_igraph, mode="all")
  #deg.dist <- degree_distribution(routes_igraph, cumulative=T, mode="all")
  #plot( x=0:max(deg), y=1-deg.dist, pch=19, cex=1.2, col="orange",
  #xlab="Degree", ylab="Cumulative Frequency")
  
  MD <- mean_distance(routes_igraph, directed=F)
  
  #clique <- cliques(routes_igraph) # list of cliques
  #sapply(cliques(routes_igraph), length) # clique sizes
  #largest_cliques(routes_igraph) # cliques with max number of nodes
  
  #cluster
  ceb <- cluster_edge_betweenness(routes_igraph)
  #dendPlot(ceb, mode="hclust")
  
  Num_Clu <- length(unique(ceb$membership))
  Max_Clu <- max(table(ceb$membership))
  
  #centrality
  eigen <- eigen_centrality(routes_igraph)
  ESD <- sd(eigen[1]$vector)
  
  #coreness
  coreness <- data.frame(graph.coreness(routes_igraph))
  M_cor <- max(coreness[1])
  SD_cor <- sd(coreness[1]$graph.coreness.routes_igraph.)
  
  #constraint
  constraint <- drop_na(data.frame(igraph::constraint(routes_igraph)))
  SD_con <- sd(constraint[1]$igraph..constraint.routes_igraph.)
  
  
  #combine
  
  net_results <- data.frame(nam_e, ED, TR, DI, MD, Num_Clu, Max_Clu, ESD, M_cor, SD_cor, SD_con)
  props <- rbind(props, net_results)
  print(nam_e)
}

#save(props, file = "Q:/Projekte/DFG_ENOC/R/ENOC_data_setup/repo_props.Rdata" )
#save(edges_by_time, file = "Q:/Projekte/DFG_ENOC/R/ENOC_data_setup/edges_by_time_props.Rdata" )
#save(nodes_by_time, file = "Q:/Projekte/DFG_ENOC/R/ENOC_data_setup/nodes_by_time_props.Rdata" )

#plots----------
load(file = "Q:/Projekte/DFG_ENOC/R/ENOC_data_setup/edges_by_time_props.Rdata" )
load(file = "Q:/Projekte/DFG_ENOC/R/ENOC_data_setup/nodes_by_time_props.Rdata" )
load(file = "Q:/Projekte/DFG_ENOC/R/ENOC_data_setup/repo_props.Rdata" )
library(ggplot2)
library(reshape)

props$year_clu <- seq(1:10)
plot(props$year_clu, props$Num_Clu)

props2 <- props[2:12]

for(cols in 1:(ncol(props2)-1)){
  
  props2[[cols]] <- (props2[[cols]]-mean(props2[[cols]]))/(sd(props2[[cols]]))
  
}

Molten <- melt(props2, id.vars = "year_clu")
colnames(Molten)[2] <- "variable"
Molten$variable <- as.character(Molten$variable)
Molten <- subset(Molten, variable == "ED" | variable == "M_cor")
ggplot(Molten, aes(x = year_clu, y = value, colour = variable)) + geom_line()




#plot network

routes_igraph <- graph_from_data_frame(d = edges_by_time[17:18],
                                       
                                       directed = FALSE)


#cluster
ceb <- cluster_edge_betweenness(routes_igraph)
#dendPlot(ceb, mode="hclust")

Num_Clu <- length(unique(ceb$membership))
Max_Clu <- max(table(ceb$membership))

V(routes_igraph)$size <- 8
V(routes_igraph)$frame.color <- "white"
V(routes_igraph)$color <- ceb$membership
V(routes_igraph)$label <- "" 

E(routes_igraph)$arrow.mode <- 0
l <- layout_with_fr(routes_igraph)

plot(routes_igraph, layout=l)


nrow(subset(final_info, temp_login == final_info$temp_login[21541]))

finder <- subset(final_info, temp_login == final_info$temp_login[234])
length(unique(finder$repo))
try2 <- finder %>% group_by(repo) %>% count()

sd(try2$n)

