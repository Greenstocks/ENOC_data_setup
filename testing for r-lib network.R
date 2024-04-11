
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


#create adjacency matrix from object---------

length(unique(result$contris))
