#install.packages("gh")
library(gh)

#install.packages("httpuv")
library(httpuv)
#install.packages("httr")
library(httr)

install.packages("allcontributors")
library(allcontributors)

#token

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

req <- GET("https://api.github.com/users/glenn-jocher/repos", gtoken)

req$
  
  a <- GET("https://api.github.com/search/issues?q=type%3apr+state%3aclosed+author%3amegawac&per_page=100&page=1")

get_contributors(org = "r-lib", repo = "pkgcache")

print("this is luca")
#examples

my_repos <- gh("GET /users/{username}/repos", username = "glenn-jocher", token = gtoken)
vapply(my_repos, "[[", "", "name")


my_contris <- gh("GET /repos/{owner}/{repo}/contributors", owner = "ultralytics", repo ="ultralytics")
vapply(my_contris, "[[", "", "login")


my_contris <- gh("GET /repos/{owner}/{repo}/contributors", owner = "r-lib", repo ="pkgcache")
sapply(my_contris, "[[", "", "contributions")


vapply(my_contris, "[[",  "contributions", numeric(1))
sapply (my_contris, max)


try <- data.frame(my_contris)


lapply(my_contris, function(x) as.numeric(unlist(x)))
a <- sapply(my_contris, max)

a <- my_contris[[1]]

a$contributions

df <- data.frame(unlist(a))



#to get all repos
my_owner <- gh("GET /repositories", since = 50000)

#to get org info
my_owner <- gh("GET /orgs/{org}", org = "flipperdevices")
my_owner <- gh("GET /orgs/{org}", org = "r-lib")


#to get the people info in an org
my_owner <- gh("GET /orgs/{org}/members", org = "ultralytics")

#to get repo infos
my_owner <- gh("GET /orgs/{org}/repos", org = "r-lib", per_page = 100, page = 2)

#to get names of contribturos of repo
my_contris <- gh("GET /repos/{owner}/{repo}/contributors", owner = "ultralytics", repo ="ultralytics")
vapply(my_contris, "[[", "", "login")
#still unclear how to extract number of contributions

#to get forks of repo
my_forks <- gh("GET /repos/{owner}/{repo}/forks", owner = "r-lib", repo ="xmlparsedata")


#to get info of repos by user
my_repos <- gh("GET /users/{username}/repos", username = "gaborcsardi")


#to get info by user
my_repos <- gh("GET /users/{username}", username = "gaborcsardi")
#to get info by user

my_repos <- gh("GET /users/{username}/contributions", username = "gaborcsardi")


#get commits of a repo
my_repos <- gh("GET /repos/{owner}/{repo}/commits", owner = "r-lib", repo ="xmlparsedata",  page =1)
my_repos[[1]]$sha

#get commits based on pull
my_repos_sha <- gh("GET /repos/{owner}/{repo}/commits/{commit_sha}/pulls"
                   , owner = "r-lib", repo ="xmlparsedata", commit_sha = my_repos[[1]]$sha)
my_repos[[1]]$body

my_repos <- gh("GET /repos/{owner}/{repo}", owner = "r-lib", repo ="xmlparsedata")

my_reposa <- gh("GET /repos/{owner}/{repo}", owner = "briatte", repo ="awesome-network-analysis")

my_reposb <- gh("GET /repos/{owner}/{repo}", owner = "hadley", repo ="adv-r")


#get languages
/repos/{owner}/{repo}/languages

?gh_qql


my_repos_com <- gh("GET /repos/{owner}/{repo}/pulls/comments", owner = "r-lib", repo ="xmlparsedata")



my_repos_com_com <- gh("GET /repos/{owner}/{repo}/pulls/comments/{comment_id}", owner = "r-lib", repo ="xmlparsedata",
                       comment_id = 313814067)


library(gh)


#get all reps of org
my_owner_general <- gh("GET /orgs/{org}", org = "r-lib")
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
  
  results <- data.frame(r_lib[id,1], contris,contris_amount)
  result <- rbind(result, results)
  Sys.sleep(2)
}


#get commits of a repo
my_repos <- gh("GET /repos/{owner}/{repo}/commits", owner = "r-lib", repo ="httr",  page =1, per_page = 100)
my_repos[[1]]$sha

#get commits based on pull
my_repos_sha <- gh("GET /repos/{owner}/{repo}/commits/{commit_sha}/pulls"
                   , owner = "r-lib", repo ="xmlparsedata", commit_sha = my_repos[[1]]$sha)
my_repos[[1]]$body




#to get info of repos by user

result <- data.frame()
for (id in 1:length(contris)){ 
  print(contris[id])
  
  my_repos <- gh("GET /users/{username}/repos", username = contris[id], per_page = 100)
  repos <- vapply(my_repos, "[[", "", "name")
  
  
  node_rep <- data.frame(contris[id], repos)
  
  result <- rbind(result, node_rep)
  Sys.sleep(2)
  
}

try <- unique(result$repos)

try <- 