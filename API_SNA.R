
## Documentation: https://cran.r-project.org/web/packages/rtweet/rtweet.pdf

## Load Libraries
library(rtweet)
library(ggplot2)
library(dplyr)
library(tidyr)
library(igraph)

##-----------------------------------------------------------

#Authorize API
token <- create_token(
  app = "darenapp",
  consumer_key = "xxxx",
  consumer_secret = "xxxx",
  access_token = "xxxx",
  access_secret = "xxxx")

#------------------------------------------------------------------------
###Set the string 
keyword <- "#WLF2018"

###Get the tweets
tweets <- search_tweets(keyword, n = 100)

###Pull the users that tweeted:
users <- dplyr::select(tweets,user_id) %>% distinct(user_id)

###Create a list of initial friends to loop through:
friendlist <- list(users$user_id)

###Loop through friend list and append to network
network <- data.frame()

for (i in friendlist){ 
  nextlist <- get_friends(i, n = 5000, retryonratelimit = TRUE, page = "-1", parse = TRUE, verbose = TRUE, token = NULL)
  
  #bind new results to existing network dataframe
  network <- rbind(network,nextlist)
}

###write this data to a table for now:
write.csv(network, file = "network.csv")

##--------------------------------------------------------------------
##Create categories of all nodes (tweeter: orange, friend: blue)
t <- unique(select(network, user))
t$type <- c("orange") 
f <- unique(select(network, user_id))
f$type <- c("blue") 
names(f) <- c("user", "type")
node <- rbind(t,f)


###sample of the friends to test the graph (better for visualizing)
gsamp <- network[sample(nrow(network),500),] 
gsamp2 <- graph.data.frame(gsamp)
plot(gsamp2, edge.arrow.size=0.1, vertex.size=3, vertex.color=node$type, vertex.label=NA)

#-----------------------------------------------------------------------
###determine centrality to 
centrality <- eigen_centrality(gsamp2)
centdf <- as.data.frame(centrality$vector)
centdf$user <- rownames(centdf)

###rank users based on centrality
ranked <- arrange(centdf,desc(centrality$vector))

#----------------------------------------------------------------------
###Get the usernames of top 10
topten <- ranked[2:10,] 
topten <- list(topten$user)
topten

###Loop through and get the usernames of the top ten users
