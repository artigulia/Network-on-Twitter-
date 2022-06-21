
######### Calling libraries ###############

library("rtweet")
library("igraph")

#Gathering the Network

key="<Enter key>"
secret="<Enter secret>"
token <- create_token(
  app="< Application name >",
  consumer_key=key,
  consumer_secret=secret)

options(RCurlOptions = list(verbose = FALSE, 
                            capath = system.file("CurlSSL", "cacert.pem", package = "RCurl"), 
                            ssl.verifypeer = FALSE))

tweets = search_tweets(
  "\"elonmusk\" \"twitter acquisition\" ",n=500,include_rts = FALSE, token = token
)
#?search_tweets
View(tweets)

# Extracting top 20 most occurring words

tweet.words = strsplit(tweets$text, "[^A-Za-z]+") #splitting words of each tweet
word.table = table(unlist(tweet.words)) #saving the tokens in a table
word.table

sort(word.table, decreasing = TRUE)[1:20] # Sorting the top 20 words in descending order

## remove any term that starts with http
tweets = tweets$text
tweets = gsub("http[^[:space:]]*", "", tweets)
## remove hashtags (terms that start with #)
tweets = gsub("#[^[:space:]]*", "", tweets)
## remove author names (terms that start with @)
tweets = gsub("@[^[:space:]]*", "", tweets)
tweets

library("tm")
tweet.corpus = Corpus(VectorSource(tweets))
## Convert to ASCII
tweet.corpus = tm_map(tweet.corpus,
                      function(x) iconv(x, to='ASCII', 
                                        sub='byte'))

tweet.corpus

tweet.corpus = tm_map(tweet.corpus, removeNumbers)
tweet.corpus = tm_map(tweet.corpus, removePunctuation)
tweet.corpus = tm_map(tweet.corpus, stripWhitespace)
tweet.corpus = tm_map(tweet.corpus, tolower)
tweet.corpus = tm_map(tweet.corpus, removeWords, 
                      stopwords("english"))
tweet.corpus = tm_map(tweet.corpus, stemDocument)

## ensure all documents are of type PlainTextDocument
#tweet.corpus = tm_map(tweet.corpus, PlainTextDocument)
## create the DTM structure
tweet.dtm = DocumentTermMatrix(tweet.corpus) # create the DocumentTermMatrix object
## extract the matrix
tweet.matrix = as.matrix(tweet.dtm) # convert to a matrix

## compute the words that appear in the most tweets
ft = colSums(tweet.matrix > 0)
sort(ft, decreasing = TRUE)[1:20]

#install.packages("tm")


################### Q2. plotting tweets ###############

tweets_data <- network_data(tweets, "mention")
tweets_data
attr(tweets_data, "id")

if (requireNamespace("igraph", quietly = TRUE)) {
  
  ## (1) convert directly to graph object representing semantic network
  tweets_data <- network_graph(tweets)
  
  ## (2) plot graph via igraph.plotting
  plot(tweets_data,layout=layout.fruchterman.reingold, vertex.size=8)
}

#undirected graph

plot(as.undirected(tweets_data),layout=layout.fruchterman.reingold, vertex.size=8)


######################## COMPONENTS ###################

# split the graph into components
gd = decompose(tweets_data)

#Size of each component
size_components_g <-clusters(tweets_data, mode="weak")$csize
size_components_g # size of each component

####

comps <- components(tweets_data)
comps

#Size of each component
comps$csize

# which is the biggest one?
max_size <- max(comps$csize)
max_size

giant_id <- which.max(comps$csize)
giant_id
#marking the largest component

plot(as.undirected(tweets_data), mark.groups = list(comps$membership == giant_id), 
     mark.col = 'grey',layout=layout.fruchterman.reingold, vertex.size=6)

## extract the subgraph containing only nodes from chosen partition.

undir = as.undirected(comps[[which.max(comps$csize)]])
plot(undir,layout=layout.fruchterman.reingold, vertex.size=6)


####### Q3. Graph STatistics -----------------------------

graph.density(undir,loop=FALSE)

diameter(undir, weights = NA)

#degree_distribution(undir)
ba_game_deg_dist_tot <- degree_distribution(undir, cumulative = FALSE, mode = c("total"))
ba_game_deg_dist_tot 

plot(ba_game_deg_dist_tot)

ba_game_plaw <- fit_power_law(ba_game_deg_dist_tot, implementation = c("plfit"), 
                              force.continuous = FALSE)
ba_game_plaw

E(undir)


###### Q4. Neighbourhood overlap ---------------------

## get the neighbourhood graph of all nodes.
gn = neighborhood(undir, order = 1)

## get pair of nodes that are at the end of each edge.
g.ends = ends(undir, E(undir))
g.ends

# number of edges
N = nrow(g.ends)
N #300

# make space for neighbourhood overlap score
NO = rep(0, N)

for (a in 1:N) {
  ## for every edge
  
  x = g.ends[a,1] # x is the node at one end of the edge
  y = g.ends[a,2] # y is the node at the other end of the edge
  
  ## compute the intersection of the neighbourhoods of x and y
  i = length(intersect(gn[[x]], gn[[y]])) - 2
  ## compute the union of the neighbourhoods of x and y
  u = length(union(gn[[x]], gn[[y]])) - 2
  
  ## Note that we subtract 2 since each neighbourhood includes x and y.
  ## we don't want to include x and y in the counts.
  
  ## the neighbourhood overlap is the intersection/union
  NO[a] = i/u
  #E(rstats_net)$weight[a] <- 1/( length( V(rstats_net)[a] ) -1 )
}


E(undir)$weight <- runif(length(E(undir)), 1, 15)
length(E(undir)$weight)
length(E(undir))
length(NO)
NO
plot(E(undir)$weight, NO)


########### PAGE Rank ###################

Account_popularity <- page_rank(undir)$vector
#Top 10 popular accounts
Account_popularity[1:10]

############## Degree of vertex ####

vertex_degree <- degree(
  undir,
  v = V(undir),
  loops = TRUE,
  normalized = FALSE
)

vertex_degree[1:10]

