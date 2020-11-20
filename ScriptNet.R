
##Network analysis of twitter data

# Count the number of nodes in retweet_graph
gorder(retweet_graph)

# Count the number of edges in retweet_graph
gsize(retweet_graph)

# Calculate the graph density of retweet_graph
graph.density(retweet_graph)

# Plot retweet_graph
plot(retweet_graph)

# From previous step
in_deg <- degree(retweet_graph, mode = "in")
out_deg <- degree(retweet_graph, mode = "out")
has_tweeted_once_never_retweeted <- in_deg == 1 & out_deg == 0
has_never_tweeted_retweeted_once <- in_deg == 0 & out_deg == 1
vertex_colors <- rep("black", gorder(retweet_graph))
vertex_colors[has_tweeted_once_never_retweeted] <- "blue"
vertex_colors[has_never_tweeted_retweeted_once] <- "green"

plot(
  # Plot the network
  retweet_graph, 
  # Set the vertex colors to vertex_colors
  vertex.color = vertex_colors
)



# Get 0.99 quantile of betweenness 
betweenness_q99 <- quantile(retweet_btw, p = 0.99)

# Get top 1% of vertices by betweenness
top_btw <- retweet_btw[retweet_btw > betweenness_q99]

# Get 0.99 quantile of eigen-centrality
eigen_centrality_q99 <- quantile(retweet_ec, p = 0.99)

# Get top 1% of vertices by eigen-centrality
top_ec <- retweet_ec[retweet_ec > eigen_centrality_q99]

# See the results as a data frame
data.frame(
  Rank = seq_along(top_btw), 
  Betweenness = names(sort(top_btw, decreasing = TRUE)), 
  EigenCentrality = names(sort(top_ec, decreasing = TRUE))
)
# Calculate eigen-centrality using eigen_centrality()
retweet_ec <- eigen_centrality(retweet_graph, directed = TRUE)$vector

# Get a summary of retweet_btw
summary(retweet_ec)

# Calc proportion of vertices with eigen-centrality close to zero
almost_zero <- 1e-10
mean(retweet_ec<almost_zero)


# From previous step
transformed_btw <- log(retweet_btw + 2)
V(retweet_graph)$size <- transformed_btw

# Subset nodes for betweenness greater than 0.99 quantile
vertices_high_btw <- V(retweet_graph)[retweet_btw > betweenness_q99]

# Induce a subgraph of the vertices with high betweenness
retweet_subgraph <- induced_subgraph(retweet_graph, vertices_high_btw)

# Plot the subgraph
plot(retweet_subgraph)



# From previous steps
list_of_clique_vertices <- cliques(mention_graph, min = 3, max = 3)
clique_ids <- lapply(list_of_clique_vertices, as_ids)
has_revodavid <- sapply(
  clique_ids, 
  function(clique) {
    "revodavid" %in% clique
  }
)
cliques_with_revodavid <- clique_ids[has_revodavid]
people_in_cliques_with_revodavid <- unique(unlist(cliques_with_revodavid))

# Induce subgraph of mention_graph with people_in_cliques_with_revodavid 
revodavid_cliques_graph <- induced_subgraph(mention_graph, people_in_cliques_with_revodavid)

# Plot the subgraph
plot(revodavid_cliques_graph)



# From previous step
retweet_graph_undir <- as.undirected(retweet_graph)
communities_fast_greedy <- cluster_fast_greedy(retweet_graph_undir)
communities_infomap <- cluster_infomap(retweet_graph_undir)
communities_louvain <- cluster_louvain(retweet_graph_undir)

compare(communities_fast_greedy, communities_infomap)

# Compare fast greedy with louvain
compare(communities_fast_greedy, communities_louvain)

# Compare infomap with louvain
compare(communities_infomap, communities_louvain)

two_users <- c("bass_analytics", "big_data_flow")

# Subset membership of communities_fast_greedy by interesting_users
membership(communities_fast_greedy)[two_users]

# Do the same for communities_infomap
membership(communities_infomap)[two_users]


# ... and for communities_louvain
membership(communities_louvain)[two_users]




# From previous step
V(retweet_graph)$color <- factor(membership(communities_louvain))
is_crossing <- crossing(communities_louvain, retweet_graph)
E(retweet_graph)$lty <- ifelse(is_crossing, "solid", "dotted")
community_size <- sizes(communities_louvain)
in_mid_community <- unlist(communities_louvain[community_size > 50 & community_size < 90])
retweet_subgraph <- induced_subgraph(retweet_graph, in_mid_community)

# Plot those mid-size communities
plot(retweet_subgraph, vertex.label = NA, edge.arrow.width = 0.8, edge.arrow.size = 0.2, 
     coords = layout_with_fr(retweet_subgraph), margin = 0, vertex.size = 3)





###Bikes


# From previous steps
subscribers <- bike_dat %>% filter(usertype == "Subscriber")
n_subscriber_trips <- nrow(subscribers)
subscriber_trip_graph <- subscribers %>% 
  group_by(from_station_id, to_station_id) %>% 
  summarize(weights = n() / n_subscriber_trips) %>%
  graph_from_data_frame()
customers <- bike_dat %>% filter(usertype == "Customer")
n_customer_trips <- nrow(customers)
customer_trip_graph <- customers %>% 
  group_by(from_station_id, to_station_id) %>% 
  summarize(weights = n() / n_customer_trips) %>%
  graph_from_data_frame()

# How many different trips were made by subscribers?
gsize(subscriber_trip_graph)

# ... and by customers?
gsize(customer_trip_graph)




# Induce a subgraph from customer_trip_graph of the first 12 vertices
twelve_customer_trip_graph <- induced.subgraph(customer_trip_graph, vids = 1:12)

# Plot the subgraph
plot(
  twelve_customer_trip_graph, 
  main = "Customers"
)


# From previous step
farthest_vertices(subscriber_trip_graph)
farthest_vertices(customer_trip_graph)

# Look at the function definition
calc_physical_distance_m

# Calc physical distance between end stations for subscriber graph
calc_physical_distance_m(200, 298)

# Calc physical distance between end stations for customer graph
calc_physical_distance_m(116, 281)



trip_deg <- data_frame(
  # Find the "out" degree distribution
  trip_out = degree(trip_g_simp, mode = "out"), 
  # ... and the "in" degree distribution
  trip_in = degree(trip_g_simp, mode = "in"),
  # Calculate the ratio of out / in
  ratio = trip_out / trip_in
)

trip_deg_filtered <- trip_deg %>%
  # Filter for rows where trips in and out are both over 10
  filter(trip_out > 10, trip_in > 10) 

# Plot histogram of filtered ratios
hist(trip_deg_filtered$ratio)




trip_strng <- data_frame(
  # Find the "out" strength distribution
  trip_out = strength(trip_g_simp, mode = "out"), 
  # ... and the "in" strength distribution
  trip_in = strength(trip_g_simp, mode = "in"),
  # Calculate the ratio of out / in
  ratio = trip_out/trip_in
)

trip_strng_filtered <- trip_strng %>%
  # Filter for rows where trips in and out are both over 10
  filter(trip_in>10, trip_out>10) 

# Plot histogram of filtered ratios
hist(trip_strng_filtered$ratio)


# From previous step
g275 <- make_ego_graph(trip_g_simp, 1, nodes = "275", mode= "out")[[1]]

# Plot ego graph
plot(
  g275, 
  edge.width = E(g275)$weight,
  # Use geographic coordinates
  layout = latlong, 
)


# This calculates weighted eigen-centrality 
ec_weight <- eigen_centrality(trip_g_simp, directed = TRUE)$vector

# Calculate unweighted eigen-centrality 
ec_unweight <- eigen_centrality(trip_g_simp, directed = TRUE, weights = NA)$vector

# This calculates weighted closeness
close_weight <- closeness(trip_g_simp)

# Calculate unweighted closeness
close_unweight <- closeness(trip_g_simp, weights = NA)

# Get vertex names
vertex_names <- names(V(trip_g_simp))

# Complete the data frame to see the results
data_frame(
  "Weighted Eigen Centrality" = vertex_names[which.min(ec_weight)],
  "Unweighted Eigen Centrality" = vertex_names[which.min(ec_unweight)],
  "Weighted Closeness" = vertex_names[which.min(close_weight)],
  "Unweighted Closeness" = vertex_names[which.min(close_unweight)]
)


# Find the actual value using transitivity()
clust_coef <- transitivity(trip_g_simp, type = "global")

# Get randomization parameters using gorder() and edge_density()
nv <- gorder(trip_g_simp)
ed <- edge_density(trip_g_simp)

# Create an empty vector to hold output of 300 simulations
graph_vec <- rep(NA, 300)

# Calculate clustering for random graphs using transitivity()
for(i in 1:300){
  graph_vec[i]<- transitivity(erdos.renyi.game(nv, ed, "gnp", directed = TRUE), type = "global")
}

# Plot a histogram of the simulated values stored in graph_vec
hist(graph_vec, xlim = c(0.35, 0.6), main = "Unweighted clustering randomization")

# Add a line with the true value stored in clust_coef
abline(v = clust_coef, col = "red")



# Find the mean local weighted clustering coeffecient using transitivity()
m_clust <- mean(transitivity(trip_g_simp, type = "weighted"))

# Get simulation parameters from trip_g_simp
nv <- gorder(trip_g_simp)
ed <- edge_density(trip_g_simp)

# Create an empty vector of size 100
graph_vec <- rep(NA, 100)

for(i in 1:100) {
  g_temp <- erdos.renyi.game(nv, ed, "gnp", directed = TRUE)
  # Sample existing weights and add them to the random graph
  E(g_temp)$weight <- sample(x = E(trip_g_simp)$weight, size = gsize(g_temp), replace = TRUE)
  # Get the mean transitivity of the temporary graph p_temp
  graph_vec[i]<- mean(transitivity(g_temp, type = "weighted"))
}

# Plot a histogram of the simulated values stored in graph_vec
hist(graph_vec, xlim = c(0.35, 0.7), main = "Unweighted clustering randomization")

# Add a line with the true value stored in m_clust
abline(v = m_clust, col = "red")





####GGnet2


# From previous step
retweet_samp <- induced_subgraph(retweet_graph, vids = verts)

# Convert to a network object
retweet_net <- asNetwork(retweet_samp)

# Plot using GGally 
ggnet2(retweet_net, edge.size = 0.5, node.color = "black", node.size = 1)


ggplot(ggnetwork(retweet_samp), aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_nodes() +
  # Change the edges to arrows of length 6 pts
  geom_edges(arrow = arrow(length = unit(6, "pt"))) +
  # Use the blank theme
  theme_blank()


# From previous step
retweet_net <- asNetwork(retweet_graph)

# Update the plot
ggnet2(
  retweet_net, 
  node.size = "cent", 
  node.color = "comm", 
  color.palette = "Spectral",
  # Set the edge color
  edge.color = c("color", "gray90")
) +
  # Turn off the size guide
  guides(size = FALSE)


# Basic plot where we set parameters for the plots using geom_edegs() and geom_nodes()
ggplot(ggnetwork(retweet_graph_smaller, arrow.gap = 0.01), 
       aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(arrow = arrow(length = unit(6, "pt"), 
                           type = "closed"), curvature = 0.2, color = "black") + 
  geom_nodes(size = 4, aes(color = comm)) + 
  theme_blank()

# Added guide legend, changed line colors, added size 
ggplot(ggnetwork(retweet_graph_smaller, arrow.gap = 0.01), 
       aes(x = x, y = y, xend = xend, yend = yend)) + 
  geom_edges(arrow = arrow(length = unit(6, "pt"), 
                           type = "closed"), curvature = 0.2, lwd = 0.3, aes(color = comm)) +
  geom_nodes(aes(color = comm, size = cent)) + 
  theme_blank() + 
  guides(color = guide_legend(title = "Community"), 
         size = guide_legend(title = "Centrality"))


# Add betweenness centrality using betweenness()
V(trip_g_simp)$cent <- betweenness(trip_g_simp)

# Create a ggplot object with ggnetwork to render using 
# ggiraph, use geom_nodes() to set centrality
g <- ggplot(ggnetwork(trip_g_simp, arrow.gap = 0.01), aes(x = x, y = y, xend = xend, yend = yend)) + 
  geom_edges(color = "black") + 
  geom_nodes(aes(size = cent)) + 
  theme_blank() 
plot(g)

# Create ggiraph object and assign the tooltip to be interactive using geom_point_interactive()
my_gg <- g + geom_point_interactive(aes(tooltip = round(cent, 2), data_id = round(cent, 2)), size = 2) 

# Plot by printing my_gg
hover_css = "cursor:pointer;fill:red;stroke:red;r:3pt"
ggiraph(code = print(my_gg), hover_css = hover_css, tooltip_offx = 10, tooltip_offy = -10)



# Add community membership as a vertex attribute using the cluster_walktrap() algorithm
V(retweet_graph)$comm <- membership(cluster_walktrap(retweet_graph))

# Induce a subgraph
retweet_samp <- induced_subgraph(retweet_graph, which(V(retweet_graph)$comm %in% 10:13))

# Plot to see what it looks like without an interactive plot using ggnetwork()
ggplot(ggnetwork(retweet_samp, arrow.gap = 0.01), aes(x = x, y = y, xend = xend, yend = yend)) + 
  geom_edges(color = "black") + 
  geom_nodes(aes(color = as.factor(comm))) + 
  theme_blank()  

# Convert retweet_samp to a networkD3 object
nd3 <- igraph_to_networkD3(retweet_samp)

# Assign grouping factor as community membership of retweet_sub_g
nd3$nodes$group = V(retweet_samp)$comm

# Render your D3.js graph using forceNetwork()
forceNetwork(Links = nd3$links, Nodes = nd3$nodes, Source = 'source', 
             Target = 'target', NodeID = 'name', Group = 'group', legend = TRUE, 
             fontSize = 20)


# Convert  trip_df to hive object using edge2HPD()
bike_hive <- edge2HPD(edge_df = as.data.frame(trip_df))

# Assign edgecolor using our custom function
trip_df$edgecolor <- dist_gradient(trip_df$geodist)

# Calculate centrality with betweenness()
bike_cent <- betweenness(trip_g)

# Add axis and radius based on longitude and radius
bike_hive$nodes$radius<- ifelse(bike_cent > 0, bike_cent, runif(1000, 0, 3))

# Set axis as integers using as.integer() 
bike_hive$nodes$axis <- as.integer(dist_stations$axis)
bike_hive$axis.cols <- rep("black", 3)

# Set the edge colors to a heatmap based on trip_df$edgecolor
bike_hive$edges$color <- trip_df$edgecolor
plotHive(bike_hive, method = "norm", bkgnd = "white")


# Add community membership as a vertex attribute using membership()
V(retweet_graph)$comm <- membership(cluster_walktrap(retweet_graph))

# Create a subgraph of retweet_graph
retweet_samp <- induced_subgraph(retweet_graph, which(V(retweet_graph)$comm %in% 10:15))

# Create a ggnetwork plot of retweet_samp
ggplot(ggnetwork(retweet_samp, arrow.gap = 0.01), aes(x = x, y = y, xend = xend, yend = yend)) + 
  geom_edges(color = "black") + 
  geom_nodes(aes(color = as.factor(comm)))+ theme_blank() +
  theme(legend.position = "none")

# Make a Biofabric plot of retweet_samp
retweet_bf <- bioFabric(retweet_samp)
# Create HTMLwidget of retweet_bf
bioFabric_htmlwidget(retweet_bf)


# Create a dataframe of start and end latitude and longitude 
# and add weights based on bike_dat
ll_to_plot <- bike_dat %>% 
  group_by(from_station_id, to_station_id, from_latitude, 
           from_longitude, to_latitude, to_longitude, usertype) %>% 
  summarize(weight = n())

# Create a base map with station points with ggmap()
ggmap(chicago) + 
  geom_segment(data = ll_to_plot, 
               aes(x = from_longitude, y = from_latitude, 
                   xend = to_longitude, yend = to_latitude, 
                   colour = usertype, size = weight), 
               alpha = 0.5)



# Inspect edgeList
head(edgeList)

# Construct the igraph object
network <- graph_from_data_frame(edgeList, directed = FALSE)

# View your igraph object
network






# Inspect the customers dataframe
head(customers)

# Count the number of churners and non-churners
table(customers$churn)

# Add a node attribute called churn
V(network)$churn <- customers$churn

# Visualize the network
plot(network, vertex.label = NA, edge.label = NA,
     edge.color = 'black', vertex.size = 2)




# Add a node attribute called color
V(network)$color <- V(network)$churn

# Change the color of churners to red and non-churners to white
V(network)$color <- gsub("1", "red", V(network)$color) 
V(network)$color <- gsub("0", "white", V(network)$color)

# Plot the network
plot(network, vertex.label = NA, edge.label = NA,
     edge.color = "black", vertex.size = 2)


# Create a subgraph with only churners
churnerNetwork <- induced_subgraph(network, 
                                   v = V(network)[which(V(network)$churn == 1)])

# Plot the churner network 
plot(churnerNetwork, vertex.label = NA, vertex.size = 2)



# Compute the churn probabilities
churnProb <- ChurnNeighbors / (ChurnNeighbors + NonChurnNeighbors)

# Find who is most likely to churn
mostLikelyChurners <- which(churnProb == max(churnProb))

# Extract the IDs of the most likely churners
customers$id[mostLikelyChurners]



# Find churn probability of the 44th customer
churnProb[44]

# Update the churn probabilties and the non-churn probabilities
churnProb_updated <- as.vector((AdjacencyMatrix %*% churnProb) / neighbors)

# Find updated churn probability of the 44th customer
churnProb_updated[44]



# Load the pROC package
library(pROC)

# Compute the AUC
auc(customers$churn, as.vector(churnProb))

# Write a for loop to update the probabilities
for(i in 1:10){
  churnProb <- as.vector((AdjacencyMatrix %*% churnProb) / neighbors)
}

# Compute the AUC again
auc(customers$churn, as.vector(churnProb))







### Homophily
#### Eigenschaften Gemeinsamkeiten
##### Gleich und gleich gesellt sich gern
##### Similar nodes connect more likely together

# Add the column edgeList$FromLabel
edgeList$FromLabel <- customers[match(edgeList$from, customers$id), 2]

# Add the column edgeList$ToLabel
edgeList$ToLabel <- customers[match(edgeList$to, customers$id), 2]

# Add the column edgeList$edgeType
edgeList$edgeType <- edgeList$FromLabel + edgeList$ToLabel

# Count the number of each type of edge
table(edgeList$edgeType)



# Count churn edges
ChurnEdges <- sum(edgeList$edgeType == 2)

# Count non-churn edges
NonChurnEdges <- sum(edgeList$edgeType == 0)

# Count mixed edges
MixedEdges <- sum(edgeList$edgeType == 1)

# Count all edges
edges <- ChurnEdges + NonChurnEdges + MixedEdges

#Print the number of edges
edges




# Count the number of churn nodes
ChurnNodes <- sum(customers$churn == 1)

# Count the number of non-churn nodes
NonChurnNodes <- sum(customers$churn == 0)

# Count the total number of nodes
nodes <- ChurnNodes + NonChurnNodes

# Compute the network connectance
connectance <- 2 * edges / nodes / (nodes - 1)

# Print the value
connectance








###### Dyadicity
#### Wie connected sind gleiche Gruppen unter sich?


# Compute the expected churn dyadicity
ExpectedDyadChurn <- ChurnNodes * (ChurnNodes - 1) * connectance / 2

# Compute the churn dyadicity
DyadChurn <- ChurnEdges / ExpectedDyadChurn

# Inspect the value
DyadChurn




# Compute the expected heterophilicity
ExpectedHet <- NonChurnNodes * ChurnNodes * connectance

# Compute the heterophilicity
Het <- MixedEdges / ExpectedHet

# Inspect the heterophilicity
Het



# Extract network degree
V(network)$degree <- degree(network, normalized=TRUE)

# Extract 2.order network degree
degree2 <- neighborhood.size(network, 2)

# Normalize 2.order network degree
V(network)$degree2 <- degree2 / (length(V(network)) - 1)


# Extract number of triangles
V(network)$triangles <- count_triangles(network)


# Extract the betweenness
V(network)$betweenness <- betweenness(network, normalized=TRUE)

# Extract the closeness
V(network)$closeness <- closeness(network, normalized=TRUE)

# Extract the eigenvector centrality
V(network)$eigenCentrality <- eigen_centrality(network, scale = TRUE)$vector


# Extract the local transitivity
V(network)$transitivity <- transitivity(network, type="local", isolates='zero')

# Compute the network's transitivity
transitivity(network)





# Extract the adjacency matrix
AdjacencyMatrix <- as_adjacency_matrix(network)

# Compute the second order matrix
SecondOrderMatrix_adj <- AdjacencyMatrix %*% AdjacencyMatrix

# Adjust the second order matrix
SecondOrderMatrix <- ((SecondOrderMatrix_adj) > 0) + 0
diag(SecondOrderMatrix) <- 0

# Inspect the second order matrix
SecondOrderMatrix[1:10, 1:10]





# Compute the number of churn neighbors
V(network)$ChurnNeighbors <- as.vector(AdjacencyMatrix %*% V(network)$Churn)

# Compute the number of non-churn neighbors
V(network)$NonChurnNeighbors <- as.vector(AdjacencyMatrix %*% (1 - V(network)$Churn))

# Compute the relational neighbor probability
V(network)$RelationalNeighbor <- as.vector(V(network)$ChurnNeighbors / 
                                             (V(network)$ChurnNeighbors + V(network)$NonChurnNeighbors))


# Compute the number of churners in the second order neighborhood
V(network)$ChurnNeighbors2 <- as.vector(SecondOrderMatrix %*% V(network)$Churn)

# Compute the number of non-churners in the second order neighborhood
V(network)$NonChurnNeighbors2 <- as.vector(SecondOrderMatrix %*% (1 - V(network)$Churn))

# Compute the relational neighbor probability in the second order neighborhood
V(network)$RelationalNeighbor2 <- as.vector(V(network)$ChurnNeighbors2 / 
                                              (V(network)$ChurnNeighbors2 + V(network)$NonChurnNeighbors2))




# Extract the average degree of neighboring nodes
V(network)$averageDegree <- 
  as.vector(AdjacencyMatrix %*% V(network)$degree) / degree



# Extract the average number of triangles of neighboring nodes
V(network)$averageTriangles <- 
  as.vector(AdjacencyMatrix %*% V(network)$triangles) / degree


# Extract the average transitivity of neighboring nodes    
V(network)$averageTransitivity<-
  as.vector(AdjacencyMatrix %*% V(network)$transitivity) / degree


# Extract the average betweenness of neighboring nodes    
V(network)$averageBetweenness <- 
  as.vector(AdjacencyMatrix %*% V(network)$betweenness) / degree




# Compute one iteration of PageRank 
iter1 <- page.rank(network, algo = 'power', options = list(niter = 1))$vector

# Compute two iterations of PageRank 
iter2 <- page.rank(network, algo = 'power', options = list(niter = 2))$vector

# Inspect the change between one and two iterations
sum(abs(iter1 - iter2))

# Inspect the change between nine and ten iterations
sum(abs(iter9 - iter10))



# Create an empty vector
value <- c()

# Write a loop to compute PageRank 
for(i in 1:15){
  value <- cbind(value, page.rank(network, algo = 'power',options = list(niter = i))$vector)
}

# Compute the differences 
difference <- colSums(abs(value[,1:14] - value[,2:15]))

# Plot the differences
plot(1:14, difference)



# Look at the distribution of standard PageRank scores
boxplots(damping = 0.85)

# Inspect the distribution of personalized PageRank scores
boxplots(damping = 0.85, personalized = TRUE)

# Look at the standard PageRank with damping factor 0.2
boxplots(damping = 0.2)

# Inspect the personalized PageRank scores with a damping factor 0.99
boxplots(damping = 0.99, personalized = TRUE)



# Compute the default PageRank score
V(network)$pr_0.85 <- page.rank(network)$vector


# Compute the PageRank score with damping 0.2
V(network)$pr_0.20 <- page.rank(network, damping = 0.2)$vector



# Compute the personalized PageRank score
V(network)$perspr_0.85 <- page.rank(network, personalized = V(network)$Churn)$vector




# Compute the personalized PageRank score with damping 0.99
V(network)$perspr_0.99 <- page.rank(network, damping = 0.99, personalized = V(network)$Churn)$vector





# Extract the dataset
dataset_full <- as_data_frame(network, what = "vertices")

# Inspect the dataset
head(dataset_full)

# Remove customers who already churned
dataset_filtered <- dataset_full[-which(dataset_full$Churn == 1), ]

# Remove useless columns
dataset <- dataset_filtered[, -c(1, 2)]




# Inspect the feature
summary(dataset$RelationalNeighborSecond)

# Find the indeces of the missing values
toReplace <- which(is.na(dataset$RelationalNeighborSecond))

# Replace the missing values with 0
dataset$RelationalNeighborSecond[toReplace] <- 0

# Inspect the feature again
summary(dataset$RelationalNeighborSecond)



# Print the column names
colnames(dataset)



#####
###Welche Daten werden entfernt, wenn sie korreliert sind? 
### Eine gute Richtlinie sind Correlationen mit mehr als 0.9 im corrplot






# Create toRemove
toRemove <- c(10, 13, 19, 22)


# Remove the columns
dataset <- dataset[, -toRemove]





# Set the seed
set.seed(7)

# Creat the index vector
index_train <- sample(1:nrow(dataset), 2 / 3 * nrow(dataset))

# Make the training set
training_set <- dataset[index_train,]

# Make the test set
test_set <- dataset[-index_train,]




# Make firstModel
firstModel <- glm(Future ~ degree + degree2 + triangles + betweenness + closeness + transitivity, family = "binomial", data = training_set)



# Build the model
secondModel <- glm(Future ~ ChurnNeighbors + RelationalNeighbor + ChurnNeighborsSecond + RelationalNeighborSecond + averageDegree + averageTriangles + averageTransitivity + averageBetweenness, 
                   family = "binomial", data = training_set)



# Build the model
thirdModel <- glm(Future~., family = "binomial", data = training_set)




# Load package
library(randomForest)

# Set seed
set.seed(863)

# Build model
rfModel <- randomForest(as.factor(Future)~. ,data=training_set)

# Plot variable importance
varImpPlot(rfModel)




# Load the package
library(pROC)

# Predict with the first model
firstPredictions <- predict(firstModel, newdata = test_set, type = "response")



# Load the package
library(pROC)

# Predict with the first model
secondPredictions <- predict(secondModel, newdata = test_set, type = "response")


# Load the package
library(pROC)

# Predict with the first model
thirdPredictions <- predict(thirdModel, newdata = test_set, type = "response")


# Load the package
library(pROC)

# Predict with the first model
rfPredictions<- predict(rfModel, newdata = test_set, type= "prob")



# Load the package
library(pROC)

# Predict with the first model
rfPredictions<- predict(rfModel, newdata = test_set, type= "prob")




# Load the package
library(pROC)

# Predict with the first model
rfPredictions<- predict(rfModel, newdata = test_set, type= "prob")