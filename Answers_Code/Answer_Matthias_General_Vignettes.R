# Install and load necessary packages
# install.packages("qgraph")
# install.packages("bootnet")
# install.packages("igraph")

library(qgraph)
library(bootnet)
library(igraph)
library(dplyr)

# Specify the path to your CSV file
csv_file <- "C:/Users/Matthias Schelfhout/OneDrive/Bureaublad/Ugent gerelateerd - Drive/2de Master/Case_Study_Analysis/dataverse_files/prepared_data.csv"

# Read the CSV file into a data frame
data <- read.csv(csv_file)

# Select columns of interest
columns_of_interest <- c("condition", "moral_judgment","moral_judgment_disgust","moral_judgment_non_disgust")

# Select only the columns of interest from the data
scores <- data[, columns_of_interest]

# Convert the "condition" column to numeric (0 for control, 1 for stress)
scores$condition <- ifelse(scores$condition == "control", 0, 1)

# Compute correlation matrix (Spearman correlation)
cor_matrix <- cor(scores, method = "spearman")

# Estimate network using EBICglasso with tuning parameter ot indicate how restrained it needs to be.
network <- estimateNetwork(scores, default = "EBICglasso", corMethod = "cor", 
                           corArgs = list(method = "spearman", use = "pairwise.complete.obs"), tuning = 0.25)

# Extract weights matrix from the estimated network
weights_matrix <- getWmat(network)

# Plot the network using qgraph before adjusting weights
node_names <- colnames(scores)
qgraph(weights_matrix, labels = TRUE, nodeNames = node_names, layout = "spring", color = rainbow(length(node_names)))

# Find the minimum weight in the weights matrix
min_weight <- min(weights_matrix)

# Shift the weights to make all of them non-negative
if (min_weight < 0) {
  weights_matrix <- weights_matrix + abs(min_weight)
}

# Convert the adjusted weights matrix to an igraph object
igraph_network <- graph_from_adjacency_matrix(weights_matrix, mode = "undirected", weighted = TRUE, diag = FALSE)

# Check if the graph is connected
is_connected <- is.connected(igraph_network)
cat("Is the graph connected?", is_connected, "\n")

# Community detection using Spinglass algorithm with negative weights handling (only if graph is connected)
if (is_connected) {
  spinglass_results <- cluster_spinglass(igraph_network, implementation = "neg")
  spinglass_membership <- membership(spinglass_results)
  cat("Spinglass Community Membership: ", spinglass_membership, "\n")
} else {
  cat("Graph is not connected, skipping Spinglass community detection.\n")
}

# Community detection using Walktrap algorithm
walktrap_results <- cluster_walktrap(igraph_network)
walktrap_membership <- membership(walktrap_results)
cat("Walktrap Community Membership: ", walktrap_membership, "\n")

# Eigenvalue plot
plot(eigen(cor_matrix)$values, type = "b")
abline(h = 1, col = "red", lty = 3)

# Centrality measures
centrality_measures <- centrality(network)
indegree <- centrality_measures$InDegree
closeness <- centrality_measures$Closeness
betweenness <- centrality_measures$Betweenness

# Plot centrality measures
centralityPlot(network)

# Bootstrap stability analysis
bootstrap_results <- bootnet(network, nboots = 2000)

# Plot bootstrap results
plot(bootstrap_results, labels = FALSE, order = "sample")

# Edge weight difference plot
plot(bootstrap_results, "edge", plot = "difference", onlyNonZero = TRUE, order = "sample")

# Centrality stability plot
plot(bootstrap_results)

# Centrality difference plot
plot(bootstrap_results, "strength", order = "sample", labels = TRUE)

