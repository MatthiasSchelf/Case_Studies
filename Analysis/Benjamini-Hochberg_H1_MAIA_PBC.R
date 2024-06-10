# Install and load necessary packages
# install.packages("igraph")

# Load the igraph package
library(igraph)

# Load the data
csv_file <- "C:/Users/carov/Documents/1 SCHOOL/1. UGent/1ste master/Year 2024/Anaysis/dataverse_files/prepared_data.csv"
data <- read.csv(csv_file)

# Filter out rows with "stress" condition, if applicable. Some questionnaires are only answered by the control condition.
# If you want to use a questionnaire that everybody answered, you can leave this out. 
data_filtered <- data[data$condition != "stress", ]

# Run one of the two following lines, whether you are interested in the general or specific condition.

# Here we put in the separate PBC and MAIA variable, specific condition
columns_of_interest <- c("PBC_1","PBC_2","PBC_3","PBC_4","PBC_5","MAIA_1_1","MAIA_1_2","MAIA_1_3","MAIA_1_4","MAIA_1_5","MAIA_1_6","MAIA_1_7","MAIA_1_8","MAIA_1_9","MAIA_1_10","MAIA_1_11","MAIA_1_12","MAIA_1_13","MAIA_1_14","MAIA_1_15","MAIA_2_1",
                         "MAIA_2_1","MAIA_2_2","MAIA_2_3","MAIA_2_4","MAIA_2_5","MAIA_2_6","MAIA_2_7","MAIA_2_8","MAIA_2_9","MAIA_2_10","MAIA_2_11","MAIA_2_12","MAIA_2_13","MAIA_2_14","MAIA_2_15","MAIA_2_16")

# Here we put the combined PBC and combined MAIA measures, general condition
# columns_of_interest <- c("PBC", "MAIA_trusting","MAIA_body_listening", "MAIA_self_regulation","MAIA_emotional_awareness","MAIA_attention_regulation","MAIA_not_worrying","MAIA_not_distracting","MAIA_noticing")

# Select only the columns of interest and put them in "scores"
scores <- data_filtered[, columns_of_interest]

# Calculate pairwise correlations between the selected columns
cor_matrix <- cor(scores)

# Create edge list including correlation values
edge_list <- data.frame(
  from = character(),
  to = character(),
  weight = numeric()
)

for (i in 1:(ncol(scores)-1)) {
  for (j in (i+1):ncol(scores)) {
    edge_list <- rbind(edge_list, data.frame(from = colnames(scores)[i],
                                             to = colnames(scores)[j],
                                             weight = cor_matrix[i, j]))
  }
}


# Remove self-loops
edge_list <- edge_list[edge_list$from != edge_list$to, ]

# Apply threshold to filter edges
threshold <- 0 # Edit this to only see higher correlations
edge_list <- edge_list[abs(edge_list$weight) >= threshold, ]

# Create an igraph object from the filtered edge list
g <- graph_from_data_frame(edge_list, directed = FALSE)

# Create a circular layout and scale it
layout <- layout_in_circle(g)
scale_factor <- 5  # Adjust this factor to make the circle larger
layout <- layout * scale_factor

# Color edges based on correlation sign
igraph::E(g)$color <- ifelse(igraph::E(g)$weight > 0, "green", "red")

# Set edge width based on correlation magnitude
igraph::E(g)$width <- abs(igraph::E(g)$weight) * 5  # You can adjust the scaling factor

# Add edge labels for correlation values
igraph::E(g)$label <- round(igraph::E(g)$weight, 2)

# Set vertex colors
igraph::V(g)$color <- "skyblue"

# Set vertex size
igraph::V(g)$size <- 10

# Set vertex label size
igraph::V(g)$label.cex <- 0.9

# Plot the network with a larger plotting area
par(mar = rep(1, 4))  # Adjust the margin size if necessary
plot(g, 
     layout = layout,
     vertex.label = igraph::V(g)$name, 
     edge.color = igraph::E(g)$color, 
     edge.width = igraph::E(g)$width, 
     vertex.color = igraph::V(g)$color, 
     vertex.size = igraph::V(g)$size, 
     vertex.label.cex = igraph::V(g)$label.cex,
     edge.label = igraph::E(g)$label,  # Add edge labels
     edge.label.cex = 0.8,  # Adjust the size of edge labels
     rescale = FALSE,  # Prevent automatic rescaling
     xlim = range(layout[, 1]),  # Set x-axis limits
     ylim = range(layout[, 2]))  # Set y-axis limits



# Here we check for significance.

# Number of pairwise correlations
num_comparisons <- nrow(edge_list)

# Test each correlation for significance
results <- sapply(1:nrow(edge_list), function(i) {
  cor_val <- edge_list$weight[i]
  p_val <- cor.test(scores[, edge_list$from[i]], scores[, edge_list$to[i]])$p.value
  c(correlation = cor_val, p_value = p_val)
})

# Combine results into a data frame
results_df <- as.data.frame(t(results))

# Add Benjamini-Hochberg adjusted p-value
results_df$adjusted_p_value <- p.adjust(results_df$p_value, method = "BH")

# Identify significant correlations
alpha <- 0.05  # significance level
results_df$is_significant <- results_df$adjusted_p_value < alpha

# Print all correlations with significance indicated
print(results_df)



# Here we will filter out the non-significant correlations and plot only the significant correlations.

# Filter out non-significant correlations
significant_edge_list <- edge_list[results_df$is_significant, ]

# Create an igraph object from the filtered significant edge list
g_significant <- graph_from_data_frame(significant_edge_list, directed = FALSE)

# Create a circular layout for the significant network
layout_significant <- layout_in_circle(g_significant)

# Scale the layout
scale_factor <- 5  # Adjust this factor to make the circle larger
layout_significant <- layout_significant * scale_factor

# Color edges based on correlation sign
igraph::E(g_significant)$color <- ifelse(igraph::E(g_significant)$weight > 0, "green", "red")

# Set edge width based on correlation magnitude
igraph::E(g_significant)$width <- abs(igraph::E(g_significant)$weight) * 5  # You can adjust the scaling factor

# Add edge labels for correlation values
igraph::E(g_significant)$label <- round(igraph::E(g_significant)$weight, 2)

# Plot the significant network with a larger plotting area
par(mar = rep(1, 4))  # Adjust the margin size if necessary
plot(g_significant, 
     layout = layout_significant,
     vertex.label = igraph::V(g_significant)$name, 
     edge.color = igraph::E(g_significant)$color, 
     edge.width = igraph::E(g_significant)$width, 
     vertex.color = igraph::V(g_significant)$color, 
     vertex.size = igraph::V(g_significant)$size, 
     vertex.label.cex = igraph::V(g_significant)$label.cex,
     edge.label = igraph::E(g_significant)$label,  # Add edge labels
     edge.label.cex = 0.8,  # Adjust the size of edge labels
     rescale = FALSE,  # Prevent automatic rescaling
     xlim = range(layout_significant[, 1]),  # Set x-axis limits
     ylim = range(layout_significant[, 2]))  # Set y-axis limits

