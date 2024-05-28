# Install and load necessary packages
#install.packages("igraph")

# Load the igraph package
library(igraph)

# Specify the path to your CSV file, change this so it fits your pc 
csv_file <- "C:/Users/Matthias Schelfhout/OneDrive/Bureaublad/Ugent gerelateerd - Drive/2de Master/Case_Study_Analysis/dataverse_files/prepared_data.csv"

# Read the CSV file into a data frame
data <- read.csv(csv_file)

# Here again choose whether you want to do the different smaller parts or more general measures 
columns_of_interest <- c("condition","moral_dilemma_dog","moral_dilemma_wallet","moral_dilemma_plane","moral_dilemma_resume","moral_dilemma_kitten","moral_dilemma_trolley","moral_dilemma_control")

#columns_of_interest <- c("condition", "moral_judgment","moral_judgment_disgust","moral_judgment_non_disgust")

# Select only the columns of interest and put them in "scores"
scores <- data[, columns_of_interest]

# Convert the 'condition' column to numeric (e.g., "stress" to 1 and "control" to 0)
scores$condition <- as.numeric(factor(scores$condition, levels = c("control", "stress")))

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

# Adjusted significance level using Bonferroni correction
alpha_adjusted <- 0.05 / num_comparisons

# Test each correlation for significance
results <- sapply(1:nrow(edge_list), function(i) {
  cor_val <- edge_list$weight[i]
  p_val <- cor.test(scores[, edge_list$from[i]], scores[, edge_list$to[i]])$p.value
  c(correlation = cor_val, p_value = p_val)
})

# Combine results into a data frame
results_df <- as.data.frame(t(results))

# Add Bonferroni adjusted p-value
results_df$adjusted_p_value <- p.adjust(results_df$p_value, method = "bonferroni")

# Identify significant correlations
results_df$is_significant <- results_df$adjusted_p_value < alpha_adjusted

# Print all correlations with significance indicated
print(results_df)



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


