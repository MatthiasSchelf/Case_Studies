#Install packages for Network Analysis 
install.packages("network")

# Specify the path to your CSV file, change this so it fits your pc 
csv_file <- "C:/Users/Matthias Schelfhout/OneDrive/Bureaublad/Ugent gerelateerd - Drive/2de Master/Case_Study_Analysis/dataverse_files/prepared_data.csv"

# Read the CSV file into a data frame
data <- read.csv(csv_file)

# Load required packages
library(network)

# Filter out rows with "stress" condition, if applicable. Some questionnaires are only answered by the control condition.
# If you want to use a questionnaire that everybody answered, you can leave this out. 
data <- data[data$condition != "stress", ]

#Indicate the columns of interest, you can change this to play around.
columns_of_interest <- c("moral_dilemma_dog","moral_dilemma_wallet","moral_dilemma_plane","moral_dilemma_resume","moral_dilemma_kitten","moral_dilemma_trolley","moral_dilemma_control")

# Select only the columns of interest and put them in "scores"
scores <- data[, columns_of_interest]

# Calculate pairwise correlations between the selected columns
cor_matrix <- cor(scores)

# Threshold for correlation strength, you can change this if you want. 
threshold <- 0.2

# Find indices of correlations above the threshold
edges <- which(cor_matrix > threshold, arr.ind = TRUE)

# Create a network object from the edge list
net <- network(edges)

# Plot the network with node labels
plot(net, label = colnames(scores))



