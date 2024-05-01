# Load necessary libraries
library(stats)

# Specify the path to your CSV file
csv_file <- "C:/Users/Matthias Schelfhout/OneDrive/Bureaublad/Ugent gerelateerd - Drive/2de Master/Case_Study_Analysis/dataverse_files/prepared_data.csv"

# Read the CSV file into a data frame
data <- read.csv(csv_file)

# Filter out rows with "stress" condition, if applicable. Some questionnaires are only answered by the control condition.
# If you want to use a questionnaire that everybody answered, you can leave this out. 
data <- data[data$condition != "stress", ]

# Indicate the columns of interest
columns_of_interest <- c("PBC_1","PBC_2","PBC_3","PBC_4","PBC_5","REI_1","REI_2","REI_3","REI_4","REI_5","REI_6","REI_7","REI_8","REI_9","REI_10","REI_11","REI_12","REI_13","REI_14","REI_15","REI_16","REI_17","REI_18","REI_19","REI_20","REI_21","REI_22","REI_23","REI_24","REI_25","REI_26","REI_27","REI_28","REI_29","REI_30","REI_31","REI_32","REI_33","REI_34","REI_35","REI_36","REI_37","REI_38","REI_39","REI_40")

# Select only the columns of interest and put them in "scores"
scores <- data[, columns_of_interest]

# Remove rows with NA values
scores <- scores[complete.cases(scores), ]

# Check the structure of your data to ensure it's loaded correctly
str(scores)

# Standardize the data (optional but recommended for k-means)
scaled_scores <- scale(scores)

# Perform the elbow method to determine the optimal number of clusters
wcss <- vector()  # Initialize vector to store within-cluster sum of squares (WCSS) values
max_clusters <- 10  # Define the maximum number of clusters you want to try

# Perform k-means clustering for different numbers of clusters
for (i in 1:max_clusters) {
  kmeans_result <- kmeans(scaled_scores, centers = i)
  wcss[i] <- kmeans_result$tot.withinss
}

# Plot the elbow method
plot(1:max_clusters, wcss, type = "b", xlab = "Number of Clusters", ylab = "Within-Cluster Sum of Squares (WCSS)", main = "Elbow Method")

################################

# Run the code until here and identify the "elbow" point and determine the optimal number of clusters
# Based on the plot, select the number of clusters that corresponds to the "elbow" point

# Perform k-means clustering with the selected number of clusters
optimal_num_clusters <- 6  # Example: If the "elbow" point suggests 3 clusters
kmeans_result <- kmeans(scaled_scores, centers = optimal_num_clusters)

# Print the cluster centers
print(kmeans_result$centers)

# Print the cluster assignments for each data point
print(kmeans_result$cluster)

# Plot the clusters 
plot(scaled_scores, col = kmeans_result$cluster)
