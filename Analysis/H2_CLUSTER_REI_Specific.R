### Specific REI Cluster Analyses

install.packages("cluster")
library(cluster)

# Load the data
wd <- "C:/Users/marie/OneDrive/Documenten/MASTERPROEF 2"
setwd(wd)
getwd()
data <- read.csv("prepared_data.csv")

# Select columns of interest for LCA (specific REI items)
columns_of_interest <- c("REI_1","REI_2","REI_3","REI_4","REI_5","REI_6","REI_7","REI_8","REI_9","REI_10",
                         "REI_11","REI_12","REI_13","REI_14","REI_15","REI_16","REI_17","REI_18","REI_19",
                         "REI_20","REI_21","REI_22","REI_23","REI_24","REI_25","REI_26","REI_27","REI_28",
                         "REI_29","REI_30","REI_31","REI_32","REI_33","REI_34","REI_35","REI_36","REI_37",
                         "REI_38","REI_39","REI_40")


table(data$condition)

# Extract selected columns
scores_numeric <- data[, columns_of_interest]

# Remove rows with missing values
scores_numeric <- na.omit(scores_numeric)

scores_numeric <- lapply(scores_numeric, function(x) {
  if (is.factor(x)) {
    as.numeric(as.factor(x))
  } else if (is.character(x)) {
    as.numeric(as.factor(x))
  } else {
    as.numeric(x)
  }
})

# Convert the list back to a data frame
scores_numeric <- as.data.frame(scores_numeric)

# DOuble check
is.numeric(scores_numeric$REI_1)

# Scale the data
scaled_data <- scale(scores_numeric)

# Perform k-means clustering 
kmeans_result <- kmeans(scaled_data, centers = 2)

# Get the cluster assignments
cluster_assignments <- kmeans_result$cluster
cluster_assignments

# Evaluate the results
library(factoextra) # For visualization
fviz_cluster(kmeans_result, data = scaled_data)

#Test for optimal amount of clusters 
fviz_nbclust(scaled_data, kmeans, method = "silhouette")


# Interpret statistics
# Summary statistics by cluster
library(dplyr)
scores_numeric$cluster <- factor(kmeans_result$cluster)
scores_numeric
summary(scores_numeric)
cluster_profiles <- scores_numeric %>% group_by(cluster) %>% summarize(across(everything(), list(mean = mean, sd = sd)))
View(cluster_profiles)

# Compute the mean scores for each REI item within each cluster
cluster_profiles <- scores_numeric %>%
  group_by(cluster) %>%
  summarize(across(starts_with("REI_"), mean, na.rm = TRUE))

# Compute the mean over all means of the REI items for Cluster 1 and Cluster 2
mean_cluster1 <- mean(rowMeans(cluster_profiles[cluster_profiles$cluster == 1, -1]), na.rm = TRUE)
mean_cluster2 <- mean(rowMeans(cluster_profiles[cluster_profiles$cluster == 2, -1]), na.rm = TRUE)

# Print the mean scores for Cluster 1 and Cluster 2
print(mean_cluster1)
print(mean_cluster2)

