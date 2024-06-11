install.packages("cluster")
library(cluster)

# Load the data
wd <- "C:/Users/marie/OneDrive/Documenten/MASTERPROEF 2"
setwd(wd)
getwd()
data <- read.csv("prepared_data.csv")


# Select columns of interest for LCA (general REI components)
columns_of_interest <- c("REI_rational_ability","REI_rational_engagement","REI_experiental_ability","REI_experiental_engagement");


# Extract selected columns
scores_numeric <- data[, columns_of_interest]

# Remove rows with missing values
scores_numeric <- na.omit(scores_numeric)


# Convert the list back to a data frame
scores_numeric <- as.data.frame(scores_numeric)


# Scale the data
scaled_data <- scale(scores_numeric)

# Perform k-means clustering 
kmeans_result <- kmeans(scaled_data, centers = 5)

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




