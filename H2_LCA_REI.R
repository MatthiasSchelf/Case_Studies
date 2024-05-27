# Install and load the mclust package
if(!require(mclust)) install.packages("mclust", dependencies=TRUE)
library(mclust)

# Load the data
csv_file <- "C:/Users/Matthias Schelfhout/OneDrive/Bureaublad/Ugent gerelateerd - Drive/2de Master/Case_Study_Analysis/dataverse_files/prepared_data.csv"
data <- read.csv(csv_file)

# Select columns of interest for LCA (REI variables)
columns_of_interest <- c("REI_1","REI_2","REI_3","REI_4","REI_5","REI_6","REI_7","REI_8","REI_9","REI_10",
                         "REI_11","REI_12","REI_13","REI_14","REI_15","REI_16","REI_17","REI_18","REI_19",
                         "REI_20","REI_21","REI_22","REI_23","REI_24","REI_25","REI_26","REI_27","REI_28",
                         "REI_29","REI_30","REI_31","REI_32","REI_33","REI_34","REI_35","REI_36","REI_37",
                         "REI_38","REI_39","REI_40")

# Extract selected columns
scores <- data[, columns_of_interest]

# Ensure that REI variables are treated as categorical
scores_categorical <- as.data.frame(lapply(scores, as.factor))

# Remove rows with missing values
scores_categorical <- na.omit(scores_categorical)

# Convert categorical data to numeric for mclust
scores_numeric <- as.data.frame(lapply(scores_categorical, as.numeric))

# Fit the LCA model with mclust
# The number of clusters G = 2 (experiental and rational)
lca_model <- Mclust(scores_numeric, G = 2)

# View the summary of the model
summary(lca_model)

# View the classification results
lca_model$classification

# View the parameters of the model
lca_model$parameters


