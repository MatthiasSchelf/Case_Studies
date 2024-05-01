# Specify the path to your CSV file, change this so it fits your pc 
csv_file <- "C:/Users/Matthias Schelfhout/OneDrive/Bureaublad/Ugent gerelateerd - Drive/2de Master/Case_Study_Analysis/dataverse_files/prepared_data.csv"

# Install packages
# install.packages("poLCA")
library(poLCA)

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

# Perform Latent Class Analysis
lca_model <- poLCA(scores, nclass = 3, maxiter = 100, tol = 1e-6)

# Summary of LCA results
summary(lca_model)

# Predict latent class membership for each individual
class_memberships <- predict(lca_model)$class

# Add the predicted class memberships to your data
data$class <- class_memberships






