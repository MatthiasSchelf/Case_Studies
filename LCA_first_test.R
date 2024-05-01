# Specify the path to your CSV file
csv_file <- "C:/Users/Matthias Schelfhout/OneDrive/Bureaublad/Ugent gerelateerd - Drive/2de Master/Case_Study_Analysis/dataverse_files/prepared_data.csv"

# Load necessary packages
# install.packages("poLCA")  # Uncomment if you haven't installed the package yet
library(poLCA)

# Read the CSV file into a data frame
data <- read.csv(csv_file)

# Indicate the columns of interest
columns_of_interest <- c("mood_pre", "STAI_pre_1_1", "STAI_pre_1_2", "STAI_pre_1_3", "moral_dilemma_dog")

# Select only the columns of interest and put them in "scores"
scores <- data[, columns_of_interest]

# Convert categorical columns to factors
scores$mood_pre <- factor(scores$mood_pre)
scores$STAI_pre_1_1 <- factor(scores$STAI_pre_1_1)
scores$STAI_pre_1_2 <- factor(scores$STAI_pre_1_2)
scores$STAI_pre_1_3 <- factor(scores$STAI_pre_1_3)
scores$moral_dilemma_dog <- factor(scores$moral_dilemma_dog)

# Perform Latent Class Analysis
lca_formula <- as.formula(paste(" ~ ", paste(columns_of_interest, collapse = " + ")))
lca_model <- poLCA(lca_formula, data = scores, nclass = 3)

# Summary of the LCA model
summary(lca_model)

# Get the probabilities of class membership
class_probabilities <- predict(lca_model, type = "class")


