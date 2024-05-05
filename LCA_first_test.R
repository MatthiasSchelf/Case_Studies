# Load the flexmix package
library(flexmix)

# Load in the data
csv_file <- "C:/Users/Matthias Schelfhout/OneDrive/Bureaublad/Ugent gerelateerd - Drive/2de Master/Case_Study_Analysis/dataverse_files/prepared_data.csv"

# Read the CSV file into a data frame
data <- read.csv(csv_file)

# Select columns of interest for LCA
columns_of_interest <- c("mood_pre", "STAI_pre_1_1", "STAI_pre_1_2", "STAI_pre_1_3", "moral_dilemma_dog")

# Extract selected columns
scores <- data[, columns_of_interest]

# Define the breaks for discretizing each continuous variable
breaks_mood <- c(0, 33, 66, 100)    # Breaks for mood_pre: 0-33, 34-66, 67-100
breaks_STAI <- c(0, 2, 3, 4)         # Breaks for STAI variables: 0-2, 3, 4
breaks_moral <- c(0, 3, 6, 9)        # Breaks for moral_dilemma_dog: 0-3, 4-6, 7-9

# Create a function to discretize continuous variables into categorical ones
discretize <- function(x, breaks) {
  cut(x, breaks = breaks, labels = FALSE)
}

# Discretize continuous variables into categorical ones
scores_categorical <- scores
scores_categorical$mood_pre <- discretize(scores$mood_pre, breaks_mood)
scores_categorical$STAI_pre_1_1 <- discretize(scores$STAI_pre_1_1, breaks_STAI)
scores_categorical$STAI_pre_1_2 <- discretize(scores$STAI_pre_1_2, breaks_STAI)
scores_categorical$STAI_pre_1_3 <- discretize(scores$STAI_pre_1_3, breaks_STAI)
scores_categorical$moral_dilemma_dog <- discretize(scores$moral_dilemma_dog, breaks_moral)

# Convert the scores_categorical to data frame
scores_categorical <- as.data.frame(scores_categorical)

# Specify the model
model <- FLXMRmultinom()

# Remove rows with missing values
scores_categorical <- na.omit(scores_categorical)

# Run latent class analysis using flexmix with categorical variables
lca_model <- flexmix(mood_pre + STAI_pre_1_1 + STAI_pre_1_2 + STAI_pre_1_3 + moral_dilemma_dog ~ 1, k = 3, data = scores_categorical, model = model)

# View the summary of the model
summary(lca_model)



