# Load necessary packages
#install.packages("lme4")
#install.packages("nlme")

library(lme4)
library(nlme)

# Specify the path to your CSV file, change this so it fits your pc 
csv_file <- "C:/Users/Matthias Schelfhout/OneDrive/Bureaublad/Ugent gerelateerd - Drive/2de Master/Case_Study_Analysis/dataverse_files/prepared_data.csv"

# Read the CSV file into a data frame
data <- read.csv(csv_file)

# Filter out rows with "stress" condition, if applicable. Some questionnaires are only answered by the control condition.
# If you want to use a questionnaire that everybody answered, you can leave this out. 
#data <- data[data$condition != "stress", ]

# Indicate the columns of interest
columns_of_interest <- c("condition","mood_pre","mood_post","STAI_pre_1_1","STAI_pre_1_2","STAI_pre_1_3","moral_dilemma_dog")
# Select only the columns of interest and put them in "scores"
scores <- data[, columns_of_interest]

# Specify the model formula
model_formula <- moral_dilemma_dog ~ STAI_pre_1_1 + STAI_pre_1_2 + (1 | mood_pre)

# Fit the model using lmer
model <- lmer(model_formula, data = scores)

# Print summary of the model
summary(model)

# Check model assumptions
plot(model)
