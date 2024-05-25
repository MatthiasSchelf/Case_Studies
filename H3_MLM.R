# Load necessary packages
#install.packages("lme4")
#install.packages("nlme")
library(lme4)
library(nlme)

# Specify the path to your CSV file, change this so it fits your pc 
csv_file <- "C:/Users/Matthias Schelfhout/OneDrive/Bureaublad/Ugent gerelateerd - Drive/2de Master/Case_Study_Analysis/dataverse_files/prepared_data.csv"

# Read the CSV file into a data frame
data <- read.csv(csv_file)

# Indicate the general columns of interest
columns_of_interest <- c("moral_judgment", "moral_judgment_disgust", "moral_judgment_non_disgust",
                         "REI_rational_ability", "REI_rational_engagement", "REI_experiental_ability", "REI_experiental_engagement",
                         "PBC", "MAIA_trusting", "MAIA_body_listening", "MAIA_self_regulation", "MAIA_emotional_awareness",
                         "MAIA_attention_regulation", "MAIA_not_worrying", "MAIA_not_distracting", "MAIA_noticing")

#Or Indicate the specific columns of interest
columns_of_interest <- c("moral_judgment","PBC_1","PBC_2","PBC_3","PBC_4","PBC_5","MAIA_1_1","MAIA_1_2","MAIA_1_3","MAIA_1_4","MAIA_1_5","MAIA_1_6","MAIA_1_7","MAIA_1_8","MAIA_1_9","MAIA_1_10","MAIA_1_11","MAIA_1_12","MAIA_1_13","MAIA_1_14","MAIA_1_15","MAIA_2_1",
                          "MAIA_2_1","MAIA_2_2","MAIA_2_3","MAIA_2_4","MAIA_2_5","MAIA_2_6","MAIA_2_7","MAIA_2_8","MAIA_2_9","MAIA_2_10","MAIA_2_11","MAIA_2_12","MAIA_2_13","MAIA_2_14","MAIA_2_15","MAIA_2_16","REI_1","REI_2","REI_3","REI_4","REI_5","REI_6","REI_7","REI_8","REI_9","REI_10","REI_11","REI_12","REI_13","REI_14","REI_15","REI_16","REI_17","REI_18","REI_19","REI_20","REI_21","REI_22","REI_23","REI_24","REI_25","REI_26","REI_27","REI_28","REI_29","REI_30","REI_31","REI_32","REI_33","REI_34","REI_35","REI_36","REI_37","REI_38","REI_39","REI_40" )


# Select only the columns of interest and put them in "scores"
scores <- data[, columns_of_interest]

# General model
# Here we consider REI, MAIA, and PBC as second-level variables and the others as first-level variables

model_formula <- moral_judgment ~  
  (1 | REI_rational_ability) + (1 | REI_rational_engagement) + (1 | REI_experiental_ability) + (1 | REI_experiental_engagement) +
  (1 | PBC) + (1 | MAIA_trusting) + (1 | MAIA_body_listening) + (1 | MAIA_self_regulation) + 
  (1 | MAIA_emotional_awareness) + (1 | MAIA_attention_regulation) + (1 | MAIA_not_worrying) +
  (1 | MAIA_not_distracting) + (1 | MAIA_noticing)

# Specific model (this takes a while to run)

model_formula <- moral_judgment ~ (1 | PBC_1) + (1 | PBC_2) + (1 | PBC_3) + (1 | PBC_4) + (1 | PBC_5) +
  (1 | MAIA_1_1) + (1 | MAIA_1_2) + (1 | MAIA_1_3) + (1 | MAIA_1_4) + (1 | MAIA_1_5) + (1 | MAIA_1_6) + 
  (1 | MAIA_1_7) + (1 | MAIA_1_8) + (1 | MAIA_1_9) + (1 | MAIA_1_10) + (1 | MAIA_1_11) + (1 | MAIA_1_12) + 
  (1 | MAIA_1_13) + (1 | MAIA_1_14) + (1 | MAIA_1_15) + (1 | MAIA_2_1) + (1 | MAIA_2_2) + (1 | MAIA_2_3) + 
  (1 | MAIA_2_4) + (1 | MAIA_2_5) + (1 | MAIA_2_6) + (1 | MAIA_2_7) + (1 | MAIA_2_8) + (1 | MAIA_2_9) + 
  (1 | MAIA_2_10) + (1 | MAIA_2_11) + (1 | MAIA_2_12) + (1 | MAIA_2_13) + (1 | MAIA_2_14) + (1 | MAIA_2_15) + 
  (1 | MAIA_2_16) + (1 | REI_1) + (1 | REI_2) + (1 | REI_3) + (1 | REI_4) + (1 | REI_5) + (1 | REI_6) + 
  (1 | REI_7) + (1 | REI_8) + (1 | REI_9) + (1 | REI_10) + (1 | REI_11) + (1 | REI_12) + (1 | REI_13) + 
  (1 | REI_14) + (1 | REI_15) + (1 | REI_16) + (1 | REI_17) + (1 | REI_18) + (1 | REI_19) + (1 | REI_20) + 
  (1 | REI_21) + (1 | REI_22) + (1 | REI_23) + (1 | REI_24) + (1 | REI_25) + (1 | REI_26) + (1 | REI_27) + 
  (1 | REI_28) + (1 | REI_29) + (1 | REI_30) + (1 | REI_31) + (1 | REI_32) + (1 | REI_33) + (1 | REI_34) + 
  (1 | REI_35) + (1 | REI_36) + (1 | REI_37) + (1 | REI_38) + (1 | REI_39) + (1 | REI_40)



# Fit the model using lmer
model <- lmer(model_formula, data = scores)

# Print summary of the model
summary(model)

# Check model assumptions
plot(model)

