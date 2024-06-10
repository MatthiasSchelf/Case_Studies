# Get the data
# Specify the path to your CSV file, change this so it fits your PC 
csv_file <- "C:/Users/Matthias Schelfhout/OneDrive/Bureaublad/Ugent gerelateerd - Drive/2de Master/Case_Study_Analysis/dataverse_files/prepared_data.csv"

# Read the CSV file into a data frame
data <- read.csv(csv_file)

# Filter out rows with "stress" condition, if applicable. Some questionnaires are only answered by the control condition.
# If you want to use a questionnaire that everybody answered, you can leave this out. 
data_filtered <- data[data$condition != "stress", ]

# Select columns of interest for LCA (specific)
columns_of_interest <- c("REI_1","REI_2","REI_3","REI_4","REI_5","REI_6","REI_7","REI_8","REI_9","REI_10",
                         "REI_11","REI_12","REI_13","REI_14","REI_15","REI_16","REI_17","REI_18","REI_19",
                         "REI_20","REI_21","REI_22","REI_23","REI_24","REI_25","REI_26","REI_27","REI_28",
                         "REI_29","REI_30","REI_31","REI_32","REI_33","REI_34","REI_35","REI_36","REI_37",
                         "REI_38","REI_39","REI_40","moral_dilemma_dog","moral_dilemma_wallet","moral_dilemma_plane","moral_dilemma_resume","moral_dilemma_kitten","moral_dilemma_trolley")

# Extract selected columns
scores <- data_filtered[, columns_of_interest]

# Calculate average for REI

# Separate REI columns
rei_columns <- grep("^REI_", colnames(scores), value = TRUE)

# Calculate the average score for each participant for REI columns only
rei_average_scores <- rowMeans(scores[, rei_columns], na.rm = TRUE)

# Add the REI average scores to the filtered data frame
scores$REI_average <- rei_average_scores

# Calculate average for vignettes

# Separate moral columns
moral_columns <- grep("^moral_", colnames(scores), value = TRUE)

# Calculate the average score for each participant for REI columns only
moral_average_scores <- rowMeans(scores[, moral_columns], na.rm = TRUE)

# Add the moral average scores to the filtered data frame
scores$moral_average <- moral_average_scores




# Now lets to the MANOVA/ANOVA, you can pick and choose





# Perform MANOVA to put all moral vignettes separately together with average of REI

dependent_vars_separately <- cbind(scores$moral_dilemma_dog, scores$moral_dilemma_wallet, scores$moral_dilemma_plane,
                        scores$moral_dilemma_resume, scores$moral_dilemma_kitten, scores$moral_dilemma_trolley)
manova_result_separately <- manova(dependent_vars_separately ~ REI_average, data = scores)
summary(manova_result_separately)

# Perform MANOVA for disgust eliciting and non-disgust eliciting

#disgust

dependent_vars_disgust <- cbind(scores$moral_dilemma_dog,  scores$moral_dilemma_plane,
                         scores$moral_dilemma_kitten)
manova_result_disgust <- manova(dependent_vars_disgust ~ REI_average, data = scores)
summary(manova_result_disgust)

#non-disgust

dependent_vars_nondisgust <- cbind(scores$moral_dilemma_wallet, scores$moral_dilemma_resume,
                                   scores$moral_dilemma_trolley)
manova_result_nondisgust <- manova(dependent_vars_nondisgust ~ REI_average, data = scores)
summary(manova_result_nondisgust)

# Perform ANOVA with average of REI and average of vignettes

anova_result_average <- lm(moral_average ~ REI_average, data = scores)
summary(anova_result_average)

# Perform ANOVA for every moral dilemma separately 

# dog

anova_result_dog <- lm(moral_dilemma_dog ~ REI_average, data = scores)
summary(anova_result_dog)

#wallet

anova_result_wallet <- lm(moral_dilemma_wallet ~ REI_average, data = scores)
summary(anova_result_wallet)

#Plane

anova_result_plane <- lm(moral_dilemma_plane ~ REI_average, data = scores)
summary(anova_result_plane)

#Resume

anova_result_resume <- lm(moral_dilemma_resume ~ REI_average, data = scores)
summary(anova_result_resume)

#Kitten

anova_result_kitten <- lm(moral_dilemma_kitten ~ REI_average, data = scores)
summary(anova_result_kitten)

#Trolley

anova_result_trolley <- lm(moral_dilemma_trolley ~ REI_average, data = scores)
summary(anova_result_trolley)





