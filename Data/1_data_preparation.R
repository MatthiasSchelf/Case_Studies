
# Setup -------------------------------------------------------------------

# Load packages
library(tidyverse)

# Read data
data <- read_csv("data/raw_data.csv")

# Data preparation --------------------------------------------------------

# Select variables of interest
data <- select(
  data, V8, V9, V10, condition, ppnr, gender, age, mood_1,
  moodPost_1, starts_with("stai"), starts_with("D"),
  starts_with("pres"), VervLeuk_1, VervLeuk_2, challenge_1,
  starts_with("pbc"), starts_with("rei"), starts_with("Maia")
)

# Rename variables
data <- rename(data,
  start_date = V8, end_date = V9, finished = V10, subject = ppnr,
  mood_pre = mood_1, mood_post = moodPost_1, moral_dilemma_dog = D1dog,
  moral_dilemma_wallet = D2wallet, moral_dilemma_plane = D3plane,
  moral_dilemma_resume = D4resume, moral_dilemma_kitten = D5kitten,
  moral_dilemma_trolley = D6trolley, moral_dilemma_control = D7control,
  presentation_experience = presBefore, presentation_unpleasant = VervLeuk_1,
  presentation_fun = VervLeuk_2, presentation_challenge = challenge_1
)

names(data) <- str_replace_all(
  names(data), "presEva",
  "presentation_evaluation"
)
names(data) <- str_replace_all(names(data), "stai", "STAI_pre_")
names(data) <- str_replace_all(names(data), "pbc", "PBC")
names(data) <- str_replace_all(names(data), "rei", "REI_")
names(data) <- str_replace_all(names(data), "Maia", "MAIA_")

STAI_post_names <- str_replace(names(data)[str_detect(
  names(data),
  "[123]post"
)], "post", "")
STAI_post_names <- str_replace(STAI_post_names, "pre", "post")
names(data)[str_detect(names(data), "[123]post")] <- STAI_post_names

# The data contains some additional test cases that were not part of the week
# in which the study was run. The study was run from November 3rd to November
# 7th 2014. The following makes sure that only the participants that
# participated in that week are in the dataset.
data <- filter(data, finished == 1)

# Data preparation: STAI --------------------------------------------------

# STAI: Pre measure

# Reverse code some of the items
data <- mutate(data,
  STAI_pre_1_1 = 5 - STAI_pre_1_1, STAI_pre_1_2 = 5 - STAI_pre_1_2,
  STAI_pre_1_5 = 5 - STAI_pre_1_5, STAI_pre_2_1 = 5 - STAI_pre_2_1,
  STAI_pre_2_3 = 5 - STAI_pre_2_3, STAI_pre_2_4 = 5 - STAI_pre_2_4,
  STAI_pre_3_1 = 5 - STAI_pre_3_1, STAI_pre_3_2 = 5 - STAI_pre_3_2,
  STAI_pre_3_5 = 5 - STAI_pre_3_5, STAI_pre_3_6 = 5 - STAI_pre_3_6
)

# Create a sum score
data <- mutate(data, STAI_pre = rowSums(select(data, contains("STAI_pre"))))

# STAI: Post measure

# Reverse code some of the items
data <- mutate(data,
  STAI_post_1_1 = 5 - STAI_post_1_1, STAI_post_1_2 = 5 - STAI_post_1_2,
  STAI_post_1_5 = 5 - STAI_post_1_5, STAI_post_2_1 = 5 - STAI_post_2_1,
  STAI_post_2_3 = 5 - STAI_post_2_3, STAI_post_2_4 = 5 - STAI_post_2_4,
  STAI_post_3_1 = 5 - STAI_post_3_1, STAI_post_3_2 = 5 - STAI_post_3_2,
  STAI_post_3_5 = 5 - STAI_post_3_5, STAI_post_3_6 = 5 - STAI_post_3_6
)

# Create a sum score
data <- mutate(data, STAI_post = rowSums(select(data, contains("STAI_post"))))

# Data preparation: MAIA --------------------------------------------------

# Subtract 1 from all scores, since according to the original publication the
# scores should range from 0 to 5, whilst now they range from 1 to 6.
data[, str_detect(names(data), "MAIA")] <- data[, str_detect(
  names(data),
  "MAIA"
)] - 1

# Reverse score some of the items
data <- mutate(data,
  MAIA_1_5 = 6 - MAIA_1_5, MAIA_1_6 = 6 - MAIA_1_6, MAIA_1_7 = 6 - MAIA_1_7,
  MAIA_1_8 = 6 - MAIA_1_8, MAIA_1_9 = 6 - MAIA_1_9
)

# Create a sumscore for each subscale
data <- mutate(data,
  MAIA_noticing = rowSums(select(data, MAIA_1_1, MAIA_1_2, MAIA_1_3, MAIA_1_4)),
  MAIA_not_distracting = rowSums(select(data, MAIA_1_5, MAIA_1_6, MAIA_1_7)),
  MAIA_not_worrying = rowSums(select(data, MAIA_1_8, MAIA_1_9, MAIA_1_10)),
  MAIA_attention_regulation = rowSums(select(
    data, MAIA_1_11, MAIA_1_12, MAIA_1_13,
    MAIA_1_14, MAIA_1_15, MAIA_1_16,
    MAIA_2_1
  )),
  MAIA_emotional_awareness = rowSums(select(
    data, MAIA_2_2, MAIA_2_3, MAIA_2_4,
    MAIA_2_5, MAIA_2_6
  )),
  MAIA_self_regulation = rowSums(select(
    data, MAIA_2_7, MAIA_2_8, MAIA_2_9,
    MAIA_2_10
  )),
  MAIA_body_listening = rowSums(select(data, MAIA_2_11, MAIA_2_12, MAIA_2_13)),
  MAIA_trusting = rowSums(select(data, MAIA_2_14, MAIA_2_15, MAIA_2_16))
)

# Data preparation: PBC ---------------------------------------------------

# Create sum score
data <- mutate(data, PBC = rowSums(select(data, contains("PBC"))))

# Data preparation: REI ---------------------------------------------------

# Reverse score some items
data <- mutate(data,
  REI_1 = 6 - REI_1, REI_4 = 6 - REI_4, REI_7 = 6 - REI_7,
  REI_11 = 6 - REI_11, REI_14 = 6 - REI_14, REI_15 = 6 - REI_15,
  REI_16 = 6 - REI_16, REI_18 = 6 - REI_18, REI_20 = 6 - REI_20,
  REI_21 = 6 - REI_21, REI_22 = 6 - REI_22, REI_24 = 6 - REI_24,
  REI_26 = 6 - REI_26, REI_28 = 6 - REI_28, REI_29 = 6 - REI_29,
  REI_35 = 6 - REI_35, REI_37 = 6 - REI_37, REI_39 = 6 - REI_39,
  REI_40 = 6 - REI_40
)

# Create sum score
data <- mutate(data,
  REI_rational_ability = rowSums(select(
    data, REI_1, REI_5, REI_11, REI_15, REI_17,
    REI_19, REI_21, REI_32, REI_37, REI_38
  )),
  REI_rational_engagement = rowSums(select(
    data, REI_4, REI_7, REI_12, REI_18, REI_24,
    REI_25, REI_27, REI_30, REI_34, REI_40
  )),
  REI_experiental_ability = rowSums(select(
    data, REI_2, REI_9, REI_10, REI_20, REI_22,
    REI_23, REI_26, REI_28, REI_31, REI_35
  )),
  REI_experiental_engagement = rowSums(select(
    data, REI_3, REI_6, REI_8, REI_13,
    REI_14, REI_16, REI_29, REI_33, REI_36,
    REI_39
  ))
)

# Data preparation: Morality judgments ------------------------------------

# Calculate means
data <- mutate(data,
  moral_judgment = rowMeans(select(data, starts_with("moral_dilemma")),
    na.rm = TRUE
  ),
  moral_judgment_disgust = rowMeans(select(
    data, moral_dilemma_dog,
    moral_dilemma_plane, moral_dilemma_kitten
  ),
  na.rm = TRUE
  ),
  moral_judgment_non_disgust = rowMeans(select(
    data, moral_dilemma_wallet,
    moral_dilemma_resume,
    moral_dilemma_trolley
  ), na.rm = TRUE)
)

# Data preparation: Presentation measures ---------------------------------

# The variables presentation_evaluation_1 to 5 should actually be combined;
# the item was accidentally a checkbox in Qualtrics, potentially allowing more
# than 1 response
data <- mutate(data,
  presentation_evaluation_2 = ifelse(presentation_evaluation_2 == 1, 2, NA),
  presentation_evaluation_3 = ifelse(presentation_evaluation_3 == 1, 3, NA),
  presentation_evaluation_4 = ifelse(presentation_evaluation_4 == 1, 4, NA),
  presentation_evaluation_5 = ifelse(presentation_evaluation_5 == 1, 5, NA)
)

data <- mutate(data,
  presentation_evaluation =
    rowSums(select(data, starts_with("presentation_evaluation")), na.rm = TRUE)
)

data <- mutate(data, presentation_evaluation = ifelse(
  presentation_evaluation == 0, NA, presentation_evaluation
))

# Remove the five original variables
data <- select(
  data, -presentation_evaluation_1, -presentation_evaluation_2,
  -presentation_evaluation_3, -presentation_evaluation_4,
  -presentation_evaluation_5
)

# Data preparation: Misc --------------------------------------------------

# Change some variables into factors
data$condition <- factor(data$condition, label = c("stress", "control"))
data$gender <- factor(data$gender, label = c("male", "female"))

# Make the age variable numeric.
data$age <- as.numeric(data$age)

# Add logbook data --------------------------------------------------------

# Load in logbook data
logbook <- read_csv("data/logbook.csv")

# Rename the ppnr variable
logbook <- rename(logbook, subject = ppnr)

# Merge with data
data <- left_join(data, logbook, by = "subject")

# Save data ---------------------------------------------------------------

# Save the prepapred data
write_csv(data, "data/prepared_data.csv", na = "")

# Save session information
writeLines(
  capture.output(sessionInfo()),
  "scripts/session_info_data_preparation.txt"
)
