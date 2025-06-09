# Load necessary libraries
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("tidyverse")
# install.packages("reshape2")

library(ggplot2)
library(dplyr)
library(reshape2)
library(readxl)
library(tidyverse)
library(knitr)
library(writexl)
library(openxlsx)

#GEN 
gentf <- read_excel("Datasets/gentf.xlsx")
gendiff <- read_excel("Datasets/gendiff1 remove table.xlsx")

#Remove row 205
gentf <- gentf %>% 
  slice(-205)  # Removes row 205

#Remove row 52
gendiff <- gendiff %>% 
  slice(-52)  

# Rename duplicated columns in gendiff to avoid conflicts
gendiff <- gendiff %>%
  rename(
    gender.r_diff = gender.r, # Rename gender.r in gendiff
    gen1_diff = gen1 # Rename gen1 in gendiff
  )

# Ensure columns are of the same type (convert to numeric if needed)
gendiff <- gendiff %>%
  mutate(
    gender.r_diff = as.numeric(gender.r_diff), # Convert to numeric
    gen1_diff = as.numeric(gen1_diff) # Convert to numeric
  )

# Create gen_updated column in gendiff based on the logic
gendiff <- gendiff %>%
  mutate(gen_updated = case_when(
    str_starts(genrev, "agree--") ~ gen1_diff, # Update to gen1_diff if genrev starts with "agree--"
    str_starts(genrev, "disagree--") ~ gender.r_diff, # Keep gender.r_diff if genrev starts with "disagree--"
    str_starts(genrev, "agree on second round") ~ gender.r_diff, # Keep gender.r_diff if genrev starts with "agree on second round"
    str_starts(genrev, "ok") ~ gender.r_diff, # Keep gender.r_diff if genrev starts with "ok"
    TRUE ~ gender.r_diff # Default to gender.r_diff for any other cases
  ))

# Create a smaller dataset with only Excerpt.x and gen_updated
gen_updated <- gendiff %>%
  select(Excerpt.x, gen_updated)

# Merge gen_updated back into gentf to create gentf_updated
gentf_updated <- gentf %>%
  left_join(gen_updated, by = "Excerpt.x", relationship = "many-to-many") %>% # Suppress the warning
  mutate(
    gender.r = as.numeric(gender.r), # Ensure gender.r is numeric
    gen_updated = as.numeric(gen_updated) # Ensure gen_updated is numeric
  ) %>%
  mutate(gen_updated = coalesce(gen_updated, gender.r)) # Update gen_updated with gender.r where applicable

# Save the updated dataset with the new gen_updated column
write.xlsx(gentf_updated, "Datasets/gentf_updated.xlsx")

#print("Merging and updating completed. The updated file is saved as 'gentf_updated.xlsx'.")

gentf_updated <- gentf_updated %>%
  mutate(
    genm_updated = ifelse(gen_updated == 1, 1, 0),  # 1 for man, 0 otherwise
    genw_updated = ifelse(gen_updated == 2, 1, 0),  # 1 for woman, 0 otherwise
    genb_updated = ifelse(gen_updated == 3, 1, 0)   # 1 for both/either, 0 otherwise
  )

gentf_updated <- gentf_updated %>%
  mutate(
    genderm.r = ifelse(gender.r == 1, 1, 0),  # 1 for man, 0 otherwise
    genderw.r = ifelse(gender.r == 2, 1, 0),  # 1 for woman, 0 otherwise
    genderb.r = ifelse(gender.r == 3, 1, 0)   # 1 for both/either, 0 otherwise
  )

gentf_updated <- gentf_updated %>%
  mutate(
    genm1 = ifelse(gen1 == 1, 1, 0),  # 1 for man, 0 otherwise
    genw1 = ifelse(gen1 == 2, 1, 0),  # 1 for woman, 0 otherwise
    genb1 = ifelse(gen1 == 3, 1, 0)   # 1 for both/either, 0 otherwise
  )

gentf_updated <- gentf_updated %>%
  mutate(
    genm2 = ifelse(gen2 == 1, 1, 0),  # 1 for man, 0 otherwise
    genw2 = ifelse(gen2 == 2, 1, 0),  # 1 for woman, 0 otherwise
    genb2 = ifelse(gen2 == 3, 1, 0)   # 1 for both/either, 0 otherwise
  )


write.xlsx(gentf_updated, "Datasets/gentf_updated.xlsx")

##Men
# Function to calculate precision, recall, and F1-score
calculate_metrics <- function(true, pred) {
  tp <- sum(true == 1 & pred == 1)  # True Positives
  fp <- sum(true == 0 & pred == 1)  # False Positives
  fn <- sum(true == 1 & pred == 0)  # False Negatives
  
  precision <- ifelse((tp + fp) > 0, tp / (tp + fp), 0)
  recall <- ifelse((tp + fn) > 0, tp / (tp + fn), 0)
  f1_score <- ifelse((precision + recall) > 0, 2 * (precision * recall) / (precision + recall), 0)
  
  return(c(precision, recall, f1_score))
}

# Create a dataframe to store results
results <- tibble(
  Method = c("Human (genderm.r)", "GPT (genm1)", "GPT (genm2)"),
  Precision = c(
    calculate_metrics(gentf_updated$genm_updated, gentf_updated$genderm.r)[1],
    calculate_metrics(gentf_updated$genm_updated, gentf_updated$genm1)[1],
    calculate_metrics(gentf_updated$genm_updated, gentf_updated$genm2)[1]
  ),
  Recall = c(
    calculate_metrics(gentf_updated$genm_updated, gentf_updated$genderm.r)[2],
    calculate_metrics(gentf_updated$genm_updated, gentf_updated$genm1)[2],
    calculate_metrics(gentf_updated$genm_updated, gentf_updated$genm2)[2]
  ),
  F1_Score = c(
    calculate_metrics(gentf_updated$genm_updated, gentf_updated$genderm.r)[3],
    calculate_metrics(gentf_updated$genm_updated, gentf_updated$genm1)[3],
    calculate_metrics(gentf_updated$genm_updated, gentf_updated$genm2)[3]
  )
)

# Print results
print(results)

##Women
# Function to calculate Precision, Recall, and F1 Score
calc_metrics <- function(pred, actual) {
  tp <- sum(pred == 1 & actual == 1)
  fp <- sum(pred == 1 & actual == 0)
  fn <- sum(pred == 0 & actual == 1)
  
  precision <- ifelse((tp + fp) > 0, tp / (tp + fp), 0)
  recall <- ifelse((tp + fn) > 0, tp / (tp + fn), 0)
  f1_score <- ifelse((precision + recall) > 0, 2 * (precision * recall) / (precision + recall), 0)
  
  return(c(precision, recall, f1_score))
}

# Apply the function to each method
results <- tibble(
  Method = c("Human (genderw.r)", "GPT (genw1)", "GPT (genw2)"),
  Precision = c(
    calc_metrics(gentf_updated$genderw.r, gentf_updated$genw_updated)[1],
    calc_metrics(gentf_updated$genw1, gentf_updated$genw_updated)[1],
    calc_metrics(gentf_updated$genw2, gentf_updated$genw_updated)[1]
  ),
  Recall = c(
    calc_metrics(gentf_updated$genderw.r, gentf_updated$genw_updated)[2],
    calc_metrics(gentf_updated$genw1, gentf_updated$genw_updated)[2],
    calc_metrics(gentf_updated$genw2, gentf_updated$genw_updated)[2]
  ),
  F1_Score = c(
    calc_metrics(gentf_updated$genderw.r, gentf_updated$genw_updated)[3],
    calc_metrics(gentf_updated$genw1, gentf_updated$genw_updated)[3],
    calc_metrics(gentf_updated$genw2, gentf_updated$genw_updated)[3]
  )
)

# Print results
print(results)

##Both/Either
library(dplyr)

# Function to calculate precision, recall, and F1-score
calculate_metrics <- function(predicted, actual) {
  true_positive <- sum(predicted == 1 & actual == 1)
  false_positive <- sum(predicted == 1 & actual == 0)
  false_negative <- sum(predicted == 0 & actual == 1)
  
  precision <- ifelse((true_positive + false_positive) > 0, true_positive / (true_positive + false_positive), 0)
  recall <- ifelse((true_positive + false_negative) > 0, true_positive / (true_positive + false_negative), 0)
  f1_score <- ifelse((precision + recall) > 0, 2 * (precision * recall) / (precision + recall), 0)
  
  return(c(precision, recall, f1_score))
}

# Compute metrics for each method
results <- tibble(
  Method = c("Human (genderb.r)", "GPT (genb1)", "GPT (genb2)"),
  Precision = c(
    calculate_metrics(gentf_updated$genderb.r, gentf_updated$genb_updated)[1],
    calculate_metrics(gentf_updated$genb1, gentf_updated$genb_updated)[1],
    calculate_metrics(gentf_updated$genb2, gentf_updated$genb_updated)[1]
  ),
  Recall = c(
    calculate_metrics(gentf_updated$genderb.r, gentf_updated$genb_updated)[2],
    calculate_metrics(gentf_updated$genb1, gentf_updated$genb_updated)[2],
    calculate_metrics(gentf_updated$genb2, gentf_updated$genb_updated)[2]
  ),
  F1_Score = c(
    calculate_metrics(gentf_updated$genderb.r, gentf_updated$genb_updated)[3],
    calculate_metrics(gentf_updated$genb1, gentf_updated$genb_updated)[3],
    calculate_metrics(gentf_updated$genb2, gentf_updated$genb_updated)[3]
  )
)

# Print results
print(results)

