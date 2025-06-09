# Load necessary libraries
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("tidyverse")
# install.packages("reshape2")
# install.packages("openxlsx")

library(ggplot2)
library(dplyr)
library(reshape2)
library(readxl)
library(tidyverse)
library(knitr)
library(openxlsx)


#LEAD
##leadup was merged in chatGPT
lead <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/lead2irrx2.xlsx")
leaddiff1 <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/Leveraging the Power of LLMs copy/LLM paper/Diffs Fasting/leaddiff1 remove table.xlsx")
leadup <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/Leveraging the Power of LLMs copy/LLM paper/Diffs Fasting/lead_updated.xlsx")

#Remove row 205
leadup <- leadup %>% 
  slice(-205)  # Removes row 205

# Step 1: Create classification variables for lead1, lead2, and leader_updated vs leader.r
leadup <- leadup %>%
  mutate(
    classification_lead1_updated = case_when(
      lead1 == 1 & leader_updated == 1 ~ "True Positive",
      lead1 == 0 & leader_updated == 0 ~ "True Negative",
      lead1 == 1 & leader_updated == 0 ~ "False Positive",
      lead1 == 0 & leader_updated == 1 ~ "False Negative"
    ),
    classification_lead2_updated = case_when(
      lead2 == 1 & leader_updated == 1 ~ "True Positive",
      lead2 == 0 & leader_updated == 0 ~ "True Negative",
      lead2 == 1 & leader_updated == 0 ~ "False Positive",
      lead2 == 0 & leader_updated == 1 ~ "False Negative"
    ),
    classification_leader_update = case_when(
      leader_updated == 1 & leader.r == 1 ~ "True Positive",
      leader_updated == 0 & leader.r == 0 ~ "True Negative",
      leader_updated == 1 & leader.r == 0 ~ "False Positive",
      leader_updated == 0 & leader.r == 1 ~ "False Negative"
    )
  )

# Step 2: Count occurrences and calculate percentages
leadupclassification_percentages <- leadup %>%
  select(classification_lead1_updated, classification_lead2_updated, classification_leader_update) %>%
  pivot_longer(cols = everything(), names_to = "Comparison_Type", values_to = "Classification") %>%
  group_by(Comparison_Type, Classification) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percent = round(Count / sum(Count) * 100, 2))  # Convert to percentage

# Step 3: Print table
print(leadupclassification_percentages)

# Step 4: Format as a cleaner table using knitr
leadupclassification_percentages %>%
  kable()


# Step 1: Compute precision for lead1, lead2, and leader.r using leader_updated as the reference
precision_scores <- leadup %>%
  summarise(
    precision_lead1 = sum(lead1 == 1 & leader_updated == 1) / sum((lead1 == 1 & leader_updated == 1) | (lead1 == 1 & leader_updated == 0)),
    precision_lead2 = sum(lead2 == 1 & leader_updated == 1) / sum((lead2 == 1 & leader_updated == 1) | (lead2 == 1 & leader_updated == 0)),
    precision_leader_r = sum(leader.r == 1 & leader_updated == 1) / sum((leader.r == 1 & leader_updated == 1) | (leader.r == 1 & leader_updated == 0))
  )

# Step 2: Print precision scores
print(precision_scores)

#F1

# Step 1: Compute precision, recall, and F1-score for lead1, lead2, and leader.r
f1_scores <- leadup %>%
  summarise(
    # Precision = TP / (TP + FP)
    precision_lead1 = sum(lead1 == 1 & leader_updated == 1) / sum((lead1 == 1 & leader_updated == 1) | (lead1 == 1 & leader_updated == 0)),
    precision_lead2 = sum(lead2 == 1 & leader_updated == 1) / sum((lead2 == 1 & leader_updated == 1) | (lead2 == 1 & leader_updated == 0)),
    precision_leader_r = sum(leader.r == 1 & leader_updated == 1) / sum((leader.r == 1 & leader_updated == 1) | (leader.r == 1 & leader_updated == 0)),
    
    # Recall = TP / (TP + FN)
    recall_lead1 = sum(lead1 == 1 & leader_updated == 1) / sum((lead1 == 1 & leader_updated == 1) | (lead1 == 0 & leader_updated == 1)),
    recall_lead2 = sum(lead2 == 1 & leader_updated == 1) / sum((lead2 == 1 & leader_updated == 1) | (lead2 == 0 & leader_updated == 1)),
    recall_leader_r = sum(leader.r == 1 & leader_updated == 1) / sum((leader.r == 1 & leader_updated == 1) | (leader.r == 0 & leader_updated == 1)),
    
    # F1-score = 2 * (Precision * Recall) / (Precision + Recall)
    f1_lead1 = 2 * (precision_lead1 * recall_lead1) / (precision_lead1 + recall_lead1),
    f1_lead2 = 2 * (precision_lead2 * recall_lead2) / (precision_lead2 + recall_lead2),
    f1_leader_r = 2 * (precision_leader_r * recall_leader_r) / (precision_leader_r + recall_leader_r)
  )

# Step 2: Print precision, recall, and F1-score
print(f1_scores)
f1_scores %>% select(f1_leader_r)


##LAP
laptf <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/laptf.xlsx")
lapdiff <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/Leveraging the Power of LLMs copy/LLM paper/Diffs Fasting/lapdiff1 remove table.xlsx")

#Remove row 205
laptf <- laptf %>% 
  slice(-205)  # Removes row 205

#Remove row duplicate row 33
lapdiff <- lapdiff %>% 
  slice(-33)  

# Rename duplicated columns in lapdiff to avoid conflicts
lapdiff <- lapdiff %>%
  rename(
    time_lapse.r_diff = time_lapse.r, # Rename time_lapse.r in lapdiff
    lap1_diff = lap1 # Rename lap1 in lapdiff
  )

# Ensure columns are of the same type (convert to numeric)
lapdiff <- lapdiff %>%
  mutate(
    time_lapse.r_diff = as.numeric(time_lapse.r_diff), # Convert to numeric
    lap1_diff = as.numeric(lap1_diff) # Convert to numeric
  )

# Create lap_updated column in lapdiff based on the logic
lapdiff <- lapdiff %>%
  mutate(lap_updated = case_when(
    str_starts(laprev, "agree--") ~ lap1_diff, # Update to lap1_diff if laprev starts with "agree--"
    str_starts(laprev, "disagree--") ~ time_lapse.r_diff, # Keep time_lapse.r_diff if laprev starts with "disagree--"
    str_starts(laprev, "agree on second round") ~ time_lapse.r_diff, # Keep time_lapse.r_diff if laprev starts with "agree on second round"
    TRUE ~ time_lapse.r_diff # Default to time_lapse.r_diff for any other cases
  ))

# Create time_lapse_updated with only Excerpt.x and lap_updated
time_lapse_updated <- lapdiff %>%
  select(Excerpt.x, lap_updated)

# Merge time_lapse_updated back into laptf to create laptf_updated
laptf_updated <- laptf %>%
  left_join(time_lapse_updated, by = "Excerpt.x", relationship = "many-to-many") %>% # Suppress the warning
  mutate(
    time_lapse.r = as.numeric(time_lapse.r), # Ensure time_lapse.r is numeric
    lap_updated = as.numeric(lap_updated) # Ensure lap_updated is numeric
  ) %>%
  mutate(lap_updated = coalesce(lap_updated, time_lapse.r)) # Update lap_updated with time_lapse.r where applicable

# Save the updated dataset with the new lap_updated column
write.xlsx(laptf_updated, "/Users/kristensyme/Desktop/R Projects/Fasting/Leveraging the Power of LLMs copy/LLM paper/Diffs Fasting/laptf_updated.xlsx")

#print("Merging and updating completed. The updated file is saved as 'laptf_updated.xlsx'.")

# Step 1: Create classification variables for time_lapse.r, lap1, and lap2
laptf_updated <- laptf_updated %>%
  mutate(
    classification_time_lapse = case_when(
      time_lapse.r == 1 & lap_updated == 1 ~ "True Positive",
      time_lapse.r == 0 & lap_updated == 0 ~ "True Negative",
      time_lapse.r == 1 & lap_updated == 0 ~ "False Positive",
      time_lapse.r == 0 & lap_updated == 1 ~ "False Negative"
    ),
    classification_lap1 = case_when(
      lap1 == 1 & lap_updated == 1 ~ "True Positive",
      lap1 == 0 & lap_updated == 0 ~ "True Negative",
      lap1 == 1 & lap_updated == 0 ~ "False Positive",
      lap1 == 0 & lap_updated == 1 ~ "False Negative"
    ),
    classification_lap2 = case_when(
      lap2 == 1 & lap_updated == 1 ~ "True Positive",
      lap2 == 0 & lap_updated == 0 ~ "True Negative",
      lap2 == 1 & lap_updated == 0 ~ "False Positive",
      lap2 == 0 & lap_updated == 1 ~ "False Negative"
    )
  )

# Step 2: Count occurrences and calculate percentages
lap_classification_percentages <- laptf_updated %>%
  select(classification_time_lapse, classification_lap1, classification_lap2) %>%
  pivot_longer(cols = everything(), names_to = "Comparison", values_to = "Classification") %>%
  group_by(Comparison, Classification) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percent = round(Count / sum(Count) * 100, 2))  # Convert to percentage

# Step 3: Print table
print(lap_classification_percentages)

# Step 4: Format as a cleaner table using knitr
lap_classification_percentages %>%
  kable()

# Step 1: Compute True Positives, False Positives, False Negatives for each comparison
metrics <- laptf_updated %>%
  summarise(
    TP_time_lapse = sum(time_lapse.r == 1 & lap_updated == 1),
    FP_time_lapse = sum(time_lapse.r == 1 & lap_updated == 0),
    FN_time_lapse = sum(time_lapse.r == 0 & lap_updated == 1),
    
    TP_lap1 = sum(lap1 == 1 & lap_updated == 1),
    FP_lap1 = sum(lap1 == 1 & lap_updated == 0),
    FN_lap1 = sum(lap1 == 0 & lap_updated == 1),
    
    TP_lap2 = sum(lap2 == 1 & lap_updated == 1),
    FP_lap2 = sum(lap2 == 1 & lap_updated == 0),
    FN_lap2 = sum(lap2 == 0 & lap_updated == 1)
  )

# Step 2: Calculate Precision, Recall, and F1-score
precision_recall_f1 <- metrics %>%
  mutate(
    precision_time_lapse = TP_time_lapse / (TP_time_lapse + FP_time_lapse),
    recall_time_lapse = TP_time_lapse / (TP_time_lapse + FN_time_lapse),
    f1_time_lapse = 2 * (precision_time_lapse * recall_time_lapse) / (precision_time_lapse + recall_time_lapse),
    
    precision_lap1 = TP_lap1 / (TP_lap1 + FP_lap1),
    recall_lap1 = TP_lap1 / (TP_lap1 + FN_lap1),
    f1_lap1 = 2 * (precision_lap1 * recall_lap1) / (precision_lap1 + recall_lap1),
    
    precision_lap2 = TP_lap2 / (TP_lap2 + FP_lap2),
    recall_lap2 = TP_lap2 / (TP_lap2 + FN_lap2),
    f1_lap2 = 2 * (precision_lap2 * recall_lap2) / (precision_lap2 + recall_lap2)
  ) %>%
  select(starts_with("precision"), starts_with("recall"), starts_with("f1"))

# Step 3: Print results
print(precision_recall_f1)


#ASC
asctf <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/asctf.xlsx")
ascdiff <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/Leveraging the Power of LLMs copy/LLM paper/Diffs Fasting/ascdiff2 remove table.xlsx")

#Remove row 205
asctf <- asctf %>% 
  slice(-205)  # Removes row 205

# Rename duplicated columns in ascdiff to avoid conflicts
ascdiff <- ascdiff %>%
  rename(
    asc_diff = asc, # Rename asc in ascdiff
    asc2_diff = asc2 # Rename asc2 in ascdiff
  )

# Ensure columns are of the same type (convert to numeric if needed)
ascdiff <- ascdiff %>%
  mutate(
    asc_diff = as.numeric(asc_diff), # Convert to numeric
    asc2_diff = as.numeric(asc2_diff) # Convert to numeric
  )

# Create asc_updated column in ascdiff based on the logic
ascdiff <- ascdiff %>%
  mutate(asc_updated = case_when(
    str_starts(ascrev, "agree--") ~ asc2_diff, # Update to asc2_diff if ascrev starts with "agree--"
    str_starts(ascrev, "disagree--") ~ asc_diff, # Keep asc_diff if ascrev starts with "disagree--"
    str_starts(ascrev, "agree on second round") ~ asc_diff, # Keep asc_diff if ascrev starts with "agree on second round"
    TRUE ~ asc_diff # Default to asc_diff for any other cases
  ))

# Create a smaller dataset with only Excerpt.x and asc_updated
asc_updated <- ascdiff %>%
  select(Excerpt.x, asc_updated)

# Merge asc_updated back into asctf to create asctf_updated
asctf_updated <- asctf %>%
  left_join(asc_updated, by = "Excerpt.x", relationship = "many-to-many") %>% # Suppress the warning
  mutate(
    asc = as.numeric(asc), # Ensure asc is numeric
    asc_updated = as.numeric(asc_updated) # Ensure asc_updated is numeric
  ) %>%
  mutate(asc_updated = coalesce(asc_updated, asc)) # Update asc_updated with asc where applicable

# Save the updated dataset with the new asc_updated column
write.xlsx(asctf_updated, "/Users/kristensyme/Desktop/R Projects/Fasting/Leveraging the Power of LLMs copy/LLM paper/Diffs Fasting/asctf_updated.xlsx")

#print("Merging and updating completed. The updated file is saved as 'asctf_updated.xlsx'.")


# Function to calculate precision, recall, and F1-score
calculate_metrics <- function(predicted, actual) {
  tp <- sum(predicted == 1 & actual == 1)  # True Positives
  fp <- sum(predicted == 1 & actual == 0)  # False Positives
  fn <- sum(predicted == 0 & actual == 1)  # False Negatives
  
  precision <- ifelse((tp + fp) > 0, tp / (tp + fp), 0)
  recall <- ifelse((tp + fn) > 0, tp / (tp + fn), 0)
  f1 <- ifelse((precision + recall) > 0, 2 * (precision * recall) / (precision + recall), 0)
  
  return(c(precision, recall, f1))
}

# Apply function to asc, asc2, and asc3
metrics <- data.frame(
  Method = c("Human (asc)", "GPT (asc2)", "GPT (asc3)"),
  Precision = c(
    calculate_metrics(asctf_updated$asc, asctf_updated$asc_updated)[1],
    calculate_metrics(asctf_updated$asc2, asctf_updated$asc_updated)[1],
    calculate_metrics(asctf_updated$asc3, asctf_updated$asc_updated)[1]
  ),
  Recall = c(
    calculate_metrics(asctf_updated$asc, asctf_updated$asc_updated)[2],
    calculate_metrics(asctf_updated$asc2, asctf_updated$asc_updated)[2],
    calculate_metrics(asctf_updated$asc3, asctf_updated$asc_updated)[2]
  ),
  F1_Score = c(
    calculate_metrics(asctf_updated$asc, asctf_updated$asc_updated)[3],
    calculate_metrics(asctf_updated$asc2, asctf_updated$asc_updated)[3],
    calculate_metrics(asctf_updated$asc3, asctf_updated$asc_updated)[3]
  )
)

# Print the metrics
print(metrics)

#VK
vktf <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/vktf.xlsx")
vkdiff <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/Leveraging the Power of LLMs copy/LLM paper/Diffs Fasting/vkdiff1 remove table.xlsx")

#Remove row 205
vktf <- vktf %>% 
  slice(-205)  # Removes row 205

# Rename duplicated columns in vkdiff to avoid conflicts
vkdiff <- vkdiff %>%
  rename(
    vk_diff = vk, # Rename vk in vkdiff
    vk1_diff = vk1 # Rename vk1 in vkdiff
  )

# Ensure columns are of the same type (convert to numeric if needed)
vkdiff <- vkdiff %>%
  mutate(
    vk_diff = as.numeric(vk_diff), # Convert to numeric
    vk1_diff = as.numeric(vk1_diff) # Convert to numeric
  )

# Create vk_updated column in vkdiff based on the logic
vkdiff <- vkdiff %>%
  mutate(vk_updated = case_when(
    str_starts(vkrev, "agree--") ~ vk1_diff, # Update to vk1_diff if vkrev starts with "agree--"
    str_starts(vkrev, "disagree--") ~ vk_diff, # Keep vk_diff if vkrev starts with "disagree--"
    str_starts(vkrev, "agree on second round") ~ vk_diff, # Keep vk_diff if vkrev starts with "agree on second round"
    TRUE ~ vk_diff # Default to vk_diff for any other cases
  ))

# Create a smaller dataset with only Excerpt.x and vk_updated
vk_updated <- vkdiff %>%
  select(Excerpt.x, vk_updated)

# Merge vk_updated back into vktf to create vktf_updated
vktf_updated <- vktf %>%
  left_join(vk_updated, by = "Excerpt.x", relationship = "many-to-many") %>% # Suppress the warning
  mutate(
    vk = as.numeric(vk), # Ensure vk is numeric
    vk_updated = as.numeric(vk_updated) # Ensure vk_updated is numeric
  ) %>%
  mutate(vk_updated = coalesce(vk_updated, vk)) # Update vk_updated with vk where applicable

# Save the updated dataset with the new vk_updated column
write.xlsx(vktf_updated, "/Users/kristensyme/Desktop/R Projects/Fasting/Leveraging the Power of LLMs copy/LLM paper/Diffs Fasting/vktf_updated.xlsx")

#print("Merging and updating completed. The updated file is saved as 'vktf_updated.xlsx'.")

# Function to calculate precision, recall, and F1-score
calculate_metrics <- function(pred, actual) {
  true_positive <- sum(pred == 1 & actual == 1)
  false_positive <- sum(pred == 1 & actual == 0)
  false_negative <- sum(pred == 0 & actual == 1)
  
  precision <- true_positive / (true_positive + false_positive)
  recall <- true_positive / (true_positive + false_negative)
  f1_score <- 2 * (precision * recall) / (precision + recall)
  
  return(c(Precision = precision, Recall = recall, F1_Score = f1_score))
}

# Calculate metrics for vk, vk1, and vk2
metrics_vk <- calculate_metrics(vktf_updated$vk, vktf_updated$vk_updated)
metrics_vk1 <- calculate_metrics(vktf_updated$vk1, vktf_updated$vk_updated)
metrics_vk2 <- calculate_metrics(vktf_updated$vk2, vktf_updated$vk_updated)

# Create a dataframe to store results
vk_metrics <- data.frame(
  Method = c("Human (vk)", "GPT (vk1)", "GPT (vk2)"),
  Precision = c(metrics_vk[1], metrics_vk1[1], metrics_vk2[1]),
  Recall = c(metrics_vk[2], metrics_vk1[2], metrics_vk2[2]),
  F1_Score = c(metrics_vk[3], metrics_vk1[3], metrics_vk2[3])
)

# Print results
print(vk_metrics)

#DEI
deitf <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/deitf.xlsx")
deidiff <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/Leveraging the Power of LLMs copy/LLM paper/Diffs Fasting/deidiff1 remove table.xlsx")

#Remove row 205
deitf <- deitf %>% 
  slice(-205)  # Removes row 205

#Remove row 26
deidiff <- deidiff %>% 
  slice(-26)  

# Rename duplicated columns in deidiff to avoid conflicts
deidiff <- deidiff %>%
  rename(
    dei_diff = dei, # Rename dei in deidiff
    dei1_diff = dei1 # Rename dei1 in deidiff
  )

# Ensure columns are of the same type (convert to numeric if needed)
deidiff <- deidiff %>%
  mutate(
    dei_diff = as.numeric(dei_diff), # Convert to numeric
    dei1_diff = as.numeric(dei1_diff) # Convert to numeric
  )

# Create dei_updated column in deidiff based on the logic
deidiff <- deidiff %>%
  mutate(dei_updated = case_when(
    str_starts(deirev, "agree--") ~ dei1_diff, # Update to dei1_diff if deirev starts with "agree--"
    str_starts(deirev, "disagree--") ~ dei_diff, # Keep dei_diff if deirev starts with "disagree--"
    str_starts(deirev, "agree on second round") ~ dei_diff, # Keep dei_diff if deirev starts with "agree on second round"
    TRUE ~ dei_diff # Default to dei_diff for any other cases
  ))

# Create a smaller dataset with only Excerpt.x and dei_updated
dei_updated <- deidiff %>%
  select(Excerpt.x, dei_updated)

# Merge dei_updated back into deitf to create deitf_updated
deitf_updated <- deitf %>%
  left_join(dei_updated, by = "Excerpt.x", relationship = "many-to-many") %>% # Suppress the warning
  mutate(
    dei = as.numeric(dei), # Ensure dei is numeric
    dei_updated = as.numeric(dei_updated) # Ensure dei_updated is numeric
  ) %>%
  mutate(dei_updated = coalesce(dei_updated, dei)) # Update dei_updated with dei where applicable

# Save the updated dataset with the new dei_updated column
write.xlsx(deitf_updated, "/Users/kristensyme/Desktop/R Projects/Fasting/Leveraging the Power of LLMs copy/LLM paper/Diffs Fasting/deitf_updated.xlsx")

#print("Merging and updating completed. The updated file is saved as 'deitf_updated.xlsx'.")

# Function to calculate precision, recall, and F1-score
calculate_metrics <- function(pred, true) {
  TP <- sum(pred == 1 & true == 1)  # True Positives
  FP <- sum(pred == 1 & true == 0)  # False Positives
  FN <- sum(pred == 0 & true == 1)  # False Negatives
  
  precision <- ifelse((TP + FP) == 0, 0, TP / (TP + FP))
  recall <- ifelse((TP + FN) == 0, 0, TP / (TP + FN))
  f1_score <- ifelse((precision + recall) == 0, 0, 2 * (precision * recall) / (precision + recall))
  
  return(c(precision, recall, f1_score))
}

# Apply the function to dei, dei1, and dei2
metrics_dei <- calculate_metrics(deitf_updated$dei, deitf_updated$dei_updated)
metrics_dei1 <- calculate_metrics(deitf_updated$dei1, deitf_updated$dei_updated)
metrics_dei2 <- calculate_metrics(deitf_updated$dei2, deitf_updated$dei_updated)

# Create a dataframe with results
dei_metrics <- tibble(
  Method = c("Human (dei)", "GPT (dei1)", "GPT (dei2)"),
  Precision = c(metrics_dei[1], metrics_dei1[1], metrics_dei2[1]),
  Recall = c(metrics_dei[2], metrics_dei1[2], metrics_dei2[2]),
  F1_Score = c(metrics_dei[3], metrics_dei1[3], metrics_dei2[3])
)

# Print the results
print(dei_metrics)

#SE
setf <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/setf.xlsx")
sediff <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/Leveraging the Power of LLMs copy/LLM paper/Diffs Fasting/sediff1 remove table.xlsx")

#Remove row 205
setf <- setf %>% 
  slice(-205)  # Removes row 205

# Rename duplicated columns in sediff to avoid conflicts
sediff <- sediff %>%
  rename(
    se_diff = se, # Rename se in sediff
    se2_diff = se2 # Rename se2 in sediff
  )

# Ensure columns are of the same type (convert to numeric if needed)
sediff <- sediff %>%
  mutate(
    se_diff = as.numeric(se_diff), # Convert to numeric
    se2_diff = as.numeric(se2_diff) # Convert to numeric
  )

# Create se_updated column in sediff based on the logic
sediff <- sediff %>%
  mutate(se_updated = case_when(
    str_starts(serev, "agree--") ~ se2_diff, # Update to se2_diff if serev starts with "agree--"
    str_starts(serev, "disagree--") ~ se_diff, # Keep se_diff if serev starts with "disagree--"
    str_starts(serev, "agree on second round") ~ se_diff, # Keep se_diff if serev starts with "agree on second round"
    TRUE ~ se_diff # Default to se_diff for any other cases
  ))

# Create a smaller dataset with only Excerpt.x and se_updated
se_updated <- sediff %>%
  select(Excerpt.x, se_updated)

# Merge se_updated back into setf to create setf_updated
setf_updated <- setf %>%
  left_join(se_updated, by = "Excerpt.x", relationship = "many-to-many") %>% # Suppress the warning
  mutate(
    se = as.numeric(se), # Ensure se is numeric
    se_updated = as.numeric(se_updated) # Ensure se_updated is numeric
  ) %>%
  mutate(se_updated = coalesce(se_updated, se)) # Update se_updated with se where applicable

# Save the updated dataset with the new se_updated column
write.xlsx(setf_updated, "/Users/kristensyme/Desktop/R Projects/Fasting/Leveraging the Power of LLMs copy/LLM paper/Diffs Fasting/setf_updated.xlsx.xlsx")

#print("Merging and updating completed. The updated file is saved as 'setf_updated.xlsx'.")

# Function to calculate precision, recall, and F1-score
calculate_metrics <- function(pred, truth) {
  tp <- sum(pred == 1 & truth == 1)
  fp <- sum(pred == 1 & truth == 0)
  fn <- sum(pred == 0 & truth == 1)
  
  precision <- ifelse((tp + fp) > 0, tp / (tp + fp), 0)
  recall <- ifelse((tp + fn) > 0, tp / (tp + fn), 0)
  f1_score <- ifelse((precision + recall) > 0, 2 * (precision * recall) / (precision + recall), 0)
  
  return(data.frame(Precision = precision, Recall = recall, F1_Score = f1_score))
}

# Compute metrics for each method
metrics_se <- calculate_metrics(setf_updated$se, setf_updated$se_updated) %>% mutate(Method = "Human (se)")
metrics_se1 <- calculate_metrics(setf_updated$se1, setf_updated$se_updated) %>% mutate(Method = "GPT (se1)")
metrics_se2 <- calculate_metrics(setf_updated$se2, setf_updated$se_updated) %>% mutate(Method = "GPT (se2)")

# Combine results into one dataframe
se_metrics <- bind_rows(metrics_se, metrics_se1, metrics_se2) %>%
  select(Method, Precision, Recall, F1_Score)

# Print results
print(se_metrics)

#SCA
scatf <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/scatf.xlsx")
scadiff <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/Leveraging the Power of LLMs copy/LLM paper/Diffs Fasting/scadiff1 remove table.xlsx")

#Remove row 205
scatf <- scatf %>% 
  slice(-205)  # Removes row 205

# Rename duplicated columns in scadiff to avoid conflicts
scadiff <- scadiff %>%
  rename(
    sca_diff = sca, # Rename sca in scadiff
    sca2_diff = sca2 # Rename sca2 in scadiff
  )

# Ensure columns are of the same type (convert to numeric if needed)
scadiff <- scadiff %>%
  mutate(
    sca_diff = as.numeric(sca_diff), # Convert to numeric
    sca2_diff = as.numeric(sca2_diff) # Convert to numeric
  )

# Create sca_updated column in scadiff based on the logic
scadiff <- scadiff %>%
  mutate(sca_updated = case_when(
    str_starts(scarev, "agree--") ~ sca2_diff, # Update to sca2_diff if scarev starts with "agree--"
    str_starts(scarev, "disagree--") ~ sca_diff, # Keep sca_diff if scarev starts with "disagree--"
    str_starts(scarev, "agree on second round") ~ sca_diff, # Keep sca_diff if scarev starts with "agree on second round"
    TRUE ~ sca_diff # Default to sca_diff for any other cases
  ))

# Create a smaller dataset with only Excerpt.x and sca_updated
sca_updated <- scadiff %>%
  select(Excerpt.x, sca_updated)

# Merge sca_updated back into scatf to create scatf_updated
scatf_updated <- scatf %>%
  left_join(sca_updated, by = "Excerpt.x", relationship = "many-to-many") %>% # Suppress the warning
  mutate(
    sca = as.numeric(sca), # Ensure sca is numeric
    sca_updated = as.numeric(sca_updated) # Ensure sca_updated is numeric
  ) %>%
  mutate(sca_updated = coalesce(sca_updated, sca)) # Update sca_updated with sca where applicable

# Save the updated dataset with the new sca_updated column
write.xlsx(scatf_updated, "/Users/kristensyme/Desktop/R Projects/Fasting/Leveraging the Power of LLMs copy/LLM paper/Diffs Fasting/scatf_updated.xlsx")

#print("Merging and updating completed. The updated file is saved as 'scatf_updated.xlsx'.")


# Function to calculate precision, recall, and F1 score
calculate_metrics <- function(pred, actual) {
  true_positive <- sum(pred == 1 & actual == 1)
  false_positive <- sum(pred == 1 & actual == 0)
  false_negative <- sum(pred == 0 & actual == 1)
  
  precision <- true_positive / (true_positive + false_positive)
  recall <- true_positive / (true_positive + false_negative)
  f1_score <- 2 * (precision * recall) / (precision + recall)
  
  return(data.frame(Precision = precision, Recall = recall, F1_Score = f1_score))
}

# Calculate metrics for each method
metrics_sca <- calculate_metrics(scatf_updated$sca, scatf_updated$sca_updated)
metrics_sca1 <- calculate_metrics(scatf_updated$sca1, scatf_updated$sca_updated)
metrics_sca2 <- calculate_metrics(scatf_updated$sca2, scatf_updated$sca_updated)

# Combine results into a single dataframe
results_scatf <- bind_rows(
  cbind(Method = "Human (sca)", metrics_sca),
  cbind(Method = "GPT (sca1)", metrics_sca1),
  cbind(Method = "GPT (sca2)", metrics_sca2)
)

# Print results
print(results_scatf)

#SW
swtf <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/swtf.xlsx")
swdiff <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/Leveraging the Power of LLMs copy/LLM paper/Diffs Fasting/swdiff1 remove table.xlsx")

#Remove row 205
swtf <- swtf %>% 
  slice(-205)  # Removes row 205

# Rename duplicated columns in swdiff to avoid conflicts
swdiff <- swdiff %>%
  rename(
    sw_diff = sw, # Rename sw in swdiff
    sw2_diff = sw2 # Rename sw2 in swdiff
  )

# Ensure columns are of the same type (convert to numeric if needed)
swdiff <- swdiff %>%
  mutate(
    sw_diff = as.numeric(sw_diff), # Convert to numeric
    sw2_diff = as.numeric(sw2_diff) # Convert to numeric
  )

# Create sw_updated column in swdiff based on the logic
swdiff <- swdiff %>%
  mutate(sw_updated = case_when(
    str_starts(swrev, "agree--") ~ sw2_diff, # Update to sw2_diff if swrev starts with "agree--"
    str_starts(swrev, "disagree--") ~ sw_diff, # Keep sw_diff if swrev starts with "disagree--"
    str_starts(swrev, "agree on second round") ~ sw_diff, # Keep sw_diff if swrev starts with "agree on second round"
    TRUE ~ sw_diff # Default to sw_diff for any other cases
  ))

# Create a smaller dataset with only Excerpt.x and sw_updated
sw_updated <- swdiff %>%
  select(Excerpt.x, sw_updated)

# Merge sw_updated back into swtf to create swtf_updated
swtf_updated <- swtf %>%
  left_join(sw_updated, by = "Excerpt.x", relationship = "many-to-many") %>% # Suppress the warning
  mutate(
    sw = as.numeric(sw), # Ensure sw is numeric
    sw_updated = as.numeric(sw_updated) # Ensure sw_updated is numeric
  ) %>%
  mutate(sw_updated = coalesce(sw_updated, sw)) # Update sw_updated with sw where applicable

# Save the updated dataset with the new sw_updated column
write.xlsx(swtf_updated, "/Users/kristensyme/Desktop/R Projects/Fasting/Leveraging the Power of LLMs copy/LLM paper/Diffs Fasting/swtf_updated.xlsx")

#print("Merging and updating completed. The updated file is saved as 'swtf_updated.xlsx'.")

# Function to calculate precision, recall, and F1 score
calculate_metrics <- function(pred, truth) {
  tp <- sum(pred == 1 & truth == 1)  # True Positives
  fp <- sum(pred == 1 & truth == 0)  # False Positives
  fn <- sum(pred == 0 & truth == 1)  # False Negatives
  
  precision <- ifelse((tp + fp) > 0, tp / (tp + fp), 0)
  recall <- ifelse((tp + fn) > 0, tp / (tp + fn), 0)
  f1_score <- ifelse((precision + recall) > 0, 2 * (precision * recall) / (precision + recall), 0)
  
  return(c(precision, recall, f1_score))
}

# Compute metrics for sw, sw2, and sw3
metrics_sw <- calculate_metrics(swtf_updated$sw, swtf_updated$sw_updated)
metrics_sw2 <- calculate_metrics(swtf_updated$sw2, swtf_updated$sw_updated)
metrics_sw3 <- calculate_metrics(swtf_updated$sw3, swtf_updated$sw_updated)

# Create a dataframe for the results
results <- data.frame(
  Method = c("Human (sw)", "GPT (sw2)", "GPT (sw3)"),
  Precision = c(metrics_sw[1], metrics_sw2[1], metrics_sw3[1]),
  Recall = c(metrics_sw[2], metrics_sw2[2], metrics_sw3[2]),
  F1_Score = c(metrics_sw[3], metrics_sw2[3], metrics_sw3[3])
)

# Print results
print(results)

#MAT
mattf <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/mattf.xlsx")
matdiff <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/Leveraging the Power of LLMs copy/LLM paper/Diffs Fasting/matdiff1 remove table.xlsx")

#Remove row 205
mattf <- mattf %>% 
  slice(-205)  # Removes row 205

# Rename duplicated columns in matdiff to avoid conflicts
matdiff <- matdiff %>%
  rename(
    mat_diff = mat, # Rename mat in matdiff
    mat1_diff = mat1 # Rename mat1 in matdiff
  )

# Ensure columns are of the same type (convert to numeric if needed)
matdiff <- matdiff %>%
  mutate(
    mat_diff = as.numeric(mat_diff), # Convert to numeric
    mat1_diff = as.numeric(mat1_diff) # Convert to numeric
  )

# Create mat_updated column in matdiff based on the logic
matdiff <- matdiff %>%
  mutate(mat_updated = case_when(
    str_starts(matrev, "agree--") ~ mat1_diff, # Update to mat1_diff if matrev starts with "agree--"
    str_starts(matrev, "disagree--") ~ mat_diff, # Keep mat_diff if matrev starts with "disagree--"
    str_starts(matrev, "agree on second round") ~ mat_diff, # Keep mat_diff if matrev starts with "agree on second round"
    TRUE ~ mat_diff # Default to mat_diff for any other cases
  ))

# Create a smaller dataset with only Excerpt.x and mat_updated
mat_updated <- matdiff %>%
  select(Excerpt.x, mat_updated)

# Merge mat_updated back into mattf to create mattf_updated
mattf_updated <- mattf %>%
  left_join(mat_updated, by = "Excerpt.x", relationship = "many-to-many") %>% # Suppress the warning
  mutate(
    mat = as.numeric(mat), # Ensure mat is numeric
    mat_updated = as.numeric(mat_updated) # Ensure mat_updated is numeric
  ) %>%
  mutate(mat_updated = coalesce(mat_updated, mat)) # Update mat_updated with mat where applicable

# Save the updated dataset with the new mat_updated column
write.xlsx(mattf_updated, "/Users/kristensyme/Desktop/R Projects/Fasting/Leveraging the Power of LLMs copy/LLM paper/Diffs Fasting/mattf_updated.xlsx.xlsx")

#print("Merging and updating completed. The updated file is saved as 'mattf_updated.xlsx'.")

# Function to calculate precision, recall, and F1-score
calculate_metrics <- function(pred, actual) {
  true_positive <- sum(pred == 1 & actual == 1)
  false_positive <- sum(pred == 1 & actual == 0)
  false_negative <- sum(pred == 0 & actual == 1)
  
  precision <- true_positive / (true_positive + false_positive)
  recall <- true_positive / (true_positive + false_negative)
  f1_score <- 2 * (precision * recall) / (precision + recall)
  
  return(c(precision, recall, f1_score))
}

# Apply the function to mat, mat1, and mat2 compared to mat_updated
metrics_mat <- calculate_metrics(mattf_updated$mat, mattf_updated$mat_updated)
metrics_mat1 <- calculate_metrics(mattf_updated$mat1, mattf_updated$mat_updated)
metrics_mat2 <- calculate_metrics(mattf_updated$mat2, mattf_updated$mat_updated)

# Create a results dataframe
results_mat <- data.frame(
  Method = c("Human (mat)", "GPT (mat1)", "GPT (mat2)"),
  Precision = c(metrics_mat[1], metrics_mat1[1], metrics_mat2[1]),
  Recall = c(metrics_mat[2], metrics_mat1[2], metrics_mat2[2]),
  F1_Score = c(metrics_mat[3], metrics_mat1[3], metrics_mat2[3])
)

# Print results
print(results_mat)


#REL
reltf <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/reltf.xlsx")
reldiff <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/Leveraging the Power of LLMs copy/LLM paper/Diffs Fasting/reldiff2 remove table.xlsx")

#Remove row 205
reltf <- reltf %>% 
  slice(-205)  # Removes row 205

# Rename duplicated columns in reldiff to avoid conflicts
reldiff <- reldiff %>%
  rename(
    rel_diff = rel, # Rename rel in reldiff
    rel2_diff = rel2 # Rename rel2 in reldiff
  )

# Ensure columns are of the same type (convert to numeric if needed)
reldiff <- reldiff %>%
  mutate(
    rel_diff = as.numeric(rel_diff), # Convert to numeric
    rel2_diff = as.numeric(rel2_diff) # Convert to numeric
  )

# Create rel_updated column in reldiff based on the logic
reldiff <- reldiff %>%
  mutate(rel_updated = case_when(
    str_starts(relrev, "agree--") ~ rel2_diff, # Update to rel2_diff if relrev starts with "agree--"
    str_starts(relrev, "disagree--") ~ rel_diff, # Keep rel_diff if relrev starts with "disagree--"
    str_starts(relrev, "agree on second round") ~ rel_diff, # Keep rel_diff if relrev starts with "agree on second round"
    TRUE ~ rel_diff # Default to rel_diff for any other cases
  ))

# Create a smaller dataset with only Excerpt.x and rel_updated
rel_updated <- reldiff %>%
  select(Excerpt.x, rel_updated)

# Merge rel_updated back into reltf to create reltf_updated
reltf_updated <- reltf %>%
  left_join(rel_updated, by = "Excerpt.x", relationship = "many-to-many") %>% # Suppress the warning
  mutate(
    rel = as.numeric(rel), # Ensure rel is numeric
    rel_updated = as.numeric(rel_updated) # Ensure rel_updated is numeric
  ) %>%
  mutate(rel_updated = coalesce(rel_updated, rel)) # Update rel_updated with rel where applicable

# Save the updated dataset with the new rel_updated column
write.xlsx(reltf_updated, "/Users/kristensyme/Desktop/R Projects/Fasting/Leveraging the Power of LLMs copy/LLM paper/Diffs Fasting/reltf_updated.xlsx")

#print("Merging and updating completed. The updated file is saved as 'reltf_updated.xlsx'.")

# Function to calculate precision, recall, and F1-score using dplyr
calculate_metrics <- function(pred, truth) {
  conf_matrix <- table(pred, truth)
  
  tp <- conf_matrix["1", "1"] # True Positives
  fp <- conf_matrix["1", "0"] # False Positives
  fn <- conf_matrix["0", "1"] # False Negatives
  
  precision <- tp / (tp + fp)
  recall <- tp / (tp + fn)
  f1 <- 2 * (precision * recall) / (precision + recall)
  
  return(c(precision, recall, f1))
}

# Apply function to each method
metrics_rel <- calculate_metrics(reltf_updated$rel, reltf_updated$rel_updated)
metrics_rel2 <- calculate_metrics(reltf_updated$rel2, reltf_updated$rel_updated)
metrics_rel3 <- calculate_metrics(reltf_updated$rel3, reltf_updated$rel_updated)

# Create dataframe for results
results_rel <- tibble(
  Method = c("Human (rel)", "GPT (rel2)", "GPT (rel3)"),
  Precision = c(metrics_rel[1], metrics_rel2[1], metrics_rel3[1]),
  Recall = c(metrics_rel[2], metrics_rel2[2], metrics_rel3[2]),
  F1_Score = c(metrics_rel[3], metrics_rel2[3], metrics_rel3[3])
)

# Print results
print(results_rel)

#GM
gmtf <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/gmtf.xlsx")
gmdiff <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/Leveraging the Power of LLMs copy/LLM paper/Diffs Fasting/gmdiff1 remove table.xlsx")

#Remove row 205
gmtf <- gmtf %>% 
  slice(-205)  # Removes row 205

# Rename duplicated columns in gmdiff to avoid conflicts
gmdiff <- gmdiff %>%
  rename(
    gm_diff = gm, # Rename gm in gmdiff
    gm1_diff = gm1 # Rename gm1 in gmdiff
  )

# Ensure columns are of the same type (convert to numeric if needed)
gmdiff <- gmdiff %>%
  mutate(
    gm_diff = as.numeric(gm_diff), # Convert to numeric
    gm1_diff = as.numeric(gm1_diff) # Convert to numeric
  )

# Create gm_updated column in gmdiff based on the logic
gmdiff <- gmdiff %>%
  mutate(gm_updated = case_when(
    str_starts(gmrev, "agree--") ~ gm1_diff, # Update to gm1_diff if gmrev starts with "agree--"
    str_starts(gmrev, "disagree--") ~ gm_diff, # Keep gm_diff if gmrev starts with "disagree--"
    str_starts(gmrev, "agree on second round") ~ gm_diff, # Keep gm_diff if gmrev starts with "agree on second round"
    TRUE ~ gm_diff # Default to gm_diff for any other cases
  ))

# Create a smaller dataset with only Excerpt.x and gm_updated
gm_updated <- gmdiff %>%
  select(Excerpt.x, gm_updated)

# Merge gm_updated back into gmtf to create gmtf_updated
gmtf_updated <- gmtf %>%
  left_join(gm_updated, by = "Excerpt.x", relationship = "many-to-many") %>% # Suppress the warning
  mutate(
    gm = as.numeric(gm), # Ensure gm is numeric
    gm_updated = as.numeric(gm_updated) # Ensure gm_updated is numeric
  ) %>%
  mutate(gm_updated = coalesce(gm_updated, gm)) # Update gm_updated with gm where applicable

# Save the updated dataset with the new gm_updated column
write.xlsx(gmtf_updated, "/Users/kristensyme/Desktop/R Projects/Fasting/Leveraging the Power of LLMs copy/LLM paper/Diffs Fasting/gmtf_updated.xlsx")

#print("Merging and updating completed. The updated file is saved as 'gmtf_updated.xlsx'.")

# Function to calculate precision, recall, and F1 score
calculate_metrics <- function(predicted, actual) {
  tp <- sum(predicted == 1 & actual == 1)  # True Positives
  fp <- sum(predicted == 1 & actual == 0)  # False Positives
  fn <- sum(predicted == 0 & actual == 1)  # False Negatives
  
  precision <- ifelse((tp + fp) > 0, tp / (tp + fp), 0)
  recall <- ifelse((tp + fn) > 0, tp / (tp + fn), 0)
  f1_score <- ifelse((precision + recall) > 0, 2 * (precision * recall) / (precision + recall), 0)
  
  return(c(precision, recall, f1_score))
}

# Calculate metrics for gm (human), gm1, gm2
metrics_gm <- data.frame(
  Method = c("Human (gm)", "GPT (gm1)", "GPT (gm2)"),
  t(sapply(list(gmtf_updated$gm, gmtf_updated$gm1, gmtf_updated$gm2), 
           calculate_metrics, actual = gmtf_updated$gm_updated))
)

# Rename columns
colnames(metrics_gm) <- c("Method", "Precision", "Recall", "F1_Score")

# Print results
print(metrics_gm)

