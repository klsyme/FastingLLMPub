library(dplyr)
library(readxl)  # Ensure readxl is loaded for Excel files
library(knitr)
library(caret) 
library(ggplot2)
library(reshape2)
library(tidyr)

ascd <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/Final Agreement Analysis/disagreement_asc.xlsx")
ascf <- ascd %>% filter(!is.na(asc.final))
# Convert columns to numeric
ascf <- ascf %>%
  mutate(across(c(asc.x, asc.y, asc.gpt), as.numeric))
# Create the asc.mr column using majority rule
ascf <- ascf %>%
  rowwise() %>%
  mutate(asc.mr = ifelse(sum(c(asc.x, asc.y, asc.gpt)) >= 2, 1, 0)) %>%
  ungroup()

deid <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/Final Agreement Analysis/disagreement_dei.xlsx")
deif <- deid %>% filter(!is.na(dei.final))
# Convert columns to numeric
deif <- deif %>%
  mutate(across(c(dei.x, dei.y, dei.gpt), as.numeric))
# Create the asc.mr column using majority rule
deif <- deif %>%
  rowwise() %>%
  mutate(dei.mr = ifelse(sum(c(dei.x, dei.y, dei.gpt)) >= 2, 1, 0)) %>%
  ungroup()

gmd <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/Final Agreement Analysis/disagreement_gm.xlsx")
gmf <- gmd %>% filter(!is.na(gm.final))
# Convert columns to numeric
gmf <- gmf %>%
  mutate(across(c(gm.x, gm.y, gm.gpt), as.numeric))
# Create the asc.mr column using majority rule
gmf <- gmf %>%
  rowwise() %>%
  mutate(gm.mr = ifelse(sum(c(gm.x, gm.y, gm.gpt)) >= 2, 1, 0)) %>%
  ungroup()

lapd <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/Final Agreement Analysis/disagreement_lap.xlsx")
lapf <- lapd %>% filter(!is.na(lap.final))
# Convert columns to numeric
lapf <- lapf %>%
  mutate(across(c(time_lapse.x, time_lapse.y, lap.gpt), as.numeric))
# Create the asc.mr column using majority rule
lapf <- lapf %>%
  rowwise() %>%
  mutate(lap.mr = ifelse(sum(c(time_lapse.x, time_lapse.y, lap.gpt)) >= 2, 1, 0)) %>%
  ungroup()

leadd <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/Final Agreement Analysis/disagreement_lead.xlsx")
leadf <- leadd %>% filter(!is.na(lead.final))
# Convert columns to numeric
leadf <- leadf %>%
  mutate(across(c(leader.x, leader.y, lead.gpt), as.numeric))
# Create the asc.mr column using majority rule
leadf <- leadf %>%
  rowwise() %>%
  mutate(lead.mr = ifelse(sum(c(leader.x, leader.y, lead.gpt)) >= 2, 1, 0)) %>%
  ungroup()

matd <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/Final Agreement Analysis/disagreement_mat.xlsx")
matf <- matd %>% filter(!is.na(mat.final))
# Convert columns to numeric
matf <- matf %>%
  mutate(across(c(mat.x, mat.y, mat.gpt), as.numeric))
# Create the asc.mr column using majority rule
matf <- matf %>%
  rowwise() %>%
  mutate(mat.mr = ifelse(sum(c(mat.x, mat.y, mat.gpt)) >= 2, 1, 0)) %>%
  ungroup()

reld <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/Final Agreement Analysis/disagreement_rel.xlsx")
relf <- reld %>% filter(!is.na(rel.final))
# Convert columns to numeric
relf <- relf %>%
  mutate(across(c(rel.x, rel.y, rel.gpt), as.numeric))
# Create the asc.mr column using majority rule
relf <- relf %>%
  rowwise() %>%
  mutate(rel.mr = ifelse(sum(c(rel.x, rel.y, rel.gpt)) >= 2, 1, 0)) %>%
  ungroup()


scad <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/Final Agreement Analysis/disagreement_sca.xlsx")
scaf <- scad %>% filter(!is.na(sca.final))
# Convert columns to numeric
scaf <- scaf %>%
  mutate(across(c(sca.x, sca.y, sca.gpt), as.numeric))
# Create the asc.mr column using majority rule
scaf <- scaf %>%
  rowwise() %>%
  mutate(sca.mr = ifelse(sum(c(sca.x, sca.y, sca.gpt)) >= 2, 1, 0)) %>%
  ungroup()

sed <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/Final Agreement Analysis/disagreement_se.xlsx")
sef <- sed %>% filter(!is.na(se.final))
# Convert columns to numeric
sef <- sef %>%
  mutate(across(c(se.x, se.y, se.gpt), as.numeric))
# Create the asc.mr column using majority rule
sef <- sef %>%
  rowwise() %>%
  mutate(se.mr = ifelse(sum(c(se.x, se.y, se.gpt)) >= 2, 1, 0)) %>%
  ungroup()

swd <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/Final Agreement Analysis/disagreement_sw.xlsx")
swf <- swd %>% filter(!is.na(sw.final))
# Convert columns to numeric
swf <- swf %>%
  mutate(across(c(sw.x, sw.y, sw.gpt), as.numeric))
# Create the asc.mr column using majority rule
swf <- swf %>%
  rowwise() %>%
  mutate(sw.mr = ifelse(sum(c(sw.x, sw.y, sw.gpt)) >= 2, 1, 0)) %>%
  ungroup()

vkd <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/Final Agreement Analysis/disagreement_vk.xlsx")
vkf <- vkd %>% filter(!is.na(vk.final))
# Convert columns to numeric
vkf <- vkf %>%
  mutate(across(c(vk.x, vk.y, vk.gpt), as.numeric))
# Create the asc.mr column using majority rule
vkf <- vkf %>%
  rowwise() %>%
  mutate(vk.mr = ifelse(sum(c(vk.x, vk.y, vk.gpt)) >= 2, 1, 0)) %>%
  ungroup()


## Precision, Recall, F1

# ASC w/ .mr

# Ensure all columns are factors with the same levels
ascf$asc.final <- as.factor(ascf$asc.final)
ascf$asc.x <- factor(ascf$asc.x, levels = levels(ascf$asc.final))
ascf$asc.y <- factor(ascf$asc.y, levels = levels(ascf$asc.final))
ascf$asc.gpt <- factor(ascf$asc.gpt, levels = levels(ascf$asc.final))
ascf$asc.mr <- factor(ascf$asc.mr, levels = levels(ascf$asc.final))  # Add asc.mr

# Function to compute precision, recall, and F1-score
compute_metrics <- function(predictions, reference) {
  # Ensure factors
  predictions <- factor(predictions, levels = levels(reference))
  
  # Compute confusion matrix
  cm <- confusionMatrix(predictions, reference)
  
  # Extract precision, recall, and compute F1-score
  precision <- cm$byClass["Pos Pred Value"]  # Precision
  recall <- cm$byClass["Sensitivity"]  # Recall
  f1_score <- 2 * (precision * recall) / (precision + recall)  # F1-score
  
  return(data.frame(Precision = precision, Recall = recall, F1 = f1_score))
}

# Compute metrics for each coder and asc.mr
metrics_ascx <- compute_metrics(ascf$asc.x, ascf$asc.final)
metrics_ascy <- compute_metrics(ascf$asc.y, ascf$asc.final)
metrics_ascgpt <- compute_metrics(ascf$asc.gpt, ascf$asc.final)
metrics_ascmr <- compute_metrics(ascf$asc.mr, ascf$asc.final)  # Compute metrics for asc.mr

# Display results
list(Coder_2 = metrics_ascx, Coder_1 = metrics_ascy, GPT = metrics_ascgpt, Majority_Rule = metrics_ascmr)


# DEI w/ .mr

# Ensure all columns are factors with the same levels
deif$dei.final <- as.factor(deif$dei.final)
deif$dei.x <- factor(deif$dei.x, levels = levels(deif$dei.final))
deif$dei.y <- factor(deif$dei.y, levels = levels(deif$dei.final))
deif$dei.gpt <- factor(deif$dei.gpt, levels = levels(deif$dei.final))
deif$dei.mr <- factor(deif$dei.mr, levels = levels(deif$dei.final))  # Add dei.mr

# # Function to compute precision, recall, and F1-score
# compute_metrics <- function(predictions, reference) {
#   # Ensure factors
#   predictions <- factor(predictions, levels = levels(reference))
#   
#   # Compute confusion matrix
#   cm <- confusionMatrix(predictions, reference)
#   
#   # Extract precision, recall, and compute F1-score
#   precision <- cm$byClass["Pos Pred Value"]  # Precision
#   recall <- cm$byClass["Sensitivity"]  # Recall
#   f1_score <- 2 * (precision * recall) / (precision + recall)  # F1-score
#   
#   return(data.frame(Precision = precision, Recall = recall, F1 = f1_score))
# }

# Compute metrics for each coder and dei.mr
metrics_deix <- compute_metrics(deif$dei.x, deif$dei.final)
metrics_deiy <- compute_metrics(deif$dei.y, deif$dei.final)
metrics_deigpt <- compute_metrics(deif$dei.gpt, deif$dei.final)
metrics_deimr <- compute_metrics(deif$dei.mr, deif$dei.final)  # Compute metrics for dei.mr

# Display results
list(Coder_2 = metrics_deix, Coder_1 = metrics_deiy, GPT = metrics_deigpt, Majority_Rule = metrics_deimr)


# GM w/ .mr
# Ensure all columns are factors with the same levels
gmf$gm.final <- as.factor(gmf$gm.final)
gmf$gm.x <- factor(gmf$gm.x, levels = levels(gmf$gm.final))
gmf$gm.y <- factor(gmf$gm.y, levels = levels(gmf$gm.final))
gmf$gm.gpt <- factor(gmf$gm.gpt, levels = levels(gmf$gm.final))
gmf$gm.mr <- factor(gmf$gm.mr, levels = levels(gmf$gm.final))  # Add gm.mr

# # Function to compute precision, recall, and F1-score
# compute_metrics <- function(predictions, reference) {
#   # Ensure factors
#   predictions <- factor(predictions, levels = levels(reference))
#   
#   # Compute confusion matrix
#   cm <- confusionMatrix(predictions, reference)
#   
#   # Extract precision, recall, and compute F1-score
#   precision <- cm$byClass["Pos Pred Value"]  # Precision
#   recall <- cm$byClass["Sensitivity"]  # Recall
#   f1_score <- 2 * (precision * recall) / (precision + recall)  # F1-score
#   
#   # Return a clean data frame
#   return(data.frame(Precision = precision, Recall = recall, F1 = f1_score))
# }

# Compute metrics for each coder and gm.mr
metrics_gmx <- compute_metrics(gmf$gm.x, gmf$gm.final)
metrics_gmy <- compute_metrics(gmf$gm.y, gmf$gm.final)
metrics_gmgpt <- compute_metrics(gmf$gm.gpt, gmf$gm.final)
metrics_gmmr <- compute_metrics(gmf$gm.mr, gmf$gm.final)  # Compute metrics for gm.mr

# Display results
list(Coder_2 = metrics_gmx, Coder_1 = metrics_gmy, GPT = metrics_gmgpt, Majority_Rule = metrics_gmmr)

# LAP
# Ensure all columns are factors with the same levels
lapf$lap.final <- as.factor(lapf$lap.final)
lapf$time_lapse.x <- factor(lapf$time_lapse.x, levels = levels(lapf$lap.final))
lapf$time_lapse.y <- factor(lapf$time_lapse.y, levels = levels(lapf$lap.final))
lapf$lap.gpt <- factor(lapf$lap.gpt, levels = levels(lapf$lap.final))
lapf$lap.mr <- factor(lapf$lap.mr, levels = levels(lapf$lap.final))  # Add lap.mr

# # Function to compute precision, recall, and F1-score
# compute_metrics <- function(predictions, reference) {
#   # Ensure factors
#   predictions <- factor(predictions, levels = levels(reference))
#   
#   # Compute confusion matrix
#   cm <- confusionMatrix(predictions, reference)
#   
#   # Extract precision, recall, and compute F1-score
#   precision <- cm$byClass["Pos Pred Value"]  # Precision
#   recall <- cm$byClass["Sensitivity"]  # Recall
#   f1_score <- 2 * (precision * recall) / (precision + recall)  # F1-score
#   
#   # Return a clean data frame
#   return(data.frame(Precision = precision, Recall = recall, F1 = f1_score))
# }

# Compute metrics for each coder and lap.mr
metrics_lapx <- compute_metrics(lapf$time_lapse.x, lapf$lap.final)
metrics_lapy <- compute_metrics(lapf$time_lapse.y, lapf$lap.final)
metrics_lapgpt <- compute_metrics(lapf$lap.gpt, lapf$lap.final)
metrics_lapmr <- compute_metrics(lapf$lap.mr, lapf$lap.final)  # Compute metrics for lap.mr

# Display results
list(Coder_2 = metrics_lapx, Coder_lap1 = metrics_lapy, GPT = metrics_lapgpt, Majority_Rule = metrics_lapmr)

# Lead
# Ensure all columns are factors with the same levels
leadf$lead.final <- as.factor(leadf$lead.final)
leadf$leader.x <- factor(leadf$leader.x, levels = levels(leadf$lead.final))
leadf$leader.y <- factor(leadf$leader.y, levels = levels(leadf$lead.final))
leadf$lead.gpt <- factor(leadf$lead.gpt, levels = levels(leadf$lead.final))
leadf$lead.mr <- factor(leadf$lead.mr, levels = levels(leadf$lead.final))  # Add lead.mr

# # Function to compute precision, recall, and F1-score
# compute_metrics <- function(predictions, reference) {
#   # Ensure factors
#   predictions <- factor(predictions, levels = levels(reference))
#   
#   # Compute confusion matrix
#   cm <- confusionMatrix(predictions, reference)
#   
#   # Extract precision, recall, and compute F1-score
#   precision <- cm$byClass["Pos Pred Value"]  # Precision
#   recall <- cm$byClass["Sensitivity"]  # Recall
#   f1_score <- 2 * (precision * recall) / (precision + recall)  # F1-score
#   
#   # Return a clean data frame
#   return(data.frame(Precision = precision, Recall = recall, F1 = f1_score))
# }

# Compute metrics for each coder and lead.mr
metrics_leadx <- compute_metrics(leadf$leader.x, leadf$lead.final)
metrics_leady <- compute_metrics(leadf$leader.y, leadf$lead.final)
metrics_leadgpt <- compute_metrics(leadf$lead.gpt, leadf$lead.final)
metrics_leadmr <- compute_metrics(leadf$lead.mr, leadf$lead.final)  # Compute metrics for lead.mr

# Display results
list(Coder_2 = metrics_leadx, Coder_1 = metrics_leady, GPT = metrics_leadgpt, Majority_Rule = metrics_leadmr)


# MAT
# Ensure all columns are factors with the same levels
matf$mat.final <- as.factor(matf$mat.final)
matf$mat.x <- factor(matf$mat.x, levels = levels(matf$mat.final))
matf$mat.y <- factor(matf$mat.y, levels = levels(matf$mat.final))
matf$mat.gpt <- factor(matf$mat.gpt, levels = levels(matf$mat.final))
matf$mat.mr <- factor(matf$mat.mr, levels = levels(matf$mat.final))  # Add mat.mr

# # Function to compute precision, recall, and F1-score
# compute_metrics <- function(predictions, reference) {
#   # Ensure factors
#   predictions <- factor(predictions, levels = levels(reference))
#   
#   # Compute confusion matrix
#   cm <- confusionMatrix(predictions, reference)
#   
#   # Extract precision, recall, and compute F1-score
#   precision <- cm$byClass["Pos Pred Value"]  # Precision
#   recall <- cm$byClass["Sensitivity"]  # Recall
#   f1_score <- 2 * (precision * recall) / (precision + recall)  # F1-score
#   
#   # Return a clean data frame
#   return(data.frame(Precision = precision, Recall = recall, F1 = f1_score))
# }

# Compute metrics for each coder and mat.mr
metrics_matx <- compute_metrics(matf$mat.x, matf$mat.final)
metrics_maty <- compute_metrics(matf$mat.y, matf$mat.final)
metrics_matgpt <- compute_metrics(matf$mat.gpt, matf$mat.final)
metrics_matmr <- compute_metrics(matf$mat.mr, matf$mat.final)  # Compute metrics for mat.mr

# Display results
list(Coder_2 = metrics_matx, Coder_1 = metrics_maty, GPT = metrics_matgpt, Majority_Rule = metrics_matmr)


# REL

# Ensure all columns are factors with the same levels
relf$rel.final <- as.factor(relf$rel.final)
relf$rel.x <- factor(relf$rel.x, levels = levels(relf$rel.final))
relf$rel.y <- factor(relf$rel.y, levels = levels(relf$rel.final))
relf$rel.gpt <- factor(relf$rel.gpt, levels = levels(relf$rel.final))
relf$rel.mr <- factor(relf$rel.mr, levels = levels(relf$rel.final))  # Add rel.mr

# # Function to compute precision, recall, and F1-score
# compute_metrics <- function(predictions, reference) {
#   # Ensure factors
#   predictions <- factor(predictions, levels = levels(reference))
#   
#   # Compute confusion matrix
#   cm <- confusionMatrix(predictions, reference)
#   
#   # Extract precision, recall, and compute F1-score
#   precision <- cm$byClass["Pos Pred Value"]  # Precision
#   recall <- cm$byClass["Sensitivity"]  # Recall
#   f1_score <- 2 * (precision * recall) / (precision + recall)  # F1-score
#   
#   # Return a clean data frame
#   return(data.frame(Precision = precision, Recall = recall, F1 = f1_score))
# }

# Compute metrics for each coder and rel.mr
metrics_relx <- compute_metrics(relf$rel.x, relf$rel.final)
metrics_rely <- compute_metrics(relf$rel.y, relf$rel.final)
metrics_relgpt <- compute_metrics(relf$rel.gpt, relf$rel.final)
metrics_relmr <- compute_metrics(relf$rel.mr, relf$rel.final)  # Compute metrics for rel.mr

# Display results
list(Coder_2 = metrics_relx, Coder_1 = metrics_rely, GPT = metrics_relgpt, Majority_Rule = metrics_relmr)

# SCA

# Ensure all columns are factors with the same levels
scaf$sca.final <- as.factor(scaf$sca.final)
scaf$sca.x <- factor(scaf$sca.x, levels = levels(scaf$sca.final))
scaf$sca.y <- factor(scaf$sca.y, levels = levels(scaf$sca.final))
scaf$sca.gpt <- factor(scaf$sca.gpt, levels = levels(scaf$sca.final))
scaf$sca.mr <- factor(scaf$sca.mr, levels = levels(scaf$sca.final))  # Add sca.mr

# # Function to compute precision, recall, and F1-score
# compute_metrics <- function(predictions, reference) {
#   # Ensure factors
#   predictions <- factor(predictions, levels = levels(reference))
#   
#   # Compute confusion matrix
#   cm <- confusionMatrix(predictions, reference)
#   
#   # Extract precision, recall, and compute F1-score
#   precision <- cm$byClass["Pos Pred Value"]  # Precision
#   recall <- cm$byClass["Sensitivity"]  # Recall
#   f1_score <- 2 * (precision * recall) / (precision + recall)  # F1-score
#   
#   # Return a clean data frame
#   return(data.frame(Precision = precision, Recall = recall, F1 = f1_score))
# }

# Compute metrics for each coder and sca.mr
metrics_scax <- compute_metrics(scaf$sca.x, scaf$sca.final)
metrics_scay <- compute_metrics(scaf$sca.y, scaf$sca.final)
metrics_scagpt <- compute_metrics(scaf$sca.gpt, scaf$sca.final)
metrics_scamr <- compute_metrics(scaf$sca.mr, scaf$sca.final)  # Compute metrics for sca.mr

# Display results
list(Coder_2 = metrics_scax, Coder_1 = metrics_scay, GPT = metrics_scagpt, Majority_Rule = metrics_scamr)

# SE

# Ensure all columns are factors with the same levels
sef$se.final <- as.factor(sef$se.final)
sef$se.x <- factor(sef$se.x, levels = levels(sef$se.final))
sef$se.y <- factor(sef$se.y, levels = levels(sef$se.final))
sef$se.gpt <- factor(sef$se.gpt, levels = levels(sef$se.final))
sef$se.mr <- factor(sef$se.mr, levels = levels(sef$se.final))  # Add se.mr

# # Function to compute precision, recall, and F1-score
# compute_metrics <- function(predictions, reference) {
#   # Ensure factors
#   predictions <- factor(predictions, levels = levels(reference))
#   
#   # Compute confusion matrix
#   cm <- confusionMatrix(predictions, reference)
#   
#   # Extract precision, recall, and compute F1-score
#   precision <- cm$byClass["Pos Pred Value"]  # Precision
#   recall <- cm$byClass["Sensitivity"]  # Recall
#   f1_score <- 2 * (precision * recall) / (precision + recall)  # F1-score
#   
#   # Return a clean data frame
#   return(data.frame(Precision = precision, Recall = recall, F1 = f1_score))
# }

# Compute metrics for each coder and se.mr
metrics_sex <- compute_metrics(sef$se.x, sef$se.final)
metrics_sey <- compute_metrics(sef$se.y, sef$se.final)
metrics_segpt <- compute_metrics(sef$se.gpt, sef$se.final)
metrics_semr <- compute_metrics(sef$se.mr, sef$se.final)  # Compute metrics for se.mr

# Display results
list(Coder_2 = metrics_sex, Coder_1 = metrics_sey, GPT = metrics_segpt, Majority_Rule = metrics_semr)

# SW
# Ensure all columns are factors with the same levels
swf$sw.final <- as.factor(swf$sw.final)
swf$sw.x <- factor(swf$sw.x, levels = levels(swf$sw.final))
swf$sw.y <- factor(swf$sw.y, levels = levels(swf$sw.final))
swf$sw.gpt <- factor(swf$sw.gpt, levels = levels(swf$sw.final))
swf$sw.mr <- factor(swf$sw.mr, levels = levels(swf$sw.final))  # Add sw.mr

# Compute metrics for each coder and sw.mr
metrics_swx <- compute_metrics(swf$sw.x, swf$sw.final)
metrics_swy <- compute_metrics(swf$sw.y, swf$sw.final)
metrics_swgpt <- compute_metrics(swf$sw.gpt, swf$sw.final)
metrics_swmr <- compute_metrics(swf$sw.mr, swf$sw.final)  # Compute metrics for sw.mr

# Display results
list(Coder_2 = metrics_swx, Coder_1 = metrics_swy, GPT = metrics_swgpt, Majority_Rule = metrics_swmr)

# VK
# Ensure all columns are factors with the same levels
vkf$vk.final <- as.factor(vkf$vk.final)
vkf$vk.x <- factor(vkf$vk.x, levels = levels(vkf$vk.final))
vkf$vk.y <- factor(vkf$vk.y, levels = levels(vkf$vk.final))
vkf$vk.gpt <- factor(vkf$vk.gpt, levels = levels(vkf$vk.final))
vkf$vk.mr <- factor(vkf$vk.mr, levels = levels(vkf$vk.final))  # Add vk.mr

# # Function to compute precision, recall, and F1-score
# compute_metrics <- function(predictions, reference) {
#   # Ensure factors
#   predictions <- factor(predictions, levels = levels(reference))
#   
#   # Check if all predictions are 0
#   if (all(predictions == 0)) {
#     # Check if there are any actual positives in the reference
#     if (sum(reference == 1) == 0) {
#       # If no actual positives, recall is undefined
#       recall <- NA
#     } else {
#       # If there are actual positives, recall is 0
#       recall <- 0
#     }
#     precision <- NA  # Precision is undefined
#     f1_score <- NA   # F1 is undefined
#   } else {
#     # Compute confusion matrix
#     cm <- confusionMatrix(predictions, reference)
#     
#     # Extract precision, recall, and compute F1-score
#     precision <- cm$byClass["Pos Pred Value"]  # Precision
#     recall <- cm$byClass["Sensitivity"]       # Recall
#     f1_score <- 2 * (precision * recall) / (precision + recall)  # F1-score
#   }
#   
#   # Return a clean data frame
#   return(data.frame(Precision = precision, Recall = recall, F1 = f1_score))
# }

# Compute metrics for each coder and vk.mr
metrics_vkx <- compute_metrics(vkf$vk.x, vkf$vk.final)
metrics_vky <- compute_metrics(vkf$vk.y, vkf$vk.final)
metrics_vkgpt <- compute_metrics(vkf$vk.gpt, vkf$vk.final)
metrics_vkmr <- compute_metrics(vkf$vk.mr, vkf$vk.final)  # Compute metrics for vk.mr

# Display results
list(Coder_2 = metrics_vkx, Coder_1 = metrics_vky, GPT = metrics_vkgpt, Majority_Rule = metrics_vkmr)

# Unresolved barcharts
calculate_metrics <- function(predicted, actual) {
  predicted <- factor(predicted, levels = levels(actual))
  # Ensure "1" is the positive class
  positive_class <- "1" 
  cm <- confusionMatrix(predicted, actual, positive = positive_class)
  precision <- cm$byClass["Pos Pred Value"]
  recall <- cm$byClass["Sensitivity"]
  f1_score <- ifelse(is.na(precision + recall), 0, 2 * (precision * recall) / (precision + recall))
  return(c(precision, recall, f1_score))
}

datasets <- list(
  list(name = "Lead", df = leadf, vars = c("leader.y", "leader.x", "lead.gpt", "lead.mr"), 
       target = "lead.final", method_names = c("Coder 1", "Coder 2", "GPT", "Majority Rule")),
  list(name = "Lap", df = lapf, vars = c("time_lapse.y", "time_lapse.x", "lap.gpt", "lap.mr"), 
       target = "lap.final", method_names = c("Coder 1", "Coder 2", "GPT", "Majority Rule")),
  list(name = "Asc", df = ascf, vars = c("asc.y", "asc.x", "asc.gpt", "asc.mr"), 
       target = "asc.final", method_names = c("Coder 1", "Coder 2", "GPT", "Majority Rule")),
  list(name = "Vk", df = vkf, vars = c("vk.y", "vk.x", "vk.gpt", "vk.mr"), 
       target = "vk.final", method_names = c("Coder 1", "Coder 2", "GPT", "Majority Rule")),
  list(name = "Dei", df = deif, vars = c("dei.y", "dei.x", "dei.gpt", "dei.mr"), 
       target = "dei.final", method_names = c("Coder 1", "Coder 2", "GPT", "Majority Rule")),
  list(name = "Se", df = sef, vars = c("se.y", "se.x", "se.gpt", "se.mr"), 
       target = "se.final", method_names = c("Coder 1", "Coder 2", "GPT", "Majority Rule")),
  list(name = "Sca", df = scaf, vars = c("sca.y", "sca.x", "sca.gpt", "sca.mr"), 
       target = "sca.final", method_names = c("Coder 1", "Coder 2", "GPT", "Majority Rule")),
  list(name = "Sw", df = swf, vars = c("sw.y", "sw.x", "sw.gpt", "sw.mr"), 
       target = "sw.final", method_names = c("Coder 1","Coder 2", "GPT", "Majority Rule")),
  list(name = "Mat", df = matf, vars = c("mat.y", "mat.x", "mat.gpt", "mat.mr"), 
       target = "mat.final", method_names = c("Coder 1", "Coder 2", "GPT", "Majority Rule")),
  list(name = "Rel", df = relf, vars = c("rel.y", "rel.x", "rel.gpt", "rel.mr"), 
       target = "rel.final", method_names = c("Coder 1", "Coder 2", "GPT", "Majority Rule")),
  list(name = "Gm", df = gmf, vars = c("gm.y", "gm.x", "gm.gpt", "gm.mr"), 
       target = "gm.final", method_names = c("Coder 1", "Coder 2", "GPT", "Majority Rule"))
)

# Processing loop with error handling
results <- data.frame()

for (dataset in datasets) {
  tryCatch({
    df <- dataset$df
    vars <- dataset$vars
    target <- dataset$target
    name <- dataset$name
    method_names <- dataset$method_names
    
    # Check if all columns exist
    missing_cols <- setdiff(c(vars, target), names(df))
    if (length(missing_cols) > 0) {
      warning(paste("In dataset", name, "missing columns:", paste(missing_cols, collapse=", ")))
      next
    }
    
    # Get all unique levels
    all_levels <- unique(na.omit(unlist(c(
      lapply(vars, function(var) levels(factor(df[[var]]))),
      levels(factor(df[[target]]))
    ))))
    
    # Convert to factors
    df[[target]] <- factor(df[[target]], levels = all_levels)
    for (var in vars) {
      df[[var]] <- factor(df[[var]], levels = all_levels)
    }
    
    # Calculate metrics
    metrics <- data.frame(
      Dataset = name,
      Method = method_names,
      Precision = sapply(vars, function(var) calculate_metrics(df[[var]], df[[target]])[1]),
      Recall = sapply(vars, function(var) calculate_metrics(df[[var]], df[[target]])[2]),
      F1 = sapply(vars, function(var) calculate_metrics(df[[var]], df[[target]])[3]),
      stringsAsFactors = FALSE
    )
    
    results <- rbind(results, metrics)
    
    # Print results
    cat("\n##", name, "##\n")
    print(kable(metrics[, -1], digits = 3, caption = paste("Metrics for", name)))
    
  }, error = function(e) {
    warning(paste("Error processing dataset", name, ":", e$message))
  })
}

# Final results
if (nrow(results) > 0) {
  cat("\n\n### Combined Results ###\n")
  print(kable(results, digits = 3))
} else {
  cat("\nNo results produced - check for errors in datasets\n")
}

# Function to filter out "Coder 1" from specific datasets
filtered_results <- results %>%
  filter(!(Dataset %in% c("Se", "Sw", "Rel", "Gm") & Method == "Coder 1"))

# Convert data to long format for ggplot
long_results <- filtered_results %>%
  pivot_longer(cols = c("Precision", "Recall", "F1"), 
               names_to = "Metric", 
               values_to = "Value") %>%
  mutate(Metric = factor(Metric, levels = c("Precision", "Recall", "F1")))  # Set correct order

# Plot using the specified color scheme
ggplot(long_results, aes(x = Method, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Dataset, scales = "free_x") +  # Separate charts per dataset
  labs(title = "Precision, Recall, and F1 Score by Dataset and Method",
       x = "Method", y = "Score") +
  scale_fill_manual(values = c("Precision" = "blue", "Recall" = "red", "F1" = "green")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability


