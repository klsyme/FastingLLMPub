library(readxl)
library(dplyr)
library(caret)

gend <- read_excel("Datasets/disagreement_gen.xlsx")
genf <- gend %>% filter(!is.na(gen.final))
# Convert columns to numeric
genf <- genf %>%
  mutate(across(c(Gender.x, Gender.y, gen.gpt2), as.numeric))


# Create binary columns: men
genf <- genf %>%
  mutate(Genderm.x = if_else(Gender.x == 1, 1, 0))

genf <- genf %>%
  mutate(Genderm.y = if_else(Gender.y == 1, 1, 0))

genf <- genf %>%
  mutate(genm.gpt2 = if_else(gen.gpt2 == 1, 1, 0))

genf <- genf %>%
  mutate(genm.final = if_else(gen.final == 1, 1, 0))

# Create the genm.mr column using majority rule
genf <- genf %>%
  rowwise() %>%  # Perform operations row-by-row
  mutate(
    genm.mr = {
      votes <- c(Genderm.x, Genderm.y, genm.gpt2)
      votes <- votes[!is.na(votes)]  # Remove NAs if present
      if(length(votes) == 0) NA else as.numeric(names(which.max(table(votes))))
    }
  ) %>%
  ungroup()  # Remove rowwise grouping


# Define the columns to evaluate
columns_to_evaluate <- c("Genderm.x", "Genderm.y", "genm.gpt2", "genm.mr")

# Initialize an empty data frame to store results
resultsm <- data.frame(Model = character(),
                      Precision = numeric(),
                      Recall = numeric(),
                      F1_Score = numeric(),
                      stringsAsFactors = FALSE)

# Loop over each column and compute metrics
for (col in columns_to_evaluate) {
  cm <- confusionMatrix(factor(genf[[col]]), factor(genf$genm.final), mode = "everything")
  
  precision <- cm$byClass["Precision"]
  recall <- cm$byClass["Recall"]
  f1_score <- 2 * (precision * recall) / (precision + recall)
  
  resultsm <- rbind(resultsm, data.frame(Model = col, Precision = precision, Recall = recall, F1_Score = f1_score))
}

# Rename row names properly
rownames(resultsm) <- resultsm$Model

# Remove the unnecessary column
resultsm <- resultsm[, -1]

# Print the cleaned results
print(resultsm)

# Convert columns to numeric
genf <- genf %>%
  mutate(across(c(Gender.x, Gender.y, gen.gpt2), as.numeric))

# Create binary columns: women
genf <- genf %>% 
  mutate(Genderw.x = if_else(Gender.x == 2, 1, 0))

genf <- genf %>% 
  mutate(Genderw.y = if_else(Gender.y == 2, 1, 0))

genf <- genf %>% 
  mutate(genw.gpt2 = if_else(gen.gpt2 == 2, 1, 0))

genf <- genf %>% 
  mutate(genw.final = if_else(gen.final == 2, 1, 0))

# Create the genw.mr column using majority rule
genf <- genf %>%
  rowwise() %>%  # Perform operations row-by-row
  mutate(
    genw.mr = {
      votes <- c(Genderw.x, Genderw.y, genw.gpt2)
      votes <- votes[!is.na(votes)]  # Remove NAs if present
      if(length(votes) == 0) NA else as.numeric(names(which.max(table(votes))))
    }
  ) %>%
  ungroup()  # Remove rowwise grouping

# Define the columns to evaluate
columns_to_evaluate <- c("Genderw.x", "Genderw.y", "genw.gpt2", "genw.mr")

# Initialize an empty data frame to store results
resultsw <- data.frame(Model = character(),
                      Precision = numeric(),
                      Recall = numeric(),
                      F1_Score = numeric(),
                      stringsAsFactors = FALSE)

# Loop over each column and compute metrics
for (col in columns_to_evaluate) {
  cm <- confusionMatrix(factor(genf[[col]]), factor(genf$genw.final), mode = "everything")
  
  precision <- cm$byClass["Precision"]
  recall <- cm$byClass["Recall"]
  f1_score <- 2 * (precision * recall) / (precision + recall)
  
  resultsw <- rbind(resultsw, data.frame(Model = col, Precision = precision, Recall = recall, F1_Score = f1_score))
}

# Remove duplicate rows
resultsw <- unique(resultsw)

# Rename row names to match the model names directly
rownames(resultsw) <- resultsw$Model

# Drop the redundant Model column
resultsw <- resultsw[, -1]

# Print the cleaned results
print(resultsw)

# Create binary columns: both
genf <- genf %>%
  mutate(Genderb.x = if_else(Gender.x == 3, 1, 0))

genf <- genf %>%
  mutate(Genderb.y = if_else(Gender.y == 3, 1, 0))

genf <- genf %>%
  mutate(genb.gpt2 = if_else(gen.gpt2 == 3, 1, 0))

genf <- genf %>%
  mutate(genb.final = if_else(gen.final == 3, 1, 0))

# Create the genb.mr column using majority rule
genf <- genf %>%
  rowwise() %>%  # Perform operations row-by-row
  mutate(
    genb.mr = {
      votes <- c(Genderb.x, Genderb.y, genb.gpt2)
      votes <- votes[!is.na(votes)]  # Remove NAs if present
      if(length(votes) == 0) NA else as.numeric(names(which.max(table(votes))))
    }
  ) %>%
  ungroup()  # Remove rowwise grouping


# Define the columns to evaluate
columns_to_evaluate <- c("Genderb.x", "Genderb.y", "genb.gpt2", "genb.mr")

# Initialize an empty data frame to store results
resultsb <- data.frame(Model = character(),
                      Precision = numeric(),
                      Recall = numeric(),
                      F1_Score = numeric(),
                      stringsAsFactors = FALSE)

# Loop over each column and compute metrics
for (col in columns_to_evaluate) {
  cm <- confusionMatrix(factor(genf[[col]]), factor(genf$genb.final), mode = "everything")
  
  precision <- cm$byClass["Precision"]
  recall <- cm$byClass["Recall"]
  f1_score <- 2 * (precision * recall) / (precision + recall)
  
  resultsb <- rbind(resultsb, data.frame(Model = col, Precision = precision, Recall = recall, F1_Score = f1_score))
}

# Rename row names to match the model names directly
rownames(resultsb) <- resultsb$Model

# Drop the redundant Model column
resultsb <- resultsb[, -1]

# Print the cleaned results
print(resultsb)


# Gender:Metric function
calculate_metrics <- function(predicted, actual) {
  predicted <- factor(predicted, levels = levels(actual))
  positive_class <- "1"
  cm <- confusionMatrix(predicted, actual, positive = positive_class)
  precision <- cm$byClass["Pos Pred Value"]
  recall <- cm$byClass["Sensitivity"]
  f1_score <- ifelse(is.na(precision + recall), 0, 2 * (precision * recall) / (precision + recall))
  return(c(precision, recall, f1_score))
}

# Gender datasets setup
gender_datasets <- list(
  list(name = "Man", df = genf, vars = c("Genderm.y", "Genderm.x", "genm.gpt2", "genm.mr"),
       target = "genm.final", method_names = c("Coder 1", "Coder 2", "GPT", "Majority Rule")),
  list(name = "Woman", df = genf, vars = c("Genderw.y", "Genderw.x", "genw.gpt2", "genw.mr"),
       target = "genw.final", method_names = c("Coder 1", "Coder 2", "GPT", "Majority Rule")),
  list(name = "Both", df = genf, vars = c("Genderb.y", "Genderb.x", "genb.gpt2", "genb.mr"),
       target = "genb.final", method_names = c("Coder 1", "Coder 2", "GPT", "Majority Rule"))
)

# Processing loop
gender_results <- data.frame()

for (dataset in gender_datasets) {
  tryCatch({
    df <- dataset$df
    vars <- dataset$vars
    target <- dataset$target
    name <- dataset$name
    method_names <- dataset$method_names
    
    all_levels <- unique(na.omit(unlist(c(
      lapply(vars, function(var) levels(factor(df[[var]]))),
      levels(factor(df[[target]]))
    ))))
    
    df[[target]] <- factor(df[[target]], levels = all_levels)
    for (var in vars) {
      df[[var]] <- factor(df[[var]], levels = all_levels)
    }
    
    metrics <- data.frame(
      Dataset = name,
      Method = method_names,
      Precision = sapply(vars, function(var) calculate_metrics(df[[var]], df[[target]])[1]),
      Recall = sapply(vars, function(var) calculate_metrics(df[[var]], df[[target]])[2]),
      F1 = sapply(vars, function(var) calculate_metrics(df[[var]], df[[target]])[3]),
      stringsAsFactors = FALSE
    )
    
    gender_results <- rbind(gender_results, metrics)
    
    cat("\n##", name, "##\n")
    print(kable(metrics[, -1], digits = 3, caption = paste("Metrics for", name)))
    
  }, error = function(e) {
    warning(paste("Error processing dataset", name, ":", e$message))
  })
}

# Plotting
long_gender_results <- gender_results %>%
  pivot_longer(cols = c("Precision", "Recall", "F1"),
               names_to = "Metric", values_to = "Value") %>%
  mutate(Metric = factor(Metric, levels = c("Precision", "Recall", "F1")))

gender_plot <- ggplot(long_gender_results, aes(x = Method, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Dataset, scales = "free_x") +
  labs(title = "Precision, Recall, and F1 Score by Gender and Method",
       x = "Method", y = "Score") +
  scale_fill_manual(values = c("Precision" = "blue", "Recall" = "red", "F1" = "green")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 12)
  )


# Save the plot
ggsave(
  filename = "/Users/kristensyme/Desktop/R Projects/Fasting/Final Agreement Analysis/gender_metrics_plot.png",
  plot = gender_plot,
  width = 10,
  height = 4,
  dpi = 300
)



