# Load required libraries
library(ggplot2)
library(reshape2)
library(dplyr)
library(ggpubr)

d <- read_excel("Datasets/final data llms.xlsx")

# Create binary columns: men
d <- d %>%
  mutate(Genderm.y = if_else(Gender.y == 1, 1, 0))

d <- d %>%
  mutate(Genderm.x = if_else(Gender.x == 1, 1, 0))

d <- d %>%
  mutate(genm.gpt2 = if_else(gen.gpt2 == 1, 1, 0))

# Create binary columns: women
d <- d %>%
  mutate(Genderw.y = if_else(Gender.y == 2, 1, 0))

d <- d %>%
  mutate(Genderw.x = if_else(Gender.x == 2, 1, 0))

d <- d %>%
  mutate(genw.gpt2 = if_else(gen.gpt2 == 2, 1, 0))

# Create binary columns: both
d <- d %>%
  mutate(Genderb.x = if_else(Gender.x == 3, 1, 0))

d <- d %>%
  mutate(Genderb.y = if_else(Gender.y == 3, 1, 0))

d <- d %>%
  mutate(genb.gpt2 = if_else(gen.gpt2 == 3, 1, 0))

# 1 Confusion Matrix: Coder 1 vs GPT 

# Define comparison pairs
compare_pairs <- list(
  c("asc.y", "asc.gpt"),
  c("dei.y", "dei.gpt"),
  c("Genderm.y", "genm.gpt2"),
  c("Genderw.y", "genw.gpt2"),
  c("Genderb.y", "genb.gpt2"),
  c("gm.y", "gm.gpt"),
  c("time_lapse.y", "lap.gpt"),
  c("leader.y", "lead.gpt"),
  c("mat.y", "mat.gpt"),
  c("rel.y", "rel.gpt"),
  c("sca.y", "sca.gpt"),
  c("se.y", "se.gpt"),
  c("sw.y", "sw.gpt"),
  c("vk.y", "vk.gpt")
)

# Titles for each comparison
title_map <- c(
  "asc.y_vs_asc.gpt" = "Altered States of Consciousness",
  "dei.y_vs_dei.gpt" = "Discomfort, Exhaustion, and Illness",
  "Genderm.y_vs_genm.gpt2" = "Gender Men",
  "Genderw.y_vs_genw.gpt2" = "Gender Women",
  "Genderb.y_vs_genb.gpt2" = "Gender Both",
  "gm.y_vs_gm.gpt" = "Group Membership",
  "time_lapse.y_vs_lap.gpt" = "Time Lapse",
  "leader.y_vs_lead.gpt" = "Leadership Status",
  "mat.y_vs_mat.gpt" = "Sexual Abstinence",
  "rel.y_vs_rel.gpt" = "Religiousity",
  "sca.y_vs_sca.gpt" = "Resource Sacrifice and Charity",
  "se.y_vs_se.gpt" = "Social and Economic Costs",
  "sw.y_vs_sw.gpt" = "Social Withdrawal",
  "vk.y_vs_vk.gpt" = "Visions and Knowledge"
)


# Plot function
plot_confmat <- function(cm, title) {
  df_cm <- as.data.frame.matrix(cm)
  df_cm$Actual <- rownames(df_cm)
  df_long <- melt(df_cm, id.vars = "Actual", variable.name = "Predicted", value.name = "Count")
  
  ggplot(df_long, aes(x = Predicted, y = Actual, fill = Count)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Count), size = 3, color = "black") +
    scale_fill_gradient(low = "#deebf7", high = "#3182bd", na.value = "white") +
    labs(
      title = title,
      x = "GPT",
      y = "Coder",
      fill = "Count"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title.x = element_text(face = "bold"),
      axis.title.y = element_text(face = "bold"),
      plot.title = element_text(size = 10, face = "bold")
    )
}

# Build plots list safely
plots <- list()

for (pair in compare_pairs) {
  key <- paste(pair[1], pair[2], sep = "_vs_")
  if (!(key %in% names(title_map))) {
    message("Skipping unknown title for key: ", key)
    next
  }
  title <- title_map[[key]]
  
  # Skip if title or columns are missing
  if (is.null(title) || !(pair[1] %in% colnames(d)) || !(pair[2] %in% colnames(d))) {
    message("Skipping invalid pair: ", key)
    next
  }
  
  x <- as.character(d[[pair[1]]])
  y <- as.character(d[[pair[2]]])
  
  if (length(x) == length(y) && length(x) > 0) {
    cm <- table(x, y)
    
    # Clean row and column names
    rownames(cm)[is.na(rownames(cm)) | rownames(cm) == ""] <- "Missing"
    colnames(cm)[is.na(colnames(cm)) | colnames(cm) == ""] <- "Missing"
    
    plots[[title]] <- plot_confmat(cm, title)
  }
}

# Combine plots into a panel
panel_plot <- ggarrange(
  plotlist = plots,
  ncol = 2,
  nrow = ceiling(length(plots) / 2),
  common.legend = FALSE
)

# Optional: Save to files
ggsave("Coder1_confusion_matrices_panel.pdf", panel_plot, width = 12, height = 18)
ggsave("Coder1_confusion_matrices_panel.png", panel_plot, width = 12, height = 18, dpi = 300)



# 2 Confusion Matrix: Coder 2 vs GPT 
# Define comparison pairs
compare_pairs2 <- list(
  c("asc.x", "asc.gpt"),
  c("dei.x", "dei.gpt"),
  c("Genderm.x", "genm.gpt2"),
  c("Genderw.x", "genw.gpt2"),
  c("Genderb.x", "genb.gpt2"),
  c("gm.x", "gm.gpt"),
  c("time_lapse.x", "lap.gpt"),
  c("leader.x", "lead.gpt"),
  c("mat.x", "mat.gpt"),
  c("rel.x", "rel.gpt"),
  c("sca.x", "sca.gpt"),
  c("se.x", "se.gpt"),
  c("sw.x", "sw.gpt"),
  c("vk.x", "vk.gpt")
)

# Titles for each comparison
title_map2 <- c(
  "asc.x_vs_asc.gpt" = "Altered States of Consciousness",
  "dei.x_vs_dei.gpt" = "Discomfort, Exhaustion, and Illness",
  "Genderm.x_vs_genm.gpt2" = "Gender Men",
  "Genderw.x_vs_genw.gpt2" = "Gender Women",
  "Genderb.x_vs_genb.gpt2" = "Gender Both",
  "gm.x_vs_gm.gpt" = "Group Membership",
  "time_lapse.x_vs_lap.gpt" = "Time Lapse",
  "leader.x_vs_lead.gpt" = "Leadership Status",
  "mat.x_vs_mat.gpt" = "Sexual Abstinence",
  "rel.x_vs_rel.gpt" = "Religiousity",
  "sca.x_vs_sca.gpt" = "Resource Sacrifice and Charity",
  "se.x_vs_se.gpt" = "Social and Economic Costs",
  "sw.x_vs_sw.gpt" = "Social Withdrawal",
  "vk.x_vs_vk.gpt" = "Visions and Knowledge"
)


# Plot function
plot_confmat2 <- function(cm, title) {
  df_cm <- as.data.frame.matrix(cm)
  df_cm$Actual <- rownames(df_cm)
  df_long <- melt(df_cm, id.vars = "Actual", variable.name = "Predicted", value.name = "Count")
  
  ggplot(df_long, aes(x = Predicted, y = Actual, fill = Count)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Count), size = 3, color = "black") +
    scale_fill_gradient(low = "#deebf7", high = "#3182bd", na.value = "white") +
    labs(
      title = title,
      x = "GPT",
      y = "Coder 2",
      fill = "Count"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title.x = element_text(face = "bold"),
      axis.title.y = element_text(face = "bold"),
      plot.title = element_text(size = 10, face = "bold")
    )
}

# Build plots list safely
plots <- list()

for (pair in compare_pairs2) {
  key <- paste(pair[1], pair[2], sep = "_vs_")
  if (!(key %in% names(title_map2))) {
    message("Skipping unknown title for key: ", key)
    next
  }
  title <- title_map2[[key]]
  
  # Skip if title or columns are missing
  if (is.null(title) || !(pair[1] %in% colnames(d)) || !(pair[2] %in% colnames(d))) {
    message("Skipping invalid pair: ", key)
    next
  }
  
  x <- as.character(d[[pair[1]]])
  y <- as.character(d[[pair[2]]])
  
  if (length(x) == length(y) && length(x) > 0) {
    cm <- table(x, y)
    
    # Clean row and column names
    rownames(cm)[is.na(rownames(cm)) | rownames(cm) == ""] <- "Missing"
    colnames(cm)[is.na(colnames(cm)) | colnames(cm) == ""] <- "Missing"
    
    plots[[title]] <- plot_confmat(cm, title)
  }
}

# Combine plots into a panel
panel_plot2 <- ggarrange(
  plotlist = plots,
  ncol = 2,
  nrow = ceiling(length(plots) / 2),
  common.legend = FALSE
)

# Save panel
ggsave("Coder2_confusion_matrices_panel.pdf", panel_plot2, width = 12, height = 18)
ggsave("Coder2_confusion_matrices_panel.png", panel_plot2, width = 12, height = 18, dpi = 300)
