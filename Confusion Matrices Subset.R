# Load required libraries
library(ggplot2)
library(reshape2)
library(dplyr)
library(ggpubr)

# Data available in Datasets in allsubset_fasting.xlsx

# 1 Confusion Matrix: human consensus vs round 1 GPT subset

# Define comparison pairs
compare_pairs <- list(
  c("asc", "asc2"),
  c("dei", "dei1"),
  c("genderm.r", "genm1"),
  c("genderw.r", "genw1"),
  c("genderb.r", "genb1"),
  c("gm", "gm1"),
  c("lap", "lap1"),
  c("lead", "lead1"),
  c("mat", "mat1"),
  c("rel", "rel2"),
  c("sca", "sca1"),
  c("se", "se1"),
  c("sw", "sw2"),
  c("vk", "vk1")
)

# Titles for each comparison
title_map <- c(
  "asc_vs_asc2" = "Altered States of Consciousness",
  "dei_vs_dei1" = "Discomfort, Exhaustion, and Illness",
  "genderm.r_vs_genm1" = "Gender Men",
  "genderw.r_vs_genw1" = "Gender Women",
  "genderb.r_vs_genb1" = "Gender Both",
  "gm_vs_gm1" = "Group Membership",
  "lap_vs_lap1" = "Time Lapse",
  "lead_vs_lead1" = "Leadership Status",
  "mat_vs_mat1" = "Sexual Abstinence",
  "rel_vs_rel2" = "Religiousity",
  "sca_vs_sca1" = "Resource Sacrifice and Charity",
  "se_vs_se1" = "Social and Economic Costs",
  "sw_vs_sw2" = "Social Withdrawal",
  "vk_vs_vk1" = "Visions and Knowledge"
)

# Plot function with light green scale for count
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
      x = "GPT Round 1",
      y = "Human Consensus",
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

# Create plots
plots <- list()
for (pair in compare_pairs) {
  key <- paste(pair[1], pair[2], sep = "_vs_")
  title <- title_map[[key]]
  
  x <- as.character(allsubset[[pair[1]]])
  y <- as.character(allsubset[[pair[2]]])
  
  if (length(x) == length(y) && length(x) > 0) {
    cm <- table(x, y)
    
    # Sanity check: remove row/col names that are NA or blank
    rownames(cm)[is.na(rownames(cm)) | rownames(cm) == ""] <- "Missing"
    colnames(cm)[is.na(colnames(cm)) | colnames(cm) == ""] <- "Missing"
    
    plots[[title]] <- plot_confmat(cm, title)
  }
}

# Combine plots in a single panel
panel_plot <- ggarrange(
  plotlist = plots,
  ncol = 2, nrow = 7,
  common.legend = TRUE,
  legend = "right"
)


# Save output
ggsave("subset1_confusion_matrices_panel.png", panel_plot, width = 14, height = 20, dpi = 300)
ggsave("subset1_confusion_matrices_panel.pdf", panel_plot, width = 14, height = 20)



# 2 Confusion Matrix: human consensus vs round 2 GPT subset

# Define comparison pairs
compare_pairs <- list(
  c("asc", "asc3"),
  c("dei", "dei2"),
  c("genderm.r", "genm2"),
  c("genderw.r", "genw2"),
  c("genderb.r", "genb2"),
  c("gm", "gm2"),
  c("lap", "lap2"),
  c("lead", "lead2"),
  c("mat", "mat2"),
  c("rel", "rel3"),
  c("sca", "sca2"),
  c("se", "se2"),
  c("sw", "sw3"),
  c("vk", "vk2")
)

title_map <- c(
  "asc_vs_asc3" = "Altered States of Consciousness",
  "dei_vs_dei2" = "Discomfort, Exhaustion, and Illness",
  "genderm.r_vs_genm2" = "Gender Men",
  "genderw.r_vs_genw2" = "Gender Women",
  "genderb.r_vs_genb2" = "Gender Both",
  "gm_vs_gm2" = "Group Membership",
  "lap_vs_lap2" = "Time Lapse",
  "lead_vs_lead2" = "Leadership Status",
  "mat_vs_mat2" = "Sexual Abstinence",
  "rel_vs_rel3" = "Religiousity",
  "sca_vs_sca2" = "Resource Sacrifice and Charity",
  "se_vs_se2" = "Social and Economic Costs",
  "sw_vs_sw3" = "Social Withdrawal",
  "vk_vs_vk2" = "Visions and Knowledge"
)


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
      x = "GPT Round 2",
      y = "Human Consensus",
      fill = "Count"
    ) +
    guides(fill = guide_colorbar(barwidth = 0.5, barheight = 5)) +  # Force legend
    theme_minimal(base_size = 10) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title.x = element_text(face = "bold"),
      axis.title.y = element_text(face = "bold"),
      plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
      legend.position = "right"
    )
}


# Generate and store individual plots
plots <- list()
for (i in seq_along(compare_pairs)) {
  pair <- compare_pairs[[i]]
  key <- paste(pair[1], pair[2], sep = "_vs_")
  
  title <- title_map[[key]]
  
  x <- as.character(allsubset[[pair[1]]])
  y <- as.character(allsubset[[pair[2]]])
  
  if (length(x) == length(y) && length(x) > 0) {
    cm <- table(x, y)
    
    # Clean up row/col names
    rownames(cm)[is.na(rownames(cm)) | rownames(cm) == ""] <- "Missing"
    colnames(cm)[is.na(colnames(cm)) | colnames(cm) == ""] <- "Missing"
    
    plots[[title]] <- plot_confmat(cm, title)
  }
}


# Arrange into a single panel
panel_plot <- ggarrange(
  plotlist = plots,
  ncol = 2, nrow = 7,
  common.legend = TRUE,
  legend = "right"
)

# Save as image and PDF
ggsave("subset2_confusion_matrices_panel.png", panel_plot, width = 14, height = 20, dpi = 300)
ggsave("subset2_confusion_matrices_panel.pdf", panel_plot, width = 14, height = 20)


# 3 Confusion Matrix: Human-GPT resolved vs round 1 GPT subset

# Define comparison pairs
compare_pairs <- list(
  c("asc_final", "asc2"),
  c("dei_final", "dei1"),
  c("genderm.final", "genm1"),
  c("genderw.final", "genw1"),
  c("genderb.final", "genb1"),
  c("gm_final", "gm1"),
  c("lap_final", "lap1"),
  c("lead_final", "lead1"),
  c("mat_final", "mat1"),
  c("rel_final", "rel2"),
  c("sca_final", "sca1"),
  c("se_final", "se1"),
  c("sw_final", "sw2"),
  c("vk_final", "vk1")
)

# Define the updated comparison pairs
compare_pairs <- list(
  c("asc_final", "asc2"),
  c("dei_final", "dei1"),
  c("genderm.final", "genm1"),
  c("genderw.final", "genw1"),
  c("genderb.final", "genb1"),
  c("gm_final", "gm1"),
  c("lap_final", "lap1"),
  c("lead_final", "lead1"),
  c("mat_final", "mat1"),
  c("rel_final", "rel2"),
  c("sca_final", "sca1"),
  c("se_final", "se1"),
  c("sw_final", "sw2"),
  c("vk_final", "vk1")
)

# Titles for each comparison
title_map <- c(
  "asc_final_vs_asc2" = "Altered States of Consciousness",
  "dei_final_vs_dei1" = "Discomfort, Exhaustion, and Illness",
  "genderm.final_vs_genm1" = "Gender Men",
  "genderw.final_vs_genw1" = "Gender Women",
  "genderb.final_vs_genb1" = "Gender Both",
  "gm_final_vs_gm1" = "Group Membership",
  "lap_final_vs_lap1" = "Time Lapse",
  "lead_final_vs_lead1" = "Leadership Status",
  "mat_final_vs_mat1" = "Sexual Abstinence",
  "rel_final_vs_rel2" = "Religiousity",
  "sca_final_vs_sca1" = "Resource Sacrifice and Charity",
  "se_final_vs_se1" = "Social and Economic Costs",
  "sw_final_vs_sw2" = "Social Withdrawal",
  "vk_final_vs_vk1" = "Visions and Knowledge"
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
      x = "GPT Round 1",
      y = "Human-AI Resolved",
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

# Generate and store individual plots
plots <- list()
for (pair in compare_pairs) {
  key <- paste(pair[1], pair[2], sep = "_vs_")
  title <- title_map[[key]]
  
  x <- as.character(allsubset[[pair[1]]])
  y <- as.character(allsubset[[pair[2]]])
  
  if (length(x) == length(y) && length(x) > 0) {
    cm <- table(x, y)
    
    # Clean up row/col names
    rownames(cm)[is.na(rownames(cm)) | rownames(cm) == ""] <- "Missing"
    colnames(cm)[is.na(colnames(cm)) | colnames(cm) == ""] <- "Missing"
    
    plots[[title]] <- plot_confmat(cm, title)
  }
}

# Combine all plots into one panel
panel_plot <- ggarrange(
  plotlist = plots,
  ncol = 2, nrow = 7,
  common.legend = TRUE,
  legend = "right"
)

# Save the panel as image and PDF
ggsave("subset3_confusion_matrices_panel.png", panel_plot, width = 14, height = 20, dpi = 300)
ggsave("subset3_confusion_matrices_panel.pdf", panel_plot, width = 14, height = 20)


# 4 Confusion Matrix: Human-AI resolved vs round 2 GPT subset

# Define comparison pairs
compare_pairs <- list(
  c("asc_final", "asc3"),
  c("dei_final", "dei2"),
  c("genderm.final", "genm2"),
  c("genderw.final", "genw2"),
  c("genderb.final", "genb2"),
  c("gm_final", "gm2"),
  c("lap_final", "lap2"),
  c("lead_final", "lead2"),
  c("mat_final", "mat2"),
  c("rel_final", "rel3"),
  c("sca_final", "sca2"),
  c("se_final", "se2"),
  c("sw_final", "sw3"),
  c("vk_final", "vk2")
)

# Titles for each comparison
title_map <- c(
  "asc_final_vs_asc3" = "Altered States of Consciousness",
  "dei_final_vs_dei2" = "Discomfort, Exhaustion, and Illness",
  "genderm.final_vs_genm2" = "Gender Men",
  "genderw.final_vs_genw2" = "Gender Women",
  "genderb.final_vs_genb2" = "Gender Both",
  "gm_final_vs_gm2" = "Group Membership",
  "lap_final_vs_lap2" = "Time Lapse",
  "lead_final_vs_lead2" = "Leadership Status",
  "mat_final_vs_mat2" = "Sexual Abstinence",
  "rel_final_vs_rel3" = "Religiousity",
  "sca_final_vs_sca2" = "Resource Sacrifice and Charity",
  "se_final_vs_se2" = "Social and Economic Costs",
  "sw_final_vs_sw3" = "Social Withdrawal",
  "vk_final_vs_vk2" = "Visions and Knowledge"
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
      x = "GPT Round 2",
      y = "Human-AI Resolved",
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

# Generate and store plots
plots <- list()
for (pair in compare_pairs) {
  key <- paste(pair[1], pair[2], sep = "_vs_")
  title <- title_map[[key]]
  
  x <- as.character(allsubset[[pair[1]]])
  y <- as.character(allsubset[[pair[2]]])
  
  if (length(x) == length(y) && length(x) > 0) {
    cm <- table(x, y)
    
    rownames(cm)[is.na(rownames(cm)) | rownames(cm) == ""] <- "Missing"
    colnames(cm)[is.na(colnames(cm)) | colnames(cm) == ""] <- "Missing"
    
    plots[[title]] <- plot_confmat(cm, title)
  }
}

# Combine into a single panel
panel_plot <- ggarrange(
  plotlist = plots,
  ncol = 2, nrow = 7,
  common.legend = TRUE,
  legend = "right"
)

# Save output
ggsave("subset4_confusion_matrices_panel.png", panel_plot, width = 14, height = 20, dpi = 300)
ggsave("subset4_confusion_matrices_panel.pdf", panel_plot, width = 14, height = 20)
