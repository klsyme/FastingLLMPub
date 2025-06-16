# Load required packages
library(readxl)
library(tidyverse)
library(irr)
library(irrCAC)
library(gt)

df <- read_excel("Datasets/final data llms.xlsx")

# --- Gwet's AC1 computation ---
compute_gwet_ac1 <- function(r1, r2) {
  d_temp <- data.frame(r1 = as.factor(r1), r2 = as.factor(r2))
  d_temp <- d_temp[complete.cases(d_temp), ]
  if (nrow(d_temp) < 2) return(NA_real_)
  
  # Align factor levels across both raters
  levels_union <- union(levels(d_temp$r1), levels(d_temp$r2))
  d_temp$r1 <- factor(d_temp$r1, levels = levels_union)
  d_temp$r2 <- factor(d_temp$r2, levels = levels_union)
  
  # Use matrix input format expected by gwet.ac1.raw()
  rating_matrix <- as.matrix(d_temp)
  
  result <- irrCAC::gwet.ac1.raw(rating_matrix)
  return(result$est$coeff.val)
}

# --- IRR Metrics Function ---
compute_irr_metrics <- function(data, var1, var2) {
  d_temp <- data %>%
    select(all_of(c(var1, var2))) %>%
    drop_na() %>%
    mutate(across(everything(), as.factor))
  
  if (nrow(d_temp) < 2) return(c(NA, NA, NA))
  
  agreement <- mean(d_temp[[1]] == d_temp[[2]]) * 100
  
  kappa_val <- tryCatch({
    kappa2(d_temp, weight = "unweighted")$value
  }, error = function(e) NA_real_)
  
  gwet_val <- tryCatch({
    compute_gwet_ac1(d_temp[[1]], d_temp[[2]])
  }, error = function(e) {
    message("Error in Gwet AC1: ", e$message)
    return(NA_real_)
  })
  
  return(c(agreement, gwet_val, kappa_val))
}

# --- Define variable groups ---
var_groups <- list(
  gen = c("Gender.x", "gen.gpt2", "Gender.y"),
  lap = c("time_lapse.x", "lap.gpt", "time_lapse.y"),
  lead = c("leader.x", "lead.gpt", "leader.y"),
  asc = c("asc.x", "asc.gpt", "asc.y"),
  vk = c("vk.x", "vk.gpt", "vk.y"),
  dei = c("dei.x", "dei.gpt", "dei.y"),
  se = c("se.x", "se.gpt"),
  sca = c("sca.x", "sca.gpt", "sca.y"),
  sw = c("sw.x", "sw.gpt"),
  mat = c("mat.x", "mat.gpt", "mat.y"),
  rel = c("rel.x", "rel.gpt", "rel.y"),
  gm = c("gm.x", "gm.gpt", "gm.y")
)

# --- Human-readable variable labels ---
var_labels <- list(
  gen = "Gender",
  lap = "Time lapse",
  lead = "Leader",
  asc = "Altered state",
  vk = "Visions/knowledge",
  dei = "Discomfort/illness",
  se = "Social/economic",
  sca = "Sacrifice/charity",
  sw = "Social withdrawal",
  mat = "Sexual abstinence",
  rel = "Religiousity",
  gm = "Group membership"
)

# --- Results initialization ---
results <- tibble(
  Variable = names(var_groups),
  Pct_C1C2 = NA_real_, Gwet_C1C2 = NA_real_, Kappa_C1C2 = NA_real_,
  Pct_GPTC1 = NA_real_, Gwet_GPTC1 = NA_real_, Kappa_GPTC1 = NA_real_,
  Pct_GPTC2 = NA_real_, Gwet_GPTC2 = NA_real_, Kappa_GPTC2 = NA_real_
)

# --- Main loop to calculate metrics ---
for (i in seq_along(var_groups)) {
  group <- var_groups[[i]]
  
  # Coder1 vs Coder2
  if (length(group) >= 2) {
    res <- compute_irr_metrics(df, group[1], group[2])
    results[i, c("Pct_GPTC2", "Gwet_GPTC2", "Kappa_GPTC2")] <- as.list(res)
  }
  
  # GPT vs Coder1
  if (length(group) >= 3) {
    res <- compute_irr_metrics(df, group[3], group[2])
    results[i, c("Pct_GPTC1", "Gwet_GPTC1", "Kappa_GPTC1")] <- as.list(res)
  }
  
  # GPT vs Coder2
  if (length(group) >= 3) {
    res <- compute_irr_metrics(df, group[3], group[1])
    results[i, c("Pct_C1C2", "Gwet_C1C2", "Kappa_C1C2")] <- as.list(res)
  }
}

# Create the gt table and assign it to an object
final_gt_table <- results %>%
  mutate(Variable = recode(Variable, !!!var_labels)) %>%
  rename(
    `Pct Agreement\n(C1 vs C2)` = Pct_C1C2,
    `Gwet AC1\n(C1 vs C2)` = Gwet_C1C2,
    `Kappa\n(C1 vs C2)` = Kappa_C1C2,
    `Pct Agreement\n(GPT vs C1)` = Pct_GPTC1,
    `Gwet AC1\n(GPT vs C1)` = Gwet_GPTC1,
    `Kappa\n(GPT vs C1)` = Kappa_GPTC1,
    `Pct Agreement\n(GPT vs C2)` = Pct_GPTC2,
    `Gwet AC1\n(GPT vs C2)` = Gwet_GPTC2,
    `Kappa\n(GPT vs C2)` = Kappa_GPTC2
  ) %>%
  gt() %>%  fmt_number(
    columns = where(is.numeric),
    decimals = 2
  ) %>%
  data_color(
    columns = matches("^Pct"),
    fn = scales::col_numeric(
      palette = c("#f94144", "#f9c74f", "#43aa8b"),
      domain = c(0, 100),
      na.color = "#4d4d4d"
    )
  ) %>%
  data_color(
    columns = matches("^Kappa"),
    fn = scales::col_bin(
      palette = c("#b2182b", "#f94144", "#f9844a",
                  "#f9c74f", "#90be6d", "#43aa8b"),
      bins = c(-Inf, 0.00, 0.20, 0.40, 0.60, 0.80, 1.00),
      na.color = "#4d4d4d"
    )
  ) %>%
  data_color(
    columns = matches("^Gwet"),
    fn = scales::col_bin(
      palette = c("#b2182b", "#f94144", "#f9844a",
                  "#f9c74f", "#90be6d", "#43aa8b"),
      bins = c(-Inf, 0.00, 0.20, 0.40, 0.60, 0.80, 1.00),
      na.color = "#4d4d4d"
    )
  ) %>%
  tab_source_note(
    source_note = md(
      "**Legend**  
      - Percent Agreement: Red (low agreement) to Green (high agreement)  
      - Gwet's AC1 & Kappa:  
        * < 0.00: Poor  
        * 0.00–0.20: Slight  
        * 0.21–0.40: Fair  
        * 0.41–0.60: Moderate  
        * 0.61–0.80: Substantial  
        * 0.81–1.00: Almost perfect"
    )
  )

# Save the gt table to PNG
# gtsave(final_gt_table, "final_table.png")

