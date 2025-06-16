# Load necessary libraries
library(readxl)  # for reading Excel files
library(dplyr)   # for data manipulation
library(ggplot2)
library(maps)
library(ggrepel)

# Read in the datasets
cultures_cleaned <- read_excel("Datasets/cultures_cleaned.xlsx")
cultures_with_coords <- read_excel("Datasets/Cultures with Coordinates.xlsx")

# Select only the columns you need
coords_selected <- cultures_with_coords %>%
  select(CULTURE, longitude, latitude, PSF)

# Perform the left join
cultures_joined_raw <- cultures_cleaned %>%
  left_join(coords_selected, by = "CULTURE")

# Convert longitude and latitude to numeric
cultures_joined <- cultures_joined_raw %>%
  mutate(
    longitude = as.numeric(longitude),
    latitude = as.numeric(latitude)
  )

# Summarize paragraph counts per culture
cultures_summary <- cultures_joined %>%
  group_by(CULTURE, SUBSISTENCE_TYPE, latitude, longitude) %>%
  summarise(
    paragraph_count = n(),
    .groups = "drop"
  )

# Get base world map
world_map <- map_data("world")

ggplot() +
  # Base map
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
               fill = "gray95", color = "gray80") +
  
  # Points for cultures
  geom_point(data = cultures_summary,
             aes(x = longitude, y = latitude,
                 shape = SUBSISTENCE_TYPE,
                 size = paragraph_count),
             color = "darkblue", alpha = 0.7) +
  
  # Culture labels
  geom_text_repel(data = cultures_summary,
                  aes(x = longitude, y = latitude, label = CULTURE),
                  size = 3, max.overlaps = 20) +
  
  # Size scaling
  scale_size_continuous(range = c(2, 8)) +
  
  # Theme and labels
  theme_minimal() +
  labs(
    title = "Map of Cultures by Subsistence Type",
    shape = "Subsistence Type",
    size = "Number of Paragraphs"
  ) +
  theme(legend.position = "right")

