source("GPT Run.R")

library(irr)
library(irrCAC)


## This creates a data frame where each row has one paragraph
paragraphs <- 
  read_excel("Subset Codes Fasting rename.xlsx") |> 
  mutate(
    Excerpt.x = str_remove_all(Excerpt.x, "\\{\\{\\d+\\}\\}"),    
    Excerpt.x = str_remove_all(Excerpt.x, "[^\x1F-\x7F]+")      
  ) 

# Prompt for time lapse
prompt <- "I will give you a paragraph of text on fasting under the heading Excerpt.x. I would like you to annotate the text as follows based on whether the description of fasting is said to have occurred in the past and is no longer practiced. Code a numerical 1 if the text indicates that fasting as described in the text occurred in the past and was no longer practiced. Code a numerical 0 if there is no mention if the fasting as described in the text occurred in the past, or if the text indicates that the fasting as described in the text is still practiced. For example, if the text uses phrases like ‘in the past’ to describe fasting or references or alludes to ‘pre-colonial times’, then code 1. Please do not provide any other information in your response besides the 1 or the 0. Here is the paragraph:"

# Generate time lapse
#GPTlapse <- paragraphs
#GPTlapse$lap1 <- map_chr(paste(prompt, GPTlapse$Excerpt.x), hey_chatGPT)

# lap1 irr with resolved codes
lap1irr <- GPTlapse %>% select(time_lapse.r, lap1)
agree(lap1irr)
gwet.ac1.raw(lap1irr, weights = "unweighted")
kappa2(lap1irr[, c("time_lapse.r", "lap1")], weight = "equal")

# Create matrix to compare resolved codes
lap1irrx <- GPTlapse %>% select(Excerpt.x, time_lapse.r, lap1)

# Filter the rows where they disagree
diffrows_lap1 <- lap1irrx %>%
  filter(time_lapse.r != lap1) %>%
  select(Excerpt.x, time_lapse.r, lap1)

# Generate lap2
#GPTlapse$lap2 <- map_chr(paste(prompt, GPTlapse$Excerpt.x), hey_chatGPT)

# lap1 irr with resolved codes
lap2irr <- GPTlapse %>% select(lap1, lap2)
agree(lap2irr)
gwet.ac1.raw(lap2irr, weights = "unweighted")
kappa2(lap2irr[, c("lap1", "lap2")], weight = "equal")

# Create matrix with all columns
lap2irrx <- GPTlapse %>% select(Excerpt.x, lap1, lap2)

# Filter the rows where they disagree
diffrows_lap2 <- lap2irrx %>%
  filter(lap1 != lap2) %>%
  select(Excerpt.x, lap1, lap2)

lap2irrx2 <- GPTlapse %>% select(Excerpt.x, time_lapse.r, lap1, lap2)

## Exporting files based on this session 
#write.xlsx(diffrows_lap1, file = "/Users/kristensyme/Desktop/R Projects/Fasting/Fasting R scripts/Fasting/lap1 different rows KSGPT.xlsx", rowNames = FALSE)
#write.xlsx(diffrows_lap2, file = "/Users/kristensyme/Desktop/R Projects/Fasting/Fasting R scripts/Fasting/lap2 different rows KSGPT.xlsx", rowNames = FALSE)
#write.xlsx(lap2irrx2, file = "/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/lap2irrx2.xlsx", rowNames = FALSE)

library(readxl)
lap <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/lap2irrx2.xlsx")

# lap1 irr with resolved codes
lap1irr <- lap %>% select(time_lapse.r, lap1)
agree(lap1irr)
gwet.ac1.raw(lap1irr, weights = "unweighted")
kappa2(lap1irr[, c("time_lapse.r", "lap1")], weight = "equal")


# lap2 irr with itself
lap2irr <- lap %>% select(lap1, lap2)
agree(lap2irr)
gwet.ac1.raw(lap2irr, weights = "unweighted")
kappa2(lap2irr[, c("lap1", "lap2")], weight = "equal")


