source("GPT Run.R")

library(irr)
library(irrCAC)
library(dplyr)
library(openxlsx)

## This creates a data frame where each row has one paragraph
paragraphs <- 
  read_excel("Subset Codes Fasting rename.xlsx") |> 
  mutate(
    Excerpt.x = str_remove_all(Excerpt.x, "\\{\\{\\d+\\}\\}"),    
    Excerpt.x = str_remove_all(Excerpt.x, "[^\x1F-\x7F]+")      
  )

# Prompt for Leader1
prompt <- "I will give you a paragraph of text on fasting under the heading Excerpt.x. I would like you to annotate the texts as follows based on the leadership status of the faster or fasters. Code a numerical 1 if the paragraph explicitly mentions that someone who is fasting is a leader, or is about to become a leader including chiefs, war leaders, shamans, priests, monks. Note that these texts often use a non-English term for the leader, so use contextual cues suggesting that the person hold power and authority in political or religious settings from the text to determine if one of the fasters is a leader. Code a numerical 0 if there is no mention that one of the fasters is a leader. For example, if a text mentions a leader, such as a priest, commanding someone else to fast, but the leader is not fasting, then code 0. Please do not provide any other information in your response besides the 1 or the 0. Here is the paragraph:"

# Generate Leader1
gptLeader <- paragraphs
#gptLeader$lead1 <- map_chr(paste(prompt, gptLeader$Excerpt.x), hey_chatGPT)

# lead1 irr with resolved codes
lead1irr <- gptLeader %>% select(leader.r, lead1)
agree(lead1irr)
gwet.ac1.raw(lead1irr, weights = "unweighted")
kappa2(lead1irr[, c("leader.r", "lead1")], weight = "equal")

# Create matrix including Excerpt.x to compare diffs
lead1irrx <- gptLeader %>% select(Excerpt.x, leader.r, lead1)

# Filter the rows where lead and lead1 are different and select the desired columns
diffrows_lead1 <- lead1irrx %>%
  filter(leader.r != lead1) %>%
  select(Excerpt.x, leader.r, lead1)

# Generate Leader2
#gptLeader$lead2 <- map_chr(paste(prompt, gptLeader$Excerpt.x), hey_chatGPT)

# lead2 irr with itself
lead2irr <- gptLeader %>% select(lead1, lead2)
agree(lead2irr)
gwet.ac1.raw(lead2irr, weights = "unweighted")
kappa2(lead2irr[, c("lead1", "lead2")], weight = "equal")

#Data frame for diff rows 2
lead2irrx <- gptLeader %>% select(Excerpt.x, lead1, lead2)

# Filter the rows where lead and lead1 are different and select the desired columns
diffrows_lead2 <- lead2irrx %>%
  filter(lead1 != lead2) %>%
  select(Excerpt.x, lead1, lead2)

lead2irrx2 <- gptLeader %>% select(Excerpt.x, leader.r, lead1, lead2)


## Exporting files based on this session 
#write.xlsx(diffrows_lead1, file = "/Users/kristensyme/Desktop/R Projects/Fasting/Fasting R scripts/Fasting/lead1 different rows KSGPT.xlsx", rowNames = FALSE)
#write.xlsx(diffrows_lead2, file = "/Users/kristensyme/Desktop/R Projects/Fasting/Fasting R scripts/Fasting/lead2 different rows KSGPT.xlsx", rowNames = FALSE)
#write.xlsx(lead2irrx2, file = "/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/lead2irrx2.xlsx", rowNames = FALSE)

library(readxl)
lead <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/lead2irrx2.xlsx")

# lead1 irr with resolved codes
lead1irr <- lead %>% select(leader.r, lead1)
agree(lead1irr)
gwet.ac1.raw(lead1irr, weights = "unweighted")
kappa2(lead1irr[, c("leader.r", "lead1")], weight = "equal")

# lead2 irr with itself
lead2irr <- lead %>% select(lead1, lead2)
agree(lead2irr)
gwet.ac1.raw(lead2irr, weights = "unweighted")
kappa2(lead2irr[, c("lead1", "lead2")], weight = "equal")
