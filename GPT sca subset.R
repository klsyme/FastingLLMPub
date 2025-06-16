source("GPT Run.R")

library(irr)
library(irrCAC)


## This creates a data frame where each row has one paragraph
paragraphs <- 
  read_excel("Costs Subset Unlisted.xlsx") |> 
  mutate(
    Excerpt.x = str_remove_all(Excerpt.x, "\\{\\{\\d+\\}\\}"),    
    Excerpt.x = str_remove_all(Excerpt.x, "[^\x1F-\x7F]+")      
  ) 

# Prompt for sca1
#prompt <- "I will give you a paragraph of text on fasting under the heading Excerpt.x. I would like you to annotate the texts as follows based on the type of cost a faster or fasters are experiencing: Code a numerical 1 if a person or people make a sacrifice of a slave or animal, or if they pay fees during the fast or around the fast. Code a numerical 0 if there is no mention a person or people make a sacrifice of a slave or animal, or if they pay fees during the fast or around the fast. Please do not provide any other information in your response besides the 1 or the 0. Here is the paragraph:"

# Generate sca1
#gptCosts <- paragraphs
# gptCosts$sca1 <- map_chr(paste(prompt, gptCosts$Excerpt.x), hey_chatGPT)

# sca1 irr with resolved codes
sca1irr <- gptCosts %>% select(sca, sca1)
agree(sca1irr)
gwet.ac1.raw(sca1irr$sca, sca1irr$sca1, weights = "unweighted")
kappa2(sca1irr[, c("sca", "sca1")], weight = "equal")

# Create matrix including Excerpt.x to compare diffs
sca1irrx <- gptCosts %>% select(Excerpt.x, sca, sca1)

# Filter the rows where sca and sca1 are different and select the desired columns
diffrows_sca1 <- sca1irrx %>%
  filter(sca != sca1) %>%
  select(Excerpt.x, sca, sca1)

# Prompt for sca2
# prompt <- "I will give you a paragraph of text on fasting under the heading Excerpt.x. I would like you to annotate the texts as follows based on the type of cost a faster or fasters are experiencing: Code a numerical 1 if a person or people make a sacrifice of a slave or animal, or if they pay fees during the fast or around the fast. Code a numerical 0 if there is no mention a person or people make a sacrifice of a slave or animal, or if they pay fees during the fast or around the fast. Please do not provide any other information in your response besides the 1 or the 0. Here is the paragraph:"

# Generate sca2
gptCosts <- paragraphs
# gptCosts$sca2 <- map_chr(paste(prompt, gptCosts$Excerpt.x), hey_chatGPT)

# sca2 irr with itself
sca2irr <- gptCosts %>% select(sca1, sca2)
agree(sca2irr)
gwet.ac1.raw(sca2irr$sca1, sca2irr$sca2, weights = "unweighted")
kappa2(sca2irr[, c("sca1", "sca2")], weight = "equal")

# Create matrix including Excerpt.x to compare diffs on two rounds of GPT
sca2irrx <- gptCosts %>% select(Excerpt.x, sca, sca2)
# Add the variable sca1 from sca1irrx to the existing dataframe sca2irrx
sca2irrx <- sca2irrx %>%
  mutate(sca1 = sca1irr$sca1)

# Filter the rows where sca and sca1 are different and select the desired columns
diffrows_sca2 <- sca2irrx %>%
  filter(sca != sca2) %>%
  select(Excerpt.x, sca, sca2)

## Exporting files based on this session 
write.xlsx(diffrows_sca1, file = "/Users/kristensyme/Desktop/R Projects/Fasting/Fasting R scripts/Fasting/sca1 different rows KSGPT.xlsx", rowNames = FALSE)
write.xlsx(diffrows_sca2, file = "/Users/kristensyme/Desktop/R Projects/Fasting/Fasting R scripts/Fasting/sca2 different rows GPTrerun.xlsx", rowNames = FALSE)
# write.xlsx(sca2irrx, file = "/Users/kristensyme/Desktop/R Projects/Fasting/Fasting R scripts/Fasting/sca2IRR.xlsx", rowNames = FALSE)

library(readxl)
sca <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/sca2IRR.xlsx")
