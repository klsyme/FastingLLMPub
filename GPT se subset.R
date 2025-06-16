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

# Prompt for se1
prompt <- "Code a numerical 1 if the faster or fasters experience social or economic costs directly attributed to fasting; for example, if fasters are vulnerable to attack, face loss of business, if fasting too much is perceived as selfish, if the faster is blamed if a hunt is unsuccessful, or if not fasting creates conflict. Code a numerical 0 if there is no mention of any of the above social or economic costs attributed directly to fasting. For example, if the text describes a conflict that is not attributed to fasting then code 0. Please use only the above criteria and avoid inferring social or economic costs. Please do not provide any other information in your response besides the 1 or the 0. Here is the paragraph:"

# Generate dei1
gptCosts <- paragraphs
gptCosts$se1 <- map_chr(paste(prompt, gptCosts$Excerpt.x), hey_chatGPT)

# se 1 irr with resolved codes
se1irr <- gptCosts %>% select(se, se1)
agree(se1irr)
gwet.ac1.raw(se1irr, weights = "unweighted")
kappa2(se1irr[, c("se", "se1")], weight = "equal")

# Create matrix including Excerpt.x to compare diffs
se1irrx <- gptCosts %>% select(Excerpt.x, se, se1)

# Filter the rows where dei and dei1 are different and select the desired columns
diffrows_se1 <- se1irrx %>%
  filter(se != se1) %>%
  select(Excerpt.x, se, se1)

# Prompt for se2--same as se1
# prompt <- "I will give you a paragraph of text on fasting under the heading Excerpt.x. I would like you to annotate the texts as follows based on the type of cost a faster or fasters are experiencing: Code a numerical 1 if the faster or fasters experience social or economic costs directly attributed to fasting; for example, if fasters are vulnerable to attack, face loss of business, if fasting too much is perceived as selfish, if the faster is blamed if a hunt is unsuccessful, or if not fasting creates conflict. Code a numerical 0 if there is no mention of any of the above social or economic costs attributed directly to fasting. For example, if the text describes a conflict that is not attributed to fasting then code 0. Please use only the above criteria and avoid inferring social or economic costs. Please do not provide any other information in your response besides the 1 or the 0. Here is the paragraph:"

# Generate se2
gptCosts <- paragraphs
gptCosts$se2 <- map_chr(paste(prompt, gptCosts$Excerpt.x), hey_chatGPT)

# se2 irr with itself
# Extract the desired columns from each dataframe
se2 <- gptCosts %>% select(se2)
se1 <- se1irr %>% select(se1)
# Combine the extracted columns into a new dataframe
se2irr <- data.frame(se1 = se1$se1, se2 = se2$se2)
# Now check IRR
agree(se2irr)
gwet.ac1.raw(se2irr, weights = "unweighted")
kappa2(se2irr[, c("se1", "se2")], weight = "equal")

# Create matrix including Excerpt.x to compare diffs on two rounds of GPT
se2irrx <- gptCosts %>% select(Excerpt.x, se, se2)

# Add the variable se1 from se1irrx to the existing dataframe se2irrx
se2irrx <- se2irrx %>%
  mutate(se1 = se1irrx$se1)

# Filter the rows where se and se2 are different and select the desired columns
diffrows_se2 <- se2irrx %>%
  filter(se != se2) %>%
  select(Excerpt.x, se, se2)

## Exporting files based on this session 
#write.xlsx(diffrows_se1, file = "/Users/kristensyme/Desktop/R Projects/Fasting/Fasting R scripts/Fasting/se1 different rows KSGPT.xlsx", rowNames = FALSE)
#write.xlsx(diffrows_se2, file = "/Users/kristensyme/Desktop/R Projects/Fasting/Fasting R scripts/Fasting/se2 different rows GPTrerun.xlsx", rowNames = FALSE)
#write.xlsx(se2irrx, file = "/Users/kristensyme/Desktop/R Projects/Fasting/Fasting R scripts/Fasting/se2IRR.xlsx", rowNames = FALSE)

library(readxl)
se <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/se2IRR.xlsx")

