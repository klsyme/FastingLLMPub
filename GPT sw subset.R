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

# Prompt for sw1
# prompt <- "I will give you a paragraph of text on fasting under the heading Excerpt.x. I would like you to annotate the texts as follows based on the type of cost a faster or fasters are experiencing: Code a numerical 1 if an individual exhibits social withdrawal, such as going into seclusion or isolation in the woods or wilderness or in a secluded shelter during the fasting, but only if it involves an individual faster. Code a numerical 0 if there is no mention of an individual exhibiting social withdrawal as defined above. For example, if the text describes a person sitting outside their house all day while fasting, then there are possibly people around, so code 0. Please do not provide any other information in your response besides the 1 or the 0. Here is the paragraph:"

# Generate sw1
gptCosts <- paragraphs
#gptCosts$sw1 <- map_chr(paste(prompt, gptCosts$Excerpt.x), hey_chatGPT)

# sw1 irr with resolved codes
sw1irr <- gptCosts %>% select(sw, sw1)
agree(sw1irr)
gwet.ac1.raw(sw1irr$sw, sw1irr$sw1, weights = "unweighted")
kappa2(sw1irr[, c("sw", "sw1")], weight = "equal")
# Run again
sw1irr2 <- sw1irr %>% select(sw, sw1)
agree(sw1irr2)

# Create matrix including Excerpt.x to compare diffs
sw1irrx <- gptCosts %>% select(Excerpt.x, sw, sw1)

# Filter the rows where sw and sw1 are different and select the desired columns
diffrows_sw1 <- sw1irr %>%
  filter(sw != sw1) %>%
  select(Excerpt.x, sw, sw1)

# Prompt for sw2--slight change
prompt <- "I will give you a paragraph of text on fasting under the heading Excerpt.x. I would like you to annotate the texts as follows based on the type of cost a faster or fasters are experiencing: Code a numerical 1 if an individual exhibits social withdrawal, such as going into seclusion or isolation in the woods or wilderness or in a secluded shelter during the fasting, but only if it involves an individual faster. Code a numerical 0 if there is no mention of an individual exhibiting social withdrawal as defined above. For example, if the text describes a person sitting outside their house all day while fasting, then there are possibly people around, so code 0. Please do not provide any other information in your response besides the 1 or the 0. Here is the paragraph:"

# Generate sw2
gptCosts <- paragraphs
#gptCosts$sw2 <- map_chr(paste(prompt, gptCosts$Excerpt.x), hey_chatGPT)

# sw2 irr with itself
# Extract the desired columns from each dataframe
sw2 <- gptCosts %>% select(sw2)
sw1 <- sw1irr %>% select(sw1)
# Combine the extracted columns into a new dataframe
sw2irr <- data.frame(sw1 = sw1$sw1, sw2 = sw2$sw2)
# Now check IRR
agree(sw2irr)
gwet.ac1.raw(sw2irr, weights = "unweighted")
kappa2(sw2irr[, c("sw1", "sw2")], weight = "equal")


# Create matrix including Excerpt.x to compare diffs on two rounds of GPT
sw2irrx <- gptCosts %>% select(Excerpt.x, sw, sw2)
# Add the variable sw1 from sw1irrx to the existing dataframe sw2irrx
sw2irrx <- sw2irrx %>%
  mutate(sw1 = sw1irr$sw1)

# Filter the rows where punish and punish1 are different and select the desired columns
diffrows_sw2 <- sw2irrx %>%
  filter(sw != sw2) %>%
  select(Excerpt.x, sw, sw2)

# sw2 irr with resolved codes
sw2irr2 <- sw2irrx %>% select(sw, sw2)
agree(sw2irr2)
gwet.ac1.raw(sw2irr2, weights = "unweighted")
kappa2(sw2irr2[, c("sw", "sw2")], weight = "equal")

# Generate sw3
gptCosts <- paragraphs
gptCosts$sw3 <- map_chr(paste(prompt, gptCosts$Excerpt.x), hey_chatGPT)

# sw2 irr with sw3
#export data
write.xlsx(gptCosts, file = "/Users/kristensyme/Desktop/R Projects/Fasting/Fasting R scripts/Fasting/sw3.xlsx", rowNames = FALSE)


#reupload data
sw3 <- read_excel("/Users/kristensyme/Desktop/Fasting OSF subset pilot data/Costs/sw3IRR.xlsx")
# Combine the extracted columns into a new dataframe
sw3irr <- sw3 %>% select(sw2, sw3)
# Now check IRR
agree(sw3irr)
gwet.ac1.raw(sw2irr, weights = "unweighted")
kappa2(sw2irr[, c("sw1", "sw2")], weight = "equal")


# Create matrix including Excerpt.x to compare diffs on two rounds of GPT
sw2irrx <- gptCosts %>% select(Excerpt.x, sw, sw2)
# Add the variable sw1 from sw1irrx to the existing dataframe sw2irrx
sw2irrx <- sw2irrx %>%
  mutate(sw1 = sw1irr$sw1)

# Filter the rows where punish and punish1 are different and select the desired columns
diffrows_sw2 <- sw2irrx %>%
  filter(sw != sw2) %>%
  select(Excerpt.x, sw, sw2)

# sw2 irr with resolved codes
sw2irr2 <- sw2irrx %>% select(sw, sw2)
agree(sw2irr2)
gwet.ac1.raw(sw2irr2, weights = "unweighted")
kappa2(sw2irr2[, c("sw", "sw2")], weight = "equal")


## Exporting files based on this session 
write.xlsx(diffrows_sw1, file = "/Users/kristensyme/Desktop/R Projects/Fasting/Fasting R scripts/Fasting/sw1 different rows KSGPT.xlsx", rowNames = FALSE)
write.xlsx(diffrows_sw2, file = "/Users/kristensyme/Desktop/R Projects/Fasting/Fasting R scripts/Fasting/sw2 different rows GPTrerun.xlsx", rowNames = FALSE)
# write.xlsx(sw2irrx, file = "/Users/kristensyme/Desktop/R Projects/Fasting/Fasting R scripts/Fasting/sw2IRR.xlsx", rowNames = FALSE)

library(readxl)
sw <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/sw3IRR.xlsx")
# double check irr
# Combine the extracted columns into a new dataframe
sw3 <- sw %>% select(sw2, sw3)
# Now check IRR
agree(sw3)
gwet.ac1.raw(sw3, weights = "unweighted")
kappa2(sw2irr[, c("sw1", "sw2")], weight = "equal")

