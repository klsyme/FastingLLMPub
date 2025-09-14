source("GPT Run.R")

## This creates a data frame where each row has one paragraph
paragraphs <- 
  read_excel("Datasets/Costs Subset Unlisted.xlsx") |> 
  mutate(
    Excerpt.x = str_remove_all(Excerpt.x, "\\{\\{\\d+\\}\\}"),    
    Excerpt.x = str_remove_all(Excerpt.x, "[^\x1F-\x7F]+")      
  ) 

# Prompt for dei1
prompt <- "I will give you a paragraph of text on fasting under the heading Excerpt.x. I would like you to annotate the texts as follows based on the type of cost a faster or fasters are experiencing: Code a numerical 1 if the faster or fasters are tired from fasting and need more rest, are thirsty, hungry, suffering, in pain, at risk of seizures or agitated directly due to abstaining from food or drink. Code a numerical 0 if there is no mention that the faster or fasters are tired from fasting and need more rest, are thirsty, hungry, suffering, in pain, at risk of seizures or agitated directly due to abstaining from food. For example, if a text describes a person who is fasting because they are already sick but doesn’t describe any additional discomfort from fasting then code 0. Please do not provide any other information in your response besides the 1 or the 0. Here is the paragraph:"

# Generate dei1
gptCosts <- paragraphs
#gptCosts$dei1 <- map_chr(paste(prompt, gptCosts$Excerpt.x), hey_chatGPT)

# Dei 1 irr with resolved codes
dei1irr <- gptCosts %>% select(dei, dei1)
agree(dei1irr)
gwet.ac1.raw(dei1irr, weights = "unweighted")
kappa2(dei1irr[, c("dei", "dei1")], weight = "equal")

# Create matrix including Excerpt.x to compare diffs
dei1irrx <- gptCosts %>% select(Excerpt.x, dei, dei1)

# Filter the rows where dei and dei1 are different and select the desired columns
diffrows_dei1 <- dei1irrx %>%
  filter(dei != dei1) %>%
  select(Excerpt.x, dei, dei1)


# Prompt for dei2--same as dei 1
# prompt <- "I will give you a paragraph of text on fasting under the heading Excerpt.x. I would like you to annotate the texts as follows based on the type of cost a faster or fasters are experiencing: Code a numerical 1 if the faster or fasters are tired from fasting and need more rest, are thirsty, hungry, suffering, in pain, at risk of seizures or agitated directly due to abstaining from food or drink. Code a numerical 0 if there is no mention that the faster or fasters are tired from fasting and need more rest, are thirsty, hungry, suffering, in pain, at risk of seizures or agitated directly due to abstaining from food. For example, if a text describes a person who is fasting because they are already sick but doesn’t describe any additional discomfort from fasting then code 0. Please do not provide any other information in your response besides the 1 or the 0. Here is the paragraph:"

# Generate dei2
gptCosts <- paragraphs
# gptCosts$dei2 <- map_chr(paste(prompt, gptCosts$Excerpt.x), hey_chatGPT)

# Dei 2 irr with itself
# Extract the desired columns from each dataframe
dei2 <- gptCosts %>% select(dei2)
dei1 <- dei1irr %>% select(dei1)
# Combine the extracted columns into a new dataframe
dei2irr <- data.frame(dei1 = dei1$dei1, dei2 = dei2$dei2)
# Now check IRR
agree(dei2irr)
gwet.ac1.raw(dei2irr, weights = "unweighted")
kappa2(dei2irr[, c("dei1", "dei2")], weight = "equal")

# Create matrix including Excerpt.x to compare diffs on two rounds of GPT
dei2irrx <- gptCosts %>% select(Excerpt.x, dei, dei2)

# Add the variable dei1 from dei1irrx to the existing dataframe dei2irrx
dei2irrx <- dei2irrx %>%
  mutate(dei1 = dei1irrx$dei1)

# Filter the rows where dei and dei2 are different and select the desired columns
diffrows_dei2 <- dei2irrx %>%
  filter(dei != dei2) %>%
  select(Excerpt.x, dei, dei2)

## Exporting files based on this session 
#write.xlsx(diffrows_dei1, file = "/Users/kristensyme/Desktop/R Projects/Fasting/Fasting R scripts/Fasting/dei1 different rows KSGPT.xlsx", rowNames = FALSE)
#write.xlsx(diffrows_dei2, file = "/Users/kristensyme/Desktop/R Projects/Fasting/Fasting R scripts/Fasting/dei2 different rows GPTrerun.xlsx", rowNames = FALSE)
# write.xlsx(dei2irrx, file = "/Users/kristensyme/Desktop/R Projects/Fasting/Fasting R scripts/Fasting/dei2IRR.xlsx", rowNames = FALSE)

dei2irrx <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/Fasting R scripts/Fasting/dei2IRR.xlsx", na = 'na')

library(readxl)
dei <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/dei2IRR.xlsx")

table(dei$dei)

