source("GPT Run.R")

library(irr)
library(irrCAC)


## This creates a data frame where each row has one paragraph
paragraphs <- 
  read_excel("Datasets/Cognitive Subset Unlisted.xlsx") |> 
  mutate(
    Excerpt.x = str_remove_all(Excerpt.x, "\\{\\{\\d+\\}\\}"),    
    Excerpt.x = str_remove_all(Excerpt.x, "[^\x1F-\x7F]+")      
  ) 

# Prompt for vk1
prompt <- "I will give you a paragraph of text on fasting under the heading Excerpt.x. I would like you to annotate the text as follows based on the type of cognitive experience the faster or fasters have during the fast. Code a numerical 1 if the text explicitly mentions that fasting leads a faster or fasters to experience dreams, prophecy, hallucinations, perceptual abnormalities, insights, creativity, clarity, or to learn some new information. Code a numerical 0 if there is no mention of fasting leading to the faster or fasters experiencing dreams, prophecy, hallucinations, perceptual abnormalities, insights, creativity, clarity, or to learn some new information. Please do not provide any other information in your response besides the 1 or the 0. Here is the paragraph:"

# Generate vk1
gptCognitive <- paragraphs
# gptCognitive$vk1 <- map_chr(paste(prompt, gptCognitive$Excerpt.x), hey_chatGPT)

# vk1 irr with resolved codes
vk1irr <- gptCognitive %>% select(vk, vk1)
agree(vk1irr)
gwet.ac1.raw(vk1irr, weights = "unweighted")
kappa2(vk1irr[, c("vk", "vk1")], weight = "equal")

# Create matrix including Excerpt.x to compare diffs
vk1irrx <- gptCognitive %>% select(Excerpt.x, vk, vk1)

# Filter the rows where vk and vk1 are different and select the desired columns
diffrows_vk1 <- vk1irrx %>%
  filter(vk != vk1) %>%
  select(Excerpt.x, vk, vk1)

# Prompt for vk2--same as vk1
prompt <- "I will give you a paragraph of text on fasting under the heading Excerpt.x. I would like you to annotate the text as follows based on the type of cognitive experience the faster or fasters have during the fast. Code a numerical 1 if the text explicitly mentions that fasting leads a faster or fasters to experience dreams, prophecy, hallucinations, perceptual abnormalities, insights, creativity, clarity, or to learn some new information. Code a numerical 0 if there is no mention of fasting leading to the faster or fasters experiencing dreams, prophecy, hallucinations, perceptual abnormalities, insights, creativity, clarity, or to learn some new information. Please do not provide any other information in your response besides the 1 or the 0. Here is the paragraph:"

# Generate vk2
gptCognitive <- paragraphs
# gptCognitive$vk2 <- map_chr(paste(prompt, gptCognitive$Excerpt.x), hey_chatGPT)

# vk2 irr with itself
# Extract the desired columns from each dataframe
vk2 <- gptCognitive %>% select(vk2)
vk1 <- vk1irrx %>% select(vk1)
# Combine the extracted columns into a new dataframe
vk2irr <- data.frame(vk1 = vk1$vk1, vk2 = vk2$vk2)
# Now check IRR
agree(vk2irr)
gwet.ac1.raw(vk2irr, weights = "unweighted")
kappa2(vk2irr[, c("vk1", "vk2")], weight = "equal")

# Create matrix including Excerpt.x to compare diffs on two rounds of GPT
vk2irrx <- gptCognitive %>% select(Excerpt.x, vk, vk2)
# Add the variable vk1 from vk1irrx to the existing dataframe vk2irrx
vk2irrx2 <- vk2irrx %>%
  mutate(vk1 = vk1irr$vk1)

## Exporting files based on this session 
#write.xlsx(diffrows_vk1, file = "/Users/kristensyme/Desktop/R Projects/Fasting/Fasting R scripts/Fasting/vk1 different rows KSGPT.xlsx", rowNames = FALSE)
#write.xlsx(vk2irrx2, file = "/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/vk2IRR.xlsx", rowNames = FALSE)



