source("GPT Run.R")

library(irr)
library(irrCAC)


## This creates a data frame where each row has one paragraph
paragraphs <- 
  read_excel("Cognitive Subset Unlisted.xlsx") |> 
  mutate(
    Excerpt.x = str_remove_all(Excerpt.x, "\\{\\{\\d+\\}\\}"),    
    Excerpt.x = str_remove_all(Excerpt.x, "[^\x1F-\x7F]+")      
  ) 

# Prompt for asc1
#prompt <- "I will give you a paragraph of text on fasting under the heading Excerpt.x. I would like you to annotate the text as follows based on the type of cognitive experience the faster or fasters have during the fast. Code a numerical 1 if the paragraph explicitly mentions fasting leading to the faster or fasters experiencing an altered state of consciousness, such as trance, meditative states, contemplative states, hypnotic states, ecstatic states, or loss of consciousness. Code a numerical 0 if there is no mention of fasting leading to the faster or fasters experiencing an altered state of consciousness as defined above. If an altered state of consciousness is mentioned, but it is not caused by fasting, then code 0. Please do not provide any other information in your response besides the 1 or the 0. Here is the paragraph:"

# Generate asc1
gptCognitive <- paragraphs
# gptCognitive$asc1 <- map_chr(paste(prompt, gptCognitive$Excerpt.x), hey_chatGPT)

# asc1 irr with resolved codes
asc1irr <- gptCognitive %>% select(asc, asc1)
agree(asc1irr)
gwet.ac1.raw(asc1irr, weights = "unweighted")
kappa2(asc1irr[, c("asc", "asc1")], weight = "equal")

# Create matrix including Excerpt.x to compare diffs
asc1irrx <- gptCognitive %>% select(Excerpt.x, asc, asc1)

# Filter the rows where asc and asc1 are different and select the desired columns
diffrows_asc1 <- asc1irrx %>%
  filter(asc != asc1) %>%
  select(Excerpt.x, asc, asc1)



# Prompt for asc2--different from asc1 by specifying to not include visionary experiences
prompt <- "I will give you a paragraph of text on fasting under the heading Excerpt.x. I would like you to annotate the text as follows based on the type of cognitive experience the faster or fasters have during the fast. Code a numerical 1 if the paragraph explicitly mentions fasting leading to the faster or fasters experiencing an altered state of consciousness, such as trance, meditative states, contemplative states, hypnotic states, ecstatic states, or loss of consciousness, not including dreams, hallucinations, revelations, or other visionary experiences. Code a numerical 0 if there is no mention of fasting leading to the faster or fasters experiencing an altered state of consciousness as defined above. If an altered state of consciousness is mentioned, but it is not caused by fasting, then code 0. Please do not provide any other information in your response besides the 1 or the 0. Here is the paragraph:"

# Generate asc2
# gptCognitive$asc2 <- map_chr(paste(prompt, gptCognitive$Excerpt.x), hey_chatGPT)

# asc2 irr with resolved codes--revised prompt
asc2irr <- gptCognitive %>% select(asc, asc2)
agree(asc2irr)
gwet.ac1.raw(asc2irr, weights = "unweighted")
kappa2(asc2irr[, c("asc", "asc2")], weight = "equal")

# Create matrix including Excerpt.x to compare diffs
asc2irrx <- gptCognitive %>% select(Excerpt.x, asc, asc2)

# Filter the rows where asc and asc2 are different and select the desired columns
diffrows_asc2irr <- asc2irrx %>%
  filter(asc != asc2) %>%
  select(Excerpt.x, asc, asc2)

# asc2 irr compared to asc1 (different prompt)
# Extract the desired columns from each dataframe
asc2 <- gptCognitive %>% select(asc2)
asc1 <- asc1irrx %>% select(asc1)
# Combine the extracted columns into a new dataframe
asc2irr2 <- data.frame(asc1 = asc1$asc1, asc2 = asc2$asc2)
# Now check IRR
agree(asc2irr2)
gwet.ac1.raw(asc2irr2, weights = "unweighted")
kappa2(asc2irr2[, c("asc1", "asc2")], weight = "equal")

# Create matrix including Excerpt.x to compare diffs on two rounds of GPT
# vk2irrx <- gptCognitive %>% select(Excerpt.x, vk, vk2)
# Add the variable vk1 from vk1irrx to the existing dataframe vk2irrx
# vk2irrx2 <- vk2irrx %>%
  # mutate(vk1 = vk1irr$vk1)

# asc2 irr with resolved codes--revised prompt
asc3irr <- gptCognitive %>% select(as, asc2)
agree(asc2irr)
gwet.ac1.raw(asc2irr, weights = "unweighted")
kappa2(asc2irr[, c("asc", "asc2")], weight = "equal")

# Create matrix including Excerpt.x to compare diffs
asc2irrx <- gptCognitive %>% select(Excerpt.x, asc, asc2)


# Extract the desired columns from each dataframe
asc <- gptCognitive %>% select(asc)
acs2 <- gptCognitive %>% select(asc2)
asc1 <- asc1irrx %>% select(asc1)
Excerpt.x <- gptCognitive %>% select(Excerpt.x)

# Combine the extracted columns into a new dataframe
asc2irrx2 <- data.frame(Excerpt.x = Excerpt.x$Excerpt.x, asc = asc$asc, asc1 = asc1$asc1, asc2 = asc2$asc2)

# Prompt for asc3--same as asc2
prompt <- "I will give you a paragraph of text on fasting under the heading Excerpt.x. I would like you to annotate the text as follows based on the type of cognitive experience the faster or fasters have during the fast. Code a numerical 1 if the paragraph explicitly mentions fasting leading to the faster or fasters experiencing an altered state of consciousness, such as trance, meditative states, contemplative states, hypnotic states, ecstatic states, or loss of consciousness, not including dreams, hallucinations, revelations, or other visionary experiences. Code a numerical 0 if there is no mention of fasting leading to the faster or fasters experiencing an altered state of consciousness as defined above. If an altered state of consciousness is mentioned, but it is not caused by fasting, then code 0. Please do not provide any other information in your response besides the 1 or the 0. Here is the paragraph:"

# Create matrix including Excerpt.x to compare diffs
asc2irrx <- gptCognitive %>% select(Excerpt.x, asc, asc2)

# Generate asc3
#gptCognitive$asc3 <- map_chr(paste(prompt, gptCognitive$Excerpt.x), hey_chatGPT)

#write.xlsx(gptCognitive, file = "/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/asc3.xlsx", rowNames = FALSE)

#upload file to generate % agreement between 2 runs
asc3 <- read_excel("/Users/kristensyme/Desktop/Fasting OSF subset pilot data/Cognitive/asc3IRR.xlsx")
asc3irr <- asc3 %>% select(asc2, asc3)
agree(asc3irr)


# asc2 irr with resolved codes--revised prompt
asc3irr <- gptCognitive %>% select(as, asc2)
agree(asc2irr)
gwet.ac1.raw(asc2irr, weights = "unweighted")
kappa2(asc2irr[, c("asc", "asc2")], weight = "equal")



## Exporting files based on this session 
# write.xlsx(diffrows_asc2irr, file = "/Users/kristensyme/Desktop/R Projects/Fasting/Fasting R scripts/Fasting/asc2 different rows KSGPT.xlsx", rowNames = FALSE)
# write.xlsx(asc2irrx2, file = "/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/asc2IRR.xlsx", rowNames = FALSE)




