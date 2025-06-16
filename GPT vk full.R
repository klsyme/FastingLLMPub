source("GPT Run.R")

library(irr)
library(irrCAC)

## This creates a data frame where each row has one paragraph
paragraphs <- 
  read_excel("df_combined full.xlsx") |> 
  mutate(
    Excerpt.x = str_remove_all(Excerpt.x, "\\{\\{\\d+\\}\\}"),    
    Excerpt.x = str_remove_all(Excerpt.x, "[^\x1F-\x7F]+")      
  ) 

# Prompt for vk.gpt
prompt <- "I will give you a paragraph of text on fasting under the heading Excerpt.x. I would like you to annotate the text as follows based on the type of cognitive experience the faster or fasters have during the fast. Code a numerical 1 if the text explicitly mentions that fasting leads a faster or fasters to experience dreams, prophecy, hallucinations, perceptual abnormalities, insights, creativity, clarity, or to learn some new information. Code a numerical 0 if there is no mention of fasting leading to the faster or fasters experiencing dreams, prophecy, hallucinations, perceptual abnormalities, insights, creativity, clarity, or to learn some new information. If a faster is talking to a spirit being, but the text does not explicitly mention that the spirit being comes from a dream or vision, then code 0. Please do not provide any other information in your response besides the 1 or the 0. Here is the paragraph:"

# Generate vk.gpt
paragraphs$vk.gpt <- map_chr(paste(prompt, paragraphs$Excerpt.x), hey_chatGPT)

table(paragraphs$vk.gpt)

# Convert NA values to 0 and all other values to 1
paragraphs$vk.gpt <- ifelse(is.na(paragraphs$vk.gpt) | paragraphs$vk.gpt == "", 0, 1)

# vk.gpt irr with coder 1
vkirr <- paragraphs %>% select(vk.x, vk.gpt)
agree(vkirr)
gwet.ac1.raw(vkirr, weights = "unweighted")
kappa2(vkirr[, c("vk.x", "vk.gpt")], weight = "equal")

# vk.gpt irr with coder 2
vkirr2 <- paragraphs %>% select(vk.y, vk.gpt)
agree(vkirr2)
gwet.ac1.raw(vkirr2, weights = "unweighted")
kappa2(vkirr2[, c("vk.y", "vk.gpt")], weight = "equal")

# human coders agreement
vkirr3 <- paragraphs %>% select(vk.y, vk.gpt)
agree(vkirr3)
gwet.ac1.raw(vkirr3, weights = "unweighted")
kappa2(vkirr3[, c("vk.y", "vk.gpt")], weight = "equal")

write.xlsx(paragraphs, file = "/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/paragraphsvk.xlsx", rowNames = FALSE)
