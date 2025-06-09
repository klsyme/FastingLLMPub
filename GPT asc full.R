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

# Prompt for asc.gpt 
prompt <- "I will give you a paragraph of text on fasting under the heading Excerpt.x. I would like you to annotate the text as follows based on the type of cognitive experience the faster or fasters have during the fast. Code a numerical 1 if the paragraph explicitly mentions fasting leading to the faster or fasters experiencing an altered state of consciousness, such as trance, meditative states, contemplative states, hypnotic states, ecstatic states, or loss of consciousness, not including dreams, hallucinations, revelations, or other visionary experiences. If dreams or visions occur with an altered state of consciousness, then code 1. Code a numerical 0 if there is no mention of fasting leading to the faster or fasters experiencing an altered state of consciousness as defined above. If an altered state of consciousness is mentioned, but it is not caused by fasting, then code 0. If dreams and visions are mentioned but an altered state of consciousness is not mentioned, then code 0.  Please do not provide any other information in your response besides the 1 or the 0. Here is the paragraph:"

# Generate asc.gpt
# paragraphs$asc.gpt <- map_chr(paste(prompt, paragraphs$Excerpt.x), hey_chatGPT)

table(paragraphs$asc.gpt)

# asc.gpt irr with coder 1
ascirr <- paragraphs %>% select(asc.x, asc.gpt)
agree(ascirr)
gwet.ac1.raw(ascirr, weights = "unweighted")
kappa2(ascirr[, c("asc.x", "asc.gpt")], weight = "equal")

# asc.gpt irr with coder 2
ascirr2 <- paragraphs %>% select(asc.y, asc.gpt)
agree(ascirr2)
gwet.ac1.raw(ascirr2, weights = "unweighted")
kappa2(ascirr2[, c("asc.y", "asc.gpt")], weight = "equal")

# human coders agreement
ascirr3 <- paragraphs %>% select(asc.y, asc.gpt)
agree(ascirr3)
gwet.ac1.raw(ascirr3, weights = "unweighted")
kappa2(ascirr3[, c("asc.y", "asc.gpt")], weight = "equal")

write.xlsx(paragraphs, file = "/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/paragraphsasc.xlsx", rowNames = FALSE)

