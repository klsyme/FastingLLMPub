source("GPT Run.R")

library(irr)
library(irrCAC)

## This creates a data frame where each row has one paragraph
paragraphs <- 
  read_excel("df_combined.xlsx") |> 
  mutate(
    Excerpt.x = str_remove_all(Excerpt.x, "\\{\\{\\d+\\}\\}"),    
    Excerpt.x = str_remove_all(Excerpt.x, "[^\x1F-\x7F]+")      
  ) 

# Prompt for dei.gpt
prompt <- "I will give you a paragraph of text on fasting under the heading Excerpt.x. I would like you to annotate the texts as follows based on the type of cost a faster or fasters are experiencing: Code a numerical 1 if the faster or fasters are tired from fasting and need more rest, are thirsty, hungry, suffering, in pain, at risk of seizures or agitated directly due to abstaining from food or drink. For example, if someone cannot work, because they are weak or tired from fasting, then code 1. Code a numerical 0 if there is no mention that the faster or fasters are tired from fasting and need more rest, are thirsty, hungry, suffering, in pain, at risk of seizures or agitated directly due to abstaining from food. For example, if a text describes a person who is fasting as part of healing practice for sickness but doesn’t describe any additional discomfort or sickness from fasting, then code 0. Do not include behavior exhibited by people in an altered state of consciousness as sickness. For example, if a shaman falls into a trance while fasting but is not sick, tired, or showing discomfort, then code 0. If sicknes, tiredness, or discomfort is caused by something besides fasting, such as herbal medicines, then code 0.  Please do not provide any other information in your response besides the 1 or the 0. Here is the paragraph:"

# Generate dei.gpt
#paragraphs$dei.gpt <- map_chr(paste(prompt, paragraphs$Excerpt.x), hey_chatGPT)

table(paragraphs$dei.gpt)

# dei.gpt irr with coder 1
deiirr <- paragraphs %>% select(dei.x, dei.gpt)
agree(deiirr)
gwet.ac1.raw(deiirr, weights = "unweighted")
kappa2(deiirr[, c("dei.x", "dei.gpt")], weight = "equal")

# dei.gpt irr with coder 2
deiirr2 <- paragraphs %>% select(dei.y, dei.gpt)
agree(deiirr2)
gwet.ac1.raw(deiirr2, weights = "unweighted")
kappa2(deiirr2[, c("dei.y", "dei.gpt")], weight = "equal")

# human coders agreement
deiirr3 <- paragraphs %>% select(dei.y, dei.gpt)
agree(deiirr3)
gwet.ac1.raw(deiirr3, weights = "unweighted")
kappa2(deiirr3[, c("dei.y", "dei.gpt")], weight = "equal")

write.xlsx(paragraphs, file = "/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/paragraphsdei.xlsx", rowNames = FALSE)
