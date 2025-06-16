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

# Prompt for rel.gpt
prompt <- "I will give you a paragraph of text on fasting under the heading Excerpt.x. I would like you to annotate the text as follows based on the type of signal observers perceive in a faster or group of fasters, or the type of signal a faster intends to communicate. Code a numerical 1 if the text provides evidence that observers detect, perceive, or infer that fasters are religious, pious, or devout based on their fasting. This includes any indication that fasting is seen as a sign of the faster's faith or religious commitment. Code a numerical 0 if there is no mention in the text that fasting is perceived as a signal of religiosity, piety, or faith by observers, or that fasters intend to communicate these qualities. If a text mentions fasting without any specific reference to how it is perceived or intended to communicate, even in a religious context, then code 0. If the text mentions faith, religion, goodness, or loyalty without linking it to the perception of fasting as a signal of religiosity then code 0. Please do not provide any other information in your response besides the 1 or the 0. Here is the paragraph:"

# Generate rel.gpt
# paragraphs$rel.gpt <- map_chr(paste(prompt, paragraphs$Excerpt.x), hey_chatGPT)

table(paragraphs$rel.gpt)

# Replace the specific incorrect entry with a single numeric 1
#paragraphs$rel.gpt[paragraphs$rel.gpt == "1\n0\n0\n0\n0\n1\n1\n0\n0\n0\n1\n0\n0"] <- 1

# rel.gpt irr with coder 1
relirr <- paragraphs %>% select(rel.x, rel.gpt)
agree(relirr)
gwet.ac1.raw(relirr, weights = "unweighted")
kappa2(relirr[, c("rel.x", "rel.gpt")], weight = "equal")

# rel.gpt irr with coder 2
relirr2 <- paragraphs %>% select(rel.y, rel.gpt)
agree(relirr2)
gwet.ac1.raw(relirr2, weights = "unweighted")
kappa2(relirr2[, c("rel.y", "rel.gpt")], weight = "equal")

# human coders agreement
relirr3 <- paragraphs %>% select(rel.y, rel.gpt)
agree(relirr3)
gwet.ac1.raw(relirr3, weights = "unweighted")
kappa2(relirr3[, c("rel.y", "rel.gpt")], weight = "equal")

write.xlsx(paragraphs, file = "/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/paragraphsrel.xlsx", rowNames = FALSE)
