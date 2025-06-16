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

# Prompt for sca.gpt
prompt <- "I will give you a paragraph of text on fasting under the heading Excerpt.x. I would like you to annotate the texts as follows based on the type of cost a faster or fasters are experiencing: Code a numerical 1 if a person or people make a sacrifice of a slave or animal, or if they pay fees or make a resourcet transfer during the fast or around the fast. Code a numerical 0 if there is no mention a person or people make a sacrifice of a slave or animal, or if they pay fees or make a resource transfer during the fast or around the fast. If a resource transfer takes place, but is not associated with fasting, then code 0. Please do not provide any other information in your response besides the 1 or the 0. Here is the paragraph:"

# Generate sca.gpt
# paragraphs$sca.gpt <- map_chr(paste(prompt, paragraphs$Excerpt.x), hey_chatGPT)

table(paragraphs$sca.gpt)

# Replace the specific incorrect entry with a single numeric 1
paragraphs$sca.gpt[paragraphs$sca.gpt == "1\n1\n0\n0\n0\n0\n0\n0\n0\n0\n0\n0\n0"] <- 1

# sca.gpt irr with coder 1
scairr <- paragraphs %>% select(sca.x, sca.gpt)
agree(scairr)
gwet.ac1.raw(scairr, weights = "unweighted")
kappa2(scairr[, c("sca.x", "sca.gpt")], weight = "equal")

# sca.gpt irr with coder 2
scairr2 <- paragraphs %>% select(sca.y, sca.gpt)
agree(scairr2)
gwet.ac1.raw(scairr2, weights = "unweighted")
kappa2(scairr2[, c("sca.y", "sca.gpt")], weight = "equal")

# human coders agreement
scairr3 <- paragraphs %>% select(sca.y, sca.gpt)
agree(scairr3)
gwet.ac1.raw(scairr3, weights = "unweighted")
kappa2(scairr3[, c("sca.y", "sca.gpt")], weight = "equal")



write.xlsx(scairr, file = "/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/paragraphssca.xlsx", rowNames = FALSE)
