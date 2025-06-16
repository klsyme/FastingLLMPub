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

# Prompt for sw.gpt
prompt <- "I will give you a paragraph of text on fasting under the heading Excerpt.x. I would like you to annotate the texts as follows based on the type of cost a faster or fasters are experiencing: Code a numerical 1 if an individual exhibits social withdrawal, such as going into seclusion or isolation in the woods or wilderness or in a secluded shelter during the fasting, but only if it involves an individual faster. Code a numerical 0 if there is no mention of an individual exhibiting social withdrawal as defined above. For example, if the text describes a person sitting outside their house all day while fasting, then there are possibly people around, so code 0. Please make sure the faster is alone and not with others. Please do not provide any other information in your response besides the 1 or the 0. Here is the paragraph:"

# Generate sw.gpt
#paragraphs$sw.gpt <- map_chr(paste(prompt, paragraphs$Excerpt.x), hey_chatGPT)

table(paragraphs$sw.gpt)

# Replace the specific incorrect entry with a single numeric 1
#paragraphs$sw.gpt[paragraphs$sw.gpt == "1\n1\n0\n0\n0\n0\n0\n0\n0\n0\n0\n0\n0"] <- 1

# sw.gpt irr with coder 1
swirr <- paragraphs %>% select(sw.x, sw.gpt)
agree(swirr)
gwet.ac1.raw(swirr, weights = "unweighted")
kappa2(swirr[, c("sw.x", "sw.gpt")], weight = "equal")

# sw.gpt irr with coder 2
swirr2 <- paragraphs %>% select(sw.y, sw.gpt)
agree(swirr2)
gwet.ac1.raw(swirr2, weights = "unweighted")
kappa2(swirr2[, c("sw.y", "sw.gpt")], weight = "equal")

# human coders agreement
swirr3 <- paragraphs %>% select(sw.y, sw.gpt)
agree(swirr3)
gwet.ac1.raw(swirr3, weights = "unweighted")
kappa2(swirr3[, c("sw.y", "sw.gpt")], weight = "equal")

write.xlsx(paragraphs, file = "/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/paragraphssw.xlsx", rowNames = FALSE)
