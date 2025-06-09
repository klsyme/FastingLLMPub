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

# Prompt for gen.gpt
prompt <- "I will give you a paragraph of text on fasting. Code a numerical 1 if the person or people fasting is a man or are men. Code a numerical 2 if the person or people who are fasting is a woman or group of women. You may use clues from the text; for instance, if there is a mention of a faster’s reduced milk supply for breastfeeding due to fasting, then code 2. Code a numerical 3 if the fasting mentioned in the paragraph applies to both men and women or the context makes it clear that both genders fast. Code a numerical 0 if the gender of the people or persons fasting is left unclear in the paragraph. If the text uses male pronouns for a specific person, code 1, and if the text uses female pronouns for a specific person, code 2. If the texts uses male pronouns, but it is not for a specific person, then code 0. If male pronouns are the only evidence that the faster is a man or a group of men, then code 0. Please do not provide any other information in your response. Here is the paragraph:"
  
# Generate gen.gpt
# paragraphs$gen.gpt2 <- map_chr(paste(prompt, paragraphs$Excerpt.x), hey_chatGPT)

table(paragraphs$gen.gpt2)
table(paragraphs$Gender.x)

# Replace the specific incorrect entry with a single numeric 1
paragraphs$gen.gpt2[paragraphs$gen.gpt2 == "1\n2\n0\n0\n0\n0\n1\n0\n0\n0\n2\n2\n0"] <- 1

# gen.gpt irr with coder 1
genirr <- paragraphs %>% select(gen.x, gen.gpt)
agree(genirr)
gwet.ac1.raw(genirr, weights = "unweighted")
kappa2(genirr[, c("gen.x", "gen.gpt")], weight = "equal")

# gen.gpt irr with coder 2
genirr2 <- paragraphs %>% select(gen.y, gen.gpt)
agree(genirr2)
gwet.ac1.raw(genirr2, weights = "unweighted")
kappa2(genirr2[, c("gen.y", "gen.gpt")], weight = "equal")

# human coders agreement
genirr3 <- paragraphs %>% select(gen.y, gen.gpt)
agree(genirr3)
gwet.ac1.raw(genirr3, weights = "unweighted")
kappa2(genirr3[, c("gen.y", "gen.gpt")], weight = "equal")

# save gen.gpt version 1
write.xlsx(paragraphs, file = "/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/paragraphsgen2.xlsx", rowNames = FALSE)
