source("/Users/kristensyme/Desktop/R Projects/GPT Final Run.R")

library(irr)
library(irrCAC)

## This creates a data frame where each row has one paragraph
paragraphs <- 
  read_excel("df_combined full.xlsx") |> 
  mutate(
    Excerpt.x = str_remove_all(Excerpt.x, "\\{\\{\\d+\\}\\}"),    
    Excerpt.x = str_remove_all(Excerpt.x, "[^\x1F-\x7F]+")      
  ) 

# Prompt for gm.gpt
prompt <- "I will give you a paragraph of text on fasting under the heading Excerpt.x. I would like you to annotate the text as follows based on the type of signal observers perceive in a faster or group of fasters, or the type of signal a faster intends to communicate. Code a numerical 1 if the text explicitly mentions that observers detect, perceive, or infer commitment or solidarity with or to group by the act or practice of fasting. Code a numerical 0 if there is no mention in the text that observers detect, perceive, or infer commitment or solidarity with or to group by the act of fasting. For example, fasting might be done in a group with other people who might be living and cooperating together, but if the text does not mention that others infer group commitment or that fasters intend to communicate group commitment, then code 0. Signals of group commitment and solidarity can be mediated through religious observance, but if the text does not mention commitment specifically to a group of people, as opposed to an ideal or faith. For example, if the text states that fasting displays solidarity or unity to a collective group, then code 1, but if the text states that fasting displays faith rather than solidarity with other believers, then code 0. If fasting is done as part of the process of joining a new group , then code 1. If  fasting is part of a puberty initiation code 0 unless the initiation is explicitly associated with new group membership. Please do not provide any other information in your response besides the 1 or the 0. Here is the paragraph:"

# Generate gm.gpt
# paragraphs$gm.gpt <- map_chr(paste(prompt, paragraphs$Excerpt.x), hey_chatGPT)

table(paragraphs$gm.gpt)

# Replace the specific incorrect entry with a single numeric 1
#paragraphs$gm.gpt[paragraphs$gm.gpt == "1\n0\n0\n0\n0\n1\n1\n0\n0\n0\n1\n0\n0"] <- 1

# gm.gpt irr with coder 1
gmirr <- paragraphs %>% select(gm.x, gm.gpt)
agree(gmirr)
gwet.ac1.raw(gmirr, weights = "unweighted")
kappa2(gmirr[, c("gm.x", "gm.gpt")], weight = "equal")

# gm.gpt irr with coder 2
gmirr2 <- paragraphs %>% select(gm.y, gm.gpt)
agree(gmirr2)
gwet.ac1.raw(gmirr2, weights = "unweighted")
kappa2(gmirr2[, c("gm.y", "gm.gpt")], weight = "equal")

# human coders agreement
gmirr3 <- paragraphs %>% select(gm.y, gm.gpt)
agree(gmirr3)
gwet.ac1.raw(gmirr3, weights = "unweighted")
kappa2(gmirr3[, c("gm.y", "gm.gpt")], weight = "equal")

write.xlsx(paragraphs, file = "/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/paragraphsgm.xlsx", rowNames = FALSE)
