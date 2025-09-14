source("GPT Run.R")

library(irr)
library(irrCAC)

## This creates a data frame where each row has one paragraph
paragraphs <- 
  read_excel("Datasets/df_combined full.xlsx") |> 
  mutate(
    Excerpt.x = str_remove_all(Excerpt.x, "\\{\\{\\d+\\}\\}"),    
    Excerpt.x = str_remove_all(Excerpt.x, "[^\x1F-\x7F]+")      
  ) 

# Prompt for lead.gpt
prompt <- "I will give you a paragraph of text on fasting under the heading Excerpt.x. I would like you to annotate the texts as follows based on the leadership status of the faster or fasters. Code a numerical 1 if the paragraph explicitly mentions that someone who is fasting is a leader, or is about to become a leader including chiefs, war leaders, shamans, priests, monks. Note that these texts often use a non-English term for the leader, so use contextual cues suggesting that the person hold power and authority in political or religious settings from the text to determine if one of the fasters is a leader. Code a numerical 0 if there is no mention that one of the fasters is a leader. For example, if a text mentions a leader, such as a priest, commanding someone else to fast, but the leader is not fasting, then code 0. Please do not provide any other information in your response besides the 1 or the 0. Here is the paragraph:"

# Generate lead.gpt
#paragraphs$lead.gpt <- map_chr(paste(prompt, paragraphs$Excerpt.x), hey_chatGPT)

table(paragraphs$lead.gpt)

# Replace the specific incorrect entry with a single numeric 1
paragraphs$lead.gpt[paragraphs$lead.gpt == "1\n0\n0\n0\n0\n1\n1\n0\n0\n0\n1\n0\n0"] <- 1

# lead.gpt irr with coder 1
leadirr <- paragraphs %>% select(leader.x, lead.gpt)
agree(leadirr)
gwet.ac1.raw(leadirr, weights = "unweighted")
kappa2(leadirr[, c("leader.x", "lead.gpt")], weight = "equal")

# lead.gpt irr with coder 2
leadirr2 <- paragraphs %>% select(leader.y, lead.gpt)
agree(leadirr2)
gwet.ac1.raw(leadirr2, weights = "unweighted")
kappa2(leadirr2[, c("leader.y", "lead.gpt")], weight = "equal")

# human coders agreement
leadirr3 <- paragraphs %>% select(leader.y, lead.gpt)
agree(leadirr3)
gwet.ac1.raw(leadirr3, weights = "unweighted")
kappa2(leadirr3[, c("leader.y", "lead.gpt")], weight = "equal")

write.xlsx(paragraphs, file = "/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/paragraphslead.xlsx", rowNames = FALSE)
