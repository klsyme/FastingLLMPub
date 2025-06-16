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

# Prompt for se.gpt
prompt <- "I will give you a paragraph of text on fasting under the heading Excerpt.x. I would like you to annotate the texts as follows based on the type of cost a faster or fasters are experiencing: Code a numerical 1 if the faster or fasters experience social or economic costs directly attributed to fasting; for example, if fasters are vulnerable to attack, face loss of business, if fasting too much is perceived as selfish, if the faster is blamed if a hunt is unsuccessful, or if not fasting creates conflict. Code a numerical 0 if there is no mention of any of the above social or economic costs attributed directly to fasting. For example, if the text describes a conflict that is not attributed to fasting then code 0. If someone is told to fast as a form of punishment or penance, code 0, unless fasting creates additional social and economic costs as defined above. Please use only the above criteria and avoid inferring social or economic costs. Please do not provide any other information in your response besides the 1 or the 0. Here is the paragraph:"
  
# Generate se.gpt
paragraphs$se.gpt <- map_chr(paste(prompt, paragraphs$Excerpt.x), hey_chatGPT)

table(paragraphs$se.gpt)

# Replace the specific incorrect entry with a single numeric 1
#paragraphs$se.gpt[paragraphs$se.gpt == "1\n0\n0\n0\n0\n1\n1\n0\n0\n0\n1\n0\n0"] <- 1

# se.gpt irr with coder 1
seirr <- paragraphs %>% select(se.x, se.gpt)
agree(seirr)
gwet.ac1.raw(seirr, weights = "unweighted")
kappa2(seirr[, c("se.x", "se.gpt")], weight = "equal")

# se.gpt irr with coder 2
seirr2 <- paragraphs %>% select(se.y, se.gpt)
agree(seirr2)
gwet.ac1.raw(seirr2, weights = "unweighted")
kappa2(seirr2[, c("se.y", "se.gpt")], weight = "equal")

# human coders agreement
seirr3 <- paragraphs %>% select(se.y, se.gpt)
agree(seirr3)
gwet.ac1.raw(seirr3, weights = "unweighted")
kappa2(seirr3[, c("se.y", "se.gpt")], weight = "equal")

write.xlsx(paragraphs, file = "/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/paragraphsse.xlsx", rowNames = FALSE)
