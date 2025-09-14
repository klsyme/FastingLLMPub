source("GPT Run.R")

library(irr)
library(irrCAC)


## This creates a data frame where each row has one paragraph
paragraphs <- 
  read_excel("Datasets/Signal Subset Unlisted.xlsx") |> 
  mutate(
    Excerpt.x = str_remove_all(Excerpt.x, "\\{\\{\\d+\\}\\}"),    
    Excerpt.x = str_remove_all(Excerpt.x, "[^\x1F-\x7F]+")      
  ) 

# Prompt for rel1
prompt <- "I will give you a paragraph of text on fasting under the heading Excerpt.x. I would like you to annotate the text as follows based on the type of signal observers perceive in a faster or group of fasters. Code a numerical 1 if the text provides evidence that observers detect, perceive, or infer that fasters are religious, pious, or devout based on their fasting. This includes any indication that fasting is seen as a sign of the faster's faith or religious commitment. Code a numerical 0 if there is no mention in the text that fasting is perceived as a signal of religiosity, piety, or faith by observers. If a text mentions fasting without any specific reference to how it is perceived, even in a religious context, then code 0. If the text mentions faith or religion without linking it to the perception of fasting as a signal of religiosity then code 0. Please do not provide any other information in your response besides the 1 or the 0. Here is the paragraph:"
  
# Generate rel1
gptSignal <- paragraphs
# gptSignal$rel1 <- map_chr(paste(prompt, gptSignal$Excerpt.x), hey_chatGPT)

# rel1 irr with resolved codes
rel1irr <- gptSignal %>% select(rel, rel1)
agree(rel1irr)
gwet.ac1.raw(rel1irr, weights = "unweighted")
kappa2(rel1irr[, c("rel", "rel1")], weight = "equal")

# Create matrix including Excerpt.x to compare diffs
rel1irrx <- gptSignal %>% select(Excerpt.x, rel, rel1)

# Filter the rows where rel and rel1 are different and select the desired columns
diffrows_rel1 <- rel1irrx %>%
  filter(rel != rel1) %>%
  select(Excerpt.x, rel, rel1)


# Prompt for rel2
prompt <-"I will give you a paragraph of text on fasting under the heading Excerpt.x. I would like you to annotate the text as follows based on the type of signal observers perceive in a faster or group of fasters. Code a numerical 1 if the text explicitly mentions that observers detect, perceive, or infer that fasters are religious, pious, or devout based on fasting or not eating. This includes any indication that fasting or not eating is seen as a sign of the faster's faith or religious commitment. Code a numerical 0 if there is no mention in the text that fasting is explicitly perceived as a signal of religiosity, piety, or faith by observers. If a text mentions fasting without any specific reference to how it is perceived by others, even in a religious context, then code 0. If the text mentions faith or religion without linking it to the perception of fasting as a signal of religiosity then code 0. If spirituality or religion are a reason for fasting, but the text does not state that others explicitly infer faith or piety in the faster then code 0. Please do not provide any other information in your response besides the 1 or the 0. Here is the paragraph:"

# Generate rel2
gptSignal <- paragraphs
# gptSignal$rel2 <- map_chr(paste(prompt, gptSignal$Excerpt.x), hey_chatGPT)

# rel2 irr with resolved codes
rel2irr <- gptSignal %>% select(rel, rel2)
agree(rel2irr)
gwet.ac1.raw(rel2irr, weights = "unweighted")
kappa2(rel2irr[, c("rel", "rel2")], weight = "equal")

# Create matrix including Excerpt.x to compare diffs
rel2irrx <- gptSignal %>% select(Excerpt.x, rel, rel2)

# Filter the rows where rel and rel2 are different and select the desired columns
diffrows_rel2 <- rel2irrx %>%
  filter(rel != rel2) %>%
  select(Excerpt.x, rel, rel2)

# Prompt for rel3--same as rel2
prompt <-"I will give you a paragraph of text on fasting under the heading Excerpt.x. I would like you to annotate the text as follows based on the type of signal observers perceive in a faster or group of fasters. Code a numerical 1 if the text explicitly mentions that observers detect, perceive, or infer that fasters are religious, pious, or devout based on fasting or not eating. This includes any indication that fasting or not eating is seen as a sign of the faster's faith or religious commitment. Code a numerical 0 if there is no mention in the text that fasting is explicitly perceived as a signal of religiosity, piety, or faith by observers. If a text mentions fasting without any specific reference to how it is perceived by others, even in a religious context, then code 0. If the text mentions faith or religion without linking it to the perception of fasting as a signal of religiosity then code 0. If spirituality or religion are a reason for fasting, but the text does not state that others explicitly infer faith or piety in the faster then code 0. Please do not provide any other information in your response besides the 1 or the 0. Here is the paragraph:"

# Generate rel3
gptSignal <- paragraphs
# gptSignal$rel3 <- map_chr(paste(prompt, gptSignal$Excerpt.x), hey_chatGPT)


# rel3 irr with rel2
# Extract the desired columns from each dataframe
rel <- relunlisted %>% select(rel)
rel1 <- rel1irr %>% select(rel1)
rel3 <- gptSignal %>% select(rel3)
rel2 <- rel2irrx %>% select(rel2)

Excerpt.x <- gptSignal %>% select(Excerpt.x)
# Combine the extracted columns into a new dataframe
rel3irr <- data.frame(rel2 = rel2$rel2, rel3 = rel3$rel3)
# Now check IRR
agree(rel3irr)
gwet.ac1.raw(rel2irr, weights = "unweighted")
kappa2(rel2irr[, c("rel1", "rel2")], weight = "equal")

rel3irrx <- data.frame(Excerpt.x = Excerpt.x$Excerpt.x, rel1 = rel1$rel1, rel2 = rel2$rel2, rel3 = rel3$rel3)


## Exporting files based on this session 
write.xlsx(diffrows_rel1, file = "/Users/kristensyme/Desktop/R Projects/Fasting/Fasting R scripts/Fasting/rel1 different rows KSGPT.xlsx", rowNames = FALSE)
write.xlsx(diffrows_rel2, file = "/Users/kristensyme/Desktop/R Projects/Fasting/Fasting R scripts/Fasting/rel2 different rows KSGPT.xlsx", rowNames = FALSE)
write.xlsx(rel3irrx, file = "/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/rel3irr.xlsx", rowNames = FALSE)

rel3irr2 <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/rel3irr.xlsx")
rel3irr <- rel3irr2 %>% select(rel2, rel3)
agree(rel3irr)

library(readxl)
relunlisted <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/Fasting R scripts/Fasting/Signal Subset Unlisted.xlsx")
relx <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/rel3irr.xlsx")

# Add human rel to gpt annotated rel
# Extract the desired columns from each dataframe
rel <- relunlisted %>% select(rel)
rel1 <- relx %>% select(rel1)
rel3 <- relx %>% select(rel3)
rel2 <- relx %>% select(rel2)
Excerpt.x <- relx %>% select(Excerpt.x)
# Combine the extracted columns into a new dataframe
relx4 <- data.frame(Excerpt.x = Excerpt.x$Excerpt.x, rel = rel$rel, rel1 = rel1$rel1, rel2 = rel2$rel2, rel3 = rel3$rel3)
library(openxlsx)
write.xlsx(relx4, file = "/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/relx4.xlsx", rowNames = FALSE)





relunlisted <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/Fasting R scripts/Fasting/Signal Subset Unlisted.xlsx")

rel4irrx <- data.frame(Excerpt.x = Excerpt.x$Excerpt.x, rel1 = rel1$rel1, rel2 = rel2$rel2, rel3 = rel3$rel3)

