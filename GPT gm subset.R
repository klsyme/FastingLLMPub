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


# Prompt for gm1
prompt <- "I will give you a paragraph of text on fasting under the heading Excerpt.x. I would like you to annotate the text as follows based on the type of signal observers perceive in a faster or group of fasters. Code a numerical 1 if the text explicitly mentions that observers detect, perceive, or infer commitment or solidarity with or to group by the act of fasting. Code a numerical 0 if there is no mention in the text that observers detect, perceive, or infer commitment or solidarity with or to group by the act of fasting. For example, fasting might be done in a group, but if the text does not mention that others infer group commitment, then code 0. Signals of group commitment and solidarity can be mediated through religious observance, but if the text does not mention commitment specifically to a group of people, as opposed to an ideal or faith. For example, if the text states that fasting displays solidarity to a collective group, then code 1, but if the text states that fasting displays faith rather than solidarity with other believers, then code 0. Please do not provide any other information in your response besides the 1 or the 0. Here is the paragraph:"

# Generate gm1
gptSignal <- paragraphs
# gptSignal$gm1 <- map_chr(paste(prompt, gptSignal$Excerpt.x), hey_chatGPT)

# gm1 irr with resolved codes
gm1irr <- gptSignal %>% select(gm, gm1)
agree(gm1irr)
gwet.ac1.raw(gm1irr, weights = "unweighted")
kappa2(gm1irr[, c("gm", "gm1")], weight = "equal")

# Create matrix including Excerpt.x to compare diffs
gm1irrx <- gptSignal %>% select(Excerpt.x, gm, gm1)

# Filter the rows where gm and gm1 are different and select the desired columns
diffrows_gm1 <- gm1irrx %>%
  filter(gm != gm1) %>%
  select(Excerpt.x, gm, gm1)

# Generate gm2
gptSignal <- paragraphs
# gptSignal$gm2 <- map_chr(paste(prompt, gptSignal$Excerpt.x), hey_chatGPT)

# gm1 irr with itself
gm2 <- gptSignal %>% select(gm2)
gm1 <- gm1irr %>% select(gm1)
# Create matrix including Excerpt.x to compare diffs
gm2irr <- data.frame(gm1 = gm1$gm1, gm2 = gm2$gm2)
agree(gm2irr)
gwet.ac1.raw(gm2irr, weights = "unweighted")
kappa2(gm2irr[, c("gm1", "gm2")], weight = "equal")

# Create matrix including Excerpt.x to compare diffs
diffrows_gm2 <- gptSignal %>% select(Excerpt.x, gm, gm2)

# Filter the rows where gm and gm1 are different and select the desired columns
diffrows_gm2 <- gm2irrx %>%
  filter(gm != gm2) %>%
  select(Excerpt.x, gm, gm2)

# Extract the desired columns from each dataframe
gm <- gptSignal %>% select(gm)
gm1 <- gm1irr %>% select(gm1)
gm2 <- gm2irrx %>% select(gm2)
Excerpt.x <- gptSignal %>% select(Excerpt.x)
# Combine the extracted columns into a new dataframe
gm2irrx <- data.frame(Excerpt.x = Excerpt.x$Excerpt.x, gm = gm$gm, gm2 = gm2$gm2, gm1 = gm1$gm1)

## Exporting files based on this session 
#write.xlsx(diffrows_gm1, file = "/Users/kristensyme/Desktop/R Projects/Fasting/Fasting R scripts/Fasting/gm1 different rows KSGPT.xlsx", rowNames = FALSE)
#write.xlsx(diffrows_gm2, file = "/Users/kristensyme/Desktop/R Projects/Fasting/Fasting R scripts/Fasting/gm2 different rows KSGPT.xlsx", rowNames = FALSE)
#write.xlsx(gm2irrx, file = "/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/gm3irr.xlsx", rowNames = FALSE)


