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

# Prompt for lap.gpt
prompt <- "I will give you a paragraph of text on fasting under the heading Excerpt.x. I would like you to annotate the text as follows based on whether the description of fasting is said to have occurred in the past and is no longer practiced. Code a numerical 1 if the text indicates that fasting as described in the text occurred in the past and was no longer practiced. It is not sufficient for the events to be discussed in the past tense. There must be explicit mention that the rituals associated with fasting have significantly changed or are no longer practiced. Code a numerical 0 if there is no mention if the fasting as described in the text occurred in the past, or if the text indicates that the fasting as described in the text is still practiced. For example, if the text uses phrases like ‘in the past’ to describe fasting or references or alludes to ‘pre-colonial times’, then code 1. Please do not provide any other information in your response besides the 1 or the 0. Here is the paragraph:"

# Generate lap.gpt
#paragraphs$lap.gpt <- map_chr(paste(prompt, paragraphs$Excerpt.x), hey_chatGPT)

table(paragraphs$lap.gpt)

# Replace the specific incorrect entry with a single numeric 1
#paragraphs$lap.gpt[paragraphs$lap.gpt == "1\n1\n0\n0\n0\n0\n0\n0\n0\n0\n0\n0\n0"] <- 1

# lap.gpt irr with coder 1
lapirr <- paragraphs %>% select(time_lapse.x, lap.gpt)
agree(lapirr)
gwet.ac1.raw(lapirr, weights = "unweighted")
kappa2(lapirr[, c("time_lapse.x", "lap.gpt")], weight = "equal")

# lap.gpt irr with coder 2
lapirr2 <- paragraphs %>% select(time_lapse.y, lap.gpt)
agree(lapirr2)
gwet.ac1.raw(lapirr2, weights = "unweighted")
kappa2(lapirr2[, c("time_lapse.y", "lap.gpt")], weight = "equal")

# human coders agreement
lapirr3 <- paragraphs %>% select(time_lapse.y, time_lapse.x)
agree(lapirr3)
gwet.ac1.raw(lapirr3, weights = "unweighted")
kappa2(lapirr3[, c("time_lapse.y", "time_lapse.x")], weight = "equal")

write.xlsx(paragraphs, file = "/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/paragraphslap.xlsx", rowNames = FALSE)
