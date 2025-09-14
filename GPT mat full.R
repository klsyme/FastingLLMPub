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

# Prompt for mat.gpt
prompt <- "I will give you a paragraph of text on fasting under the heading Excerpt.x. I would like you to annotate the texts as follows based on the type of cost a faster or fasters are experiencing: Code a numerical 1 if the text explicitly mentions that sex is prohibited for the faster or fasters during or around the time of fasting; code a numerical 0 if there is no explicit mention that sex is prohibited for the faster or fasters during or around the time of fasting; please do not provide any other information in your response besides the 1 or the 0. Here is the paragraph:"
  
# Generate mat.gpt
#paragraphs$mat.gpt <- map_chr(paste(prompt, paragraphs$Excerpt.x), hey_chatGPT)

table(paragraphs$mat.gpt)

# Replace the specific incorrect entry with a single numeric 1
#paragraphs$mat.gpt[paragraphs$mat.gpt == "1\n1\n0\n0\n0\n0\n0\n0\n0\n0\n0\n0\n0"] <- 1

# mat.gpt irr with coder 1
matirr <- paragraphs %>% select(mat.x, mat.gpt)
agree(matirr)
gwet.ac1.raw(matirr, weights = "unweighted")
kappa2(matirr[, c("mat.x", "mat.gpt")], weight = "equal")

# mat.gpt irr with coder 2
matirr2 <- paragraphs %>% select(mat.y, mat.gpt)
agree(matirr2)
gwet.ac1.raw(matirr2, weights = "unweighted")
kappa2(matirr2[, c("mat.y", "mat.gpt")], weight = "equal")

# human coders agreement
matirr3 <- paragraphs %>% select(mat.y, mat.x)
agree(matirr3)
gwet.ac1.raw(matirr3, weights = "unweighted")
kappa2(matirr3[, c("mat.y", "mat.x")], weight = "equal")

write.xlsx(paragraphs, file = "/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/paragraphsmat.xlsx", rowNames = FALSE)
