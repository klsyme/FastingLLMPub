source("GPT Run.R")

## This creates a data frame where each row has one paragraph
paragraphs <- 
  read_excel("Costs Subset Unlisted.xlsx") |> 
  mutate(
    Excerpt.x = str_remove_all(Excerpt.x, "\\{\\{\\d+\\}\\}"),    
    Excerpt.x = str_remove_all(Excerpt.x, "[^\x1F-\x7F]+")      
  ) 

# Prompt for mat1 -- next round must work on output instructions
prompt <- "I will give you a paragraph of text on fasting under the heading Excerpt.x. I would like you to annotate the texts as follows based on the type of cost a faster or fasters are experiencing: Code '1' if the text explicitly mentions that sex is prohibited for the faster or fasters during or around the time of fasting; code a '0' if there is no explicit mention that sex is prohibited for the faster or fasters during or around the time of fasting. Here is the paragraph:"
  
  
# Generate mat 1 column vars
gptCosts <- paragraphs
#gptCosts$mat1 <- map_chr(paste(prompt, gptCosts$Excerpt.x), hey_chatGPT)

# Export and Clean the output for IRR
#write.xlsx(gptCosts, file = "/Users/kristensyme/Desktop/R Projects/Fasting/Fasting R scripts/Fasting/gptMat1.xlsx")

# Import cleaned output for mat1
mat1 <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/Fasting R scripts/Fasting/gptMat1.xlsx", na = 'na')

# Mat 1 irr with resolved codes
mat1irr <- mat1 %>% select(mat, mat1)
agree(mat1irr)
gwet.ac1.raw(mat1irr, weights = "unweighted")
kappa2(mat1irr[, c("mat", "mat1")], weight = "equal")
# Run again
mat1irr2 <- mat1irr %>% select(mat, mat1)
agree(mat1irr2)

# Filter the rows where mat and mat1 are different and select the desired columns
different_rows <- mat1 %>%
  filter(mat != mat1) %>%
  select(Excerpt.x, mat, mat1)

# Print the result
print(different_rows)

# Prompt for mat2
prompt <- "I will give you a paragraph of text on fasting under the heading Excerpt.x. I would like you to annotate the texts as follows based on the type of cost a faster or fasters are experiencing: Code a numerical 1 if the text explicitly mentions that sex is prohibited for the faster or fasters during or around the time of fasting; code a numerical 0 if there is no explicit mention that sex is prohibited for the faster or fasters during or around the time of fasting; please do not provide any other information in your response besides the 1 or the 0. Here is the paragraph:"

# Generate mat 2 column vars
# gptCosts <- paragraphs (don't need to include this on each round)
# gptCosts$mat2 <- map_chr(paste(prompt, gptCosts$Excerpt.x), hey_chatGPT)

# Mat 2 irr with resolved codes and mat1
# Create data frame for excerpt.x, mat, mat1, mat2
mat2irr <- data.frame(
  mat = mat1irr$mat,
  mat1 = mat1irr$mat1,
  mat2 = gptCosts$mat2
)

agree(mat2irr$mat1, mat2irr$mat2) #perfect reliability
gwet.ac1.raw(mat2irr$mat1, mat2irr$mat2, weights = "unweighted")
kappa2(mat2irr[, c("mat2", "mat1")], weight = "equal")

# Don't need to do diff rows since the output is the same is the mat1

# Create new data frame to export mat1 and mat2 with gpt Costs
gptcostsmat <- data.frame(
  gptCosts,
  mat1 = mat1irr$mat1
)

# Extract the desired columns from each dataframe
mat <- gptCosts %>% select(mat)
Excerpt.x <- gptCosts %>% select(Excerpt.x)
mat1 <- mat2irr %>% select(mat1)
mat2 <- mat2irr %>% select(mat2)

mat2irrx <- data.frame(Excerpt.x = Excerpt.x$Excerpt.x, mat = mat$mat, mat1 = mat1$mat1, mat2 = mat2$mat2)


## Exporting 3 files based on this session gptCosts, mat2irr, different_rows
write.xlsx(different_rows, file = "/Users/kristensyme/Desktop/R Projects/Fasting/Fasting R scripts/Fasting/mat different rows KSGPT.xlsx", rowNames = FALSE)
write.xlsx(mat2irr, file = "/Users/kristensyme/Desktop/R Projects/Fasting/Fasting R scripts/Fasting/matIRR.xlsx", rowNames = FALSE)
# write.xlsx(mat2irrx, file = "/Users/kristensyme/Desktop/R Projects/Fasting/Fasting R scripts/Fasting/mat2irr.xlsx", rowNames = FALSE)



