source("GPT Run.R")

## This creates a data frame where each row has one paragraph
paragraphs <- 
  read_excel("Datasets/Subset Codes Fasting rename.xlsx") |> 
  mutate(
    Excerpt.x = str_remove_all(Excerpt.x, "[{][{][\\d ]+[}][}]"),
    Excerpt.x = str_remove_all(Excerpt.x, "[^\x1F-\x7F]+")
  )

Gender <- paragraphs

library(irr)
library(irrCAC)

# Prompt for gen1
prompt <- "I will give you a paragraph of text on fasting. Code a numerical 1 if the person or people fasting is a man or are men. Code a numerical 2 if the person or people who are fasting is a woman or group of women. You may use clues from the text; for instance, if there is a mention of a faster’s reduced milk supply for breastfeeding due to fasting, then code 2. Code a numerical 3 if the fasting mentioned in the paragraph applies to both men and women or the context makes it clear that both genders fast. Code a numerical 0 if the gender of the people or persons fasting is left unclear in the paragraph. Please do not provide any other information in your response. Here is the paragraph:"
  

# Generate gen1
#Gender$gen1 <- map_chr(paste(prompt, gender$Excerpt.x), hey_chatGPT)

# gen1 irr with resolved codes
gen1irr <- Gender %>% select(gender.r, gen1)
agree(gen1irr)
gwet.ac1.raw(gen1irr, weights = "unweighted")
kappa2(gen1irr[, c("gender.r", "gen1")], weight = "equal")

# Create matrix including Excerpt.x to compare diffs
gen1irrx <- Gender %>% select(Excerpt.x, gender.r, gen1)

# Filter the rows where cs and cs1 are different and select the desired columns
diffrows_gen1 <- gen1irrx %>%
  filter(gender.r != gen1) %>%
  select(Excerpt.x, gender.r, gen1)

# Generate gen2
Gender$gen2 <- map_chr(paste(prompt, Gender$Excerpt.x), hey_chatGPT)

# gen2 irr with itself
gen2irr <- Gender %>% select(gen1, gen2)
agree(gen2irr)
gwet.ac1.raw(gen2irr, weights = "unweighted")
kappa2(gen2irr[, c("gen1", "gen2")], weight = "equal")

# Create matrix including Excerpt.x to compare diffs
gen2irrx <- Gender %>% select(Excerpt.x, gen1, gen2)

# Filter the rows where gen1 and gen2 are different and select the desired columns
diffrows_gen2 <- gen2irrx %>%
  filter(gen1 != gen2) %>%
  select(Excerpt.x, gen1, gen2)

gen2irrx2 <- Gender %>% select(Excerpt.x, gender.r, gen1, gen2)


## Exporting files based on this session 
#write.xlsx(diffrows_gen1, file = "/Users/kristensyme/Desktop/R Projects/Fasting/Fasting R scripts/Fasting/gen1 different rows KSGPT.xlsx", rowNames = FALSE)
#write.xlsx(diffrows_gen2, file = "/Users/kristensyme/Desktop/R Projects/Fasting/Fasting R scripts/Fasting/gen2 different rows KSGPT.xlsx", rowNames = FALSE)
#write.xlsx(gen2irrx2, file = "/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/gen2irrx2.xlsx", rowNames = FALSE)

#Add new Variable for gender combining 0 and 3

Gender <- Gender %>%
  mutate(gender.r2 = ifelse(gender.r == 0, 3, gender.r))

# Prompt for gen3
prompt <- "I will give you a paragraph of text on fasting. Code a numerical 1 if the person or people fasting is a man or are men. Code a numerical 2 if the person or people who are fasting is a woman or group of women. You may use clues from the text; for instance, if there is a mention of a faster’s reduced milk supply for breastfeeding due to fasting, then code 2. Code a numerical 3 if the fasting mentioned in the paragraph applies to both men and women or the context makes it clear that both genders fast. Also, code a numerical 3 if the gender of the people or persons fasting is left unclear in the paragraph. If the text uses male pronouns for a specific person, code 1, and if the text uses female pronouns for a specific person, code 2. If the texts uses male pronouns, but it is not for a specific person, then code 3. If male pronouns are the only evidence that the faster is a man or a group of men, then code 3. Please do not provide any other information in your response. Here is the paragraph"
Gender$gen3 <- map_chr(paste(prompt, Gender$Excerpt.x), hey_chatGPT)

# gen3 irr with gender.r
gen3irr <- Gender %>% select(gender.r2, gen3)
agree(gen3irr)
gwet.ac1.raw(gen3irr, weights = "unweighted")
kappa2(gen3irr[, c("gender.r2", "gen3")], weight = "equal")

# Create matrix including Excerpt.x to compare diffs
gen3irrx <- Gender %>% select(Excerpt.x, gender.r2, gen3)

# Filter the rows where gen1 and gen2 are different and select the desired columns
diffrows_gen3 <- gen3irrx %>%
  filter(gender.r2 != gen3) %>%
  select(Excerpt.x, gender.r2, gen3)

gen3irrx3 <- Gender %>% select(Excerpt.x, gender.r2, gen3)

# Prompt for gen4
prompt <- "I will give you a paragraph of text on fasting. Code a numerical 1 if the person or people fasting is a man or are men. Code a numerical 2 if the person or people who are fasting is a woman or group of women. You may use clues from the text; for instance, if there is a mention of a faster’s reduced milk supply for breastfeeding due to fasting, then code 2. Code a numerical 3 if the fasting mentioned in the paragraph applies to both men and women or the context makes it clear that both genders fast. Code a numerical 0 if the gender of the people or persons fasting is left unclear in the paragraph. If the text uses male pronouns for a specific person, code 1, and if the text uses female pronouns for a specific person, code 2. If the texts uses male pronouns, but it is not for a specific person, then code 0. If male pronouns are the only evidence that the faster is a man or a group of men, then code 0. Please do not provide any other information in your response. Here is the paragraph:"
Gender$gen4 <- map_chr(paste(prompt, Gender$Excerpt.x), hey_chatGPT)

# gen4 irr with gender.r
gen4irr <- Gender %>% select(gender.r2, gen4)
agree(gen4irr)
gwet.ac1.raw(gen3irr, weights = "unweighted")
kappa2(gen3irr[, c("gender.r2", "gen3")], weight = "equal")

# Create matrix including Excerpt.x to compare diffs
gen3irrx <- Gender %>% select(Excerpt.x, gender.r2, gen3)

library(readxl)
gender <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/gen2irrx2.xlsx")



