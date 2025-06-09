library(irr)
library(irrCAC)
library(readxl)

#GENDER IRR
gender <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/gen2irrx2.xlsx")
gender2 <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/Leveraging the Power of LLMs copy/LLM paper/Diffs Fasting/gentf_updated.xlsx")

#Remove row 205
gender <- gender %>% 
  slice(-205)  # Removes row 205

# gen1 irr with resolved codes
gen1irr <- gender %>% select(gender.r, gen1)
agree(gen1irr)
gwet.ac1.raw(gen1irr, weights = "unweighted")
kappa2(gen1irr[, c("gender.r", "gen1")], weight = "equal")

# lead2 irr with itself
gen2irr <- gender %>% select(gen1, gen2)
agree(gen2irr)
gwet.ac1.raw(gen2irr, weights = "unweighted")
kappa2(gen2irr[, c("gen1", "gen2")], weight = "equal")


#LEADER IRR
lead <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/lead2irrx2.xlsx")

#Remove row 205
lead <- lead %>% 
  slice(-205)  # Removes row 205

# lead1 irr with resolved codes
lead1irr <- lead %>% select(leader.r, lead1)
agree(lead1irr)
gwet.ac1.raw(lead1irr, weights = "unweighted")
kappa2(lead1irr[, c("leader.r", "lead1")], weight = "equal")

# lead2 irr with itself
lead2irr <- lead %>% select(lead1, lead2)
agree(lead2irr)
gwet.ac1.raw(lead2irr, weights = "unweighted")
kappa2(lead2irr[, c("lead1", "lead2")], weight = "equal")

#LAP IRR
lap <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/lap2irrx2.xlsx")

#Remove row 205
lap <- lap %>% 
  slice(-205)  # Removes row 205

# lap1 irr with resolved codes
lap1irr <- lap %>% select(time_lapse.r, lap1)
agree(lap1irr)
gwet.ac1.raw(lap1irr, weights = "unweighted")
kappa2(lap1irr[, c("time_lapse.r", "lap1")], weight = "equal")

# lap2 irr with itself
lap2irr <- lap %>% select(lap1, lap2)
agree(lap2irr)
gwet.ac1.raw(lap2irr, weights = "unweighted")
kappa2(lap2irr[, c("lap1", "lap2")], weight = "equal")


#ASC IRR
asc <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/asc3IRR.xlsx")

#Remove row 205
asc <- asc %>% 
  slice(-205)  # Removes row 205

ascirr1 <- asc %>% select(asc, asc2)
agree(ascirr1)
gwet.ac1.raw(ascirr1, weights = "unweighted")
kappa2(ascirr1[, c("asc", "asc2")], weight = "equal")

ascirr2 <- asc %>% select(asc2, asc3)
agree(ascirr2)
gwet.ac1.raw(ascirr2, weights = "unweighted")
kappa2(ascirr2[, c("asc2", "asc3")], weight = "equal")

#VK IRR
vk <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/vk2IRR.xlsx")

#Remove row 205
vk <- vk %>% 
  slice(-205)  # Removes row 205

# vk1 irr with resolved codes
vk1irr <- vk %>% select(vk, vk1)
agree(vk1irr)
gwet.ac1.raw(vk1irr, weights = "unweighted")
kappa2(vk1irr[, c("vk", "vk1")], weight = "equal")

# vk1 and vk2 irr 
vk2irr <- vk %>% select(vk1, vk2)
agree(vk2irr)
gwet.ac1.raw(vk2irr, weights = "unweighted")
kappa2(vk2irr[, c("vk1", "vk2")], weight = "equal")

#DEI IRR
dei <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/dei2IRR.xlsx")

#Remove row 205
dei <- dei %>% 
  slice(-205)  # Removes row 205

# dei1 irr with resolved codes
dei1irr <- dei %>% select(dei, dei1)
agree(dei1irr)
gwet.ac1.raw(dei1irr, weights = "unweighted")
kappa2(dei1irr[, c("dei", "dei1")], weight = "equal")

# dei2 irr with resolved codes
dei2irr <- dei %>% select(dei1, dei2)
agree(dei2irr)
gwet.ac1.raw(dei2irr, weights = "unweighted")
kappa2(dei2irr[, c("dei1", "dei2")], weight = "equal")

#SE IRR
se <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/se2IRR.xlsx")

#Remove row 205
se <- se %>% 
  slice(-205)  # Removes row 205

# se1 irr with resolved codes
se1irr <- se %>% select(se, se1)
agree(se1irr)
gwet.ac1.raw(se1irr, weights = "unweighted")
kappa2(se1irr[, c("se", "se1")], weight = "equal")

# se2 irr with resolved codes
se2irr <- se %>% select(se1, se2)
agree(se2irr)
gwet.ac1.raw(se2irr, weights = "unweighted")
kappa2(se2irr[, c("se1", "se2")], weight = "equal")

#SCA IRR
sca <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/sca2IRR.xlsx")

#Remove row 205
sca <- sca %>% 
  slice(-205)  # Removes row 205

# sca1 irr with resolved codes
sca1irr <- sca %>% select(sca, sca1)
agree(sca1irr)
gwet.ac1.raw(sca1irr, weights = "unweighted")
kappa2(sca1irr[, c("sca", "sca1")], weight = "equal")

# sca2 irr with resolved codes
sca2irr <- sca %>% select(sca1, sca2)
agree(sca2irr)
gwet.ac1.raw(sca2irr, weights = "unweighted")
kappa2(sca2irr[, c("sca1", "sca2")], weight = "equal")

#SW IRR
sw <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/sw3IRR.xlsx")

#Remove row 205
sw <- sw %>% 
  slice(-205)  # Removes row 205

#sw with resolved codes
sw1irr <- sw %>% select(sw, sw2)
agree(sw1irr)
gwet.ac1.raw(sw1irr, weights = "unweighted")
kappa2(sw1irr[, c("sw", "sw2")], weight = "equal")

sw2irr <- sw %>% select(sw2, sw3)
agree(sw2irr)
gwet.ac1.raw(sw2irr, weights = "unweighted")
kappa2(sw2irr[, c("sw2", "sw3")], weight = "equal")

#MAT IRR
mat <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/mat2irr.xlsx")

#Remove row 205
mat <- mat %>% 
  slice(-205)  # Removes row 205

# mat1 irr with resolved codes
mat1irr <- mat %>% select(mat, mat1)
agree(mat1irr)
gwet.ac1.raw(mat1irr, weights = "unweighted")
kappa2(mat1irr[, c("mat", "mat1")], weight = "equal")

# mat2 irr with resolved codes
mat2irr <- mat %>% select(mat1, mat2)
agree(mat2irr)
gwet.ac1.raw(mat2irr, weights = "unweighted")
kappa2(mat2irr[, c("mat1", "mat2")], weight = "equal")

#REL IRR
rel <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/relx4.xlsx")

#Remove row 205
rel <- rel %>% 
  slice(-205)  # Removes row 205

# rel1 irr with resolved codes
rel1irr <- rel %>% select(rel, rel2)
agree(rel1irr)
gwet.ac1.raw(rel1irr, weights = "unweighted")
kappa2(rel1irr[, c("rel", "rel2")], weight = "equal")

# rel2 irr with resolved codes
rel2irr <- rel %>% select(rel2, rel3)
agree(rel2irr)
gwet.ac1.raw(rel2irr, weights = "unweighted")
kappa2(rel2irr[, c("rel2", "rel3")], weight = "equal")

#GM IRR
gm <- read_excel("/Users/kristensyme/Desktop/R Projects/Fasting/GPT Analyses Output/gm3irr.xlsx")

#Remove row 205
gm <- gm %>% 
  slice(-205)  

# gm1 irr with resolved codes
gm1irr <- gm %>% select(gm, gm1)
agree(gm1irr)
gwet.ac1.raw(gm1irr, weights = "unweighted")
kappa2(gm1irr[, c("gm", "gm1")], weight = "equal")

# gm2 irr with resolved codes
gm2irr <- gm %>% select(gm1, gm2)
agree(gm2irr)
gwet.ac1.raw(gm2irr, weights = "unweighted")
kappa2(gm2irr[, c("gm1", "gm2")], weight = "equal")



