# Load required packages
library(readxl)
library(tidyverse)
library(irr)
library(irrCAC)

d <- read_excel("Datasets/allsubset_fasting.xlsx")

## Compared to Human Consensus

# Asc
asc2irr <- d %>% select(asc, asc2)
agree(asc2irr)
gwet.ac1.raw(asc2irr, weights = "unweighted")
kappa2(asc2irr[, c("asc", "asc2")], weight = "equal")

# Dei
dei2irr <- d %>% select(dei, dei2)
agree(dei2irr)
gwet.ac1.raw(dei2irr, weights = "unweighted")
kappa2(dei2irr[, c("dei", "dei2")], weight = "equal")

# Genm2 - Gender man
genm2irr <- d %>% select(genm2, genderm.r)
agree(genm2irr)
gwet.ac1.raw(genm2irr, weights = "unweighted")
kappa2(genm2irr[, c("genderm.r", "genm2")], weight = "equal")

# Genw2 - Gender woman
genw2irr <- d %>% select(genw2, genderw.r)
agree(genw2irr)
gwet.ac1.raw(genw2irr, weights = "unweighted")
kappa2(genw2irr[, c("genderw.r", "genw2")], weight = "equal")

# Genb2 - Gender both
genb2irr <- d %>% select(genb2, genderb.r)
agree(genb2irr)
gwet.ac1.raw(genb2irr, weights = "unweighted")
kappa2(genb2irr[, c("genderb.r", "genb2")], weight = "equal")

# Gm
gm2irr <- d %>% select(gm, gm2)
agree(gm2irr)
gwet.ac1.raw(gm2irr, weights = "unweighted")
kappa2(gm2irr[, c("gm", "gm2")], weight = "equal")

# Lap
lap2irr <- d %>% select(lap, lap2)
agree(lap2irr)
gwet.ac1.raw(lap2irr, weights = "unweighted")
kappa2(lap2irr[, c("lap", "lap2")], weight = "equal")

# Lead
lead2irr <- d %>% select(lead, lead2)
agree(lead2irr)
gwet.ac1.raw(lead2irr, weights = "unweighted")
kappa2(lead2irr[, c("lead", "lead2")], weight = "equal")

# Mat
mat2irr <- d %>% select(mat, mat2)
agree(mat2irr)
gwet.ac1.raw(mat2irr, weights = "unweighted")
kappa2(mat2irr[, c("mat", "mat2")], weight = "equal")

# Rel
rel2irr <- d %>% select(rel, rel2)
agree(rel2irr)
gwet.ac1.raw(rel2irr, weights = "unweighted")
kappa2(rel2irr[, c("rel", "rel2")], weight = "equal")

# Sca
sca2irr <- d %>% select(sca, sca2)
agree(sca2irr)
gwet.ac1.raw(sca2irr, weights = "unweighted")
kappa2(sca2irr[, c("sca", "sca2")], weight = "equal")

# Se
se2irr <- d %>% select(se, se2)
agree(se2irr)
gwet.ac1.raw(se2irr, weights = "unweighted")
kappa2(se2irr[, c("se", "se2")], weight = "equal")

# Sw
sw2irr <- d %>% select(sw, sw2)
agree(sw2irr)
gwet.ac1.raw(sw2irr, weights = "unweighted")
kappa2(sw2irr[, c("sw", "sw2")], weight = "equal")

# Vk
vk2irr <- d %>% select(vk, vk2)
agree(vk2irr)
gwet.ac1.raw(vk2irr, weights = "unweighted")
kappa2(vk2irr[, c("vk", "vk2")], weight = "equal")
