library(writexl)
library(readxl)

#Upload all files
lead <- read_excel("Datasets/lead_updated.xlsx")
#Remove duplicate row 205
lead <- lead %>% 
  slice(-205)  # Removes row 205
lap <- read_excel("Datasets/laptf_updated.xlsx")
asc <- read_excel("Datasets/asctf_updated.xlsx")
vk <- read_excel("Datasets/vktf_updated.xlsx")
dei <- read_excel("Datasets/deitf_updated.xlsx")
se <- read_excel("Datasets/setf_updated.xlsx.xlsx")
sca <- read_excel("Datasets/scatf_updated.xlsx")
sw <- read_excel("Datasets/swtf_updated.xlsx")
mat <- read_excel("Datasets/mattf_updated.xlsx.xlsx")
rel <- read_excel("Datasets/reltf_updated.xlsx")
gm <- read_excel("Datasets/gmtf_updated.xlsx")
gender <- read_excel("Datasets/gentf_updated.xlsx")

allsubset <- data.frame(
  Excerpt = asc$Excerpt.x,
  asc = asc$asc,
  asc2 = asc$asc2,
  asc3 = asc$asc3,
  asc_final = asc$asc_updated,
  dei = dei$dei,
  dei1 = dei$dei1,
  dei2 = dei$dei2,
  dei_final = dei$dei_updated,
  genderm.final = gender$genm_updated,
  genderw.final = gender$genw_updated,
  genderb.final = gender$genb_updated,
  genderm.r = gender$genderm.r,
  genderw.r = gender$genderw.r,
  genderb.r = gender$genderb.r,
  genm1 = gender$genm1,
  genw1 = gender$genw1,
  genb1 = gender$genb1,
  genm2 = gender$genm2,
  genw2 = gender$genw2,
  genb2 = gender$genb2,
  gm = gm$gm,
  gm1 = gm$gm1,
  gm2 = gm$gm2,
  gm_final = gm$gm_updated,
  lap = lap$time_lapse.r,
  lap1 = lap$lap1,
  lap2 = lap$lap2,
  lap_final = lap$lap_updated,
  lead = lead$leader.r,
  lead1 = lead$lead1,
  lead2 = lead$lead2,
  lead_final = lead$leader_updated,
  mat = mat$mat,
  mat1 = mat$mat1,
  mat2 = mat$mat2,
  mat_final = mat$mat_updated,
  rel = rel$rel,
  rel2 = rel$rel2,
  rel3 = rel$rel3,
  rel_final = rel$rel_updated,
  sca = sca$sca,
  sca1 = sca$sca1,
  sca2 = sca$sca2,
  sca_final = sca$sca_updated,
  se = se$se,
  se1 = se$se1,
  se2 = se$se2,
  se_final = se$se_updated,
  sw = sw$sw,
  sw2 = sw$sw2,
  sw3 = sw$sw3,
  sw_final = sw$sw_updated,
  vk = vk$vk,
  vk1 = vk$vk1,
  vk2 = vk$vk2,
  vk_final = vk$vk_updated
)

# Write the allsubset dataframe to an Excel file
write_xlsx(allsubset, path = "/Users/kristensyme/Dropbox/FastingLLMPub/allsubset_fasting.xlsx")
