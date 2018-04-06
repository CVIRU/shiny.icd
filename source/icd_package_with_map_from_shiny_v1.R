# |----------------------------------------------------------------------------------|
# | Project: ICD-9 Shiny App                                                         |
# | Script: ICD-9 codes using our Shiny app mapping file with package 'icd'          |
# | Authors: Davit Sargsyan                                                          |   
# | Created: 04/04/2018                                                              |
# | Modified:                                                                        |
# |----------------------------------------------------------------------------------|
require(icd)
require(data.table)

# Part I: Load 'l1' from Shiny mapping file
load("docs/icd9_map_2018-04-04.RData")
l1
# $`Essential hypertension`
# [1] "4010" "4011"
# 
# $`Hypertensive heart disease`
# [1] "40201" "40211" "40290"
# 
# $`Acute myocardial infarction`
# [1] "41001" "41002" "41010" "41011"
# 
# $`Acute pulmonary heart disease`
# [1] "4150"  "41511" "41513"
# 
# $`Other and unspecified intracranial hemorrhage`
# [1] "4320" "4321"

class(l1)
# "icd_comorbidity_map" "list" 

# Part II: load simulated data----
load("data/dt.sim.RData")
dt.sim

# Separate visit numbers and diagnoses only----
tmp <- data.table(visit = 1:nrow(dt.sim),
                  dt.sim[, DX1:DX9])
tmp

# # How many comorbidities each record has?-----
# ?icd_count_codes_wide
# dtt <- icd_count_codes_wide(x = tmp,
#                             visit_name = "visit",
#                             return_df = TRUE)
# head(dtt)

# List of data frames with comorbidities, one per DX----
dtt <- list()
for(i in 1:9){
  dtt[[i]] <- icd9_comorbid(x = tmp,
                            map = l1,
                            visit_name = "visit",
                            icd_name = names(tmp)[i + 1])
}
head(dtt[[1]])
dt2 <- Reduce("+", dtt)
head(dt2)
sum(rowSums(dt2))


# Part III: load MIDAS----
load("E:/MIDAS/midas15_clean.RData")
midas15

tmp <- data.table(visit = 1:nrow(midas15),
                  midas15[, DX1:DX9])
tmp
rm(midas15)
gc()

dtt <- list()
system.time({
  for(i in 1:9){
    dtt[[i]] <- icd9_comorbid(x = tmp,
                              map = l1,
                              visit_name = "visit",
                              icd_name = names(tmp)[i + 1])
  }
})
# Not too slow and low memory use!
# user  system elapsed 
# 1409.95   26.21 1442.20 
gc()
head(dtt[[1]])
dt1 <- Reduce("+", dtt)
gc()
head(dt1)
sum(rowSums(dt1) > 0)
# 632,982 records had at least one comorid condition

dt1 <- apply(dt1, 2, function(a) a > 0)
head(dt1)
sum(rowSums(dt1) > 0)
# 632,982 records had at least one comorid condition