# |----------------------------------------------------------------------------------|
# | Project: Ecxample of using package 'icd'                                         |
# | Script: Ecxample of using package 'icd'                                          |
# | Authors: Davit Sargsyan                                                          |   
# | Created: 03/31/2018                                                              |
# |----------------------------------------------------------------------------------|

require(icd)
?icd

require(data.table)
dt1 <- fread("tmp/Aortic Stenosis ICD9 Legend.csv")
dt1

icd::icd_comorbid(x = dt1$ICD9Coded)
icd::

pts <- icd_long_data(visit_name = c("2", "1", "2", "3", "3"),
                     icd9 = c("39891", "40110", "09322", "41514", "39891"))
icd_comorbid(pts, icd9_map_ahrq, short_code = TRUE) # visit_name is now sorted
pts <- icd_long_data(
  visit_name = c("1", "2", "3", "4", "4"),
  icd_name = c("20084", "1742", "30410", "41514", "95893"),
  date = as.Date(c("2011-01-01", "2011-01-02", "2011-01-03",
                   "2011-01-04", "2011-01-04")))
pt_hccs <- icd_comorbid_hcc(pts, date_name = "date")

pts10 <- icd_long_data(
  visit_name = c("a", "b", "c", "d", "e"),
  icd_name = c("I058", NA, "T82817A", "", "I69369"),
  date = as.Date(c("2011-01-01", "2011-01-02", "2011-01-03", "2011-01-03", "2011-01-03")))

icd10_comorbid(pts10, map = icd10_map_ahrq)
length(icd9_majors)


data(package = "icd")
data(icd9_majors)
dt1 <- data.table(diagnosis = names(icd9_majors),
           icd9 = icd9_majors)

data(icd_chapters)
dt1 <- data.table(diagnosis = names(icd9_chapters),
                  icd9 = icd9_chapters)
dt1$diagnosis
write.csv(dt1, 
          file = "tmp/dt1.csv")

# Billable ICD9 codes, revisions 23 to 32
?icd9cm_billable
dt1 <- icd9cm_billable
names(dt1)
head(dt1[[6]])
dt1 <- rbindlist(dt1)
dt1 <- do.call(rbindlist)
dt1

write.csv(dt1, 
          file = "tmp/dt1.csv")


dt1 <- icd9cm_hierarchy
head(dt1)

# New comorbidity map
# Example:
icd10_map_ahrq
class(icd10_map_ahrq)

# New
map1 <- list(a = c("I674",
                   "O10111",
                   "O10112"),
             b = c( "O10113",
                    "O10119",
                    "O1012"))
map1

map1 <- as.icd_comorbidity_map(map1)
map1
class(map1)
