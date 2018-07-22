# devtools::install_github("jackwasey/icd")
# devtools::install_github("jackwasey/icd.data")


require(data.table)
require(icd)

# ICD-9----
# Flat table of latest ICD-9-CM diagnosis codes
head(icd9cm_hierarchy)

# Chapters
head(icd9_chapters)

# Sub-chapters
head(icd9_sub_chapters)

# Major
head(icd9_majors)

# Function to get all onbillable ICD9 codes and labels----
icd9cm_get_nonbillable <- function() {
  options(stringsAsFactors = FALSE)
  require(icd.data)
  require(data.table)
  icd9chap <- data.frame(do.call("rbind",
                                 icd9_chapters))
  icd9schap <- data.frame(do.call("rbind",
                                  icd9_sub_chapters))
  out <- list()
  for (i in 1:length(icd9_majors)) {
    ndx.chap <- (icd9chap$start <= icd9_majors[i] &
      icd9chap$end >= icd9_majors[i])
    ndx.schap <- (icd9schap$start <= icd9_majors[i] &
      icd9schap$end >= icd9_majors[i])
    out[[i]] <- data.frame(chapter = rownames(icd9chap)[ndx.chap],
                           chapter_start = icd9chap$start[ndx.chap],
                           chapter_end = icd9chap$end[ndx.chap],
                           sub_chapter = if (sum(ndx.schap) > 0) rownames(icd9schap)[ndx.schap] else NA,
                           sub_chapter_start = if (sum(ndx.schap) > 0) icd9schap$start[ndx.schap] else NA,
                           sub_chapter_end = if (sum(ndx.schap) > 0) icd9schap$end[ndx.schap] else NA,
                           major = names(icd9_majors)[i],
                           major_code = icd9_majors[i])
  }
  icd9cm__nonbillable <- data.table(do.call("rbind", out))
  return(icd9cm__nonbillable)
}

available_icd9_versions <- function() {
  require(icd.data)
  names(icd9cm_billable)
}

available_icd9_versions()

icd9_version = available_icd9_versions()[1]
icd9_version

icd9cm_merge_version_x <- function(icd9_version) {
  icd9cm_nonbillable <- icd9cm_get_nonbillable()
  icd9cm_billable_vx <- icd9cm_billable[[which(names(icd9cm_billable) == icd9_version)]]

  icd9cm_billable_vx$major_code <- substr(icd9cm_billable_vx$code, 1, 3)
  icd9cm_billable_vx$major_code[substr(icd9cm_billable_vx$code, 1, 1) == "E"] <- 
    substr(icd9cm_billable_vx$code[substr(icd9cm_billable_vx$code, 1, 1) == "E"], 1, 4)
  # head(icd9cm_billable_vx)
  
  icd9cm_vx <- merge(icd9cm_nonbillable,
                     icd9cm_billable_vx,
                     by = "major_code")
  
  # For some versions (<=26) no long description is available
  # Set to short description
  icd9cm_vx$long_desc[is.na(icd9cm_vx$long_desc)] <- 
    icd9cm_vx$short_desc[is.na(icd9cm_vx$long_desc)]
  return(icd9cm_vx)
}

icd9cm_vx <- icd9cm_merge_version_x(26)

# All available verions of ICD9 dx (V23-V32)----
length(icd9cm_billable)


# V32 only
head(icd9cm_billable[[1]])

# ICD-10----
# Flat table of latest ICD-9-CM diagnosis codes
head(icd10cm_hierarchy)
# DOES OT EXIST!

# Chapters
head(icd10_chapters)

# Sub-chapters
head(icd10_sub_chapters)

# Major
head(icd10_majors)

# Procedures
length(icd10_pcs)
nrow(icd10_pcs[[1]])
head(icd10_pcs[[1]])

# The following data sets were created in 'icd9-sg_get_data_v1.R' script----
load("data/icd9_pcs_chapters.RData")
icd9_pcs_chapters

load("data/icd9_pcs_sub_chapters.RData")
icd9_pcs_sub_chapters

load("data/icd9cm_pcs_billable.RData")
names(icd9cm_pcs_billable)
head(icd9cm_pcs_billable[[which(names(icd9cm_pcs_billable) == 32)]])

# Part II: pre-defined mappings----
# a. Revised Elixhauser system----
tmp <- icd9_map_ahrq
tmp