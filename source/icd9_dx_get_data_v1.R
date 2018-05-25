# |----------------------------------------------------------------------------------|
# | Project: ICD-9 Shiny App                                                         |
# | Script: functins for getting specific ICD-9 version                              |
# | Authors: Davit Sargsyan                                                          |   
# | Created: 05/24/2018                                                              |
# |----------------------------------------------------------------------------------|
options(stringsAsFactors = FALSE)
require(icd.data)

# Get all nonbillable ICD9 DX codes and labels----
icd9cm_dx_get_nonbillable <- function() {
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
  icd9cm_nonbillable <- data.frame(do.call("rbind", out))
  return(icd9cm_nonbillable)
}

# # ATTENTION (05/24/2018, DS)! overlapping sub-chapters:----
# tmp <- icd9cm_nonbillable[duplicated(icd9cm_nonbillable$major), ]
# tmp <- icd9cm_nonbillable[icd9cm_nonbillable$major %in% tmp$major, ]
# write.csv(tmp,
#           file = "tmp/overlapping_sub_chapters.csv",
#           row.names = FALSE)
# Sumbitted to GitHub: https://github.com/jackwasey/icd.data/issues/2

# NOTE (DS, 05/24/2018): there are wholes in sub-chapters, e.g. between 
# 280-289: Diseases Of The Blood And Blood-Forming Organs, and 740-759: Congenital Anomalies
# Opened new issue: https://github.com/jackwasey/icd.data/issues/1

# Get all available ICD-9 versions----
available_icd9_versions <- function() {
  names(icd9cm_billable)
}

# Get merged data for a given ICD-9 version----
icd9cm_merge_version_dx <- function(icd9_version) {
  icd9cm_nonbillable <- icd9cm_dx_get_nonbillable()
  icd9cm_billable_vx <- icd9cm_billable[[which(names(icd9cm_billable) == icd9_version)]]
  
  icd9cm_billable_vx$major_code <- substr(icd9cm_billable_vx$code, 1, 3)
  icd9cm_billable_vx$major_code[substr(icd9cm_billable_vx$code, 1, 1) == "E"] <- 
    substr(icd9cm_billable_vx$code[substr(icd9cm_billable_vx$code, 1, 1) == "E"], 1, 4)

  icd9cm_vx <- merge(icd9cm_nonbillable,
                     icd9cm_billable_vx,
                     by = "major_code")
  
  # For some versions (<=26) no long description is available
  # Set to short description
  icd9cm_vx$long_desc[is.na(icd9cm_vx$long_desc)] <- 
    icd9cm_vx$short_desc[is.na(icd9cm_vx$long_desc)]
  
  return(icd9cm_vx)
}
