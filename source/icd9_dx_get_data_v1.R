# |----------------------------------------------------------------------------------|
# | Project: ICD-9 Shiny App                                                         |
# | Script: functins for getting specific ICD-9 version                              |
# | Authors: Davit Sargsyan                                                          |   
# | Created: 05/24/2018                                                              |
# |----------------------------------------------------------------------------------|
options(stringsAsFactors = FALSE)
require(icd.data)

# Issues----
# Issue2 (05/24/2018, DS): overlapping sub-chapters:----
# https://github.com/jackwasey/icd.data/issues/2
# icd9cm_nonbillable <- icd9cm_dx_get_nonbillable()
# tmp <- icd9cm_nonbillable[duplicated(icd9cm_nonbillable$major), ]
# tmp <- icd9cm_nonbillable[icd9cm_nonbillable$major %in% tmp$major, ]
# tmp
# write.csv(tmp,
#           file = "tmp/overlapping_sub_chapters.csv",
#           row.names = FALSE)
# For now, remove the following categories manually:
# 1. Psychoses
# 2. Fractures
# 3. Open Wounds
# 4. Transport Accidents

# Get all nonbillable ICD9 DX codes and labels----
icd9cm_dx_get_nonbillable <- function() {
  icd9chap <- data.frame(do.call("rbind",
                                 icd9_chapters))
  icd9schap <- data.frame(do.call("rbind",
                                  icd9_sub_chapters))
  
  # TEMPORARY FIX for ISSUE 2 (05/25/2018): remove 4 super-sub-chapters
  icd9schap <- icd9schap[!(rownames(icd9schap) %in% c("Psychoses",
                                                     "Fractures",
                                                     "Open Wounds",
                                                     "Transport Accidents")), ]
  
  out <- list()
  for (i in 1:length(icd9_majors)) {
    ndx.chap <- (icd9chap$start <= icd9_majors[i] &
                   icd9chap$end >= icd9_majors[i])
    ndx.schap <- (icd9schap$start <= icd9_majors[i] &
                    icd9schap$end >= icd9_majors[i])
    
    out[[i]] <- data.frame(chapter = rownames(icd9chap)[ndx.chap],
                           chapter_start = icd9chap$start[ndx.chap],
                           chapter_end = icd9chap$end[ndx.chap],
                           sub_chapter = if (sum(ndx.schap) > 0) rownames(icd9schap)[ndx.schap] else rownames(icd9chap)[ndx.chap],
                           sub_chapter_start = if (sum(ndx.schap) > 0) icd9schap$start[ndx.schap] else icd9chap$start[ndx.chap],
                           sub_chapter_end = if (sum(ndx.schap) > 0) icd9schap$end[ndx.schap] else icd9chap$end[ndx.chap],
                           major = names(icd9_majors)[i],
                           major_code = icd9_majors[i])
  }
  icd9cm_nonbillable <- data.frame(do.call("rbind", out))
  # head(icd9cm_nonbillable)

  return(icd9cm_nonbillable)
}

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