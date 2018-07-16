# devtools::install_github("jackwasey/icd")
# devtools::install_github("jackwasey/icd.data")


#' Function to get all onbillable ICD9 codes and labels
#' @import icd.data
#' @import data.table
#' @export
icd9cm_get_nonbillable <- function() {
  icd9chap <- data.frame(do.call("rbind",
                                 icd9_chapters))
  icd9schap <- data.frame(do.call("rbind",
                                  icd9_sub_chapters))
  out <- list()
  for (i in seq_along(icd.data::icd9_majors)) {
    ndx.chap <- (icd9chap$start <= icd.data::icd9_majors[i] &
                   icd9chap$end >= icd.data::icd9_majors[i])
    ndx.schap <- (icd9schap$start <= icd.data::icd9_majors[i] &
                    icd9schap$end >= icd.data::icd9_majors[i])
    out[[i]] <- data.frame(chapter = rownames(icd9chap)[ndx.chap],
                           chapter_start = icd9chap$start[ndx.chap],
                           chapter_end = icd9chap$end[ndx.chap],
                           sub_chapter = if (sum(ndx.schap) > 0) rownames(icd9schap)[ndx.schap] else NA,
                           sub_chapter_start = if (sum(ndx.schap) > 0) icd9schap$start[ndx.schap] else NA,
                           sub_chapter_end = if (sum(ndx.schap) > 0) icd9schap$end[ndx.schap] else NA,
                           major = names(icd.data::icd9_majors)[i],
                           major_code = icd.data::icd9_majors[i])
  }
  icd9cm__nonbillable <- data.table(do.call("rbind", out))
  return(icd9cm__nonbillable)
}

#' show available ICD-9 versions
#' @examples
#' \dontrun{
#' available_icd9_versions()
#' names(icd.data::icd9cm_billable)
#' icd9cm_vx <- icd9cm_merge_version_x(26)
#' }
#' @export
available_icd9_versions <- function() {
  names(icd.data::icd9cm_billable)
}

icd9cm_merge_version_x <- function(icd9_version = available_icd9_versions()[1]) {
  icd9cm_nonbillable <- icd9cm_get_nonbillable()
  icd9cm_billable_vx <- icd9cm_billable[[which(names(icd.data::icd9cm_billable) == icd9_version)]]

  icd9cm_billable_vx$major_code <- substr(icd.data::icd9cm_billable_vx$code, 1, 3)
  icd9cm_billable_vx$major_code[substr(icd.data::icd9cm_billable_vx$code, 1, 1) == "E"] <-
    substr(icd9cm_billable_vx$code[substr(icd.data::icd9cm_billable_vx$code, 1, 1) == "E"], 1, 4)

  icd9cm_vx <- merge(icd9cm_nonbillable,
                     icd.data::icd9cm_billable_vx,
                     by = "major_code")

  # For some versions (<=26) no long description is available
  # Set to short description
  icd9cm_vx$long_desc[is.na(icd9cm_vx$long_desc)] <-
    icd9cm_vx$short_desc[is.na(icd9cm_vx$long_desc)]
  return(icd9cm_vx)
}

