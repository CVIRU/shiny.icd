# |----------------------------------------------------------------------------------|
# | Project: ICD-9 Shiny App                                                         |
# | Script: Make chapter and subchapter objects for ICD9 procedure codes 9manually)  |
# | Authors: Davit Sargsyan                                                          |   
# | Created: 04/27/2018                                                              |
# | Modified: 05/24/2018, DS: standard way of getting the codes                      |
# |----------------------------------------------------------------------------------|
options(stringsAsFactors = FALSE)
require(foreach)
require(xlsx)

# Get all nonbillable ICD9 PROC codes and labels----
icd9cm_pcs_get_nonbillable <- function() {
  icd9_pcs_chapters <- data.frame(chapter = c("Procedures And Interventions , Not Elsewhere Classified",
                                             "Operations On The Nervous System",
                                             "Operations On The Endocrine System",
                                             "Operations On The Eye",
                                             "Other Miscellaneous Diagnostic And Therapeutic Procedures",
                                             "Operations On The Ear",
                                             "Operations On The Nose, Mouth, And Pharynx",
                                             "Operations On The Respiratory System",
                                             "Operations On The Cardiovascular System",
                                             "Operations On The Hemic And Lymphatic System",
                                             "Operations On The Digestive System",
                                             "Operations On The Urinary System",
                                             "Operations On The Male Genital Organs",
                                             "Operations On The Female Genital Organs",
                                             "Obstetrical Procedures",
                                             "Operations On The Musculoskeletal System",
                                             "Operations On The Integumentary System",
                                             "Miscellaneous Diagnostic And Therapeutic Procedures"),
                                 start = c("00",
                                           "01",
                                           "06",
                                           "08",
                                           "17",
                                           "18",
                                           "21",
                                           "30",
                                           "35",
                                           "40",
                                           "42",
                                           "55",
                                           "60",
                                           "65",
                                           "72",
                                           "76",
                                           "85",
                                           "87"),
                                 end = c("00",
                                         "05",
                                         "07",
                                         "16",
                                         "17",
                                         "20",
                                         "29",
                                         "34",
                                         "39",
                                         "41",
                                         "54",
                                         "59",
                                         "64",
                                         "71",
                                         "75",
                                         "84",
                                         "86",
                                         "99"))
  
  icd9_pcs_sub_chapters <- data.frame(sub_chapter = c("Procedures And Interventions , Not Elsewhere Classified",
                                                     
                                                     "Incision And Excision Of Skull, Brain, And Cerebral Meninges",
                                                     "Other Operations On Skull, Brain, And Cerebral Meninges",
                                                     "Operations On Spinal Cord And Spinal Canal Structures",
                                                     "Operations On Cranial And Peripheral Nerves",
                                                     "Operations On Sympathetic Nerves Or Ganglia",
                                                     
                                                     "Operations On Thyroid And Parathyroid Glands",
                                                     "Operations On Other Endocrine Glands",
                                                     
                                                     "Operations On Eyelids",
                                                     "Operations On Lacrimal System",
                                                     "Operations On Conjunctiva",
                                                     "Operations On Cornea",
                                                     "Operation On Iris, Ciliary Body, Sclera, And Anterior Chamber",
                                                     "Operations On Lens",
                                                     "Operations On Retina, Choroid, Vitreous, And Posterior Chamber",
                                                     "Operations On Extraocular Muscles",
                                                     "Operations On Orbit And Eyeball",
                                                     
                                                     "Other Miscellaneous Procedures",
                                                     
                                                     "Operations On External Ear",
                                                     "Reconstructive Operations On Middle Ear",
                                                     "Other Operations On Middle And Inner Ear",
                                                     
                                                     "Operation On Nose",
                                                     "Operations On Nasal Sinuses",
                                                     "Removal And Restoration Of Teeth",
                                                     "Other Operations On Teeth, Gums, And Alveoli",
                                                     "Operations On Tongue",
                                                     "Operations On Salivary Glands And Ducts",
                                                     "Other Operations On Mouth And Face",
                                                     "Operations On Tonsils And Adenoids",
                                                     "Operation On Pharynx",
                                                     
                                                     "Excision Of Larynx",
                                                     "Other Operations On Larynx And Trachea",
                                                     "Excision Of Lung And Bronchus",
                                                     "Other Operations On Lung And Bronchus",
                                                     "Operations On Chest Wall, Pleura, Mediastinum, And Diaphragm",
                                                     
                                                     "Operations On Valves And Septa Of Heart",
                                                     "Operations On Vessels Of Heart",
                                                     "Other Operations On Heart And Pericardium",
                                                     "Incision, Excision, And Occlusion Of Vessels",
                                                     "Other Operations On Vessels",
                                                     
                                                     "Operations On Lymphatic System",
                                                     "Operations On Bone Marrow And Spleen",
                                                     
                                                     "Operations On Esophagus",
                                                     "Incision And Excision Of Stomach",
                                                     "Other Operations On Stomach",
                                                     "Incision, Excision, And Anastomosis Of Intestine",
                                                     "Other Operations On Intestine",
                                                     "Operations On Appendix",
                                                     "Operations On Rectum, Rectosigmoid, And Perirectal Tissue",
                                                     "Operations On Anus",
                                                     "Operations On Liver",
                                                     "Operations On Gallbladder And Biliary Tract",
                                                     "Operations On Pancreas",
                                                     "Repair Of Hernia",
                                                     "Other Operations On Abdominal Region",
                                                     
                                                     "Operations On Kidney",
                                                     "Operations On Ureter",
                                                     "Operations On Urinary Bladder",
                                                     "Operations On Urethra",
                                                     "Other Operations On Urinary Tract",
                                                     
                                                     "Operations On Prostate And Seminal Vesicles",
                                                     "Operations On Scrotum And Tunica Vaginalis",
                                                     "Operations On Testes",
                                                     "Operations On Spermatic Cord, Epididymis, And Vas Deferens",
                                                     "Operations On Penis",
                                                     
                                                     "Operations On Ovary",
                                                     "Operations On Fallopian Tubes",
                                                     "Operations On Cervix",
                                                     "Other Incision And Excision Of Uterus",
                                                     "Other Operations On Uterus And Supporting Structures",
                                                     "Operations On Vagina And Cul-De-Sac",
                                                     "Operations On Vulva And Perineum",
                                                     
                                                     "Forceps, Vacuum, And Breech Delivery",
                                                     "Other Procedures Inducing Or Assisting Delivery",
                                                     "Cesarean Section And Removal Of Fetus",
                                                     "Other Obstetric Operations",
                                                     
                                                     "Operations On Facial Bones And Joints",
                                                     "Incision, Excision, And Division Of Other Bones",
                                                     "Other Operations On Bones, Except Facial Bones",
                                                     "Reduction Of Fracture And Dislocation",
                                                     "Incision And Excision Of Joint Structures",
                                                     "Repair And Plastic Operations On Joint Structures",
                                                     "Operations On Muscle, Tendon, And Fascia Of Hand",
                                                     "Operations On Muscle, Tendon, Fascia, And Bursa, Except Hand",
                                                     "Other Procedures On Musculoskeletal System",
                                                     
                                                     "Operations On The Breast",
                                                     "Operations On Skin And Subcutaneous Tissue",
                                                     
                                                     "Diagnostic Radiology",
                                                     "Other Diagnostic Radiology And Related Techniques",
                                                     "Interview, Evaluation, Consultation, And Examination",
                                                     "Microscopic Examination--I",
                                                     "Microscopic Examination--Ii",
                                                     "Nuclear Medicine",
                                                     "Physical Therapy, Respiratory Therapy, Rehabilitation, And Related Procedures",
                                                     "Procedures Related To The Psyche",
                                                     "Ophthalmologic And Otologic Diagnosis And Treatment",
                                                     "Nonoperative Intubation And Irrigation",
                                                     "Replacement And Removal Of Therapeutic Appliances",
                                                     "Nonoperative Removal Of Foreign Body Or Calculus",
                                                     "Other Nonoperative Procedures"),
                                     start = substr(100:199, 2, 3),
                                     end = substr(100:199, 2, 3))
  out <- list()
  for (i in 1:nrow(icd9_pcs_sub_chapters)) {
    ndx.chap <- (icd9_pcs_chapters$start <= icd9_pcs_sub_chapters$start[i] &
                   icd9_pcs_chapters$end >= icd9_pcs_sub_chapters$start[i])
    out[[i]] <- data.frame(chapter = icd9_pcs_chapters$chapter[ndx.chap],
                           chapter_start = icd9_pcs_chapters$start[ndx.chap],
                           chapter_end = icd9_pcs_chapters$end[ndx.chap],
                           sub_chapter = icd9_pcs_sub_chapters$sub_chapter[i],
                           sub_chapter_start = icd9_pcs_sub_chapters$start[i],
                           sub_chapter_end = icd9_pcs_sub_chapters$end[i],
                           major = icd9_pcs_sub_chapters$sub_chapter[i],
                           major_code = icd9_pcs_sub_chapters$start[i])
  }
  icd9cm_pcs_nonbillable <- data.frame(do.call("rbind", out))
  
  # # Save data as lists and share with Jack; ONLY RUN IT ONCE!----
  # # a. Chapters----
  # icd9_pcs_chapters <- split(x = as.matrix(icd9_pcs_chapters[, -1]),
  #                            f = factor(icd9_pcs_chapters$chapter,
  #                                       levels = icd9_pcs_chapters$chapter))
  # icd9_pcs_chapters <- lapply(icd9_pcs_chapters,
  #                             function(a) {
  #                               names(a) <- c("start",
  #                                             "end")
  #                               return(a)
  #                             })
  # save(icd9_pcs_chapters,
  #      file = "tmp/icd9_pcs_chapters.RData")
  # 
  # # b. Sub-Chapters----
  # icd9_pcs_sub_chapters <- split(x = as.matrix(icd9_pcs_sub_chapters[, -1]),
  #                                f = factor(icd9_pcs_sub_chapters$sub_chapter,
  #                                           levels = icd9_pcs_sub_chapters$sub_chapter))
  # icd9_pcs_sub_chapters <- lapply(icd9_pcs_sub_chapters,
  #                             function(a) {
  #                               names(a) <- c("start",
  #                                             "end")
  #                               return(a)
  #                             })
  # save(icd9_pcs_sub_chapters,
  #      file = "tmp/icd9_pcs_sub_chapters.RData")
  
  return(icd9cm_pcs_nonbillable)
}

# Get merged data for a given ICD-9 version----
icd9cm_merge_version_pcs <- function(icd9_version) {
  # Load data----
  lfile <- dir("original")
  lfile
  
  icd9cm_pcs_billable <- list()
  foreach(i = 1:length(lfile)) %do% {
    icd9cm_pcs_billable[[i]] <- read.xlsx2(file = file.path("original",
                                                           lfile[i]),
                                          sheetIndex = 1)[, c(1, 3, 2)]
    colnames(icd9cm_pcs_billable[[i]]) <- c("code",
                                            "short_desc",
                                           "long_desc")
  }
  names(icd9cm_pcs_billable) <- substr(x = lfile,
                                      start = 4,
                                      stop = 5)
  
  # # Save data as lists and share with Jack; ONLY RUN IT ONCE!----
  # head(icd9cm_pcs_billable[[1]])
  # save(icd9cm_pcs_billable,
  #      file = "tmp/icd9cm_pcs_billable.RData")
  
  icd9cm_nonbillable <- icd9cm_pcs_get_nonbillable()
  icd9cm_billable_vx <- icd9cm_pcs_billable[[which(names(icd9cm_pcs_billable) == icd9_version)]]
  
  icd9cm_billable_vx$major_code <- substr(icd9cm_billable_vx$code, 1, 2)
  icd9cm_billable_vx$major_code[substr(icd9cm_billable_vx$code, 1, 1) == "E"] <- 
    substr(icd9cm_billable_vx$code[substr(icd9cm_billable_vx$code, 1, 1) == "E"], 1, 4)
  
  icd9cm_vx <- merge(icd9cm_nonbillable,
                     icd9cm_billable_vx,
                     by = "major_code")
  
  return(icd9cm_vx)
}