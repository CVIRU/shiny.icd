# |----------------------------------------------------------------------------------|
# | Project: ICD-9 Shiny App                                                         |
# | Script: Add procedure codes to the package                                       |
# | Authors: Davit Sargsyan                                                          |   
# | Created: 04/27/2018                                                              |
# | Modified: 05/24/2018, DS:  |
# |----------------------------------------------------------------------------------|
# sink("tmp/log_icd_package_procedure_icd9_v1.txt")

require(icd)
require(data.table)
require(foreach)
require(xlsx)

# Load data----
lfile <- dir("data/original")
lfile

icd9cm_sg_billable <- list()
foreach(i = 1:length(lfile)) %do% {
  icd9cm_sg_billable[[i]] <- read.xlsx2(file = file.path("data/original",
                                                        lfile[i]),
                                       sheetIndex = 1)[, 1:3]
  colnames(icd9cm_sg_billable[[i]]) <- c("code",
                                         "long_desc",
                                         "short_desc")
}
names(icd9cm_sg_billable) <- substr(x = lfile,
                                    start = 4,
                                    stop = 5)
head(icd9cm_sg_billable$`30`)

# Use Version 30 only for now (2012)----
names(icd9cm_sg_billable)
for (i in 1:length(icd9cm_sg_billable)) {
  icd9cm_sg_billable[[i]]$major_code <- substr(x = icd9cm_sg_billable[[i]]$code,
                                               start = 1,
                                               stop = 2)
}

icd9cm_sg_nonbillable <- data.frame(sub_code = unique(icd9cm_sg_hierarchy$sub_code),
                      
                      chapter = c("Procedures And Interventions , Not Elsewhere Classified",
                                  rep("Operations On The Nervous System", 5),
                                  rep("Operations On The Endocrine System", 2),
                                  rep("Operations On The Eye", 9),
                                  "Other Miscellaneous Diagnostic And Therapeutic Procedures",
                                  rep("Operations On The Ear", 3),
                                  rep("Operations On The Nose, Mouth, And Pharynx", 9),
                                  rep("Operations On The Respiratory System", 5),
                                  rep("Operations On The Cardiovascular System", 5),
                                  rep("Operations On The Hemic And Lymphatic System"),
                                  rep("Operations On The Digestive System"),
                                  rep("Operations On The Urinary System"),
                                  rep(""),
                                  rep("Operations On The Male Genital Organs"),
                                  rep("Operations On The Female Genital Organs"),
                                  rep("Obstetrical Procedures"),
                                  rep("Operations On The Musculoskeletal System"),
                                  rep("Operations On The Integumentary System"),
                                  rep("Miscellaneous Diagnostic And Therapeutic Procedures")),
                      
                      sub_chapter = c("Procedures And Interventions , Not Elsewhere Classified",
                                      
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
                                      "Other Nonoperative Procedures"))
mapping
icd9cm_sg_hierarchy <- merge(icd9cm_sg_hierarchy,
                             mapping,
                             by = "sub_code") 

# TEMPORARY: SET MAJOR = SUB_CHAPTER
icd9cm_sg_hierarchy$major <- icd9cm_sg_hierarchy$sub_chapter

# Rearrange to match diagnoses code file
colnames(icd9cm_hierarchy)
colnames(icd9cm_sg_hierarchy)
icd9cm_sg_hierarchy <- icd9cm_sg_hierarchy[, c(2, 4, 3, 1, 7, 6, 5)]
head(icd9cm_sg_hierarchy)

save(icd9cm_sg_hierarchy,
     file = "source/icd9cm_sg_hierarchy.RData")

# sessionInfo()
# sink()