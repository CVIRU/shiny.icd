# |------------------------------------------------------------------------------|
# | Project: Simulate dataset that is structured like MIDAS                      |
# |          (BUT NO REAL PATIENTS' DATA!)                                       |
# | Script: Data simulation                                                      |     
# | Author: Davit Sargsyan                                                       | 
# | Created: 02/05/2018                                                          |
# | Modified:                                                                    |
# |------------------------------------------------------------------------------|
# Header----
# Save consol output to a log file
sink(file = "tmp/log_midas_data_simulation_v1.txt")
date()

# ICD-9 codes can be found here:
# http://www.icd9data.com/2015/Volume1/default.htm

# Load packages
require(data.table)
require(ggplot2)

# Part I: Load MIDAS15
load("E:/MIDAS/midas15_clean.RData")
midas15
cnames <- colnames(midas15)
cnames
midas15$PRIME

# Diagnoses----
udx <- lapply(midas15[, DX1:DX9],
              function(a){
                unique(a)
              })
udx <- unique(do.call("c", udx))
length(udx)
# 16,102 unique ICD-9 codes. Save the list.
save(udx,
     file = "data/unique_dx.RData")

# Part II: Simulate data----
# Skip Part I and load diagnoses----
load("data/unique_dx.RData")

# Simulate demographics----
N = 1000
dt1 <- data.table(Patient_ID = 1:N,
                  # Birthday
                  patbdte = sample(seq(from = as.Date('1920/01/01'), 
                                       to = as.Date('1970/12/31'), 
                                       by="day"), 
                                   size = N,
                                   replace = TRUE),
                  # Death day
                  NEWDTD = sample(seq(from = as.Date('1994/01/01'), 
                                      to = as.Date('2015/12/31'), 
                                      by="day"), 
                                  size = N,
                                  replace = TRUE),
                  SEX = factor(sample(x = c("M", "F"),
                                      size = N,
                                      replace = TRUE)),
                  # Primary insurance
                  PRIME = factor(sample(x = c("Medicare", 
                                              "Commercial",
                                              "Medicaid/Self-Pay/Other"),
                                        size = N,
                                        replace = TRUE,
                                        prob = c(0.5, 0.4, 0.1))))
summary(dt1)

# Sample 500 diagnosis and create a matrix of DX1-DX9
udx.keep <- sample(udx, 500)
udx.keep

# Create K records-----
K = 5000
dxx <- list()
for(i in 1:K) {
  n.dx <- sample(x = 1:9,
                 size = 1)
  dxx[[i]] <- c(sample(x = udx.keep,
                       size = n.dx,
                       replace = FALSE),
                rep(NA, 9 - n.dx)) 
}
dxx <- do.call("rbind", dxx)
colnames(dxx) <- paste("DX", 1:9, sep = "")
dxx

# Assign IDs
dt2 <- data.table(Patient_ID = sample(x = 1:N, 
                                      size = K,
                                      replace = TRUE),
                  dxx)
dt2 <- dt2[order(dt2$Patient_ID),]
dt2

# Admission dates
dt2[, ADMDAT := sample(seq(from = as.Date('1994/01/01'), 
                           to = as.Date('2015/12/31'), 
                           by = "day"), 
                       size = .N,
                       replace = FALSE),
    by = Patient_ID]
dt2 <- dt2[order(ADMDAT),]
dt2 <- dt2[order(Patient_ID),]
dt2[, N := 1:.N,
    by = Patient_ID]
dt2

# Merge demographics with diagnises----
dt3 <- merge(dt1,
             dt2,
             by = "Patient_ID")
dt3

# If the latest admissin is past the death date, reset death date to none----
dt3[, lastAdm := ADMDAT[max(N)],
    by = Patient_ID]
dt3$NEWDTD[dt3$NEWDTD <= dt3$lastAdm] <- NA
# Number of people dead----
length(unique(dt3$Patient_ID[!is.na(dt3$NEWDTD)]))

summary(dt3)

# Save the data----
dt.sim <- dt3
dt.sim[, lastAdm := NULL]
dt.sim[, N := NULL]

save(dt.sim,
     file = "tmp/dt.sim.RData")
write.csv(dt.sim,
          file = "tmp/dt.sim.csv",
          row.names = FALSE)

# Part III: processed data simulation----
# Study Objectives: to compare survival of ulcerative colitis patients 
# after a major cardiovascular event (AMI, stroke or TIA) at 30, 90, 180 days
# and 1 year after discharge

# Number of subjects
N = 1000

# Risk factors and outcomes----
# NOTE: AMI, Stroke and Tia are outcomes in UC patients;
# everything else is pre-existing condition.
# Assume no prior AMI, Stroke or TIA.
rfLegend <- data.table(abrvName = c("uc",
                                    "ami",
                                    "stroke",
                                    "tia",
                                    "chf",
                                    "hyp",
                                    "diab",
                                    "asc",
                                    "copd",
                                    "lipid",
                                    "cld",
                                    "akd",
                                    "ckd",
                                    "cmyo",
                                    "anem",
                                    "obes",
                                    "sleep",
                                    "tyr",
                                    "cancer",
                                    "pca",
                                    "cabg",
                                    "abl",
                                    "pace",
                                    "icd"),
                       fullName = c("Ulcerative Colitis",
                                    "Acute Miocardial Infarction",
                                    "Stroke",
                                    "Transient Ischemic Attack",
                                    "Congestive Heart Failure",
                                    "Hypertension",
                                    "Diabetes",
                                    "Atherosclerosis",
                                    "Chronic Obstructive Pulmonary Disease",
                                    "Disorders of Lipid Metabolism",
                                    "Chronic Liver Disease and Cirrhosis",
                                    "Acute Kidney Failure",
                                    "Chronic Kidney Disease",
                                    "Cardiomyopathy",
                                    "Anemia",
                                    "Obesity",
                                    "Sleep Apnea",
                                    "Tyroiditis",
                                    "Cancer",
                                    "Percutaneous Coronary Intervention",
                                    "Coronary Artery Bypass Grafting",
                                    "Ablation",
                                    "Pacemaker",
                                    "Implantable Cardioverter Defibrillator"))
rfLegend

# Random matrix with 20% 1s/80% 0s
dt.rf <- data.table(matrix(sample(x = 0:1, 
                                  size = N*nrow(rfLegend),
                                  replace = TRUE,
                                  prob = c(0.8, 
                                           0.2)),
                           nrow = N))
colnames(dt.rf) <- rfLegend$abrvName

# Set all patients to have UC
dt.rf$uc <- 1

# Set all patients to have one of the 3 events but make them mutually exclusive
ndx.col <- sample(x = 1:3, 
                  size = N,
                  replace = TRUE,
                  prob = c(0.6, 0.3, 0.1))

dt.rf$ami <- dt.rf$stroke <- dt.rf$tia <- 0
dt.rf$ami[ndx.col == 1] <- dt.rf$stroke[ndx.col == 2] <- dt.rf$tia[ndx.col == 3] <- 1

summary(dt.rf)

dt4 <- data.table(Patient_ID = 1:N,
                  # Birthday
                  patbdte = sample(seq(from = as.Date('1920/01/01'), 
                                       to = as.Date('1970/12/31'), 
                                       by = "day"), 
                                   size = N,
                                   replace = TRUE),
                  # Admission day (for one of the 3 major CVI events)
                  ADMDAT = sample(seq(from = as.Date('1994/01/01'), 
                                      to = as.Date('2015/12/31'), 
                                      by = "day"), 
                                  size = N,
                                  replace = TRUE),
                                  # Death day
                                  NEWDTD = sample(seq(from = as.Date('1994/01/01'), 
                                                      to = as.Date('2015/12/31'), 
                                                      by = "day"), 
                                                  size = N,
                                                  replace = TRUE),
                                  SEX = factor(sample(x = c("M", "F"),
                                                      size = N,
                                                      replace = TRUE)),
                                  # Primary insurance
                                  PRIME = factor(sample(x = c("Medicare", 
                                                              "Commercial",
                                                              "Medicaid/Self-Pay/Other"),
                                                        size = N,
                                                        replace = TRUE,
                                                        prob = c(0.5, 0.4, 0.1))),
                                  dt.rf)
# Reset death date if it's before admission date
dt4$NEWDTD[dt4$NEWDTD <= dt4$ADMDAT] <- NA
# Number of people dead----
length(unique(dt4$Patient_ID[!is.na(dt4$NEWDTD)]))
summary(dt4)

# Saved processed data
dt.proc <- dt4

save(dt.proc,
     file = "tmp/dt.proc.RData")
write.csv(dt.proc,
          file = "tmp/dt.proc.csv",
          row.names = FALSE)

sink()
beepr::beep(3)