require(data.table)
require(icd)

load("source/dt.sim.RData")
dt.dx <- dt.sim
dt.dx
length(unique(dt.dx$Patient_ID))
length(unique(dt.dx$Record))
# 995

map <- fread("source/icd9_codes_2018_07_21.csv",
             colClasses = c("character"))

l1 <- as.comorbidity_map(split(x = map$code,
                               f = map$sub_chapter))
l1

# idIn <- "Patient_ID"
idIn <- "Record"
icdIn <- c("DX1",
           "DX2")

dtt <- list()
for(i in 1:length(icdIn)){
  dtt[[i]] <- comorbid(x = dt.dx,
                       map = l1,
                       visit_name = idIn,
                       icd_name = icdIn[i])
}
dt.comorb <- data.table(unique(dt.dx[, 
                                     colnames(dt.dx) == idIn,
                                     with = FALSE]),
                        apply(Reduce("+",
                                     dtt),
                              MARGIN = 2,
                              function(a){
                                a > 0
                              }))
head(dt.comorb)
summary(dt.comorb)
