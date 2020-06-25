filePath<-system.file("extdata","out_nrisk.dat"
                      ,package = "JPRAT")
dat_raw<-read.table(filePath, header=FALSE)
example4_ruData<-import.dat(dat_raw)

nrisk<-read.table("out_nrisk.dat")
#save(data_predict, file="data/data_predict.rdata")
usethis::use_data(nrisk, internal=TRUE, overwrite = TRUE)
