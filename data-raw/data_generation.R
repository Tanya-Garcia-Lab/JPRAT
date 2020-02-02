library(usethis)
library(JPRAT)

## predict
data_predict<-read.csv("data-raw/data_predict.csv")


save(data_predict, file="data/data_predict.rdata")
usethis::use_data(data_predict, overwrite = TRUE)

## cohort
data_cohort<-read.csv("data-raw/data_cohort.csv")


save(data_cohort, file="data/data_cohort.rdata")
usethis::use_data(data_cohort, overwrite = TRUE)


## pharos
data_pharos<-read.csv("data-raw/data_pharos.csv")


save(data_pharos, file="data/data_pharos.rdata")
usethis::use_data(data_pharos, overwrite = TRUE)
