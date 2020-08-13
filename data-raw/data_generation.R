## predict
data_predict<-read.csv("data-raw/data_predict.csv")
data_predict<-data_predict[,-1]

#save(data_predict, file="data/data_predict.rdata")
usethis::use_data(data_predict, overwrite = TRUE)

## cohort
data_cohort<-read.csv("data-raw/data_cohort.csv")
data_cohort<-data_cohort[,-1]

#save(data_cohort, file="data/data_cohort.rdata")
usethis::use_data(data_cohort, overwrite = TRUE)


## pharos
data_pharos<-read.csv("data-raw/data_pharos.csv")
data_pharos<-data_pharos[,-1]

#save(data_pharos, file="data/data_pharos.rdata")
usethis::use_data(data_pharos, overwrite = TRUE)


## predict
ex_data_predict<-read.csv("data-raw/real_predict_data.csv")
ex_data_predict<-ex_data_predict[,-1]

#save(data_predict, file="data/data_predict.rdata")
usethis::use_data(ex_data_predict, overwrite = TRUE)

## cohort
ex_data_cohort<-read.csv("data-raw/real_cohort_data.csv")
ex_data_cohort<-ex_data_cohort[,-1]

#save(data_cohort, file="data/data_cohort.rdata")
usethis::use_data(ex_data_cohort, overwrite = TRUE)


## pharos
ex_data_pharos<-read.csv("data-raw/real_pharos_data.csv")
ex_data_pharos<-ex_data_pharos[,-1]

#save(data_pharos, file="data/data_pharos.rdata")
usethis::use_data(ex_data_pharos, overwrite = TRUE)
