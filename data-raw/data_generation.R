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


## NEW DATA SETS FROM model A parameter setting A
simu_data1_model_A<-read.csv("data-raw/simu_data1_for_jprat.csv")
simu_data1_model_A<-simu_data1_model_A[,-1]

usethis::use_data(simu_data1_model_A, overwrite = TRUE)

## study2
simu_data2_model_A<-read.csv("data-raw/simu_data2_for_jprat.csv")
simu_data2_model_A<-simu_data2_model_A[,-1]

usethis::use_data(simu_data2_model_A, overwrite = TRUE)

