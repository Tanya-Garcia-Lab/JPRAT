#########################################################################
## Functions that are used for each data set. Call functions in main.R ##
#########################################################################


## function to check existence of a variable
check.existence <- function(var.name){
  return(exists(as.character(substitute(var.name))))
}





#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+
## Function that lists variables used
#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+ #+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+

get.all.variable.names <- function(study){

  ## if psych.name does not exist
  if(!exists(as.character(substitute(psych.name)))){
    psych.name <- NULL
  }

  if(study=="cohort"){
    ########
    ## id ##
    ########
    id.name <- "id"

    ######################
    ## site information ##
    ######################
    site.variable <- "cno"

    ###########################
    ## birthyear information ##
    ###########################
    birthyear.variable <- "birthyr"

    ############
    ## gender ##
    ############
    gender.var <- "C_GENDER"
    female.label <- 1
    male.label <- 2

    ###############
    ## time name ##
    ###############
    time.name.order <- "EVENT_ID"
    time.name <- time.name.order
    event.name <- time.name  ## CHECK if we need to adjust time.baseline.motor=1??
    event.name.motor <- event.name


    time.baseline <- "BL"
    age.name <- NULL
    age.name.motor <-  "uhd1days"
    age.name.mci <-  "uhd2days"
    age.name.behavior <- "uhd3days"
    age.name.tfc <- "uhd6days"



    followup.motor <- "uhd1days"
    followup.mci <- "uhd2days"
    followup.behavior <- "uhd3days"
    followup.tfc <- "uhd6days"

    ##########################
    ## motor diagnosis info ##
    ##########################
    clinical.diagnosis.year <- "C_CLINDXYR"
    tms.name <- "uhdrmotor"
    dcl.name <- "C_DIAGCONL"

    ###################
    ## Baseline info ##
    ###################
    base.var <-  c("birthyr","age")
    base.label <- c("birthyr","base_age")

    #########################
    ## cognitive variables ##
    #########################
    sdmt.name <- "C_CASYDIG"
    stroop_color.name <- "C_CASTRPCN"
    stroop_word.name <- "C_CASTRPWR"
    stroop_interference.name <- "C_CASTRPIN"

    ########################################
    ## Total functional capacity variable ##
    ########################################
    tfc.name <- "tfc"

    ########################
    ## behavior variables ##
    ########################
    cutoff.apathy <- 2
    cutoff.irb <- 2
    cutoff.dep <- 2

    apathy.sev.name <- "C_APATHSEV"
    apathy.frq.name <- "C_APATHFRQ"

    irb.sev.name <- "C_IRBEHSEV"
    irb.frq.name <- "C_IRBEHFRQ"

    dep.sev.name <- "C_DEPMDSEV"
    dep.frq.name <- "C_DEPMDFRQ"

    ###################
    ## cag variables ##
    ###################
    cag.var <- c("Blood_ALLELE_1","Blood_ALLELE_2")

    ###################
    ## race variable ##
    ###################
    race.variable <- "C_RACEGRP"

    #################################
    ## who knows genetic knowledge ##
    #################################
    gene.know <- NULL

    ########################
    ## education variable ##
    ########################
    education.var <-  c("C_EDUCYRS","C_EDUC")
    education.label <- c("educyrs","educ")

  } else if(study=="track"){
    ########
    ## id ##
    ########
    id.name <- "subject"

    ###################
    ## site variable ##
    ###################
    site.variable <- NULL  ## 5/11/2018: Need to fill in for future use


    ###########################
    ## birthyear information ##
    ###########################
    birthyear.variable <- NULL ## 5/11/2018: Need to fill in for future use

    ############
    ## gender ##
    ############
    gender.var <- "gender"
    female.label <- "f"
    male.label <- "m"

    ###################
    ## time variable ##
    ###################
    time.name.order <- "visit"
    time.name <- time.name.order
    event.name <- time.name
    event.name.motor <- event.name

    time.baseline <- 1
    age.name <- "age"
    age.name.motor <-  age.name
    age.name.mci <-  age.name
    age.name.behavior <- age.name
    age.name.tfc <- age.name

    followup.motor <- "fu"
    followup.mci <- "fu"
    followup.behavior <- "fu"
    followup.tfc <- "fu"

    ##########################
    ## motor diagnosis info ##
    ##########################
    clinical.diagnosis.year <- NULL
    tms.name <- "motorscore"
    dcl.name <- "diagconf"


    ###################
    ## Baseline info ##
    ###################
    base.var <-  "age_b"
    base.label <- "base_age"

    #########################
    ## cognitive variables ##
    #########################
    sdmt.name <- "sdmt"
    stroop_color.name <- NULL
    stroop_word.name <- "stroop"
    stroop_interference.name <- NULL

    ########################################
    ## Total functional capacity variable ##
    ########################################
    tfc.name <- NULL  		## NEED TO FILL IN IF NEEDED IN FUTURE

    ########################
    ## behavior variables ##
    ########################
    cutoff.apathy <- 2
    cutoff.irb <- 2
    cutoff.dep <- 11  ## CHECK WITH KAREN!!!!!!!!!!!!!

    psych.name <- c("anxscore",  ## HAD-SIS anxiety subscore
                    "depscore",	  ## HAD-SIS depression subscore
                    "irrscore",	  ## HAD-SIS irritability subscore
                    "outwardirrscore",  ## HAD-SIS outward irritability subscore
                    "inwardirrscore", 	## HAD-SIS inward irritability subscore
                    "bditotal",			## Beck Depression Score
                    "apathysscore",		## Baltimore apathy score
                    "apathycscore", 	## Baltimore Apathy score
                    "irtysscore",		## Baltimore irritability score
                    "irtycscore",		## Baltimore irritability score
                    "think",			## PBA preseverative thinking
                    "beh",				## PBA aggressive behavior
                    "behave_all",		## PBA total behavior score
                    "affect",			## PBA affect factor
                    "irritability",		## PBA irritability factor
                    "irr_der_2",		## PBA irritability factor without perseveration
                    "apathy",			## PBA apathy
                    "disor",			## PBA disorientation
                    "psych"			## PBA psychosis score
    )

    apathy.sev.name <- c("apathysscore", ## Baltimore apathy score, self-rating
                         "apathy")    ## and PBA apathy score
    apathy.frq.name <- NULL

    irb.sev.name <- c("irtysscore",  ## Baltimore Irritability score, self-rating
                      "irrscore",    ## HAD-SIS irritability score
                      "irritability")  ## PBA irritability factor

    irb.frq.name <- NULL

    dep.sev.name <- c("bditotal",	## Beck depression score
                      "depscore")  	## HAD-SIS depression subscore
    dep.frq.name <- NULL

    ###################
    ## cag variables ##
    ###################
    cag.var <- "cag"

    ## who knows genetic knowledge
    gene.know <- NULL

    ###################
    ## race variable ##
    ###################
    race.variable <- NULL  ## CHECK what it is

    ########################
    ## education variable ##
    ########################
    education.var <-  "education"
    education.label <- "educ"

  } else if(study=="pharos"){
    ########
    ## id ##
    ########
    id.name <- "id"

    ###################
    ## site variable ##
    ###################
    site.variable <- "uhdrsite"

    ###########################
    ## birthyear information ##
    ###########################
    birthyear.variable <- "dob"

    ############
    ## gender ##
    ############
    gender.var <- "mhxsex"
    female.label <- 1
    male.label <- 2

    ###############
    ## time name ##
    ###############
    time.name.order <- "visitdt_age"
    time.name <- time.name.order
    time.baseline <- c(-1,0,1)
    age.name <- "visitdt_age"
    age.name.motor <- "irmadate_age"  ##  use independent rater info
    age.name.behavior <- "visitdt_age"
    age.name.mci <- "visitdt_age"
    age.name.tfc <- "visitdt_age"

    event.name <- "uhdrvisit"
    event.name.motor <- "irmavisit"



    followup.motor <- NULL
    followup.mci <- NULL
    followup.behavior <- NULL
    followup.tfc <- NULL

    ##########################
    ## motor diagnosis info ##
    ##########################
    clinical.diagnosis.year <- NULL
    tms.name <- "irmamotor"
    dcl.name <- "irmadiagnosis"

    ###################
    ## Baseline info ##
    ###################
    base.var <-  "visitdt_age"
    base.label <- c("base_age","birthyr")

    #########################
    ## cognitive variables ##
    #########################
    sdmt.name <- "uhdrsymbdigit"
    stroop_color.name <- "uhdrstropcolor"
    stroop_word.name <- "uhdrstropread"
    stroop_interference.name <- "uhdrstropinter"

    ########################################
    ## Total functional capacity variable ##
    ########################################
    tfc.name <- "tfc"

    ########################
    ## behavior variables ##
    ########################
    cutoff.apathy <- 2
    cutoff.irb <- 2
    cutoff.dep <- 2

    apathy.sev.name <- "uhdrapathysev"
    apathy.frq.name <- "uhdrapathyfrq"

    irb.sev.name <- "uhdrirritfsev"
    irb.frq.name <- "uhdrirritfrq"

    dep.sev.name <- "uhdrdeprsev"
    dep.frq.name <- "uhdrdeprfrq"

    ###################
    ## cag variables ##
    ###################
    cag.var <- c("caga1","caga2")  ## CAG repeats on each allele

    ###################
    ## race variable ##
    ###################
    race.variable <- "mhxrace"

    ## who knows genetic knowledge
    gene.know <- c("gene.site.investigator",
                   "gene.indep.rater","gene.study.coord",
                   "gene.study.subj")

    ########################
    ## education variable ##
    ########################
    education.var <- "mhxeduc" ## education in years
    education.label <- "educ"
  } else if(study=="predict"){
    ########
    ## id ##
    ########
    id.name <- "SUBJID"


    ###################
    ## site variable ##
    ###################
    site.variable <- "siteid"


    ###########################
    ## birthyear information ##
    ###########################
    birthyear.variable <- NULL

    ############
    ## gender ##
    ############
    gender.var <- "GENDER"
    female.label <- "f"
    male.label <- "m"

    ###################
    ## time variable ##
    ###################
    time.name.order <- "EVENT"
    time.name <- "YEAR"
    event.name <- "EVENT"
    event.name.motor <- event.name

    time.baseline <- as.numeric(paste(seq(1,10),"01",sep=""))
    age.name <- "AGE"
    age.name.motor <- "YEAR"
    age.name.mci <-  "YEAR"
    age.name.behavior <- "YEAR"
    age.name.tfc <- "YEAR"


    followup.motor <- NULL
    followup.mci <- NULL
    followup.behavior <- NULL
    followup.tfc <- NULL

    ##########################
    ## motor diagnosis info ##
    ##########################
    clinical.diagnosis.year <- NULL
    tms.name <- "TOTAL_MOTOR_SCORE"
    dcl.name <- "DCL"

    ###################
    ## Baseline info ##
    ###################
    base.var <-  c("EVENT","YEAR","AGE")
    base.label <- c("base_age","base_year","birthyr")

    #########################
    ## cognitive variables ##
    #########################
    sdmt.name <- "SDMT"
    stroop_color.name <- "STROOP_COLOR"
    stroop_word.name <- "STROOP_WORD"
    stroop_interference.name <- "STROOP_INTERFERENCE"

    ########################################
    ## Total functional capacity variable ##
    ########################################
    tfc.name <- "TFC"

    ########################
    ## behavior variables ##
    ########################
    cutoff.apathy <- 2
    cutoff.irb <- 2
    cutoff.dep <- 2

    dep.frq.name <- "q25a" #Frequency: Within the past month, how often have you been feeling sad, down, or blue?
    dep.sev.name <- "q25b" #Severity: How has your mood affected your daily activities? [Evidence of sad mood from behavioral observation includes sad voice or expression, tearfulness.]

    apathy.frq.name <- "q26a" 		 #Frequency: Within the past month, how often have you found that you have lost interest in things that used to be important to you? For example, are you just as interested as always in trying new things, starting
    apathy.sev.name <- "q26b" #Severity: How has apathy impacted your ability to carry out daily activities?

    irb.frq.name <- "q30a" #Frequency: In the past month, how often have you felt impatient, irritable, or cranky?
    irb.sev.name <- "q30b" #Severity: How has irritability impacted your ability to get along with others?




    ###################
    ## cag variables ##
    ###################
    cag.var <- c("CAG")

    ###################
    ## race variable ##
    ###################
    race.variable <- "RACE"

    #################################
    ## who knows genetic knowledge ##
    #################################
    gene.know <- NULL

    ########################
    ## education variable ##
    ########################
    education.var <- "EDUC_YRS" ## education in years
    education.label <- "educ"
  } else if(study=="enroll"){
    ########
    ## id ##
    ########
    id.name <- "subjid"


    ###################
    ## site variable ##
    ###################
    site.variable <- NULL  ## 5/11/2018: Need to fill in if we want to use this variable



    ###########################
    ## birthyear information ##
    ###########################
    birthyear.variable <- NULL ## 5/11/2018: Need to fill in if we want to use this

    ############
    ## gender ##
    ############
    gender.var <- "sex"
    female.label <- "f"
    male.label <- "m"

    ####################
    ## time variables ##
    ####################
    time.name.order <- "seq"
    time.name <- time.name.order
    event.name <- time.name
    event.name.motor <- event.name
    time.baseline <- 1

    age.name <- "age"
    age.name.motor <- age.name
    age.name.mci <-  age.name
    age.name.behavior <- age.name
    age.name.tfc <- age.name

    followup.motor <- "visdy"
    followup.mci <- "visdy"
    followup.behavior <- "visdy"
    followup.tfc <- "visdy"

    ##########################
    ## motor diagnosis info ##
    ##########################
    clinical.diagnosis.year <- NULL
    tms.name <- "motscore"  ## maybe also include "miscore"???????????????
    dcl.name <- "diagconf"

    ###################
    ## Baseline info ##
    ###################
    base.var <-   "age_0"
    base.label <- "base_age"


    #########################
    ## cognitive variables ##
    #########################
    sdmt.name <- "sdmt1"
    stroop_color.name <- "scnt1"
    stroop_word.name <- "swrt1"
    stroop_interference.name <- "sit1"

    ########################################
    ## Total functional capacity variable ##
    ########################################
    tfc.name <- NULL	## NEED TO FILL IN IF NEEDED IN FUTURE


    ########################
    ## behavior variables ##
    ########################
    cutoff.apathy <- 2
    cutoff.irb <- 2
    cutoff.dep <- 2

    apathy.sev.name <-  "pbas6sv"
    apathy.frq.name <- NULL

    irb.sev.name <- "pbas4sv"
    irb.frq.name <- NULL

    dep.sev.name <- "pbas1sv"
    dep.frq.name <- NULL

    ###################
    ## cag variables ##
    ###################
    cag.var <- c("caghigh","caglow")

    ## race variable
    race.variable <- "race"

    ## who knows genetic knowledge
    gene.know <- NULL

    ########################
    ## education variable ##
    ########################
    education.var <- "isced"
    education.label <- "educ"
  }

  ## cag limits
  cag.min <- 36
  cag.max <- 50


  ## TFC cutoffs
  cutoff.tfc.stageone <- c(11,13)  ## stage 1: TFC is 11-13
  cutoff.tfc.stagetwo <- c(7,10)   ## stage 2: TFC is 7-10
  cutoff.tfc.stagethree <- c(3,6)   ## stage 2: TFC is 7-10


  out <- list(


    ## id name
    id.name=id.name,

    ## site variable name
    site.variable=site.variable,

    ## birthyear information
    birthyear.variable=birthyear.variable,

    ## gender
    gender.var=gender.var,
    female.label=female.label,
    male.label=male.label,

    ## time name
    time.name.order=time.name.order,
    time.name=time.name,
    event.name=event.name,
    event.name.motor=event.name.motor,

    time.baseline=time.baseline,
    age.name=age.name,
    age.name.motor=age.name.motor,
    age.name.behavior=age.name.behavior,
    age.name.mci=age.name.mci,
    age.name.tfc=age.name.tfc,

    followup.motor=followup.motor,
    followup.mci=followup.mci,
    followup.behavior=followup.behavior,
    followup.tfc=followup.tfc,

    ## motor diagnosis info
    clinical.diagnosis.year=clinical.diagnosis.year,
    tms.name=tms.name,
    dcl.name=dcl.name,

    ## baseline info
    base.var=base.var,
    base.label=base.label,

    ## cognitive variables
    sdmt.name=sdmt.name,
    stroop_color.name=stroop_color.name,
    stroop_word.name=stroop_word.name,
    stroop_interference.name=stroop_interference.name,

    ## Total Functional Capacity (TFC) variables
    tfc.name=tfc.name,
    cutoff.tfc.stageone=cutoff.tfc.stageone,
    cutoff.tfc.stagetwo=cutoff.tfc.stagetwo,
    cutoff.tfc.stagethree=cutoff.tfc.stagethree,

    ## behavior variables
    psych.name = psych.name, ## for TRACK data

    cutoff.apathy=cutoff.apathy,
    cutoff.irb=cutoff.irb,
    cutoff.dep=cutoff.dep,

    apathy.sev.name=apathy.sev.name,
    apathy.frq.name=apathy.frq.name,

    irb.sev.name=irb.sev.name,
    irb.frq.name=irb.frq.name,

    dep.sev.name=dep.sev.name,
    dep.frq.name=dep.frq.name,

    ## cag
    cag.var=cag.var,
    cag.min=cag.min,
    cag.max=cag.max,

    ##race
    race.variable=race.variable,

    ## gene.know
    gene.know=gene.know,

    ## education
    education.var=education.var,
    education.label=education.label
  )

  return(out)
}

####################################
## Functions used to get MCI data ##
####################################

organize.mci.behavior.data <- function(location,study,type=c("cognitive","behavior")[1]){
  #####################################
  ## Get data sets used for analysis ##
  #####################################
  all.study.data <- get.study.data(study)
  cag.data <- all.study.data$cag.data
  cognitive.data <- all.study.data$cognitive.data
  behavior.data <- all.study.data$behavior.data
  education.data <- all.study.data$education.data
  demographic.data <- all.study.data$demographic.data
  birth.data <- all.study.data$birth.data
  var.names <- all.study.data$var.names

  ############################
  ## Extract variable names ##
  ############################

  time.name.order <- var.names$time.name.order
  time.name <- var.names$time.name
  time.baseline <- var.names$time.baseline
  cognitive.variable.name <- c(var.names$sdmt.name,
                               var.names$stroop_color.name,
                               var.names$stroop_word.name,
                               var.names$stroop_interference.name)

  behavior.variable.name <- c(var.names$apathy.sev.name,
                              var.names$apathy.frq.name,
                              var.names$irb.sev.name,
                              var.names$irb.frq.name,
                              var.names$dep.sev.name,
                              var.names$dep.frq.name)
  cag.name <- "CAG"

  #################
  ## get id name ##
  #################
  id.name <- var.names$id.name


  ###################
  ## get education ##
  ###################
  education <- extract.education(id.name,education.data,
                                 var.name=var.names$education.var,
                                 var.label=var.names$education.label)
  education2 <- binary.education(id.name,education,study)



  ################
  ## get gender ##
  ################
  gender <- compute.gender(id.name,
                           var.name=var.names$gender.var,
                           female.label=var.names$female.label,
                           male.label=var.names$male.label,
                           demographic.data)

  ####################
  ## get CAG repeat ##
  ####################
  cag <- compute.cag.repeats(id.name,cag.data,study,var.name=var.names$cag.var)
  cag.unif <- reverse.cag(cag$CAG,xmin=var.names$cag.min,xmax=var.names$cag.max)
  cag <- data.frame(cag=cag,cag.unif=cag.unif)
  colnames(cag) <- c(id.name,"CAG","CAG-unif")


  #########################################
  ## get birth date, and age at baseline ##
  #########################################
  birth.base.age <- compute.birth.base.age(id.name,
                                           birth.data,study,
                                           var.name=var.names$base.var,
                                           var.label=var.names$base.label,
                                           time.name=var.names$time.name,
                                           time.name.order=var.names$time.name.order,
                                           time.baseline=var.names$time.baseline,
                                           event.name=var.names$event.name.motor,
                                           age.name=var.names$age.name,
                                           birthyear.variable=var.names$birthyear.variable)


  #################################################
  ## Set up for cognitive or behavior extraction ##
  #################################################
  if(type=="cognitive"){
    data.use <- cognitive.data
    variable.name.use <- cognitive.variable.name
    file.name <- "_cog_data.csv"
    event.name.use <- var.names$event.name
  } else if(type=="behavior"){
    data.use <- behavior.data
    variable.name.use <- behavior.variable.name
    file.name <- "_beh_data.csv"
    event.name.use <- var.names$event.name
  }

  ########################
  ## get data ##
  ########################
  if(!is.null(data.use)){
    status.demog <- merge.all(list(demographic=cag[,c(id.name,cag.name)],

                                   mydata=data.use[,c(id.name,unique(c(time.name,time.name.order,event.name.use)),variable.name.use)]),
                              by.name=id.name)
    data.out <- extract.baseline.cognitive.behavior.scores(status.demog,
                                                           study,type,id.name,cag.name,var.names,event.name.use)


    ## add education, gender, and baseline age
    data.out <- merge.all(list(orig=data.out,
                               education2=education2,
                               gender=gender,
                               birth.base.age=birth.base.age[,c(id.name,"base_age")]),
                          by.name=id.name)


    #######################
    ## write data output ##
    #######################
    write.csv(data.out,paste(location,study,file.name,sep=""),row.names=FALSE)
  } else {
    cat("NO DATA AVAILABLE")
  }
}


################################################################
## Function for Psychiatric measures for paper with Hiral Shah##
################################################################

histogram.psych.data <- function(output,study,type,cag.cutoff=36){
  ####################
  ## variable names ##
  ####################
  var.names <- get.all.variable.names(study)
  time.name.order <- var.names$time.name.order
  time.name <- var.names$time.name
  time.baseline <- var.names$time.baseline

  behavior.variable.name <- c(var.names$apathy.sev.name,
                              var.names$irb.sev.name,
                              var.names$dep.sev.name)

  cag.name <- "CAG"

  #######################################
  ## focus on people with CAG >=cutoff ##
  #######################################
  index.cag <- which(output[,cag.name]>=cag.cutoff)
  data.use <- output[index.cag,]

  ####################
  ## make histogram ##
  ####################
  for(ee in 1:length(behavior.variable.name)){
    postscript(paste(study,"_",behavior.variable.name[ee],".eps",sep=""))

    ## make table of score >=x
    sample.size <- sum(!is.na(data.use[,behavior.variable.name[ee]]))
    cutoffs.table <- table(data.use[,behavior.variable.name[ee]])
    cutoffs.sum <- rev(cumsum(rev(cutoffs.table)))

    par(mar=c(5.1,5.1,4.1,2.1))
    barplot(cutoffs.sum,xlab="Cut-off",ylab="Number whose score exceeds cutoff",main="",
            cex.lab=2.5,cex.axis=2.5,cex.lab=2.5)

    ##  par( "usr" ) returns a vector containing xleft, xright, ybottom, ytop.
    usr <- par( "usr" )
    text( usr[ 2], usr[ 4 ], paste("Sample size:",sample.size,sep=" "),
          adj = c( 1, 1 ),
          col = "blue",cex=2.5)
    dev.off()
  }

}

organize.psych.histogram.data <- function(location,study,type=c("cognitive","behavior")[1]){
  #####################################
  ## Get data sets used for analysis ##
  #####################################
  all.study.data <- get.study.data(study)
  cag.data <- all.study.data$cag.data
  behavior.data <- all.study.data$behavior.data
  var.names <- all.study.data$var.names

  ####################
  ## variable names ##
  ####################
  time.name.order <- var.names$time.name.order
  time.name <- var.names$time.name
  time.baseline <- var.names$time.baseline

  behavior.variable.name <- c(var.names$apathy.sev.name,
                              var.names$irb.sev.name,
                              var.names$dep.sev.name)

  cag.name <- "CAG"

  #################
  ## get id name ##
  #################
  id.name <- var.names$id.name

  ####################
  ## get CAG repeat ##
  ####################
  cag <- compute.cag.repeats(id.name,cag.data,study,var.name=var.names$cag.var)


  ##########################################
  ## Maximum of scores measured over time ##
  ##########################################
  time.name <- var.names$time.name
  time.name.order <- var.names$time.name.order
  age.name <- var.names$age.name
  event.name <- var.names$event.name

  for(ee in 1:length(behavior.variable.name)){
    variable.name <- behavior.variable.name[ee]
    out.scores.tmp <- get.max.event.score(id.name=id.name,
                                          data=behavior.data,
                                          variable.name,
                                          time.name,
                                          time.name.order,
                                          age.name,
                                          event.name)
    if(ee==1){
      ## store ID and maximum score
      out.scores <- out.scores.tmp
    } else{
      ## store maximum score only
      out.scores <- cbind(out.scores,out.scores.tmp[,variable.name])
    }
  }

  ## out.scores names
  colnames(out.scores) <- c(id.name,behavior.variable.name)

  ## combine CAG and variable scores
  data.to.merge <- list(
    cag=cag,
    out.scores=out.scores)

  output <- merge.all(data.to.merge,by.name=id.name)
  return(output)
}


organize.psych.data <- function(location,study,type=c("cognitive","behavior")[1]){
  ##########################
  ## Only built for TRACK ##
  ##########################

  #####################################
  ## Get data sets used for analysis ##
  #####################################
  all.study.data <- get.study.data(study)
  demographic.data <- all.study.data$demographic.data
  behavior.data <- all.study.data$behavior.data
  imaging.data <- all.study.data$imaging.data

  var.names <- all.study.data$var.names

  ####################
  ## variable names ##
  ####################
  time.name.order <- var.names$time.name.order
  time.name <- var.names$time.name
  time.baseline <- var.names$time.baseline

  behavior.variable.name <- var.names$psych.name

  cag.name <- "CAG"

  #################
  ## get id name ##
  #################
  id.name <- var.names$id.name

  ######################
  ## date information	##
  ######################
  dates.data <- demographic.data

  ####################
  ## get CAG repeat ##
  ####################
  cag <- compute.cag.repeats(id.name,data=demographic.data,
                             study,var.name=var.names$cag.var)



  ################
  ## get gender ##
  ################
  gender <- compute.gender(id.name,
                           var.name=var.names$gender.var,
                           female.label=var.names$female.label,
                           male.label=var.names$male.label,
                           demographic.data)



  #########################################
  ## get birth date, and age at baseline ##
  #########################################
  birth.base.age <- compute.birth.base.age(id.name,
                                           demographic.data,study,
                                           var.name=var.names$base.var,
                                           var.label=var.names$base.label,
                                           time.name=var.names$time.name,
                                           time.name.order=var.names$time.name.order,
                                           time.baseline=var.names$time.baseline,
                                           event.name=var.names$event.name.motor,
                                           age.name=var.names$age.name,
                                           birthyear.variable=var.names$birthyear.variable)



  ###################
  ## get censorage ##
  ###################
  ## COHORT: don't use hdstatfu days: not as good as DCL
  censorage <- compute.censoring.age(id.name,
                                     data=demographic.data,
                                     dates=birth.base.age,
                                     baseline.data=demographic.data,
                                     study,
                                     time.name=var.names$time.name,
                                     time.name.order=var.names$time.name.order,
                                     age.name=var.names$age.name.motor,
                                     followup.motor=var.names$followup.motor)

  ## data to merge
  data.to.merge.tmp <- list(cag=cag,
                            gender=gender,
                            birth.base.age=birth.base.age)

  data.to.merge <- merge.all(data.to.merge.tmp,by.name=id.name)



  #+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+
  ## Time-to-event ages 					##
  #+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+

  mci.test.name <- NULL
  control.data <- NULL
  data <- behavior.data
  age.name <- var.names$age.name
  followup.name <- var.names$followup.behavior
  event.info <- "behavior"
  event.name <- var.names$event.name
  cutoff.use <- unlist(psych.cutoffs(study)[behavior.variable.name])

  #####################################
  ## behavior time-to-event outcomes ##
  #####################################
  for(bb in 1:length(behavior.variable.name)){

    out <- get.event.data(id.name,
                          mci.test.name,
                          control.data,
                          data,
                          dates.data=dates.data,
                          censorage,
                          variable.name=behavior.variable.name[bb],
                          new.variable.name=behavior.variable.name[bb],
                          age.name=age.name,
                          time.name=var.names$time.name,
                          time.name.order=var.names$time.name.order,
                          time.baseline=var.names$time.baseline,
                          event.name=event.name,
                          followup.name=followup.name,
                          tms.name=var.names$tms.name,
                          study,
                          event.info=event.info,
                          cutoff=cutoff.use[bb],
                          gene.know=var.names$gene.know)

    age.event <- out$data.out

    ## combine baseline variables and variable scores
    data.to.merge.tmp <- list(
      old=data.to.merge,
      new=age.event)


    data.to.merge <- merge.all(data.to.merge.tmp,by.name=id.name)

  }

  ##  add in volumetric imaging data at baseline
  data.to.merge.tmp <- list(
    old=data.to.merge,
    new=imaging.data)

  data.to.merge <- merge.all(data.to.merge.tmp,by.name=id.name)


  return(data.to.merge)
}


#####################################
## Functions used to get data sets ##
#####################################
organize.data.sets <- function(location,study,use.normative.data){
  ## MCI data
  if(use.normative.data==TRUE){
    mci.data <- read.csv("data/mci_data_normative.csv",header=TRUE,row.names=1)
  } else {
    mci.data <- read.csv("data/mci_data.csv",header=TRUE,row.names=1)
  }

  ## BEH data
  beh.data <- read.csv("data/beh_data.csv",header=TRUE,row.names=1)

  #####################################
  ## Get data sets used for analysis ##
  #####################################
  all.study.data <- get.study.data(study)
  baseline.data <- all.study.data$baseline.data
  behavior.data <- all.study.data$behavior.data
  cag.data <- all.study.data$cag.data
  cognitive.data<- all.study.data$cognitive.data
  demographic.data<- all.study.data$demographic.data
  education.data<- all.study.data$education.data
  impairment.data<- all.study.data$impairment.data
  motor.data<- all.study.data$motor.data
  race.data<- all.study.data$race.data
  symptom.data<- all.study.data$symptom.data
  tfc.data<- all.study.data$tfc.data

  birth.data<- all.study.data$birth.data
  censor.data<- all.study.data$censor.data
  site.data <- all.study.data$site.data

  var.names <- all.study.data$var.names

  #################
  ## get id name ##
  #################
  id.name <- var.names$id.name

  #+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+
  ## Baseline information 				##
  #+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+

  ##############
  ## get race ##
  ##############
  if(study=="cohort" | study=="pharos" | study=="predict" | study=="enroll"){
    race <- extract.race(id.name,race.data,var.names$race.variable,study)
  } else {
    race <- NULL
  }

  ###################
  ## get education ##
  ###################
  education <- extract.education(id.name,education.data,
                                 var.name=var.names$education.var,
                                 var.label=var.names$education.label)
  education2 <- binary.education(id.name,education,study)

  ################
  ## get gender ##
  ################
  gender <- compute.gender(id.name,var.name=var.names$gender.var,
                           female.label=var.names$female.label,
                           male.label=var.names$male.label,
                           demographic.data)

  ##########################
  ## get site information ##
  ##########################

  site.information <- extract.variable(site.data,
                                       var.name=var.names$site.variable,
                                       var.label="site",
                                       id.name)


  ####################
  ## get CAG repeat ##
  ####################
  cag <- compute.cag.repeats(id.name,cag.data,study,var.name=var.names$cag.var)
  cag.unif <- reverse.cag(cag$CAG,xmin=var.names$cag.min,xmax=var.names$cag.max)
  cag <- data.frame(cag=cag,cag.unif=cag.unif)
  colnames(cag) <- c(id.name,"CAG","CAG-unif")

  ###############################
  ## genetic testing knowledge ##
  ###############################
  #	if(study=="cohort" | study=="predict"){
  #		if(study=="cohort"){
  #			##########################################################
  #			## response to "Has subject had genetic testing for HD" ##
  #			##########################################################
  #			gene.know.base <- cohort.compute.gentest(baseline.data,var.name="hdage_base")
  #			gene.know.nobase <- cohort.compute.gentest(baseline.data,var.name="hdage_nobase")
  #			gene.know.new.base <- cohort.compute.gentest(baseline.data,var.name="hdage_new_base")
  #			gene.know.new.nobase <- cohort.compute.gentest(baseline.data,var.name="hdage_new_nobase")
  #		}
  #
  #		if(study=="predict"){
  #			gene.know.base <- predict.compute.gentest(baseline.data,var.name="hdage_base")
  #			gene.know.nobase <- predict.compute.gentest(baseline.data,var.name="hdage_nobase")
  #			gene.know.new.base <- predict.compute.gentest(baseline.data,var.name="hdage_new_base")
  #			gene.know.new.nobase <- predict.compute.gentest(baseline.data,var.name="hdage_new_nobase")
  #		}
  #	} else {
  #		gene.know.base <- NULL
  #		gene.know.nobase <- NULL
  #		gene.know.new.base <- NULL
  #		gene.know.new.nobase <- NULL
  #	}

  #########################################
  ## get birth date, and age at baseline ##
  #########################################
  birth.base.age <- compute.birth.base.age(id.name,
                                           birth.data,study,
                                           var.name=var.names$base.var,
                                           var.label=var.names$base.label,
                                           time.name=var.names$time.name,
                                           time.name.order=var.names$time.name.order,
                                           time.baseline=var.names$time.baseline,
                                           event.name=var.names$event.name.motor,
                                           age.name=var.names$age.name,
                                           birthyear.variable=var.names$birthyear.variable)

  ###################
  ## get censorage ##
  ###################
  ## COHORT: don't use hdstatfu days: not as good as DCL
  censorage <- compute.censoring.age(id.name,
                                     data=censor.data,
                                     dates=birth.base.age,
                                     baseline.data=baseline.data,
                                     study,
                                     time.name=var.names$time.name,
                                     time.name.order=var.names$time.name.order,
                                     age.name=var.names$age.name.motor,
                                     followup.motor=var.names$followup.motor)


  #+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+
  ## Time-to-event ages 					##
  #+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+

  ######################
  ## date information	##
  ######################
  if(study=="cohort" | study=="predict"){
    dates.data <- birth.base.age
  } else if(study=="pharos" | study=="enroll"){
    dates.data <- NULL
  } else if(study=="track"){
    dates.data <- demographic.data
  }

  if(!is.null(impairment.data)){  ## only for COHORT
    ###################################
    ## get first ages of impairment  ##
    ###################################
    cog.age <- cohort.first.cchdage(impairment.data,id.name,interest.var="C_CCCOGAGE",interest.name="cog",censorage)
    apathy.age <- cohort.first.cchdage(impairment.data,id.name,interest.var="C_CCAPTAGE",interest.name="apt",censorage)
    irritate.age <- cohort.first.cchdage(impairment.data,id.name,interest.var="C_CCIRBAGE",interest.name="irb",censorage)
    depression.age <- cohort.first.cchdage(impairment.data,id.name,interest.var="C_CCDEPAGE",interest.name="dep",censorage)

  } else {
    cog.age <- NULL
    apathy.age <- NULL
    irritate.age <- NULL
    depression.age <- NULL
  }

  if(!is.null(symptom.data)){
    if(study=="cohort"){
      #############################
      ## get first motor symptom ##
      #############################
      motor.symptom <- compute.motor.symptom(data=symptom.data,dates.data=birth.base.age,id.name,censorage)
    }

    if(study=="pharos"){
      ################################
      ## age of first motor symptom ##
      ################################
      motor.symptom <- pharos.compute.motor.symptom(data=symptom.data,variable.id=id.name,censorage=censorage)
    }
  } else {
    motor.symptom <- NULL
  }


  #######################################################################################
  ## Total functional capacity (TFC) stage 1 (score: 11-13), and stage 2 (score: 7-10) ##
  #######################################################################################

  mci.test.name <- NULL
  control.data <- NULL
  data <- tfc.data
  variable.name <- var.names$tfc.name
  age.name <- var.names$age.name.tfc
  event.name <- var.names$event.name
  followup.name <- var.names$followup.tfc
  event.info <- "tfc"


  #######################################
  ## TFC stage one criteria: TFC >= 11 ##
  #######################################
  new.variable.name <- "tfcone"
  cutoff <- var.names$cutoff.tfc.stageone

  tfc1.out <- get.event.data(id.name,
                             mci.test.name,
                             control.data,
                             data,
                             dates.data=dates.data,
                             censorage,
                             variable.name,
                             new.variable.name,
                             age.name=age.name,
                             time.name=var.names$time.name,
                             time.name.order=var.names$time.name.order,
                             time.baseline=var.names$time.baseline,
                             event.name=event.name,
                             followup.name=followup.name,
                             tms.name=var.names$tms.name,
                             study,
                             event.info=event.info,
                             cutoff=cutoff,
                             gene.know=var.names$gene.know)

  tfc.stage1.age <- tfc1.out$data.out
  tfc.stage1.age.nobase <- tfc1.out$data.out.nobase


  #######################################
  ## TFC stage two criteria: 7 <= TFC <=10 ##
  #######################################
  new.variable.name <- "tfctwo"
  cutoff <- var.names$cutoff.tfc.stagetwo

  tfc2.out <- get.event.data(id.name,
                             mci.test.name,
                             control.data,
                             data,
                             dates.data=dates.data,
                             censorage,
                             variable.name,
                             new.variable.name,
                             age.name=age.name,
                             time.name=var.names$time.name,
                             time.name.order=var.names$time.name.order,
                             time.baseline=var.names$time.baseline,
                             event.name=event.name,
                             followup.name=followup.name,
                             tms.name=var.names$tms.name,
                             study,
                             event.info=event.info,
                             cutoff=cutoff,
                             gene.know=var.names$gene.know)

  tfc.stage2.age <- tfc2.out$data.out
  tfc.stage2.age.nobase <- tfc2.out$data.out.nobase

  #######################################
  ## TFC stage 3 criteria: 3 <= TFC <=6 ##
  #######################################
  new.variable.name <- "tfcthree"
  cutoff <- var.names$cutoff.tfc.stagethree

  tfc3.out <- get.event.data(id.name,
                             mci.test.name,
                             control.data,
                             data,
                             dates.data=dates.data,
                             censorage,
                             variable.name,
                             new.variable.name,
                             age.name=age.name,
                             time.name=var.names$time.name,
                             time.name.order=var.names$time.name.order,
                             time.baseline=var.names$time.baseline,
                             event.name=event.name,
                             followup.name=followup.name,
                             tms.name=var.names$tms.name,
                             study,
                             event.info=event.info,
                             cutoff=cutoff,
                             gene.know=var.names$gene.know)

  tfc.stage3.age <- tfc3.out$data.out
  tfc.stage3.age.nobase <- tfc3.out$data.out.nobase

  ##########################
  ## get age of diagnosis ##
  ##########################
  mci.test.name <- NULL
  control.data <- NULL
  data <- motor.data
  variable.name <- var.names$dcl.name
  age.name <- var.names$age.name.motor
  event.name <- var.names$event.name.motor
  followup.name <- var.names$followup.motor
  event.info <- "motor"


  ##################################
  ## diagnosis criteria: DCL >=4  ##
  ##################################
  new.variable.name <- "hdage"
  cutoff <- 4

  out <- get.event.data(id.name,
                        mci.test.name,
                        control.data,
                        data,
                        dates.data=dates.data,
                        censorage,
                        variable.name,
                        new.variable.name,
                        age.name=age.name,
                        time.name=var.names$time.name,
                        time.name.order=var.names$time.name.order,
                        time.baseline=var.names$time.baseline,
                        event.name=event.name,
                        followup.name=followup.name,
                        tms.name=var.names$tms.name,
                        study,
                        event.info=event.info,
                        cutoff=cutoff,
                        gene.know=var.names$gene.know)

  age.diag <- out$data.out
  age.diag.base <- out$data.out.base
  age.diag.nobase <- out$data.out.nobase

  #################################
  ## diagnosis criteria: DCL >=3 ##
  #################################
  new.variable.name <- "hdage_new"
  cutoff <- 3

  out <- get.event.data(id.name,
                        mci.test.name,
                        control.data,
                        data,
                        dates.data=dates.data,
                        censorage,
                        variable.name,
                        new.variable.name,
                        age.name=age.name,
                        time.name=var.names$time.name,
                        time.name.order=var.names$time.name.order,
                        time.baseline=var.names$time.baseline,
                        event.name=event.name,
                        followup.name=followup.name,
                        tms.name=var.names$tms.name,
                        study,
                        event.info=event.info,
                        cutoff=cutoff,
                        gene.know=var.names$gene.know)

  age.diag.new <- out$data.out
  age.diag.new.base <- out$data.out.base
  age.diag.new.nobase <- out$data.out.nobase


  ##############################################
  ## get first ages of behavioral impairment  ##
  ##############################################
  beh.ages <- NULL
  beh.ages.mci <- NULL

  if(!is.null(behavior.data)){
    ## Common variables
    control.data <- beh.data
    data <- behavior.data
    event.info <- "behavior"
    age.name <- var.names$age.name.behavior
    event.name <- var.names$event.name
    followup.name <- var.names$followup.behavior

    ## Apathy severity: COHORT, PHAROS, PREDICT, ENROLL
    ##0 = no evidence,
    ##1 = equivocal,
    ##2 = mild apathy-subject not initiating conversation or activity but is responsive,
    ##3 = moderate apathy-sometimes responds to efforts to get involved in conversation/activities,
    ##4 = severe apathy-generally unresponsive to attempts to involve subject in activities or conversation

    ## Apathy PBA: TRACK
    ## For behavioral impairments, we took results from PBA assessment. See Kingma et al (2008; General Hospital Psychiatry) in "Notes" folder.
    ## The 5-point PBA rating scales, one subscale for severity and one for frequency, are modelled after the behavioural section of the UHDRS,
    ## using the scores 0 (absent) 1 (questionable), 2 (mild), 3 (moderate) and 4 (severe).
    ## Unlike the UHDRS, which rates behaviour in the last 6 months, the PBA solely assesses behavioural problems in the 4 weeks prior to the interview.

    variable.name <- var.names$apathy.sev.name
    new.variable.name <- "apt2"
    cutoff <- var.names$cutoff.apathy
    mci.test.name <- NULL
    beh.ages <- c(beh.ages,new.variable.name)

    apathy <- get.event.data(id.name,
                             mci.test.name,
                             control.data,
                             data,
                             dates.data=dates.data,
                             censorage,
                             variable.name,
                             new.variable.name,
                             age.name=age.name,
                             time.name=var.names$time.name,
                             time.name.order=var.names$time.name.order,
                             time.baseline=var.names$time.baseline,
                             event.name=event.name,
                             followup.name=followup.name,
                             tms.name=var.names$tms.name,
                             study,
                             event.info=event.info,
                             cutoff=cutoff,
                             gene.know=var.names$gene.know)

    apathy.age2 <- apathy$data.out
    apathy.age2.nobase <- apathy$data.out.nobase


    ############################################
    ## apathy MCI: score > mean + 1.5SD##
    ############################################
    variable.name <- c(var.names$apathy.sev.name,
                       var.names$apathy.frq.name)
    new.variable.name <- "aptmci"
    cutoff <- "upper"
    mci.test.name <- "APATHYMULT"
    beh.ages.mci <- c(beh.ages.mci,new.variable.name)


    apathy.info <- get.event.data(id.name,
                                  mci.test.name,
                                  control.data,
                                  data,
                                  dates.data=dates.data,
                                  censorage,
                                  variable.name,
                                  new.variable.name,
                                  age.name=age.name,
                                  time.name=var.names$time.name,
                                  time.name.order=var.names$time.name.order,
                                  time.baseline=var.names$time.baseline,
                                  event.name=event.name,
                                  followup.name=followup.name,
                                  tms.name=var.names$tms.name,
                                  study,
                                  event.info=event.info,
                                  cutoff=cutoff,
                                  gene.know=var.names$gene.know)

    apathy.age3 <- apathy.info$data.out
    apathy.age3.nobase <- apathy.info$data.out.nobase


    ## Irritability severity: COHORT, PHAROS, PREDICT, ENROLL
    ## 0 = behavior well controlled,
    ## 1 = questionable or equivocal,
    ## 2 = definite but mild,
    ## 3 = moderate, others change their behavior to avoid irritating subject,
    ## 4 = severe irritability

    ## Irritability PBA: TRACK
    ## For behavioral impairments, we took results from PBA assessment. See Kingma et al (2008; General Hospital Psychiatry) in "Notes" folder.
    ## The 5-point PBA rating scales, one subscale for severity and one for frequency, are modelled after the behavioural section of the UHDRS,
    ## using the scores 0 (absent) 1 (questionable), 2 (mild), 3 (moderate) and 4 (severe).
    ## Unlike the UHDRS, which rates behaviour in the last 6 months, the PBA solely assesses behavioural problems in the 4 weeks prior to the interview.

    variable.name <- var.names$irb.sev.name
    new.variable.name <- "irb2"
    cutoff <- var.names$cutoff.irb
    mci.test.name <- NULL
    beh.ages <- c(beh.ages,new.variable.name)


    irb <- get.event.data(id.name,
                          mci.test.name,
                          control.data,
                          data,
                          dates.data=dates.data,
                          censorage,
                          variable.name,
                          new.variable.name,
                          age.name=age.name,
                          time.name=var.names$time.name,
                          time.name.order=var.names$time.name.order,
                          time.baseline=var.names$time.baseline,
                          event.name=event.name,
                          followup.name=followup.name,
                          tms.name=var.names$tms.name,
                          study,
                          event.info=event.info,
                          cutoff=cutoff,
                          gene.know=var.names$gene.know)

    irritate.age2 <- irb$data.out
    irritate.age2.nobase <- irb$data.out.nobase

    ############################################
    ## irritability MCI:  score > mean + 1.5SD##
    ############################################
    variable.name <- c(var.names$irb.sev.name,
                       var.names$irb.frq.name)
    new.variable.name <- "irbmci"
    cutoff <- "upper"
    mci.test.name <- "IRBMULT"
    beh.ages.mci <- c(beh.ages.mci,new.variable.name)


    irb.info <- get.event.data(id.name,
                               mci.test.name,
                               control.data,
                               data,
                               dates.data=dates.data,
                               censorage,
                               variable.name,
                               new.variable.name,
                               age.name=age.name,
                               time.name=var.names$time.name,
                               time.name.order=var.names$time.name.order,
                               time.baseline=var.names$time.baseline,
                               event.name=event.name,
                               followup.name=followup.name,
                               tms.name=var.names$tms.name,
                               study,
                               event.info=event.info,
                               cutoff=cutoff,
                               gene.know=var.names$gene.know)

    irritate.age3 <- irb.info$data.out
    irritate.age3.nobase <- irb.info$data.out.nobase

    ## Depression severity: COHORT, PHAROS, PREDICT, ENROLL
    ## 0 = no mood disturbance,
    ## 1 = questionable or equivocal,
    ## 2 = mild, responds to redirection and reassurance,
    ## 3= moderately depressed, expresses distress,
    ## 4 = severe, significant suffering and loss of functioning

    ## depression score: TRACK
    ## See paper by Snaith et al (2003) in "Notes" folder of TRACK
    ## This enabled a reduction of the number of items in the questionnaire to just seven reflecting anxiety and seven reflecting depression.
    ## (Of the seven depression items five reflected aspects of reduction in pleasure response).
    ## Each item had been answered by the patient on a four point (0–3) response category so the possible scores ranged from
    ##  0 to 21 for anxiety and 0 to 21 for depression.
    ## An analysis of scores on the two subscales of a further sample, in the same clinical setting, enabled provision of information that
    ##   a score of 0 to 7 for either subscale could be regarded as being in the normal range,
    ##   a score of 11 or higher indicating probable presence ('caseness') of the mood disorder and
    ##   a score of 8 to 10 being just suggestive of the presence of the respective state.
    ## Further work indicated that the two subscales, anxiety and depression, were independent measures.

    variable.name <- var.names$dep.sev.name
    new.variable.name <- "dep2"
    cutoff <- var.names$cutoff.dep
    mci.test.name <- NULL
    beh.ages <- c(beh.ages,new.variable.name)

    dep <- get.event.data(id.name,
                          mci.test.name,
                          control.data,
                          data,
                          dates.data=dates.data,
                          censorage,
                          variable.name,
                          new.variable.name,
                          age.name=age.name,
                          time.name=var.names$time.name,
                          time.name.order=var.names$time.name.order,
                          time.baseline=var.names$time.baseline,
                          event.name=event.name,
                          followup.name=followup.name,
                          tms.name=var.names$tms.name,
                          study,
                          event.info=event.info,
                          cutoff=cutoff,
                          gene.know=var.names$gene.know)

    depression.age2 <- dep$data.out
    depression.age2.nobase <- dep$data.out.nobase

    ############################################
    ## depression MCI:  score > mean + 1.5SD##
    ############################################
    variable.name <- c(var.names$dep.sev.name,var.names$dep.frq.name)
    new.variable.name <- "depmci"
    cutoff <- "upper"
    mci.test.name <- "DEPMULT"
    beh.ages.mci <- c(beh.ages.mci,new.variable.name)

    dep.info <- get.event.data(id.name,
                               mci.test.name,
                               control.data,
                               data,
                               dates.data=dates.data,
                               censorage,
                               variable.name,
                               new.variable.name,
                               age.name=age.name,
                               time.name=var.names$time.name,
                               time.name.order=var.names$time.name.order,
                               time.baseline=var.names$time.baseline,
                               event.name=event.name,
                               followup.name=followup.name,
                               tms.name=var.names$tms.name,
                               study,
                               event.info=event.info,
                               cutoff=cutoff,
                               gene.know=var.names$gene.know)

    depression.age3 <- dep.info$data.out
    depression.age3.nobase <- dep.info$data.out.nobase
  } else {
    apathy.age2 <- NULL
    apathy.age2.nobase <- NULL
    irritate.age2 <- NULL
    irritate.age2.nobase <- NULL
    depression.age2 <- NULL
    depression.age2.nobase <- NULL

    apathy.age3 <- NULL
    apathy.age3.nobase <- NULL
    irritate.age3 <- NULL
    irritate.age3.nobase <- NULL
    depression.age3 <- NULL
    depression.age3.nobase <- NULL
  }




  ####################################
  ## Mild Cognitive Impairment ages ##
  ####################################
  criteria <- "mci"

  control.data <- mci.data
  data <- cognitive.data

  ## append education and age at baseline information to
  ##  determine which categories to compare to
  data.to.merge <- list(cognitive.data=cognitive.data,
                        education2=education2,
                        birth.base.age=birth.base.age)

  data <- merge.all(data.to.merge,by.name=id.name)

  event.info <- "mci"
  age.name <- var.names$age.name.mci
  event.name <- var.names$event.name
  followup.name <- var.names$followup.mci
  cutoff <- "lower"
  mci.ages <- NULL

  ## SDMT
  ## raw score
  if(!is.null(var.names$sdmt.name)){
    variable.name <- var.names$sdmt.name
    new.variable.name <- "sdmt"
    mci.test.name <- "SDMT"
    mci.ages <- c(mci.ages,new.variable.name)

    ################################################
    ## use all people to determine MCI categories ##
    ################################################
    subset.cutoffs <- FALSE

    mci.info <- get.event.data(id.name,
                               mci.test.name,
                               control.data,
                               data,
                               dates.data=dates.data,
                               censorage,
                               variable.name,
                               new.variable.name,
                               age.name=age.name,
                               time.name=var.names$time.name,
                               time.name.order=var.names$time.name.order,
                               time.baseline=var.names$time.baseline,
                               event.name=event.name,
                               followup.name=followup.name,
                               tms.name=var.names$tms.name,
                               study,
                               event.info=event.info,
                               cutoff=cutoff,
                               gene.know=var.names$gene.know,
                               subset.cutoffs=subset.cutoffs)

    sdmt.mci.age <- mci.info$data.out
    sdmt.mci.age.nobase <- mci.info$data.out.nobase

    ##################################
    ## use different MCI categories ##
    ##################################
    if(use.normative.data==FALSE){
      subset.cutoffs <- TRUE
      new.variable.name <- paste0(new.variable.name,"_cat")

      mci.info <- get.event.data(id.name,
                                 mci.test.name,
                                 control.data,
                                 data,
                                 dates.data=dates.data,
                                 censorage,
                                 variable.name,
                                 new.variable.name,
                                 age.name=age.name,
                                 time.name=var.names$time.name,
                                 time.name.order=var.names$time.name.order,
                                 time.baseline=var.names$time.baseline,
                                 event.name=event.name,
                                 followup.name=followup.name,
                                 tms.name=var.names$tms.name,
                                 study,
                                 event.info=event.info,
                                 cutoff=cutoff,
                                 gene.know=var.names$gene.know,
                                 subset.cutoffs=subset.cutoffs)

      sdmt.mci.category.age <- mci.info$data.out
      sdmt.mci.category.age.nobase <- mci.info$data.out.nobase
    } else {
      sdmt.mci.category.age <- NULL
      sdmt.mci.category.age.nobase <- NULL
    }

  } else {
    sdmt.mci.age <- NULL
    sdmt.mci.age.nobase <- NULL
    sdmt.mci.category.age <- NULL
    sdmt.mci.category.age.nobase <- NULL
  }

  ## Stroop color
  ## number correct
  if(!is.null(var.names$stroop_color.name)){
    variable.name <- var.names$stroop_color.name
    new.variable.name <- "strcol"
    mci.test.name <- "STROOP_COLOR"
    mci.ages <- c(mci.ages,new.variable.name)

    ################################################
    ## use all people to determine MCI categories ##
    ################################################
    subset.cutoffs <- FALSE

    mci.info <- get.event.data(id.name,
                               mci.test.name,
                               control.data,
                               data,
                               dates.data=dates.data,
                               censorage,
                               variable.name,
                               new.variable.name,
                               age.name=age.name,
                               time.name=var.names$time.name,
                               time.name.order=var.names$time.name.order,
                               time.baseline=var.names$time.baseline,
                               event.name=event.name,
                               followup.name=followup.name,
                               tms.name=var.names$tms.name,
                               study,
                               event.info=event.info,
                               cutoff=cutoff,
                               gene.know=var.names$gene.know,
                               subset.cutoffs)

    stroop.color.mci.age <- mci.info$data.out
    stroop.color.mci.age.nobase<- mci.info$data.out.nobase

    ##################################
    ## use different MCI categories ##
    ##################################
    if(use.normative.data==FALSE){
      subset.cutoffs <- TRUE
      new.variable.name <- paste0(new.variable.name,"_cat")


      mci.info <- get.event.data(id.name,
                                 mci.test.name,
                                 control.data,
                                 data,
                                 dates.data=dates.data,
                                 censorage,
                                 variable.name,
                                 new.variable.name,
                                 age.name=age.name,
                                 time.name=var.names$time.name,
                                 time.name.order=var.names$time.name.order,
                                 time.baseline=var.names$time.baseline,
                                 event.name=event.name,
                                 followup.name=followup.name,
                                 tms.name=var.names$tms.name,
                                 study,
                                 event.info=event.info,
                                 cutoff=cutoff,
                                 gene.know=var.names$gene.know,
                                 subset.cutoffs)

      stroop.color.mci.category.age <- mci.info$data.out
      stroop.color.mci.category.age.nobase<- mci.info$data.out.nobase
    } else {
      stroop.color.mci.category.age <- NULL
      stroop.color.mci.category.age.nobase<- NULL
    }

  } else {
    stroop.color.mci.age <- NULL
    stroop.color.mci.age.nobase<- NULL
    stroop.color.mci.category.age <- NULL
    stroop.color.mci.category.age.nobase<- NULL

  }

  ## Stroop word
  ## number correct
  if(!is.null(var.names$stroop_word.name)){
    variable.name <- var.names$stroop_word.name
    new.variable.name <- "strwrd"
    mci.test.name <- "STROOP_WORD"
    mci.ages <- c(mci.ages,new.variable.name)

    ################################################
    ## use all people to determine MCI categories ##
    ################################################
    subset.cutoffs <- FALSE

    mci.info <- get.event.data(id.name,
                               mci.test.name,
                               control.data,
                               data,
                               dates.data=dates.data,
                               censorage,
                               variable.name,
                               new.variable.name,
                               age.name=age.name,
                               time.name=var.names$time.name,
                               time.name.order=var.names$time.name.order,
                               time.baseline=var.names$time.baseline,
                               event.name=event.name,
                               followup.name=followup.name,
                               tms.name=var.names$tms.name,
                               study,
                               event.info=event.info,
                               cutoff=cutoff,
                               gene.know=var.names$gene.know)

    stroop.word.mci.age <- mci.info$data.out
    stroop.word.mci.age.nobase<- mci.info$data.out.nobase

    ################################################
    ## use different MCI categories ##
    ## - not applicable to normative data
    ################################################
    if(use.normative.data==FALSE){
      subset.cutoffs <- TRUE
      new.variable.name <- paste0(new.variable.name,"_cat")

      mci.info <- get.event.data(id.name,
                                 mci.test.name,
                                 control.data,
                                 data,
                                 dates.data=dates.data,
                                 censorage,
                                 variable.name,
                                 new.variable.name,
                                 age.name=age.name,
                                 time.name=var.names$time.name,
                                 time.name.order=var.names$time.name.order,
                                 time.baseline=var.names$time.baseline,
                                 event.name=event.name,
                                 followup.name=followup.name,
                                 tms.name=var.names$tms.name,
                                 study,
                                 event.info=event.info,
                                 cutoff=cutoff,
                                 gene.know=var.names$gene.know)

      stroop.word.mci.category.age <- mci.info$data.out
      stroop.word.mci.category.age.nobase<- mci.info$data.out.nobase
    } else {
      stroop.word.mci.category.age <- NULL
      stroop.word.mci.category.age.nobase<- NULL
    }
  } else {
    stroop.word.mci.age <- NULL
    stroop.word.mci.age.nobase<- NULL
    stroop.word.mci.category.age <- NULL
    stroop.word.mci.category.age.nobase<- NULL

  }


  ## Stroop interference
  ## number correct
  if(!is.null(var.names$stroop_interference.name)){
    variable.name <- var.names$stroop_interference.name
    new.variable.name <- "strint"
    mci.test.name <- "STROOP_INTERFERENCE"
    mci.ages <- c(mci.ages,new.variable.name)

    ################################################
    ## use all people to determine MCI categories ##
    ################################################
    subset.cutoffs <- FALSE

    mci.info <- get.event.data(id.name,
                               mci.test.name,
                               control.data,
                               data,
                               dates.data=dates.data,
                               censorage,
                               variable.name,
                               new.variable.name,
                               age.name=age.name,
                               time.name=var.names$time.name,
                               time.name.order=var.names$time.name.order,
                               time.baseline=var.names$time.baseline,
                               event.name=event.name,
                               followup.name=followup.name,
                               tms.name=var.names$tms.name,
                               study,
                               event.info=event.info,
                               cutoff=cutoff,
                               gene.know=var.names$gene.know)

    stroop.interference.mci.age <- mci.info$data.out
    stroop.interference.mci.age.nobase <- mci.info$data.out.nobase

    ################################################
    ## use different MCI categories ##
    ################################################
    if(use.normative.data==FALSE){
      subset.cutoffs <- TRUE
      new.variable.name <- paste0(new.variable.name,"_cat")

      mci.info <- get.event.data(id.name,
                                 mci.test.name,
                                 control.data,
                                 data,
                                 dates.data=dates.data,
                                 censorage,
                                 variable.name,
                                 new.variable.name,
                                 age.name=age.name,
                                 time.name=var.names$time.name,
                                 time.name.order=var.names$time.name.order,
                                 time.baseline=var.names$time.baseline,
                                 event.name=event.name,
                                 followup.name=followup.name,
                                 tms.name=var.names$tms.name,
                                 study,
                                 event.info=event.info,
                                 cutoff=cutoff,
                                 gene.know=var.names$gene.know)

      stroop.interference.mci.category.age <- mci.info$data.out
      stroop.interference.mci.category.age.nobase <- mci.info$data.out.nobase
    } else {
      stroop.interference.mci.category.age <- NULL
      stroop.interference.mci.category.age.nobase <- NULL
    }
  } else {
    stroop.interference.mci.age <- NULL
    stroop.interference.mci.age.nobase <- NULL
    stroop.interference.mci.category.age <- NULL
    stroop.interference.mci.category.age.nobase <- NULL

  }

  ##########################
  ## merge all onset ages ##
  ##########################
  onset.to.merge <- list(
    age.diag=age.diag,
    age.diag.base=age.diag.base,
    age.diag.nobase=age.diag.nobase,

    age.diag.new=age.diag.new,
    age.diag.new.base=age.diag.new.base,
    age.diag.new.nobase=age.diag.new.nobase,



    apathy.age2=apathy.age2,
    apathy.age2.nobase=apathy.age2.nobase,

    irritate.age2=irritate.age2,
    irritate.age2.nobase=irritate.age2.nobase,

    depression.age2=depression.age2,
    depression.age2.nobase=depression.age2.nobase,

    apathy.age3=apathy.age3,
    apathy.age3.nobase=apathy.age3.nobase,

    irritate.age3=irritate.age3,
    irritate.age3.nobase=irritate.age3.nobase,

    depression.age3=depression.age3,
    depression.age3.nobase=depression.age3.nobase,


    sdmt.mci.age=sdmt.mci.age,
    sdmt.mci.age.nobase=sdmt.mci.age.nobase,

    stroop.color.mci.age=stroop.color.mci.age,
    stroop.color.mci.age.nobase=stroop.color.mci.age.nobase,

    stroop.word.mci.age=stroop.word.mci.age,
    stroop.word.mci.age.nobase=stroop.word.mci.age.nobase,

    stroop.interference.mci.age=stroop.interference.mci.age,
    stroop.interference.mci.age.nobase=stroop.interference.mci.age.nobase,


    ##############################
    ## different MCI categories ##
    ##############################
    sdmt.mci.category.age=sdmt.mci.category.age,
    sdmt.mci.category.age.nobase=sdmt.mci.category.age.nobase,

    stroop.color.mci.category.age=stroop.color.mci.category.age,
    stroop.color.mci.category.age.nobase=stroop.color.mci.category.age.nobase,

    stroop.word.mci.category.age=stroop.word.mci.category.age,
    stroop.word.mci.category.age.nobase=stroop.word.mci.category.age.nobase,

    stroop.interference.mci.category.age=stroop.interference.mci.category.age,
    stroop.interference.mci.category.age.nobase=stroop.interference.mci.category.age.nobase,

    ################
    ## TFC stages ##
    ################
    tfc.stage1.age =tfc.stage1.age,
    tfc.stage1.age.nobase=tfc.stage1.age.nobase,
    tfc.stage2.age =tfc.stage2.age,
    tfc.stage2.age.nobase=tfc.stage2.age.nobase,
    tfc.stage3.age =tfc.stage3.age,
    tfc.stage3.age.nobase=tfc.stage3.age.nobase#,
    ##cog.age=cog.age,
    ##apathy.age=apathy.age,
    ##irritate.age=irritate.age,
    ##depression.age=depression.age,
    ##motor.symptom=motor.symptom,
  )

  all.onset <- merge.all(onset.to.merge,by.name=id.name)


  #################################################
  ## merge data sets together with baseline data ##
  #################################################
  baseline.to.merge <- list(all.onset=all.onset,
                            #education=education,
                            education2=education2,
                            cag=cag,
                            gender=gender,
                            #gene.know.base=gene.know.base,
                            #gene.know.nobase=gene.know.nobase,
                            #gene.know.new.base=gene.know.new.base,
                            #gene.know.new.nobase=gene.know.new.nobase,
                            birth.base.age=birth.base.age,
                            race=race,
                            lastyear=censorage,
                            site.information=site.information
  )

  all.data <- merge.all(baseline.to.merge,by.name=id.name)


  ###################
  ## get one event ##
  ###################
  if(!is.null(mci.ages)){
    ## at least one cognitive impairment
    one.mci <- get.mci.events(id.name,all.data,onset.ages=mci.ages,name="mcione",
                              mci.event.min=1,mci.event.max=1e10)
    one.mci.nobase <- get.mci.events(id.name,all.data,
                                     onset.ages=paste(mci.ages,"nobase",sep="_"),name="mcione_nobase",
                                     mci.event.min=1,mci.event.max=1e10)


    ## only one cognitive impairment (single-domain MCI)
    single_domain.mci <- get.mci.events(id.name,all.data,
                                        onset.ages=mci.ages,
                                        name="mcisd",
                                        mci.event.min=1,mci.event.max=1)

    single_domain.mci.nobase <- get.mci.events(id.name,
                                               all.data,
                                               onset.ages=paste0(mci.ages,"_nobase"),
                                               name="mcisd_nobase",
                                               mci.event.min=1,mci.event.max=1)

    ## at least 2 cognitive impairments (multi-domain MCI)
    multi_domain.mci <- get.mci.events(id.name,all.data,
                                       onset.ages=mci.ages,
                                       name="mcimd",
                                       mci.event.min=2,mci.event.max=1e10)
    multi_domain.mci.nobase <- get.mci.events(id.name,
                                              all.data,
                                              onset.ages=paste0(mci.ages,"_nobase"),
                                              name="mcimd_nobase",
                                              mci.event.min=2,mci.event.max=1e10)

    if(use.normative.data==FALSE){
      ## define single and multi-domain MCI based on categories

      single_domain.mci <- get.mci.events(id.name,all.data,
                                          onset.ages=paste0(mci.ages,"_cat"),
                                          name="mcisd",
                                          mci.event.min=1,mci.event.max=1)

      single_domain.mci.nobase <- get.mci.events(id.name,
                                                 all.data,
                                                 onset.ages=paste0(mci.ages,"_cat_nobase"),
                                                 name="mcisd_nobase",
                                                 mci.event.min=1,mci.event.max=1)

      ## at least 2 cognitive impairments (multi-domain MCI)
      multi_domain.mci <- get.mci.events(id.name,all.data,
                                         onset.ages=paste0(mci.ages,"_cat"),
                                         name="mcimd",
                                         mci.event.min=2,mci.event.max=1e10)

      multi_domain.mci.nobase <- get.mci.events(id.name,
                                                all.data,
                                                onset.ages=paste0(mci.ages,"_cat_nobase"),
                                                name="mcimd_nobase",
                                                mci.event.min=2,mci.event.max=1e10)
    }
  } else{
    one.mci <- NULL
    one.mci.nobase <- NULL
    single_domain.mci <- NULL
    single_domain.mci.nobase <- NULL
    multi_domain.mci <- NULL
    multi_domain.mci.nobase <- NULL
  }

  if(!is.null(beh.ages)){
    one.beh <- get.mci.events(id.name,all.data,onset.ages=beh.ages,name="behone",mci.event.min=1,mci.event.max=1e10)
    one.beh.nobase <- get.mci.events(id.name,all.data,onset.ages=paste(beh.ages,"nobase",sep="_"),name="behone_nobase",mci.event.min=1,mci.event.max=1e10)
    all.beh <- get.mci.events(id.name,all.data,onset.ages=beh.ages,name="behall",mci.event.min=1,mci.event.max=1e10)
    all.beh.nobase <- get.mci.events(id.name,all.data,onset.ages=paste(beh.ages,"nobase",sep="_"),name="behall_nobase",mci.event.min=1,mci.event.max=1e10)

  } else{
    one.beh <- NULL
    one.beh.nobase <- NULL
    all.beh <- NULL
    all.beh.nobase <- NULL
  }

  if(!is.null(beh.ages.mci)){
    one.beh.mci <- get.mci.events(id.name,all.data,onset.ages=beh.ages.mci,name="behmcione",mci.event.min=1,mci.event.max=1e10)
    all.beh.mci <- get.mci.events(id.name,all.data,onset.ages=beh.ages.mci,name="behmciall",mci.event.min=1,mci.event.max=1e10)

  } else{
    one.beh.mci <- NULL
    all.beh.mci <- NULL
  }

  #################################################
  ## merge data sets together with one event data ##
  #################################################
  one.event.to.merge <- list(all.data=all.data,
                             one.mci=one.mci,
                             one.mci.nobase=one.mci.nobase,
                             single_domain.mci=single_domain.mci,
                             single_domain.mci.nobase=single_domain.mci.nobase,
                             multi_domain.mci=multi_domain.mci,
                             multi_domain.mci.nobase=multi_domain.mci.nobase,


                             one.beh=one.beh,
                             one.beh.nobase=one.beh.nobase,
                             all.beh=all.beh,
                             all.beh.nobase=all.beh.nobase,
                             one.beh.mci=one.beh.mci,
                             all.beh.mci=all.beh.mci
  )

  all.data <- merge.all(one.event.to.merge,by.name=id.name)


  #######################
  ## write data output ##
  #######################
  write.csv(all.data,paste(location,study,"_data.csv",sep=""),row.names=FALSE)

}



###############################################
## Main functions used to organize real data ##
###############################################

#############################################
## function to extract data to be analyzed ##
#############################################
data.for.analysis <- function(all.data,baseage.cutoff,
                              cag.at.risk.cutoff=c(36,50),
                              cag.not.at.risk.cutoff=30,
                              study,data.to.use=c("at_risk","not_at_risk","all")[1],
                              print.clean.data=TRUE){

  ##############################################
  ## Extract data for which baseline age >=18 ##
  ##############################################
  if(study!="enroll"){
    data.out <- all.data[which(all.data[,"base_age"]>=baseage.cutoff),]
  } else {
    data.out <-
      all.data[which(as.numeric.factor(all.data[,"base_age"])>=baseage.cutoff),]
  }


  ######################
  ## subjects at risk ##
  ######################
  index.cag <- which(data.out[,"CAG"]>=cag.at.risk.cutoff[1] &
                       data.out[,"CAG"] <=cag.at.risk.cutoff[2])
  data.at.risk <- data.out[index.cag,]

  ##########################
  ## subjects not at risk ##
  ##########################
  index.not.risk <- which(data.out[,"CAG"]<= cag.not.at.risk.cutoff)
  data.not.at.risk <- data.out[index.not.risk,]

  ###############
  ## Data sets ##
  ###############
  if(data.to.use=="at_risk"){
    data.sets <- data.at.risk
    filename <- c("clean_")
  } else if(data.to.use=="not_at_risk"){
    data.sets <- data.not.at.risk
    filename <- c("clean_norisk_")
  } else if(data.to.use=="all"){
    data.sets <- data.out
    filename <- c("clean_all_")
  }

  ############################
  ## write out data at risk ##
  ############################
  if(print.clean.data==TRUE){
    write.table(data.sets,paste(filename,study,".dat",sep=""),row.names=FALSE,col.names=TRUE)
  }

  return(data.sets)
}


#########################################
## Function to compare group variables ##
#########################################


get.my.tables.stat <- function(all.data,zz.use,s.use.names,delta.names,cutoff=50){

  ########################################
  ## convert uniform CAG to regular CAG ##
  ########################################
  all.data$CAG <- convert.cag(all.data$CAG,xmin=36,xmax=cutoff)

  #################
  ## sample size ##
  #################
  sample.size <- nrow(all.data)
  names(sample.size) <- "Sample size"

  #####################
  ## censoring rates ##
  #####################
  censoring.out <- apply(all.data[,delta.names],2,get.censoring)
  names(censoring.out) <- s.use.names


  ####################
  ## covariate info ##
  ####################

  info.covariates.to.report <- function(name){
    if(name=="base_age"){
      out <- get.mean.sd(all.data[,name],digits=2)
      names(out) <- "Average age at baseline (SD)"
    } else if(name=="CAG"){
      out <- get.mean.sd(all.data[,name],digits=2)
      names(out) <- "Average CAG-repeat length (SD)"
    } else if(name=="gender"){
      out <- round(mean(all.data[,name])*100,digits=2)
      names(out) <- " \\% Female"
    } else if(name=="educ_cat"){
      out <- round(mean(all.data[,name])*100,digits=2)
      names(out) <- " \\% Subjects with $\\geq$ 15 years of education"
    } else if(grepl("gene.*",name)){
      out <- round(mean(all.data[,name])*100,digits=2)
      names(out) <- " \\% Subjects with gene knowledge known"
    }
    return(out)
  }

  aout <- NULL
  cov.use <- c("CAG",zz.use)
  for(ii in 1:length(cov.use)){
    aout <- c(aout,info.covariates.to.report(cov.use[ii]))
  }

  data.out <- data.matrix(c(sample.size,censoring.out,aout))

  return(data.out)
}



######################################
## summary table of nonconverters   ##
######################################

nonconverters.table <- function(study.names,covariates.use,my.data,
                                results.to.report=c("mean-sd","quantiles")[1]){

  #############################################################
  ## put my.data in one big data frame, with study indicator ##
  #############################################################
  ## put my.data in a data frame
  my.data.all <- merge.all.data(study.names,my.data)

  ## Extract complete cases based on covariates ##
  data.sample <- my.data.all[complete.cases(my.data.all[,
                                                        c(covariates.use)]),]

  ## table.names.use
  table.names.use <- table.names


  cov.use <- c(covariates.use)
  cov.names <- unlist(table.names.use[cov.use])

  data.out <- array(NA,dim=c(1+length(cov.names),length(study.names)),
                    dimnames=list(c("Sample size", cov.names),study.names))

  for(ss in 1:length(study.names)){
    index.use <- which(data.sample$study==study.names[ss])
    data.use <- data.sample[index.use,]

    ## sample size information
    data.out["Sample size",ss] <- nrow(data.use)

    ## other covariate information
    for(ii in 1:length(cov.use)){
      data.out[cov.names[ii],ss] <-
        info.covariates(data.use,cov.use[ii],cov.names[ii],
                        results.to.report)
    }
  }


  return(data.out)
}


##################################################################
## put list of data in one big data frame, with study indicator ##
##################################################################
merge.all.data <- function(study.names,my.data){

  ## put my.data in a data frame
  my.data.all <- data.frame(study=study.names[1],
                            my.data[[study.names[1]]])

  for(ss in 2:length(study.names)){
    my.data.all.tmp <- data.frame(study=study.names[ss],
                                  my.data[[study.names[ss]]])
    my.data.all <- merge(my.data.all,my.data.all.tmp,all=TRUE)
  }
  return(my.data.all)
}

##################################################################
## Extract complete cases based on events, censoring indicator	##
##  and covariates 												##
##################################################################
extract.complete.cases <- function(main.events.use,delta.main.events.use,covariates.use,
                                   my.data.all){
  data.sample <- my.data.all[complete.cases(my.data.all[,
                                                        c(main.events.use,delta.main.events.use,covariates.use)]),]
  return(data.sample)
}

#######################################################################
## function to convert factor to numeric without loss of information ##
#######################################################################
as.numeric.factor <- function(x) {as.numeric(levels(x))[x]}


######################################
## summary table of characteristics ##
######################################

clinical.tables <- function(study.names,covariates.use,events.use,my.data,
                            p.adjust.method=c("none","bonferroni","BH")[1],
                            paper.type=c("clinical","statistics")[1],
                            results.to.report=c("mean-sd","quantiles")[1],
                            main.events.use= c("hdage_nobase","mcione","dep2")){


  ## extract quantiles of a variable
  get.quantiles <- function(x,digits=1){
    if(is.factor(x)){
      x <- as.numeric.factor(x)
    }

    tmp.out <- quantile(x)
    out <- paste(round(tmp.out["50%"],digits),
                 " (",round(tmp.out["25%"],digits),", ",
                 round(tmp.out["75%"],digits),
                 ")",sep="")
    return(out)
  }

  ## extract mean and standard deviation of a variable
  get.mean.sd <- function(x,digits=1){
    if(is.factor(x)){
      x <- as.numeric.factor(x)
    }

    tmp.out <- c(mean(x,na.rm=TRUE),sd(x,na.rm=TRUE))
    tmp.out <- round(tmp.out,digits)
    out <- paste(tmp.out[1]," (",tmp.out[2],")",sep="")
    return(out)
  }

  ## extract total and percentage
  get.total.percentage <- function(x,digits=1){
    if(is.factor(x)){
      x <- as.numeric.factor(x)
    }

    tmp.out <- c(sum(x,na.rm=TRUE),mean(x,na.rm=TRUE)*100)
    tmp.out <- round(tmp.out,digits)
    out <- paste(tmp.out[1]," (",tmp.out[2],")",sep="")
    return(out)
  }


  ## Compare medians using Kruskal-Wallis Test:
  ## The null hypothesis is that the medians of all groups are equal,
  ##  and the alternative hypothesis is that at least one
  ##  population median of one group is different from the
  ##  population median of at least one other group.
  get.kw.test <- function(name,data.use,digits){
    formula.expression <- paste(name,"study",sep="~")
    out <- kruskal.test(eval(parse(text=formula.expression)),
                        data=data.use)
    pvalue <- round(out$p.value,digits)
    return(pvalue)
  }

  get.chisq.test <- function(name,data.use,digits){
    case.vector <- tapply(data.use[,name],data.use[,"study"],sum,na.rm=TRUE)
    total.vector <- table(data.use[,"study"])

    out <- prop.test(case.vector,total.vector)
    pvalue <- round(out$p.value,digits)
    return(pvalue)
  }


  info.covariates <- function(data.use,name,name.use,results.to.report="mean-sd"){
    if(name=="base_age" | name=="CAG" |
       name == "SDMT" | name == "STROOP_COLOR" |
       name == "STROOP_WORD" | name == "STROOP_INTERFERENCE"){
      if(results.to.report=="quantiles"){
        ## report quantiles (format for JAMA journal)
        out <- get.quantiles(as.numeric(data.use[,name]),digits=1)
      } else {
        ## report mean and SD (table for statistics paper)
        out <- get.mean.sd(as.numeric(data.use[,name]),digits=1)
      }
    } else if(name=="gender" | name=="educ_cat" ){
      out <- get.total.percentage(data.use[,name],digits=0)
    } else if(grepl("delta",name)){
      out <- get.total.percentage(data.use[,name],digits=0)
    }

    names(out) <- name.use
    return(out)
  }

  pvalues.covariates <- function(data.use,name,name.use){
    if(name=="base_age" | name=="CAG"){
      out <- get.kw.test(name,data.use,digits=2)
    } else if(name=="gender" | name=="educ_cat" | grepl("delta",name)){
      out <- get.chisq.test(name,data.use,digits=2)
    }

    names(out) <- name.use
    return(out)
  }




  #############################################################
  ## put my.data in one big data frame, with study indicator ##
  #############################################################
  ## put my.data in a data frame
  my.data.all <- merge.all.data(study.names,my.data)

  ##########################################################
  ## Extract complete cases based on: main.events.use 	##
  ##########################################################
  delta.main.events.use <- paste("delta",main.events.use,sep=".")
  data.sample <- extract.complete.cases(main.events.use,
                                        delta.main.events.use,covariates.use,my.data.all)

  ##########################################
  ## get all pair-combinations of studies ##
  ##########################################
  study.combinations <- combn(study.names,2)
  names.pairs <- function(x){
    out <- paste(x,collapse="-")
    return(out)
  }
  colnames(study.combinations) <- apply(study.combinations,2,names.pairs)

  ######################
  ## define delta.use ##
  ######################
  ##delta.use <- colnames(my.data.all)[which(!colnames(my.data.all) %in% covariates.use)]
  ##delta.use <- delta.use[ grepl("delta",delta.use)]
  delta.use <- paste("delta",events.use,sep=".")


  ################################
  ## create data frame to output ##
  ################################
  if(paper.type=="statistics"){
    stat.table.names <- list(base_age="Average age at baseline in years (SD)",
                             CAG="Average CAG repeat-length (SD)",
                             gender="\\% Female",
                             educ_cat="\\% Subjects with $\\geq$ 15 years of education",
                             delta.hdage_nobase="\\% Experience motor-diagnosis (\\% censoring)",
                             delta.mcione="\\% Experience cognitive impairment (\\% censoring)",
                             delta.behone="\\% Experience moderate behavioral abnormality (\\% censoring)",
                             delta.strcol="\\% Experience STRCOL abnormality (\\% censoring)",
                             delta.strwrd="\\% Experience STRWRD abnormality (\\% censoring)",
                             delta.strint="\\% Experience STRINT abnormality (\\% censoring)",
                             delta.sdmt="\\% Experience SDMT abnormality (\\% censoring)",
                             delta.hdage_new_nobase="\\% Experience motor-diagnosis, DCL $\\geq$3 (\\% censoring)",
                             delta.apt2="~~~~~Mild apathy",
                             delta.irb2="~~~~~Mild irritability",
                             delta.dep2="\\% Experience mild depression (\\% censoring)",
                             delta.behall="~~~~~Mild apathy, irritability, and depression"
    )

    table.names.use <- stat.table.names
  } else {
    table.names <- list(base_age="Age at baseline",
                        CAG="CAG repeat length",
                        gender="Female sex",
                        educ_cat="Higher education",
                        delta.hdage_nobase="Experience Motor Phenoconversion",
                        delta.mcione="Experience Cognitive Impairment",
                        delta.mcione_cat="Experience Cognitive Impairment",

                        delta.mcisd="Experience Single-Domain Cognitive Impairment",
                        delta.mcisd_cat="Experience Single-Domain Cognitive Impairment",

                        delta.mcimd="Experience Multi-Domain Cognitive Impairment",
                        delta.mcimd_cat="Experience Multi-Domain Cognitive Impairment",

                        delta.mcisd_nobase="Experience Single-Domain Cognitive Impairment After Baseline",
                        delta.mcisd_cat_nobase="Experience Single-Domain Cognitive Impairment After Baseline",

                        delta.mcimd_nobase="Experience Multi-Domain Cognitive Impairment After Baseline",
                        delta.mcimd_cat_nobase="Experience Multi-Domain Cognitive Impairment After Baseline",


                        delta.behone="Experience Moderate Behavioral Abnormality",
                        delta.strcol="~~~~~Below-healthy stroop color naming score",
                        delta.strcol_cat="~~~~~Below-healthy stroop color naming score",

                        delta.strwrd="~~~~~Below-healthy stroop word naming score",
                        delta.strwrd_cat="~~~~~Below-healthy stroop word naming score",

                        delta.strint="~~~~~Below-healthy stroop interference score",
                        delta.strint_cat="~~~~~Below-healthy stroop interference score",

                        delta.sdmt="~~~~~Below-healthy SDMT score",
                        delta.sdmt_cat="~~~~~Below-healthy SDMT score",

                        delta.mciall="~~~~~Below-healthy on all stroop and SDMT tests",
                        delta.apt2="~~~~~Mild apathy",
                        delta.irb2="~~~~~Mild irritability",
                        delta.dep2="~~~~~Mild depression",
                        delta.behall="~~~~~Mild apathy, irritability, and depression",
                        delta.strcol="~~~~~Below-healthy stroop color naming score",
                        delta.strwrd="~~~~~Below-healthy stroop word naming score",
                        delta.strint="~~~~~Below-healthy stroop interference score",
                        delta.sdmt="~~~~~Below-healthy SDMT score",
                        delta.mciall="~~~~~Below-healthy on all stroop and SDMT tests",
                        delta.apt2="~~~~~Mild apathy",
                        delta.irb2="~~~~~Mild irritability",
                        delta.dep2="~~~~~Mild depression",
                        delta.behall="~~~~~Mild apathy, irritability, and depression",
                        ##
                        delta.aptmci="~~~~~Abnormal apathy",
                        delta.irbmci="~~~~~Abnormal irritability",
                        delta.depmci="~~~~~Abnormal depression",
                        #
                        SDMT= "SDMT",
                        STROOP_COLOR = "Stroop Color",
                        STROOP_WORD = "Stroop Word",
                        STROOP_INTERFERENCE = "Stroop Interference",
                        ## total functional capacity
                        delta.tfcone="Experience Stage 1 TFC",
                        delta.tfctwo="Experience Stage 2 TFC",
                        delta.tfcthree="Experience Stage 3 TFC"
    )



    table.names.use <- table.names
  }

  cov.use <- c(covariates.use,delta.use)
  cov.names <- unlist(table.names.use[cov.use])

  if(paper.type=="clinical"){
    ## clinical paper
    data.out <- array(NA,dim=c(1+length(cov.names),length(study.names)+ncol(study.combinations)+1),
                      dimnames=list(c("Sample size", cov.names),c(study.names,"p-value-ALL",
                                                                  paste("p-value",colnames(study.combinations),sep="-"))))
  } else {
    ## statistics paper
    data.out <- array(NA,dim=c(1+length(cov.names),length(study.names)),
                      dimnames=list(c("Sample size", cov.names),study.names))
  }

  for(ss in 1:length(study.names)){
    index.use <- which(data.sample$study==study.names[ss])
    data.use <- data.sample[index.use,]

    ## sample size information
    data.out["Sample size",ss] <- nrow(data.use)

    ## other covariate information
    for(ii in 1:length(cov.use)){
      data.out[cov.names[ii],ss] <-
        info.covariates(data.use,cov.use[ii],cov.names[ii],results.to.report)
    }
  }

  ######################
  ## compute p-values ##
  ######################
  if(paper.type=="clinical"){
    ## tests for all groups
    for(ii in 1:length(cov.use)){
      data.out[cov.names[ii],"p-value-ALL"] <- pvalues.covariates(data.use=data.sample,cov.use[ii],cov.names[ii])
    }

    ## tests for pairs of groups
    for(ii in 1:length(cov.use)){
      for(ss in 1:ncol(study.combinations)){
        data.use <- data.sample[which(data.sample$study%in% study.combinations[,ss]),]
        data.out[cov.names[ii],paste("p-value",colnames(study.combinations)[ss],sep="-")] <-
          pvalues.covariates(data.use,cov.use[ii],cov.names[ii])
      }

      ## adjust p-values for multiple comparisons
      data.out[cov.names[ii],paste("p-value",colnames(study.combinations),sep="-")] <-
        p.adjust(data.out[cov.names[ii],paste("p-value",colnames(study.combinations),sep="-")],method=p.adjust.method)
    }
  }

  ###########################################################
  ## Add number of subjects who experience all main events ##
  ###########################################################
  data.out <- rbind(data.out,rep(NA,ncol(data.out)))
  rownames(data.out)[nrow(data.out)] <- "Experience all main events"

  myrowsum <- function(x,y){
    return(sum(x)==y)
  }

  for (ss in 1:length(study.names)){
    index.use <- which(data.sample$study==study.names[ss])
    data.use <- data.sample[index.use,]

    ## sample size information
    data.out["Experience all main events",ss] <- sum(apply(data.use[,delta.main.events.use],1,myrowsum,y=length(delta.main.events.use)))
  }


  return(data.out)
}




########################################################################################################
## 2x2 summary table for percentage of subjects who experience each event
########################################################################################################
## Input:
## study.names: names of the studies used in the analysis
## covariates.use: name of covariates used in the analysis. This
##			variable is used to extract the complete cases (i.e., no NA)
##			from the data.
## events.use : name of events used in the analysis
## delta.use : name of censoring indicator used in the analysis
## my.data: data set containing all information
## time_val: time points to evaluate if event occured by that time
## main.events.use: This variable is used to extract the complete cases (i.e., no NA)
##			from the data.  This variable may differ from the events.use.
## participants.experience.all.events: If TRUE, this means we subset
##			the data to those participants that experience all events
##			by the end of the study period (i.e., delta=1).
##			If FALSE, this means, we include participants who may
##			not experience all the events (i.e., delta=0).
two.by.two.events.tables <- function(study.names,
                                     covariates.use,
                                     events.use,
                                     delta.use,
                                     my.data,
                                     time_val,
                                     main.events.use=c("hdage_nobase","mcione"),
                                     participants.experience.all.events=TRUE){

  #############################################################
  ## put my.data in one big data frame, with study indicator ##
  #############################################################
  ## put my.data in a data frame
  my.data.all <- merge.all.data(study.names,my.data)

  ##########################################################
  ## Extract complete cases based on: main.events.use 	##
  ##########################################################
  delta.main.events.use <- paste("delta",main.events.use,sep=".")
  data.sample <- extract.complete.cases(main.events.use,delta.main.events.use,
                                        covariates.use,
                                        my.data.all)

  ###################
  ## create output ##
  ###################
  ## output.two.by.two: this is a list that will contain 2x2 tables
  ## for each study, and when we consider all studies together ("ALL")
  output.two.by.two <- get.empty.list(c(study.names,"ALL"))

  ## create event combinations
  event.combinations.info <- get.combi.info(events.use)
  event.combinations.names <- event.combinations.info$combi.names

  ## we create a 2x2 table output for each time point, and pair of event combinations
  for(rr in 1:length(output.two.by.two)){
    output.two.by.two[[rr]] <- array(0,dim=c(length(time_val),
                                             length(event.combinations.names),
                                             2,2),
                                     dimnames=list(times=paste0("t=",time_val),
                                                   event.combinations=event.combinations.names,
                                                   c("Yes","No"),
                                                   c("Yes","No")))
  }


  #######################
  ## Create 2x2 tables ##
  #######################
  for(rr in 1:length(output.two.by.two)){
    ## we check if we need to subset to the study data
    if(names(output.two.by.two)[rr] %in% study.names){
      ## subset to the study data
      index.use <- which(data.sample$study==study.names[rr])
      data.use <- data.sample[index.use,]
    } else {
      data.use <- data.sample
    }

    ## get subset of data for which delta=1 for all events
    if(participants.experience.all.events==TRUE){
      data.use <- all.events.occur(data.use,delta.use)
      if(!names(output.two.by.two)[rr] %in% study.names){
        ## print number of people in ALL studies
        print(dim(data.use))
      }
    }

    ## get data of interest: events and delta.events
    data.use <- data.use[,c(events.use,delta.use)]

    ## identify which individuals experienced each event by each time point
    for(tt in 1:length(time_val)){
      ## for each time point, create a temporary data set that indicates
      ## if the event occurred by time t or not
      data.tmp <- data.use
      for(ii in 1:length(delta.use)){
        tmp <- (data.use[,events.use[ii]] <=time_val[tt] & data.use[,delta.use[ii]]==1)
        data.tmp <- data.frame(data.tmp,tmp)
        colnames(data.tmp)[ncol(data.tmp)] <- paste(events.use[ii],"occur",sep="_")
      }

      ## create the 2x2 table
      for(uu in 1:ncol(event.combinations.info$combi.choice)){
        ## we look at each combination of events
        event.combinations.use <- event.combinations.info$combi.choice[,uu]
        table.tmp <- table(data.tmp[,paste(event.combinations.use,"occur",sep="_")])
        if("TRUE" %in% rownames(table.tmp) & "TRUE" %in% colnames(table.tmp)){
          output.two.by.two[[rr]][tt,uu,"Yes","Yes"] <- table.tmp["TRUE","TRUE"]
        }

        if("TRUE" %in% rownames(table.tmp) & "FALSE" %in% colnames(table.tmp)){
          output.two.by.two[[rr]][tt,uu,"Yes","No"] <- table.tmp["TRUE","FALSE"]
        }

        if("FALSE" %in% rownames(table.tmp) & "TRUE" %in% colnames(table.tmp)){
          output.two.by.two[[rr]][tt,uu,"No","Yes"] <- table.tmp["FALSE","TRUE"]
        }

        if("FALSE" %in% rownames(table.tmp) & "FALSE" %in% colnames(table.tmp)){
          output.two.by.two[[rr]][tt,uu,"No","No"] <- table.tmp["FALSE","FALSE"]
        }
      }
    }

  }
  return(output.two.by.two)
}


## create porportions from output of two.by.two.events.tables function
two.by.two.tables.proportions <- function(two.by.two.info,
                                          events.use,time_val){

  ## create event combinations
  event.combinations.info <- get.combi.info(events.use)
  event.combinations.names <- event.combinations.info$combi.names

  output <- two.by.two.info

  for(rr in 1:length(output)){
    for(tt in 1:length(time_val)){
      for(uu in 1:ncol(event.combinations.info$combi.choice)){
        output[[rr]][tt,uu,,] <- prop.table(two.by.two.info[[rr]][tt,uu,,])
      }
    }
  }

  return(output)
}


## test for independence in two.by.two.event.tables output
two.by.two.test.independence <- function(two.by.two.info,
                                         events.use,time_val, study.names){

  ## create event combinations
  event.combinations.info <- get.combi.info(events.use)
  event.combinations.names <- event.combinations.info$combi.names

  ###################
  ## create output ##
  ###################
  ## output :
  ## we report p-values for each time point, and pair of event combinations
  output <- array(0,dim=c(length(study.names)+1,
                          length(time_val),
                          length(event.combinations.names)),
                  dimnames=list(
                    study=c(study.names,"ALL"),
                    times=paste0("t=",time_val),
                    event.combinations=event.combinations.names))


  ## Get p-values from chi-squared test
  ## Null Hypothesis: Event 1 and 2 are independent
  ## Alternate Hypothesis: Event 1 and 2 are not independent
  for(rr in 1:length(dimnames(output)$study)){
    for(tt in 1:length(time_val)){
      for(uu in 1:ncol(event.combinations.info$combi.choice)){
        chisquare.info <- chisq.test(two.by.two.info[[rr]][tt,uu,,])
        output[rr,tt,uu] <- chisquare.info$p.value
      }
    }
  }
  return(output)
}


## adjust p-value across event-comparisons
## use output from two.by.two.test.independence function
two.by.two.adjust.pvalues <- function(input,time_val,events.use,
                                      p.adjust.methods){
  ## output
  output <- input

  ## create event combinations
  event.combinations.info <- get.combi.info(events.use)
  event.combinations.names <- event.combinations.info$combi.names

  for(rr in 1:length(dimnames(output)$study)){
    for(tt in 1:length(time_val)){
      ##print(p.adjust.methods)
      output[rr,tt,] <- p.adjust(input[rr,tt,],method=p.adjust.methods)
    }
  }

  ###########################
  ## Print out the results ##
  ###########################
  print.two.by.two.adjust.pvalues(output,p.adjust.methods)

  return(output)
}

## print output from p-value adjustment
print.two.by.two.adjust.pvalues <- function(input,p.adjust.methods){
  ## study names
  study.names.tmp <- dimnames(input)$study

  for(rr in 1:length(study.names.tmp)){
    ############################
    cat("\n\n P-value adjustment:",p.adjust.methods, "for study:", study.names.tmp[rr],"\n\n")
    ############################
    foo <- input[rr,,]
    myprint(foo)
  }

}





########################################################################################################
## summary table for percentage of subjects who experience each event given they have experienced both #
########################################################################################################
## Input:
## study.names: names of the studies used in the analysis
## covariates.use: name of covariates used in the analysis. This
##			variable is used to extract the complete cases (i.e., no NA)
##			from the data.
## events.use : name of events used in the analysis
## my.data: data set containing all information
## time_val: time points to evaluate if event occured by that time
## main.events.use: This variable is used to extract the complete cases (i.e., no NA)
##			from the data.  This variable may differ from the events.use.
## participants.experience.all.events: If TRUE, this means we subset
##			the data to those participants that experience all events
##			by the end of the study period (i.e., delta=1).
##			If FALSE, this means, we include participants who may
##			not experience all the events (i.e., delta=0).
compare.events.tables <- function(study.names,
                                  covariates.use,
                                  events.use,my.data,
                                  time_val,
                                  main.events.use=c("hdage_nobase","mcione"),
                                  participants.experience.all.events=FALSE){

  #############################################################
  ## put my.data in one big data frame, with study indicator ##
  #############################################################
  ## put my.data in a data frame
  my.data.all <- merge.all.data(study.names,my.data)

  ##########################################################
  ## Extract complete cases based on: main.events.use 	##
  ##########################################################
  delta.main.events.use <- paste("delta",main.events.use,sep=".")
  data.sample <- extract.complete.cases(main.events.use,delta.main.events.use,
                                        covariates.use,
                                        my.data.all)

  ######################
  ## define delta.use ##
  ######################
  delta.use <- paste("delta",events.use,sep=".")


  #################################
  ## create data frame to output ##
  #################################
  stat.events.table.names <- list(
    delta.hdage_nobase="\\% Motor-diagnosis occurs by age $t$",
    delta.mcione="\\% Cognitive Impairment occurs by age $t$",
    delta.behone="\\% Behavioral abnormality occurs by age $t$",
    delta.dep2="\\% Mild depression occurs by age $t$",
    delta.tfcone="\\% Total functional capacity is stage 1 by age $t$",
    delta.tfctwo="\\% Total functional capacity is stage 2 by age $t$",
    delta.tfcthree="\\% Total functional capacity is stage 3 by age $t$",
    delta.mcisd="\\Single-domain mild cognitive impairment occurs by age $t$",
    delta.mcisd_cat="\\Single-domain mild cognitive impairment occurs by age $t$",
    delta.mcimd="\\Multi-domain mild cognitive impairment occurs by age $t$",
    delta.mcimd_cat="\\Multi-domain mild cognitive impairment occurs by age $t$"
  )

  table.names.use <- stat.events.table.names
  delta.names <- unlist(table.names.use[delta.use])

  for(ss in 1:length(study.names)){
    data.out <- array(NA,dim=c(length(delta.names), length(time_val)),
                      dimnames=list(delta.names,
                                    paste("t=",time_val,sep="")))
    ## get study data
    index.use <- which(data.sample$study==study.names[ss])
    data.use <- data.sample[index.use,]

    ## get subset of data for which delta=1 for all events
    if(participants.experience.all.events==TRUE){
      data.use <- all.events.occur(data.use,delta.use)
    }

    ## get data of interest: events and delta.events
    data.use <- data.use[,c(events.use,delta.use)]

    ## other covariate information
    for(ii in 1:length(delta.names)){
      for(tt in 1:length(time_val)){
        data.out[delta.names[ii],tt] <-  round(mean(data.use[,events.use[ii]]<=time_val[tt])*100,
                                               digits=2)
      }
    }

    ##############################################
    cat("\n\n ## Study: ",study.names[ss]," ##\n\n")
    ##############################################
    myprint(data.out)
  }
}

## function to get subset of data for which delta=1 for all events
all.events.occur <- function(x,delta.use){
  ## get subset of data for which delta=1 for all events
  data.use.tmp <- x[,delta.use]
  check.total <- function(x,delta.use){
    return(sum(x)==length(delta.use))
  }
  data.use.tmp.check <- apply(data.use.tmp,1,check.total,delta.use)
  index.use.tmp <- which(data.use.tmp.check==TRUE)
  data.use <- x[index.use.tmp,]
  return(data.use)
}


########################################################
## function to display tables for Gamm-Clinical paper ##
########################################################



################################
## function to display tables ##
################################
#' @import xtable
get.my.tables <- function(all.data,baseage.cutoff=18,
                          cag.atrisk=36,cag.cutoff.hi=50,study="cohort"){

  ## For  MCI events: we allow it to happen at baseline.
  ##		If we DO NOT want baseline, need to add after mci.ages,
  ##		mci.ages <- paste(mci.ages,"nobase",sep="_")

  if(study=="cohort"){
    ## SDMT, Stroop Color, Stroop Word, Stroop Interference
    mci.ages <- c("sdmt","strcol","strwrd","strint")

    ## Apathy, irritability, depression
    beh.ages <- c("apt2","irb2","dep2")

    ## Motor symptom
    motor.symptom.ages<- NULL ##"symp"

    ## use.pharos: show genetic knowledge
    use.pharos <- FALSE
    show.gene.know <- FALSE

  } else if(study=="pharos"){
    ## SDMT, Stroop Color, Stroop Word, Stroop Interference
    mci.ages <- c("sdmt","strcol","strwrd","strint")

    ## Apathy, irritability, depression
    beh.ages <- c("apt2","irb2","dep2")

    ## Motor symptom
    motor.symptom.ages <- NULL

    ## use.pharos: show genetic knowledge
    use.pharos <- FALSE ##TRUE
    show.gene.know <- use.pharos

  } else if(study=="predict"){
    ## SDMT, Stroop Color, Stroop Word, Stroop Interference
    mci.ages <- c("sdmt","strcol","strwrd","strint")

    ## Apathy, irritability, depression
    beh.ages <- NULL

    ## Motor symptom
    motor.symptom.ages <- NULL

    ## use.pharos: show genetic knowledge
    use.pharos <- FALSE
    show.gene.know <- FALSE

  } else if(study=="track"){
    ## SDMT,  Stroop Word
    mci.ages <- c("sdmt","strwrd")

    ## Apathy, irritability, depression
    beh.ages <- c("apt2","irb2","dep2")

    ## Motor symptom
    motor.symptom.ages <- NULL

    ## use.pharos: show genetic knowledge
    use.pharos <- FALSE
    show.gene.know <- FALSE

  } else if(study=="enroll"){
    ## SDMT, Stroop Color, Stroop Word, Stroop Interference
    mci.ages <- c("sdmt","strcol","strwrd","strint")

    ## Apathy, irritability, depression
    beh.ages <- c("apt2","irb2","dep2")

    ## Motor symptom
    motor.symptom.ages <- NULL

    ## use.pharos: show genetic knowledge
    use.pharos <- FALSE
    show.gene.know <- FALSE

  }
  ## Motor onset
  motor.onset.ages <- c("hdage_base","hdage_nobase",
                        "hdage_new_nobase")

  ## mci no base
  if(!is.null(mci.ages)){
    mci.ages.nobase <- paste(mci.ages,"nobase",sep="_")
  } else {
    mci.ages.nobase <- NULL
  }

  ## behavioral no base
  if(!is.null(beh.ages)){
    beh.ages.nobase <- paste(beh.ages,"nobase",sep="_")
  } else {
    beh.ages.nobase <- NULL
  }


  ##############################################
  ## Extract data for which baseline age >=18 ##
  ##############################################
  if(study!="enroll"){
    data.out <- all.data[which(all.data[,"base_age"]>=baseage.cutoff),]
  } else {
    data.out <-
      all.data[which(as.numeric.factor(all.data[,"base_age"])>=baseage.cutoff),]
  }

  ###################
  ## get one event ##
  ###################

  ## mci
  if(!is.null(mci.ages)){
    one.mci <- get.one.event(data.out,onset.ages=mci.ages,name="mcione")
    data.out <- data.frame(data.out,one.mci)
    mci.ages <- c(mci.ages,"mcione")
  } else {
    one.mci <- NULL
  }

  ## mci no base
  if(!is.null(mci.ages.nobase)){
    one.mci.nobase <- get.one.event(data.out,onset.ages=mci.ages.nobase,
                                    name="mcione_nobase")
    data.out <- data.frame(data.out,one.mci.nobase)
    mci.ages.nobase <- c(mci.ages.nobase,"mcione_nobase")
  } else {
    one.mci.nobase <- NULL
  }

  ## behavioral
  if(!is.null(beh.ages)){
    one.beh <- get.one.event(data.out,onset.ages=beh.ages,name="behone")
    data.out <- data.frame(data.out,one.beh)
    beh.ages <- c(beh.ages,"behone")
  } else{
    one.beh <- NULL
  }

  ## behavioral no base
  if(!is.null(beh.ages.nobase)){
    one.beh.nobase <- get.one.event(data.out,onset.ages=beh.ages.nobase,
                                    name="behone_nobase")
    data.out <- data.frame(data.out,one.beh.nobase)
    beh.ages.nobase <- c(beh.ages.nobase,"behone_nobase")
  } else{
    one.beh <- NULL
  }


  ######################
  ## subjects at risk ##
  ######################
  at.risk <- which(data.out[,"CAG"]>=cag.atrisk)
  data.at.risk <- data.out[at.risk,]

  ##########################
  ## subjects not at risk ##
  ##########################
  data.not.at.risk <- data.out[-at.risk,]

  ###############
  ## Data sets ##
  ###############
  data.sets <- list(data.all=data.out,
                    data.at.risk=data.at.risk,
                    data.not.at.risk=data.not.at.risk)


  ##################################
  ## Summarize General covariates ##
  ##################################
  function.use <- "continuous"

  interest <- c("CAG")
  interest.names <- interest
  covariates.cag <- summarize.data.sets.covariates(data.sets,
                                                   covariates.interest=interest,
                                                   covariates.names=interest.names,
                                                   function.use=function.use,
                                                   table.name="CAG")

  interest <- c("base_age")
  interest.names <- interest
  covariates.baseage <-  summarize.data.sets.covariates(data.sets,
                                                        covariates.interest=interest,
                                                        covariates.names=interest.names,
                                                        function.use=function.use,
                                                        table.name="BL Age")

  function.use <- "discrete"
  interest <- "gender"
  interest.names <- "female"
  covariates.gender <-  summarize.data.sets.covariates(data.sets,
                                                       covariates.interest=interest,
                                                       covariates.names=interest.names,function.use=function.use,
                                                       table.name="\\% Females")


  interest <- c("educ_cat")
  interest.names <- interest
  covariates.educ <- summarize.data.sets.covariates(data.sets,
                                                    covariates.interest=interest,
                                                    covariates.names=interest.names,function.use=function.use,
                                                    table.name="\\% Higher Educ")

  ####################
  ## Report results ##
  ####################
  cat("\n\n##  CAG ##\n\n")
  foo <- covariates.cag
  myprint(foo)

  ## Age at baseline
  cat("\n\n##  AGE AT BL ##\n\n")
  foo <- covariates.baseage
  myprint(foo)


  ## Gender
  cat("\n\n##  GENDER ##\n\n")
  foo <- covariates.gender
  myprint(foo)

  ## Education
  cat("\n\n##  EDUCATION ##\n\n")
  foo <- covariates.educ
  myprint(foo)

  ###################
  ## motor symptom ##
  ###################
  if(!is.null(motor.symptom.ages)){
    onset.ages <- motor.symptom ## "symp"
    onset.names <- c("Age at first motor symptom")
    filler.names <- "none"

    function.use <- "symp.onset"
    onset.out <- NULL

    for(kk in 1:length(onset.ages)){

      onset.out.tmp <- summarize.data.sets.covariates(data.sets,
                                                      covariates.interest=NULL,
                                                      covariates.names=NULL,
                                                      onset.age.use=onset.ages[kk],
                                                      onset.names=onset.names[kk],
                                                      function.use=function.use,filler=filler.names[kk],
                                                      table.name="Motor Symptom")
      onset.out <- rbind(onset.out,onset.out.tmp)
    }

    cat("\n\n## Motor symptom times ##\n\n")
    foo <- onset.out
    myprint(foo)
  }


  ##################
  ## overall onset ##
  ###################
  all.events <- c(motor.symptom.ages,motor.onset.ages,
                  mci.ages,mci.ages.nobase,
                  beh.ages,beh.ages.nobase
  )
  names.all.events <- as.character(event.names.key[all.events])
  filler.names.events <- as.character(filler.names.key[all.events])
  if(!is.null(all.events)){
    onset.ages <- all.events
    onset.names <- names.all.events
    filler.names <- filler.names.events

    function.use <- "onset"
    onset.out <- NULL
    for(kk in 1:length(onset.ages)){

      onset.out.tmp <- summarize.data.sets.covariates(data.sets,
                                                      covariates.interest=NULL,
                                                      covariates.names=NULL,
                                                      onset.age.use=onset.ages[kk],
                                                      onset.names=onset.names[kk],
                                                      function.use=function.use,filler=filler.names[kk])
      onset.out <- rbind(onset.out,onset.out.tmp)
      ##cat("\n\n## ", onset.names[kk],"##\n\n")
      ##print(xtable(onset.out,digits=1))
    }

    cat("\n\n## Onset times ##\n\n")
    foo <- onset.out
    myprint(foo)
  }

  #################
  ## motor onset ##
  #################
  onset.ages <- c("hdage_nobase",
                  "hdage_new_nobase")

  onset.names <- c(
    "When DCL=4 post-BL",
    #"Age at first motor diagnosis (at BL)",
    "When DCL=3 or 4 post-BL")

  function.use <- "motor.onset"
  motor.onset.out <- NULL
  for(kk in 1:length(onset.ages)){

    motor.onset.out.tmp <- summarize.data.sets.covariates(data.sets,
                                                          covariates.interest=NULL,
                                                          covariates.names=NULL,
                                                          onset.age.use=onset.ages[kk],
                                                          onset.names=onset.names[kk],
                                                          function.use=function.use,
                                                          use.pharos=use.pharos,filler=onset.names[kk],
                                                          show.gene.know=show.gene.know)
    motor.onset.out <- rbind(motor.onset.out,motor.onset.out.tmp)

    ##cat("\n\n## ", onset.names[kk],"##\n\n")
    ##print(xtable(t(onset.out)))
  }

  cat("\n\n## Onset/Gene knowledge ##\n\n")
  foo <- motor.onset.out
  myprint(foo)


  ####################
  ## MCI covariates ##
  ####################
  onset.ages <- mci.ages
  if(!is.null(onset.ages)){
    onset.names <- as.character(mci.event.names.key[onset.ages])
    filler.names <- as.character(mci.filler.names.key[onset.ages])

    function.use <- "mci.onset"
    mci.onset.out <- NULL
    for(kk in 1:length(onset.ages)){

      mci.onset.out.tmp <- summarize.data.sets.covariates(data.sets,
                                                          covariates.interest=NULL,
                                                          covariates.names=NULL,
                                                          onset.age.use=onset.ages[kk],
                                                          onset.names=onset.names[kk],
                                                          function.use=function.use,
                                                          use.pharos=use.pharos, filler=filler.names[kk])
      mci.onset.out <- rbind(mci.onset.out,mci.onset.out.tmp)
    }

    cat("\n\n## MCI Onset ##\n\n")
    foo <- mci.onset.out
    myprint(foo)

    ## simultaneous experience?
    onset.ages <- onset.ages[-length(onset.ages)] ## remove mcione
    onset.names <-  as.character(mci.event.names.key[onset.ages])

    function.use <- "multi.onset"
    multi.out <- summarize.data.sets.covariates(data.sets,
                                                covariates.interest=NULL,
                                                covariates.names=NULL,
                                                onset.age.use=onset.ages,
                                                onset.names=onset.names,
                                                function.use=function.use,
                                                use.pharos=use.pharos)


    cat("\n\n##  Multi MCI onset##\n\n")
    print(xtable(multi.out,digits=1))
  }

  #############################
  ## MCI covariates: NO base ##
  #############################
  onset.ages <- mci.ages.nobase
  if(!is.null(onset.ages)){
    onset.names <- as.character(mci.event.names.key[onset.ages])
    filler.names <- as.character(mci.filler.names.key[onset.ages])

    function.use <- "mci.onset"
    mci.onset.out <- NULL
    for(kk in 1:length(onset.ages)){

      mci.onset.out.tmp <- summarize.data.sets.covariates(data.sets,
                                                          covariates.interest=NULL,
                                                          covariates.names=NULL,
                                                          onset.age.use=onset.ages[kk],
                                                          onset.names=onset.names[kk],
                                                          function.use=function.use,
                                                          use.pharos=use.pharos, filler=filler.names[kk])
      mci.onset.out <- rbind(mci.onset.out,mci.onset.out.tmp)
    }

    cat("\n\n## MCI Onset: NO Baseline ##\n\n")
    foo <- mci.onset.out
    myprint(foo)

    ## simultaneous experience?
    onset.ages <- onset.ages[-length(onset.ages)] ## remove mcione
    onset.names <-  as.character(mci.event.names.key[onset.ages])

    function.use <- "multi.onset"
    multi.out <- summarize.data.sets.covariates(data.sets,
                                                covariates.interest=NULL,
                                                covariates.names=NULL,
                                                onset.age.use=onset.ages,
                                                onset.names=onset.names,
                                                function.use=function.use,
                                                use.pharos=use.pharos)


    cat("\n\n##  Multi MCI onset: No baseline##\n\n")
    print(xtable(multi.out,digits=1))
  }




  ###########################
  ## Behavioral ages		 ##
  ###########################
  onset.ages <- beh.ages
  if(!is.null(onset.ages)){
    onset.names <- as.character(beh.event.names.key[onset.ages])
    filler.names <- as.character(beh.filler.names.key[onset.ages])

    function.use <- "beh.onset"
    beh.onset.out <- NULL

    for(kk in 1:length(onset.ages)){

      beh.onset.out.tmp <- summarize.data.sets.covariates(data.sets,
                                                          covariates.interest=NULL,
                                                          covariates.names=NULL,
                                                          onset.age.use=onset.ages[kk],
                                                          onset.names=onset.names[kk],
                                                          function.use=function.use,
                                                          use.pharos=use.pharos,filler=filler.names[kk])
      beh.onset.out <- rbind(beh.onset.out,beh.onset.out.tmp)
    }

    cat("\n\n## Beh onset ##\n\n")
    foo <- beh.onset.out
    myprint(foo)

    ## simultaneous experience?
    onset.ages <- onset.ages[-length(onset.ages)] ## remove mcione
    onset.names <-  as.character(beh.event.names.key[onset.ages])

    function.use <- "multi.onset"
    multi.out <- summarize.data.sets.covariates(data.sets,
                                                covariates.interest=NULL,
                                                covariates.names=NULL,
                                                onset.age.use=onset.ages,
                                                onset.names=onset.names,
                                                function.use=function.use,
                                                use.pharos=use.pharos)

    cat("\n\n## Multi BEH onset ##\n\n")
    print(xtable(multi.out,digits=1))
  }


  #######################################
  ## Behavioral ages: NO baseline		 ##
  #######################################
  onset.ages <- beh.ages.nobase
  if(!is.null(onset.ages)){
    onset.names <- as.character(beh.event.names.key[onset.ages])
    filler.names <- as.character(beh.filler.names.key[onset.ages])

    function.use <- "beh.onset"
    beh.onset.out <- NULL

    for(kk in 1:length(onset.ages)){

      beh.onset.out.tmp <- summarize.data.sets.covariates(data.sets,
                                                          covariates.interest=NULL,
                                                          covariates.names=NULL,
                                                          onset.age.use=onset.ages[kk],
                                                          onset.names=onset.names[kk],
                                                          function.use=function.use,
                                                          use.pharos=use.pharos,filler=filler.names[kk])
      beh.onset.out <- rbind(beh.onset.out,beh.onset.out.tmp)
    }

    cat("\n\n## Beh onset: NO baseline ##\n\n")
    foo <- beh.onset.out
    myprint(foo)

    ## simultaneous experience?
    onset.ages <- onset.ages[-length(onset.ages)] ## remove mcione
    onset.names <-  as.character(beh.event.names.key[onset.ages])

    function.use <- "multi.onset"
    multi.out <- summarize.data.sets.covariates(data.sets,
                                                covariates.interest=NULL,
                                                covariates.names=NULL,
                                                onset.age.use=onset.ages,
                                                onset.names=onset.names,
                                                function.use=function.use,
                                                use.pharos=use.pharos)

    cat("\n\n## Multi BEH onset: No baseline ##\n\n")
    print(xtable(multi.out,digits=1))
  }


  ##################
  ## for analysis ##
  ##################

  cat("\n\n## DATA FOR ANALYSIS ## \n\n")

  data.for.use <- data.at.risk

  cat("\n\n## check CAG repeats ## \n\n")
  print(table(data.for.use$CAG))

  ## only use data for CAG < cutoff
  index.cag <- which(data.for.use$CAG <=cag.cutoff.hi)
  data.for.use <- data.for.use[index.cag,]

  ## cag limits
  cag.limit <- c(min(data.for.use$CAG),max(data.for.use$CAG))
  print(cag.limit)

  ## standardize cag variable to be in [0,1]
  ##data.for.use$CAG <- (data.for.use$CAG-min(data.for.use$CAG))/(max(data.for.use$CAG)-min(data.for.use$CAG))
  data.for.use$CAG <- reverse.cag(data.for.use$CAG,xmin=36,xmax=cag.cutoff.hi)

  cat("\n\n## unique CAG repeats ## \n\n")
  print(sort(unique(data.for.use$CAG)))

  print(table(data.for.use$CAG))

  #######################
  ## write data output ##
  #######################
  write.table(data.for.use,paste("clean_",study,".dat",sep=""),row.names=FALSE,col.names=TRUE)
  write.table(cag.limit,paste(study,"_cag_limits.dat",sep=""),row.names=FALSE,col.names=FALSE)

  return(data.sets)

}



#####################################
## function for tables: stat paper ##
#####################################
#' @import xtable
get.my.tables.stat.old <- function(all.data,cutoff=50,data.name="cohort"){

  #use.pharos <- TRUE
  use.pharos <- FALSE
  use.predict <- FALSE

  #if(data.name=="pharos"){
  #  use.pharos <- TRUE
  #}

  if(data.name=="predict"){
    use.predict <- TRUE
  }

  ########################################
  ## convert uniform CAG to regular CAG ##
  ########################################
  all.data$CAG <- convert.cag(all.data$CAG,xmin=36,xmax=cutoff)

  ######################
  ## subjects at risk ##
  ######################
  at.risk <- which(all.data$CAG>=36)
  data.at.risk <- all.data[at.risk,]

  ##########################
  ## subjects not at risk ##
  ##########################
  data.not.at.risk <- all.data[-at.risk,]

  ###############
  ## Data sets ##
  ###############
  data.sets<- list(data.all=all.data,
                   data.at.risk=data.at.risk,
                   data.not.at.risk=data.not.at.risk)

  ########################
  ## General covariates ##
  ########################
  function.use <- "continuous"

  interest <- c("CAG")
  interest.names <- interest
  covariates.cag <- summarize.data.sets.covariates(data.sets,
                                                   covariates.interest=interest,
                                                   covariates.names=interest.names,function.use=function.use,
                                                   table.name="CAG")

  interest <- c("base_age")
  interest.names <- interest
  covariates.baseage <-  summarize.data.sets.covariates(data.sets,
                                                        covariates.interest=interest,
                                                        covariates.names=interest.names,function.use=function.use,
                                                        table.name="BL Age")

  function.use <- "discrete"
  interest <- "gender"
  interest.names <- "female"
  covariates.gender <-  summarize.data.sets.covariates(data.sets,
                                                       covariates.interest=interest,
                                                       covariates.names=interest.names,function.use=function.use,
                                                       table.name="\\% Females")


  interest <- c("educ_cat")
  interest.names <- interest
  covariates.educ <- summarize.data.sets.covariates(data.sets,
                                                    covariates.interest=interest,
                                                    covariates.names=interest.names,function.use=function.use,
                                                    table.name="\\% Higher Educ")

  cat("\n\n##  CAG ##\n\n")
  foo <- covariates.cag
  print(xtable(data.frame(row = rownames(foo),data.frame(foo))),
        sanitize.text.function=function(x){x},
        include.rownames = FALSE)

  ## Age at baseline
  cat("\n\n##  AGE AT BL ##\n\n")
  foo <- covariates.baseage
  print(xtable(data.frame(row = rownames(foo),data.frame(foo))),
        sanitize.text.function=function(x){x},
        include.rownames = FALSE)


  ## Gender
  cat("\n\n##  GENDER ##\n\n")
  foo <- covariates.gender
  print(xtable(data.frame(row = rownames(foo),data.frame(foo))),
        sanitize.text.function=function(x){x},
        include.rownames = FALSE)

  ## Education
  cat("\n\n##  EDUCATION ##\n\n")
  foo <- covariates.educ
  print(xtable(data.frame(row = rownames(foo),data.frame(foo))),
        sanitize.text.function=function(x){x},
        include.rownames = FALSE)

  #################
  ## motor onset ##
  #################
  onset.ages <- c("hdage_nobase")

  onset.names <- c("When DCL=4 post-BL")

  function.use <- "motor.onset"
  motor.onset.out <- NULL
  for(kk in 1:length(onset.ages)){
    motor.onset.out.tmp <- summarize.data.sets.covariates(data.sets,
                                                          covariates.interest=NULL,
                                                          covariates.names=NULL,
                                                          onset.age.use=onset.ages[kk],
                                                          onset.names=onset.names[kk],
                                                          function.use=function.use,
                                                          use.pharos=use.pharos,filler=onset.names[kk],
                                                          show.gene.know=FALSE)
    motor.onset.out <- rbind(motor.onset.out,motor.onset.out.tmp)

    ##cat("\n\n## ", onset.names[kk],"##\n\n")
    ##print(xtable(t(onset.out)))
  }

  cat("\n\n## Onset/Gene knowledge ##\n\n")
  foo <- motor.onset.out
  print(xtable(data.frame(row = rownames(foo),data.frame(foo))),
        sanitize.text.function=function(x){x},
        include.rownames = FALSE)

  ####################
  ## MCI covariates ##
  ####################
  onset.ages <- c("sdmt")

  onset.names <- c("SDMT score")

  filler.names <- c("When MCI occurs based on SDMT")

  function.use <- "mci.onset"
  mci.onset.out <- NULL
  for(kk in 1:length(onset.ages)){
    mci.onset.out.tmp <- summarize.data.sets.covariates(data.sets,
                                                        covariates.interest=NULL,
                                                        covariates.names=NULL,
                                                        onset.age.use=onset.ages[kk],
                                                        onset.names=onset.names[kk],
                                                        function.use=function.use,
                                                        use.pharos=use.pharos, filler=filler.names[kk],
                                                        show.gene.know=FALSE)
    mci.onset.out <- rbind(mci.onset.out,mci.onset.out.tmp)
    ##cat("\n\n## ", onset.names[kk],"##\n\n")
    ##print(xtable(t(onset.out)))
  }

  cat("\n\n## MCI Onset ##\n\n")
  foo <- mci.onset.out
  print(xtable(data.frame(row = rownames(foo),data.frame(foo))),
        sanitize.text.function=function(x){x},
        include.rownames = FALSE)
}





#####################################################
## new function to summarize tables for stat paper ##
#####################################################
get.censoring <- function(x,digits=2){
  tmp.out <- c(mean(x,na.rm=TRUE),mean(1-x,na.rm=TRUE))*100
  tmp.out <- round(tmp.out,digits)
  out <- paste(tmp.out[1]," (",tmp.out[2],")",sep="")
  return(out)
}





get.my.tables.bookchapter <- function(all.data.input,zz.use,s.use.names,delta.names,cag.atrisk,use.at.risk=TRUE){
  ########################
  ## get the data used  ##
  ########################
  all.data <- all.data.input[,c(zz.use,s.use.names,delta.names)]

  ## get complete cases
  #all.data <- all.data[complete.cases(all.data),]

  ####################################
  ## Get individuals at risk or not ##
  ####################################
  index.use <- which(all.data[,"CAG"]>=cag.atrisk)
  if(use.at.risk==TRUE){
    all.data <- all.data[index.use,]
  } else {
    all.data <- all.data[-index.use,]
  }

  #################
  ## sample size ##
  #################
  sample.size <- nrow(all.data)
  names(sample.size) <- "Sample size"

  #################
  ## event rates ##
  #################
  censoring.out <- apply(as.matrix(all.data[,delta.names]),2,get.total.percentage)
  names(censoring.out) <- s.use.names


  ####################
  ## covariate info ##
  ####################

  info.covariates.to.report <- function(name){
    if(name=="base_age"){
      out <- get.mean.sd(as.numeric(all.data[,name]),digits=1)
      names(out) <- "Average age at baseline (SD)"
    } else if(name=="CAG"){
      out <- get.mean.sd(all.data[,name],digits=1)
      names(out) <- "Average CAG-repeat length (SD)"
    } else if(name=="gender"){
      out <- get.total.percentage(all.data[,name],digits=1)
      names(out) <- c("Female")
    } else if(name=="educ_cat"){
      out <- get.total.percentage(all.data[,name],digits=1)
      names(out) <- " \\% Subjects with $\\geq$ 15 years of education"
    } else if(grepl("gene.*",name)){
      out <- round(mean(all.data[,name])*100,digits=1)
      names(out) <- " \\% Subjects with gene knowledge known"
    } else if(name=="race_cat"){
      race.tmp <- as.matrix(table(all.data[,name]))
      out <- get.total.percentage2(race.tmp,digits=1)
      names(out) <- rownames(race.tmp)
    }
    return(out)
  }

  aout <- NULL
  cov.use <- zz.use
  for(ii in 1:length(cov.use)){
    aout <- c(aout,info.covariates.to.report(cov.use[ii]))
  }

  data.out <- data.matrix(c(sample.size,censoring.out,aout))

  return(data.out)
}




############################################################
## Coding to check duplicates between studies in the data ##
############################################################
#' @import data.table
count.dups <- function(DF){

  DT <- data.table::data.table(DF)
  DT[,.N, by = names(DT)]
}



## overlap.variables.use : variables we use to check for duplicates between studies
## data.complete.cases: all data sets concatenated together
check.for.duplicates <- function(overlap.variables.use,
                                 data.complete.cases,
                                 my.data.all,
                                 study.names){

  ## update data.complete.cases to use complete information for overlap.variables.use
  new.data.complete.cases <- data.complete.cases[complete.cases(data.complete.cases[,overlap.variables.use]),]

  ## find duplicates in the data
  ## duplicate number can be 1, 2, 3,...
  ## When duplicate=1, it just means the value only observed once, so this is not a duplicate in our definition.
  duplicate.check <- count.dups(new.data.complete.cases[,overlap.variables.use])

  ## duplicates are those indices where the number of duplicates ("N") is 2 or more
  index.multiple.duplicates <- which(duplicate.check[,"N"]>1)

  ## Identify indivuduals for which 2 or more people share the same overlap.variables.use Values
  duplicate.results <- duplicate.check[index.multiple.duplicates,]

  #####################################################
  ## Summarize results in terms of overlap per study ##
  #####################################################
  ## create study combinations
  if(length(study.names)>1){
    study.combinations <- get.empty.list(paste0("numstudy",2:length(study.names)))
    for(rr in 2:length(study.names)){
      study.combinations[[rr-1]] <- combn(study.names,rr)
    }
  }

  ## paste names in alphabetical order
  paste.names.alphabeticaly <- function(x){
    return(paste(sort(x),collapse="-"))
  }

  ## make a list of where to store duplicates in the studies
  study.intersection.names <- NULL
  for(rr in 1:length(study.combinations)){
    study.intersection.names <- c(study.intersection.names,
                                  apply(study.combinations[[rr]],2,
                                        paste.names.alphabeticaly
                                  ))
  }

  study.duplicates <- get.empty.list(study.intersection.names)

  #############################
  ## get overlap information ##
  #############################
  ## We store in study.duplicates the information based on overlap.variables.use
  ##  plus the study IDs and subject IDs.
  ## This helps us to see if the information is truly overlapped or not.
  ## Also, study.duplicates is a list that also contains information for duplicates
  ## 	found within each study. But looking at that data, we will see that these people are
  ##  actually unique by their IDs.
  for(ii in 1:nrow(duplicate.results)){
    index.use <- which(my.data.all[,"birthyr"]==as.numeric(duplicate.results[ii,"birthyr"]) &
                         my.data.all[,"site"]==as.numeric(duplicate.results[ii,"site"]) &
                         my.data.all[,"gender"]==as.numeric(duplicate.results[ii,"gender"]) &
                         my.data.all[,"CAG"]==as.numeric(duplicate.results[ii,"CAG"]))

    study.overlap <- my.data.all[index.use,c("id",overlap.variables.use,"study")]
    study.names.overlap <- unique(study.overlap[,"study"])

    ## find studies of intersection
    where.study.overlap.occurs <- paste.names.alphabeticaly(as.character(study.names.overlap))
    study.duplicates[[where.study.overlap.occurs]] <-
      rbind(study.duplicates[[where.study.overlap.occurs]],
            data.frame(study.overlap,match=ii))

  }

  ######################################################################
  ## It can happen that 2 or more people in Study A match a person in Study B.
  ## However, this still means that there is one person that is duplicated in Study A and B,
  ##  and one person who is unique.
  ## We will randomly select the unique person so we are appropriately counting people.
  ############################################################################

  for(rr in 1:length(study.duplicates)){
    if(names(study.duplicates)[rr] %in% study.intersection.names){
      ## this indicates there are duplicates between studies
      find.matches <- unique(study.duplicates[[rr]][,"match"])

      if(length(find.matches)>0){
        for(kk in 1:length(find.matches)){
          index.match <- which(study.duplicates[[rr]][,"match"]==find.matches[kk])
          study.duplicates.tmp <- study.duplicates[[rr]][index.match,]

          study.table.info <- table(study.duplicates.tmp[,"study"])
          study.with.multiple.duplicates <- names(study.table.info)[which(study.table.info>1)]

          if(length(study.with.multiple.duplicates)>0){

            for(pp in 1:length(study.with.multiple.duplicates)){
              index.to.move <- which(study.duplicates.tmp[,"study"]==study.with.multiple.duplicates[pp])
              tmp <- study.duplicates.tmp[index.to.move,]
              random.index.to.move <- sample(1:nrow(tmp),nrow(tmp)-1)


              ## add duplicate to other study
              study.duplicates[[study.with.multiple.duplicates[pp]]] <-
                rbind(study.duplicates[[study.with.multiple.duplicates[pp]]],
                      tmp[random.index.to.move,])

              ## remove multiple duplicates
              index.in.study.duplicates <- which(rownames(study.duplicates[[rr]])%in% rownames(study.duplicates.tmp)[index.to.move])
              study.duplicates[[rr]] <- study.duplicates[[rr]][-index.in.study.duplicates,]


              ## add back in duplicates that were NOT removed
              study.duplicates[[rr]] <-
                rbind(study.duplicates[[rr]],
                      tmp[-random.index.to.move,])
              study.duplicates[[rr]] <- study.duplicates[[rr]][order(study.duplicates[[rr]][,"match"]),]
            }
          }
        }
      }
    }

  }


  ######################################################
  ## create summary statistics on overlap information ##
  ######################################################

  ## create table to summarize number of duplicates between studies
  overlap.summary.table <- rep(0,length(study.duplicates))
  names(overlap.summary.table) <- names(study.duplicates)

  ## check how many unique ids are there in the duplicates
  unique.ids.overlap.summary.table <- overlap.summary.table

  ## total number of people in study combinations
  study.combination.totals <- overlap.summary.table

  ## Sample sizes in the studies
  total.number.in.studies <-  table(data.complete.cases[,"study"])

  ## remove duplicates between studies from the data
  my.data.all.without.duplicates <- my.data.all


  for(rr in 1:length(overlap.summary.table)){
    overlap.summary.table[rr] <- length(unique(study.duplicates[[rr]][,"match"]))

    unique.ids.overlap.summary.table[rr] <- length(study.duplicates[[rr]][,"id"])-
      length(unique(study.duplicates[[rr]][,"id"]))
    ## count number of people in the study
    study.names.tmp <- unlist(strsplit(names(overlap.summary.table)[rr],"-"))
    study.combination.totals[rr] <- sum(total.number.in.studies[study.names.tmp])


    #######################################
    ## remove duplicates between studies ##
    #######################################
    if(names(study.duplicates)[rr] %in% study.intersection.names){
      ## this indicates there are duplicates between studies
      find.matches <- unique(study.duplicates[[rr]][,"match"])

      if(length(find.matches)>0){
        for(uu in 1:length(find.matches)){
          index.match <- which(study.duplicates[[rr]][,"match"]==find.matches[uu])

          study.duplicates.tmp <- study.duplicates[[rr]][index.match,]


          ## randomly choose which individual to remove
          random.id<- sample(1:nrow(study.duplicates.tmp),size=(nrow(study.duplicates.tmp)-1))

          which.ids.to.remove <- which(rownames(my.data.all.without.duplicates)%in% rownames(study.duplicates.tmp)[random.id])


          my.data.all.without.duplicates <- my.data.all.without.duplicates[-which.ids.to.remove,]
        }
      }
    }

  }


  return(list(overlap.summary.table=overlap.summary.table,
              unique.ids.overlap.summary.table=unique.ids.overlap.summary.table,
              study.combination.totals=study.combination.totals,
              my.data.all.without.duplicates=my.data.all.without.duplicates))
}


#########################################
## Function to call organize.data.sets ##
#########################################
## This functions writes the data to a file.
call.organize.data.sets <- function(location,study.names,use.normative.data){
  for(ss in 1:length(study.names)){
    study <- study.names[ss]
    organize.data.sets(location,study,use.normative.data)
  }
}


#################################################
## Function to call organize.mci.behavior.data ##
#################################################
## This functions writes the data to a file.
call.organize.mci.behavior.data <- function(location,study.names,
                                            type=c("cognitive","behavior")[1]){
  for(ss in 1:length(study.names)){
    study <- study.names[ss]
    organize.mci.behavior.data(location,study,type)
  }
}

##############################################
## Function to read and merge all data sets ##
##############################################
read.and.merge.data.sets <- function(study.names,
                                     baseage.cutoff,
                                     cag.at.risk.cutoff,
                                     cag.not.at.risk.cutoff,
                                     location,
                                     data.to.use=c("at_risk","not_at_risk","all")[1],
                                     print.clean.data,
                                     read.filename){

  ## where to store the data
  my.data <- get.empty.list(study.names)

  ## put all data into a list
  for(ss in 1:length(study.names)){
    study <- study.names[ss]
    all.data <- read.csv(paste(location,study,read.filename,sep=""),header=TRUE)
    if(study=="predict"){
      ## fix id name
      colnames(all.data)[1] <- "id"
    }
    my.data[[study]] <- data.for.analysis(all.data,baseage.cutoff,
                                          cag.at.risk.cutoff,cag.not.at.risk.cutoff,
                                          study,data.to.use,print.clean.data)
  }
  return(my.data)
}

###############################################################################
## Function to organize and summarize MCI/Behavior data for Control subjects ##
###############################################################################
## function to get cutoffs for mci/behavior data for all studies
get.cutoffs.mci.behavior <- function(data,cag.cutoff=36,
                                     interest=c("SDMT","STROOP_COLOR",
                                                "STROOP_WORD","STROOP_INTERFERENCE"),
                                     group.categories=NULL){

  ## get controls
  data.new <- data[which(data$CAG<=cag.cutoff),]

  if(!is.null(group.categories)){

    ###########################################################################
    ## Categories are determined by different Age-ranges and education level ##
    ###########################################################################
    mci.data <- NULL
    for(rr in 1:nrow(group.categories)){
      if(!is.na(group.categories[rr,"education"])){
        data.tmp.index <- which(data.new[,"base_age"] >= group.categories[rr,"age_low"] &
                                  data.new[,"base_age"] <= group.categories[rr,"age_high"] &
                                  data.new[,"educ_cat"]==group.categories[rr,"education"])
      } else {
        data.tmp.index <- which(data.new[,"base_age"] >= group.categories[rr,"age_low"] &
                                  data.new[,"base_age"] <= group.categories[rr,"age_high"])
      }
      data.tmp <- data.new[data.tmp.index,]

      mean.measures <- apply(data.tmp[,interest],2,mean,na.rm=TRUE)
      sd.measures <- apply(data.tmp[,interest],2,sd,na.rm=TRUE)
      sample.size <- length(data.tmp.index)

      mci.data.tmp <- cbind(mean.measures,sd.measures)
      rownames(mci.data.tmp) <- NULL

      colnames(mci.data.tmp) <- c("mean","sd")
      mci.data.tmp <- data.frame(COGTEST=interest,
                                 sample_size=sample.size,
                                 age_low=group.categories[rr,"age_low"],
                                 age_high=group.categories[rr,"age_high"],
                                 education=group.categories[rr,"education"],
                                 mci.data.tmp,row.names=NULL)


      mci.data <- rbind(mci.data,mci.data.tmp)
    }
    rownames(mci.data) <- 1:nrow(mci.data)
  } else {
    ####################################################
    ## All participants are used to determine cutoffs ##
    ####################################################
    data.tmp <- data.new
    mean.measures <- apply(data.tmp[,interest],2,mean,na.rm=TRUE)
    sd.measures <- apply(data.tmp[,interest],2,sd,na.rm=TRUE)

    mci.data.tmp <- cbind(mean.measures,sd.measures)
    colnames(mci.data.tmp) <- c("mean","sd")
    mci.data <- mci.data.tmp
  }


  return(mci.data)
}


#' @import xtable
call.create.mci.behavior.cutoffs.and.summaries <- function(location,
                                                           study.names,type,
                                                           baseage.cutoff,
                                                           cag.at.risk.cutoff,
                                                           cag.not.at.risk.cutoff,
                                                           data.to.use,
                                                           print.clean.data,
                                                           covariates.use,
                                                           variables.for.cutoff){

  ######################################
  ## Print MCI/Behavior data to files ##
  ######################################
  call.organize.mci.behavior.data(location,study.names,type)

  if(type=="cognitive"){
    read.filename <- "_cog_data.csv"
  } else {
    read.filename <- "_beh_data.csv"
  }

  ######################################################
  ## Read-in data sets and subset to data of interest ##
  ######################################################
  my.data <- read.and.merge.data.sets(study.names,
                                      baseage.cutoff,
                                      cag.at.risk.cutoff,
                                      cag.not.at.risk.cutoff,
                                      location,
                                      data.to.use,
                                      print.clean.data,
                                      read.filename)

  #################################
  ## Adjust variables for cutoff ##
  #################################
  if(is.null(variables.for.cutoff)){
    variables.for.cutoff <- colnames(my.data[[1]])
    index.to.remove <- which(variables.for.cutoff=="id")
    variables.for.cutoff <- variables.for.cutoff[-index.to.remove]
    variables.for.cutoff <- setdiff(variables.for.cutoff,covariates.use)
  }

  ########################################
  cat("\n\n Summary Table of characteristics:\n")
  #########################################
  out <- nonconverters.table(study.names,
                             covariates.use,my.data,results.to.report="mean-sd")
  myprint(out)


  ###############################################
  ## Make MCI/BEHAVIOR cutoff tables ##
  ###############################################

  all.data <- merge.all.data(study.names,my.data)
  all.data <- all.data[complete.cases(all.data[,
                                               c(covariates.use)]),]




  if(type=="cognitive"){
    ## We further divide data by age ranges and education level.
    ## We create all combinations between age-ranges and education levels.
    age.low <- c(18,30,40,50,60,70,80,90)
    age.hi  <- c(29,39,49,59,69,79,89,1e10)
    education <- rep(c(0,1),length(age.low))
    group.categories <- cbind(cbind(rep(age.low,each=2),rep(age.hi,each=2)),education)
    colnames(group.categories) <- c("age_low","age_high","education")
    ## add all data as an option
    group.categories <- rbind(c(18,1e10,NA),group.categories)
    filename <- c("data/mci_data.csv")
  }  else {
    group.categories <- NULL
    filename <- c("data/beh_data.csv")
  }

  cutoff.data <- get.cutoffs.mci.behavior(all.data,
                                          cag.cutoff=cag.not.at.risk.cutoff,
                                          interest=variables.for.cutoff,
                                          group.categories)

  ####################
  ## Check cut-offs ##
  ####################
  new.cutoff <- NULL
  for(ii in 1:nrow(cutoff.data)){
    if(type=="cognitive") {
      new.cutoff <- c(new.cutoff,cutoff.data[ii,"mean"]-1.5 * cutoff.data[ii,"sd"])
    } else {
      new.cutoff <- c(new.cutoff,cutoff.data[ii,"mean"]+1.5 * cutoff.data[ii,"sd"])
    }
  }

  ## we call it "mci.cutoff" in organize.data.sets()
  ##   so we need to keep this name.
  cutoff.data <- cbind(cutoff.data,mci.cutoff=new.cutoff)

  #######################
  ## write data output ##
  #######################
  write.csv(cutoff.data,filename,row.names=TRUE)


  print(xtable(cutoff.data))
}


##############################################
## Parameters for analysis: used everywhere ##
##############################################
default.options.for.analysis<- function(){
  location <- "data/"
  study.names <- c("pharos","predict","cohort")
  baseage.cutoff <- 18
  cag.at.risk.cutoff <- c(36,50)
  cag.not.at.risk.cutoff <- 30
  covariates.use <- c("CAG","base_age","gender","educ_cat")
  return(list(location=location,
              study.names=study.names,
              baseage.cutoff=baseage.cutoff,
              cag.at.risk.cutoff=cag.at.risk.cutoff,
              cag.not.at.risk.cutoff=cag.not.at.risk.cutoff,
              covariates.use=covariates.use))
}


## default events analyzed
default.event.options <- function(paper.type){
  if(paper.type=="clinical"){
    mci.specific.events <- c("sdmt","strcol","strwrd","strint")
    mci.ages <- c("mcimd")
    tfc.ages <- c("tfctwo") # c("tfcone","tfcthree")
    #beh.ages <- c("dep2")#,"behone","dep2","apt2","irb2")
    beh.ages <- NULL
    motor.onset.ages <- c("hdage_nobase")
  } else if(paper.type=="statistics"){
    mci.specific.events <- NULL
    mci.ages <- "mcione"
    tfc.ages <- NULL
    beh.ages <- NULL
    motor.onset.ages <- "hdage_nobase"
  }

  ## table.events.use: events reported in the clinical table
  table.events.use <- c(motor.onset.ages,beh.ages,tfc.ages,
                        mci.ages,mci.specific.events)

  ## events analyzed
  events.use <- c(motor.onset.ages,beh.ages,tfc.ages,
                  mci.ages)

  ## main.events.used: events used to set complete cases
  main.events.use <- events.use

  events.use.control <- c(mci.ages,paste0(mci.ages,"_nobase"))

  ## variables used to find duplicates between studies
  overlap.variables.use <- c("birthyr","site","gender","CAG")

  ## time-variables of interest
  time_val <- seq(40,60,by=5)

  return(list(mci.specific.events=mci.specific.events,
              mci.ages=mci.ages,
              tfc.ages=tfc.ages,
              beh.ages=beh.ages,
              motor.onset.ages=motor.onset.ages,
              table.events.use=table.events.use,
              events.use=events.use,
              main.events.use=main.events.use,
              events.use.control=events.use.control,
              overlap.variables.use=overlap.variables.use,
              time_val=time_val))
}

######################################################################
## Function to restrict data sets created to variables of interest	##
## and report summary measures 										##
######################################################################

call.clean.data.create.tables <- function(
  location,
  study.names,
  baseage.cutoff,
  cag.at.risk.cutoff,
  cag.not.at.risk.cutoff,
  covariates.use,
  remove.duplicates,
  paper.type,
  results.to.report,
  read.filename
){

  ############################
  ## Set events to analyzes ##
  ############################

  default.event.variables <- default.event.options(paper.type)
  mci.specific.events <- default.event.variables$mci.specific.events
  mci.ages <- default.event.variables$mci.ages
  tfc.ages <- default.event.variables$tfc.ages
  beh.ages <- default.event.variables$beh.ages
  motor.onset.ages <- default.event.variables$motor.onset.ages
  table.events.use <- default.event.variables$table.events.use
  events.use <- default.event.variables$events.use
  delta.use <- paste("delta",events.use,sep=".")
  main.events.use <- default.event.variables$main.events.use
  delta.main.events.use <- paste0("delta.",main.events.use)
  events.use.control <- default.event.variables$events.use.control
  overlap.variables.use <- default.event.variables$overlap.variables.use
  time_val <- default.event.variables$time_val

  ##########################
  ## Not at-risk subjects ##
  ##########################
  data.to.use <- "not_at_risk"
  print.clean.data <- FALSE
  my.data.controls <- read.and.merge.data.sets(study.names,
                                               baseage.cutoff,
                                               cag.at.risk.cutoff,
                                               cag.not.at.risk.cutoff,
                                               location,
                                               data.to.use,
                                               print.clean.data,
                                               read.filename)

  if(paper.type=="clinical"){
    ########################################
    cat("\n\n Controls experience MCI at baseline or not:\n")
    #########################################

    out <- clinical.tables(study.names,covariates.use,
                           events.use=events.use.control,
                           my.data.controls,
                           p.adjust.method="none",paper.type=paper.type,
                           results.to.report=results.to.report,
                           main.events.use=events.use.control)
    foo <- out
    myprint(foo)
  }


  ######################################################
  ## RESULTS FOR At-risk subjects 					##
  ######################################################
  data.to.use <- "at_risk"
  print.clean.data <- TRUE
  my.data <- read.and.merge.data.sets(study.names,
                                      baseage.cutoff,
                                      cag.at.risk.cutoff,
                                      cag.not.at.risk.cutoff,
                                      location,
                                      data.to.use,
                                      print.clean.data,
                                      read.filename)

  #################################################################
  ## For our analysis, we use the complete cases
  ## based on the events (main.events.use),
  ## censoring indicator (delta.main.events.use),
  ## and covariates.
  #################################################################
  ## put my.data in one big data frame, with study indicator
  my.data.all <- merge.all.data(study.names,my.data)

  ## Extract complete cases based on:
  ## main.events.use,delta.main.events.use,and covariates
  data.complete.cases <- extract.complete.cases(main.events.use,
                                                delta.main.events.use,
                                                covariates.use,
                                                my.data.all)

  #########################################################
  ## Find study overlap and remove duplicates 		   ##
  #########################################################

  duplicate.out <- check.for.duplicates(overlap.variables.use,
                                        data.complete.cases,
                                        my.data.all,
                                        study.names)
  ##############################################
  cat("\n\n Number of Duplicates:\n\n")
  ##############################################
  print(duplicate.out$overlap.summary.table)

  ##############################################
  cat("\n\n Percentages of Duplicates:\n\n")
  ##############################################
  print(round(duplicate.out$overlap.summary.table/duplicate.out$study.combination.totals * 100,2))

  if(remove.duplicates==TRUE){
    my.data.all <- duplicate.out$my.data.all.without.duplicates

    ## update files
    for(ss in 1:length(study.names)){
      index.study <- which(my.data.all[,"study"]== study.names[ss])
      index.study.column <- which(colnames(my.data.all)=="study")
      write.table(my.data.all[index.study,-index.study.column],paste("clean_",study.names[ss],".dat",sep=""),row.names=FALSE,col.names=TRUE)

      ## update my.data
      my.data[[study.names[ss]]] <- my.data.all[index.study,-index.study.column]
    }

    ## update complete cases
    data.complete.cases <- extract.complete.cases(main.events.use,delta.main.events.use,
                                                  covariates.use,
                                                  my.data.all)
  }

  ########################################
  cat("\n\n At-Risk Results :\n")
  #########################################

  ########################################
  cat("\n\n No p-value adjustment:\n")
  #########################################
  out <- clinical.tables(study.names,covariates.use,
                         events.use=table.events.use,my.data,
                         p.adjust.method="none",paper.type=paper.type,
                         results.to.report=results.to.report,
                         main.events.use)
  foo <- out
  myprint(foo)

  if(paper.type=="clinical"){
    ########################################
    cat("\n\n Bonferroni correction:\n")
    ########################################
    out <- clinical.tables(study.names,covariates.use,
                           events.use=table.events.use,my.data,
                           p.adjust.method="bonferroni",
                           paper.type=paper.type,
                           results.to.report=results.to.report,
                           main.events.use)
    foo <- out
    myprint(foo)


    ########################################
    cat("\n\n Benjamini-Hochberg correction:\n")
    ########################################
    out <- clinical.tables(study.names,covariates.use,
                           events.use=table.events.use,my.data,
                           p.adjust.method="BH",
                           paper.type=paper.type,
                           results.to.report=results.to.report,
                           main.events.use)
    foo <- out
    myprint(foo)
  }

  ## 5-21-2018: This is not the table the reviewer is asking for
  ## clinical paper.
  ########################################
  cat("\n\n Summary Table of Events occurrances by different times t:\n")
  #########################################
  out.events <- compare.events.tables(study.names,
                                      covariates.use,events.use=events.use,my.data,time_val,
                                      main.events.use,
                                      participants.experience.all.events=TRUE)



  if(paper.type=="clinical"){
    ########################################
    cat("\n\n Clinical Paper Results: 2x2 Table of Events occurrances by different times t:\n")
    #########################################
    participants.experience.all.events <- FALSE

    two.by.two.info <- two.by.two.events.tables(study.names,
                                                covariates.use,
                                                events.use,
                                                delta.use,
                                                my.data,
                                                time_val,
                                                main.events.use,
                                                participants.experience.all.events)


    ## create porportions from output of two.by.two.events.tables function
    proportions.two.by.two <- two.by.two.tables.proportions(two.by.two.info,
                                                            events.use,time_val)

    ## test for independence
    pvalues.independence.two.by.two <- two.by.two.test.independence(two.by.two.info,
                                                                    events.use,time_val, study.names)

    ## adjust p-values

    ########################################
    cat("\n\n No p-value adjustment:\n")
    #########################################
    no.adjust.pvalues.independence.two.by.two <-
      two.by.two.adjust.pvalues(pvalues.independence.two.by.two,
                                time_val,events.use,p.adjust.methods="none")


    ########################################
    cat("\n\n Bonferroni correction:\n")
    ########################################
    no.adjust.pvalues.independence.two.by.two <-
      two.by.two.adjust.pvalues(pvalues.independence.two.by.two,
                                time_val,events.use,p.adjust.methods="bonferroni")

    ########################################
    cat("\n\n Benjamini-Hochberg correction:\n")
    ########################################
    no.adjust.pvalues.independence.two.by.two <-
      two.by.two.adjust.pvalues(pvalues.independence.two.by.two,
                                time_val,events.use,p.adjust.methods="BH")

  }


}
#########################################################################
## Functions that are used for each data set. Call functions in main.R ##
#########################################################################

## function to check existence of a variable
check.existence <- function(var.name){
  return(exists(as.character(substitute(var.name))))
}





#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+
## Function that lists variables used
#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+ #+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+

get.all.variable.names <- function(study){

  ## if psych.name does not exist
  if(!exists(as.character(substitute(psych.name)))){
    psych.name <- NULL
  }

  if(study=="cohort"){
    ########
    ## id ##
    ########
    id.name <- "id"

    ######################
    ## site information ##
    ######################
    site.variable <- "cno"

    ###########################
    ## birthyear information ##
    ###########################
    birthyear.variable <- "birthyr"

    ############
    ## gender ##
    ############
    gender.var <- "C_GENDER"
    female.label <- 1
    male.label <- 2

    ###############
    ## time name ##
    ###############
    time.name.order <- "EVENT_ID"
    time.name <- time.name.order
    event.name <- time.name  ## CHECK if we need to adjust time.baseline.motor=1??
    event.name.motor <- event.name


    time.baseline <- "BL"
    age.name <- NULL
    age.name.motor <-  "uhd1days"
    age.name.mci <-  "uhd2days"
    age.name.behavior <- "uhd3days"
    age.name.tfc <- "uhd6days"



    followup.motor <- "uhd1days"
    followup.mci <- "uhd2days"
    followup.behavior <- "uhd3days"
    followup.tfc <- "uhd6days"

    ##########################
    ## motor diagnosis info ##
    ##########################
    clinical.diagnosis.year <- "C_CLINDXYR"
    tms.name <- "uhdrmotor"
    dcl.name <- "C_DIAGCONL"

    ###################
    ## Baseline info ##
    ###################
    base.var <-  c("birthyr","age")
    base.label <- c("birthyr","base_age")

    #########################
    ## cognitive variables ##
    #########################
    sdmt.name <- "C_CASYDIG"
    stroop_color.name <- "C_CASTRPCN"
    stroop_word.name <- "C_CASTRPWR"
    stroop_interference.name <- "C_CASTRPIN"

    ########################################
    ## Total functional capacity variable ##
    ########################################
    tfc.name <- "tfc"

    ########################
    ## behavior variables ##
    ########################
    cutoff.apathy <- 2
    cutoff.irb <- 2
    cutoff.dep <- 2

    apathy.sev.name <- "C_APATHSEV"
    apathy.frq.name <- "C_APATHFRQ"

    irb.sev.name <- "C_IRBEHSEV"
    irb.frq.name <- "C_IRBEHFRQ"

    dep.sev.name <- "C_DEPMDSEV"
    dep.frq.name <- "C_DEPMDFRQ"

    ###################
    ## cag variables ##
    ###################
    cag.var <- c("Blood_ALLELE_1","Blood_ALLELE_2")

    ###################
    ## race variable ##
    ###################
    race.variable <- "C_RACEGRP"

    #################################
    ## who knows genetic knowledge ##
    #################################
    gene.know <- NULL

    ########################
    ## education variable ##
    ########################
    education.var <-  c("C_EDUCYRS","C_EDUC")
    education.label <- c("educyrs","educ")

  } else if(study=="track"){
    ########
    ## id ##
    ########
    id.name <- "subject"

    ###################
    ## site variable ##
    ###################
    site.variable <- NULL  ## 5/11/2018: Need to fill in for future use


    ###########################
    ## birthyear information ##
    ###########################
    birthyear.variable <- NULL ## 5/11/2018: Need to fill in for future use

    ############
    ## gender ##
    ############
    gender.var <- "gender"
    female.label <- "f"
    male.label <- "m"

    ###################
    ## time variable ##
    ###################
    time.name.order <- "visit"
    time.name <- time.name.order
    event.name <- time.name
    event.name.motor <- event.name

    time.baseline <- 1
    age.name <- "age"
    age.name.motor <-  age.name
    age.name.mci <-  age.name
    age.name.behavior <- age.name
    age.name.tfc <- age.name

    followup.motor <- "fu"
    followup.mci <- "fu"
    followup.behavior <- "fu"
    followup.tfc <- "fu"

    ##########################
    ## motor diagnosis info ##
    ##########################
    clinical.diagnosis.year <- NULL
    tms.name <- "motorscore"
    dcl.name <- "diagconf"


    ###################
    ## Baseline info ##
    ###################
    base.var <-  "age_b"
    base.label <- "base_age"

    #########################
    ## cognitive variables ##
    #########################
    sdmt.name <- "sdmt"
    stroop_color.name <- NULL
    stroop_word.name <- "stroop"
    stroop_interference.name <- NULL

    ########################################
    ## Total functional capacity variable ##
    ########################################
    tfc.name <- NULL  		## NEED TO FILL IN IF NEEDED IN FUTURE

    ########################
    ## behavior variables ##
    ########################
    cutoff.apathy <- 2
    cutoff.irb <- 2
    cutoff.dep <- 11  ## CHECK WITH KAREN!!!!!!!!!!!!!

    psych.name <- c("anxscore",  ## HAD-SIS anxiety subscore
                    "depscore",	  ## HAD-SIS depression subscore
                    "irrscore",	  ## HAD-SIS irritability subscore
                    "outwardirrscore",  ## HAD-SIS outward irritability subscore
                    "inwardirrscore", 	## HAD-SIS inward irritability subscore
                    "bditotal",			## Beck Depression Score
                    "apathysscore",		## Baltimore apathy score
                    "apathycscore", 	## Baltimore Apathy score
                    "irtysscore",		## Baltimore irritability score
                    "irtycscore",		## Baltimore irritability score
                    "think",			## PBA preseverative thinking
                    "beh",				## PBA aggressive behavior
                    "behave_all",		## PBA total behavior score
                    "affect",			## PBA affect factor
                    "irritability",		## PBA irritability factor
                    "irr_der_2",		## PBA irritability factor without perseveration
                    "apathy",			## PBA apathy
                    "disor",			## PBA disorientation
                    "psych"			## PBA psychosis score
    )

    apathy.sev.name <- c("apathysscore", ## Baltimore apathy score, self-rating
                         "apathy")    ## and PBA apathy score
    apathy.frq.name <- NULL

    irb.sev.name <- c("irtysscore",  ## Baltimore Irritability score, self-rating
                      "irrscore",    ## HAD-SIS irritability score
                      "irritability")  ## PBA irritability factor

    irb.frq.name <- NULL

    dep.sev.name <- c("bditotal",	## Beck depression score
                      "depscore")  	## HAD-SIS depression subscore
    dep.frq.name <- NULL

    ###################
    ## cag variables ##
    ###################
    cag.var <- "cag"

    ## who knows genetic knowledge
    gene.know <- NULL

    ###################
    ## race variable ##
    ###################
    race.variable <- NULL  ## CHECK what it is

    ########################
    ## education variable ##
    ########################
    education.var <-  "education"
    education.label <- "educ"

  } else if(study=="pharos"){
    ########
    ## id ##
    ########
    id.name <- "id"

    ###################
    ## site variable ##
    ###################
    site.variable <- "uhdrsite"

    ###########################
    ## birthyear information ##
    ###########################
    birthyear.variable <- "dob"

    ############
    ## gender ##
    ############
    gender.var <- "mhxsex"
    female.label <- 1
    male.label <- 2

    ###############
    ## time name ##
    ###############
    time.name.order <- "visitdt_age"
    time.name <- time.name.order
    time.baseline <- c(-1,0,1)
    age.name <- "visitdt_age"
    age.name.motor <- "irmadate_age"  ##  use independent rater info
    age.name.behavior <- "visitdt_age"
    age.name.mci <- "visitdt_age"
    age.name.tfc <- "visitdt_age"

    event.name <- "uhdrvisit"
    event.name.motor <- "irmavisit"



    followup.motor <- NULL
    followup.mci <- NULL
    followup.behavior <- NULL
    followup.tfc <- NULL

    ##########################
    ## motor diagnosis info ##
    ##########################
    clinical.diagnosis.year <- NULL
    tms.name <- "irmamotor"
    dcl.name <- "irmadiagnosis"

    ###################
    ## Baseline info ##
    ###################
    base.var <-  "visitdt_age"
    base.label <- c("base_age","birthyr")

    #########################
    ## cognitive variables ##
    #########################
    sdmt.name <- "uhdrsymbdigit"
    stroop_color.name <- "uhdrstropcolor"
    stroop_word.name <- "uhdrstropread"
    stroop_interference.name <- "uhdrstropinter"

    ########################################
    ## Total functional capacity variable ##
    ########################################
    tfc.name <- "tfc"

    ########################
    ## behavior variables ##
    ########################
    cutoff.apathy <- 2
    cutoff.irb <- 2
    cutoff.dep <- 2

    apathy.sev.name <- "uhdrapathysev"
    apathy.frq.name <- "uhdrapathyfrq"

    irb.sev.name <- "uhdrirritfsev"
    irb.frq.name <- "uhdrirritfrq"

    dep.sev.name <- "uhdrdeprsev"
    dep.frq.name <- "uhdrdeprfrq"

    ###################
    ## cag variables ##
    ###################
    cag.var <- c("caga1","caga2")  ## CAG repeats on each allele

    ###################
    ## race variable ##
    ###################
    race.variable <- "mhxrace"

    ## who knows genetic knowledge
    gene.know <- c("gene.site.investigator",
                   "gene.indep.rater","gene.study.coord",
                   "gene.study.subj")

    ########################
    ## education variable ##
    ########################
    education.var <- "mhxeduc" ## education in years
    education.label <- "educ"
  } else if(study=="predict"){
    ########
    ## id ##
    ########
    id.name <- "SUBJID"


    ###################
    ## site variable ##
    ###################
    site.variable <- "siteid"


    ###########################
    ## birthyear information ##
    ###########################
    birthyear.variable <- NULL

    ############
    ## gender ##
    ############
    gender.var <- "GENDER"
    female.label <- "f"
    male.label <- "m"

    ###################
    ## time variable ##
    ###################
    time.name.order <- "EVENT"
    time.name <- "YEAR"
    event.name <- "EVENT"
    event.name.motor <- event.name

    time.baseline <- as.numeric(paste(seq(1,10),"01",sep=""))
    age.name <- "AGE"
    age.name.motor <- "YEAR"
    age.name.mci <-  "YEAR"
    age.name.behavior <- "YEAR"
    age.name.tfc <- "YEAR"


    followup.motor <- NULL
    followup.mci <- NULL
    followup.behavior <- NULL
    followup.tfc <- NULL

    ##########################
    ## motor diagnosis info ##
    ##########################
    clinical.diagnosis.year <- NULL
    tms.name <- "TOTAL_MOTOR_SCORE"
    dcl.name <- "DCL"

    ###################
    ## Baseline info ##
    ###################
    base.var <-  c("EVENT","YEAR","AGE")
    base.label <- c("base_age","base_year","birthyr")

    #########################
    ## cognitive variables ##
    #########################
    sdmt.name <- "SDMT"
    stroop_color.name <- "STROOP_COLOR"
    stroop_word.name <- "STROOP_WORD"
    stroop_interference.name <- "STROOP_INTERFERENCE"

    ########################################
    ## Total functional capacity variable ##
    ########################################
    tfc.name <- "TFC"

    ########################
    ## behavior variables ##
    ########################
    cutoff.apathy <- 2
    cutoff.irb <- 2
    cutoff.dep <- 2

    dep.frq.name <- "q25a" #Frequency: Within the past month, how often have you been feeling sad, down, or blue?
    dep.sev.name <- "q25b" #Severity: How has your mood affected your daily activities? [Evidence of sad mood from behavioral observation includes sad voice or expression, tearfulness.]

    apathy.frq.name <- "q26a" 		 #Frequency: Within the past month, how often have you found that you have lost interest in things that used to be important to you? For example, are you just as interested as always in trying new things, starting
    apathy.sev.name <- "q26b" #Severity: How has apathy impacted your ability to carry out daily activities?

    irb.frq.name <- "q30a" #Frequency: In the past month, how often have you felt impatient, irritable, or cranky?
    irb.sev.name <- "q30b" #Severity: How has irritability impacted your ability to get along with others?




    ###################
    ## cag variables ##
    ###################
    cag.var <- c("CAG")

    ###################
    ## race variable ##
    ###################
    race.variable <- "RACE"

    #################################
    ## who knows genetic knowledge ##
    #################################
    gene.know <- NULL

    ########################
    ## education variable ##
    ########################
    education.var <- "EDUC_YRS" ## education in years
    education.label <- "educ"
  } else if(study=="enroll"){
    ########
    ## id ##
    ########
    id.name <- "subjid"


    ###################
    ## site variable ##
    ###################
    site.variable <- NULL  ## 5/11/2018: Need to fill in if we want to use this variable



    ###########################
    ## birthyear information ##
    ###########################
    birthyear.variable <- NULL ## 5/11/2018: Need to fill in if we want to use this

    ############
    ## gender ##
    ############
    gender.var <- "sex"
    female.label <- "f"
    male.label <- "m"

    ####################
    ## time variables ##
    ####################
    time.name.order <- "seq"
    time.name <- time.name.order
    event.name <- time.name
    event.name.motor <- event.name
    time.baseline <- 1

    age.name <- "age"
    age.name.motor <- age.name
    age.name.mci <-  age.name
    age.name.behavior <- age.name
    age.name.tfc <- age.name

    followup.motor <- "visdy"
    followup.mci <- "visdy"
    followup.behavior <- "visdy"
    followup.tfc <- "visdy"

    ##########################
    ## motor diagnosis info ##
    ##########################
    clinical.diagnosis.year <- NULL
    tms.name <- "motscore"  ## maybe also include "miscore"???????????????
    dcl.name <- "diagconf"

    ###################
    ## Baseline info ##
    ###################
    base.var <-   "age_0"
    base.label <- "base_age"


    #########################
    ## cognitive variables ##
    #########################
    sdmt.name <- "sdmt1"
    stroop_color.name <- "scnt1"
    stroop_word.name <- "swrt1"
    stroop_interference.name <- "sit1"

    ########################################
    ## Total functional capacity variable ##
    ########################################
    tfc.name <- NULL	## NEED TO FILL IN IF NEEDED IN FUTURE


    ########################
    ## behavior variables ##
    ########################
    cutoff.apathy <- 2
    cutoff.irb <- 2
    cutoff.dep <- 2

    apathy.sev.name <-  "pbas6sv"
    apathy.frq.name <- NULL

    irb.sev.name <- "pbas4sv"
    irb.frq.name <- NULL

    dep.sev.name <- "pbas1sv"
    dep.frq.name <- NULL

    ###################
    ## cag variables ##
    ###################
    cag.var <- c("caghigh","caglow")

    ## race variable
    race.variable <- "race"

    ## who knows genetic knowledge
    gene.know <- NULL

    ########################
    ## education variable ##
    ########################
    education.var <- "isced"
    education.label <- "educ"
  }

  ## cag limits
  cag.min <- 36
  cag.max <- 50


  ## TFC cutoffs
  cutoff.tfc.stageone <- c(11,13)  ## stage 1: TFC is 11-13
  cutoff.tfc.stagetwo <- c(7,10)   ## stage 2: TFC is 7-10
  cutoff.tfc.stagethree <- c(3,6)   ## stage 2: TFC is 7-10


  out <- list(


    ## id name
    id.name=id.name,

    ## site variable name
    site.variable=site.variable,

    ## birthyear information
    birthyear.variable=birthyear.variable,

    ## gender
    gender.var=gender.var,
    female.label=female.label,
    male.label=male.label,

    ## time name
    time.name.order=time.name.order,
    time.name=time.name,
    event.name=event.name,
    event.name.motor=event.name.motor,

    time.baseline=time.baseline,
    age.name=age.name,
    age.name.motor=age.name.motor,
    age.name.behavior=age.name.behavior,
    age.name.mci=age.name.mci,
    age.name.tfc=age.name.tfc,

    followup.motor=followup.motor,
    followup.mci=followup.mci,
    followup.behavior=followup.behavior,
    followup.tfc=followup.tfc,

    ## motor diagnosis info
    clinical.diagnosis.year=clinical.diagnosis.year,
    tms.name=tms.name,
    dcl.name=dcl.name,

    ## baseline info
    base.var=base.var,
    base.label=base.label,

    ## cognitive variables
    sdmt.name=sdmt.name,
    stroop_color.name=stroop_color.name,
    stroop_word.name=stroop_word.name,
    stroop_interference.name=stroop_interference.name,

    ## Total Functional Capacity (TFC) variables
    tfc.name=tfc.name,
    cutoff.tfc.stageone=cutoff.tfc.stageone,
    cutoff.tfc.stagetwo=cutoff.tfc.stagetwo,
    cutoff.tfc.stagethree=cutoff.tfc.stagethree,

    ## behavior variables
    psych.name = psych.name, ## for TRACK data

    cutoff.apathy=cutoff.apathy,
    cutoff.irb=cutoff.irb,
    cutoff.dep=cutoff.dep,

    apathy.sev.name=apathy.sev.name,
    apathy.frq.name=apathy.frq.name,

    irb.sev.name=irb.sev.name,
    irb.frq.name=irb.frq.name,

    dep.sev.name=dep.sev.name,
    dep.frq.name=dep.frq.name,

    ## cag
    cag.var=cag.var,
    cag.min=cag.min,
    cag.max=cag.max,

    ##race
    race.variable=race.variable,

    ## gene.know
    gene.know=gene.know,

    ## education
    education.var=education.var,
    education.label=education.label
  )

  return(out)
}

####################################
## Functions used to get MCI data ##
####################################

organize.mci.behavior.data <- function(location,study,type=c("cognitive","behavior")[1]){
  #####################################
  ## Get data sets used for analysis ##
  #####################################
  all.study.data <- get.study.data(study)
  cag.data <- all.study.data$cag.data
  cognitive.data <- all.study.data$cognitive.data
  behavior.data <- all.study.data$behavior.data
  education.data <- all.study.data$education.data
  demographic.data <- all.study.data$demographic.data
  birth.data <- all.study.data$birth.data
  var.names <- all.study.data$var.names

  ############################
  ## Extract variable names ##
  ############################

  time.name.order <- var.names$time.name.order
  time.name <- var.names$time.name
  time.baseline <- var.names$time.baseline
  cognitive.variable.name <- c(var.names$sdmt.name,
                               var.names$stroop_color.name,
                               var.names$stroop_word.name,
                               var.names$stroop_interference.name)

  behavior.variable.name <- c(var.names$apathy.sev.name,
                              var.names$apathy.frq.name,
                              var.names$irb.sev.name,
                              var.names$irb.frq.name,
                              var.names$dep.sev.name,
                              var.names$dep.frq.name)
  cag.name <- "CAG"

  #################
  ## get id name ##
  #################
  id.name <- var.names$id.name


  ###################
  ## get education ##
  ###################
  education <- extract.education(id.name,education.data,
                                 var.name=var.names$education.var,
                                 var.label=var.names$education.label)
  education2 <- binary.education(id.name,education,study)



  ################
  ## get gender ##
  ################
  gender <- compute.gender(id.name,
                           var.name=var.names$gender.var,
                           female.label=var.names$female.label,
                           male.label=var.names$male.label,
                           demographic.data)

  ####################
  ## get CAG repeat ##
  ####################
  cag <- compute.cag.repeats(id.name,cag.data,study,var.name=var.names$cag.var)
  cag.unif <- reverse.cag(cag$CAG,xmin=var.names$cag.min,xmax=var.names$cag.max)
  cag <- data.frame(cag=cag,cag.unif=cag.unif)
  colnames(cag) <- c(id.name,"CAG","CAG-unif")


  #########################################
  ## get birth date, and age at baseline ##
  #########################################
  birth.base.age <- compute.birth.base.age(id.name,
                                           birth.data,study,
                                           var.name=var.names$base.var,
                                           var.label=var.names$base.label,
                                           time.name=var.names$time.name,
                                           time.name.order=var.names$time.name.order,
                                           time.baseline=var.names$time.baseline,
                                           event.name=var.names$event.name.motor,
                                           age.name=var.names$age.name,
                                           birthyear.variable=var.names$birthyear.variable)


  #################################################
  ## Set up for cognitive or behavior extraction ##
  #################################################
  if(type=="cognitive"){
    data.use <- cognitive.data
    variable.name.use <- cognitive.variable.name
    file.name <- "_cog_data.csv"
    event.name.use <- var.names$event.name
  } else if(type=="behavior"){
    data.use <- behavior.data
    variable.name.use <- behavior.variable.name
    file.name <- "_beh_data.csv"
    event.name.use <- var.names$event.name
  }

  ########################
  ## get data ##
  ########################
  if(!is.null(data.use)){
    status.demog <- merge.all(list(demographic=cag[,c(id.name,cag.name)],

                                   mydata=data.use[,c(id.name,unique(c(time.name,time.name.order,event.name.use)),variable.name.use)]),
                              by.name=id.name)
    data.out <- extract.baseline.cognitive.behavior.scores(status.demog,
                                                           study,type,id.name,cag.name,var.names,event.name.use)


    ## add education, gender, and baseline age
    data.out <- merge.all(list(orig=data.out,
                               education2=education2,
                               gender=gender,
                               birth.base.age=birth.base.age[,c(id.name,"base_age")]),
                          by.name=id.name)


    #######################
    ## write data output ##
    #######################
    write.csv(data.out,paste(location,study,file.name,sep=""),row.names=FALSE)
  } else {
    cat("NO DATA AVAILABLE")
  }
}


################################################################
## Function for Psychiatric measures for paper with Hiral Shah##
################################################################

histogram.psych.data <- function(output,study,type,cag.cutoff=36){
  ####################
  ## variable names ##
  ####################
  var.names <- get.all.variable.names(study)
  time.name.order <- var.names$time.name.order
  time.name <- var.names$time.name
  time.baseline <- var.names$time.baseline

  behavior.variable.name <- c(var.names$apathy.sev.name,
                              var.names$irb.sev.name,
                              var.names$dep.sev.name)

  cag.name <- "CAG"

  #######################################
  ## focus on people with CAG >=cutoff ##
  #######################################
  index.cag <- which(output[,cag.name]>=cag.cutoff)
  data.use <- output[index.cag,]

  ####################
  ## make histogram ##
  ####################
  for(ee in 1:length(behavior.variable.name)){
    postscript(paste(study,"_",behavior.variable.name[ee],".eps",sep=""))

    ## make table of score >=x
    sample.size <- sum(!is.na(data.use[,behavior.variable.name[ee]]))
    cutoffs.table <- table(data.use[,behavior.variable.name[ee]])
    cutoffs.sum <- rev(cumsum(rev(cutoffs.table)))

    par(mar=c(5.1,5.1,4.1,2.1))
    barplot(cutoffs.sum,xlab="Cut-off",ylab="Number whose score exceeds cutoff",main="",
            cex.lab=2.5,cex.axis=2.5,cex.lab=2.5)

    ##  par( "usr" ) returns a vector containing xleft, xright, ybottom, ytop.
    usr <- par( "usr" )
    text( usr[ 2], usr[ 4 ], paste("Sample size:",sample.size,sep=" "),
          adj = c( 1, 1 ),
          col = "blue",cex=2.5)
    dev.off()
  }

}

organize.psych.histogram.data <- function(location,study,type=c("cognitive","behavior")[1]){
  #####################################
  ## Get data sets used for analysis ##
  #####################################
  all.study.data <- get.study.data(study)
  cag.data <- all.study.data$cag.data
  behavior.data <- all.study.data$behavior.data
  var.names <- all.study.data$var.names

  ####################
  ## variable names ##
  ####################
  time.name.order <- var.names$time.name.order
  time.name <- var.names$time.name
  time.baseline <- var.names$time.baseline

  behavior.variable.name <- c(var.names$apathy.sev.name,
                              var.names$irb.sev.name,
                              var.names$dep.sev.name)

  cag.name <- "CAG"

  #################
  ## get id name ##
  #################
  id.name <- var.names$id.name

  ####################
  ## get CAG repeat ##
  ####################
  cag <- compute.cag.repeats(id.name,cag.data,study,var.name=var.names$cag.var)


  ##########################################
  ## Maximum of scores measured over time ##
  ##########################################
  time.name <- var.names$time.name
  time.name.order <- var.names$time.name.order
  age.name <- var.names$age.name
  event.name <- var.names$event.name

  for(ee in 1:length(behavior.variable.name)){
    variable.name <- behavior.variable.name[ee]
    out.scores.tmp <- get.max.event.score(id.name=id.name,
                                          data=behavior.data,
                                          variable.name,
                                          time.name,
                                          time.name.order,
                                          age.name,
                                          event.name)
    if(ee==1){
      ## store ID and maximum score
      out.scores <- out.scores.tmp
    } else{
      ## store maximum score only
      out.scores <- cbind(out.scores,out.scores.tmp[,variable.name])
    }
  }

  ## out.scores names
  colnames(out.scores) <- c(id.name,behavior.variable.name)

  ## combine CAG and variable scores
  data.to.merge <- list(
    cag=cag,
    out.scores=out.scores)

  output <- merge.all(data.to.merge,by.name=id.name)
  return(output)
}


organize.psych.data <- function(location,study,type=c("cognitive","behavior")[1]){
  ##########################
  ## Only built for TRACK ##
  ##########################

  #####################################
  ## Get data sets used for analysis ##
  #####################################
  all.study.data <- get.study.data(study)
  demographic.data <- all.study.data$demographic.data
  behavior.data <- all.study.data$behavior.data
  imaging.data <- all.study.data$imaging.data

  var.names <- all.study.data$var.names

  ####################
  ## variable names ##
  ####################
  time.name.order <- var.names$time.name.order
  time.name <- var.names$time.name
  time.baseline <- var.names$time.baseline

  behavior.variable.name <- var.names$psych.name

  cag.name <- "CAG"

  #################
  ## get id name ##
  #################
  id.name <- var.names$id.name

  ######################
  ## date information	##
  ######################
  dates.data <- demographic.data

  ####################
  ## get CAG repeat ##
  ####################
  cag <- compute.cag.repeats(id.name,data=demographic.data,
                             study,var.name=var.names$cag.var)



  ################
  ## get gender ##
  ################
  gender <- compute.gender(id.name,
                           var.name=var.names$gender.var,
                           female.label=var.names$female.label,
                           male.label=var.names$male.label,
                           demographic.data)



  #########################################
  ## get birth date, and age at baseline ##
  #########################################
  birth.base.age <- compute.birth.base.age(id.name,
                                           demographic.data,study,
                                           var.name=var.names$base.var,
                                           var.label=var.names$base.label,
                                           time.name=var.names$time.name,
                                           time.name.order=var.names$time.name.order,
                                           time.baseline=var.names$time.baseline,
                                           event.name=var.names$event.name.motor,
                                           age.name=var.names$age.name,
                                           birthyear.variable=var.names$birthyear.variable)



  ###################
  ## get censorage ##
  ###################
  ## COHORT: don't use hdstatfu days: not as good as DCL
  censorage <- compute.censoring.age(id.name,
                                     data=demographic.data,
                                     dates=birth.base.age,
                                     baseline.data=demographic.data,
                                     study,
                                     time.name=var.names$time.name,
                                     time.name.order=var.names$time.name.order,
                                     age.name=var.names$age.name.motor,
                                     followup.motor=var.names$followup.motor)

  ## data to merge
  data.to.merge.tmp <- list(cag=cag,
                            gender=gender,
                            birth.base.age=birth.base.age)

  data.to.merge <- merge.all(data.to.merge.tmp,by.name=id.name)



  #+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+
  ## Time-to-event ages 					##
  #+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+

  mci.test.name <- NULL
  control.data <- NULL
  data <- behavior.data
  age.name <- var.names$age.name
  followup.name <- var.names$followup.behavior
  event.info <- "behavior"
  event.name <- var.names$event.name
  cutoff.use <- unlist(psych.cutoffs(study)[behavior.variable.name])

  #####################################
  ## behavior time-to-event outcomes ##
  #####################################
  for(bb in 1:length(behavior.variable.name)){

    out <- get.event.data(id.name,
                          mci.test.name,
                          control.data,
                          data,
                          dates.data=dates.data,
                          censorage,
                          variable.name=behavior.variable.name[bb],
                          new.variable.name=behavior.variable.name[bb],
                          age.name=age.name,
                          time.name=var.names$time.name,
                          time.name.order=var.names$time.name.order,
                          time.baseline=var.names$time.baseline,
                          event.name=event.name,
                          followup.name=followup.name,
                          tms.name=var.names$tms.name,
                          study,
                          event.info=event.info,
                          cutoff=cutoff.use[bb],
                          gene.know=var.names$gene.know)

    age.event <- out$data.out

    ## combine baseline variables and variable scores
    data.to.merge.tmp <- list(
      old=data.to.merge,
      new=age.event)


    data.to.merge <- merge.all(data.to.merge.tmp,by.name=id.name)

  }

  ##  add in volumetric imaging data at baseline
  data.to.merge.tmp <- list(
    old=data.to.merge,
    new=imaging.data)

  data.to.merge <- merge.all(data.to.merge.tmp,by.name=id.name)


  return(data.to.merge)
}


#####################################
## Functions used to get data sets ##
#####################################
organize.data.sets <- function(location,study,use.normative.data){
  ## MCI data
  if(use.normative.data==TRUE){
    mci.data <- read.csv("data/mci_data_normative.csv",header=TRUE,row.names=1)
  } else {
    mci.data <- read.csv("data/mci_data.csv",header=TRUE,row.names=1)
  }

  ## BEH data
  beh.data <- read.csv("data/beh_data.csv",header=TRUE,row.names=1)

  #####################################
  ## Get data sets used for analysis ##
  #####################################
  all.study.data <- get.study.data(study)
  baseline.data <- all.study.data$baseline.data
  behavior.data <- all.study.data$behavior.data
  cag.data <- all.study.data$cag.data
  cognitive.data<- all.study.data$cognitive.data
  demographic.data<- all.study.data$demographic.data
  education.data<- all.study.data$education.data
  impairment.data<- all.study.data$impairment.data
  motor.data<- all.study.data$motor.data
  race.data<- all.study.data$race.data
  symptom.data<- all.study.data$symptom.data
  tfc.data<- all.study.data$tfc.data

  birth.data<- all.study.data$birth.data
  censor.data<- all.study.data$censor.data
  site.data <- all.study.data$site.data

  var.names <- all.study.data$var.names

  #################
  ## get id name ##
  #################
  id.name <- var.names$id.name

  #+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+
  ## Baseline information 				##
  #+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+

  ##############
  ## get race ##
  ##############
  if(study=="cohort" | study=="pharos" | study=="predict" | study=="enroll"){
    race <- extract.race(id.name,race.data,var.names$race.variable,study)
  } else {
    race <- NULL
  }

  ###################
  ## get education ##
  ###################
  education <- extract.education(id.name,education.data,
                                 var.name=var.names$education.var,
                                 var.label=var.names$education.label)
  education2 <- binary.education(id.name,education,study)

  ################
  ## get gender ##
  ################
  gender <- compute.gender(id.name,var.name=var.names$gender.var,
                           female.label=var.names$female.label,
                           male.label=var.names$male.label,
                           demographic.data)

  ##########################
  ## get site information ##
  ##########################

  site.information <- extract.variable(site.data,
                                       var.name=var.names$site.variable,
                                       var.label="site",
                                       id.name)


  ####################
  ## get CAG repeat ##
  ####################
  cag <- compute.cag.repeats(id.name,cag.data,study,var.name=var.names$cag.var)
  cag.unif <- reverse.cag(cag$CAG,xmin=var.names$cag.min,xmax=var.names$cag.max)
  cag <- data.frame(cag=cag,cag.unif=cag.unif)
  colnames(cag) <- c(id.name,"CAG","CAG-unif")

  ###############################
  ## genetic testing knowledge ##
  ###############################
  #	if(study=="cohort" | study=="predict"){
  #		if(study=="cohort"){
  #			##########################################################
  #			## response to "Has subject had genetic testing for HD" ##
  #			##########################################################
  #			gene.know.base <- cohort.compute.gentest(baseline.data,var.name="hdage_base")
  #			gene.know.nobase <- cohort.compute.gentest(baseline.data,var.name="hdage_nobase")
  #			gene.know.new.base <- cohort.compute.gentest(baseline.data,var.name="hdage_new_base")
  #			gene.know.new.nobase <- cohort.compute.gentest(baseline.data,var.name="hdage_new_nobase")
  #		}
  #
  #		if(study=="predict"){
  #			gene.know.base <- predict.compute.gentest(baseline.data,var.name="hdage_base")
  #			gene.know.nobase <- predict.compute.gentest(baseline.data,var.name="hdage_nobase")
  #			gene.know.new.base <- predict.compute.gentest(baseline.data,var.name="hdage_new_base")
  #			gene.know.new.nobase <- predict.compute.gentest(baseline.data,var.name="hdage_new_nobase")
  #		}
  #	} else {
  #		gene.know.base <- NULL
  #		gene.know.nobase <- NULL
  #		gene.know.new.base <- NULL
  #		gene.know.new.nobase <- NULL
  #	}

  #########################################
  ## get birth date, and age at baseline ##
  #########################################
  birth.base.age <- compute.birth.base.age(id.name,
                                           birth.data,study,
                                           var.name=var.names$base.var,
                                           var.label=var.names$base.label,
                                           time.name=var.names$time.name,
                                           time.name.order=var.names$time.name.order,
                                           time.baseline=var.names$time.baseline,
                                           event.name=var.names$event.name.motor,
                                           age.name=var.names$age.name,
                                           birthyear.variable=var.names$birthyear.variable)

  ###################
  ## get censorage ##
  ###################
  ## COHORT: don't use hdstatfu days: not as good as DCL
  censorage <- compute.censoring.age(id.name,
                                     data=censor.data,
                                     dates=birth.base.age,
                                     baseline.data=baseline.data,
                                     study,
                                     time.name=var.names$time.name,
                                     time.name.order=var.names$time.name.order,
                                     age.name=var.names$age.name.motor,
                                     followup.motor=var.names$followup.motor)


  #+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+
  ## Time-to-event ages 					##
  #+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+

  ######################
  ## date information	##
  ######################
  if(study=="cohort" | study=="predict"){
    dates.data <- birth.base.age
  } else if(study=="pharos" | study=="enroll"){
    dates.data <- NULL
  } else if(study=="track"){
    dates.data <- demographic.data
  }

  if(!is.null(impairment.data)){  ## only for COHORT
    ###################################
    ## get first ages of impairment  ##
    ###################################
    cog.age <- cohort.first.cchdage(impairment.data,id.name,interest.var="C_CCCOGAGE",interest.name="cog",censorage)
    apathy.age <- cohort.first.cchdage(impairment.data,id.name,interest.var="C_CCAPTAGE",interest.name="apt",censorage)
    irritate.age <- cohort.first.cchdage(impairment.data,id.name,interest.var="C_CCIRBAGE",interest.name="irb",censorage)
    depression.age <- cohort.first.cchdage(impairment.data,id.name,interest.var="C_CCDEPAGE",interest.name="dep",censorage)

  } else {
    cog.age <- NULL
    apathy.age <- NULL
    irritate.age <- NULL
    depression.age <- NULL
  }

  if(!is.null(symptom.data)){
    if(study=="cohort"){
      #############################
      ## get first motor symptom ##
      #############################
      motor.symptom <- compute.motor.symptom(data=symptom.data,dates.data=birth.base.age,id.name,censorage)
    }

    if(study=="pharos"){
      ################################
      ## age of first motor symptom ##
      ################################
      motor.symptom <- pharos.compute.motor.symptom(data=symptom.data,variable.id=id.name,censorage=censorage)
    }
  } else {
    motor.symptom <- NULL
  }


  #######################################################################################
  ## Total functional capacity (TFC) stage 1 (score: 11-13), and stage 2 (score: 7-10) ##
  #######################################################################################

  mci.test.name <- NULL
  control.data <- NULL
  data <- tfc.data
  variable.name <- var.names$tfc.name
  age.name <- var.names$age.name.tfc
  event.name <- var.names$event.name
  followup.name <- var.names$followup.tfc
  event.info <- "tfc"


  #######################################
  ## TFC stage one criteria: TFC >= 11 ##
  #######################################
  new.variable.name <- "tfcone"
  cutoff <- var.names$cutoff.tfc.stageone

  tfc1.out <- get.event.data(id.name,
                             mci.test.name,
                             control.data,
                             data,
                             dates.data=dates.data,
                             censorage,
                             variable.name,
                             new.variable.name,
                             age.name=age.name,
                             time.name=var.names$time.name,
                             time.name.order=var.names$time.name.order,
                             time.baseline=var.names$time.baseline,
                             event.name=event.name,
                             followup.name=followup.name,
                             tms.name=var.names$tms.name,
                             study,
                             event.info=event.info,
                             cutoff=cutoff,
                             gene.know=var.names$gene.know)

  tfc.stage1.age <- tfc1.out$data.out
  tfc.stage1.age.nobase <- tfc1.out$data.out.nobase


  #######################################
  ## TFC stage two criteria: 7 <= TFC <=10 ##
  #######################################
  new.variable.name <- "tfctwo"
  cutoff <- var.names$cutoff.tfc.stagetwo

  tfc2.out <- get.event.data(id.name,
                             mci.test.name,
                             control.data,
                             data,
                             dates.data=dates.data,
                             censorage,
                             variable.name,
                             new.variable.name,
                             age.name=age.name,
                             time.name=var.names$time.name,
                             time.name.order=var.names$time.name.order,
                             time.baseline=var.names$time.baseline,
                             event.name=event.name,
                             followup.name=followup.name,
                             tms.name=var.names$tms.name,
                             study,
                             event.info=event.info,
                             cutoff=cutoff,
                             gene.know=var.names$gene.know)

  tfc.stage2.age <- tfc2.out$data.out
  tfc.stage2.age.nobase <- tfc2.out$data.out.nobase

  #######################################
  ## TFC stage 3 criteria: 3 <= TFC <=6 ##
  #######################################
  new.variable.name <- "tfcthree"
  cutoff <- var.names$cutoff.tfc.stagethree

  tfc3.out <- get.event.data(id.name,
                             mci.test.name,
                             control.data,
                             data,
                             dates.data=dates.data,
                             censorage,
                             variable.name,
                             new.variable.name,
                             age.name=age.name,
                             time.name=var.names$time.name,
                             time.name.order=var.names$time.name.order,
                             time.baseline=var.names$time.baseline,
                             event.name=event.name,
                             followup.name=followup.name,
                             tms.name=var.names$tms.name,
                             study,
                             event.info=event.info,
                             cutoff=cutoff,
                             gene.know=var.names$gene.know)

  tfc.stage3.age <- tfc3.out$data.out
  tfc.stage3.age.nobase <- tfc3.out$data.out.nobase

  ##########################
  ## get age of diagnosis ##
  ##########################
  mci.test.name <- NULL
  control.data <- NULL
  data <- motor.data
  variable.name <- var.names$dcl.name
  age.name <- var.names$age.name.motor
  event.name <- var.names$event.name.motor
  followup.name <- var.names$followup.motor
  event.info <- "motor"


  ##################################
  ## diagnosis criteria: DCL >=4  ##
  ##################################
  new.variable.name <- "hdage"
  cutoff <- 4

  out <- get.event.data(id.name,
                        mci.test.name,
                        control.data,
                        data,
                        dates.data=dates.data,
                        censorage,
                        variable.name,
                        new.variable.name,
                        age.name=age.name,
                        time.name=var.names$time.name,
                        time.name.order=var.names$time.name.order,
                        time.baseline=var.names$time.baseline,
                        event.name=event.name,
                        followup.name=followup.name,
                        tms.name=var.names$tms.name,
                        study,
                        event.info=event.info,
                        cutoff=cutoff,
                        gene.know=var.names$gene.know)

  age.diag <- out$data.out
  age.diag.base <- out$data.out.base
  age.diag.nobase <- out$data.out.nobase

  #################################
  ## diagnosis criteria: DCL >=3 ##
  #################################
  new.variable.name <- "hdage_new"
  cutoff <- 3

  out <- get.event.data(id.name,
                        mci.test.name,
                        control.data,
                        data,
                        dates.data=dates.data,
                        censorage,
                        variable.name,
                        new.variable.name,
                        age.name=age.name,
                        time.name=var.names$time.name,
                        time.name.order=var.names$time.name.order,
                        time.baseline=var.names$time.baseline,
                        event.name=event.name,
                        followup.name=followup.name,
                        tms.name=var.names$tms.name,
                        study,
                        event.info=event.info,
                        cutoff=cutoff,
                        gene.know=var.names$gene.know)

  age.diag.new <- out$data.out
  age.diag.new.base <- out$data.out.base
  age.diag.new.nobase <- out$data.out.nobase


  ##############################################
  ## get first ages of behavioral impairment  ##
  ##############################################
  beh.ages <- NULL
  beh.ages.mci <- NULL

  if(!is.null(behavior.data)){
    ## Common variables
    control.data <- beh.data
    data <- behavior.data
    event.info <- "behavior"
    age.name <- var.names$age.name.behavior
    event.name <- var.names$event.name
    followup.name <- var.names$followup.behavior

    ## Apathy severity: COHORT, PHAROS, PREDICT, ENROLL
    ##0 = no evidence,
    ##1 = equivocal,
    ##2 = mild apathy-subject not initiating conversation or activity but is responsive,
    ##3 = moderate apathy-sometimes responds to efforts to get involved in conversation/activities,
    ##4 = severe apathy-generally unresponsive to attempts to involve subject in activities or conversation

    ## Apathy PBA: TRACK
    ## For behavioral impairments, we took results from PBA assessment. See Kingma et al (2008; General Hospital Psychiatry) in "Notes" folder.
    ## The 5-point PBA rating scales, one subscale for severity and one for frequency, are modelled after the behavioural section of the UHDRS,
    ## using the scores 0 (absent) 1 (questionable), 2 (mild), 3 (moderate) and 4 (severe).
    ## Unlike the UHDRS, which rates behaviour in the last 6 months, the PBA solely assesses behavioural problems in the 4 weeks prior to the interview.

    variable.name <- var.names$apathy.sev.name
    new.variable.name <- "apt2"
    cutoff <- var.names$cutoff.apathy
    mci.test.name <- NULL
    beh.ages <- c(beh.ages,new.variable.name)

    apathy <- get.event.data(id.name,
                             mci.test.name,
                             control.data,
                             data,
                             dates.data=dates.data,
                             censorage,
                             variable.name,
                             new.variable.name,
                             age.name=age.name,
                             time.name=var.names$time.name,
                             time.name.order=var.names$time.name.order,
                             time.baseline=var.names$time.baseline,
                             event.name=event.name,
                             followup.name=followup.name,
                             tms.name=var.names$tms.name,
                             study,
                             event.info=event.info,
                             cutoff=cutoff,
                             gene.know=var.names$gene.know)

    apathy.age2 <- apathy$data.out
    apathy.age2.nobase <- apathy$data.out.nobase


    ############################################
    ## apathy MCI: score > mean + 1.5SD##
    ############################################
    variable.name <- c(var.names$apathy.sev.name,
                       var.names$apathy.frq.name)
    new.variable.name <- "aptmci"
    cutoff <- "upper"
    mci.test.name <- "APATHYMULT"
    beh.ages.mci <- c(beh.ages.mci,new.variable.name)


    apathy.info <- get.event.data(id.name,
                                  mci.test.name,
                                  control.data,
                                  data,
                                  dates.data=dates.data,
                                  censorage,
                                  variable.name,
                                  new.variable.name,
                                  age.name=age.name,
                                  time.name=var.names$time.name,
                                  time.name.order=var.names$time.name.order,
                                  time.baseline=var.names$time.baseline,
                                  event.name=event.name,
                                  followup.name=followup.name,
                                  tms.name=var.names$tms.name,
                                  study,
                                  event.info=event.info,
                                  cutoff=cutoff,
                                  gene.know=var.names$gene.know)

    apathy.age3 <- apathy.info$data.out
    apathy.age3.nobase <- apathy.info$data.out.nobase


    ## Irritability severity: COHORT, PHAROS, PREDICT, ENROLL
    ## 0 = behavior well controlled,
    ## 1 = questionable or equivocal,
    ## 2 = definite but mild,
    ## 3 = moderate, others change their behavior to avoid irritating subject,
    ## 4 = severe irritability

    ## Irritability PBA: TRACK
    ## For behavioral impairments, we took results from PBA assessment. See Kingma et al (2008; General Hospital Psychiatry) in "Notes" folder.
    ## The 5-point PBA rating scales, one subscale for severity and one for frequency, are modelled after the behavioural section of the UHDRS,
    ## using the scores 0 (absent) 1 (questionable), 2 (mild), 3 (moderate) and 4 (severe).
    ## Unlike the UHDRS, which rates behaviour in the last 6 months, the PBA solely assesses behavioural problems in the 4 weeks prior to the interview.

    variable.name <- var.names$irb.sev.name
    new.variable.name <- "irb2"
    cutoff <- var.names$cutoff.irb
    mci.test.name <- NULL
    beh.ages <- c(beh.ages,new.variable.name)


    irb <- get.event.data(id.name,
                          mci.test.name,
                          control.data,
                          data,
                          dates.data=dates.data,
                          censorage,
                          variable.name,
                          new.variable.name,
                          age.name=age.name,
                          time.name=var.names$time.name,
                          time.name.order=var.names$time.name.order,
                          time.baseline=var.names$time.baseline,
                          event.name=event.name,
                          followup.name=followup.name,
                          tms.name=var.names$tms.name,
                          study,
                          event.info=event.info,
                          cutoff=cutoff,
                          gene.know=var.names$gene.know)

    irritate.age2 <- irb$data.out
    irritate.age2.nobase <- irb$data.out.nobase

    ############################################
    ## irritability MCI:  score > mean + 1.5SD##
    ############################################
    variable.name <- c(var.names$irb.sev.name,
                       var.names$irb.frq.name)
    new.variable.name <- "irbmci"
    cutoff <- "upper"
    mci.test.name <- "IRBMULT"
    beh.ages.mci <- c(beh.ages.mci,new.variable.name)


    irb.info <- get.event.data(id.name,
                               mci.test.name,
                               control.data,
                               data,
                               dates.data=dates.data,
                               censorage,
                               variable.name,
                               new.variable.name,
                               age.name=age.name,
                               time.name=var.names$time.name,
                               time.name.order=var.names$time.name.order,
                               time.baseline=var.names$time.baseline,
                               event.name=event.name,
                               followup.name=followup.name,
                               tms.name=var.names$tms.name,
                               study,
                               event.info=event.info,
                               cutoff=cutoff,
                               gene.know=var.names$gene.know)

    irritate.age3 <- irb.info$data.out
    irritate.age3.nobase <- irb.info$data.out.nobase

    ## Depression severity: COHORT, PHAROS, PREDICT, ENROLL
    ## 0 = no mood disturbance,
    ## 1 = questionable or equivocal,
    ## 2 = mild, responds to redirection and reassurance,
    ## 3= moderately depressed, expresses distress,
    ## 4 = severe, significant suffering and loss of functioning

    ## depression score: TRACK
    ## See paper by Snaith et al (2003) in "Notes" folder of TRACK
    ## This enabled a reduction of the number of items in the questionnaire to just seven reflecting anxiety and seven reflecting depression.
    ## (Of the seven depression items five reflected aspects of reduction in pleasure response).
    ## Each item had been answered by the patient on a four point (0–3) response category so the possible scores ranged from
    ##  0 to 21 for anxiety and 0 to 21 for depression.
    ## An analysis of scores on the two subscales of a further sample, in the same clinical setting, enabled provision of information that
    ##   a score of 0 to 7 for either subscale could be regarded as being in the normal range,
    ##   a score of 11 or higher indicating probable presence ('caseness') of the mood disorder and
    ##   a score of 8 to 10 being just suggestive of the presence of the respective state.
    ## Further work indicated that the two subscales, anxiety and depression, were independent measures.

    variable.name <- var.names$dep.sev.name
    new.variable.name <- "dep2"
    cutoff <- var.names$cutoff.dep
    mci.test.name <- NULL
    beh.ages <- c(beh.ages,new.variable.name)

    dep <- get.event.data(id.name,
                          mci.test.name,
                          control.data,
                          data,
                          dates.data=dates.data,
                          censorage,
                          variable.name,
                          new.variable.name,
                          age.name=age.name,
                          time.name=var.names$time.name,
                          time.name.order=var.names$time.name.order,
                          time.baseline=var.names$time.baseline,
                          event.name=event.name,
                          followup.name=followup.name,
                          tms.name=var.names$tms.name,
                          study,
                          event.info=event.info,
                          cutoff=cutoff,
                          gene.know=var.names$gene.know)

    depression.age2 <- dep$data.out
    depression.age2.nobase <- dep$data.out.nobase

    ############################################
    ## depression MCI:  score > mean + 1.5SD##
    ############################################
    variable.name <- c(var.names$dep.sev.name,var.names$dep.frq.name)
    new.variable.name <- "depmci"
    cutoff <- "upper"
    mci.test.name <- "DEPMULT"
    beh.ages.mci <- c(beh.ages.mci,new.variable.name)

    dep.info <- get.event.data(id.name,
                               mci.test.name,
                               control.data,
                               data,
                               dates.data=dates.data,
                               censorage,
                               variable.name,
                               new.variable.name,
                               age.name=age.name,
                               time.name=var.names$time.name,
                               time.name.order=var.names$time.name.order,
                               time.baseline=var.names$time.baseline,
                               event.name=event.name,
                               followup.name=followup.name,
                               tms.name=var.names$tms.name,
                               study,
                               event.info=event.info,
                               cutoff=cutoff,
                               gene.know=var.names$gene.know)

    depression.age3 <- dep.info$data.out
    depression.age3.nobase <- dep.info$data.out.nobase
  } else {
    apathy.age2 <- NULL
    apathy.age2.nobase <- NULL
    irritate.age2 <- NULL
    irritate.age2.nobase <- NULL
    depression.age2 <- NULL
    depression.age2.nobase <- NULL

    apathy.age3 <- NULL
    apathy.age3.nobase <- NULL
    irritate.age3 <- NULL
    irritate.age3.nobase <- NULL
    depression.age3 <- NULL
    depression.age3.nobase <- NULL
  }




  ####################################
  ## Mild Cognitive Impairment ages ##
  ####################################
  criteria <- "mci"

  control.data <- mci.data
  data <- cognitive.data

  ## append education and age at baseline information to
  ##  determine which categories to compare to
  data.to.merge <- list(cognitive.data=cognitive.data,
                        education2=education2,
                        birth.base.age=birth.base.age)

  data <- merge.all(data.to.merge,by.name=id.name)

  event.info <- "mci"
  age.name <- var.names$age.name.mci
  event.name <- var.names$event.name
  followup.name <- var.names$followup.mci
  cutoff <- "lower"
  mci.ages <- NULL

  ## SDMT
  ## raw score
  if(!is.null(var.names$sdmt.name)){
    variable.name <- var.names$sdmt.name
    new.variable.name <- "sdmt"
    mci.test.name <- "SDMT"
    mci.ages <- c(mci.ages,new.variable.name)

    ################################################
    ## use all people to determine MCI categories ##
    ################################################
    subset.cutoffs <- FALSE

    mci.info <- get.event.data(id.name,
                               mci.test.name,
                               control.data,
                               data,
                               dates.data=dates.data,
                               censorage,
                               variable.name,
                               new.variable.name,
                               age.name=age.name,
                               time.name=var.names$time.name,
                               time.name.order=var.names$time.name.order,
                               time.baseline=var.names$time.baseline,
                               event.name=event.name,
                               followup.name=followup.name,
                               tms.name=var.names$tms.name,
                               study,
                               event.info=event.info,
                               cutoff=cutoff,
                               gene.know=var.names$gene.know,
                               subset.cutoffs=subset.cutoffs)

    sdmt.mci.age <- mci.info$data.out
    sdmt.mci.age.nobase <- mci.info$data.out.nobase

    ##################################
    ## use different MCI categories ##
    ##################################
    if(use.normative.data==FALSE){
      subset.cutoffs <- TRUE
      new.variable.name <- paste0(new.variable.name,"_cat")

      mci.info <- get.event.data(id.name,
                                 mci.test.name,
                                 control.data,
                                 data,
                                 dates.data=dates.data,
                                 censorage,
                                 variable.name,
                                 new.variable.name,
                                 age.name=age.name,
                                 time.name=var.names$time.name,
                                 time.name.order=var.names$time.name.order,
                                 time.baseline=var.names$time.baseline,
                                 event.name=event.name,
                                 followup.name=followup.name,
                                 tms.name=var.names$tms.name,
                                 study,
                                 event.info=event.info,
                                 cutoff=cutoff,
                                 gene.know=var.names$gene.know,
                                 subset.cutoffs=subset.cutoffs)

      sdmt.mci.category.age <- mci.info$data.out
      sdmt.mci.category.age.nobase <- mci.info$data.out.nobase
    } else {
      sdmt.mci.category.age <- NULL
      sdmt.mci.category.age.nobase <- NULL
    }

  } else {
    sdmt.mci.age <- NULL
    sdmt.mci.age.nobase <- NULL
    sdmt.mci.category.age <- NULL
    sdmt.mci.category.age.nobase <- NULL
  }

  ## Stroop color
  ## number correct
  if(!is.null(var.names$stroop_color.name)){
    variable.name <- var.names$stroop_color.name
    new.variable.name <- "strcol"
    mci.test.name <- "STROOP_COLOR"
    mci.ages <- c(mci.ages,new.variable.name)

    ################################################
    ## use all people to determine MCI categories ##
    ################################################
    subset.cutoffs <- FALSE

    mci.info <- get.event.data(id.name,
                               mci.test.name,
                               control.data,
                               data,
                               dates.data=dates.data,
                               censorage,
                               variable.name,
                               new.variable.name,
                               age.name=age.name,
                               time.name=var.names$time.name,
                               time.name.order=var.names$time.name.order,
                               time.baseline=var.names$time.baseline,
                               event.name=event.name,
                               followup.name=followup.name,
                               tms.name=var.names$tms.name,
                               study,
                               event.info=event.info,
                               cutoff=cutoff,
                               gene.know=var.names$gene.know,
                               subset.cutoffs)

    stroop.color.mci.age <- mci.info$data.out
    stroop.color.mci.age.nobase<- mci.info$data.out.nobase

    ##################################
    ## use different MCI categories ##
    ##################################
    if(use.normative.data==FALSE){
      subset.cutoffs <- TRUE
      new.variable.name <- paste0(new.variable.name,"_cat")


      mci.info <- get.event.data(id.name,
                                 mci.test.name,
                                 control.data,
                                 data,
                                 dates.data=dates.data,
                                 censorage,
                                 variable.name,
                                 new.variable.name,
                                 age.name=age.name,
                                 time.name=var.names$time.name,
                                 time.name.order=var.names$time.name.order,
                                 time.baseline=var.names$time.baseline,
                                 event.name=event.name,
                                 followup.name=followup.name,
                                 tms.name=var.names$tms.name,
                                 study,
                                 event.info=event.info,
                                 cutoff=cutoff,
                                 gene.know=var.names$gene.know,
                                 subset.cutoffs)

      stroop.color.mci.category.age <- mci.info$data.out
      stroop.color.mci.category.age.nobase<- mci.info$data.out.nobase
    } else {
      stroop.color.mci.category.age <- NULL
      stroop.color.mci.category.age.nobase<- NULL
    }

  } else {
    stroop.color.mci.age <- NULL
    stroop.color.mci.age.nobase<- NULL
    stroop.color.mci.category.age <- NULL
    stroop.color.mci.category.age.nobase<- NULL

  }

  ## Stroop word
  ## number correct
  if(!is.null(var.names$stroop_word.name)){
    variable.name <- var.names$stroop_word.name
    new.variable.name <- "strwrd"
    mci.test.name <- "STROOP_WORD"
    mci.ages <- c(mci.ages,new.variable.name)

    ################################################
    ## use all people to determine MCI categories ##
    ################################################
    subset.cutoffs <- FALSE

    mci.info <- get.event.data(id.name,
                               mci.test.name,
                               control.data,
                               data,
                               dates.data=dates.data,
                               censorage,
                               variable.name,
                               new.variable.name,
                               age.name=age.name,
                               time.name=var.names$time.name,
                               time.name.order=var.names$time.name.order,
                               time.baseline=var.names$time.baseline,
                               event.name=event.name,
                               followup.name=followup.name,
                               tms.name=var.names$tms.name,
                               study,
                               event.info=event.info,
                               cutoff=cutoff,
                               gene.know=var.names$gene.know)

    stroop.word.mci.age <- mci.info$data.out
    stroop.word.mci.age.nobase<- mci.info$data.out.nobase

    ################################################
    ## use different MCI categories ##
    ## - not applicable to normative data
    ################################################
    if(use.normative.data==FALSE){
      subset.cutoffs <- TRUE
      new.variable.name <- paste0(new.variable.name,"_cat")

      mci.info <- get.event.data(id.name,
                                 mci.test.name,
                                 control.data,
                                 data,
                                 dates.data=dates.data,
                                 censorage,
                                 variable.name,
                                 new.variable.name,
                                 age.name=age.name,
                                 time.name=var.names$time.name,
                                 time.name.order=var.names$time.name.order,
                                 time.baseline=var.names$time.baseline,
                                 event.name=event.name,
                                 followup.name=followup.name,
                                 tms.name=var.names$tms.name,
                                 study,
                                 event.info=event.info,
                                 cutoff=cutoff,
                                 gene.know=var.names$gene.know)

      stroop.word.mci.category.age <- mci.info$data.out
      stroop.word.mci.category.age.nobase<- mci.info$data.out.nobase
    } else {
      stroop.word.mci.category.age <- NULL
      stroop.word.mci.category.age.nobase<- NULL
    }
  } else {
    stroop.word.mci.age <- NULL
    stroop.word.mci.age.nobase<- NULL
    stroop.word.mci.category.age <- NULL
    stroop.word.mci.category.age.nobase<- NULL

  }


  ## Stroop interference
  ## number correct
  if(!is.null(var.names$stroop_interference.name)){
    variable.name <- var.names$stroop_interference.name
    new.variable.name <- "strint"
    mci.test.name <- "STROOP_INTERFERENCE"
    mci.ages <- c(mci.ages,new.variable.name)

    ################################################
    ## use all people to determine MCI categories ##
    ################################################
    subset.cutoffs <- FALSE

    mci.info <- get.event.data(id.name,
                               mci.test.name,
                               control.data,
                               data,
                               dates.data=dates.data,
                               censorage,
                               variable.name,
                               new.variable.name,
                               age.name=age.name,
                               time.name=var.names$time.name,
                               time.name.order=var.names$time.name.order,
                               time.baseline=var.names$time.baseline,
                               event.name=event.name,
                               followup.name=followup.name,
                               tms.name=var.names$tms.name,
                               study,
                               event.info=event.info,
                               cutoff=cutoff,
                               gene.know=var.names$gene.know)

    stroop.interference.mci.age <- mci.info$data.out
    stroop.interference.mci.age.nobase <- mci.info$data.out.nobase

    ################################################
    ## use different MCI categories ##
    ################################################
    if(use.normative.data==FALSE){
      subset.cutoffs <- TRUE
      new.variable.name <- paste0(new.variable.name,"_cat")

      mci.info <- get.event.data(id.name,
                                 mci.test.name,
                                 control.data,
                                 data,
                                 dates.data=dates.data,
                                 censorage,
                                 variable.name,
                                 new.variable.name,
                                 age.name=age.name,
                                 time.name=var.names$time.name,
                                 time.name.order=var.names$time.name.order,
                                 time.baseline=var.names$time.baseline,
                                 event.name=event.name,
                                 followup.name=followup.name,
                                 tms.name=var.names$tms.name,
                                 study,
                                 event.info=event.info,
                                 cutoff=cutoff,
                                 gene.know=var.names$gene.know)

      stroop.interference.mci.category.age <- mci.info$data.out
      stroop.interference.mci.category.age.nobase <- mci.info$data.out.nobase
    } else {
      stroop.interference.mci.category.age <- NULL
      stroop.interference.mci.category.age.nobase <- NULL
    }
  } else {
    stroop.interference.mci.age <- NULL
    stroop.interference.mci.age.nobase <- NULL
    stroop.interference.mci.category.age <- NULL
    stroop.interference.mci.category.age.nobase <- NULL

  }

  ##########################
  ## merge all onset ages ##
  ##########################
  onset.to.merge <- list(
    age.diag=age.diag,
    age.diag.base=age.diag.base,
    age.diag.nobase=age.diag.nobase,

    age.diag.new=age.diag.new,
    age.diag.new.base=age.diag.new.base,
    age.diag.new.nobase=age.diag.new.nobase,



    apathy.age2=apathy.age2,
    apathy.age2.nobase=apathy.age2.nobase,

    irritate.age2=irritate.age2,
    irritate.age2.nobase=irritate.age2.nobase,

    depression.age2=depression.age2,
    depression.age2.nobase=depression.age2.nobase,

    apathy.age3=apathy.age3,
    apathy.age3.nobase=apathy.age3.nobase,

    irritate.age3=irritate.age3,
    irritate.age3.nobase=irritate.age3.nobase,

    depression.age3=depression.age3,
    depression.age3.nobase=depression.age3.nobase,


    sdmt.mci.age=sdmt.mci.age,
    sdmt.mci.age.nobase=sdmt.mci.age.nobase,

    stroop.color.mci.age=stroop.color.mci.age,
    stroop.color.mci.age.nobase=stroop.color.mci.age.nobase,

    stroop.word.mci.age=stroop.word.mci.age,
    stroop.word.mci.age.nobase=stroop.word.mci.age.nobase,

    stroop.interference.mci.age=stroop.interference.mci.age,
    stroop.interference.mci.age.nobase=stroop.interference.mci.age.nobase,


    ##############################
    ## different MCI categories ##
    ##############################
    sdmt.mci.category.age=sdmt.mci.category.age,
    sdmt.mci.category.age.nobase=sdmt.mci.category.age.nobase,

    stroop.color.mci.category.age=stroop.color.mci.category.age,
    stroop.color.mci.category.age.nobase=stroop.color.mci.category.age.nobase,

    stroop.word.mci.category.age=stroop.word.mci.category.age,
    stroop.word.mci.category.age.nobase=stroop.word.mci.category.age.nobase,

    stroop.interference.mci.category.age=stroop.interference.mci.category.age,
    stroop.interference.mci.category.age.nobase=stroop.interference.mci.category.age.nobase,

    ################
    ## TFC stages ##
    ################
    tfc.stage1.age =tfc.stage1.age,
    tfc.stage1.age.nobase=tfc.stage1.age.nobase,
    tfc.stage2.age =tfc.stage2.age,
    tfc.stage2.age.nobase=tfc.stage2.age.nobase,
    tfc.stage3.age =tfc.stage3.age,
    tfc.stage3.age.nobase=tfc.stage3.age.nobase#,
    ##cog.age=cog.age,
    ##apathy.age=apathy.age,
    ##irritate.age=irritate.age,
    ##depression.age=depression.age,
    ##motor.symptom=motor.symptom,
  )

  all.onset <- merge.all(onset.to.merge,by.name=id.name)


  #################################################
  ## merge data sets together with baseline data ##
  #################################################
  baseline.to.merge <- list(all.onset=all.onset,
                            #education=education,
                            education2=education2,
                            cag=cag,
                            gender=gender,
                            #gene.know.base=gene.know.base,
                            #gene.know.nobase=gene.know.nobase,
                            #gene.know.new.base=gene.know.new.base,
                            #gene.know.new.nobase=gene.know.new.nobase,
                            birth.base.age=birth.base.age,
                            race=race,
                            lastyear=censorage,
                            site.information=site.information
  )

  all.data <- merge.all(baseline.to.merge,by.name=id.name)


  ###################
  ## get one event ##
  ###################
  if(!is.null(mci.ages)){
    ## at least one cognitive impairment
    one.mci <- get.mci.events(id.name,all.data,onset.ages=mci.ages,name="mcione",
                              mci.event.min=1,mci.event.max=1e10)
    one.mci.nobase <- get.mci.events(id.name,all.data,
                                     onset.ages=paste(mci.ages,"nobase",sep="_"),name="mcione_nobase",
                                     mci.event.min=1,mci.event.max=1e10)


    ## only one cognitive impairment (single-domain MCI)
    single_domain.mci <- get.mci.events(id.name,all.data,
                                        onset.ages=mci.ages,
                                        name="mcisd",
                                        mci.event.min=1,mci.event.max=1)

    single_domain.mci.nobase <- get.mci.events(id.name,
                                               all.data,
                                               onset.ages=paste0(mci.ages,"_nobase"),
                                               name="mcisd_nobase",
                                               mci.event.min=1,mci.event.max=1)

    ## at least 2 cognitive impairments (multi-domain MCI)
    multi_domain.mci <- get.mci.events(id.name,all.data,
                                       onset.ages=mci.ages,
                                       name="mcimd",
                                       mci.event.min=2,mci.event.max=1e10)
    multi_domain.mci.nobase <- get.mci.events(id.name,
                                              all.data,
                                              onset.ages=paste0(mci.ages,"_nobase"),
                                              name="mcimd_nobase",
                                              mci.event.min=2,mci.event.max=1e10)

    if(use.normative.data==FALSE){
      ## define single and multi-domain MCI based on categories

      single_domain.mci <- get.mci.events(id.name,all.data,
                                          onset.ages=paste0(mci.ages,"_cat"),
                                          name="mcisd",
                                          mci.event.min=1,mci.event.max=1)

      single_domain.mci.nobase <- get.mci.events(id.name,
                                                 all.data,
                                                 onset.ages=paste0(mci.ages,"_cat_nobase"),
                                                 name="mcisd_nobase",
                                                 mci.event.min=1,mci.event.max=1)

      ## at least 2 cognitive impairments (multi-domain MCI)
      multi_domain.mci <- get.mci.events(id.name,all.data,
                                         onset.ages=paste0(mci.ages,"_cat"),
                                         name="mcimd",
                                         mci.event.min=2,mci.event.max=1e10)

      multi_domain.mci.nobase <- get.mci.events(id.name,
                                                all.data,
                                                onset.ages=paste0(mci.ages,"_cat_nobase"),
                                                name="mcimd_nobase",
                                                mci.event.min=2,mci.event.max=1e10)
    }
  } else{
    one.mci <- NULL
    one.mci.nobase <- NULL
    single_domain.mci <- NULL
    single_domain.mci.nobase <- NULL
    multi_domain.mci <- NULL
    multi_domain.mci.nobase <- NULL
  }

  if(!is.null(beh.ages)){
    one.beh <- get.mci.events(id.name,all.data,onset.ages=beh.ages,name="behone",mci.event.min=1,mci.event.max=1e10)
    one.beh.nobase <- get.mci.events(id.name,all.data,onset.ages=paste(beh.ages,"nobase",sep="_"),name="behone_nobase",mci.event.min=1,mci.event.max=1e10)
    all.beh <- get.mci.events(id.name,all.data,onset.ages=beh.ages,name="behall",mci.event.min=1,mci.event.max=1e10)
    all.beh.nobase <- get.mci.events(id.name,all.data,onset.ages=paste(beh.ages,"nobase",sep="_"),name="behall_nobase",mci.event.min=1,mci.event.max=1e10)

  } else{
    one.beh <- NULL
    one.beh.nobase <- NULL
    all.beh <- NULL
    all.beh.nobase <- NULL
  }

  if(!is.null(beh.ages.mci)){
    one.beh.mci <- get.mci.events(id.name,all.data,onset.ages=beh.ages.mci,name="behmcione",mci.event.min=1,mci.event.max=1e10)
    all.beh.mci <- get.mci.events(id.name,all.data,onset.ages=beh.ages.mci,name="behmciall",mci.event.min=1,mci.event.max=1e10)

  } else{
    one.beh.mci <- NULL
    all.beh.mci <- NULL
  }

  #################################################
  ## merge data sets together with one event data ##
  #################################################
  one.event.to.merge <- list(all.data=all.data,
                             one.mci=one.mci,
                             one.mci.nobase=one.mci.nobase,
                             single_domain.mci=single_domain.mci,
                             single_domain.mci.nobase=single_domain.mci.nobase,
                             multi_domain.mci=multi_domain.mci,
                             multi_domain.mci.nobase=multi_domain.mci.nobase,


                             one.beh=one.beh,
                             one.beh.nobase=one.beh.nobase,
                             all.beh=all.beh,
                             all.beh.nobase=all.beh.nobase,
                             one.beh.mci=one.beh.mci,
                             all.beh.mci=all.beh.mci
  )

  all.data <- merge.all(one.event.to.merge,by.name=id.name)


  #######################
  ## write data output ##
  #######################
  write.csv(all.data,paste(location,study,"_data.csv",sep=""),row.names=FALSE)

}





#########################################
## Function to compare group variables ##
#########################################


get.my.tables.stat <- function(all.data,zz.use,s.use.names,delta.names,cutoff=50){

  ########################################
  ## convert uniform CAG to regular CAG ##
  ########################################
  all.data$CAG <- convert.cag(all.data$CAG,xmin=36,xmax=cutoff)

  #################
  ## sample size ##
  #################
  sample.size <- nrow(all.data)
  names(sample.size) <- "Sample size"

  #####################
  ## censoring rates ##
  #####################
  censoring.out <- apply(all.data[,delta.names],2,get.censoring)
  names(censoring.out) <- s.use.names


  ####################
  ## covariate info ##
  ####################

  info.covariates.to.report <- function(name){
    if(name=="base_age"){
      out <- get.mean.sd(all.data[,name],digits=2)
      names(out) <- "Average age at baseline (SD)"
    } else if(name=="CAG"){
      out <- get.mean.sd(all.data[,name],digits=2)
      names(out) <- "Average CAG-repeat length (SD)"
    } else if(name=="gender"){
      out <- round(mean(all.data[,name])*100,digits=2)
      names(out) <- " \\% Female"
    } else if(name=="educ_cat"){
      out <- round(mean(all.data[,name])*100,digits=2)
      names(out) <- " \\% Subjects with $\\geq$ 15 years of education"
    } else if(grepl("gene.*",name)){
      out <- round(mean(all.data[,name])*100,digits=2)
      names(out) <- " \\% Subjects with gene knowledge known"
    }
    return(out)
  }

  aout <- NULL
  cov.use <- c("CAG",zz.use)
  for(ii in 1:length(cov.use)){
    aout <- c(aout,info.covariates.to.report(cov.use[ii]))
  }

  data.out <- data.matrix(c(sample.size,censoring.out,aout))

  return(data.out)
}



######################################
## summary table of nonconverters   ##
######################################

nonconverters.table <- function(study.names,covariates.use,my.data,
                                results.to.report=c("mean-sd","quantiles")[1]){

  #############################################################
  ## put my.data in one big data frame, with study indicator ##
  #############################################################
  ## put my.data in a data frame
  my.data.all <- merge.all.data(study.names,my.data)

  ## Extract complete cases based on covariates ##
  data.sample <- my.data.all[complete.cases(my.data.all[,
                                                        c(covariates.use)]),]

  ## table.names.use
  table.names.use <- table.names


  cov.use <- c(covariates.use)
  cov.names <- unlist(table.names.use[cov.use])

  data.out <- array(NA,dim=c(1+length(cov.names),length(study.names)),
                    dimnames=list(c("Sample size", cov.names),study.names))

  for(ss in 1:length(study.names)){
    index.use <- which(data.sample$study==study.names[ss])
    data.use <- data.sample[index.use,]

    ## sample size information
    data.out["Sample size",ss] <- nrow(data.use)

    ## other covariate information
    for(ii in 1:length(cov.use)){
      data.out[cov.names[ii],ss] <-
        info.covariates(data.use,cov.use[ii],cov.names[ii],
                        results.to.report)
    }
  }


  return(data.out)
}


##################################################################
## put list of data in one big data frame, with study indicator ##
##################################################################
merge.all.data <- function(study.names,my.data){

  ## put my.data in a data frame
  my.data.all <- data.frame(study=study.names[1],
                            my.data[[study.names[1]]])

  for(ss in 2:length(study.names)){
    my.data.all.tmp <- data.frame(study=study.names[ss],
                                  my.data[[study.names[ss]]])
    my.data.all <- merge(my.data.all,my.data.all.tmp,all=TRUE)
  }
  return(my.data.all)
}

##################################################################
## Extract complete cases based on events, censoring indicator	##
##  and covariates 												##
##################################################################
extract.complete.cases <- function(main.events.use,delta.main.events.use,covariates.use,
                                   my.data.all){
  data.sample <- my.data.all[complete.cases(my.data.all[,
                                                        c(main.events.use,delta.main.events.use,covariates.use)]),]
  return(data.sample)
}






########################################################################################################
## 2x2 summary table for percentage of subjects who experience each event
########################################################################################################
## Input:
## study.names: names of the studies used in the analysis
## covariates.use: name of covariates used in the analysis. This
##			variable is used to extract the complete cases (i.e., no NA)
##			from the data.
## events.use : name of events used in the analysis
## delta.use : name of censoring indicator used in the analysis
## my.data: data set containing all information
## time_val: time points to evaluate if event occured by that time
## main.events.use: This variable is used to extract the complete cases (i.e., no NA)
##			from the data.  This variable may differ from the events.use.
## participants.experience.all.events: If TRUE, this means we subset
##			the data to those participants that experience all events
##			by the end of the study period (i.e., delta=1).
##			If FALSE, this means, we include participants who may
##			not experience all the events (i.e., delta=0).
two.by.two.events.tables <- function(study.names,
                                     covariates.use,
                                     events.use,
                                     delta.use,
                                     my.data,
                                     time_val,
                                     main.events.use=c("hdage_nobase","mcione"),
                                     participants.experience.all.events=TRUE){

  #############################################################
  ## put my.data in one big data frame, with study indicator ##
  #############################################################
  ## put my.data in a data frame
  my.data.all <- merge.all.data(study.names,my.data)

  ##########################################################
  ## Extract complete cases based on: main.events.use 	##
  ##########################################################
  delta.main.events.use <- paste("delta",main.events.use,sep=".")
  data.sample <- extract.complete.cases(main.events.use,delta.main.events.use,
                                        covariates.use,
                                        my.data.all)

  ###################
  ## create output ##
  ###################
  ## output.two.by.two: this is a list that will contain 2x2 tables
  ## for each study, and when we consider all studies together ("ALL")
  output.two.by.two <- get.empty.list(c(study.names,"ALL"))

  ## create event combinations
  event.combinations.info <- get.combi.info(events.use)
  event.combinations.names <- event.combinations.info$combi.names

  ## we create a 2x2 table output for each time point, and pair of event combinations
  for(rr in 1:length(output.two.by.two)){
    output.two.by.two[[rr]] <- array(0,dim=c(length(time_val),
                                             length(event.combinations.names),
                                             2,2),
                                     dimnames=list(times=paste0("t=",time_val),
                                                   event.combinations=event.combinations.names,
                                                   c("Yes","No"),
                                                   c("Yes","No")))
  }


  #######################
  ## Create 2x2 tables ##
  #######################
  for(rr in 1:length(output.two.by.two)){
    ## we check if we need to subset to the study data
    if(names(output.two.by.two)[rr] %in% study.names){
      ## subset to the study data
      index.use <- which(data.sample$study==study.names[rr])
      data.use <- data.sample[index.use,]
    } else {
      data.use <- data.sample
    }

    ## get subset of data for which delta=1 for all events
    if(participants.experience.all.events==TRUE){
      data.use <- all.events.occur(data.use,delta.use)
      if(!names(output.two.by.two)[rr] %in% study.names){
        ## print number of people in ALL studies
        print(dim(data.use))
      }
    }

    ## get data of interest: events and delta.events
    data.use <- data.use[,c(events.use,delta.use)]

    ## identify which individuals experienced each event by each time point
    for(tt in 1:length(time_val)){
      ## for each time point, create a temporary data set that indicates
      ## if the event occurred by time t or not
      data.tmp <- data.use
      for(ii in 1:length(delta.use)){
        tmp <- (data.use[,events.use[ii]] <=time_val[tt] & data.use[,delta.use[ii]]==1)
        data.tmp <- data.frame(data.tmp,tmp)
        colnames(data.tmp)[ncol(data.tmp)] <- paste(events.use[ii],"occur",sep="_")
      }

      ## create the 2x2 table
      for(uu in 1:ncol(event.combinations.info$combi.choice)){
        ## we look at each combination of events
        event.combinations.use <- event.combinations.info$combi.choice[,uu]
        table.tmp <- table(data.tmp[,paste(event.combinations.use,"occur",sep="_")])
        if("TRUE" %in% rownames(table.tmp) & "TRUE" %in% colnames(table.tmp)){
          output.two.by.two[[rr]][tt,uu,"Yes","Yes"] <- table.tmp["TRUE","TRUE"]
        }

        if("TRUE" %in% rownames(table.tmp) & "FALSE" %in% colnames(table.tmp)){
          output.two.by.two[[rr]][tt,uu,"Yes","No"] <- table.tmp["TRUE","FALSE"]
        }

        if("FALSE" %in% rownames(table.tmp) & "TRUE" %in% colnames(table.tmp)){
          output.two.by.two[[rr]][tt,uu,"No","Yes"] <- table.tmp["FALSE","TRUE"]
        }

        if("FALSE" %in% rownames(table.tmp) & "FALSE" %in% colnames(table.tmp)){
          output.two.by.two[[rr]][tt,uu,"No","No"] <- table.tmp["FALSE","FALSE"]
        }
      }
    }

  }
  return(output.two.by.two)
}


## create porportions from output of two.by.two.events.tables function
two.by.two.tables.proportions <- function(two.by.two.info,
                                          events.use,time_val){

  ## create event combinations
  event.combinations.info <- get.combi.info(events.use)
  event.combinations.names <- event.combinations.info$combi.names

  output <- two.by.two.info

  for(rr in 1:length(output)){
    for(tt in 1:length(time_val)){
      for(uu in 1:ncol(event.combinations.info$combi.choice)){
        output[[rr]][tt,uu,,] <- prop.table(two.by.two.info[[rr]][tt,uu,,])
      }
    }
  }

  return(output)
}


## test for independence in two.by.two.event.tables output
two.by.two.test.independence <- function(two.by.two.info,
                                         events.use,time_val, study.names){

  ## create event combinations
  event.combinations.info <- get.combi.info(events.use)
  event.combinations.names <- event.combinations.info$combi.names

  ###################
  ## create output ##
  ###################
  ## output :
  ## we report p-values for each time point, and pair of event combinations
  output <- array(0,dim=c(length(study.names)+1,
                          length(time_val),
                          length(event.combinations.names)),
                  dimnames=list(
                    study=c(study.names,"ALL"),
                    times=paste0("t=",time_val),
                    event.combinations=event.combinations.names))


  ## Get p-values from chi-squared test
  ## Null Hypothesis: Event 1 and 2 are independent
  ## Alternate Hypothesis: Event 1 and 2 are not independent
  for(rr in 1:length(dimnames(output)$study)){
    for(tt in 1:length(time_val)){
      for(uu in 1:ncol(event.combinations.info$combi.choice)){
        chisquare.info <- chisq.test(two.by.two.info[[rr]][tt,uu,,])
        output[rr,tt,uu] <- chisquare.info$p.value
      }
    }
  }
  return(output)
}


## adjust p-value across event-comparisons
## use output from two.by.two.test.independence function
two.by.two.adjust.pvalues <- function(input,time_val,events.use,
                                      p.adjust.methods){
  ## output
  output <- input

  ## create event combinations
  event.combinations.info <- get.combi.info(events.use)
  event.combinations.names <- event.combinations.info$combi.names

  for(rr in 1:length(dimnames(output)$study)){
    for(tt in 1:length(time_val)){
      ##print(p.adjust.methods)
      output[rr,tt,] <- p.adjust(input[rr,tt,],method=p.adjust.methods)
    }
  }

  ###########################
  ## Print out the results ##
  ###########################
  print.two.by.two.adjust.pvalues(output,p.adjust.methods)

  return(output)
}

## print output from p-value adjustment
print.two.by.two.adjust.pvalues <- function(input,p.adjust.methods){
  ## study names
  study.names.tmp <- dimnames(input)$study

  for(rr in 1:length(study.names.tmp)){
    ############################
    cat("\n\n P-value adjustment:",p.adjust.methods, "for study:", study.names.tmp[rr],"\n\n")
    ############################
    foo <- input[rr,,]
    myprint(foo)
  }

}





## function to get subset of data for which delta=1 for all events
all.events.occur <- function(x,delta.use){
  ## get subset of data for which delta=1 for all events
  data.use.tmp <- x[,delta.use]
  check.total <- function(x,delta.use){
    return(sum(x)==length(delta.use))
  }
  data.use.tmp.check <- apply(data.use.tmp,1,check.total,delta.use)
  index.use.tmp <- which(data.use.tmp.check==TRUE)
  data.use <- x[index.use.tmp,]
  return(data.use)
}


########################################################
## function to display tables for Gamm-Clinical paper ##
########################################################



################################
## function to display tables ##
################################

get.my.tables <- function(all.data,baseage.cutoff=18,
                          cag.atrisk=36,cag.cutoff.hi=50,study="cohort"){

  ## For  MCI events: we allow it to happen at baseline.
  ##		If we DO NOT want baseline, need to add after mci.ages,
  ##		mci.ages <- paste(mci.ages,"nobase",sep="_")

  if(study=="cohort"){
    ## SDMT, Stroop Color, Stroop Word, Stroop Interference
    mci.ages <- c("sdmt","strcol","strwrd","strint")

    ## Apathy, irritability, depression
    beh.ages <- c("apt2","irb2","dep2")

    ## Motor symptom
    motor.symptom.ages<- NULL ##"symp"

    ## use.pharos: show genetic knowledge
    use.pharos <- FALSE
    show.gene.know <- FALSE

  } else if(study=="pharos"){
    ## SDMT, Stroop Color, Stroop Word, Stroop Interference
    mci.ages <- c("sdmt","strcol","strwrd","strint")

    ## Apathy, irritability, depression
    beh.ages <- c("apt2","irb2","dep2")

    ## Motor symptom
    motor.symptom.ages <- NULL

    ## use.pharos: show genetic knowledge
    use.pharos <- FALSE ##TRUE
    show.gene.know <- use.pharos

  } else if(study=="predict"){
    ## SDMT, Stroop Color, Stroop Word, Stroop Interference
    mci.ages <- c("sdmt","strcol","strwrd","strint")

    ## Apathy, irritability, depression
    beh.ages <- NULL

    ## Motor symptom
    motor.symptom.ages <- NULL

    ## use.pharos: show genetic knowledge
    use.pharos <- FALSE
    show.gene.know <- FALSE

  } else if(study=="track"){
    ## SDMT,  Stroop Word
    mci.ages <- c("sdmt","strwrd")

    ## Apathy, irritability, depression
    beh.ages <- c("apt2","irb2","dep2")

    ## Motor symptom
    motor.symptom.ages <- NULL

    ## use.pharos: show genetic knowledge
    use.pharos <- FALSE
    show.gene.know <- FALSE

  } else if(study=="enroll"){
    ## SDMT, Stroop Color, Stroop Word, Stroop Interference
    mci.ages <- c("sdmt","strcol","strwrd","strint")

    ## Apathy, irritability, depression
    beh.ages <- c("apt2","irb2","dep2")

    ## Motor symptom
    motor.symptom.ages <- NULL

    ## use.pharos: show genetic knowledge
    use.pharos <- FALSE
    show.gene.know <- FALSE

  }
  ## Motor onset
  motor.onset.ages <- c("hdage_base","hdage_nobase",
                        "hdage_new_nobase")

  ## mci no base
  if(!is.null(mci.ages)){
    mci.ages.nobase <- paste(mci.ages,"nobase",sep="_")
  } else {
    mci.ages.nobase <- NULL
  }

  ## behavioral no base
  if(!is.null(beh.ages)){
    beh.ages.nobase <- paste(beh.ages,"nobase",sep="_")
  } else {
    beh.ages.nobase <- NULL
  }


  ##############################################
  ## Extract data for which baseline age >=18 ##
  ##############################################
  if(study!="enroll"){
    data.out <- all.data[which(all.data[,"base_age"]>=baseage.cutoff),]
  } else {
    data.out <-
      all.data[which(as.numeric.factor(all.data[,"base_age"])>=baseage.cutoff),]
  }

  ###################
  ## get one event ##
  ###################

  ## mci
  if(!is.null(mci.ages)){
    one.mci <- get.one.event(data.out,onset.ages=mci.ages,name="mcione")
    data.out <- data.frame(data.out,one.mci)
    mci.ages <- c(mci.ages,"mcione")
  } else {
    one.mci <- NULL
  }

  ## mci no base
  if(!is.null(mci.ages.nobase)){
    one.mci.nobase <- get.one.event(data.out,onset.ages=mci.ages.nobase,
                                    name="mcione_nobase")
    data.out <- data.frame(data.out,one.mci.nobase)
    mci.ages.nobase <- c(mci.ages.nobase,"mcione_nobase")
  } else {
    one.mci.nobase <- NULL
  }

  ## behavioral
  if(!is.null(beh.ages)){
    one.beh <- get.one.event(data.out,onset.ages=beh.ages,name="behone")
    data.out <- data.frame(data.out,one.beh)
    beh.ages <- c(beh.ages,"behone")
  } else{
    one.beh <- NULL
  }

  ## behavioral no base
  if(!is.null(beh.ages.nobase)){
    one.beh.nobase <- get.one.event(data.out,onset.ages=beh.ages.nobase,
                                    name="behone_nobase")
    data.out <- data.frame(data.out,one.beh.nobase)
    beh.ages.nobase <- c(beh.ages.nobase,"behone_nobase")
  } else{
    one.beh <- NULL
  }


  ######################
  ## subjects at risk ##
  ######################
  at.risk <- which(data.out[,"CAG"]>=cag.atrisk)
  data.at.risk <- data.out[at.risk,]

  ##########################
  ## subjects not at risk ##
  ##########################
  data.not.at.risk <- data.out[-at.risk,]

  ###############
  ## Data sets ##
  ###############
  data.sets <- list(data.all=data.out,
                    data.at.risk=data.at.risk,
                    data.not.at.risk=data.not.at.risk)


  ##################################
  ## Summarize General covariates ##
  ##################################
  function.use <- "continuous"

  interest <- c("CAG")
  interest.names <- interest
  covariates.cag <- summarize.data.sets.covariates(data.sets,
                                                   covariates.interest=interest,
                                                   covariates.names=interest.names,
                                                   function.use=function.use,
                                                   table.name="CAG")

  interest <- c("base_age")
  interest.names <- interest
  covariates.baseage <-  summarize.data.sets.covariates(data.sets,
                                                        covariates.interest=interest,
                                                        covariates.names=interest.names,
                                                        function.use=function.use,
                                                        table.name="BL Age")

  function.use <- "discrete"
  interest <- "gender"
  interest.names <- "female"
  covariates.gender <-  summarize.data.sets.covariates(data.sets,
                                                       covariates.interest=interest,
                                                       covariates.names=interest.names,function.use=function.use,
                                                       table.name="\\% Females")


  interest <- c("educ_cat")
  interest.names <- interest
  covariates.educ <- summarize.data.sets.covariates(data.sets,
                                                    covariates.interest=interest,
                                                    covariates.names=interest.names,function.use=function.use,
                                                    table.name="\\% Higher Educ")

  ####################
  ## Report results ##
  ####################
  cat("\n\n##  CAG ##\n\n")
  foo <- covariates.cag
  myprint(foo)

  ## Age at baseline
  cat("\n\n##  AGE AT BL ##\n\n")
  foo <- covariates.baseage
  myprint(foo)


  ## Gender
  cat("\n\n##  GENDER ##\n\n")
  foo <- covariates.gender
  myprint(foo)

  ## Education
  cat("\n\n##  EDUCATION ##\n\n")
  foo <- covariates.educ
  myprint(foo)

  ###################
  ## motor symptom ##
  ###################
  if(!is.null(motor.symptom.ages)){
    onset.ages <- motor.symptom ## "symp"
    onset.names <- c("Age at first motor symptom")
    filler.names <- "none"

    function.use <- "symp.onset"
    onset.out <- NULL

    for(kk in 1:length(onset.ages)){

      onset.out.tmp <- summarize.data.sets.covariates(data.sets,
                                                      covariates.interest=NULL,
                                                      covariates.names=NULL,
                                                      onset.age.use=onset.ages[kk],
                                                      onset.names=onset.names[kk],
                                                      function.use=function.use,filler=filler.names[kk],
                                                      table.name="Motor Symptom")
      onset.out <- rbind(onset.out,onset.out.tmp)
    }

    cat("\n\n## Motor symptom times ##\n\n")
    foo <- onset.out
    myprint(foo)
  }


  ##################
  ## overall onset ##
  ###################
  all.events <- c(motor.symptom.ages,motor.onset.ages,
                  mci.ages,mci.ages.nobase,
                  beh.ages,beh.ages.nobase
  )
  names.all.events <- as.character(event.names.key[all.events])
  filler.names.events <- as.character(filler.names.key[all.events])
  if(!is.null(all.events)){
    onset.ages <- all.events
    onset.names <- names.all.events
    filler.names <- filler.names.events

    function.use <- "onset"
    onset.out <- NULL
    for(kk in 1:length(onset.ages)){

      onset.out.tmp <- summarize.data.sets.covariates(data.sets,
                                                      covariates.interest=NULL,
                                                      covariates.names=NULL,
                                                      onset.age.use=onset.ages[kk],
                                                      onset.names=onset.names[kk],
                                                      function.use=function.use,filler=filler.names[kk])
      onset.out <- rbind(onset.out,onset.out.tmp)
      ##cat("\n\n## ", onset.names[kk],"##\n\n")
      ##print(xtable(onset.out,digits=1))
    }

    cat("\n\n## Onset times ##\n\n")
    foo <- onset.out
    myprint(foo)
  }

  #################
  ## motor onset ##
  #################
  onset.ages <- c("hdage_nobase",
                  "hdage_new_nobase")

  onset.names <- c(
    "When DCL=4 post-BL",
    #"Age at first motor diagnosis (at BL)",
    "When DCL=3 or 4 post-BL")

  function.use <- "motor.onset"
  motor.onset.out <- NULL
  for(kk in 1:length(onset.ages)){

    motor.onset.out.tmp <- summarize.data.sets.covariates(data.sets,
                                                          covariates.interest=NULL,
                                                          covariates.names=NULL,
                                                          onset.age.use=onset.ages[kk],
                                                          onset.names=onset.names[kk],
                                                          function.use=function.use,
                                                          use.pharos=use.pharos,filler=onset.names[kk],
                                                          show.gene.know=show.gene.know)
    motor.onset.out <- rbind(motor.onset.out,motor.onset.out.tmp)

    ##cat("\n\n## ", onset.names[kk],"##\n\n")
    ##print(xtable(t(onset.out)))
  }

  cat("\n\n## Onset/Gene knowledge ##\n\n")
  foo <- motor.onset.out
  myprint(foo)


  ####################
  ## MCI covariates ##
  ####################
  onset.ages <- mci.ages
  if(!is.null(onset.ages)){
    onset.names <- as.character(mci.event.names.key[onset.ages])
    filler.names <- as.character(mci.filler.names.key[onset.ages])

    function.use <- "mci.onset"
    mci.onset.out <- NULL
    for(kk in 1:length(onset.ages)){

      mci.onset.out.tmp <- summarize.data.sets.covariates(data.sets,
                                                          covariates.interest=NULL,
                                                          covariates.names=NULL,
                                                          onset.age.use=onset.ages[kk],
                                                          onset.names=onset.names[kk],
                                                          function.use=function.use,
                                                          use.pharos=use.pharos, filler=filler.names[kk])
      mci.onset.out <- rbind(mci.onset.out,mci.onset.out.tmp)
    }

    cat("\n\n## MCI Onset ##\n\n")
    foo <- mci.onset.out
    myprint(foo)

    ## simultaneous experience?
    onset.ages <- onset.ages[-length(onset.ages)] ## remove mcione
    onset.names <-  as.character(mci.event.names.key[onset.ages])

    function.use <- "multi.onset"
    multi.out <- summarize.data.sets.covariates(data.sets,
                                                covariates.interest=NULL,
                                                covariates.names=NULL,
                                                onset.age.use=onset.ages,
                                                onset.names=onset.names,
                                                function.use=function.use,
                                                use.pharos=use.pharos)


    cat("\n\n##  Multi MCI onset##\n\n")
    print(xtable(multi.out,digits=1))
  }

  #############################
  ## MCI covariates: NO base ##
  #############################
  onset.ages <- mci.ages.nobase
  if(!is.null(onset.ages)){
    onset.names <- as.character(mci.event.names.key[onset.ages])
    filler.names <- as.character(mci.filler.names.key[onset.ages])

    function.use <- "mci.onset"
    mci.onset.out <- NULL
    for(kk in 1:length(onset.ages)){

      mci.onset.out.tmp <- summarize.data.sets.covariates(data.sets,
                                                          covariates.interest=NULL,
                                                          covariates.names=NULL,
                                                          onset.age.use=onset.ages[kk],
                                                          onset.names=onset.names[kk],
                                                          function.use=function.use,
                                                          use.pharos=use.pharos, filler=filler.names[kk])
      mci.onset.out <- rbind(mci.onset.out,mci.onset.out.tmp)
    }

    cat("\n\n## MCI Onset: NO Baseline ##\n\n")
    foo <- mci.onset.out
    myprint(foo)

    ## simultaneous experience?
    onset.ages <- onset.ages[-length(onset.ages)] ## remove mcione
    onset.names <-  as.character(mci.event.names.key[onset.ages])

    function.use <- "multi.onset"
    multi.out <- summarize.data.sets.covariates(data.sets,
                                                covariates.interest=NULL,
                                                covariates.names=NULL,
                                                onset.age.use=onset.ages,
                                                onset.names=onset.names,
                                                function.use=function.use,
                                                use.pharos=use.pharos)


    cat("\n\n##  Multi MCI onset: No baseline##\n\n")
    print(xtable(multi.out,digits=1))
  }




  ###########################
  ## Behavioral ages		 ##
  ###########################
  onset.ages <- beh.ages
  if(!is.null(onset.ages)){
    onset.names <- as.character(beh.event.names.key[onset.ages])
    filler.names <- as.character(beh.filler.names.key[onset.ages])

    function.use <- "beh.onset"
    beh.onset.out <- NULL

    for(kk in 1:length(onset.ages)){

      beh.onset.out.tmp <- summarize.data.sets.covariates(data.sets,
                                                          covariates.interest=NULL,
                                                          covariates.names=NULL,
                                                          onset.age.use=onset.ages[kk],
                                                          onset.names=onset.names[kk],
                                                          function.use=function.use,
                                                          use.pharos=use.pharos,filler=filler.names[kk])
      beh.onset.out <- rbind(beh.onset.out,beh.onset.out.tmp)
    }

    cat("\n\n## Beh onset ##\n\n")
    foo <- beh.onset.out
    myprint(foo)

    ## simultaneous experience?
    onset.ages <- onset.ages[-length(onset.ages)] ## remove mcione
    onset.names <-  as.character(beh.event.names.key[onset.ages])

    function.use <- "multi.onset"
    multi.out <- summarize.data.sets.covariates(data.sets,
                                                covariates.interest=NULL,
                                                covariates.names=NULL,
                                                onset.age.use=onset.ages,
                                                onset.names=onset.names,
                                                function.use=function.use,
                                                use.pharos=use.pharos)

    cat("\n\n## Multi BEH onset ##\n\n")
    print(xtable(multi.out,digits=1))
  }


  #######################################
  ## Behavioral ages: NO baseline		 ##
  #######################################
  onset.ages <- beh.ages.nobase
  if(!is.null(onset.ages)){
    onset.names <- as.character(beh.event.names.key[onset.ages])
    filler.names <- as.character(beh.filler.names.key[onset.ages])

    function.use <- "beh.onset"
    beh.onset.out <- NULL

    for(kk in 1:length(onset.ages)){

      beh.onset.out.tmp <- summarize.data.sets.covariates(data.sets,
                                                          covariates.interest=NULL,
                                                          covariates.names=NULL,
                                                          onset.age.use=onset.ages[kk],
                                                          onset.names=onset.names[kk],
                                                          function.use=function.use,
                                                          use.pharos=use.pharos,filler=filler.names[kk])
      beh.onset.out <- rbind(beh.onset.out,beh.onset.out.tmp)
    }

    cat("\n\n## Beh onset: NO baseline ##\n\n")
    foo <- beh.onset.out
    myprint(foo)

    ## simultaneous experience?
    onset.ages <- onset.ages[-length(onset.ages)] ## remove mcione
    onset.names <-  as.character(beh.event.names.key[onset.ages])

    function.use <- "multi.onset"
    multi.out <- summarize.data.sets.covariates(data.sets,
                                                covariates.interest=NULL,
                                                covariates.names=NULL,
                                                onset.age.use=onset.ages,
                                                onset.names=onset.names,
                                                function.use=function.use,
                                                use.pharos=use.pharos)

    cat("\n\n## Multi BEH onset: No baseline ##\n\n")
    print(xtable(multi.out,digits=1))
  }


  ##################
  ## for analysis ##
  ##################

  cat("\n\n## DATA FOR ANALYSIS ## \n\n")

  data.for.use <- data.at.risk

  cat("\n\n## check CAG repeats ## \n\n")
  print(table(data.for.use$CAG))

  ## only use data for CAG < cutoff
  index.cag <- which(data.for.use$CAG <=cag.cutoff.hi)
  data.for.use <- data.for.use[index.cag,]

  ## cag limits
  cag.limit <- c(min(data.for.use$CAG),max(data.for.use$CAG))
  print(cag.limit)

  ## standardize cag variable to be in [0,1]
  ##data.for.use$CAG <- (data.for.use$CAG-min(data.for.use$CAG))/(max(data.for.use$CAG)-min(data.for.use$CAG))
  data.for.use$CAG <- reverse.cag(data.for.use$CAG,xmin=36,xmax=cag.cutoff.hi)

  cat("\n\n## unique CAG repeats ## \n\n")
  print(sort(unique(data.for.use$CAG)))

  print(table(data.for.use$CAG))

  #######################
  ## write data output ##
  #######################
  write.table(data.for.use,paste("clean_",study,".dat",sep=""),row.names=FALSE,col.names=TRUE)
  write.table(cag.limit,paste(study,"_cag_limits.dat",sep=""),row.names=FALSE,col.names=FALSE)

  return(data.sets)

}



#####################################
## function for tables: stat paper ##
#####################################
get.my.tables.stat.old <- function(all.data,cutoff=50,data.name="cohort"){

  #use.pharos <- TRUE
  use.pharos <- FALSE
  use.predict <- FALSE

  #if(data.name=="pharos"){
  #  use.pharos <- TRUE
  #}

  if(data.name=="predict"){
    use.predict <- TRUE
  }

  ########################################
  ## convert uniform CAG to regular CAG ##
  ########################################
  all.data$CAG <- convert.cag(all.data$CAG,xmin=36,xmax=cutoff)

  ######################
  ## subjects at risk ##
  ######################
  at.risk <- which(all.data$CAG>=36)
  data.at.risk <- all.data[at.risk,]

  ##########################
  ## subjects not at risk ##
  ##########################
  data.not.at.risk <- all.data[-at.risk,]

  ###############
  ## Data sets ##
  ###############
  data.sets<- list(data.all=all.data,
                   data.at.risk=data.at.risk,
                   data.not.at.risk=data.not.at.risk)

  ########################
  ## General covariates ##
  ########################
  function.use <- "continuous"

  interest <- c("CAG")
  interest.names <- interest
  covariates.cag <- summarize.data.sets.covariates(data.sets,
                                                   covariates.interest=interest,
                                                   covariates.names=interest.names,function.use=function.use,
                                                   table.name="CAG")

  interest <- c("base_age")
  interest.names <- interest
  covariates.baseage <-  summarize.data.sets.covariates(data.sets,
                                                        covariates.interest=interest,
                                                        covariates.names=interest.names,function.use=function.use,
                                                        table.name="BL Age")

  function.use <- "discrete"
  interest <- "gender"
  interest.names <- "female"
  covariates.gender <-  summarize.data.sets.covariates(data.sets,
                                                       covariates.interest=interest,
                                                       covariates.names=interest.names,function.use=function.use,
                                                       table.name="\\% Females")


  interest <- c("educ_cat")
  interest.names <- interest
  covariates.educ <- summarize.data.sets.covariates(data.sets,
                                                    covariates.interest=interest,
                                                    covariates.names=interest.names,function.use=function.use,
                                                    table.name="\\% Higher Educ")

  cat("\n\n##  CAG ##\n\n")
  foo <- covariates.cag
  print(xtable(data.frame(row = rownames(foo),data.frame(foo))),
        sanitize.text.function=function(x){x},
        include.rownames = FALSE)

  ## Age at baseline
  cat("\n\n##  AGE AT BL ##\n\n")
  foo <- covariates.baseage
  print(xtable(data.frame(row = rownames(foo),data.frame(foo))),
        sanitize.text.function=function(x){x},
        include.rownames = FALSE)


  ## Gender
  cat("\n\n##  GENDER ##\n\n")
  foo <- covariates.gender
  print(xtable(data.frame(row = rownames(foo),data.frame(foo))),
        sanitize.text.function=function(x){x},
        include.rownames = FALSE)

  ## Education
  cat("\n\n##  EDUCATION ##\n\n")
  foo <- covariates.educ
  print(xtable(data.frame(row = rownames(foo),data.frame(foo))),
        sanitize.text.function=function(x){x},
        include.rownames = FALSE)

  #################
  ## motor onset ##
  #################
  onset.ages <- c("hdage_nobase")

  onset.names <- c("When DCL=4 post-BL")

  function.use <- "motor.onset"
  motor.onset.out <- NULL
  for(kk in 1:length(onset.ages)){
    motor.onset.out.tmp <- summarize.data.sets.covariates(data.sets,
                                                          covariates.interest=NULL,
                                                          covariates.names=NULL,
                                                          onset.age.use=onset.ages[kk],
                                                          onset.names=onset.names[kk],
                                                          function.use=function.use,
                                                          use.pharos=use.pharos,filler=onset.names[kk],
                                                          show.gene.know=FALSE)
    motor.onset.out <- rbind(motor.onset.out,motor.onset.out.tmp)

    ##cat("\n\n## ", onset.names[kk],"##\n\n")
    ##print(xtable(t(onset.out)))
  }

  cat("\n\n## Onset/Gene knowledge ##\n\n")
  foo <- motor.onset.out
  print(xtable(data.frame(row = rownames(foo),data.frame(foo))),
        sanitize.text.function=function(x){x},
        include.rownames = FALSE)

  ####################
  ## MCI covariates ##
  ####################
  onset.ages <- c("sdmt")

  onset.names <- c("SDMT score")

  filler.names <- c("When MCI occurs based on SDMT")

  function.use <- "mci.onset"
  mci.onset.out <- NULL
  for(kk in 1:length(onset.ages)){
    mci.onset.out.tmp <- summarize.data.sets.covariates(data.sets,
                                                        covariates.interest=NULL,
                                                        covariates.names=NULL,
                                                        onset.age.use=onset.ages[kk],
                                                        onset.names=onset.names[kk],
                                                        function.use=function.use,
                                                        use.pharos=use.pharos, filler=filler.names[kk],
                                                        show.gene.know=FALSE)
    mci.onset.out <- rbind(mci.onset.out,mci.onset.out.tmp)
    ##cat("\n\n## ", onset.names[kk],"##\n\n")
    ##print(xtable(t(onset.out)))
  }

  cat("\n\n## MCI Onset ##\n\n")
  foo <- mci.onset.out
  print(xtable(data.frame(row = rownames(foo),data.frame(foo))),
        sanitize.text.function=function(x){x},
        include.rownames = FALSE)
}





#####################################################
## new function to summarize tables for stat paper ##
#####################################################
get.censoring <- function(x,digits=2){
  tmp.out <- c(mean(x,na.rm=TRUE),mean(1-x,na.rm=TRUE))*100
  tmp.out <- round(tmp.out,digits)
  out <- paste(tmp.out[1]," (",tmp.out[2],")",sep="")
  return(out)
}





get.my.tables.bookchapter <- function(all.data.input,zz.use,s.use.names,delta.names,cag.atrisk,use.at.risk=TRUE){
  ########################
  ## get the data used  ##
  ########################
  all.data <- all.data.input[,c(zz.use,s.use.names,delta.names)]

  ## get complete cases
  #all.data <- all.data[complete.cases(all.data),]

  ####################################
  ## Get individuals at risk or not ##
  ####################################
  index.use <- which(all.data[,"CAG"]>=cag.atrisk)
  if(use.at.risk==TRUE){
    all.data <- all.data[index.use,]
  } else {
    all.data <- all.data[-index.use,]
  }

  #################
  ## sample size ##
  #################
  sample.size <- nrow(all.data)
  names(sample.size) <- "Sample size"

  #################
  ## event rates ##
  #################
  censoring.out <- apply(as.matrix(all.data[,delta.names]),2,get.total.percentage)
  names(censoring.out) <- s.use.names


  ####################
  ## covariate info ##
  ####################

  info.covariates.to.report <- function(name){
    if(name=="base_age"){
      out <- get.mean.sd(as.numeric(all.data[,name]),digits=1)
      names(out) <- "Average age at baseline (SD)"
    } else if(name=="CAG"){
      out <- get.mean.sd(all.data[,name],digits=1)
      names(out) <- "Average CAG-repeat length (SD)"
    } else if(name=="gender"){
      out <- get.total.percentage(all.data[,name],digits=1)
      names(out) <- c("Female")
    } else if(name=="educ_cat"){
      out <- get.total.percentage(all.data[,name],digits=1)
      names(out) <- " \\% Subjects with $\\geq$ 15 years of education"
    } else if(grepl("gene.*",name)){
      out <- round(mean(all.data[,name])*100,digits=1)
      names(out) <- " \\% Subjects with gene knowledge known"
    } else if(name=="race_cat"){
      race.tmp <- as.matrix(table(all.data[,name]))
      out <- get.total.percentage2(race.tmp,digits=1)
      names(out) <- rownames(race.tmp)
    }
    return(out)
  }

  aout <- NULL
  cov.use <- zz.use
  for(ii in 1:length(cov.use)){
    aout <- c(aout,info.covariates.to.report(cov.use[ii]))
  }

  data.out <- data.matrix(c(sample.size,censoring.out,aout))

  return(data.out)
}






#########################################
## Function to call organize.data.sets ##
#########################################
## This functions writes the data to a file.
call.organize.data.sets <- function(location,study.names,use.normative.data){
  for(ss in 1:length(study.names)){
    study <- study.names[ss]
    organize.data.sets(location,study,use.normative.data)
  }
}


#################################################
## Function to call organize.mci.behavior.data ##
#################################################
## This functions writes the data to a file.
call.organize.mci.behavior.data <- function(location,study.names,
                                            type=c("cognitive","behavior")[1]){
  for(ss in 1:length(study.names)){
    study <- study.names[ss]
    organize.mci.behavior.data(location,study,type)
  }
}

##############################################
## Function to read and merge all data sets ##
##############################################
read.and.merge.data.sets <- function(study.names,
                                     baseage.cutoff,
                                     cag.at.risk.cutoff,
                                     cag.not.at.risk.cutoff,
                                     location,
                                     data.to.use=c("at_risk","not_at_risk","all")[1],
                                     print.clean.data,
                                     read.filename){

  ## where to store the data
  my.data <- get.empty.list(study.names)

  ## put all data into a list
  for(ss in 1:length(study.names)){
    study <- study.names[ss]
    all.data <- read.csv(paste(location,study,read.filename,sep=""),header=TRUE)
    if(study=="predict"){
      ## fix id name
      colnames(all.data)[1] <- "id"
    }
    my.data[[study]] <- data.for.analysis(all.data,baseage.cutoff,
                                          cag.at.risk.cutoff,cag.not.at.risk.cutoff,
                                          study,data.to.use,print.clean.data)
  }
  return(my.data)
}


##############################################
## Parameters for analysis: used everywhere ##
##############################################
default.options.for.analysis<- function(){
  location <- "data/"
  study.names <- c("pharos","predict","cohort")
  baseage.cutoff <- 18
  cag.at.risk.cutoff <- c(36,50)
  cag.not.at.risk.cutoff <- 30
  covariates.use <- c("CAG","base_age","gender","educ_cat")
  return(list(location=location,
              study.names=study.names,
              baseage.cutoff=baseage.cutoff,
              cag.at.risk.cutoff=cag.at.risk.cutoff,
              cag.not.at.risk.cutoff=cag.not.at.risk.cutoff,
              covariates.use=covariates.use))
}


## default events analyzed
default.event.options <- function(paper.type){
  if(paper.type=="clinical"){
    mci.specific.events <- c("sdmt","strcol","strwrd","strint")
    mci.ages <- c("mcimd")
    tfc.ages <- c("tfctwo") # c("tfcone","tfcthree")
    #beh.ages <- c("dep2")#,"behone","dep2","apt2","irb2")
    beh.ages <- NULL
    motor.onset.ages <- c("hdage_nobase")
  } else if(paper.type=="statistics"){
    mci.specific.events <- NULL
    mci.ages <- "mcione"
    tfc.ages <- NULL
    beh.ages <- NULL
    motor.onset.ages <- "hdage_nobase"
  }

  ## table.events.use: events reported in the clinical table
  table.events.use <- c(motor.onset.ages,beh.ages,tfc.ages,
                        mci.ages,mci.specific.events)

  ## events analyzed
  events.use <- c(motor.onset.ages,beh.ages,tfc.ages,
                  mci.ages)

  ## main.events.used: events used to set complete cases
  main.events.use <- events.use

  events.use.control <- c(mci.ages,paste0(mci.ages,"_nobase"))

  ## variables used to find duplicates between studies
  overlap.variables.use <- c("birthyr","site","gender","CAG")

  ## time-variables of interest
  time_val <- seq(40,60,by=5)

  return(list(mci.specific.events=mci.specific.events,
              mci.ages=mci.ages,
              tfc.ages=tfc.ages,
              beh.ages=beh.ages,
              motor.onset.ages=motor.onset.ages,
              table.events.use=table.events.use,
              events.use=events.use,
              main.events.use=main.events.use,
              events.use.control=events.use.control,
              overlap.variables.use=overlap.variables.use,
              time_val=time_val))
}


