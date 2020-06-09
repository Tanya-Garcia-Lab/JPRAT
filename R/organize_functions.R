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




######################################
## summary table of nonconverters   ##
######################################

nonconverters.table <- function(study.names,covariates.use,my.data,
                                results.to.report=c("mean-sd","quantiles")[1]){

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


  #############################################################
  ## put my.data in one big data frame, with study indicator ##
  #############################################################
  ## put my.data in a data frame
  my.data.all <- merge.all.data(study.names,my.data)

  ## Extract complete cases based on covariates ##
  data.sample <- my.data.all[complete.cases(my.data.all[,
                                                        c(covariates.use)]),]

  ## table.names.use
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


#####################################################
## new function to summarize tables for stat paper ##
#####################################################
get.censoring <- function(x,digits=2){
  tmp.out <- c(mean(x,na.rm=TRUE),mean(1-x,na.rm=TRUE))*100
  tmp.out <- round(tmp.out,digits)
  out <- paste(tmp.out[1]," (",tmp.out[2],")",sep="")
  return(out)
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



