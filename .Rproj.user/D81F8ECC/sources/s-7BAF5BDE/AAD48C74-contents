
#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+##
# Oct/11/19
# we are not interested in simulated data set at this point       ##
#
#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+##


#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+##
## This code creates default settings for the simulated data sets ##
#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+##

get.functional.alpha.coefficients <-
  function(axmod,study,common.param.data){
    if(axmod=="line"){
      if(common.param.data==TRUE){ # common alpha, beta
        if(censoring.high==FALSE){
          a0 <- 2
        } else {
          a0 <- 0.15
        }
      } else {
        if(study==1){
          a0 <- 2 #4
        } else if(study==2){
          a0 <- 1
        } else if(study==3){
          a0 <- 2
        } else if(study==4){
          a0 <- 1
        } else if(study==5){
          a0 <- 2.5
        } else if(study==6){
          a0 <- 3.5
        }	else if(study==7){
          a0 <- 4.5
        } else if(study==8){
          a0 <- 1.5
        } else if(study==9){
          a0 <- 0.75
        } else if(study==10){
          a0 <- 4.75
        }
      }
    } else if(axmod=="logx"){
      if(common.param.data==TRUE){ # common alpha,beta
        a0 <- 2
      } else {
        if(study==1){
          a0 <- 5
        } else if(study==2){
          a0 <- 3
        } else {
          a0 <- 2
        }
      }
    } else if(axmod=="expx"){
      if(common.param.data==TRUE){ # common alpha, beta
        a0 <- 2
      } else {
        if(study==1){
          a0 <- 5
        } else if(study==2){
          a0 <- 3
        } else {
          a0 <- 2
        }
      }
    } else if(axmod=="wah2"){
      if(common.param.data==TRUE){ # common alpha,beta
        if(censoring.high==FALSE){
          a0 <- 1
        } else {
          a0 <- 0.15
        }
      } else {
        if(study==1){
          a0 <- 2
        } else if(study==2){
          a0 <- 1
        } else if(study==3){
          a0 <- 2
        } else if(study==4){
          a0 <- 1
        } else if(study==5){
          a0 <- 2.5
        } else if(study==6){
          a0 <- 3.5
        }	else if(study==7){
          a0 <- 4.5
        } else if(study==8){
          a0 <- 1.5
        } else if(study==9){
          a0 <- 0.75
        } else if(study==10){
          a0 <- 4.75
        }
      }
    } else if(axmod=="wah4"){
      if(common.param.data==TRUE){ # common alpha, beta
        a0 <- 0.3
      } else{
        if(study==1){
          a0 <- 0.15
        } else if(study==2){
          a0 <- 0.3
        } else {
          a0 <- 0.2
        }
      }
    } else if(axmod=="lgit"){
      if(common.param.data==TRUE) { # common alpha, beta
        a0 <- 1.5
      } else {
        if(study==1){
          a0 <- 5
        } else if(study==2){
          a0 <- 3
        } else {
          a0 <- 2
        }
      }
    } else {
      if(common.param.data==TRUE){ # common alpha,beta
        a0 <- 1.5
      } else {
        if(study==1){
          a0 <- 5
        } else if(study==2){
          a0 <- 3
        } else {
          a0 <- 2
        }
      }
    }
    return(a0)
  }


default.simulation.setting <- function(){

  ####################################
  ## Settings for reviewer requests ##
  ####################################

  ## simulate.high.censoring.rates: should we simulate data with high censoring?
  if(!exists(as.character(substitute(simulate.high.censoring.rates)))){
    simulate.high.censoring.rates <- FALSE
  }


  ## pseudovalues.replaced.by.arbitrary.values: set pseudo-value to 0.5?
  if(!exists(as.character(substitute(pseudovalues.replaced.by.arbitrary.values)))){
    pseudovalues.replaced.by.arbitrary.values <- FALSE
  }


  ######################################
  ## Data setup for simulation study  ##
  ######################################

  ## number.of.studies: number of studies.
  if(!exists(as.character(substitute(number.of.studies)))){
    number.of.studies <- 3
  }


  ## Names of the studies
  if(!exists(as.character(substitute(number.of.studies)))){
    study.names <- paste0("study",1:number.of.studies)
  }

  ## compute number of studies
  number.of.studies <- length(study.names)


  ###########################
  ## Number of simulations ##
  ###########################
  number.of.simulations <- 10 			 ## number of simulations, 10


  ##############################################################
  ## simulated.parameters.common.for.all.studies: common alpha, beta across studies in data generation
  #######################################
  if(!exists(as.character(substitute(simulated.parameters.common.for.all.studies)))){
    simulated.parameters.common.for.all.studies <- FALSE
  }



  #################################################
  ## Generation of covariates and random effects ##
  #################################################


  ## simulated.random.effect.distribution: specification of density for random effect R (this is "v" in the paper).
  if(!exists(as.character(substitute(simulated.random.effect.distribution)))){
    simulated.random.effect.distribution <- "norm"
  }

  ## unknown.random.effect.parameters: specification of which parameters in
  ## of random effects is unknown
  ##  (this is "v" in the paper). See make.normal() in functions_for_simulated_data.R_codes/realdata/main
  ## to see an example of its use.
  if(!exists(as.character(substitute(unknown.random.effect.parameters)))){
    unknown.random.effect.parameters <- "unknown.second.parameter"
  }

  ## simulated.z.covariate.distribution: specification of density for covariate Z.
  if(!exists(as.character(substitute(simulated.z.covariate.distribution)))){
    ##simulated.z.covariate.distribution <- "binom"
    simulated.z.covariate.distribution <- "unif" ##1/23/2017: "unif" is the default choice and what we use for results in paper.
    ##simulated.z.covariate.distribution <- "norR"
    ##simulated.z.covariate.distribution <- "uniR"
  }


  ## unknown.z.covariate.parameters: specification of number of unknown
  ## parameters in density for covariate Z.
  ##  see make.normal() in for an example of its use.
  if(!exists(as.character(substitute(unknown.z.covariate.parameters)))){
    unknown.z.covariate.parameters <- "none"
  }


  ## simulated.x.covariate.distribution: specification of density for covariate X.
  if(!exists(as.character(substitute(simulated.x.covariate.distribution)))){
    simulated.x.covariate.distribution <- "unif"
    ##simulated.x.covariate.distribution <- "uniR"
    ##simulated.x.covariate.distribution <- "norR"
  }


  ## unknown.x.covariate.parameters: specification of number of unknown
  ## parameters in density for covariate X.
  ## see make.normal() in main.R.
  if(!exists(as.character(substitute(unknown.x.covariate.parameters)))){
    unknown.x.covariate.parameters <- "none"
  }

  ## generate data so that censoring depends on z (when Z=binary)
  if(!exists(as.character(substitute(simulated.censoring.depends.on.z)))){
    simulated.censoring.depends.on.z <- TRUE
    if(simulated.z.covariate.distribution!="binom"){
      simulated.censoring.depends.on.z <- FALSE ## we only have it setup for discrete Z
    }
  }

  ## random effect R and covariates X,Z dependent?
  if(!exists(as.character(substitute(simulated.random.effects.depends.on.covariates)))){
    simulated.random.effects.depends.on.covariates <- FALSE
  }

  ## simulated.censor.rate: censoring rate for the studies
  if(!exists(as.character(substitute(simulated.censor.rate)))){
    simulated.censor.rate <- rep(0,number.of.studies)
  }

  ## simulate.data.with.random.effects: generate data with random effects
  if(!exists(as.character(substitute(simulate.data.with.random.effects)))){
    simulate.data.with.random.effects<- TRUE
  }


  ## estimation assuming censoring depends on z (when Z=binary)
  if(!exists(as.character(substitute(estimation.when.censoring.depends.on.z)))){
    estimation.when.censoring.depends.on.z <- TRUE
    if(use_real_data==FALSE){
      if(simulated.z.covariate.distribution!="binom"){
        estimation.when.censoring.depends.on.z <- FALSE ## we only have it setup for discrete Z
      }
    }
  }

  ## use.bootstrap.variance: compute bootstrap variances?
  if(!exists(as.character(substitute(use.bootstrap.variance)))){
    use.bootstrap.variance <- FALSE
  }

  #######################
  ## Sample sizes used ##
  #######################
  if(simulate.high.censoring.rates==FALSE){
    if(number.of.studies==1){		 ## sample size n for each study
      n <- 500
    } else if(number.of.studies==2){
      n <- c(400,500)
    } else if(number.of.studies==3){
      n <- c(300,400,500)                       ## ulc: this sample size
    }
  } else {
    ## high censoring rate
    if(number.of.studies==1){		 ## sample size n for each study
      n <- 900
    } else if(number.of.studies==2){
      n <- c(900,500)
    } else if(number.of.studies==3){
      n <- c(400,500,900)
    }
  }


  ####################
  ## Censoring rate ##
  ####################
  if(simulate.high.censoring.rates==FALSE){
    if(number.of.studies==1){              ## censoring rates
      simulated.censor.rate <- 30
      ##simulated.censor.rate <- 0
    } else if(number.of.studies==2){
      simulated.censor.rate <- c(30,50)
      ##simulated.censor.rate <- c(0,0)
    } else if(number.of.studies==3){
      simulated.censor.rate <- c(0,30,50)  ## ulc: this simulated censoring rate
    }
  } else {
    if(number.of.studies==1){              ## censoring rates
      simulated.censor.rate <- 60
      ##simulated.censor.rate <- 0
    } else if(number.of.studies==2){
      simulated.censor.rate <- c(70,60)
      ##simulated.censor.rate <- c(0,0)
    } else if(number.of.studies==3){
      simulated.censor.rate <- c(40,60,70)
    }
  }




  ##########################################################
  ## Time points used to evaluate functional coefficients ##
  ##########################################################
  time.points.for.prediction <-  c(seq(40,49,by=1),seq(50,60,by=1))

  ## time points to show individual results at and label for alpha(x,t)
  time.points.of.interest <- c(45,55)
  time.points.of.interest.ci <- time.points.of.interest

  label.for.alpha.values.over.time <- time.points.of.interest

  time.points.for.conditional.prediction.toadd  <- NULL
  time.points.for.conditional.prediction <- NULL



  ##################################
  ## Generation of x's 			##
  ##################################
  functional.covariate.values.of.interest <- 0.5
  functional.covariate.values.of.interest.ci <- 0.5
  functional.covariate.names <- "CAG"
  xmin <- NULL
  xmax <- NULL
  functional.covariate.values.for.prediction <-
    get.functional.covariate.values.for.prediction(use_real_data,
                                                   xmin,xmax,functional.covariate.values.of.interest)


  ####################################
  ## Generation of model parameters ##
  ####################################




  ##########################
  ## alpha(x,t) functions ##
  ##########################


  ## functional.alpha.form  and functional.g.form: alpha(x,t)=a0 * functional.alpha.form() * functional.g.form()
  if(simulated.parameters.common.for.all.studies==FALSE){ ## no common alpha, beta
    if(number.of.studies==1){
      functional.alpha.form <- list(study1=c("line","wah2"))
      functional.g.form <- "log"
    } else if(number.of.studies==2){

      functional.alpha.form <- list(study1=c("line","wah2"),
                                    study2=c("wah2","line"))
      functional.g.form <- "log"
    } else if(number.of.studies==3){
      functional.alpha.form <- list(study1=c("cohort1","cohort2"),
                                    study2=c("predict1","predict2"),
                                    study3=c("pharos1","pharos2"))
      functional.g.form <- "noall"
    } else if(number.of.studies==10){
      functional.alpha.form <- list(study1=c("line","wah2"),
                                    study2=c("line","wah2"),
                                    study3=c("line","wah2"),
                                    study4=c("line","wah2"),
                                    study5=c("line","wah2"),
                                    study6=c("line","wah2"),
                                    study7=c("line","wah2"),
                                    study8=c("line","wah2"),
                                    study9=c("line","wah2"),
                                    study10=c("wah2","line"))
      functional.g.form <- "log"
    }
  } else { ## common alpha, beta
    if(number.of.studies==1){
      functional.alpha.form <- list(study1=c("line","wah2"))
      functional.g.form <- "log"
    } else if(number.of.studies==2){
      functional.alpha.form <- list(study1=c("line","wah2"),
                                    study2=c("line","wah2"))
      functional.g.form <- "log"
    } else if(number.of.studies==3){
      functional.alpha.form <- list(study1=c("all1","all2"),
                                    study2=c("all1","all2"),
                                    study3=c("all1","all2"))
      functional.g.form <- "all"
    }
  }



  ###########################
  ## Number of event types ##
  ###########################
  number.of.clinical.events <- max(unlist((lapply(functional.alpha.form,length.apply))))   ## number of events (m_i in paper)
  list.name <- paste("event",1:number.of.clinical.events,sep="")

  event.outcome.names <- list.name
  delta.names <- paste0("delta",1:number.of.clinical.events)

  ##################################
  ## True parameter values: gamma ##
  ##################################

  ## gamma_i : represents the difference between event i and (i-1)
  if(number.of.clinical.events==1){

    event.coefficients <- 0
  } else if(number.of.clinical.events==2){
    if(simulate.high.censoring.rates==FALSE){
      event.coefficients <- c(0,0.25)
    } else {
      event.coefficients <- c(0,0.1)
    }
  } else if(number.of.clinical.events==3){
    event.coefficients <- c(0,0.5,1)
  }
  ##event.coefficients <- NULL

  if(!is.null(event.coefficients)){
    use.functional.event.coefficients <- TRUE
  }

  ##################################
  ## True parameter values: omega ##
  ##################################

  ## omega_s
  ## omega_s represents the difference between study s and (s-1)
  if(number.of.studies==1){
    study.coefficients <- 0
  } else if(number.of.studies==2){
    study.coefficients <- c(0,1)
  } else if(number.of.studies==3){
    study.coefficients <- c(0,0.75,1.25)
  }
  ##study.coefficients <- NULL


  if(!is.null(study.coefficients)){
    use.functional.study.coefficients <- TRUE
  }


  ####################################################
  ## True parameter values: functional.beta.coefficients(t)=beta * functional.g.form() ##
  ####################################################

  ## functional.beta.intercept: intercept
  functional.beta.intercept <- 0.25
  ##functional.beta.intercept <- NULL

  if(!is.null(functional.beta.intercept)){
    use.functional.beta.intercept <- TRUE
  }

  ##################################################
  ## True parameter values:beta(t)=beta * functional.g.form() ##
  ##################################################

  ## beta_is
  if(simulated.parameters.common.for.all.studies==FALSE){ ## no common alpha,beta
    if(number.of.studies==1){
      if(number.of.clinical.events==1){
        ##functional.beta.coefficients <- list(functional.beta.coefficients1=c(4,2,1))
        functional.beta.coefficients <- list(study1=list(functional.beta.coefficients1=c(4)))
      } else if(number.of.clinical.events==2){
        ##functional.beta.coefficients <- list(functional.beta.coefficients1=c(4,2,1),functional.beta.coefficients2=c(2,1,1))
        functional.beta.coefficients <- list(study1=list(functional.beta.coefficients1=c(4),functional.beta.coefficients2=c(2)))
      } else if(number.of.clinical.events==3){
        functional.beta.coefficients <-list(study1= list(functional.beta.coefficients1=c(4),functional.beta.coefficients2=c(0.5),functional.beta.coefficients3=c(2)))
      }
    } else if(number.of.studies==2){
      if(number.of.clinical.events==1){
        ##functional.beta.coefficients <- list(functional.beta.coefficients1=c(4,2,1))
        functional.beta.coefficients <- list(study1=list(functional.beta.coefficients1=c(4)),
                                             study2=list(functional.beta.coefficients1=c(2)))
      } else if(number.of.clinical.events==2){
        if(simulate.high.censoring.rates==FALSE){
          functional.beta.coefficients <- list(study1=list(functional.beta.coefficients1=c(1),functional.beta.coefficients2=c(0.25)),
                                               study2=list(functional.beta.coefficients1=c(0.25),functional.beta.coefficients2=c(1)))
        } else {
          functional.beta.coefficients <- list(study1=list(functional.beta.coefficients1=c(0),functional.beta.coefficients2=c(0.15)),
                                               study2=list(functional.beta.coefficients1=c(0.15),functional.beta.coefficients2=c(0)))
        }
      } else if(number.of.clinical.events==3){
        ##functional.beta.coefficients <- list(functional.beta.coefficients1=c(4,2,1),functional.beta.coefficients2=c(2,1,1),functional.beta.coefficients3=c(3,1,2))
        functional.beta.coefficients <-list(study1= list(functional.beta.coefficients1=c(4),functional.beta.coefficients2=c(0.5),functional.beta.coefficients3=c(2)),
                                            study2= list(functional.beta.coefficients1=c(2),functional.beta.coefficients2=c(1),functional.beta.coefficients3=c(0.5)))
      }
    } else if(number.of.studies==3){
      if(number.of.clinical.events==1){
        functional.beta.coefficients <-list(study1= list(functional.beta.coefficients1=c(-0.25),functional.beta.coefficients2=c(0.25),functional.beta.coefficients3=c(0.35)))
      } else if(number.of.clinical.events==2){ ## this case
        functional.beta.coefficients <- list(study1=list(functional.beta.coefficients1=c(-0.25),functional.beta.coefficients2=c(-0.25)),
                                             study2=list(functional.beta.coefficients1=c(0.25),functional.beta.coefficients2=c(-0.15)),
                                             study3=list(functional.beta.coefficients1=c(0.35),functional.beta.coefficients2=c(0.25)))
      } else if(number.of.clinical.events==3){
        functional.beta.coefficients <-list(study1= list(functional.beta.coefficients1=c(-0.25),functional.beta.coefficients2=c(-0.25),functional.beta.coefficients3=c(-0.25)),
                                            study2= list(functional.beta.coefficients1=c(0.25),functional.beta.coefficients2=c(-0.15),functional.beta.coefficients3=c(0.45)),
                                            study3= list(functional.beta.coefficients1=c(0.35),functional.beta.coefficients2=c(0.25),functional.beta.coefficients3=c(0.30)))
      }
    } else if(number.of.studies==10){
      if(number.of.clinical.events==1){
        functional.beta.coefficients <-list(study1= list(functional.beta.coefficients1=c(-0.25),functional.beta.coefficients2=c(0.25),functional.beta.coefficients3=c(0.35)))
      } else if(number.of.clinical.events==2){
        functional.beta.coefficients <- list(study1=list(functional.beta.coefficients1=c(-0.25),functional.beta.coefficients2=c(-0.25)),
                                             study2=list(functional.beta.coefficients1=c(0.25),functional.beta.coefficients2=c(-0.15)),
                                             study3=list(functional.beta.coefficients1=c(0.35),functional.beta.coefficients2=c(-0.35)),
                                             study4=list(functional.beta.coefficients1=c(0.15),functional.beta.coefficients2=c(-0.15)),
                                             study5=list(functional.beta.coefficients1=c(0.45),functional.beta.coefficients2=c(-0.45)),
                                             study6=list(functional.beta.coefficients1=c(0.20),functional.beta.coefficients2=c(-0.25)),
                                             study7=list(functional.beta.coefficients1=c(0.30),functional.beta.coefficients2=c(-0.35)),
                                             study8=list(functional.beta.coefficients1=c(0.40),functional.beta.coefficients2=c(-0.40)),
                                             study9=list(functional.beta.coefficients1=c(0.10),functional.beta.coefficients2=c(-0.10)),
                                             study10=list(functional.beta.coefficients1=c(0.50),functional.beta.coefficients2=c(-0.50)))
      } else if(number.of.clinical.events==3){
        functional.beta.coefficients <-list(study1= list(functional.beta.coefficients1=c(-0.25),functional.beta.coefficients2=c(-0.25),functional.beta.coefficients3=c(-0.25)),
                                            study2= list(functional.beta.coefficients1=c(0.25),functional.beta.coefficients2=c(-0.15),functional.beta.coefficients3=c(0.45)),
                                            study3= list(functional.beta.coefficients1=c(0.35),functional.beta.coefficients2=c(0.25),functional.beta.coefficients3=c(0.30)))
      }
    }
  } else { ## common alpha, beta
    if(number.of.studies==1){
      if(number.of.clinical.events==1){
        ##functional.beta.coefficients <- list(functional.beta.coefficients1=c(4,2,1))
        functional.beta.coefficients <- list(study1=list(functional.beta.coefficients1=c(4)))
      } else if(number.of.clinical.events==2){
        ##functional.beta.coefficients <- list(functional.beta.coefficients1=c(4,2,1),functional.beta.coefficients2=c(2,1,1))
        functional.beta.coefficients <- list(study1=list(functional.beta.coefficients1=c(4),functional.beta.coefficients2=c(2)))
      } else if(number.of.clinical.events==3){
        ##functional.beta.coefficients <- list(functional.beta.coefficients1=c(4,2,1),functional.beta.coefficients2=c(2,1,1),functional.beta.coefficients3=c(3,1,2))
        functional.beta.coefficients <-list(study1= list(functional.beta.coefficients1=c(4),functional.beta.coefficients2=c(0.5),functional.beta.coefficients3=c(2)))
      }
    } else if(number.of.studies==2){
      if(number.of.clinical.events==1){
        functional.beta.coefficients <- list(study1=list(functional.beta.coefficients1=c(2)),
                                             study2=list(functional.beta.coefficients1=c(2)))
      } else if(number.of.clinical.events==2){

        if(simulate.high.censoring.rates==FALSE){
          functional.beta.coefficients <- list(study1=list(functional.beta.coefficients1=c(1),functional.beta.coefficients2=c(0.25)),
                                               study2=list(functional.beta.coefficients1=c(1),functional.beta.coefficients2=c(0.25)))
        } else {
          functional.beta.coefficients <- list(study1=list(functional.beta.coefficients1=c(0),functional.beta.coefficients2=c(0.1)),
                                               study2=list(functional.beta.coefficients1=c(0),functional.beta.coefficients2=c(0.1)))

        }
      } else if(number.of.clinical.events==3){
        functional.beta.coefficients <-list(study1= list(functional.beta.coefficients1=c(3),functional.beta.coefficients2=c(0.5),functional.beta.coefficients3=c(2)),
                                            study2= list(functional.beta.coefficients1=c(3),functional.beta.coefficients2=c(0.5),functional.beta.coefficients3=c(2)))
      }
    } else if(number.of.studies==3){
      if(number.of.clinical.events==1){
        functional.beta.coefficients <- list(study1=list(functional.beta.coefficients1=c(2)),
                                             study2=list(functional.beta.coefficients1=c(2)),
                                             study3=list(functional.beta.coefficients1=c(2)))
      } else if(number.of.clinical.events==2){
        functional.beta.coefficients <- list(study1=list(functional.beta.coefficients1=c(-3),functional.beta.coefficients2=c(4)),
                                             study2=list(functional.beta.coefficients1=c(-3),functional.beta.coefficients2=c(4)),
                                             study3=list(functional.beta.coefficients1=c(-3),functional.beta.coefficients2=c(4)))
      } else if(number.of.clinical.events==3){
        functional.beta.coefficients <-list(study1= list(functional.beta.coefficients1=c(-0.25),functional.beta.coefficients2=c(0.5),functional.beta.coefficients3=c(0.75)),
                                            study2= list(functional.beta.coefficients1=c(-0.25),functional.beta.coefficients2=c(0.5),functional.beta.coefficients3=c(0.75)),
                                            study3= list(functional.beta.coefficients1=c(-0.25),functional.beta.coefficients2=c(0.5),functional.beta.coefficients3=c(0.75)))
      }
    }


  }


  ###########################
  ## dimension of beta: number.of.functional.beta.coefficients.per.study##
  ###########################
  ## number.of.functional.beta.coefficients: list of beta_is dimensions for each study
  number.of.functional.beta.coefficients.per.study<- lapply(functional.beta.coefficients,double.length.apply)
  maximum.number.of.functional.beta.coefficients <- max(unlist(  number.of.functional.beta.coefficients.per.study))
  nonfunctional.covariate.names <- paste0("z",maximum.number.of.functional.beta.coefficients)

  ########################################
  ## Where to evaluate z's at			##
  ########################################
  if(simulated.z.covariate.distribution=="unif"){
    nonfunctional.covariate.values.for.prediction <- as.matrix(c(0.50,0.75))
    z.discrete.info <- FALSE
  } else if(simulated.z.covariate.distribution=="binom"){
    nonfunctional.covariate.values.for.prediction <- as.matrix(c(0,1))
    z.discrete.info <- FALSE  ## don't want to compute marginal for simulations
  }
  colnames(nonfunctional.covariate.values.for.prediction) <- paste("Z",1:ncol(nonfunctional.covariate.values.for.prediction),sep="") ## Z1
  ## labels for plots
  z.label.names <- paste("zz",nonfunctional.covariate.values.for.prediction,sep="")
  rownames(nonfunctional.covariate.values.for.prediction) <- z.label.names


  #######################################################
  ## Which z-covariate predictions to compare ##
  #######################################################

  ## We only set two options, so we compared both to each other.
  nonfunctional.covariate.comparisons <- list(comp1=c(1,2))
  functional.covariate.comparisons.for.sample.size <- NULL

  ###################################
  ## functional alpha coefficients ##
  ###################################
  a0 <- get.empty.list(paste("study",1:number.of.studies,sep=""))
  for(ss in 1:number.of.studies){
    a0.tmp <- NULL
    for(k in 1:number.of.clinical.events){
      a0.tmp <- c(a0.tmp,get.functional.alpha.coefficients(functional.alpha.form[[ss]][[k]],ss,simulated.parameters.common.for.all.studies))
    }
    a0[[ss]] <- a0.tmp
  }


  ############################
  ## for ueffect (not used) ##
  ############################
  ##study.random.effect.parameter <- c(mean=0,sd=1.5) ## u random effect variance
  study.random.effect.parameter <- NULL


  ###################################################
  ## for density of random effect r ("v" in paper) ##
  ###################################################

  #######################################################################
  ### where to fix:  errors in random.effect.first.parameter2 not found
  #######################################################################
  ### edit 062119
  ### when the distribution is normal, gamma, uniform, noncentral t reffect
  ### allocate random.effect.first.parameters2 =NULL
  ### allocate random.effect.second.parameters2=NULL
  ### allocate mix_n=NULL
  #########################################################################


  if(simulated.random.effect.distribution=="norm"){
    ## normal reffect
    random.effect.first.parameter <- rep(0,number.of.studies) # mean  ## THIS CASE
    random.effect.second.parameter <- rep(1,number.of.studies) # sd
    ### edit
    random.effect.first.parameter2 <- NULL # mean2
    random.effect.second.parameter2 <- NULL # sd2
    mix_n <- NULL   # mix_n
    random.effect.third.parameter <- NULL ## u random effect variance
  } else if(simulated.random.effect.distribution=="gamm"){
    ## gamma reffect
    random.effect.first.parameter <- rep(1.5,number.of.studies) # shape
    random.effect.second.parameter <- rep(2.,number.of.studies)  # scal
    random.effect.third.parameter <- rep(1.5,number.of.studies) ## u random effect variance
    ### edit
    random.effect.first.parameter2 <- NULL # mean2
    random.effect.second.parameter2 <- NULL # sd2
    mix_n <- NULL   # mix_n
  } else if(simulated.random.effect.distribution=="unif"){
    ## unif reffect
    random.effect.first.parameter <- rep(-1,number.of.studies) # low
    random.effect.second.parameter <- rep(1,number.of.studies)   # hi
    random.effect.third.parameter <- rep(1.5,number.of.studies) ## u random effect variance
    ### edit
    random.effect.first.parameter2 <- NULL # mean2
    random.effect.second.parameter2 <- NULL # sd2
    mix_n <- NULL   # mix_n
  } else if(simulated.random.effect.distribution=="tdis"){
    ## noncentral t reffect
    random.effect.first.parameter <- rep(3,number.of.studies) # df
    random.effect.second.parameter <- rep(0,number.of.studies)  # ncp
    random.effect.third.parameter <- rep(1.5,number.of.studies) ## u random effect variance
    ### edit
    random.effect.first.parameter2 <- NULL # mean2
    random.effect.second.parameter2 <- NULL # sd2
    mix_n <- NULL   # mix_n
  } else if(simulated.random.effect.distribution=="mixn"){
    ## mixture of normal reffect
    random.effect.first.parameter <- rep(3,number.of.studies)  # mean1
    random.effect.second.parameter <- rep(1,number.of.studies)    # sd1
    random.effect.first.parameter2 <- rep(-3,number.of.studies) # mean2
    random.effect.second.parameter2 <- rep(0.25,number.of.studies) # sd2
    mix_n <- rep(0.9,number.of.studies)   # mix_n
    random.effect.third.parameter <- rep(1.5,number.of.studies) ## u random effect variance
  }


  ######################
  ## for density of Z ##
  ######################
  if(simulated.z.covariate.distribution=="norm"){
    ## normal zeffect
    z.covariate.first.parameter <- rep(0,number.of.studies) # mean
    z.covariate.second.parameter <- rep(1,number.of.studies) # sd
  } else if(simulated.z.covariate.distribution=="unif"){
    ## unif zeffect
    z.covariate.first.parameter <- rep(0,number.of.studies)  # low
    z.covariate.second.parameter <- rep(1,number.of.studies)  # hi
  } else if(simulated.z.covariate.distribution=="binom"){
    ## binomial zefffect
    z.covariate.first.parameter <- rep(1,number.of.studies)   # size
    z.covariate.second.parameter <- rep(0.5,number.of.studies) # prob
  }

  ######################
  ## for density of X ##
  ######################
  if(simulated.x.covariate.distribution=="norm"){
    ## normal xeffect
    x.covariate.first.parameter <-rep(0,number.of.studies) # mean
    x.covariate.second.parameter <- rep(1,number.of.studies)  # sd
  } else {
    ## unif xeffect
    x.covariate.first.parameter <- rep(0,number.of.studies)  # low
    x.covariate.second.parameter <- rep(1,number.of.studies)  # hi
  }

  #######################
  ## Loading fake data ##
  #######################
  data.sets.as.list <- NULL

  othercovariate.names <- NULL
  nonfunctional.covariate.comparisons <- NULL
  type1.error <- NULL
  type2.error <- NULL
  treatment.effect <- NULL
  dropout.rate <- NULL
  functional.covariate.comparisons <- NULL

  ######################
  ## Plotting options ##
  ######################
  ## do we show the plots for the parameters?
  plot.parameters <- FALSE
  do.plots <- FALSE ## do we show plots?
  plot.confidence.intervals <- TRUE  ## do we show CIs?
  color.labels <- NULL
  legend.labels <- NULL
  event.comparison.table <- NULL

  ##output
  return(list(
    study.names=study.names,
    data.sets.as.list=data.sets.as.list,
    time.points.for.prediction =time.points.for.prediction,
    time.points.for.conditional.prediction.toadd  =time.points.for.conditional.prediction.toadd,
    time.points.for.conditional.prediction =time.points.for.conditional.prediction,
    nonfunctional.covariate.names=nonfunctional.covariate.names,
    nonfunctional.covariate.values.for.prediction = nonfunctional.covariate.values.for.prediction,
    functional.covariate.names=functional.covariate.names,
    functional.covariate.values.of.interest = functional.covariate.values.of.interest,
    functional.covariate.values.of.interest.ci = functional.covariate.values.of.interest.ci,
    xmin=xmin,
    functional.covariate.comparisons=functional.covariate.comparisons,
    xmax=xmax,
    functional.covariate.values.for.prediction=functional.covariate.values.for.prediction,
    othercovariate.names=othercovariate.names,
    event.outcome.names=event.outcome.names,
    delta.names=delta.names,
    use.functional.beta.intercept=use.functional.beta.intercept,
    use.functional.event.coefficients=use.functional.event.coefficients,
    use.functional.study.coefficients=use.functional.study.coefficients,
    time.points.of.interest = time.points.of.interest,
    time.points.of.interest.ci = time.points.of.interest.ci,
    label.for.alpha.values.over.time=label.for.alpha.values.over.time,
    nonfunctional.covariate.comparisons = 	nonfunctional.covariate.comparisons,
    plot.parameters=plot.parameters,
    do.plots=do.plots,
    plot.confidence.intervals=plot.confidence.intervals,
    type1.error=type1.error,
    type2.error=type2.error,
    treatment.effect=treatment.effect,
    dropout.rate=dropout.rate,
    color.labels=color.labels,
    legend.labels=legend.labels,
    event.comparison.table=event.comparison.table,

    ###############
    number.of.simulations=number.of.simulations,
    n=n,
    functional.alpha.form =functional.alpha.form,


    simulate.high.censoring.rates=simulate.high.censoring.rates,
    pseudovalues.replaced.by.arbitrary.values =pseudovalues.replaced.by.arbitrary.values,

    simulated.parameters.common.for.all.studies=simulated.parameters.common.for.all.studies,
    simulated.random.effect.distribution =simulated.random.effect.distribution,
    unknown.random.effect.parameters=unknown.random.effect.parameters,
    simulated.z.covariate.distribution =simulated.z.covariate.distribution,
    unknown.z.covariate.parameters = unknown.z.covariate.parameters,
    simulated.x.covariate.distribution =simulated.x.covariate.distribution,
    unknown.x.covariate.parameters = unknown.x.covariate.parameters,
    simulated.censoring.depends.on.z = simulated.censoring.depends.on.z,
    simulated.random.effects.depends.on.covariates =simulated.random.effects.depends.on.covariates,
    simulated.censor.rate= simulated.censor.rate,
    simulate.data.with.random.effects = simulate.data.with.random.effects,

    functional.g.form =functional.g.form,
    event.coefficients=event.coefficients,
    study.coefficients= study.coefficients,
    functional.beta.intercept =functional.beta.intercept,
    functional.beta.coefficients=functional.beta.coefficients,
    number.of.functional.beta.coefficients.per.study = number.of.functional.beta.coefficients.per.study,
    maximum.number.of.functional.beta.coefficients = maximum.number.of.functional.beta.coefficients,
    a0=a0,
    study.random.effect.parameter=study.random.effect.parameter,
    random.effect.first.parameter=random.effect.first.parameter,
    random.effect.second.parameter=random.effect.second.parameter,
    random.effect.first.parameter2=random.effect.first.parameter2,
    random.effect.second.parameter2=random.effect.second.parameter2,
    random.effect.third.parameter=random.effect.third.parameter,
    mix_n =mix_n,
    z.covariate.first.parameter = z.covariate.first.parameter,
    z.covariate.second.parameter = z.covariate.second.parameter,
    x.covariate.first.parameter =x.covariate.first.parameter,
    x.covariate.second.parameter = x.covariate.second.parameter,
    estimation.when.censoring.depends.on.z = estimation.when.censoring.depends.on.z,
    use.bootstrap.variance=use.bootstrap.variance,
    number.of.studies=number.of.studies,
    functional.covariate.comparisons.for.sample.size=functional.covariate.comparisons.for.sample.size

  ))
}


#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+##
# Oct/11/19
# end of the simulation setting                                   ##
#
#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+##







#############################################################################################
## default.estimation.setting: function that creates default estimation settings for JPRAT ##
#############################################################################################

default.estimation.setting <- function(use.bootstrap.variance, number.of.bootstraps){ #use.bootstrap.variance

  ###########################################
  ## iseed: seed set for simulation study. ##
  ###########################################

  if(!exists(as.character(substitute(iseed)))){
    iseed <- 1
  }


  ## splines.constrained.at.0: to keep the design matrix correct, we need to have B(0)=0.
  if(!exists(as.character(substitute(splines.constrained.at.0)))){
    splines.constrained.at.0 <- TRUE
  }

  #+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+###+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+##
  #########################################################
  #these arguments are defined by users now   ##
  #########################################################
  ## check.study.equality: check if studies are similar? using number.of.bootstrapsstrap joint confidence interval.
  #if(!exists(as.character(substitute(check.study.equality)))){
  #  if(what.analyzed.separately=="none"){
  #    check.study.equality <- TRUE  ## Set to FALSE and simulations run faster since they don't run 100 number.of.bootstrapsstraps.
  #  } else {
  #    check.study.equality <- FALSE
  #  }
  #}
  ## what.analyzed.separately: study events and studies, or just events, or just studies, or none separately
  ## if(!exists(as.character(substitute(what.analyzed.separately)))){
  ##  what.analyzed.separately <- "none"
  ## }
  ## number.of.bootstraps: number of number.of.bootstrapsstrap replicates used to test equality of studies, 100
  #if(!exists(as.character(substitute(number.of.bootstraps)))){
  #  number.of.bootstraps <- 100
  #}
  ## estimated.parameters.common.for.all.studies: common alpha, beta across studies
  #if(!exists(as.character(substitute(estimated.parameters.common.for.all.studies)))){
  #  estimated.parameters.common.for.all.studies <- FALSE
  #}
  #+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+###+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+##

  #+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+###+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+##
  ##########################################################
  #See default.options.for.data.setting()                 ##
  #these arguments have defulat options in data analysis  ##
  ##########################################################
  # estimate.variances: variance is estimated using gamm4
  # if(!exists(as.character(substitute(estimate.variances)))){
  #  estimate.variances <- "est" # est, quant, none
  # }
  ## clusters.are.families: indicator if the clusters are families (i.e., each member in the cluster is different).
  ##  In this paper, the cluster within a study corresponds to events from the same individual.
  ##  That is, the cluster info is from the same individual.
  #if(!exists(as.character(substitute(clusters.are.families)))){
  #  clusters.are.families <- FALSE
  #}
  # glm.link.type : link to be used in GAMM
  #if(!exists(as.character(substitute(glm.link.type)))){
  #  glm.link.type <- "logit" ## "logit" for proportional odds model; "cloglog" for Cox PH model.
  #}
  #+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+###+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+##

  ## family.data: indicator if the clusters are families (i.e., each member in the cluster is different).
  ##  In this paper, the cluster within a study corresponds to events from the same individual.
  ##  That is, the cluster info is from the same individual.
  if(!exists(as.character(substitute(family.data)))){
    family.data <- FALSE
  }



  if(use.bootstrap.variance==TRUE){
    var.lo <- "varlo"##"number.of.bootstraps_varlo"
    var.hi <- "varhi"##"number.of.bootstraps_varlo"
  } else{
    var.lo <- "varlo"
    var.hi <- "varhi"
  }

  ## plot.kaplan.meier.curves: include Kaplan-Meier estimator in plots?
  ## This is to make Figure 1 in Biostatistics paper.
  if(!exists(as.character(substitute(plot.kaplan.meier.curves)))){
    plot.kaplan.meier.curves <- FALSE
  }

  list(#number.of.bootstraps =number.of.bootstraps,
    #check.study.equality=check.study.equality,
    #what.analyzed.separately =what.analyzed.separately,
    #estimated.parameters.common.for.all.studies =estimated.parameters.common.for.all.studies,
    #estimate.variances =estimate.variances,
    #glm.link.type = glm.link.type,
    #clusters.are.families =clusters.are.families,
    ##
    iseed=iseed,
    plot.kaplan.meier.curves=plot.kaplan.meier.curves,
    family.data=family.data,
    var.lo =var.lo,
    var.hi= var.hi,
    splines.constrained.at.0 = splines.constrained.at.0
  )
}




#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#########
## This code creates default settings for  how to plot and display results from
## the analysis.
#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#########

###########################################
# Oct/11/19                              ##
# We only consider use_real_data=TRUE,   ##
# No need to choose this option          ##
###########################################

default.results.setting <- function(#use_real_data,
                                    nonfunctional.covariate.values.for.prediction,
                                    functional.covariate.values.of.interest,
                                    parameter.label
                                    ){


  ## plotted.time.range.displayed: if we want to see results for only a specific subset of time points.
  if(!exists(as.character(substitute(plotted.time.range.displayed)))){
    plotted.time.range.displayed <- NULL
  }

  ## normalize.cag.repeat.values: we scale the CAG repeats to a uniform [0,1] scale
  if(!exists(as.character(substitute(normalize.cag.repeat.values)))){
    if(use_real_data==TRUE){
      normalize.cag.repeat.values <- TRUE
    } else{
      normalize.cag.repeat.values <- FALSE
    }
  }


  ## y-label for comparing studies (only used for clinical paper)
  ## if(!exists(as.character(substitute(ylabel.for.plots.comparing.studies)))){
  ##  ylabel.for.plots.comparing.studies <- ""
  ## }

  ## x-label for comparing studies (only used for clinical paper)
  ## if(!exists(as.character(substitute(xlabel.for.plots.comparing.studies)))){
  ##  xlabel.for.plots.comparing.studies <- ""
  ## }

  ## add a legend to display number at risk table (only used for clinical paper)
  ##if(!exists(as.character(substitute(ylabel.for.plots.comparing.studies)))){
  ##  add.number.at.risk.legend <- FALSE
  ##}


  ##################################
  ## Number of significant digits ##
  ##################################
  number.of.significant.digits <- 2



  ######################
  ## Plotting options ##
  ######################
  plot.nadaraya.watson <- FALSE	## plot.nadaraya.watson: do we produce Nadaraya-Watson estimator plots? (not used).
  use.bootstraps.confidence.interval <- FALSE	## use bootstrap confidence intervals?
  #if(use_real_data==FALSE){
  #  plotted.functional.alpha.range.displayed <- NULL
  #  plotted.functional.beta.range.displayed <- NULL
  #  legend.position.use <- "topleft"



  #} else {
    ## Do we show plots?
    number.of.significant.digits <- 2
    plotted.functional.beta.range.displayed <- NULL
    plotted.functional.alpha.range.displayed <- NULL
    legend.position.use <- "topright"

  #}


  #######################
  ## Ranges for y-axis ##
  #######################
  ## create a key for ylim settings
  ylim.key <- list(
    beta0 = c(-10,8),
    Z1 = c(-5,5),
    base_age=c(-1,1),
    base_age_quart=c(-1,1),
    base_age_kmeans=c(-1,1),
    educ_cat=c(-2,4),
    gender=c(-2,4),
    gamma=c(-5,10),
    omega=c(-15,10),
    alphax=c(-10,10),
    alphat=c(-10,10),
    Ft=c(0,1)
  )

  ylim.setting <- array(0,dim=c(length(parameter.label)+2+1,2),
                        dimnames=list(c(parameter.label,"alphax","alphat","Ft"),
                                      c("min","max")))

  ## print(rownames(ylim.setting))
  kk <- 1
  for(ii in 1:nrow(ylim.setting)){
    names.tmp <- rownames(ylim.setting)[ii]
    #print(names.tmp)
    if(names.tmp %in% names(ylim.key)){
      ##print(unlist(ylim.key[names.tmp]))
      ylim.setting[ii,] <- unlist(ylim.key[names.tmp])
      ##print(names.tmp)
    } else {

      names.tmp <- colnames(nonfunctional.covariate.values.for.prediction)[kk]
      ylim.setting[ii,] <- unlist(ylim.key[names.tmp])
      ##print(names.tmp)
      ##print(ylim.key[names.tmp])

      kk <- kk+1
    }
  }



  ####################################
  ## file labels for alpha(x,t) ##
  ####################################
  functional.covariate.values.of.interest.ci <- functional.covariate.values.of.interest
  #if(use_real_data==FALSE){
  #  alpha_label_x_direction <- functional.covariate.values.of.interest * 100
  #} else {
    alpha_label_x_direction <- 1:length(functional.covariate.values.of.interest)
  #}

  ###########################################
  ## axis-labels for plots with alpha(x,t) ##
  ###########################################
  #if(paper.type=="statistics" | use_real_data==FALSE){

  #  alphax.ylab <- ""
  #  alphax.xlab <- ""
  #  alphat.ylab <- ""
  #  alphat.xlab <- ""
  #} else {
    alphax.ylab <- "Effect on Outcome"
    alphax.xlab <- "CAG repeats"
    alphat.ylab <- "Effect on Outcome"
    alphat.xlab <- "Age (years)"
  #}

  ###########################################
  ## file labels for z-covariates ##
  ###########################################
  #if(use_real_data==FALSE){
  #  z_file_labels <- paste0("zz",1:nrow(nonfunctional.covariate.values.for.prediction))
  #} else {
    z_file_labels <- paste("zz",letters[1:nrow(nonfunctional.covariate.values.for.prediction)],sep="")

  #}

  ###########################
  ## cutoff for parameters ##
  ###########################
  alpha.cut <- NULL
  beta.cut <- NULL

  list(plotted.time.range.displayed =plotted.time.range.displayed,
       normalize.cag.repeat.values=normalize.cag.repeat.values,
       #ylabel.for.plots.comparing.studies =ylabel.for.plots.comparing.studies,
       #xlabel.for.plots.comparing.studies =xlabel.for.plots.comparing.studies,
       #add.number.at.risk.legend= add.number.at.risk.legend,
       alpha_label_x_direction=alpha_label_x_direction,
       plotted.functional.alpha.range.displayed=plotted.functional.alpha.range.displayed,
       plotted.functional.beta.range.displayed=plotted.functional.beta.range.displayed,
       number.of.significant.digits =number.of.significant.digits,
       legend.position.use=legend.position.use,
       alpha.cut=alpha.cut,
       beta.cut=beta.cut,
       alphax.ylab =alphax.ylab,
       #	alphax.ylab =alphax.ylab,
       alphax.xlab = alphax.xlab,
       alphat.ylab = alphat.ylab,
       alphat.xlab = alphat.xlab,
       plot.nadaraya.watson =plot.nadaraya.watson,
       use.bootstraps.confidence.interval =use.bootstraps.confidence.interval,
       z_file_labels =z_file_labels,
       ylim.setting=ylim.setting,
       ylim.key=ylim.key
  )


}








####################################################################################################
## default.real.data.setting: function that creates default settings to run JPRAT on one data set ##
####################################################################################################
## Input:
## - study.names: names of studies analyzed
##
## Output:
## - use_real_data: indicator that states whether or not this is real data. (default is TRUE)
## - number.of.simulations: default is 1.
## - use.functional.beta.intercept: do we include a functional intercept beta_es(t) (default is TRUE)
## - use.functional.event.coefficients: do we include a functional event-specific coefficient \gamma_e(t) (default is TRUE)
## - use.functional.study.coefficients: do we include a functional study-specific coefficient \omega_s(t) (default is TRUE)


##############################################
# Oct/11/19                                 ##
# We only consider use_real_data=TRUE       ##
#                                           ##
# These are user's choice (global objects)  ##
# use.functional.beta.intercept             ##
# use.functional.event.coefficients         ##
# use.functional.study.coefficients         ##
##############################################

#default.real.data.setting <- function(number.of.simulations){
#  use_real_data<-TRUE
#  number.of.simulations <- 1

#  use.functional.beta.intercept <- TRUE
#  use.functional.event.coefficients <- TRUE
#   use.functional.study.coefficients <- TRUE
#
#   return(list(
#     use_real_data=use_real_data,
#     number.of.simulations=number.of.simulations,
#     use.functional.beta.intercept=use.functional.beta.intercept,
#     use.functional.event.coefficients=use.functional.event.coefficients,
#     use.functional.study.coefficients=use.functional.study.coefficients
#   ))
#
# }
#



#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+###+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+##

#################################################################################################################
## default.options.for.data.setting: function that creates default settings to run JPRAT on one data set       ##
#################################################################################################################

#' default.options.for.data.setting
#'
#' @return this function returns default parameters setting to run JPRAT on one data set.
#' @export
#'
#' @examples
default.options.for.data.setting <- function(){ #use.bootstrap.variance


  ###########################################
  ## iseed: seed set for simulation study. ##
  ###########################################

  #if(!exists(as.character(substitute(iseed)))){
  #  iseed <- 1
  #}


  #################################################
  ## number.of.simulations: 1 for data analysis ###
  #################################################

  if(!exists(as.character(substitute(number.of.simulations)))){
    number.of.simulations <- 1
  }

  #############################
  ## Use only real data sets ##
  #############################

  if(!exists(as.character(substitute(use_real_data)))){
    use_real_data <- TRUE
  }

  #####################################################################################################################
  ## clusters.are.families: indicator if the clusters are families (i.e., each member in the cluster is different).  ##
  ##  In this paper, the cluster within a study corresponds to events from the same individual.                      ##
  ##  That is, the cluster info is from the same individual.                                                         ##
  #####################################################################################################################
  if(!exists(as.character(substitute(clusters.are.families)))){
    clusters.are.families <- FALSE
  }


  if(!exists(as.character(substitute(combine.data)))){
    combine.data <- FALSE
  }



  # estimate.variances: variance is estimated using gamm4
  if(!exists(as.character(substitute(estimate.variances)))){
    estimate.variances <- "est" # est, quant, none
  }






  # glm.link.type : link to be used in GAMM
  if(!exists(as.character(substitute(glm.link.type)))){
    glm.link.type <- "logit" ## "logit" for proportional odds model; "cloglog" for Cox PH model.
  }



  #####################################################
  ##  To get results                                 ##
  #####################################################

  ## estimated.parameters.common.for.all.studies: common alpha, beta across studies
  if(!exists(as.character(substitute(paper.type)))){
    paper.type <- "clinical"
  }

  if(!exists(as.character(substitute(plot.parameters)))){
    plot.parameters <- FALSE
  }

  #if(use.bootstrap.variance==TRUE){
  #  var.lo <- "varlo"##"number.of.bootstraps_varlo"
  #  var.hi <- "varhi"##"number.of.bootstraps_varlo"
  #} else{
  #  var.lo <- "varlo"
  #  var.hi <- "varhi"
  #}

  ## plot.kaplan.meier.curves: include Kaplan-Meier estimator in plots?
  ## This is to make Figure 1 in Biostatistics paper.
  #if(!exists(as.character(substitute(plot.kaplan.meier.curves)))){
  #  plot.kaplan.meier.curves <- FALSE
  #}

  list(
    use_real_data=use_real_data,
    number.of.simulations=number.of.simulations,
    estimate.variances=estimate.variances,
    glm.link.type = glm.link.type,
    clusters.are.families =clusters.are.families,
    paper.type=paper.type,
    plot.parameters=plot.parameters,
    combine.data=combine.data
    ##
    #iseed=iseed,
    #plot.kaplan.meier.curves=plot.kaplan.meier.curves,
    #family.data=family.data,
    #var.lo =var.lo,
    #var.hi= var.hi,
    #splines.constrained.at.0 = splines.constrained.at.0,
    #number.of.bootstraps =number.of.bootstraps,
    #check.study.equality=check.study.equality,
    #what.analyzed.separately =what.analyzed.separately,
    #estimated.parameters.common.for.all.studies =estimated.parameters.common.for.all.studies
  )

}

