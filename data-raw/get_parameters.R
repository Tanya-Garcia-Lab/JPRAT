##################################################
## get.empty.list: make empty lists for storage ##
##################################################


get.empty.list <- function(names){
  out <-  vector("list",length(names))
  names(out) <- names
  return(out)
}

length.apply <- function(x){
  return(length(x))
}


double.length.apply <- function(x){
  return(lapply(x,function(y) length(y)))
}



get.parameters<-function(param.setting){

    if(param.setting==1){

    method <- "gamm4"
    real_data <- FALSE
    num_study <- 2
    frform <- "norm"

    #This states that we generate data where the true model has shared study parameters ("Parameter Setting B" in the Biostatistics paper): TRUE
    #This states that we generate data where the true model has distinct study parameters ("Parameter Setting A" in the Biostatistics paper): FALSE
    common.param.data <- FALSE #TRUE
    #We do not analyze the data separately. Results when we estimated under Model A: a Joint model with distinct study parameters.
    analyze.separately <- "none"

    #Results when we estimated under Model A: a Joint model with distinct study parameters.
    common.param.estimation <- FALSE
    #defined as 30, but it is actually 40%. Check the output.
    censorrate <- rep(30,num_study)  ## actually it is 40, check output
    write.output<-TRUE

    }else{

    method <- "gamm4"
    real_data <- FALSE
    num_study <- 2
    frform <- "norm"

    censorrate <- rep(30,num_study)  ## actually it is 40, check output
    common.param.data <- FALSE

    common.param.estimation <- FALSE
    analyze.separately <- "study"
    write.output<-TRUE
    }

  if(!exists(as.character(substitute(iseed)))){
    iseed <- 1
  }


  ## type_fr: specification of number of unknown parameters in density
  ##   for random effect r (this is "v" in the paper). See make.normal() in main.R
  if(!exists(as.character(substitute(type_fr)))){
    type_fr <- "par2"
  }

  ## type_fzr: specification of number of unknown parameters in density for covariate Z.
  ##  see make.normal() in main.R
  if(!exists(as.character(substitute(type_fzr)))){
    type_fzr <- "none"
  }

  ## type_fx: specification of number of unknown parameters in density for covariate X.
  ## see make.normal() in main.R.
  if(!exists(as.character(substitute(type_fx)))){
    type_fx <- "none"
  }

  ## frform: specification of density for random effect R (this is "v" in the paper).
  # if(!exists(as.character(substitute(frform)))){
  #   frform <- "norm"
  # }

  ## fzrform: specification of density for covariate Z.
  if(!exists(as.character(substitute(fzrform)))){
    ##fzrform <- "binom"
    fzrform <- "unif" ##1/23/2017: "unif" is the default choice and what we use for results in paper.
    ##fzrform <- "norR"
    ##fzrform <- "uniR"
  }

  ## generate data so that censoring depends on z (when Z=binary)
  if(!exists(as.character(substitute(gen.cens.depend.z)))){
    gen.cens.depend.z <- TRUE
    if(fzrform!="binom"){
      gen.cens.depend.z <- FALSE ## we only have it setup for discrete Z
    }
  }

  ## fxform: specification of density for covariate X.
  if(!exists(as.character(substitute(fxform)))){

    fxform <- "unif"
    ##fxform <- "uniR"
    ##fxform <- "norR"
  }

  ## random effect R and covariates X,Z dependent?
   if(!exists(as.character(substitute(randomeffects.covariates.dependent)))){
     randomeffects.covariates.dependent <- FALSE
   }

  ## censorrate: censoring rate for the studies
   if(!exists(as.character(substitute(censorrate)))){
     censorrate <- rep(0,num_study)
   }

  ## use.random.effects: generate data with random effects
  if(!exists(as.character(substitute(use.random.effects)))){
    use.random.effects <- TRUE
  }

  # common.param.data: common alpha, beta across studies in data generation
  if(!exists(as.character(substitute(common.param.data)))){
    common.param.data <- FALSE
  }



  if(real_data==FALSE){

    ######################################
    ## Data setup for simulation study  ##
    ######################################

    #simus <- 1 			 ## number of simulations, 10
    study.names <- 1:num_study     ## names of the study


    if(num_study==1){		 ## sample size n for each study
      n <- 500
    } else if(num_study==2){
      n <- c(400,500)
    } else if(num_study==3){
      n <- c(300,400,500)
    }
    #n <- rep(500,num_study)
    #n <- rep(100,num_study)

    if(num_study==1){              ## censoring rates
      censorrate <- 30
      ##censorrate <- 0
    } else if(num_study==2){
      censorrate <- c(30,50)
      ##censorrate <- c(0,0)
    } else if(num_study==3){
      censorrate <- c(0,30,50)
    }


    nmax <- max(n)		 ## maximum sample size across all studies

    ######################
    ## Time points used ##
    ######################

    ## time points for estimation
    time_val <-  c(seq(40,49,by=1),seq(50,60,by=1))

    ## number of time points for evaluation
    num_time <- length(time_val)

    ## where to do predicted probabilities: Pr(T<t|T >= t0)
    ## time_choice.predicted, and time_choice.predicted + *.toadd must be in time_val
    # time_choice.predicted <- c(40,45)
    # time_choice.predicted.toadd  <- c(5,10)
    #
    # ## time points to show individual results at and label for alpha(x,t)
    # time_choice <- c(45,55)
    #alphat_lab <- time_choice

    ## 6/1/2017: time_choice no longer a list
    ##time_choice <-  get.empty.list(paste("study",1:num_study,sep=""))
    ##for(ss in 1:num_study){
    ##  time_choice[[ss]] <- c(45,55)
    ##}

    ##############
    ## X's used ##
    ##############

    ## x's where alpha(x,t) are evaluated at
     xks <- get.empty.list(paste("study",1:num_study,sep=""))
     for(ss in 1:num_study){
       xks[[ss]] <- seq(0,1,by=0.01)
     }

    ## x's to show results at. Common for ALL studies.
    # xx_choice <- c(0.5)

    ## alpha(x,t) label in x-dimension
    # alphax_lab <- xx_choice * 100

    ##########################
    ## alpha(x,t) functions ##
    ##########################

    ## axmod  and gtmod: alpha(x,t)=a0 * axmod() * gtmod()
    if(common.param.data==FALSE){ ## no common alpha, beta
      if(num_study==1){
        axmod <- list(study1=c("line","wah2"))
        gtmod <- "log"
      } else if(num_study==2){
        axmod <- list(study1=c("line","wah2"),
                      study2=c("wah2","line"))
        gtmod <- "log"
      } else if(num_study==3){
        axmod <- list(study1=c("cohort1","cohort2"),
                      study2=c("predict1","predict2"),
                      study3=c("pharos1","pharos2"))
        gtmod <- "noall"
      } else if(num_study==10){
        axmod <- list(study1=c("line","wah2"),
                      study2=c("line","wah2"),
                      study3=c("line","wah2"),
                      study4=c("line","wah2"),
                      study5=c("line","wah2"),
                      study6=c("line","wah2"),
                      study7=c("line","wah2"),
                      study8=c("line","wah2"),
                      study9=c("line","wah2"),
                      study10=c("wah2","line"))
        gtmod <- "log"
      }
    } else { ## common alpha, beta
      if(num_study==1){
        axmod <- list(study1=c("line","wah2"))
        gtmod <- "log"
      } else if(num_study==2){
        axmod <- list(study1=c("line","wah2"),
                      study2=c("line","wah2"))
        gtmod <- "log"
      } else if(num_study==3){
        axmod <- list(study1=c("all1","all2"),
                      study2=c("all1","all2"),
                      study3=c("all1","all2"))
        gtmod <- "all"
      }
    }

    ###########################
    ## Number of event types ##
    ###########################
    np <- max(unlist((lapply(axmod,length.apply))))   ## number of events (m_i in paper)


    ##################################
    ## True parameter values: gamma ##
    ##################################

    ## gamma_i
    if(np==1){
      ## gamma_i represents the difference between event i and (i-1)
      gamma.param <- 0
    } else if(np==2){
      gamma.param <- c(0,0.25)
    } else if(np==3){
      gamma.param <- c(0,0.5,1)
    }
    ##gamma.param <- NULL

    ##################################
    ## True parameter values: omega ##
    ##################################

    ## omega_s
    ## omega_s represents the difference between study s and (s-1)
    if(num_study==1){
      omega.param <- 0
    } else if(num_study==2){
      omega.param <- c(0,1)
    } else if(num_study==3){
      omega.param <- c(0,0.75,1.25)
    }

    if(common.param.data==TRUE){
      ## there are no differences between the studies.
      omega.param <- rep(0,num_study)
    }
    ##omega.param <- NULL

    if(common.param.estimation==TRUE){
      omega.param <- NULL
    }

    ####################################################
    ## True parameter values: beta0(t)=beta * gtmod() ##
    ####################################################

    ## beta0int: intercept
    beta0int <- 0.25
    ##beta0int <- NULL

    ##################################################
    ## True parameter values:beta(t)=beta * gtmod() ##
    ##################################################


    ## beta_is
    if(common.param.data==FALSE){ ## no common alpha,beta
      if(num_study==1){
        if(np==1){
          ##beta0 <- list(beta01=c(4,2,1))
          beta0 <- list(study1=list(beta01=c(4)))
        } else if(np==2){
          ##beta0 <- list(beta01=c(4,2,1),beta02=c(2,1,1))
          beta0 <- list(study1=list(beta01=c(4),beta02=c(2)))
        } else if(np==3){
          beta0 <-list(study1= list(beta01=c(4),beta02=c(0.5),beta03=c(2)))
        }
      } else if(num_study==2){
        if(np==1){
          ##beta0 <- list(beta01=c(4,2,1))
          beta0 <- list(study1=list(beta01=c(4)),
                        study2=list(beta01=c(2)))
        } else if(np==2){
          ##beta0 <- list(beta01=c(4,2,1),beta02=c(2,1,1))
          beta0 <- list(study1=list(beta01=c(1),beta02=c(0.25)),
                        study2=list(beta01=c(0.25),beta02=c(1)))
        } else if(np==3){
          ##beta0 <- list(beta01=c(4,2,1),beta02=c(2,1,1),beta03=c(3,1,2))
          beta0 <-list(study1= list(beta01=c(4),beta02=c(0.5),beta03=c(2)),
                       study2= list(beta01=c(2),beta02=c(1),beta03=c(0.5)))
        }
      } else if(num_study==3){
        if(np==1){
          beta0 <-list(study1= list(beta01=c(-0.25),beta02=c(0.25),beta03=c(0.35)))
        } else if(np==2){
          beta0 <- list(study1=list(beta01=c(-0.25),beta02=c(-0.25)),
                        study2=list(beta01=c(0.25),beta02=c(-0.15)),
                        study3=list(beta01=c(0.35),beta02=c(0.25)))
        } else if(np==3){
          beta0 <-list(study1= list(beta01=c(-0.25),beta02=c(-0.25),beta03=c(-0.25)),
                       study2= list(beta01=c(0.25),beta02=c(-0.15),beta03=c(0.45)),
                       study3= list(beta01=c(0.35),beta02=c(0.25),beta03=c(0.30)))
        }
      } else if(num_study==10){
        if(np==1){
          beta0 <-list(study1= list(beta01=c(-0.25),beta02=c(0.25),beta03=c(0.35)))
        } else if(np==2){
          beta0 <- list(study1=list(beta01=c(-0.25),beta02=c(-0.25)),
                        study2=list(beta01=c(0.25),beta02=c(-0.15)),
                        study3=list(beta01=c(0.35),beta02=c(-0.35)),
                        study4=list(beta01=c(0.15),beta02=c(-0.15)),
                        study5=list(beta01=c(0.45),beta02=c(-0.45)),
                        study6=list(beta01=c(0.20),beta02=c(-0.25)),
                        study7=list(beta01=c(0.30),beta02=c(-0.35)),
                        study8=list(beta01=c(0.40),beta02=c(-0.40)),
                        study9=list(beta01=c(0.10),beta02=c(-0.10)),
                        study10=list(beta01=c(0.50),beta02=c(-0.50)))
        } else if(np==3){
          beta0 <-list(study1= list(beta01=c(-0.25),beta02=c(-0.25),beta03=c(-0.25)),
                       study2= list(beta01=c(0.25),beta02=c(-0.15),beta03=c(0.45)),
                       study3= list(beta01=c(0.35),beta02=c(0.25),beta03=c(0.30)))
        }
      }
    } else { ## common alpha, beta
      if(num_study==1){
        if(np==1){
          ##beta0 <- list(beta01=c(4,2,1))
          beta0 <- list(study1=list(beta01=c(4)))
        } else if(np==2){
          ##beta0 <- list(beta01=c(4,2,1),beta02=c(2,1,1))
          beta0 <- list(study1=list(beta01=c(4),beta02=c(2)))
        } else if(np==3){
          ##beta0 <- list(beta01=c(4,2,1),beta02=c(2,1,1),beta03=c(3,1,2))
          beta0 <-list(study1= list(beta01=c(4),beta02=c(0.5),beta03=c(2)))
        }
      } else if(num_study==2){
        if(np==1){
          beta0 <- list(study1=list(beta01=c(2)),
                        study2=list(beta01=c(2)))
        } else if(np==2){
          beta0 <- list(study1=list(beta01=c(1),beta02=c(0.25)),
                        study2=list(beta01=c(1),beta02=c(0.25)))
        } else if(np==3){
          beta0 <-list(study1= list(beta01=c(3),beta02=c(0.5),beta03=c(2)),
                       study2= list(beta01=c(3),beta02=c(0.5),beta03=c(2)))
        }
      } else if(num_study==3){
        if(np==1){
          beta0 <- list(study1=list(beta01=c(2)),
                        study2=list(beta01=c(2)),
                        study3=list(beta01=c(2)))
        } else if(np==2){
          beta0 <- list(study1=list(beta01=c(-3),beta02=c(4)),
                        study2=list(beta01=c(-3),beta02=c(4)),
                        study3=list(beta01=c(-3),beta02=c(4)))
        } else if(np==3){
          beta0 <-list(study1= list(beta01=c(-0.25),beta02=c(0.5),beta03=c(0.75)),
                       study2= list(beta01=c(-0.25),beta02=c(0.5),beta03=c(0.75)),
                       study3= list(beta01=c(-0.25),beta02=c(0.5),beta03=c(0.75)))
        }
      }
    }

    # ###########################
    # ## dimension of beta: lb ##
    # ###########################
    # ## lb: list of beta_is dimensions for each study
     lb <- lapply(beta0,double.length.apply)
    #
    # ## lb.max: maximum dimension in lb
     lb.max <- max(unlist(lb))

    ## beta_lab: label of beta_is parameters
    # beta_lab <- lapply(lb,double.get.label)

    ########################################
    ## Where to evaluate z's at		##
    ## z.choice: number of z evaluations	##
    ########################################
    # z.choice <- 2
    # #z.choice <- 1

    ## zeval: where to evaluate z's at in each study
    # zeval <- array(0,dim=c(num_study,z.choice,np,lb.max),
    #                dimnames=list(
    #                  paste("ss",1:num_study,sep=""),
    #                  paste("zz",1:z.choice,sep=""),
    #                  paste("np",1:np,sep=""),
    #                  paste("lb",1:lb.max,sep="")))

    # if(fzrform=="unif"){
    #   zeval[,1,,] <- 0.50
    #   zeval[,2,,] <- 0.75
    # } else if(fzrform=="binom"){
    #   zeval[,1,,] <- 0
    #   zeval[,2,,] <- 1
    # }

    # ## label for z's
    #  z_lab <- c("1","2")
    #
    # ##zeval[,1,,] <- 1.5
    # ##z_lab <- c("1")


    ###################################################
    ## for loading fake data			   ##
    ## we use this "fake" data in real data analysis.##
    ###################################################
    xmin <- NULL
    xmax <- NULL
    # z_tmp <- NULL
    # x_tmp <- NULL
    # s_tmp <- NULL
    # delta_tmp <- NULL
    # ii.choose <- NULL

    #################
    ## For results ##
    #################
    # do.plots <- FALSE ## do we show plots?
    # conf.int.use <- TRUE  ## do we show CIs?
    # alpha.cut <- NULL
    # beta.cut <- NULL
    # ylim.set <- c(-5,5)	  ## y-axis limits for plots
    # ylim.x <- c(-8,8)
    # ylim.beta <- c(-8,8)
    # ylim.time <- c(-8,8)
    # ylim.Ft <- c(0,1.2)
    # round.set <- 2
    # xx_choice.ci <- xx_choice
    # time_choice.ci <- time_choice
    # list.name <- paste("np",1:np,sep="")
    # s.names <- list.name
    # s.names.short <- 1:np
    # legend.position.use <- "topleft"


  }

  #####################################################
  ## set terms needed whether real_data=TRUE or FALSE##
  #####################################################

  ## m: number of events in each study for each subject
  m <- array(0,dim=c(num_study,nmax),
             dimnames=list(
               paste("ss",1:num_study,sep=""),
               paste("n",1:nmax,sep="")))
  for(ss in 1:num_study){
    m[ss,1:n[ss]] <- rep(np,n[ss])
  }

  ## for kaplan-meier jack-knife implementation (these are all equal to np: number of events)
  maxm  <- max(m)
  p <- np
  # m0_qvs <- p

  ##########################################
  ## choice for a0: alpha(x,t)=a0*axmod() ##
  ##########################################

  geta0 <- function(axmod,study,common.param.data){
    if(axmod=="line"){
      if(common.param.data==TRUE){ # common alpha, beta
        a0 <- 2
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
        a0 <- 1
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

  a0 <- get.empty.list(paste("study",1:num_study,sep=""))
  for(ss in 1:num_study){
    a0.tmp <- NULL
    for(k in 1:np){
      a0.tmp <- c(a0.tmp,geta0(axmod[[ss]][[k]],ss,common.param.data))
    }
    a0[[ss]] <- a0.tmp
  }


  ###############
  ## for gamm4 ##
  ###############
  ## number of knots
  # get.knot <- function(axmod){
  #   if(axmod=="wah4" | axmod=="wah3" | axmod=="real"){
  #     a <- 8
  #   } else {
  #     a <- 5
  #   }
  #   return(a)
  # }

  ## knot length
  # knot.length <- NULL
  # for(k in 1:np){
  #   knot.length <- c(knot.length,get.knot(axmod[k]))
  # }

  ## dim of alpha(x,t) in x-dimension
  la <- 1

  ############################
  ## for ueffect (not used) ##
  ############################
  ##par_fu <- c(mean=0,sd=1.5) ## u random effect variance
  par_fu <- NULL


  ###################################################
  ## for density of random effect r ("v" in paper) ##
  ###################################################

  if(frform=="norm"){
    ## normal reffect
    par1_fr <- rep(0,num_study) # mean
    par2_fr <- rep(1,num_study) # sd
    par1_fr2<-NULL
    par2_fr2<-NULL
    par3_fr<-NULL
    mix_n<-NULL
  } else if(frform=="gamm"){
    ## gamma reffect
    par1_fr <- rep(1.5,num_study) # shape
    par2_fr <- rep(2.,num_study)  # scal
    par3_fr <- rep(1.5,num_study) ## u random effect variance
    mix_n<-NULL
  } else if(frform=="unif"){
    ## unif reffect
    par1_fr <- rep(-1,num_study) # low
    par2_fr <- rep(1,num_study)   # hi
    par3_fr <- rep(1.5,num_study) ## u random effect variance
    mix_n<-NULL
  } else if(frform=="tdis"){
    ## noncentral t reffect
    par1_fr <- rep(3,num_study) # df
    par2_fr <- rep(0,num_study)  # ncp
    par3_fr <- rep(1.5,num_study) ## u random effect variance
    mix_n<-NULL

  } else if(frform=="mixn"){
    ## mixture of normal reffect
    par1_fr <- rep(3,num_study)  # mean1
    par2_fr <- rep(1,num_study)    # sd1
    par1_fr2 <- rep(-3,num_study) # mean2
    par2_fr2 <- rep(0.25,num_study) # sd2
    mix_n <- rep(0.9,num_study)   # mix_n
    par3_fr <- rep(1.5,num_study) ## u random effect variance
  }


  ######################
  ## for density of Z ##
  ######################
  if(fzrform=="norm"){
    ## normal zeffect
    par1_fzr <- rep(0,num_study) # mean
    par2_fzr <- rep(1,num_study) # sd
  } else if(fzrform=="unif"){
    ## unif zeffect
    par1_fzr <- rep(0,num_study)  # low
    par2_fzr <- rep(1,num_study)  # hi
  } else if(fzrform=="binom"){
    ## binomial zefffect
    par1_fzr <- rep(1,num_study)   # size
    par2_fzr <- rep(0.5,num_study) # prob
  }

  ######################
  ## for density of X ##
  ######################
  if(fxform=="norm"){
    ## normal xeffect
    par1_fx <-rep(0,num_study) # mean
    par2_fx <- rep(1,num_study)  # sd
  } else {
    ## unif xeffect
    par1_fx <- rep(0,num_study)  # low
    par2_fx <- rep(1,num_study)  # hi
  }

  #########################
  ## for plotting output ##
  #########################
  # plot.nw <- FALSE	## plot.nw: do we produce Nadaraya-Watson estimator plots? (not used).
  # boot.ci <- FALSE	## use bootstrap confidence intervals?
  #
  #

list(#method=method, # not used for data setup
  iseed=iseed, # not used for data setup
  beta0int= beta0int,
  gamma.param=gamma.param,
  omega.param=omega.param,
  m=m, n=n,
  real_data=real_data,
  num_study=num_study,
  np=np,
  lb=lb,
  time_val=time_val,
  use.random.effects=use.random.effects,
  beta0=beta0,
  a0=a0,
  lb.max=lb.max,
  num_time=num_time,
  frform=frform,
  fzrform= fzrform,
  fxform=fxform,
  type_fr= type_fr,
  type_fzr=type_fzr,
  type_fx=type_fx,
  par_fu= par_fu,
  par1_fr=par1_fr,
  par2_fr=par2_fr,
  par1_fr2=par1_fr2,
  par2_fr2=par2_fr2,
  mix_n=mix_n,
  #par3_fr= par3_fr,
  par1_fzr=par1_fzr,
  par2_fzr=par2_fzr,
  par1_fx= par1_fx,
  par2_fx=par2_fx,

  axmod=axmod,
  gtmod=gtmod,
  censorrate=censorrate,
  gen.cens.depend.z=gen.cens.depend.z,
  #
  #common.param.data=common.param.data,
  # analyze.separately=analyze.separately,  # not used for data setup
  # common.param.estimation=common.param.estimation, # not used for data setup
  #
  #
  # arbitrary=arbitrary, # not used for data setup
  #
  randomeffects.covariates.dependent=randomeffects.covariates.dependent,
  ######################################
  ## Data setup for simulation study  ##
  ######################################
  #simus=simus,
  #study.names=study.names, ## num_study=2, c(400,500)
  #n=n,  ## num_study=2, c(400,500)
  #censorrate-censorrate, # c(30, 50)
  #nmax=nmax,
  #
  #time_choice.predicted=time_choice.predicted,
  #time_choice.predicted.toadd= time_choice.predicted.toadd,
  #time_choice=time_choice,
  #alphat_lab=alphat_lab,
  xks=xks,
  #xx_choice= xx_choice,
  #alphax_lab=alphax_lab,

  #beta_lab=beta_lab,
  #z.choice=z.choice,
  #zeval=zeval,
  #z_lab=z_lab,
  #xmin=xmin,
  #xmax=xmax,
  #z_tmp=z_tmp,
  #x_tmp=x_tmp,
  #s_tmp= s_tmp,
  #delta_tmp=delta_tmp,
  ##ii.choice=ii.choice,
  #do.plots=do.plots,
  #conf.int.use=conf.int.use,
  #alpha.cut=alpha.cut,
  #beta.cut=beta.cut,
  #ylim.set= ylim.set,
  #ylim.beta=ylim.beta,
  #ylim.time= ylim.time,
  #ylim.Ft=ylim.Ft,
  #round.set=round.set,
  #xx_choice.ci=xx_choice.ci,
  #time_choice.ci=time_choice.ci,
  #list.name=list.name,
  #s.names=s.names,
  #s.names.short=s.names.short, # not used for data setup
  #legend.position.use=legend.position.use, # not used for data setup
  #####################################################
  ## set terms needed whether real_data=TRUE or FALSE##
  #####################################################
  maxm=maxm,
  p=p,
  la=la,
  write.output=write.output
  #m0_qvs=m0_qvs, # not used for data setup
  ##########################################
  ## choice for a0: alpha(x,t)=a0*axmod() ##
  ##########################################

  #knot.length=knot.length,

  #par_fu=par_fu,

     )
}


