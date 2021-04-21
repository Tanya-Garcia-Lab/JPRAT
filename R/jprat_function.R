#' @title JPRAT algorithm
#' @description This function calls JPRAT algorithm and runs it.
#'
#'
#' @param study.names A character vector of name of studies used in the analysis. e.g., c("cohort", "predict", "pharos").
#' @param input.data.list A list of datasets to be analyzed. In the case, you use datasets given from JPRAT, then there are two options:
#' use list(cohort=data_cohort, predict=data_predict, pharos=data_pharos) if you choose \code{what.analyzed.separately="event" or "eventstudy"};
#' otherwise use list(study1=simu_data1_model_A, study2=simu_data2_model_A) (See \code{what.analyzed.separately="none" or "study"} for option details).
#'
#' @param nonfunctional.covariate.names A character vector for the name of the nonfunctional covariates that is used in the analysis.
#' @param functional.covariate.names A character vector for the name of the functional covariates that is used in the analysis.
#'
#' @param othercovariate.names A character vector for the name of other covariates, which are not to be analyzed but kept within data during the analysis procedure.
#' @param event.outcome.names A character vector for the name of the time-to-event of interest (outcomes).
#' @param delta.names A character vector for the name of the censoring indicator for the time-to-event outcomes.
#'
#' @param time.points.for.prediction A vector of time points at which the predicted values of marginal distribution will be estimated. Users may encounter a warning message that "Not enough data at time points (e.g., base_age);  May have divergent in the integration while bootstrapping." We recommend that users evaluate the proportional odds model at the average of nonfunctional covariate (e.g., base_age) across all subjects in each study. This may get rid of the warning messages.
#' @param time.points.for.conditional.prediction A vector of time points \eqn{t} at which the predicted values of conditional distribution \eqn{Pr(T <t + t_{0}| T>t)} will be estimated.
#' @param time.points.for.conditional.prediction.toadd A vector of future time points \eqn{t_{0}} at which the predicted values of conditional distribution \eqn{Pr(T <t + t_{0} | T>t)} will be estimated.
#' @param nonfunctional.covariate.value A numeric value interested for the nonfunctional covariate. e.g., 40 for \code{base_age}, which will be used for prediction.
#' @param functional.covariate.values.of.interest A vector of specific functional covariate values \eqn{X=x} where the smooth functional parameter \eqn{\alpha(X=x,t)} will be estimated. For example, we set \eqn{x=46,48,50} in the analysis. We recommend that the specific functional covariate values \eqn{X=x} should be chosen between the minimum and the maximum of functional covariate values. Otherwise, users may encounter a warning message.
#' @param number.of.bootstraps The number of bootstrap iterations. The bootstrap procedure is to do a hypothesis testing, which compares functional parameters over interested time points among studies.
#' @param use.functional.beta.intercept A logical value whether a functional intercept \eqn{\beta_{0}(t)} is included in the time-varying proportional odds model. The default option is TRUE.
#' @param use.functional.event.coefficients A logical value whether the event specific effect \eqn{\gamma_{e}(t)} will be used in the time-varying proportional odds model. The default option is TRUE.
#' @param use.functional.study.coefficients A logical value whether the study specific effect \eqn{\omega_{s}(t)} will be used in the time-varying proportional odds model. The default option is TRUE.
#' @param check.study.equality A logical value whether estimates are similar across studies: The default option is TRUE when what.analyzed.separately is "none"; otherwise, FALSE.
#' @param estimated.parameters.common.for.all.studies A logical value whether the model parameters are same across studies. The default option is FALSE.
#' @param what.analyzed.separately A character value to determine whether the event information from all studies is analyzed jointly or separately with distinct or shared study parameters in the model: the options are "studyevent" (studies and event), "study", "event", or "none".
#' @param estimation.when.censoring.depends.on.z A logical value whether the estimation process assumes that a censoring distribution depends on nonfunctional covariates \eqn{Z}. If covariates \eqn{Z} does not follow a binomial distribution, then the default option is FALSE.
#' @param use.bootstrap.variance  A logical value whether the bootstrap variances will be evaluated. The default option is TRUE.
#' @param estimate.variances A logical value how variances will be estimated. If this is TRUE, there are two options: "est" (estimated), and "quant" (quantiles from bootstrap).
#'                           If this is FALSE, the only option is  "none" (no variances are estimated). The default option is "est".
#' @param write.output A logical value whether outputs should be saved into data files. The default option is TRUE.
#'
#' @details The Joint Progression of Risk Assessment Tool (JPRAT) evaluates and compares all outcomes simultaneously
#'          across multiple studies and adjusts for outcome dependencies.
#'          The function \code{\link{jprat.wrapper}} is an estimation procedure that handles multiple data hierarchy
#'          using an additive logistic mixed effect model. The algorithm handles censoring with pseudo-values and
#'          functional covariate effects with splines.
#'          There are four options whether the event information from all studies is analyzed jointly or separately with distinct or shared study parameters in the model.
#'          If you choose
#'          \itemize{
#'          \item \code{what.analyzed.separately}="study", then JPRAT assumes separate study models, i.e., event information from each study is analyzed separately.
#'          \item \code{what.analyzed.separately}="event", then JPRAT analyzes the events separately and uses common parameters across all events.
#'          \item \code{what.analyzed.separately}="none",  then JPRAT does not analyze the data separately.
#'          \item \code{what.analyzed.separately}="studyevent", then JPRAT analyzes the evnts and sutdies separately.
#'}
#'
#'@references To see the detailed description for the JPRAT estimation procedure, we refer to
#'          \itemize{
#'          \item Garcia, T. P., Marder, K., Wang, Y. (2017). Time-varying proportional odds model for mega-analysis of clustered event
#'          times. Biostatistics, 20(1), 129-146.
#'          }
#'          To see the detailed real data analysis, we refer to
#'          \itemize{
#'          \item Garcia, T.P., Wang, Y., Shoulson, I., Paulsen, J.S. and Marder, K. (2018). Disease progression in Huntington disease:
#'          An analysis of multiple longitudinal outcomes. Journal of Huntington's disease, 7, 337-344}
#'
#' @return  A list of
#'          \item{jprat.output}{A list of estimated values. If write.output=TRUE, the list contains simulation results as data.theta and combi.out;
#'          \itemize{
#'          \item theta.out: a data frame of the estimated values for all components ("beta0", "beta1",  "alphas", "Ft", "Ft.predicted")
#'          at each iteration for the clinical events of interest per study. The columns of the data frame include iteration number (iters), the name of studies (study), the names of outcomes (event), the names for all components to be estimated (theta), the labels of the nonfunctional covariates Z (zz)),
#'          the labels of the functional covariates X (xx), all estimated values for theta: "est", "varest", "varlo", "varhi", "boot_varest", "boot_varlo", "boot_varhi" (val)
#'          and the time points.
#'          \item combi.out: a data frame of the difference of the estimated values between a pair of studies for all components ("beta0", "beta1", "alphas", "Ft", "Ft.predicted")
#'          at each iteration for the clinical events of interest per a pair of studies.
#'          The columns of the data frame include the number of iteration (iters), the names for the combinations of studies (study): cohort-predict, cohort-pharos, predict-pharos),
#'          the names of outcome events (event), the names for all components to be estimated (theta), the labels of the nonfunctional covariates Z (zz), the labels of the functional covariates X (xx),
#'          all estimated values for theta: "est", "varest", "varlo", "varhi", "boot_varest", "boot_varlo","boot_varhi" (val) and the time points.}
#'          If write.output=FALSE, jprat.output returns a list whose elements contain the estimated values for \eqn{beta(t)}, \eqn{alpha(X,t)}, \eqn{Ft(t)} and \eqn{F_{es}(t|X, Z)} for each study (see \code{null.theta.simus.est.ciboot} for the array from)
#'           and for a pair of studies for comparison (see \code{null.theta.simus.est.ci} for the array form).}
#'          \item{eflag}{an integer number to check if any error comes up while estimation algorithm is processed.
#'                       If this value is -1, then the marginal distribution \eqn{F_{es}(t|X, Z)} has missing values (NA).}
#'
#' @import mgcv
#'
#'
jprat.wrapper <- function(
  study.names,
  input.data.list,
  nonfunctional.covariate.names,
  functional.covariate.names,
  othercovariate.names,
  event.outcome.names,
  delta.names,
  time.points.for.prediction,
  time.points.for.conditional.prediction,
  time.points.for.conditional.prediction.toadd,
  nonfunctional.covariate.value,
  functional.covariate.values.of.interest,
  number.of.bootstraps,
  use.functional.beta.intercept,
  use.functional.event.coefficients,
  use.functional.study.coefficients,
  check.study.equality,
  estimated.parameters.common.for.all.studies,
  what.analyzed.separately,
  estimation.when.censoring.depends.on.z,
  use.bootstrap.variance,
  estimate.variances,
  write.output
){

  default.options<-default.options.for.data.setting()

  #####################################################################################
  ## Obtain default options : See function default.options.for.data.setting in       ##
  ## pseudo_default_setting_estimation_results.R                                     ##
  #####################################################################################


  ## default values
  glm.link.type<- default.options$glm.link.type
  clusters.are.families<-default.options$clusters.are.families
  use_real_data<-default.options$use_real_data

  ## reformatted.datasets
  reformatted.data<-data.reformatted.for.jprat.analysis(use_real_data,
                                                        study.names,
                                                        input.data.list,
                                                        time.points.for.prediction,
                                                        nonfunctional.covariate.names,
                                                        functional.covariate.names,
                                                        nonfunctional.covariate.value,
                                                        functional.covariate.values.of.interest)


  ## datasets are reformatted, so that jprat can use them for anlaysis
  data.sets.as.list<-reformatted.data$data.sets.as.list;
  nonfunctional.covariate.values.for.prediction<-reformatted.data$nonfunctional.covariate.values.for.prediction;
  xmin<-reformatted.data$xmin;
  xmax<-reformatted.data$xmax;
  functional.covariate.values.for.prediction<-reformatted.data$functional.covariate.values.for.prediction;




  ######################################################################
  ## Convert new variables to old variable names to use original code ##
  ######################################################################
  old.names <- convert.new.notation.to.old.for.jprat(study.names,
                                                     data.sets.as.list=data.sets.as.list,
                                                     time.points.for.prediction,
                                                     time.points.for.conditional.prediction,
                                                     time.points.for.conditional.prediction.toadd,
                                                     nonfunctional.covariate.names,
                                                     nonfunctional.covariate.values.for.prediction=nonfunctional.covariate.values.for.prediction,
                                                     functional.covariate.names,
                                                     functional.covariate.values.for.prediction=functional.covariate.values.for.prediction,
                                                     xmin=xmin,
                                                     xmax=xmax,
                                                     othercovariate.names,
                                                     event.outcome.names,
                                                     delta.names,
                                                     use_real_data=use_real_data,
                                                     use.functional.beta.intercept,
                                                     use.functional.event.coefficients,
                                                     use.functional.study.coefficients,
                                                     number.of.bootstraps,
                                                     check.study.equality,
                                                     what.analyzed.separately,
                                                     estimated.parameters.common.for.all.studies,
                                                     estimation.when.censoring.depends.on.z,
                                                     glm.link.type,
                                                     use.bootstrap.variance,
                                                     clusters.are.families
  )

  ############################
  ## Output to old notation ##
  ############################


  #######################################
  ##Input converted to old notation   ###
  ##used in jprat main estmiation     ###
  #######################################
  common.param.estimation <- old.names$common.param.estimation
  analyze.separately <- old.names$analyze.separately
  boot <- old.names$boot
  est.cens.depend.z <- old.names$est.cens.depend.z
  real_data <- old.names$real_data
  time_choice.predicted <- old.names$time_choice.predicted
  time_choice.predicted.toadd <- old.names$time_choice.predicted.toadd
  var.boot  <- old.names$var.boot
  arbitrary <- old.names$arbitrary
  beta0int <- old.names$beta0int
  a0 <- old.names$a0
  combi.study <- old.names$combi.study
  combi.choice <- old.names$combi.choice
  combi.names  <- old.names$combi.names
  compute.study.differences <- old.names$compute.study.differences
  family.data <- old.names$family.data
  gamma.param <- old.names$gamma.param
  la <- old.names$la
  lb <- old.names$lb
  knot.length <- old.names$knot.length
  link.type <- old.names$link.type
  m <- old.names$m
  method <- old.names$method
  maxm <- old.names$maxm
  num_study <- old.names$num_study
  num_time <- old.names$num_time
  n <- old.names$n
  np <- old.names$np
  num_xx <- old.names$num_xx
  nmax <- old.names$nmax
  omega.param <- old.names$omega.param
  param.label <- old.names$param.label
  par_fu <- old.names$par_fu
  spline.constrain <- old.names$spline.constrain
  time_val <- old.names$time_val
  xks  <- old.names$xks
  zeval  <- old.names$zeval
  z.choice  <- old.names$z.choice




  #######################################
  ##Input converted to old notation   ###
  ## obtain null array                ###
  #######################################
  s.names <- old.names$s.names
  simus <- old.names$simus
  theta.names <- old.names$theta.names
  theta.names.combi <- old.names$theta.names.combi
  x_lab_names  <- old.names$x_lab_names
  z_lab_names  <- old.names$z_lab_names

  ###################################################
  ## Form dataset for real data or simulated data  ##
  ###################################################
  axmod <- old.names$axmod
  beta0 <- old.names$beta0
  censorrate <- old.names$censorrate
  delta_tmp.list <- old.names$delta_tmp.list
  frform <- old.names$frform
  fzrform <- old.names$fzrform
  fxform <- old.names$fxform
  gtmod <- old.names$gtmod
  gen.cens.depend.z <- old.names$gen.cens.depend.z  #null
  mix_n <- old.names$mix_n
  s_tmp.list <- old.names$s_tmp.list
  type_fr <- old.names$type_fr
  use.random.effects  <- old.names$use.random.effects
  x_tmp.list  <- old.names$x_tmp.list

  ##############################
  # labeling for results data ##
  ##############################
  iseed <- old.names$iseed


  ################################################
  # Generating fixed effects for simulated data ##
  # (not used)                                  ##
  ################################################
  lb.max <- old.names$lb.max
  par1_fzr <- old.names$par1_fzr
  par2_fzr <- old.names$par2_fzr
  par1_fx <- old.names$par1_fx
  par2_fx <- old.names$par2_fx
  par1_fr <- old.names$par1_fr
  par2_fr <- old.names$par2_fr
  par1_fr2 <- old.names$par1_fr2
  par2_fr2 <- old.names$par2_fr2
  type_fzr <- old.names$type_fzr
  type_fx <- old.names$type_fx
  z_tmp.list  <- old.names$z_tmp.list



  #######################################
  ## get null theta arrays for storage ##
  #######################################

  ##all_null_theta : array to store all estimates. set to NULL values


  null.theta <- all_null_theta(theta.names,
                               study.names,
                               event.names=s.names,
                               z_lab_names,
                               x_lab_names,
                               label.dim.simus=simus,
                               label.name.simus=paste("iter",1:simus,sep=""),
                               time_val,
                               param.label,
                               time_choice.predicted,time_choice.predicted.toadd,
                               la
  )

  ## get combi null theta arrays
  combi.null.theta <- all_null_theta(theta.names.combi,
                                     study.names=combi.names,
                                     event.names=s.names,
                                     z_lab_names,
                                     x_lab_names,
                                     label.dim.simus=simus,
                                     label.name.simus=paste("iter",1:simus,sep=""),
                                     time_val,
                                     param.label,
                                     time_choice.predicted,time_choice.predicted.toadd,
                                     la)


  ## for bootstrap
  boot.null.theta <- all_null_theta(theta.names,
                                    study.names,
                                    event.names=s.names,
                                    z_lab_names,
                                    x_lab_names,
                                    label.dim.simus=boot,
                                    label.name.simus=paste("boot",1:boot,sep=""),
                                    time_val,param.label,
                                    time_choice.predicted,time_choice.predicted.toadd,
                                    la
  )

  boot.combi.null.theta <- all_null_theta(theta.names.combi,
                                          study.names=combi.names,
                                          event.names=s.names,
                                          z_lab_names,
                                          x_lab_names,
                                          label.dim.simus=boot,
                                          label.name.simus=paste("boot",1:boot,sep=""),
                                          time_val,param.label,
                                          time_choice.predicted,time_choice.predicted.toadd,
                                          la
  )


  ## estimation will be stored here: see while loop method=="gamm4"
  ## beta terms
  betaest.store <- null.theta$null.theta.simus.est.ciboot$beta

  ## alpha terms
  alphasest.store <- null.theta$null.theta.simus.est.ciboot$alpha

  ## Ft terms
  Ftest.store <- null.theta$null.theta.simus.est.ciboot$Ft




  if(num_study > 1){
    ## storage for comparing study differences
    betabootci.store <- combi.null.theta$null.theta.simus.est.ci$beta   ## for beta
    alphasbootci.store <- combi.null.theta$null.theta.simus.est.ci$alpha ## for alpha
    Ftbootci.store <- combi.null.theta$null.theta.simus.est.ci$Ft  ## for Ft
  } else {
    betabootci.store <- NULL
    alphasbootci.store <- NULL
    Ftbootci.store <- NULL
  }



  ## Ft predicted terms
  Ftest.predicted.store <-  null.theta$null.theta.simus.est.ciboot$Ft.predicted  ## NULL

  ## NOT USED
  alphasijest.store <- NULL
  alphasijest.var.store <- NULL
  alphasijbootci.store <- NULL
  Ftijest.store <- NULL
  Ftijest.var.store <- NULL
  Ftijbootci.store <- NULL


  ## where to store number of event times <=t0
  count.store <- NULL
  count.store.outside <- NULL


  ######################################################################
  ## get true values                                                  ##
  ## (only relevant for simulated data. Otherwise, this is not used)  ##
  ######################################################################
  ## sigmar may need EDITING if I change the way par2_fr,par_fu is formed.
  truth <-get.truth(combi.study,combi.choice,
    real_data,num_study,np,lb,num_time,
    beta0int,beta0,gamma.param,omega.param,
    time_val,time_choice.predicted,time_choice.predicted.toadd,
    num_xx,a0,axmod,la,xks,zeval,z.choice,
    sigmar=par2_fr[1],sigmau=par_fu["sd"],gtmod,use.random.effects,
    null.theta,combi.null.theta)

  betadiff.store <- truth$beta.diff
  alphasdiff.store <- truth$alphas.diff
  Ftdiff.store <- truth$Ft.diff


  ##############
  ## set seed ##
  ##############
  set.seed(iseed)
  #print(paste("iseed=",iseed,sep=""))

  ############################
  ## generate fixed effects ##
  ############################
  fixed.effects <- generate.fixed.effects(num_study,
                                          nmax,np,lb.max,n,lb,
                                          fzrform,par1_fzr,par2_fzr,type_fzr,
                                          fxform,par1_fx,par2_fx,type_fx,
                                          real_data,
                                          x_tmp.list,
                                          z_tmp.list)

  #+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#++#+#+#+#+#++#+#+#+#+#+
  #+
  #+
  #+               while Loop : main estimation and bootratp estimation
  #+
  #+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#++#+#+#+#+#++#+#+#+#+#+



  iters <-1
  ##iters <-10  ## for testing
  while(iters <= simus){
    ##################################
    ## get data (simulated or real) ##
    ##################################
    ## keep x, z fixed in each simulation

    ## form s, delta, Y terms (uncensored (1) or not (0), not observed (999) ) : starting data
    data <- simu.data.fixed.effects(
      x=fixed.effects$x,
      z=fixed.effects$z,
      delta_tmp.list,
      s_tmp.list,
      a0,
      axmod,
      num_time,
      censorrate,
      frform,
      type_fr,
      par1_fr,par2_fr,par1_fr2,par2_fr2,mix_n, par_fu, ## null
      real_data,
      time_val,
      beta0int,
      beta0,
      gamma.param,omega.param,
      n,
      nmax,
      m,
      maxm,
      la,lb,
      num_study,
      np,
      gtmod,
      use.random.effects,
      gen.cens.depend.z)



    #####################################
    # no examples but add documentations#
    #####################################
    jprat.out<-jprat.main.estimates(method="gamm4",
                                    compute.study.differences=compute.study.differences,
                                    var.boot=var.boot,
                                    ########################
                                    # For main estimation function
                                    ########################
                                    arbitrary,
                                    num_study,np,
                                    count.store,
                                    count.store.outside,
                                    data,
                                    num_time,
                                    time_val,
                                    time_choice.predicted,
                                    time_choice.predicted.toadd,
                                    n,nmax,m,maxm,
                                    la,lb,
                                    xks,
                                    truth,
                                    num_xx,
                                    knot.length,
                                    real_data,
                                    family.data,
                                    zeval,
                                    z.choice,
                                    param.label,
                                    beta0int,
                                    gamma.param,omega.param,
                                    spline.constrain,
                                    common.param.estimation,
                                    est.cens.depend.z,
                                    par_fu,
                                    analyze.separately,
                                    link.type,
                                    null.theta,
                                    z.proportions=NULL,
                                    ###########################
                                    ###For jprat estimation ###
                                    ###########################
                                    betaest.store,
                                    alphasest.store,
                                    Ftest.store,
                                    Ftest.predicted.store,
                                    ###########################
                                    #For bootstraps estimation#
                                    ###########################
                                    combi.study,
                                    combi.choice,
                                    combi.names,
                                    boot=boot,
                                    boot.null.theta=boot.null.theta,
                                    boot.combi.null.theta=boot.combi.null.theta,
                                    betabootci.store=betabootci.store,
                                    alphasbootci.store=alphasbootci.store,
                                    Ftbootci.store=Ftbootci.store,
                                    #######################
                                    # iteration
                                    #######################
                                    iters=iters
    )


    iters<-jprat.out$iters+1



  }##end while loop

  eflag<-jprat.out$eflag



  ## array results from jprat
  betaest.store<-jprat.out$betaest.store
  alphasest.store<-jprat.out$alphasest.store
  Ftest.store<-jprat.out$Ftest.store
  Ftest.predicted.store<-jprat.out$Ftest.predicted.store
  betabootci.store<-jprat.out$betabootci.store
  alphasbootci.store<-jprat.out$alphasbootci.store
  Ftbootci.store<-jprat.out$Ftbootci.store


  #+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#++#+#+#+#+#++#+#+#+#+#+
  #+
  #+
  #+                End of Test while Loop : main estimation and bootratp estimation
  #+
  #+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#++#+#+#+#+#++#+#+#+#+#+



  ###########################
  ## write data for output ##
  ###########################
   ## if write.output=TRUE, jprat.output returns contains the list of theta.out and combi.out
   ## theta.out: dataframe of beta, alphas, Ft, Ft.predicted
   ## combi.out: dataframe of betabootci.store, alphasbootci.store, Ftbootci.store
   ## if write.output=FALSE, jprat.output returns, which contains arrays such as betaest.store,
   ## alphaest.store, Ftest.store, Ftest.predicted.store, and betabootci.store, alphasbootci.store, and Ftbootci.store

  if(write.output==TRUE){

  ## get all dimension to save results with their names: beta, alphas, Ft, Ft.predicted
  dim.order.all <- get.all.dim.order(theta.names)
  ## theta.names: Ft, Ft.predicted
  flatten.theta.name <- get.flatten.theta.name(theta.names)

  ## re-order columns of truth
  if(!is.null(time_choice.predicted)){

    truth.colm.names <- c("study","event","theta","zz","xx","time0",
                          paste("t",time_val,sep=""))
    theta.colm.names <- c("iters","study","event","theta","zz","xx","val","time0",
                          paste("t",time_val,sep=""))

  } else{

    truth.colm.names <- c("study","event","theta","zz","xx",
                          paste("t",time_val,sep=""))
    theta.colm.names <- c("iters","study","event","theta","zz","xx","val",
                          paste("t",time_val,sep=""))

  }

  combi.colm.names <- c("iters","study","event","theta","zz","xx","val",
                        paste("t",time_val,sep=""))


  ###########
  ## truth ##
  ###########
  ## flatten along time components
  truth.out.tmp <- get.theta.output(all.est.store=truth,
                                    dim.order=dim.order.all$dim.order,
                                    flatten.name="time",
                                    flatten.theta.name=flatten.theta.name,
                                    extension=".true",
                                    theta.names)

  truth.out <- merge.by.columns(truth.out.tmp)
  truth.out <- truth.out[,truth.colm.names]


  #######################################
  ### return restuls from jpart.out()  ##
  #######################################

  theta.est.list <- list(beta.tmp=betaest.store,
                         alphas.tmp=alphasest.store,
                         Ft.tmp=Ftest.store,
                         Ft.predicted.tmp=Ftest.predicted.store)


  ## change the above estimation list
  theta.out.all <- get.theta.output(all.est.store=theta.est.list,
                                    dim.order=dim.order.all$dim.order.simus.ci,
                                    flatten.name="time",
                                    flatten.theta.name=flatten.theta.name,
                                    extension=".tmp",
                                    theta.names)

  theta.out <- merge.by.columns(theta.out.all)
  theta.out <- theta.out[,theta.colm.names]


  #####################
  ## Compare studies ##
  #####################
  if(num_study > 1){

    combi.theta.est.list <- list(beta.tmp=betabootci.store,
                                 alphas.tmp=alphasbootci.store,
                                 Ft.tmp=Ftbootci.store)

    combi.theta.out.all <- get.theta.output(all.est.store=combi.theta.est.list,
                                            dim.order=dim.order.all$dim.order.simus.ci,
                                            flatten.name="time",
                                            flatten.theta.name=flatten.theta.name,
                                            extension=".tmp",
                                            theta.names.combi)

    combi.out <- merge.by.columns(combi.theta.out.all)
    combi.out <- combi.out[,combi.colm.names]


  } else{

    combi.out <- 0
  }


  #########################
  ## filename for output ##
  #########################
  filename <- paste("output_iseed_",iseed,".dat",sep="")
  filename <- paste("real_",filename,sep="")

  write.table(theta.out, paste("out_thetaest_",filename,sep=""), col.names=FALSE, row.names=FALSE)
  write.table(combi.out, paste("out_combi_",filename,sep=""),col.names=FALSE,row.names=FALSE,na="0")

  jprat.output<- list(data.theta=theta.out, data.combi=combi.out)

  }else{

      jprat.output=list(
      beta.array=betaest.store,
      alpha.array=alphasest.store,
      Ft.array=Ftest.store,
      Ft.predicted.array=Ftest.predicted.store,
      beta.diff.array=betabootci.store,
      alpha.diff.array=alphasbootci.store,
      Ft.diff.array=Ftbootci.store
    )

  }




  list(jprat.output=jprat.output,
       eflag=eflag)
}
