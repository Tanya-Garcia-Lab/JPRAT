#' @title Initialization of parameters and coefficients
#' @description This function is to initialize a list of all components to be estimated to zeros. The structure of each component is a multi-dimensional array, whose dimension size depends on the number of studies, the number of events,
#' the number of covariates, and the length of time points where estimates will be evaluated, etc.
#'
#' @param theta.names A character vector of functional parameters' names that will be estimated in the model: "beta" (include intercept), "alphas" and "Ft" (or "Ft.predicted") corresponds to
#'                     \eqn{\beta_{es}(t)} (\eqn{\beta_0(t)}) ,  \eqn{\alpha(X, t)}, and \eqn{F_{es}(t)}.
#' @inheritParams jprat.wrapper
#' @param event.names See the argument \code{event.outcome.names} in the \code{\link{jprat.wrapper}} function.
#' @param z_lab_names A vector of character values for the names of nonfunctional covariates Z.
#' @param x_lab_names  A vector of character values for the names of functional covariates X in the functional coefficients \eqn{\alpha(X, t)}.
#' @param label.dim.simus The number of simulation runs (or bootstrap iteration runs), which determines the length of the dimension when extra dimensions are added to store the simulation results.
#' @param label.name.simus A character string of names that will be used to label the dimensions (in row wise) whose lengths are given by \code{label.dim.simus} when extra dimensions are added to store the simulation results.
#' @param time_val See the argument \code{time.points.for.prediction} in the \code{\link{jprat.wrapper}} function.
#' @param param.label A character vector of the names for all coefficients of nonfunctional covariate Z in the model.
#' @param time_choice.predicted See the argument \code{time.points.for.conditional.prediction} in the \code{\link{jprat.wrapper}} function.
#' @param time_choice.predicted.toadd See the argument \code{time.points.for.conditional.prediction.toadd} in the \code{\link{jprat.wrapper}} function.
#' @param la A length of smooth functional parameters \eqn{\alpha(X,t)} in the model. The default value is 1 in the data analysis study.
#'
#' @details A list of zero arrays will be created to store those estimates of \eqn{\beta_{es}(t)} (\eqn{\beta_0(t)}) ,  \eqn{\alpha(X, t)}, and \eqn{F_{es}(t)} in the time varying, proportional odds model.
#' @references Garcia, T.P., Marder, K., and Wang, Y. (2019). Time-varying proportional odds model for mega-analysis of clustered event times. Biostatistics, 20(1), 129-146.
#' @return A list of
#'
#'\item{null.theta}{A list of all estimated parameters and coefficients ("beta", "alphas", "Ft" and ``Ft.predicted") in the model.
#'                  Each element of the list contains arrays of zeros at the specific time points, the values of covariates and the event of interest in each study,
#'                  whose dimension determined by the length of studies, events, time points
#'                  where prediction occurs, and coefficients of the nonfunctional covariates.}
#'
#'
#'\item{null.theta.simus}{A list of all estimated parameters and coefficients ("beta", "alphas", "Ft" and ``Ft.predicted"), where each element of the list contains arrays of zeros.
#'  Each element has an additional dimension as the first dimension of each array, which is extended arrays from \code{null.theta}
#'  The number of simulations determines the length of the additional dimension (\code{label.dim.simus}) with its dimension names (\code{label.names.simus}) as a character string.}
#'
#'
#'\item{null.theta.ci}{A list of all estimated parameters and coefficients ("beta", "alphas", "Ft" and ``Ft.predicted"), where each element of the list contains arrays of zeros.
#', where three dimensions for the estimated variance, the lower and upper bounds of the confidence intervals were added to the last dimensions of arrays in \code{null.theta}.}
#'
#'
#'\item{null.theta.est.ci}{A list of all estimated parameters and coefficients ("beta", "alphas", "Ft" and ``Ft.predicted"), where each element of the list contains arrays of zeros.
#' Four dimensions for the estimates, the estimated variance, the lower and upper bounds of the confidence intervals were added to the last dimensions of arrays in \code{null.theta}.}
#'
#'
#'\item{null.theta.simus.ci}{
#'A list of all estimated parameters and coefficients ("beta", "alphas", "Ft" and ``Ft.predicted"),
#'where each element of the list contains arrays of zeros. Three dimensions for the estimated variance, the lower and upper bounds of the confidence intervals
#'were added to the last dimensions of arrays in \code{null.theta.simus}.}
#'
#'\item{null.theta.simus.est.ci}{A list of all estimated parameters and coefficients ("beta", "alphas", "Ft" and ``Ft.predicted"),
#'where each element of the list contains arrays of zeros. Four dimensions for the estimates, the estimated variance, the lower and upper bounds of the confidence intervals
#'were added to the last dimensions of arrays in \code{null.theta.simus}.}
#'
#'
#'\item{null.theta.simus.est.ciboot}{A list of all estimated parameters and coefficients ("beta", "alphas", "Ft" and ``Ft.predicted"),
#'where each element of the list contains arrays of zeros. Seven dimensions for the estimates, the estimated variance, the lower and upper bounds of the confidence intervals,
#'the estimated bootstrap variances, the lower and upper bounds of bootstrap confidence intervals were added to the last dimensions of arrays in \code{null.theta.simus}.}
#'
#'
#' @examples
#'
#'
#' theta.names=c("beta", "alphas", "Ft")
#' study.names=c("cohort", "predict", "pharos")
#' event.names=c("hdage_nobase", "mcione", "dep2");
#' z_lab_names=c("base_age");
#' x_lab_names=c(40, 46);
#' simus=5;
#' label.name.simus=paste("iter",1:simus,sep="")
#' time_val=seq(40, 100, by=5)
#' param.label=c("beta0", "beta1");
#' time_choice.predicted=NULL;
#' time_choice.predicted.toadd=NULL;
#' la=1
#'
#' null.theta <- all_null_theta(theta.names, study.names, event.names=event.names,
#'                              z_lab_names, x_lab_names, label.dim.simus=simus,
#'                                label.name.simus=label.name.simus, time_val, param.label,
#'                                time_choice.predicted, time_choice.predicted.toadd, la)
#'
all_null_theta <- function(theta.names,
                           study.names,
                           event.names,
                           z_lab_names,
                           x_lab_names,
                           label.dim.simus,label.name.simus,
                           time_val,param.label,
                           time_choice.predicted,time_choice.predicted.toadd,
                           la){


  ## obtain null array for theta.names (beta, alphas, Ft)
  null.theta <- get.null.theta(theta.names,
                               study.names,
                               event.names,
                               z_lab_names,
                               x_lab_names,
                               time_val,param.label,
                               time_choice.predicted,
                               time_choice.predicted.toadd,la)

  ## add simus layer
  null.theta.simus <- add.dimension.null(null.theta,location="first",
                                         label.dim=label.dim.simus,label.name=label.name.simus)

  ## ci results layer
  null.theta.ci <-  add.dimension.null(null.theta,location="last",
                                       label.dim=3,label.name=c("varest","varlo","varhi"))

  ## est, ci results
  null.theta.est.ci <-  add.dimension.null(null.theta,location="last",
                                           label.dim=4,label.name=c("est","varest","varlo","varhi"))   # estimation for each time point


  ## simus and ci results layer to null.theta
  null.theta.simus.ci <-  add.dimension.null(null.theta.simus,location="last",
                                             label.dim=3,label.name=c("varest","varlo","varhi"))

  ## simus, est, ci results
  null.theta.simus.est.ci <- add.dimension.null(null.theta.simus,location="last",
                                                label.dim=4,label.name=c("est","varest","varlo","varhi"))

  ## simus, est, ci and ciboot  results
  null.theta.simus.est.ciboot <- add.dimension.null(null.theta.simus,location="last",
                                                    label.dim=7,label.name=c("est","varest","varlo","varhi",
                                                                             "boot_varest","boot_varlo","boot_varhi"))

  list(null.theta=null.theta,
       null.theta.ci=null.theta.ci,
       null.theta.est.ci =null.theta.est.ci,
       null.theta.simus=null.theta.simus,
       null.theta.simus.ci=null.theta.simus.ci,
       null.theta.simus.est.ci=null.theta.simus.est.ci,
       null.theta.simus.est.ciboot=null.theta.simus.est.ciboot
       )
}



#' @title JPRAT algorithm
#' @description This function is to call and run JPRAT algorithm.
#'
#' @param study.names A character vector of name of studies used in the analysis. e.g., c("cohort", "predict", "pharos").
#' @param input.data.list Lists of datasets to be analyzed. In the case, you use datasets given from JPRAT, then there are two options:
#' use list(cohort=data_cohort, predict=data_predict, pharos=data_pharos) if you choose \code{what.analyzed.separately}="event" or "eventstudy";
#' otherwise use list(study1=simu_data1_model_A, study2=simu_data2_model_A). (See \code{what.analyzed.separately} for option details.)
#'
#' @param nonfunctional.covariate.names A character vector for the name of the nonfunctional covariates that is used in the analysis.
#' @param functional.covariate.names A character vector for the name of the functional covariates that is used in the analysis.
#'
#' @param othercovariate.names A character vector for the name of other covariates, which are not to be analyzed but kept within data during the analysis procedure.
#' @param event.outcome.names A character vector for the name of the time-to-event of interest (outcomes).
#' @param delta.names A character vector for the name of the censoring indicator for the time-to-event outcomes.
#'
#' @param time.points.for.prediction A vector of time points at which the predicted values of marginal distribution will be estimated. Users may encounter a warning message that "Not enough data at time points (e.g., base_age);  May have divergent in the integration while bootstrapping." We recommend that users evaluate the proportional odds model at the average of nonfunctional covariate (e.g., base_age) across all subjects in each study. This may get rid of the warning messages.
#' @param time.points.for.conditional.prediction A vector of time points \eqn{t} at which the predicted values of conditional distribution \eqn{Pr(T <t + t_0| T>t)} will be estimated.
#' @param time.points.for.conditional.prediction.toadd A vector of future time points \eqn{t_0} at which the predicted values of conditional distribution \eqn{Pr(T <t + t_0 | T>t)} will be estimated.
#' @param nonfunctional.covariate.value A numeric value interested for the nonfunctional covariate. e.g., 40 for \code{base_age}, which will be used for prediction.
#' @param functional.covariate.values.of.interest A vector of specific functional covariate values \eqn{X=x} where the smooth functional parameter \eqn{\alpha(X=x,t)} will be estimated. For example, we set \eqn{x=46,48,50} in the analysis. We recommend that the specific functional covariate values \eqn{X=x} should be chosen between the minimum and the maximum of functional covariate values. Otherwise, users may encounter a warning message.
#' @param number.of.bootstraps The number of bootstrap iterations. The bootstrap procedure is to do a hypothesis testing, which compares functional parameters over interested time points among studies.
#' @param use.functional.beta.intercept A logical value whether a functional intercept \eqn{\beta_0(t)} is included in the time-varying proportional odds model. The default option is TRUE.
#' @param use.functional.event.coefficients A logical value whether the event specific effect \eqn{\gamma_e(t)} will be used in the time-varying proportional odds model. The default option is TRUE.
#' @param use.functional.study.coefficients A logical value whether the study specific effect \eqn{\omega_s(t)} will be used in the time-varying proportional odds model. The default option is TRUE.
#' @param check.study.equality A logical value whether estimates are similar across studies: The default option is TRUE when what.analyzed.separately is "none"; otherwise, FALSE.
#' @param estimated.parameters.common.for.all.studies A logical value whether the model parameters are same across studies. The default option is FALSE.
#' @param what.analyzed.separately A character value to determine whether analysis will be performed separately or jointly: the options are "studyevent" (studies and event), "study", "event", or "none".
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
#'          Garcia, T. P., Marder, K., Wang, Y. (2017). Time-varying proportional odds model for mega-analysis of clustered event
#'          times. Biostatistics, 20(1), 129-146.
#'          To see the detailed real data analysis, we refer to
#'          Garcia, T.P., Wang, Y., Shoulson, I., Paulsen, J.S. and Marder, K. (2018). Disease progression in Huntington disease:
#'          An analysis of multiple longitudinal outcomes. Journal of Huntington's disease, 7, 337-344
#'
#' @return  A list of
#'
#'
#'              \item{jprat.output}{the list of estimated values. If write.output=TRUE, the list contains data.theta and combi.out, where theta.out is the data frame of the estimated values for all components ("beta0", "beta1",  "alpha1", "Ft", "Ft.predicted")
#'                           at each iteration for the clinical events of interest per study. The columns of the data frame include iters (iteration number), study(names of studies), event (the names of outcome events), theta (names for all components to be estimated), zz (the labels of the nonfunctional covariates Z),
#'                           xx (the labels of the functional covariates X), val (all estimated values for theta: "est”, "varest”, "varlo”, "varhi”, "boot_varest”, "boot_varlo”, "boot_varhi”)
#'                           and the flatten time points, and combi.out is a data frame of the difference of the estimated values between a pair of studies for all components ("beta0", "beta1", "alpha1", "Ft", "Ft.predicted")
#'                           at each iteration for the clinical events of interest per a pair of studies.
#'                           The columns of the data frame include iters (iteration number), study(names for the combinations of studies: cohort-predict, cohort-pharos, predict-pharos),
#'                           event (the names of outcome events), theta (names for all components to be estimated), zz (the labels of the nonfunctional covariates Z), xx (the labels of the functional covariates X),
#'                           val (all estimated values for theta: "est", "varest", "varlo", "varhi", "boot_varest", "boot_varlo",
#'                           "boot_varhi") and the flatten time points.}
#'          \item{eflag}{an integer number to check if any error comes up while processing the estimation algorithm.
#'                       If this value is -1, then the marginal distribution \eqn{F_{es}(t|X, Z)} has missing values (NA).}
#'
#' @import mgcv
#' @examples
#'
#' ## See more examples man/example.R
#'
#' ## Four options whether study and event are analyzed separately:
#' ## what.analyzed.separately="studyevent", "event", "study" or "none"
#'
#' what.analyzed.separately= "studyevent"
#'
#' ## Load your data in the list format: should match order of study.names
#' ## Choose different list of data depending on the settings such as
#'
#' input.data.list=list(cohort=data_cohort, predict=data_predict, pharos=data_pharos);
#' ####################################
#' # For JPRAT estimation: Input data #
#' ####################################
#'
#' study.names=c("cohort", "predict", "pharos");
#' othercovariate.names=c("firstyear", "lastyear");
#' event.outcome.names=c("hdage_nobase", "mcione", "dep2");
#' delta.names=c("delta.hdage_nobase", "delta.mcione", "delta.dep2");
#' nonfunctional.covariate.names=c("base_age");
#' functional.covariate.names="CAG";
#'
#' time.points.for.prediction=seq(46, 66, by=5);
#' time.points.for.conditional.prediction=c(46,51, 56);
#' time.points.for.conditional.prediction.toadd=c(5);
#' nonfunctional.covariate.value=c(40);
#' functional.covariate.values.of.interest=c(46, 48, 50) ;
#'
#' number.of.bootstraps=10;
#' use.functional.beta.intercept= TRUE ;
#' use.functional.event.coefficients= TRUE;
#' use.functional.study.coefficients=TRUE;
#' check.study.equality=FALSE
#' estimated.parameters.common.for.all.studies=FALSE
#'
#' use.bootstrap.variance=TRUE;
#' estimation.when.censoring.depends.on.z=FALSE ;
#' write.output=TRUE;
#'
#' #############################################
#' ## a function to estimate JPRAT algorithm  ##
#' #############################################
#'
#' jprat.estimat.results<-jprat.wrapper(study.names=study.names,
#'    input.data.list=input.data.list,
#'    nonfunctional.covariate.names=nonfunctional.covariate.names,
#'    functional.covariate.names=functional.covariate.names,
#'    othercovariate.names=othercovariate.names,
#'    event.outcome.names=event.outcome.names,
#'    delta.names=delta.names,
#'    time.points.for.prediction=time.points.for.prediction,
#'    time.points.for.conditional.prediction=time.points.for.conditional.prediction,
#'    time.points.for.conditional.prediction.toadd=time.points.for.conditional.prediction.toadd,
#'    nonfunctional.covariate.value=nonfunctional.covariate.value,
#'    functional.covariate.values.of.interest=functional.covariate.values.of.interest,
#'    number.of.bootstraps=number.of.bootstraps,
#'    use.functional.beta.intercept=use.functional.beta.intercept,
#'    use.functional.event.coefficients=use.functional.event.coefficients,
#'    use.functional.study.coefficients=use.functional.study.coefficients,
#'    check.study.equality=check.study.equality,
#'    estimated.parameters.common.for.all.studies=estimated.parameters.common.for.all.studies,
#'    what.analyzed.separately=what.analyzed.separately,
#'    estimation.when.censoring.depends.on.z=estimation.when.censoring.depends.on.z,
#'    use.bootstrap.variance=use.bootstrap.variance, write.output=write.output)
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


  ###############################################################################
  ## Data arguments are reformatted:                                           ##
  ##                         see data.reformatted.for.jprat.analysis in main.R ##
  ## added on 10/08/19                                                         ##
  ###############################################################################

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
                                                     #functional.covariate.values.of.interest,
                                                     #functional.covariate.values.of.interest.ci,
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
                                                     #estimate.variances,
                                                     estimation.when.censoring.depends.on.z,
                                                     glm.link.type,
                                                     use.bootstrap.variance,
                                                     clusters.are.families
                                                     ##########################
                                                     ## For plotting results ##
                                                     ##########################
                                                     #time.points.of.interest,
                                                     #time.points.of.interest.ci,
                                                     #label.for.alpha.values.over.time,
                                                     #nonfunctional.covariate.comparisons,
                                                     #plot.parameters,
                                                     #do.plots,
                                                     #plot.confidence.intervals,
                                                     #color.labels,
                                                     #legend.labels,
                                                     #event.comparison.table,
                                                     #functional.covariate.comparisons,
                                                     #functional.covariate.comparisons.for.sample.size,
                                                     ################################
                                                     ## for computing sample sizes ##
                                                     ################################
                                                     #type1.error,
                                                     #type2.error,
                                                     #treatment.effect,
                                                     #dropout.rate
  )

  ############################
  ## Output to old notation ##
  ############################


  #######################################
  ##Input converted to old notation   ###
  ##used in jprat main estmiation     ###
  #######################################
  common.param.estimation <- old.names$common.param.estimation ## TRUE
  analyze.separately <- old.names$analyze.separately # "event"
  boot <- old.names$boot #100 ## labeling
  est.cens.depend.z <- old.names$est.cens.depend.z ## False
  real_data <- old.names$real_data # TRUE ## data
  time_choice.predicted <- old.names$time_choice.predicted  # NULL
  time_choice.predicted.toadd <- old.names$time_choice.predicted.toadd # NULL
  var.boot  <- old.names$var.boot # TRUE
  arbitrary <- old.names$arbitrary # FALSE
  beta0int <- old.names$beta0int #0.5
  a0 <- old.names$a0 #NULL
  combi.study <- old.names$combi.study # 3
  combi.choice <- old.names$combi.choice
  combi.names  <- old.names$combi.names #"cohort-predict" "cohort-pharos"  "predict-pharos"
  compute.study.differences <- old.names$compute.study.differences # FALSE
  family.data <- old.names$family.data # FALSE
  gamma.param <- old.names$gamma.param # null
  la <- old.names$la #1
  lb <- old.names$lb #
  knot.length <- old.names$knot.length #8
  link.type <- old.names$link.type  # "logit"
  m <- old.names$m
  method <- old.names$method # "gamm4"
  maxm <- old.names$maxm     # 1
  num_study <- old.names$num_study # 3
  num_time <- old.names$num_time 		#13
  n <- old.names$n   # 430 915 346
  np <- old.names$np 	# 1
  num_xx <- old.names$num_xx  # 18
  nmax <- old.names$nmax #  915
  omega.param <- old.names$omega.param # NULL
  param.label <- old.names$param.label # "beta0" "beta1"
  par_fu <- old.names$par_fu  # null
  spline.constrain <- old.names$spline.constrain # TRUE
  time_val <- old.names$time_val #  40  45  50  55  60  65  70  75  80  85  90  95 100
  xks  <- old.names$xks       # points for x where to be estimated for each study
  zeval  <- old.names$zeval
  z.choice  <- old.names$z.choice    # 4




  #######################################
  ##Input converted to old notation   ###
  ## obtain null array                ###
  #######################################

  s.names <- old.names$s.names   ## event names # "hdage_nobase"
  simus <- old.names$simus # 1  NUMBER OF SIMULATION
  theta.names <- old.names$theta.names  # "beta"   "alphas" "Ft"
  theta.names.combi <- old.names$theta.names.combi  # "beta"   "alphas" "Ft"
  x_lab_names  <- old.names$x_lab_names  ## xx 36, xx37.4. ..x44..... xx 50
  z_lab_names  <- old.names$z_lab_names   # A, B, C, D

  ###################################################
  ## Form dataset for real data or simulated data  ##
  ###################################################
  axmod <- old.names$axmod  #$cohort
  beta0 <- old.names$beta0  # 1
  censorrate <- old.names$censorrate # NULL
  delta_tmp.list <- old.names$delta_tmp.list # 0 or 1 for each study
  frform <- old.names$frform  # NULL
  fzrform <- old.names$fzrform # NULL
  fxform <- old.names$fxform # NULL
  gtmod <- old.names$gtmod  # null
  gen.cens.depend.z <- old.names$gen.cens.depend.z  #null
  mix_n <- old.names$mix_n 	# null
  s_tmp.list <- old.names$s_tmp.list # CAG repeat length for all subject for each study
  type_fr <- old.names$type_fr    # null
  use.random.effects  <- old.names$use.random.effects
  x_tmp.list  <- old.names$x_tmp.list ## all values for x_tmp for each study

  ##############################
  # labeling for results data ##
  ##############################
  iseed <- old.names$iseed #1





  ################################################
  # Generating fixed effects for simulated data ##
  # (not used)                                  ##
  ################################################
  lb.max <- old.names$lb.max # 1
  par1_fzr <- old.names$par1_fzr # parameter each covar # null
  par2_fzr <- old.names$par2_fzr  # null
  par1_fx <- old.names$par1_fx  # null
  par2_fx <- old.names$par2_fx  # null
  par1_fr <- old.names$par1_fr  # null
  par2_fr <- old.names$par2_fr  # null
  par1_fr2 <- old.names$par1_fr2  # null
  par2_fr2 <- old.names$par2_fr2  # null
  type_fzr <- old.names$type_fzr  # null
  type_fx <- old.names$type_fx    # null
  z_tmp.list  <- old.names$z_tmp.list # all values for z_tmp (BASE AGE) for each study




  ########################################
  ## Not used parameters in JPRAT  ##
  ########################################

  ##################
  ## from results ##
  ##################
  #alpha.cut <- old.names$alpha.cut    ## NULL
  #add.second.legend <- old.names$add.second.legend ## TRUE
  #alphax_lab <- old.names$alphax_lab # 1,2,3
  #beta.cut <- old.names$beta.cut #NULL
  #boot.ci <- old.names$boot.ci # FALSE
  #convert.x <- old.names$convert.x #TRUE
  #legend.position.use <- old.names$legend.position.use # topright
  #round.set <- old.names$round.set # 2
  #plot.nw <- old.names$plot.nw #FALSE
  #study.ylab <- old.names$study.ylab #"Probability"
  #study.xlab <- old.names$study.xlab #"Age (years)"
  #time.cut  <- old.names$time.cut #NULL
  #ylim.key <- old.names$ylim.key # ylim for different covariate
  #ylim.setting <- old.names$ylim.setting #beta0  -10   8
  #beta1   -1   1
  #alphax -10  10
  #alphat -10  10
  #Ft       0   1

  #z_lab  <- old.names$z_lab #  "zza" "zzb" "zzc" "zzd"
  #alphax.ylab <- old.names$alphax.ylab # "Effect on Outcome"
  #alphax.xlab <- old.names$alphax.xlab  #"CAG repeats"
  #alphat.ylab <- old.names$alphat.ylab  # "Effect on Outcome"
  #alphat.xlab <- old.names$alphat.xlab  # "Age (years)"



  #######################################
  ##Input converted to old notation   ###
  #######################################


  #alphat_lab <- old.names$alphat_lab ## 45, 55
  #colors.use <- old.names$colors.use  #hdage_nobase
  #"firebrick1"


  #par3_fr <- old.names$par3_fr # null
  #plot.KM <- old.names$plot.KM #FALSE
  #randomeffects.covariate.dependent <- old.names$randomeffects.covariate.dependent  # NULL
  #var.lo  <- old.names$var.lo   # "varlo"
  #var.hi  <- old.names$var.hi   # "varhi"
  #beta_lab  <- old.names$beta_lab
  #common.param.data <- old.names$common.param.data # NULL
  #gamm.orig <- old.names$gamm.orig # false
  #xx_choice.ci  <- old.names$xx_choice   # 42, 44, 46
  #zeval.tmp  <- old.names$zeval.tmp     #   base_age_kmeans
  #	  A        28.11354
  #		B        37.37259
  #		C        46.57556
  #		D        58.28165
  #conf.int.use <- old.names$conf.int.use		## TRUE
  #nn.comparison <- old.names$nn.comparison  ## nc1: 1
  #s.names.short  <- old.names$s.names.short #hdage_nobase
  #"Motor Diagnosis (DCL=4)"
  #time_choice <- old.names$time_choice ## 45, 55
  #time_choice.ci <- old.names$time_choice.ci ## 40 45 50 55 60

  #xx_choice  <- old.names$xx_choice # 42, 44, 46
  #var.est  <- old.names$var.est # "est"
  #zz_comp  <- old.names$zz_comp # 1 1
  #xx_comp <- old.names$xx_comp	# 42, 46
  #xx_choice.sample.size <- old.names$xx_choice.sample.size  ## 42







  #######################################
  ## get null theta arrays for storage ##
  #######################################

  ##all_null_theta : array to store all estimates. set to NULL values




  ##################################
  # no examples but add documentations
  ##################################
  null.theta <- all_null_theta(theta.names,
                               study.names,
                               event.names=s.names,  #"hdage_nobase"
                               z_lab_names,          # base_age: A, B, C,D
                               x_lab_names,         ## points for CAG repeats
                               label.dim.simus=simus,
                               label.name.simus=paste("iter",1:simus,sep=""),
                               time_val,  ## 40  45  50  55  60  65  70  75  80  85  90  95 100
                               param.label,  ## beta0 beta1
                               time_choice.predicted,time_choice.predicted.toadd, ## null
                               la  # 1
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
                                     param.label, ## beta0 beta1
                                     time_choice.predicted,time_choice.predicted.toadd,
                                     la)


  #, , zz = B, xx = xx44.4, time = t100

  #event
  #study            hdage_nobase
  #cohort-predict            0
  #cohort-pharos             0
  #predict-pharos            0

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

  ## difference is 0?
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
      s_tmp.list, ## CAG repeat length?
      a0,##null
      axmod, ##"real"
      num_time, ## 13
      censorrate,## null
      frform, ## null
      type_fr, ## null
      par1_fr,par2_fr,par1_fr2,par2_fr2,mix_n, par_fu, ## null
      real_data,   ## true
      time_val,    ## 40, 45, 50, ...100
      beta0int, ## 0.5
      beta0,
      gamma.param,omega.param,## null
      n, ## number of sample size
      nmax,
      m, ## observed event 0 or 1
      maxm,
      la,lb,
      num_study,
      np,
      gtmod, ## null
      use.random.effects, ## null
      gen.cens.depend.z)	## null



    #####################################
    # no examples but add documentations#
    #####################################
    jprat.out<-jprat.main.estimates(method="gamm4",  ## conditional paramters to estimate using gamm4
                                    compute.study.differences=compute.study.differences, ## conditional parameter to do boostrap procedure
                                    var.boot=var.boot, ## conditional parameter to do boostrap procedure
                                    ########################
                                    # For main estimation function
                                    ########################
                                    arbitrary, ## FALSE
                                    num_study,np,
                                    count.store, # null
                                    count.store.outside, # null
                                    data, #=data, ## data from simu.data.fixed.effects()
                                    num_time, ## 13
                                    time_val, ## specific time value 40, 45, ..., 90, 95, 100
                                    time_choice.predicted, ## null
                                    time_choice.predicted.toadd, ## null
                                    n,nmax,m,maxm,
                                    la,lb,
                                    xks, ## 0, 0.1, ....0.7, ..., 0.9, 1
                                    truth, #=truth, ## where do we define?
                                    num_xx,  ## 18
                                    knot.length, ## 8, where do we define?
                                    real_data, ## TRUE
                                    family.data, ## FALSE
                                    zeval, ## at which points were estimated: k-means group
                                    z.choice, ## 4
                                    param.label, ## "beta0" "beta1"
                                    beta0int, ## 0.5
                                    gamma.param,omega.param, ## null
                                    spline.constrain, ## TRUE
                                    common.param.estimation, ## TRUE
                                    est.cens.depend.z, ## FALSE
                                    par_fu, ## null
                                    analyze.separately, ## "event"
                                    link.type, ## "logit"
                                    null.theta, #=null.theta, ## why do we need it?
                                    z.proportions=NULL, ## null
                                    ###########################
                                    #For jprat estimation
                                    ###########################
                                    betaest.store,
                                    alphasest.store,
                                    Ftest.store,
                                    Ftest.predicted.store,
                                    #combi.null.theta,
                                    ###########################
                                    #For bootstraps estimation
                                    ###########################
                                    combi.study,
                                    combi.choice,
                                    combi.names,
                                    boot=boot,
                                    boot.null.theta=boot.null.theta,
                                    boot.combi.null.theta=boot.combi.null.theta, #=boot.combi.null.theta,
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

  ## No need to report these.
  #count.store<-jprat.out$count.store
  #count.store.outside<-jprat.out$count.store.outside



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
  dim.order.all <- get.all.dim.order(theta.names)   ## name of dimension order
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

  ## flatten along time components,
  ## ulc: not sure  exactly what flatten means?
  truth.out.tmp <- get.theta.output(all.est.store=truth,
                                    dim.order=dim.order.all$dim.order,
                                    flatten.name="time",
                                    flatten.theta.name=flatten.theta.name,
                                    extension=".true",
                                    theta.names)

  truth.out <- merge.by.columns(truth.out.tmp)
  truth.out <- truth.out[,truth.colm.names]


  ##########################
  ## beta/alpha/Ft/Ftpred ##
  ##########################
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


  ### compare
  #write.csv(combi.out, file="original_while_loop_results_combi_out.csv")
  #write.csv(combi.out, file="newcode_while_loop_results_combi_out.csv")
  #write.csv(combi.out, file="speudo_combi_out.csv")
  #write.csv(theta.out, file="speudo_theta_out.csv")
  #write.csv(Ftest.store, file="speudo_Ftstore.csv")


  #########################
  ## filename for output ##
  #########################
  filename <- paste("output_iseed_",iseed,".dat",sep="")

  #if(use_real_data==TRUE){
  filename <- paste("real_",filename,sep="")
  #}

    #write.table(truth.out,paste("out_truth_",filename,sep=""),
    #            col.names=FALSE,row.names=FALSE)

    write.table(theta.out,
                paste("out_thetaest_",filename,sep=""), col.names=FALSE, row.names=FALSE)

    write.table(combi.out,
                paste("out_combi_",filename,sep=""),col.names=FALSE,row.names=FALSE,na="0")

   # write.table(jprat.out$count.store,
   #              paste("out_count_",filename,sep=""),col.names=FALSE,row.names=FALSE,na="0")

   #  write.table(jprat.out$count.store.outside,
   #              paste("out_outside_count_",filename,sep=""),col.names=FALSE,row.names=FALSE,na="0")


    jprat.output<- list(data.theta=theta.out, data.combi=combi.out)

  }else{

    #perm.alphaest.store <- aperm(alphasest.store,c("iters","study","event","time","xx","theta","val")) ## permutation  of alpha.array with the following order

      jprat.output=list(
      beta.array=betaest.store,
      alpha.array=alphasest.store,
      #alpha.array.new=perm.alphaest.store,
      Ft.array=Ftest.store,
      Ft.predicted.array=Ftest.predicted.store,
      beta.diff.array=betabootci.store,
      alpha.diff.array=alphasbootci.store,
      Ft.diff.array=Ftbootci.store
    )

  }




  list(#truth.out=truth.out,
       jprat.output=jprat.output,
      # theta.out=theta.out,
      # combi.out=combi.out,
       #count.store=count.store,
       #count.store.outside=count.store.outside,
      # Ftest.store=Ftest.store,
       eflag=eflag)
}
