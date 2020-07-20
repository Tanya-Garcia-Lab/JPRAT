#############################################
## R code to read in data and show results ##
#############################################
#' This function returns analysis results including plots and tables.
#'
#' @param study.names See this argument in the \code{\link{jprat.wrapper}} function.
#' @param input.data.list See this argument in the \code{\link{jprat.wrapper}} function.
#' @param nonfunctional.covariate.names See this argument in the \code{\link{jprat.wrapper}} function.
#' @param functional.covariate.names See this argument in the \code{\link{jprat.wrapper}} function.
#' @param othercovariate.names See this argument in the \code{\link{jprat.wrapper}} function.
#' @param event.outcome.names See this argument in the \code{\link{jprat.wrapper}} function.
#' @param delta.names See this argument in the \code{\link{jprat.wrapper}} function.
#' @param time.points.for.prediction See this argument in the \code{\link{jprat.wrapper}} function.
#' @param time.points.for.conditional.prediction See this argument in the \code{\link{jprat.wrapper}} function.
#' @param time.points.for.conditional.prediction.toadd See this argument in the \code{\link{jprat.wrapper}} function.
#' @param nonfunctional.covariate.value See this argument in the \code{\link{jprat.wrapper}} function.
#' @param functional.covariate.values.of.interest See this argument in the \code{\link{jprat.wrapper}} function.
#' @param use.functional.beta.intercept See this argument in the \code{\link{jprat.wrapper}} function.
#' @param use.functional.event.coefficients See this argument in the \code{\link{jprat.wrapper}} function.
#' @param use.functional.study.coefficients See this argument in the \code{\link{jprat.wrapper}} function.
#' @param check.study.equality See this argument in the \code{\link{jprat.wrapper}} function.
#' @param estimated.parameters.common.for.all.studies See this argument in the \code{\link{jprat.wrapper}} function.
#' @param what.analyzed.separately See this argument in the \code{\link{jprat.wrapper}} function.
#' @param estimation.when.censoring.depends.on.z See this argument in the \code{\link{jprat.wrapper}} function.
#' @param use.bootstrap.variance See this argument in the \code{\link{jprat.wrapper}} function.
#' @param functional.covariate.values.of.interest.ci a vector of specific functional covariate values of X,
#'         where the confidence interval of smooth functional parameter \eqn{\alpha(X=x,t)} will be estimated.
#'         For example, we set the values \eqn{x=46, 51} in the analysis.
#'         We recommend users to choose the same vector of functional covariate values X as in the interested functional covariate values (\code{functional.covariate.values.of.interest})
#'        to ease estimation procedure in the analysis.
#' @param number.of.bootstraps ADD DETAILS HERE!
#' @param time.points.of.interest a vector of specific time points at which the smooth functional parameters \eqn{\alpha(X,t)}
#'        will be predicted. These time points are used to be labeled for the plot.
#'        For example, we set the values \eqn{x=46, 56} in the analysis.
#'        We recommend users to choose these time points vector as the subset of time points for prediction (\code{time.points.for.prediction}).
#' @param time.points.of.interest.ci a vector of time points at which confidence intervals of \eqn{\alpha(X,t)} are predicted.
#'         These time points are used to be labeled for the plot. For example, we set the values \eqn{x=46, 51, 56, 61, 66} in the analysis.
#'         We recommend that these time points vector should be chosen as the subset of time points for prediction (\code{time.points.for.prediction}) or as the same time points for estimating \eqn{\alpha(X,t)} (\code{time.points.of.interest}).
#' @param label.for.alpha.values.over.time  a vector of specific time points at which the smooth functional parameters \eqn{\alpha(X,t)} will be predicted.
#'                       These time points are used to be labeled for the plot. Currently, we do not make a plot for the smooth functional parameters \eqn{\alpha(X,t)}.
#'                       Default is NULL.
#' @param which.nonfunctional.covariate.comparisons  a numeric vector for which nonfunctional covariates will be compared. In our analysis, we only consider one nonfunctional covariate named "base_age", default is c(1, 1).
#'        Currently, we only consider one nonfunctional covariate and there is nothing to compare between covariates.
#' @param color.names a character vector to denote which colors will be used for the labels of events of interest in plots. e.g., c("firebrick1", "darkgreen", "black").
#' @param legend.names a character vector to label event of interests in plots. e.g., c("Motor Diagnosis (DCL=4)",  "Cognitive Impairment" , "Stage II TFC").
#' @param functional.covariate.comparisons a vector of functional covariate values of X at which the smooth functional parameters \eqn{\alpha(X=x,\cdot)} were estimated and compared.
#'        For example, the functional covariate values are \eqn{x=c(46, 51)} and the parameters of \eqn{\alpha(x=46,\cdot)} and \eqn{\alpha(x=51,\cdot)} will be compared.
#'        We recommend that these covariate values X should be the same as the vector of functional covariate values of X for confidence intervals (\code{functional.covariate.values.of.interest.ci}).
#' @param functional.covariate.comparisons.for.sample.size a specific functional covariate values of X to use the sample size calculation,
#'              which will be compared across studies. Currently, we are not interested in sample size calculation. Default is NULL.
#' @param do.plots a logical value whether the results plot will be generated. Default is TRUE.
#' @param add.number.at.risk.legend a logical value whether number of subjects who is at risks at the interested time points will be added with legends in plots.
#' @param plot.confidence.intervals a logical value whether the confidence intervals of the predicted marginal distributions will be shown in the plot. Default is TRUE.
#' @param ylabel.for.plots.comparing.studies a character value for y-label in plots when studies are comparing or event of interests were compared in each study. Default is "Probability".
#' @param xlabel.for.plots.comparing.studies a character value for x-label in plots when studies are comparing for each event of interest or event of interests were compared in each study. Default is "Age(years)".
#' @param file.name.for.analysis a character value, which is used as the part of plot names.
#' @param data.theta data file for the estimated values of all components as a result of analysis. Default is data.truth and this file is automatically created if users choose \code{write.output=TRUE}.
#'                   See return value \code{theta.out} in the \code{\link{jprat.wrapper}} function.
#' @param data.combi data file for the difference between the estimated values of all components among studies as a result of analysis.
#'         Default is data.combi and this file is automatically created if users choose \code{write.output=TRUE}.
#'         See return value \code{combi.out} in the \code{\link{jprat.wrapper}} function.
#' @param data.count data file for counts for binary status of censored, uncensored and missing subjects as a result of analysis.
#' Default is data.count and this file is automatically created if users choose \code{write.output=TRUE}. See return value \code{count.store} in the \code{\link{jprat.wrapper}} function.
#' @param data.count.outside  data file for counts for binary status of censored, uncensored outside [0,1] as a result of analysis. Default is \code{data.count.outside} and this file is automatically created if users choose \code{write.output=TRUE}.
#'                             See return value \code{count.store.outside} in the \code{\link{jprat.wrapper}} function.
#' @param nrisk data file for the number of subjects who is at risk at the interested the time points defined by users. This is automatically calculated.
#'              Default is nrisk and this file is automatically created.
#' @param data.truth.other This is not a users' interest argument. Analysis results from COX model. Use this if the proportional odds model is compared to COX model, which is not considered. Default is NULL.
#' @param data.theta.other This is not a users' interest argument. Analysis results from COX model. Use this if the proportional odds model is compared to COX model, which is not considered. Default is NULL.
#' @param nrisk.other This is not a users' interest argument. Analysis results from COX model. Use this if the proportional odds model is compared to COX model, which is not considered. Default is NULL.
#' @param data.combi.other  This is not a users' interest argument. Analysis results from COX model. Use this if the proportional odds model is compared to COX model, which is not considered. Default is NULL.
#' @param s.names.use This is not a users' interest argument. Default is NULL.
#' @param s.names.other.use This is not a users' interest argument. Default is NULL.
#' @param type1.error type 1 error rate, which is for sample size calculations. We do not consider this arguments in the analysis. Default is NULL.
#' @param type2.error type 2 error rate, which is for sample size calculations. We do not consider this arguments in the analysis. Default is NULL.
#' @param treatment.effect a numeric vector of the treatment effects for sample size calculations. We do not consider this arguments in the analysis. We do not consider this arguments in the analysis. Default is NULL.
#' @param dropout.rate a numeric vector of drop-out rates, which is used for sample size calculations. We do not consider this arguments in the analysis. Default is NULL.
#' @param show.results.description a logical value whether the description of plots and tables will be produced. Default is FALSE.
#'
#' @details The number of risks at each time point will be printed in the plot.
#'
#' @return This function returns ggplots.
#'
#' @import abind
#' @export
#'
#' @example man/examples/example.R
#'
#'
view.all.results <- function(
  ############################
  # arguments for JPRAT     ##
  ############################
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
  #############################
  ## Input: How to analysis  ##
  #############################
  number.of.bootstraps,
  use.functional.beta.intercept,
  use.functional.event.coefficients,
  use.functional.study.coefficients,
  check.study.equality,
  estimated.parameters.common.for.all.studies,
  what.analyzed.separately,
  estimation.when.censoring.depends.on.z,
  use.bootstrap.variance,
  #############################
  ## To get Results          ##
  #############################
  ##########################
  ## For plotting results ##
  ##########################
  functional.covariate.values.of.interest.ci,
  time.points.of.interest,
  time.points.of.interest.ci,
  label.for.alpha.values.over.time=NULL,
  which.nonfunctional.covariate.comparisons,
  color.names,
  legend.names,
  functional.covariate.comparisons,
  functional.covariate.comparisons.for.sample.size=NULL,
  #############################
  # plot options             ##
  #############################
  do.plots,
  plot.confidence.intervals,
  ###########################
  # Label figures or files ##
  ###########################
  add.number.at.risk.legend,
  ylabel.for.plots.comparing.studies,
  xlabel.for.plots.comparing.studies,
  file.name.for.analysis,
  #######################
  ## Output from JPRAT ##
  #######################
  #data.truth=data.truth,
  data.theta=data.theta,
  data.combi=data.combi,
  data.count=data.count,
  data.count.outside=data.count.outside,
  nrisk=nrisk,
  data.truth.other=NULL,
  data.theta.other=NULL,
  nrisk.other=NULL,
  data.combi.other=NULL,
  s.names.use=NULL,
  s.names.other.use=NULL,
  ########################################################
  ## for computing sample sizes: users do not need them ##
  ########################################################
  type1.error=NULL,
  type2.error=NULL,
  treatment.effect=NULL,
  dropout.rate=NULL,
  #####################################
  ## Do we show results description? ##
  #####################################
  show.results.description
){


  #####################################################################################
  ## Obtain default options : See function default.options.for.data.setting in       ##
  ## pseudo_default_setting_estimation_results.R                                     ##
  ## added on 10/08/19                                                               ##
  #####################################################################################

  default.options<-default.options.for.data.setting()

  ## default values
  use_real_data<-default.options$use_real_data
  estimate.variances<-default.options$estimate.variances
  glm.link.type<- default.options$glm.link.type
  clusters.are.families<-default.options$clusters.are.families
  paper.type<-default.options$paper.type
  plot.parameters<-default.options$plot.parameters
  combine.data<-default.options$combine.data

  ########################################################################################
  ## Data sets are reformed: see function data.reformatted.for.jprat.analysis in main.R ##
  ## added on 10/08/19                                                                  ##
  ########################################################################################

  reformatted.data.for.jprat<-data.reformatted.for.jprat.analysis(use_real_data,
                                                                  study.names,
                                                                  input.data.list,
                                                                  time.points.for.prediction,
                                                                  nonfunctional.covariate.names,
                                                                  functional.covariate.names,
                                                                  nonfunctional.covariate.value,
                                                                  functional.covariate.values.of.interest)



  ## datasets are reformmated, so that jprat can use them
  data.sets.as.list<-reformatted.data.for.jprat$data.sets.as.list;
  nonfunctional.covariate.values.for.prediction<-reformatted.data.for.jprat$nonfunctional.covariate.values.for.prediction;
  xmin<-reformatted.data.for.jprat$xmin;
  xmax<-reformatted.data.for.jprat$xmax;
  functional.covariate.values.for.prediction<-reformatted.data.for.jprat$functional.covariate.values.for.prediction;




  ########################################################################
  ## Data sets are reformmated for getting results:                        ##
  ## see function data.reformatted.for.analysis.results in main.R       ##
  ## added on 10/09/19                                                  ##
  ########################################################################

  reformatted.data.for.analysis.results<-data.reformatted.for.analysis.results(study.names,
                                                                               event.outcome.names,
                                                                               color.names,
                                                                               legend.names,
                                                                               which.nonfunctional.covariate.comparisons)

  ## datasets are reformmated, so that results plots and tables can be produced
  num_study<-reformatted.data.for.analysis.results$num_study;
  nonfunctional.covariate.comparisons<-reformatted.data.for.analysis.results$nonfunctional.covariate.comparisons;
  color.labels<-reformatted.data.for.analysis.results$color.labels;
  legend.labels<-reformatted.data.for.analysis.results$legend.labels;
  event.comparison.table<-reformatted.data.for.analysis.results$event.comparison.table;




  ######################################################################
  ## Convert new variables to old variable names to use original code ##
  ######################################################################






  old.names <- convert.new.notation.to.old.for.get.results(study.names,
                                                           #data.sets.as.list=data.sets.as.list,
                                                           time.points.for.prediction,
                                                           time.points.for.conditional.prediction,
                                                           time.points.for.conditional.prediction.toadd,
                                                           nonfunctional.covariate.names,
                                                           nonfunctional.covariate.values.for.prediction=nonfunctional.covariate.values.for.prediction,
                                                           functional.covariate.names,
                                                           functional.covariate.values.of.interest,
                                                           functional.covariate.values.of.interest.ci,
                                                           functional.covariate.values.for.prediction=functional.covariate.values.for.prediction,
                                                           xmin=xmin,
                                                           xmax=xmax,
                                                           #othercovariate.names,
                                                           event.outcome.names,
                                                           #delta.names,
                                                           use_real_data=use_real_data,
                                                           use.functional.beta.intercept,
                                                           use.functional.event.coefficients,
                                                           use.functional.study.coefficients,
                                                           number.of.bootstraps,
                                                           #check.study.equality,
                                                           what.analyzed.separately,
                                                           estimated.parameters.common.for.all.studies,
                                                           #estimate.variances=estimate.variances,
                                                           #estimation.when.censoring.depends.on.z,
                                                           glm.link.type=glm.link.type,
                                                           use.bootstrap.variance,
                                                           #clusters.are.families=clusters.are.families,
                                                           ##########################
                                                           ## For plotting results ##
                                                           ##########################
                                                           time.points.of.interest,
                                                           time.points.of.interest.ci,
                                                           label.for.alpha.values.over.time=NULL,
                                                           nonfunctional.covariate.comparisons=nonfunctional.covariate.comparisons,
                                                           #plot.parameters=plot.parameters,
                                                           #do.plots,
                                                           plot.confidence.intervals=plot.confidence.intervals,
                                                           color.labels=color.labels,
                                                           legend.labels=legend.labels,
                                                           event.comparison.table=event.comparison.table,
                                                           functional.covariate.comparisons=functional.covariate.comparisons,
                                                           functional.covariate.comparisons.for.sample.size,
                                                           add.number.at.risk.legend=add.number.at.risk.legend,
                                                           ylabel.for.plots.comparing.studies=ylabel.for.plots.comparing.studies,
                                                           xlabel.for.plots.comparing.studies=xlabel.for.plots.comparing.studies,
                                                           ################################
                                                           ## for computing sample sizes ##
                                                           ################################
                                                           type1.error=NULL,
                                                           type2.error=NULL,
                                                           treatment.effect=NULL,
                                                           dropout.rate=NULL
  )



  #######################################################################
  ### if users have their own data, we do not define analysis.to.run: ###
  #######################################################################
  #######################################################################
  comp <- file.name.for.analysis #analysis.to.run


  ##################
  ## from results ##
  ##################
  alpha.cut <- old.names$alpha.cut
  add.second.legend <- old.names$add.second.legend
  alphax_lab <- old.names$alphax_lab
  beta.cut <- old.names$beta.cut
  convert.x <- old.names$convert.x
  legend.position.use <- old.names$legend.position.use
  round.set <- old.names$round.set


  study.ylab <- old.names$study.ylab
  study.xlab <- old.names$study.xlab
  time.cut  <- old.names$time.cut
  ylim.key <- old.names$ylim.key
  ylim.setting <- old.names$ylim.setting








  ##Input converted to old notation
  analyze.separately <- old.names$analyze.separately

  ### change code
  colors.use <- old.names$colors.use
  common.param.estimation <- old.names$common.param.estimation
  conf.int.use <- old.names$conf.int.use
  nn.comparison <- old.names$nn.comparison
  real_data <- old.names$real_data

  ## change code
  s.names.short  <- old.names$s.names.short
  s.names <- old.names$s.names

  time_choice.predicted <- old.names$time_choice.predicted
  time_choice.predicted.toadd <- old.names$time_choice.predicted.toadd
  time_choice <- old.names$time_choice
  time_choice.ci <- old.names$time_choice.ci


  xx_choice  <- old.names$xx_choice
  zz_comp  <- old.names$zz_comp
  xx_comp <- old.names$xx_comp
  xx_choice.sample.size <- old.names$xx_choice.sample.size

  combi.study <- old.names$combi.study
  combi.choice <- old.names$combi.choice
  combi.names  <- old.names$combi.names



  family.data <- old.names$family.data
  la <- old.names$la
  link.type <- old.names$link.type
  num_study <- old.names$num_study
  num_time <- old.names$num_time
  np <- old.names$np
  num_xx <- old.names$num_x
  param.label <- old.names$param.label
  plot.KM <- old.names$plot.KM


  simus <- old.names$simus
  theta.names <- old.names$theta.names
  theta.names.combi <- old.names$theta.names.combi
  time_val <- old.names$time_val
  var.lo  <- old.names$var.lo
  var.hi  <- old.names$var.hi


  xks  <- old.names$xks
  x_lab_names  <- old.names$x_lab_names
  xx_choice.ci  <- old.names$xx_choice
  z.choice  <- old.names$z.choice
  z_lab_names  <- old.names$z_lab_names

  ## IF combine.data=FALSE,
  #zeval.tmp  <- old.names$zeval.tmp

  ## if parmaeter.plot=TRUE, the following arguments are needed
  alphax.ylab <- old.names$alphax.ylab
  alphax.xlab <- old.names$alphax.xlab
  alphat.ylab <- old.names$alphat.ylab
  alphat.xlab <- old.names$alphat.xlab
  alphat_lab <- old.names$alphat_lab
  # z_tmp.list  <- old.names$z_tmp.list
  # x_tmp.list  <- old.names$x_tmp.list
  # zeval  <- old.names$zeval
  # s_tmp.list <- old.names$s_tmp.list
  # compute.study.differences <- old.names$compute.study.differences
  # delta_tmp.list <- old.names$delta_tmp.list


  #####################################################
  ## following arguments are not used to get results ##
  #####################################################
  # boot.ci <- old.names$boot.ci
  # plot.nw <- old.names$plot.nw
  # z_lab  <- old.names$z_lab
  # boot <- old.names$boot
  # est.cens.depend.z <- old.names$est.cens.depend.z



  ## Depends on simulated or real data
  # a0 <- old.names$a0
  # arbitrary <- old.names$arbitrary
  # axmod <- old.names$axmod
  # beta0int <- old.names$beta0int
  # beta0 <- old.names$beta0
  # beta_lab  <- old.names$beta_lab
  # censorrate <- old.names$censorrate
  # common.param.data <- old.names$common.param.data
  # frform <- old.names$frform
  # fzrform <- old.names$fzrform
  # fxform <- old.names$fxform
  # gamm.orig <- old.names$gamm.orig
  # gamma.param <- old.names$gamma.param
  # gtmod <- old.names$gtmod
  # gen.cens.depend.z <- old.names$gen.cens.depend.z
  # iseed <- old.names$iseed
  # knot.length <- old.names$knot.length
  # lb <- old.names$lb
  # lb.max <- old.names$lb.max
  # m <- old.names$m
  # method <- old.names$method
  # mix_n <- old.names$mix_n
  # maxm <- old.names$maxm
  # nmax <- old.names$nmax
  # omega.param <- old.names$omega.param
  # n <- old.names$n
  # par1_fzr <- old.names$par1_fzr
  # par2_fzr <- old.names$par2_fzr
  # par1_fx <- old.names$par1_fx
  # par2_fx <- old.names$par2_fx
  # par1_fr <- old.names$par1_fr
  # par2_fr <- old.names$par2_fr
  # par1_fr2 <- old.names$par1_fr2
  # par2_fr2 <- old.names$par2_fr2
  # par3_fr <- old.names$par3_fr
  # par_fu <- old.names$par_fu
  # randomeffects.covariate.dependent <-
  # 	old.names$randomeffects.covariate.dependent
  # spline.constrain <- old.names$spline.constrain
  # type_fzr <- old.names$type_fzr
  # type_fx <- old.names$type_fx
  # type_fr <- old.names$type_fr
  # use.random.effects  <- old.names$ use.random.effects
  # var.est  <- old.names$var.est
  # var.boot  <- old.names$var.boot



  ################
  ## File names ##
  ################

  filename_new <- ""

  if(common.param.estimation==TRUE){
    filename_new <- paste("j",filename_new,sep="")
  }

  if(plot.KM==TRUE){
    filename_new <- paste("k",filename_new,sep="")
  }

  #if(link.type=="cloglog"){
  #  filename_new <- paste("c",filename_new,sep="")
  #}


  # if(real_data==FALSE){
  #   if(randomeffects.covariates.dependent==TRUE){
  #     filename_new <- paste("dep_",filename_new,sep="")
  #   }
  #
  #   if(family.data==TRUE){
  #     filename_new <- paste("fam_",filename_new,sep="")
  #   }
  # }


  ####################
  ## Number at risk ##
  ####################
  get.number.at.risk <- function(nrisk){
    nrisk.array <- apply(nrisk[,c("xx","zz","event","study")],2,unique)
    nrisk.array$time <- paste("t",time_val,sep="")
    nrisk.array <- array(0,dim=lapply(nrisk.array,length),
                         dimnames=nrisk.array)
    nrisk.array <- aperm(nrisk.array,c("study","event","zz","xx","time"))
    number.at.risk <- unflatten.array(nrisk.array,
                                      names(dimnames(nrisk.array)),
                                      nrisk,flatten.name="time")
    return(number.at.risk)
  }

  if(real_data==TRUE){

     number.at.risk <- get.number.at.risk(nrisk)
  }






  ###########################################
  ## number of digits for printing results ##
  ###########################################
  digits.tmp <- 2
  digits.tmp.short <- 2

  ##################################
  ## beta and alpha estimate info ##
  ##################################

  ## time points
  time_val <- time_val

  ## number of time points
  num_time <- length(time_val)


  ## xx points
  tmp.list <- get.empty.list(study.names)
  xx_val <- xks

  ## number of xx points
  num_xx <- max(unlist(lapply(xx_val,length.apply)))


  ## nsimu
  nsimu <- nrow(data.theta)/
    (num_study*np* 7 * (
      length(param.label) +  ## number of betas
        length(xx_val[[1]]) +  ## number of alphas
        z.choice * length(xx_val[[1]]) +## Ft_
        z.choice * length(xx_val[[1]]) * length(time_choice.predicted.toadd)
      ## Ft.predicted
    )
    )


  ############
  ## counts ##
  ############

  #if(1==0){ ## for testing
  colnames(data.count) <- c("study","time","y1","y0_nocens","y0_cens","yother")
  counts.array <- array(0,dim=c(nsimu,num_study,num_time,4),
                        dimnames=list(
                          paste("iter",1:nsimu,sep=""),
                          paste("ss",1:num_study,sep=""),
                          paste("t",time_val,sep=""),
                          c("y1","y0_nocens","y0_cens","yother")))

  for(ss in 1:num_study){
    for(t in 1:num_time){
      index <- intersect(which(data.count$study==ss),
                         which(data.count$time==as.numeric(time_val[t])))
      counts.array[,ss,t,] <- as.matrix(data.count[index,c("y1","y0_nocens",
                                                           "y0_cens","yother")])
    }
  }
  #}


  ####################
  ## counts outside ##
  ####################
  ##if(1==0){ ## for testing
  colnames(data.count.outside) <- c("study","time","zero","ones")
  counts.outside.array <- array(0,dim=c(nsimu,num_study,num_time,2),
                                dimnames=list(
                                  paste("iter",1:nsimu,sep=""),
                                  paste("ss",1:num_study,sep=""),
                                  paste("t",time_val,sep=""),
                                  c("zero","ones")))

  for(ss in 1:num_study){
    for(t in 1:num_time){
      index <- intersect(which(data.count.outside$study==ss),
                         which(data.count.outside$time==as.numeric(time_val[t])))
      counts.outside.array[,ss,t,] <-
        as.matrix(data.count.outside[index,c("zero","ones")])
    }
  }
  #} ## end test



  ######################
  ## organize results ##
  ######################
  my.out <- sort.results(
    combi.names,
    num_study,
    simus=nsimu,
    time_val,param.label,
    num_xx,la,z.choice,
    time_choice.predicted,time_choice.predicted.toadd,
    #data.truth,
    data.theta,
    data.combi,
    alpha.cut,
    beta.cut,
    theta.names,
    theta.names.combi,
    study.names,
    s.names,
    z_lab_names,
    x_lab_names
  )

  betat <- my.out$betat
  alphat <- my.out$alphat
  alphat.new <- my.out$alphat.new
  Ft <- my.out$Ft
  Ft.predicted <- my.out$Ft.predicted
  #
  beta.array <- my.out$beta.array
  alpha.array <- my.out$alpha.array
  alpha.array.new<- my.out$alpha.array.new
  Ft.array <- my.out$Ft.array
  Ft.predicted.array<- my.out$Ft.predicted.array
  #
  beta.diff.array <- my.out$beta.diff.array
  alpha.diff.array <- my.out$alpha.diff.array
  Ft.diff.array <- my.out$Ft.diff.array

  ###############################################
  ## if other exists: get needed matrices	     ##
  ## used to compare COX with PROP ODD model   ##
  ###############################################
  # if(!is.null(data.theta.other) & !is.null(nrisk.other) &
  #    !is.null(data.truth.other) & !is.null(data.combi.other)){
  #
  #
  #   number.at.risk.other <- get.number.at.risk(nrisk.other)
  #   study.names.other <- dimnames(number.at.risk.other)$study
  #   num_study.other <- length(study.names.other)
  #   s.names.other <- dimnames(number.at.risk.other)$event
  #
  #   combi.info.other <- get.combi.info(study.names.other)
  #   combi.study.other <- combi.info.other$combi.study
  #   combi.choice.other <- combi.info.other$combi.choice
  #   combi.names.other <- combi.info.other$combi.names
  #
  #   theta.names.other <- as.character(unique(data.truth.other[,3]))
  #   param.label.other <- theta.names.other[!grepl(c("alpha*"),theta.names.other) &
  #                                            !grepl("Ft*",theta.names.other)]
  #
  #   my.out.other <- sort.results(
  #     combi.names.other,
  #     num_study.other,
  #     simus=nsimu,
  #     time_val,param.label.other,
  #     num_xx,la,z.choice,
  #     time_choice.predicted,time_choice.predicted.toadd,
  #     data.truth=data.truth.other,
  #     data.theta=data.theta.other,
  #     data.combi=data.combi.other,
  #     alpha.cut,
  #     beta.cut,
  #     theta.names.other,
  #     theta.names.combi,
  #     study.names.other,
  #     s.names.other,
  #     z_lab_names,
  #     x_lab_names)
  #
  #   betat.other <- my.out.other$betat
  #   alphat.other <- my.out.other$alphat
  #   alphat.new.other <- my.out.other$alphat.new
  #   Ft.other <- my.out.other$Ft
  #   Ft.predicted.other <- my.out.other$Ft.predicted
  #   #
  #   beta.array.other <- my.out.other$beta.array
  #   alpha.array.other <- my.out.other$alpha.array
  #   alpha.array.new.other <- my.out.other$alpha.array.new
  #   Ft.array.other <- my.out.other$Ft.array
  #   Ft.predicted.array.other <- my.out.other$Ft.predicted.array
  #   #
  #   beta.diff.array.other <- my.out.other$beta.diff.array
  #   alpha.diff.array.other <- my.out.other$alpha.diff.array
  #   Ft.diff.array.other <- my.out.other$Ft.diff.array
  #

  ########################
  ## combine study info ##
  ########################
  # if(combine.data==TRUE){
  #   betat <- merge.results(betat,betat.other,
  #                          study.names,s.names.use,param.label,
  #                          study.names.other,s.names.other.use,param.label.other,
  #                          type="beta")
  #   alphat <- merge.results(alphat,alphat.other,
  #                           study.names,s.names.use,param.label,
  #                           study.names.other,s.names.other.use,param.label.other,
  #                           type="alpha")
  #
  #   alphat.new <- merge.results(alphat.new,alphat.new.other,
  #                               study.names,s.names.use,param.label,
  #                               study.names.other,s.names.other.use,param.label.other,
  #                               type="alpha")
  #
  #   Ft <- merge.results(Ft,Ft.other,
  #                       study.names,s.names.use,param.label,
  #                       study.names.other,s.names.other.use,param.label.other,
  #                       type="Ft")
  #
  #   Ft.predicted <- merge.results(Ft.predicted,Ft.predicted.other,
  #                                 study.names,s.names.use,param.label,
  #                                 study.names.other,s.names.other.use,param.label.other,
  #                                 type="Ft.predicted")
  #
  #   ##
  #   beta.array$out <-  merge.results(beta.array$out,beta.array.other$out,
  #                                    study.names,s.names.use,param.label,
  #                                    study.names.other,s.names.other.use,param.label.other,
  #                                    type="beta-out")
  #   beta.array$out.mean <-  merge.results(beta.array$out.mean,beta.array.other$out.mean,
  #                                         study.names,s.names.use,param.label,
  #                                         study.names.other,s.names.other.use,param.label.other,
  #                                         type="beta-mean")
  #
  #   alpha.array$out <-  merge.results(alpha.array$out,alpha.array.other$out,
  #                                     study.names,s.names.use,param.label,
  #                                     study.names.other,s.names.other.use,param.label.other,
  #                                     type="alpha-out")
  #   alpha.array$out.mean <-  merge.results(alpha.array$out.mean,alpha.array.other$out.mean,
  #                                          study.names,s.names.use,param.label,
  #                                          study.names.other,s.names.other.use,param.label.other,
  #                                          type="alpha-mean")
  #
  #   alpha.array.new$out <-  merge.results(alpha.array.new$out,alpha.array.new.other$out,
  #                                         study.names,s.names.use,param.label,
  #                                         study.names.other,s.names.other.use,param.label.other,
  #                                         type="alpha-out")
  #   alpha.array.new$out.mean <-  merge.results(alpha.array.new$out.mean,
  #                                              alpha.array.new.other$out.mean,
  #                                              study.names,s.names.use,param.label,
  #                                              study.names.other,s.names.other.use,param.label.other,
  #                                              type="alpha-mean")
  #
  #
  #   Ft.array$out <-  merge.results(Ft.array$out,Ft.array.other$out,
  #                                  study.names,s.names.use,param.label,
  #                                  study.names.other,s.names.other.use,param.label.other,
  #                                  type="Ft-out")
  #   Ft.array$out.mean <-  merge.results(Ft.array$out.mean,
  #                                       Ft.array.other$out.mean,
  #                                       study.names,s.names.use,param.label,
  #                                       study.names.other,s.names.other.use,param.label.other,
  #                                       type="Ft-mean")
  #
  #   Ft.array$out.mono <-  merge.results(Ft.array$out.mono,Ft.array.other$out.mono,
  #                                       study.names,s.names.use,param.label,
  #                                       study.names.other,s.names.other.use,param.label.other,
  #                                       type="Ft-out")
  #   Ft.array$out.mono.mean <-  merge.results(Ft.array$out.mono.mean,
  #                                            Ft.array.other$out.mono.mean,
  #                                            study.names,s.names.use,param.label,
  #                                            study.names.other,s.names.other.use,param.label.other,
  #                                            type="Ft-mean")
  #
  #   Ft.predicted.array$out <-  merge.results(Ft.predicted.array$out,
  #                                            Ft.predicted.array.other$out,
  #                                            study.names,s.names.use,param.label,
  #                                            study.names.other,s.names.other.use,param.label.other,
  #                                            type="Ft.predicted-out")
  #   Ft.predicted.array$out.mean <-  merge.results(Ft.predicted.array$out.mean,
  #                                                 Ft.predicted.array.other$out.mean,
  #                                                 study.names,s.names.use,param.label,
  #                                                 study.names.other,s.names.other.use,param.label.other,
  #                                                 type="Ft.predicted-mean")
  #
  #   #
  #   beta.diff.array$out <-  merge.results(beta.diff.array$out,beta.diff.array.other$out,
  #                                         combi.names,s.names.use,param.label,
  #                                         combi.names.other,s.names.other.use,param.label.other,
  #                                         type="beta-out")
  #   beta.diff.array$out.mean <-  merge.results(beta.diff.array$out.mean,
  #                                              beta.diff.array.other$out.mean,
  #                                              combi.names,s.names.use,param.label,
  #                                              combi.names.other,s.names.other.use,param.label.other,
  #                                              type="beta-mean")
  #
  #   alpha.diff.array$out <-  merge.results(alpha.diff.array$out,
  #                                          alpha.diff.array.other$out,
  #                                          combi.names,s.names.use,param.label,
  #                                          combi.names.other,s.names.other.use,param.label.other,
  #                                          type="alpha-out")
  #   alpha.diff.array$out.mean <-  merge.results(alpha.diff.array$out.mean,
  #                                               alpha.diff.array.other$out.mean,
  #                                               combi.names,s.names.use,param.label,
  #                                               combi.names.other,s.names.other.use,param.label.other,
  #                                               type="alpha-mean")
  #
  #   Ft.diff.array$out <-  merge.results(Ft.diff.array$out,Ft.diff.array.other$out,
  #                                       combi.names,s.names.use,param.label,
  #                                       combi.names.other,s.names.other.use,param.label.other,
  #                                       type="Ft-out")
  #   Ft.diff.array$out.mean <-  merge.results(Ft.diff.array$out.mean,
  #                                            Ft.diff.array.other$out.mean,
  #                                            combi.names,s.names.use,param.label,
  #                                            combi.names.other,s.names.other.use,param.label.other,
  #                                            type="Ft-mean")
  #
  #   number.at.risk <-  merge.results(number.at.risk,
  #                                    number.at.risk.other,
  #                                    study.names,s.names.use,param.label,
  #                                    study.names.other,s.names.other.use,param.label.other,
  #                                    type="nrisk")
  #
  #   #######################
  #   ## other info needed ##
  #   #######################
  #
  #   study.names <- dimnames(number.at.risk)$study
  #   num_study <- length(study.names)
  #   s.names <- dimnames(number.at.risk)$event
  #   param.label <- dimnames(betat)$theta
  #   nn.comparison[[1]] <- 1:length(s.names)
  #
  #
  #   #################################
  #   ## short names for event types ##
  #   #################################
  #   legend.label.key <- get.legend.labels.for.HD(paper.type)
  #   s.names.short <- unlist(legend.label.key[s.names])
  #   full.colors <- get.color.key.for.HD()
  #   colors.use <- unlist(full.colors[s.names])
  #
  #
  #
  #   ylim.setting <- array(0,dim=c(length(param.label)+2+1,2),
  #                         dimnames=list(c(param.label,"alphax","alphat","Ft"),
  #                                       c("min","max")))
  #   kk <- 1
  #   for(ii in 1:nrow(ylim.setting)){
  #     names.tmp <- rownames(ylim.setting)[ii]
  #     if(names.tmp %in% names(ylim.key)){
  #       ylim.setting[ii,] <- unlist(ylim.key[names.tmp])
  #     } else {
  #       names.tmp <- colnames(zeval.tmp)[kk]
  #       ylim.setting[ii,] <- unlist(ylim.key[names.tmp])
  #       kk <- kk+1
  #     }
  #   }
  #
  #
  #}
  # }




  ###############
  ## Get plots ##
  ###############
  get.plot.default <- function(){
    conf.int <- conf.int.use
    data.extra <- NULL

    if(real_data==TRUE){
      legend.position <- NULL  ## no legend used
      ##legend.position <- "bottomright"
      color.list <- "black"
      label.names <- 	c("Proposed Method")
      add.second.legend  <- FALSE
    } else {
      color.list <- c("black","blue")
      legend.position <- "bottomright"
      label.names <- 	c("Truth","Proposed method")
      add.second.legend <- FALSE
    }

    if(!is.null(legend.position)){
      if(add.second.legend==TRUE & legend.position=="bottomright"){
        legend.position <- "topright"
      }
    }

    plot.info <- list(color.list=color.list,
                      legend.position=legend.position,
                      label.names=label.names,
                      conf.int=conf.int,
                      add.second.legend=add.second.legend,
                      var.lo=var.lo,
                      var.hi=var.hi,
                      data.extra=data.extra,
                      est="est")
    return(plot.info)
  }

  ## set default plot values
  plot.info <- get.plot.default()


  if(show.results.description==TRUE){

    ###########################
    cat("\n\n INDVIDUAL PLOTS\n\n" )
    ###########################

  }


  if(real_data==TRUE){


    ####################################
    # edit plot file name on oct/11/19 #
    ####################################
    extra <- paste("_plot_", comp, sep="")

    if(!is.null(data.theta.other) & combine.data==FALSE){
      ## PLOT proportional odds and proportional hazards
      if(plot.KM==TRUE){
        color.list  <- c("blue","red","black")
        label.names  <-  c("Proportional Odds","Proportional Hazards","Kaplan-Meier")
        ##add.second.legend <- TRUE
      } else{
        color.list <- c("blue","black")
        label.names <-  c("Proportional Odds","Kaplan-Meier")
        ##add.second.legend <- TRUE
      }
    } else {
      ## ONLY plot proportional odds
      if(plot.KM==TRUE){
        color.list  <- c("blue","black")
        label.names  <-  c("Proportional Odds","Kaplan-Meier")
        ##add.second.legend <- TRUE
      } else{
        color.list <- c("black")
        label.names <-  c("Proportional Odds")
        ##add.second.legend <- TRUE
      }
    }

    ## update color list and label names
    plot.info$color.list=color.list
    plot.info$label.names=label.names
  } else {
    extra <- NULL
  }
  plot.info$ylim.use=ylim.setting







  if(real_data==TRUE & plot.KM==FALSE & do.plots==TRUE){


    if(show.results.description==TRUE){

      #########################################################################
      cat("\n\n Plots comparing event types for each study separately\n\n ")
      #########################################################################

    }


    for(ss in 1:num_study){
      ##legend.position <- legend.position.use

      if(study.names[ss]=="pharos" & (common.param.estimation==TRUE  ||
                                      analyze.separately!="none")){
        legend.position <- legend.position.use
      } else {
        if(paper.type=="statistics"){
          legend.position <- NULL  ## no legend
        } else {
          legend.position <- legend.position.use
        }
      }

      ## labels are at different CAG
      if(convert.x==TRUE){
        cag.uniform <- reverse.cag(xx_choice,xmin=xmin,xmax=xmax)
      } else {
        cag.uniform <- xx_choice
      }
      index_xx_use <- which(round(xx_val[[ss]],3)%in%round(cag.uniform,3))
      index_tt_use <- which(round(time_val,3)%in%time_choice)
      color.list <- as.vector(colors.use)
      label.names <- s.names.short


      for(nn_comp in 1:length(nn.comparison)){


        if(show.results.description==TRUE){

          ##############################################################
          cat("\n\n Plot of F(t|x,z) over t at different event types")    ##
          cat("\n\n Plot compares different event types at different (x,z) combinations.")	##
          cat("\n\n Plot shown for different studies separately.") 	    	  ##
          ##############################################################

        }


        index_a <- s.names[nn.comparison[[nn_comp]]]

        for(zz in 1:z.choice){

          tmp <- 0
          for(i in index_xx_use){
            tmp <-tmp+1
            filename.set <- paste(filename_new,"Ft_ss_",study.names[ss],"_nn_plot_",comp,
                                  "_z",zz,"_diff_events_x",alphax_lab[tmp],sep="")

            #plot.at.a.over.b(filename=filename.set,
            #estimate=adrop(Ft.array$out.mono.mean[ss,index_a,zz,i,,"est",drop=FALSE],
            #		drop=c(1,3,4,6)),
            #theta.array.lo=adrop(Ft.array$out.mono.mean[ss,index_a,zz,i,,var.lo,drop=FALSE],
            #		drop=c(1,3,4,6)),
            #theta.array.hi=adrop(Ft.array$out.mono.mean[ss,index_a,zz,i,,var.hi,drop=FALSE],
            #		drop=c(1,3,4,6)),
            #			index_a=index_a,
            #			bvalues=time_val,
            #			bvalues.cut=time.cut,
            #			color.list=color.list,
            #			cex.lab=3,cex.axis=3,lwd=5,cex.legend=3,
            #			legend.position=legend.position,
            #			ylim=ylim.setting["Ft",],
            #			conf.int=conf.int.use,label.names=label.names,
            #			ylab.use=study.ylab,
            #               xlab.use=study.xlab,
            #              add.second.legend=add.second.legend
            #		)

            ## ggplot version

            #####################################
            # no examples but add documentations#
            #####################################

            ggplot_at_a_over_b(filename=paste("gg_",filename.set,sep=""),
                               estimate=adrop(Ft.array$out.mono.mean[ss,index_a,zz,i,,"est",drop=FALSE],
                                              drop=c(1,3,4,6)),
                               theta.array.lo=adrop(Ft.array$out.mono.mean[ss,index_a,zz,i,,var.lo,drop=FALSE],
                                                    drop=c(1,3,4,6)),
                               theta.array.hi=adrop(Ft.array$out.mono.mean[ss,index_a,zz,i,,var.hi,drop=FALSE],
                                                    drop=c(1,3,4,6)),
                               index_a=index_a,
                               bvalues=time_val,
                               bvalues.cut=time.cut,
                               color.list=color.list,
                               nrisk=adrop(number.at.risk[ss,index_a,zz,i,,drop=FALSE],drop=c(1,3,4)),
                               margin.move=unit(c(0,-100,0,0),"mm"),
                               ##cex.line=1,cex.size=12,
                               ylim=ylim.setting["Ft",],
                               conf.int=conf.int.use,label.names=label.names,
                               ylab.use=study.ylab,
                               xlab.use=study.xlab,
                               add.second.legend=add.second.legend)
          }

          if(plot.parameters==TRUE){

            if(show.results.description==TRUE){

              ###########################################################
              cat("\n\n Plot of alpha(x,t) over t at different event types")  	       ##
              cat("\n\n Plot compares different event types at different x's")	##
              cat("\n\n Plot shown for different studies separately.") 	       ##
              ###########################################################

            }

            tmp <- 0
            for(i in index_xx_use){
              tmp <-tmp+1
              ##x11()
              ## This would need to be adjusted if la >1
              #plot.at.a.over.b(filename=paste(filename_new,"alpha_ss_",study.names[ss],
              #                                "_nn_comp_",comp,"_diff_events_x",alphax_lab[tmp],sep=""),
              #                 estimate=adrop(alpha.array$out.mean[ss,index_a,i,,la,"est",drop=FALSE],
              #                                drop=c(1,3,5,6)),
              #                 theta.array.lo=adrop(alpha.array$out.mean[ss,index_a,i,,la,var.lo,drop=FALSE],
              #                                      drop=c(1,3,5,6)),
              #                 theta.array.hi=adrop(alpha.array$out.mean[ss,index_a,i,,la,var.hi,drop=FALSE],
              #                                      drop=c(1,3,5,6)),
              #                 index_a=index_a,
              #                 bvalues=time_val,
              #                 bvalues.cut=time.cut,
              #                 color.list=color.list,
              #                 cex.lab=3,cex.axis=3,lwd=5,cex.legend=3,
              #                 legend.position=legend.position,
              #                 ylim=ylim.setting["alphat",],
              #                 conf.int=conf.int.use,label.names=label.names)

              ggplot_at_a_over_b(filename=paste("gg_",
                                                filename_new,"alpha_ss_",study.names[ss],
                                                "_nn_plot_",comp,"_diff_events_x",alphax_lab[tmp],sep=""),
                                 estimate=adrop(alpha.array$out.mean[ss,index_a,i,,la,"est",drop=FALSE],
                                                drop=c(1,3,5,6)),
                                 theta.array.lo=adrop(alpha.array$out.mean[ss,index_a,i,,la,var.lo,drop=FALSE],
                                                      drop=c(1,3,5,6)),
                                 theta.array.hi=adrop(alpha.array$out.mean[ss,index_a,i,,la,var.hi,drop=FALSE],
                                                      drop=c(1,3,5,6)),
                                 index_a=index_a,
                                 bvalues=time_val,
                                 bvalues.cut=time.cut,
                                 color.list=color.list,
                                 nrisk=NULL,
                                 ## to make y-label closer to y-axis
                                 margin.move=unit(c(0,0,0,0),"mm"),
                                 ##cex.line=1,cex.size=12,
                                 ylim=ylim.setting["alphat",],
                                 conf.int=conf.int.use,
                                 label.names=label.names,
                                 ylab.use=alphat.ylab,
                                 xlab.use=alphat.xlab,
                                 add.second.legend=add.second.legend)


            }
          }



          if(plot.parameters==TRUE){

            if(show.results.description==TRUE){

              ###########################################################
              cat("\n\n Plot of alpha(x,t) over x at different event types")    ##
              cat("\n\n Plot compares different event types at different t's") ##
              cat("\n\n Plot shown for different studies separately.") 	       ##
              ###########################################################
            }

            ## b-value is over (converted) CAG repeats x
            if(convert.x==TRUE){
              b.use <- convert.cag(xx_val[[ss]],xmin=xmin,xmax=xmax)
            } else {
              b.use <- xx_val[[ss]]
            }

            tmp <- 0
            for(i in index_tt_use){
              tmp <-tmp+1
              #x11()

              ## This would need to be adjusted if la >1
              # plot.at.a.over.b(filename=paste(filename_new,"alpha_ss_",study.names[ss],
              #                                 "_nn_plot_",comp,"_diff_events_t",alphat_lab[tmp],sep=""),
              #                  estimate=adrop(alpha.array.new$out.mean[ss,index_a,i,,la,"est",drop=FALSE],
              #                                 drop=c(1,3,5,6)),
              #                  theta.array.lo=adrop(alpha.array.new$out.mean[ss,index_a,i,,la,var.lo,drop=FALSE],
              #                                       drop=c(1,3,5,6)),
              #                  theta.array.hi=adrop(alpha.array.new$out.mean[ss,index_a,i,,la,var.hi,drop=FALSE],
              #                                       drop=c(1,3,5,6)),
              #                  index_a=index_a,
              #                  bvalues=b.use,
              #                  bvalues.cut=NULL,
              #                  color.list=color.list,
              #                  cex.lab=3,cex.axis=3,lwd=5,cex.legend=3,
              #                  legend.position=legend.position,
              #                  ylim=ylim.setting["alphax",],
              #                  conf.int=conf.int.use,label.names=label.names)


              ggplot_at_a_over_b(filename=paste("gg_",
                                                filename_new,"alpha_ss_",study.names[ss],
                                                "_nn_plot_",comp,"_diff_events_t",alphat_lab[tmp],sep=""),
                                 estimate=adrop(alpha.array.new$out.mean[ss,index_a,i,,la,"est",drop=FALSE],
                                                drop=c(1,3,5,6)),
                                 theta.array.lo=adrop(alpha.array.new$out.mean[ss,index_a,i,,la,var.lo,drop=FALSE],
                                                      drop=c(1,3,5,6)),
                                 theta.array.hi=adrop(alpha.array.new$out.mean[ss,index_a,i,,la,var.hi,drop=FALSE],
                                                      drop=c(1,3,5,6)),
                                 index_a=index_a,
                                 bvalues=b.use,
                                 bvalues.cut=NULL,
                                 color.list=color.list,
                                 nrisk=NULL,
                                 ## to make y-label closer to y-axis
                                 margin.move=unit(c(0,0,0,0),"mm"),
                                 ##cex.line=1,cex.size=12,
                                 ylim=ylim.setting["alphax",],
                                 conf.int=conf.int.use,
                                 label.names=label.names,
                                 ylab.use=alphax.ylab,
                                 xlab.use=alphax.xlab,
                                 add.second.legend=add.second.legend)
            }

          }


          if(plot.parameters==TRUE){

            if(show.results.description==TRUE){

              ###########################################################
              cat("\n\n Plot of beta(t) vs over t") 				       ##
              cat("\n\n Plot compares beta(t) for different event types.")      ##
              cat("\n\n Plot shown for different studies separately.") 	       ##
              ###########################################################

            }

            tmp <- 0
            for(i in 1:length(param.label)){
              tmp <-tmp+1
              ##x11()
              # plot.at.a.over.b(filename=paste("ss_",study.names[ss],
              #                                 "_nn_plot_",comp,"_diff_events_",filename_new,param.label[i],sep=""),
              #                  estimate=adrop(beta.array$out.mean[ss,index_a,,i,"est",drop=FALSE],
              #                                 drop=c(1,4,5)),
              #                  theta.array.lo=adrop(beta.array$out.mean[ss,index_a,,i,var.lo,drop=FALSE],
              #                                       drop=c(1,4,5)),
              #                  theta.array.hi=adrop(beta.array$out.mean[ss,index_a,,i,var.hi,drop=FALSE],
              #                                       drop=c(1,4,5)),
              #                  index_a=index_a,
              #                  bvalues=time_val,
              #                  bvalues.cut=time.cut,
              #                  color.list=color.list,
              #                  cex.lab=3,cex.axis=3,lwd=5,cex.legend=3,
              #                  legend.position=legend.position,
              #                  ylim=ylim.setting[param.label[i],],
              #                  conf.int=conf.int.use,label.names=label.names)

              ## ggplot?

            }
          }
        }
      }
    }
  }

  #############################################################
  ## Plots comparing studies for each event type separately  ##
  #############################################################

  if(real_data==TRUE && plot.KM==FALSE){

    for(nn in 1:np){
      legend.position <- legend.position.use

      ## Assume xx_val same for all studies
      ss <- 1
      if(convert.x==TRUE){
        cag.uniform <- reverse.cag(xx_choice,xmin=xmin,xmax=xmax)
      } else {
        cag.uniform <- xx_choice
      }
      index_xx_use <- which(round(xx_val[[ss]],3)%in%round(cag.uniform,3))
      index_tt_use <- which(round(time_val,3)%in%time_choice)
      color.list <- 1:length(study.names)
      label.names <- toupper(study.names)

      if(show.results.description==TRUE){

        ##############################################################
        cat("\n\n Plot of F(t|x,z) over t at different studies")    ##
        cat("\n\n Plot compares different studies different (x,z) combinations.")	##
        ##############################################################

      }

      index_a <- 1:length(study.names)

      for(zz in 1:z.choice){
        tmp <- 0
        for(i in index_xx_use){
          tmp <-tmp+1
          ##x11()
          filename.use <- paste(filename_new,
                                "Ft_",s.names[nn],
                                "_z",zz,"_x",alphax_lab[tmp],
                                "_plot_",comp,"_diff_study",
                                sep="")

          #plot.at.a.over.b(filename=filename.use,
          #estimate=adrop(Ft.array$out.mono.mean[index_a,nn,zz,i,,"est",drop=FALSE],
          #		drop=c(2,3,4,6)),
          #theta.array.lo=adrop(Ft.array$out.mono.mean[index_a,nn,zz,i,,var.lo,drop=FALSE],
          #		drop=c(2,3,4,6)),
          #theta.array.hi=adrop(Ft.array$out.mono.mean[index_a,nn,zz,i,,var.hi,drop=FALSE],
          #		drop=c(2,3,4,6)),
          #			index_a=index_a,
          #			bvalues=time_val,
          #			bvalues.cut=time.cut,
          #			color.list=color.list,
          #			cex.lab=3,cex.axis=3,lwd=5,cex.legend=3,
          #			legend.position=legend.position,
          #			ylim=ylim.setting["Ft",],
          #			conf.int=conf.int.use,label.names=label.names,
          #			ylab.use=study.ylab,
          #			xlab.use=study.xlab,
          #			add.second.legend=add.second.legend)

          ## ggplot version
          ggplot_at_a_over_b(filename=paste("gg_",filename.use,sep=""),
                             estimate=adrop(Ft.array$out.mono.mean[index_a,nn,zz,i,,"est",drop=FALSE],
                                            drop=c(2,3,4,6)),
                             theta.array.lo=adrop(Ft.array$out.mono.mean[index_a,nn,zz,i,,var.lo,drop=FALSE],
                                                  drop=c(2,3,4,6)),
                             theta.array.hi=adrop(Ft.array$out.mono.mean[index_a,nn,zz,i,,var.hi,drop=FALSE],
                                                  drop=c(2,3,4,6)),
                             index_a=index_a,
                             bvalues=time_val,
                             bvalues.cut=time.cut,
                             color.list=color.list,
                             nrisk=adrop(number.at.risk[index_a,nn,"zz-study",i,,drop=FALSE],
                                         drop=c(2,3,4)),
                             ##cex.line=1,cex.size=12,
                             ylim=ylim.setting["Ft",],
                             conf.int=conf.int.use,label.names=label.names,
                             ylab.use=study.ylab,
                             xlab.use=study.xlab,
                             add.second.legend=add.second.legend)
        }



        if(plot.parameters==TRUE){

          if(show.results.description==TRUE){

            ###########################################################
            cat("\n\n Plot of alpha(x,t) over t at different event types\n\n")  	       ##
            ###########################################################

          }

          tmp <- 0
          for(i in index_xx_use){
            tmp <-tmp+1
            ##x11()
            ## this needs to be adjusted if la > 1
            #plot.at.a.over.b(filename=paste(filename_new,
            #"alpha_",s.names[nn],"_x",alphax_lab[tmp],
            #"_comp_",comp,"_diff_study",sep=""),
            #estimate=adrop(alpha.array$out.mean[index_a,nn,i,,la,"est",drop=FALSE],
            #		drop=c(2,3,5,6)),
            #theta.array.lo=adrop(alpha.array$out.mean[index_a,nn,i,,la,var.lo,drop=FALSE],
            #		drop=c(2,3,5,6)),
            #theta.array.hi=adrop(alpha.array$out.mean[index_a,nn,i,,la,var.hi,drop=FALSE],
            #		drop=c(2,3,5,6)),
            #			index_a=index_a,
            #			bvalues=time_val,
            #			bvalues.cut=time.cut,
            #			color.list=color.list,
            #			cex.lab=3,cex.axis=3,lwd=5,cex.legend=3,
            #			legend.position=legend.position,
            #			ylim=ylim.setting["alphat",],
            #			conf.int=conf.int.use,label.names=label.names)
          }
        }

        if(plot.parameters==TRUE){

          if(show.results.description==TRUE){

            ###########################################################
            cat("\n\n Plot of alpha(x,t) over x at different event types\n\n")    ##
            ###########################################################

          }

          ## b-value is over (converted) CAG repeats x
          if(convert.x==TRUE){
            b.use <- convert.cag(xx_val[[ss]],xmin=xmin,xmax=xmax)
          } else {
            b.use <- xx_val[[ss]]
          }

          tmp <- 0
          for(i in index_tt_use){
            tmp <-tmp+1
            #x11()

            ## this needs to be adjusted if la >1
            #	plot.at.a.over.b(filename=paste(filename_new,
            #	"alpha_",s.names[nn],"_t",alphat_lab[tmp],
            #	"_comp_",comp,"_diff_study",sep=""),
            #	estimate=adrop(alpha.array.new$out.mean[index_a,nn,i,,la,"est",drop=FALSE],
            #				drop=c(2,3,5,6)),
            #theta.array.lo=adrop(alpha.array.new$out.mean[index_a,nn,i,,la,var.lo,drop=FALSE],
            #			drop=c(2,3,5,6)),
            #theta.array.hi=adrop(alpha.array.new$out.mean[index_a,nn,i,,la,var.hi,drop=FALSE],
            #			drop=c(2,3,5,6)),
            #				index_a=index_a,
            #				bvalues=b.use,
            #				bvalues.cut=NULL,
            #				color.list=color.list,
            #				cex.lab=3,cex.axis=3,lwd=5,cex.legend=3,
            #				legend.position=legend.position,
            #				ylim=ylim.setting["alphax",],
            #				conf.int=conf.int.use,label.names=label.names)
          }
        }


        if(plot.parameters==TRUE){

          if(show.results.description==TRUE){

            ###########################################################
            cat("\n\n Plot of beta(t) vs over t\n\n") 				       ##
            ###########################################################
          }

          tmp <- 0
          for(i in 1:length(param.label)){
            tmp <-tmp+1
            ##x11()
            plot.at.a.over.b(filename=paste(filename_new,param.label[i],
                                            "_",s.names[nn],
                                            "_comp_",comp,"_diff_study",sep=""),
                             estimate=adrop(beta.array$out.mean[index_a,nn,,i,"est",drop=FALSE],
                                            drop=c(2,4,5)),
                             theta.array.lo=adrop(beta.array$out.mean[index_a,nn,,i,var.lo,drop=FALSE],
                                                  drop=c(2,4,5)),
                             theta.array.hi=adrop(beta.array$out.mean[index_a,nn,,i,var.hi,drop=FALSE],
                                                  drop=c(2,4,5)),
                             index_a=index_a,
                             bvalues=time_val,
                             bvalues.cut=time.cut,
                             color.list=color.list,
                             cex.lab=3,cex.axis=3,lwd=5,cex.legend=3,
                             legend.position=legend.position,
                             ylim=ylim.setting[param.label[i],],
                             conf.int=conf.int.use,label.names=label.names)
          }
        }


      }
    }
  }



  #####################
  ## plot the counts ##
  #####################
  if(1==0){ ## for testing
    for(ss in 1:num_study){
      plot.counts(paste("ss_",study.names[ss],"_",filename_new,"beta",sep=""),time_val,
                  adrop(counts.array[,ss,,,drop=FALSE],drop=2))
    }
  }




  ###################
  ## Print results ##
  ###################

  ###################
  ## count results ##
  ###################
  if(1==0){ ## for testing

    if(show.results.description==TRUE){

      ########################################
      cat("\n\n ## Counting results \n\n")  ##
      ########################################

    }

    for(ss in 1:num_study){
      cat("\n\n ## Study ",study.names[ss],"## \n\n")
      print(xtable(apply(adrop(counts.array[,ss,,,drop=FALSE],drop=2),c(2,3),mean),
                   digits=digits.tmp))
    }
  }


  ###########################
  ## count outside results ##
  ###########################
  if(1==0){ ## for testing

    if(show.results.description==TRUE){

      ############################################################
      cat("\n\n ## Counting Pseudo-values outside [0,1] \n\n")
      ############################################################

    }

    for(ss in 1:num_study){
      cat("\n\n ## Study ",study.names[ss],"## \n\n")
      print(xtable(apply(adrop(counts.outside.array[,ss,,,drop=FALSE],drop=2),c(2,3),mean),
                   digits=digits.tmp))
    }
  } ## end test


  ########################
  ## Real study results ##
  ########################

  #####################
  ## print 95\% CI's ##
  #####################
  if(real_data==TRUE){ # & is.null(dropout.rate)!=TRUE){

    ###########
    ## alpha ##
    ###########
    if(1==0){

      alpha.cis <- get.cis(out=alpha.array$out.mean[,,,,,c("est",var.lo,var.hi),drop=FALSE],
                           flatten.name="val",
                           theta=NULL,  ## "alpha"
                           time_choice=time_choice.ci,
                           xx_val=xx_val[[1]],
                           xmin=xmin,xmax=xmax,
                           xx_choose=xx_choice.ci,
                           zz_choose=NULL,
                           convert=convert.x,
                           round.set=digits.tmp,
                           var.lo,var.hi,est="est",
                           noshow=c("est",var.lo,var.hi)
      )

    }

    ##########
    ## beta ##
    ##########
    if(1==0){

      beta.cis <- get.cis(out=beta.array$out.mean[,,,,c("est",var.lo,var.hi),drop=FALSE],
                          flatten.name="val",
                          theta=NULL,
                          time_choice=time_choice.ci,
                          xx_val=NULL,
                          xmin=xmin,xmax=xmax,
                          xx_choose=NULL,
                          zz_choose=NULL,
                          convert=convert.x,
                          round.set=digits.tmp,
                          var.lo,var.hi,est="est",
                          noshow=c("est",var.lo,var.hi))

    }

    ########
    ## Ft ##
    ########

    #####################################
    # no examples but add documentations#
    #####################################

    Ft.cis <- get.cis(out=Ft.array$out.mean[,,,,,c("est",var.lo,var.hi),drop=FALSE],
                      flatten.name="val",
                      theta="Ft",
                      time_choice=time_choice.ci,
                      xx_val=xx_val[[1]],
                      xmin=xmin,xmax=xmax,
                      xx_choose=xx_choice.ci,
                      zz_choose =z_lab_names,
                      convert=convert.x,
                      round.set=digits.tmp,
                      var.lo,var.hi,est="est",
                      noshow=c("est",var.lo,var.hi))


    if(show.results.description==TRUE){

      ######################
      cat("\n\n  Average CI length ##\n\n")
      #######################

    }


    for(xx in 1:length(xx_choice.ci)){
      for(nn in 1:np){
        for(ss in 1:num_study){

          index.use <- which(Ft.cis[,"xx"]==paste("xx",xx_choice.ci[xx],sep="") &
                               Ft.cis[,"event"]==s.names[nn] &
                               Ft.cis[,"study"]==study.names[ss])

          if(show.results.description==TRUE){
            #cat("\n\n ## Ft CIs:",study.names[ss], s.names[nn],",CAG ",xx_choice.ci[xx],":")
            #print(mean(Ft.cis[index.use,"ci.length"]))
          }
        }
      }
    }

    #######################
    ## print out results ##
    #######################

    if(1==0){

      if(show.results.description==TRUE){

        cat("###################################")
        cat("\n\n ## Ft CIs ##\n\n")
        cat("###################################")

      }

      foo <- Ft.cis
      myprint(foo)

    }



    if(1==0){

      if(show.results.description==TRUE){

        cat("###################################")
        cat("\n\n ## beta CIs for ##\n\n")
        cat("###################################")

      }

      foo <- beta.cis
      myprint(foo)
    }


    if(1==0){

      if(show.results.description==TRUE){

        cat("###################################")
        cat("\n\n ## alpha CIs ##\n\n")
        cat("###################################")

      }

      foo <- alpha.cis
      myprint(foo)
    }

    #######################
    ## Print non-significant ci's ##
    #######################

    if(show.results.description==TRUE){

      cat("###################################")
      cat("\n\n ## Ft CIs: non-sig ##\n\n")
      cat("###################################")

    }

    print.differences(out=Ft.cis,name.interest="sign.change",check.reject.H0=FALSE)



    if(1==0){

      if(show.results.description==TRUE){

        cat("###################################")
        cat("\n\n ## beta CIs: non-sig ##\n\n")
        cat("###################################")

      }

      print.differences(out=beta.cis,name.interest="sign.change",check.reject.H0=FALSE)

    }


    if(1==0){

      if(show.results.description==TRUE){

        cat("###################################")
        cat("\n\n ## alpha CIs: non-sig##\n\n")
        cat("###################################")

      }

      print.differences(out=alpha.cis,name.interest="sign.change",check.reject.H0=FALSE)

    }


    ###############################
    ## Table of Pr(T<t|T>t0)     ##
    ###############################

    ##################
    ## Ft.predicted ##
    ##################
    if(!is.null(time_choice.predicted)){
      Ft.predicted.cis <- get.cis(
        out=Ft.predicted.array$out.mean[,,,,,,c("est",var.lo,var.hi),
                                        drop=FALSE],
        flatten.name="val",
        theta="Ftpred",
        time_choice=time_choice.ci,
        xx_val=xx_val[[1]],
        xmin=xmin,xmax=xmax,
        xx_choose=xx_choice.ci,
        zz_choose =z_lab_names,
        convert=convert.x,
        round.set=digits.tmp,
        var.lo,var.hi,est="est",
        noshow=c("est",var.lo,var.hi,"sign.change"))

      if(show.results.description==TRUE){

        cat("###################################")
        cat("\n\n ## Ft predicted CIs ##\n\n")
        cat("###################################")

      }

      Ft.predicted.cis <- Ft.predicted.cis[order(Ft.predicted.cis$event),]
      foo <- Ft.predicted.cis
      myprint(foo)




      # for(dd in 1:length(dropout.rate)){
      #   dropout.rate.use <- dropout.rate[dd]
      #
      #   cat("###################################")
      #   cat("\n\n ## Estimated sample size for treatment effect
      #       using results from Ft.predicted.cis, dropout=",dropout.rate.use,"##\n\n")
      #   cat("###################################")
      #
      #
      #   sample.size.results <- get.sample.size(
      #     out.Ft=Ft.predicted.array$out.mean[,,,,,,c("est","varest"),
      #                                        drop=FALSE],
      #     type1.error,
      #     type2.error,
      #     treatment.effect,
      #     study.names,
      #     event.names=s.names,
      #     z_lab_names,
      #     x_lab_names,
      #     time_choice.predicted,
      #     time_choice.predicted.toadd,
      #     dropout.rate=dropout.rate.use
      #   )
      #
      #
      #   sample.size.results.cis <- get.cis(
      #     out=sample.size.results,
      #     flatten.name="val",
      #     theta="Ftpred",
      #     time_choice=time_choice.ci,
      #     xx_val=xx_val[[1]],
      #     xmin=xmin,xmax=xmax,
      #     xx_choose=xx_choice.sample.size,
      #     zz_choose =z_lab_names,
      #     convert=convert.x,
      #     round.set=0,
      #     var.lo="ci.lo",
      #     var.hi="ci.hi",est="sample.size",
      #     noshow=c("est",var.lo,var.hi,"sign.change"),
      #     track.sign.change=FALSE)
      #
      #
      #   sample.size.results.cis <-
      #     sample.size.results.cis[order(sample.size.results.cis$event),]
      #   foo <- sample.size.results.cis
      #   myprint(foo)
      # }

      if(do.plots==TRUE){
        for(ss in 1:num_study){
          ##legend.position <- legend.position.use

          if(study.names[ss]=="pharos" & (common.param.estimation==TRUE  ||
                                          analyze.separately!="none")){
            legend.position <- legend.position.use
          } else {
            if(paper.type=="statistics"){
              legend.position <- NULL  ## no legend
            } else {
              legend.position <- legend.position.use
            }
          }

          ## labels are at different CAG
          if(convert.x==TRUE){
            cag.uniform <- reverse.cag(xx_choice,xmin=xmin,xmax=xmax)
          } else {
            cag.uniform <- xx_choice
          }
          index_xx_use <- which(round(xx_val[[ss]],3)%in%round(cag.uniform,3))
          color.list <- as.vector(colors.use)
          label.names <- s.names.short


          for(nn_comp in 1:length(nn.comparison)){

            if(show.results.description==TRUE){

              ##############################################################
              cat("\n\n Error plots")    ##
              cat("\n\n Plot compares different event types at different (x,z) combinations.")	##
              cat("\n\n Plot shown for different studies separately.") 	    	  ##
              ##############################################################

            }

            index_a <- s.names[nn.comparison[[nn_comp]]]

            for(zz in 1:z.choice){
              tmp <- 0
              for(i in index_xx_use){
                tmp <-tmp+1

                for(tt0 in 1:length(time_choice.predicted.toadd)){
                  filename.set <- paste(filename_new,"Ft_pred_ss_",study.names[ss],"_nn_plot_",comp,
                                        "_z",zz,"_diff_events_x",alphax_lab[tmp],"_tpred",time_choice.predicted.toadd[tt0],sep="")

                  ## ggplot version

                  #####################################
                  # no examples but add documentations#
                  #####################################

                  ggplot_error_bars(filename=paste("gg_",filename.set,sep=""),
                                    estimate=adrop(Ft.predicted.array$out.mean[ss,index_a,zz,i,,tt0,"est",drop=FALSE],
                                                   drop=c(1,3,4,6,7)),
                                    theta.array.lo=adrop(Ft.predicted.array$out.mean[ss,index_a,zz,i,,tt0,var.lo,drop=FALSE],
                                                         drop=c(1,3,4,6,7)),
                                    theta.array.hi=adrop(Ft.predicted.array$out.mean[ss,index_a,zz,i,,tt0,var.hi,drop=FALSE],
                                                         drop=c(1,3,4,6,7)),
                                    index_a=index_a,
                                    bvalues=time_choice.predicted,
                                    color.list=color.list,
                                    ##ylim=ylim.setting["Ft",],
                                    ylim=c(0,0.8),
                                    conf.int=conf.int.use,label.names=label.names,
                                    ylab.use=study.ylab,
                                    xlab.use="Current Age (years)")
                }
              }

            }
          }
        }
      }
    }

    ##########################
    ## difference estiamtes ##
    ##########################
    if(num_study>1){

      if(show.results.description==TRUE){

        #######################################
        cat("\n\n ## Study comparison ##\n\n")
        #######################################

      }

      ###########
      ## alpha ##
      ###########
      if(1==0){

        alpha.diff.cis <- get.cis(
          out=alpha.diff.array$out.mean[,,,,,c("varest",var.lo,var.hi),drop=FALSE],
          flatten.name="val",
          theta="alpha",
          time_choice=time_choice.ci,
          xx_val=xx_val[[1]],
          xmin=xmin,xmax=xmax,
          xx_choose=xx_choice.ci,
          zz_choose=NULL,
          convert=convert.x,
          round.set=digits.tmp,
          var.lo,var.hi,est="varest",
          noshow=c("varest",var.lo,var.hi)
        )

      }

      ##########
      ## beta ##
      ##########
      if(1==0){


        beta.diff.cis <- get.cis(
          out=beta.diff.array$out.mean[,,,,c("varest",var.lo,var.hi),drop=FALSE],
          flatten.name="val",
          theta=NULL,
          time_choice=time_choice.ci,
          xx_val=NULL,
          xmin=xmin,xmax=xmax,
          xx_choose=NULL,
          zz_choose=NULL,
          convert=convert.x,
          round.set=digits.tmp,
          var.lo,var.hi,est="varest",
          noshow=c("varest",var.lo,var.hi))

      }


      ########
      ## Ft ##
      ########
      Ft.diff.cis <- get.cis(
        out=Ft.diff.array$out.mean[,,,,,c("varest",var.lo,var.hi),drop=FALSE],
        flatten.name="val",
        theta="Ft",
        time_choice=time_choice.ci,
        xx_val=xx_val[[1]],
        xmin=xmin,xmax=xmax,
        xx_choose=xx_choice.ci,
        zz_choose =z_lab_names,
        convert=convert.x,
        round.set=digits.tmp,
        var.lo,var.hi,est="varest",
        noshow=c("varest",var.lo,var.hi))


      ##################
      ## for printing ##
      ##################

      if(show.results.description==TRUE){

        cat("###################################")
        cat("\n\n ## Ft diff CIs ##\n\n")
        cat("###################################")

      }

      print.differences(out=Ft.diff.cis,name.interest="sign.change")



      if(1==0){

        if(show.results.description==TRUE){

          cat("###################################")
          cat("\n\n ## beta diff CIs ##\n\n")
          cat("###################################")

        }

        print.differences(out=beta.diff.cis,name.interest="sign.change")

      }

      if(1==0){

        if(show.results.description==TRUE){

          cat("###################################")
          cat("\n\n ## alpha diff CIs##\n\n")
          cat("###################################")

        }

        print.differences(out=alpha.diff.cis,name.interest="sign.change")
      }

    }
  }




}



