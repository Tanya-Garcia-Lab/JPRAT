#########################
# Begin documentation  ##
#########################
#' @title Initialization of Parameters and Coefficients
#' @description This function constructs a list of all parameters and coefficients. The structure of each parameter or coefficient is a multi-dimensional array,
#' whose dimension size depends on the number of studies, the number of events, the number of covariates and the length of time points,etc.
#' This function also initializes all parameters and coefficients to zero arrays.
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
#'                  whose dimension is determined by the length of studies, events, time points
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




####################################
## main jprat estimation function ##
####################################
#' @title Main Algorithm for JPRAT Estimation
#'
#' @description This function returns the estimated JPRAT parameters in an additive logistic mixed effect model.
#' This function also returns a bootstrap estimation procedure to do a hypothesis testing,
#' which assesses if functional terms between studies differ over a range of time points.
#' It uses the maximizing double penalized quasi-likelihood (DPQL) to estimate the JPRAT parameters and coefficients
#' and the generalized approximate cross-validation for the smoothing parameters \eqn{\lambda}.
#' This can be done using \code{gamm4} function in \code{\link{mgcv}} package in R.
#'
#' @param method A character value for which estimation procedure will be used to estimate all parameters of the additive logistic mixed effects model: "gamm4" or "new".
#' The default option is "gamm4".
#' @param compute.study.differences A logical value whether study differences will be estimated: it is only valid when the number of studies (\code{number.of.studies}) are greater than 1,
#'                                  when parameters were estimated differently (\code{estimated.parameters.common.for.all.studies=FALSE}), and when estimates are similar across studies (\code{check.study.equality=TRUE}).
#' @param var.boot A logical value whether bootstrap variances are estimated. See the argument \code{use.bootstrap.variance} in the \code{\link{jprat.wrapper}} function. The default option is TRUE.
#' @param arbitrary ADD DETAILS HERE!
#' @param num_study The number of studies used in analyses. If the real data analysis used three studies called "cohort", "predict", "pharos", then number of studies is 3. i.e., \code{num_study=3}.
#' @param np The number of clinical events, which is the length of the character vector for names of time-to-event outcomes (\code{event.outcome.names}) in \code{\link{jprat.wrapper}} function.
#' @param count.store A null object to store number of event times, which occurs before \eqn{t_0}.
#' @param count.store.outside A null object to store number of event times, which occurs before \eqn{t_0} for outside [0,1].
#' @param data A data set as starting values used for estimation procedure;
#'             a list of starting values for binary event status (y_start), the missing indicator of the binary event status (ymiss_ind_start), the nonfunctional covariate Z (z_start),
#'             the functional covariate X (x_start), the time-to-events or censoring times (s_start), the mixture probabilities for the jack-knife pseudo-values (q_start),
#'             the censoring indicators (delta_start), the original onset ages (onset_age_orig_start), the indicator of no risk (norisk_ind_start), the counting numbers for the status of each event of interests (count),
#'             the pseudo-values (ytest).
#' @param num_time A length of the vector of time points at which the predicted values of marginal distributions will be estimated.
#'                 See the argument \code{time.points.for.prediction} in the \code{\link{jprat.wrapper}} function.
#' @inheritParams all_null_theta
#' @param null.theta A list of all estimated parameters and coefficients ("beta", "alphas", "Ft" and ``Ft.predicted") in the model.
#'             Each element of the list contains arrays of zeros at the specific time points, the values of covariates and the event of interest in each study,
#'             whose dimension is determined by the length of studies, events, time points where prediction occurs, and coefficients of the nonfunctional covariates.
#' @param n A vector of the sample sizes for all studies.
#' @param nmax The maximum sample size across all studies.
#' @param m An array of the number of clinical events for each individual and per study,
#'          whose dimension is the number of studies (\code{num_study})
#'          by the maximum number of the sample sizes across studies (\code{nmax}).
#' @param maxm The maximum value of  the events across all subjects for all studies.
#' @param lb A list of the number (dimension) for the coefficients of nonfunctional covariate Z (see the argument \code{functional.beta.coefficients}) per study.
#' @param xks A list of studies whose element of the list is the numeric vector of values for the functional covariates \eqn{X} in smooth functional parameter \eqn{\alpha(X, t)}.
#' @param truth A list of true model coefficients for "beta" (\eqn{\beta_{0}(t), \beta_{es}(t)}), "alphas" \eqn{\alpha(X,t)}, "Ft" (\eqn{F_{es}(t|X,Z)}), "Ft.predicted", "beta.diff", and "Ft.diff".
#' @param num_xx The number of functional covariate values \eqn{X} at which the functional parameters \eqn{\alpha(X, t)} are evaluated.
#' @param knot.length The number of knots used to construct the basis functions.
#' @param real_data A logical value whether the real data will be used. The default option is TRUE.
#' @param family.data A logical value if the clusters are families, where each member in the cluster is different.
#' @param zeval An array of the values of the nonfunctional covariate Z for all studies,
#'              where the order of dimensions for this array is the number of studies (\code{num_study}),
#'              the number of nonfunctional covariate \eqn{Z} (\code{z.choice}),
#'              the length of the character vector for the names of time-to-event outcomes (\code{event.outcome.names})
#'              and the number of coefficients of nonfunctional covariate Z (\code{functional.beta.coefficients}) per study.
#' @param z.choice The number of nonfunctional covariate \eqn{Z} used in the analysis.
#' @param param.label A character vector of the names for all coefficients of nonfunctional covariate Z in the model.
#' @param beta0int A fixed number for the intercept \eqn{\beta_0(t)} in the model. The default value is 0.25.
#' @param gamma.param  A numeric vector of event specific coefficient, \eqn{\gamma_{e}(t)}. The default value is NULL.
#' @param omega.param A numeric vector of study specific coefficient, \eqn{\omega_s(t)}. The default value is NULL.
#' @param spline.constrain A logical value if B-spline is constrained at 0. To keep the design matrix correct, we need to have \eqn{B(0)=0}. The default option is TRUE.
#' @param common.param.estimation A logical value whether the model parameters are the same across studies: The default option is FALSE. See the argument \code{estimated.parameters.common.for.all.studies} in the \code{\link{jprat.wrapper}} function.
#' @param est.cens.depend.z A logical value whether the estimation process assumes that a censoring distribution depends on nonfunctional covariates \eqn{Z}.
#'                          If covariates Z does not follow a binomial distribution, then the default option is FALSE. See the argument \code{estimation.when.censoring.depends.on.z} in the \code{\link{jprat.wrapper}} function.
#' @param par_fu A vector of parameters for the normal distribution of random effect for all studies: "mean" and "sd".
#' @param analyze.separately  A character value to determine whether analysis will be performed separately or jointly: the options are "studyevents" (studies and event), "studies", "events", or "none". See the argument \code{what.analyzed.separately} in the \code{\link{jprat.wrapper}} function.
#' @param link.type A character value for the name of the link function for the additive mixed effect model (the generalized linear model): "logit” for proportional odds model or "cloglog" for cox proportional hazards. See the argument \code{glm.link.type} in the \code{\link{jprat.wrapper}} function.
#' @param z.proportions The marginal probability of the nonfunctional coavariate \eqn{Z}. The default value is NULL.
#' @param betaest.store An array to store the estimated functional parameters \eqn{\beta(t)} (beta) at each time point. See the return value \code{null.theta.simus.est.ciboot}
#'                       in the \code{\link{all_null_theta}} function.
#' @param alphasest.store An array to store the estimated smooth functional parameter \eqn{\alpha(X,t)} (alphas). See the return value \code{null.theta.simus.est.ciboot} in the \code{\link{all_null_theta}} function for the dimension of this array.
#' @param Ftest.store An array to store the estimated marginal distribution \eqn{F_{es}(t)} (Ft). See the return value \code{null.theta.simus.est.ciboot} in the \code{\link{all_null_theta}} function for the dimension of this array.
#' @param Ftest.predicted.store An array of values for the predicted monotone marginal distributions \eqn{F_{es}(t|X, Z, t>t_{0})}
#'                              beyond time \eqn{t_{0}} (\code{Ftest.predicted}) in the \code{\link{gamm4.estimates}} function
#'                              and the return value \code{Ft.predicted.var.boot} in the \code{\link{boot.compare.studies}} function at each iteration.
#'                              See the return value \code{null.theta.simus.est.ciboot} in the \code{\link{all_null_theta}} function for the dimensions of the array.
#' @param combi.study The total number of distinct studies to be compared.
#' @param combi.choice A matrix of all combination of two pairs of studies to be compared (paired in column-wise), whose dimension is 2 by the total number of studies (\code{combi.study}).
#' @param combi.names A character vector of names for the pairs of study combinations. For example, cohort-predict, cohort-pharos, predict-pharos.
#' @param boot  The number of bootstrap iterations. The bootstrap procedure is to do hypothesis testing, which compares functional parameters over the time points among studies.
#'              The default value is 100. See the argument \code{number.of.bootstraps} in the \code{\link{jprat.wrapper}} function.
#' @param boot.null.theta A list of "null.theta",  "null.theta.simus", "null.theta.simus.ci", "null.theta.ci",
#'                        "null.theta.simus.est.ci", "null.theta.simus.est.ciboot", and ``null.theta.est.ci", which are return values in the \code{\link{all_null_theta}} function for the bootstrap estimates.
#'                         The label for simulation dimensions is "boot".
#' @param boot.combi.null.theta A list of "null.theta",  null.theta.simus", "null.theta.simus.ci", "null.theta.ci", "null.theta.simus.est.ci",
#'                              "null.theta.simus.est.ciboot", and "null.theta.est.ci", which are return values in the \code{\link{all_null_theta}} function
#'                               for comparing bootstrap estimates ("beta”, "alphas”, "Ft” and "Ft.predicted”) among different studies.
#'                               The label for simulation dimensions is "boot".
#' @param betabootci.store A multi-dimensional zero array to store the bootstrap estimated functional parameters \eqn{\beta(t)} for comparing study differences.
#' @param alphasbootci.store A multi-dimensional zero array to store the bootstrap estimated smooth functional parameters \eqn{\alpha(X, t)} for comparing study differences.
#' @param Ftbootci.store A multi-dimensional zero array to store the bootstrap estimated marginal distributions \eqn{Ft_{es}(t|X, Z)} for comparing study differences.
#' @param betadiff.store A multi-dimensional zero array to store the differences of the functional parameters \eqn{\beta(t)}
#'                       between a pair of studies (cohort-predict, cohort-pharos, predict-pharos) at each time point (\code{time_val}) for each clinical event per study.
#' @param alphasdiff.store A multi-dimensional zero array to store the differences of the smooth functional parameters \eqn{\alpha(X, t)}
#'                         between a pair of studies (cohort-predict, cohort-pharos, predict-pharos) at each time point (\code{time_val}) for each clinical event per study.
#' @param Ftdiff.store A multi-dimensional zero array to store the true values for the differences of the monotone marginal distribution \eqn{F_{es}(t|X,Z)}  between a pair of studies (cohort-predict, cohort-pharos, predict-pharos) at each time point (\code{time_val}) and different covariate values of \eqn{X} and \eqn{Z} for each clinical event per study.
#' @param iters The number of iterations for while loops.
#'
#'
#'
#' @return A list of
#'
#' \item{eflag}{An integer number to check if any error comes up while the JPRAT algorithm is processed.
#'       This value is 0 if there are no errors. If this value is -1, then the marginal distribution of \eqn{F_{es}(t|X, Z)} has missing values (NA).}
#' \item{iters}{The number of iterations for while loops.}
#' \item{count.store}{A data frame for the rate of event times for the uncensored, censored, uncensored but zero, and other cases,
#'         which depend on the binary status of the events for each subject: "zero" (censored), "one" (uncensored), or "others" (missing).
#'         The event times are counted at all time points (\code{time_val}) for all studies.}
#'  \item{count.store.outside}{A data frame of the rate of event times for the uncensored and the censored,
#'                                 which depend on the binary status of the events for each subject outside [0,1]:
#'                                  "zero" (censored), "one" (uncensored).  The event times are counted at all time points (\code{time_val})
#'                                  for all studies.}
#' \item{betaest.store}{An array of values for \eqn{\hat\beta(t)} at each iteration. (See the return value \code{betaest} in the
#'       \code{\link{gamm4.estimates}} function and the return value \code{beta.var.boot} in  the \code{\link{boot.compare.studies}} function.)}
#' \item{alphasest.store}{An array of values for the estimated smooth functional parameters \eqn{\alpha(X, t)}
#'      (See the return value \code{alphasest} in the \code{\link{gamm4.estimates}} function and
#'       the return value \code{alphas.var.boot} in the \code{\link{boot.compare.studies}} function) at each iteration.
#'       }
#' \item{Ftest.store}{An array of values for the estimated monotone marginal distributions \eqn{F_{es}(t|X, Z)} at each iteration.
#'                   (See the return value \code{Ftest} in the \code{\link{gamm4.estimates}} function
#'                   and the return value \code{Ft.var.boot} in the  \code{\link{boot.compare.studies}} function.)}
#' \item{Ftest.predicted.store}{An array of values for the predicted monotone marginal distributions
#'                             \eqn{F_{es}(t|X, Z, t>t_{0})} beyond time \eqn{t_0} at each iteration.
#'                             (See the return value \code{Ftest.predicted} in the \code{\link{gamm4.estimates}} function
#'                             and the return value \code{Ft.predicted.var.boot} in the \code{\link{boot.compare.studies}} function.)}
#'
#' \item{betabootci.store}{An array of the estimated functional parameters \eqn{\beta(t)} for the study differences
#' including their estimates (\code{betadiff.store}) and the bootstrap estimates at each iteration (See the return value \code{betabootci}
#' in the \code{\link{boot.compare.studies}} function).}
#' \item{alphasbootci.store}{An array of the estimated smooth functional parameters \eqn{\alpha(X, t)} for the study differences
#'                        including their estimates (\code{alphasdiff.store}) and the bootstrap estimates (see the return value \code{alphasbootci}
#'                        in the \code{\link{boot.compare.studies}} function) at each iteration.}
#' \item{Ftbootci.store}{An array of the estimated monotone marginal distribution \eqn{F_{es}(t|X,Z)}
#'                      for the study differences including their estimates (\code{Ftdiff.store})
#'                      and the bootstrap estimates (see return value \code{Ftbootci} in the \code{\link{boot.compare.studies}} function)
#'                      at each iteration. }
#' @details For the structure of the arrays of \code{betabootci.store}, \code{alphasbootci.store} and \code{Ftbootci.store}, see the return value of \code{null.theta.simus.est.ci}
#'           in the \code{\link{all_null_theta}} function.
#'          For the structure of the arrays of \code{betaest.store}, \code{alphasest.store},\code{Ftest.store}, and \code{Ftest.predicted.store},
#'          see the return value of \code{null.theta.simus.est.ciboot} in the \code{\link{all_null_theta}} function.
#' @import abind
#' @import gamm4
#'
jprat.main.estimates<-function(method="gamm4",
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
                               #For jprat estimation
                               ###########################
                               betaest.store,
                               alphasest.store,
                               Ftest.store,
                               Ftest.predicted.store,
                               ###########################
                               #For bootstraps estimation
                               ###########################
                               combi.study,
                               combi.choice,
                               combi.names,
                               boot=boot,
                               boot.null.theta=boot.null.theta,
                               boot.combi.null.theta=boot.combi.null.theta,
                               betabootci.store=betabootci.store,
                               alphasbootci.store=alphasbootci.store,
                               Ftbootci.store= Ftbootci.store,
                               betadiff.store=betadiff.store,
                               alphasdiff.store=alphasdiff.store,
                               Ftdiff.store=Ftdiff.store,
                               #######################
                               # iteration
                               #######################
                               iters=iters
){

  #################
  ## get Pr(Z=z) ##
  #################
  z.proportions <- z.proportions  ## NOT Implemented
  method<-method;
  iters<-iters;

  if(method=="gamm4"){

    #####################################
    # no examples but add documentations#
    #####################################

    gamm4.est <- gamm4.estimates(arbitrary,
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
                                 z.proportions=z.proportions)


    ## Check if there is no error in estimation procedure; 0
    eflag <- gamm4.est$eflag


    if(eflag!=-1){
      ###################
      ## store results ##
      ###################
      betaest.store[iters,,,,,c("est", "varest","varlo","varhi")] <-
        gamm4.est$betaest  ## beta estimation

      alphasest.store[iters,,,,,,c("est", "varest","varlo","varhi")] <-
        gamm4.est$alphasest  ## alpha estimation
      Ftest.store[iters,,,,,,c("est", "varest","varlo","varhi")] <-
        gamm4.est$Ftest          ## Ftest estimation

      if(!is.null(time_choice.predicted)){
        Ftest.predicted.store[iters,,,,,,,c("est", "varest","varlo","varhi")] <-
          gamm4.est$Ftest.predicted
      }


      ##################################
      # Boostrap estimation
      #
      ##################################

      if(compute.study.differences==TRUE | var.boot==TRUE){

        if(!is.null(time_choice.predicted)){
          Ftest.predicted.tmp <- adrop(Ftest.predicted.store[iters,,,,,,,
                                                             "est",drop=FALSE],
                                       drop=c(1,8))
        } else {
          Ftest.predicted.tmp <- NULL

        }

        est.values <- list(betaest.est=adrop(betaest.store[iters,,,,,"est",drop=FALSE],
                                             drop=c(1,6)),
                           alphasest.est=adrop(alphasest.store[iters,,,,,,
                                                               "est",drop=FALSE],drop=c(1,7)),
                           Ftest.est=adrop(Ftest.store[iters,,,,,,"est",drop=FALSE],drop=c(1,7)),
                           Ftest.predicted.est= Ftest.predicted.tmp)



        #####################################
        # no examples but add documentations#
        #####################################

        my.boot <- boot.compare.studies(arbitrary,
                                        combi.study,combi.choice,combi.names,
                                        num_study,boot,np,data,num_xx,num_time,
                                        time_val,time_choice.predicted,
                                        time_choice.predicted.toadd,
                                        xks,n,nmax,m,
                                        maxm,la,lb,truth,real_data,
                                        knot.length,
                                        family.data,zeval,z.choice,
                                        param.label,beta0int,gamma.param,omega.param,
                                        spline.constrain,
                                        common.param.estimation,est.cens.depend.z,
                                        par_fu,analyze.separately,link.type,
                                        boot.null.theta, boot.combi.null.theta,
                                        var.boot,
                                        compute.study.differences,
                                        est.values ,
                                        z.proportions)

        if(compute.study.differences==TRUE){
          betabootci.store[iters,,,,,c("est")] <- betadiff.store
          betabootci.store[iters,,,,,c("varest","varlo","varhi")] <- my.boot$betabootci

          alphasbootci.store[iters,,,,,,c("est")] <- alphasdiff.store
          alphasbootci.store[iters,,,,,,c("varest","varlo","varhi")] <- my.boot$alphasbootci

          Ftbootci.store[iters,,,,,,c("est")] <- Ftdiff.store
          Ftbootci.store[iters,,,,,,c("varest","varlo","varhi")] <- my.boot$Ftbootci
        }

        if(var.boot==TRUE){
          betaest.store[iters,,,,,c("boot_varest", "boot_varlo","boot_varhi")] <-
            my.boot$beta.var.boot
          alphasest.store[iters,,,,,,c("boot_varest", "boot_varlo","boot_varhi")] <-
            my.boot$alphas.var.boot
          Ftest.store[iters,,,,,,c("boot_varest", "boot_varlo","boot_varhi")] <-
            my.boot$Ft.var.boot

          if(!is.null(time_choice.predicted)){

            Ftest.predicted.store[iters,,,,,,,c("boot_varest",
                                                "boot_varlo","boot_varhi")] <-
              my.boot$Ft.predicted.var.boot


            Ftest.predicted.store[iters,,,,,,,c("varest", "varlo","varhi")] <-
              my.boot$Ft.predicted.var.boot
          }
        }
      }
      count.store <- gamm4.est$count.store
      count.store.outside <- gamm4.est$count.store.outside

    }  else{
      if(real_data==TRUE){
        stop("Data is not good.")

      }
    }
  }


  list(iters=iters, eflag=eflag,
       count.store=count.store,
       count.store.outside=count.store.outside,
       betaest.store=betaest.store,
       alphasest.store=alphasest.store,
       Ftest.store=Ftest.store,
       Ftest.predicted.store=Ftest.predicted.store,
       betabootci.store=betabootci.store,
       alphasbootci.store=alphasbootci.store,
       Ftbootci.store=Ftbootci.store

  )
}




#' @title gamm4 Estimation
#' @description This function returns the estimated jack-knife pseudo-values using the Kaplan-Meier estimator
#' and returns the estimated JPRAT parameters using the generalized additive mixed model (\code{\link{gamm4}}) in \code{\link{mgcv}}.
#' @inheritParams jprat.main.estimates
#' @inheritParams all_null_theta
#' @param null.theta A list of all estimated parameters and coefficients ("beta", "alphas", "Ft" and ``Ft.predicted") in the model.
#'                  Each element of the list contains arrays of zeros at the specific time points, the values of covariates and the event of interest in each study,
#'                  whose dimension is determined by the length of studies, events, time points
#'                  where prediction occurs, and coefficients of the nonfunctional covariates.
#'
#' @return A list of
#'
#'\item{betaest}{A multi-dimensional array of the estimated functional coefficient \eqn{\beta(t)} (est), its estimated variance(Varest), estimated lower bounds(varlo) and upper bounds(varhi) of
#'                the confidence intervals at each time point for the event of interest per study.
#'               The dimensions of the array are the number of studies, the number of clinical events per study (np),
#'            the length of the vector of time points, the length of parameter label (i.e., beta0, beta1)
#'                and the length of the character vectors of estimates such as c("est", "varest", "varlo", "varhi").}
#'\item{alphasest}{A multi-dimensional array of the estimated smooth functional coefficient \eqn{\alpha(X,t)}
#'                (est), its estimated variances (varest), estimated lower bounds (varlo)
#'                and upper bounds (varhi) of the confidence intervals at specific value of x and each time point
#'                for the event of interest per study.
#'                The dimensions of the array are the number of studies,
#'                the number of clinical events per study, the length of the functional covariate values of X,
#'                the length of the vector of time points, the length of smooth functional parameters \eqn{\alpha(X,t)} (la)
#'                and the length of the character vectors of estimates such as c("est", "varest", "varlo", "varhi").}
#'\item{count.store}{See the return value of the \code{\link{jprat.main.estimates}} function.}
#'\item{count.store.outside}{See this return value of the \code{\link{jprat.main.estimates}} function.}
#'\item{Ftest}{A multi-dimensional array of the estimated monotone marginal distribution \eqn{F_{es}(t|X, Z)} (Ft) (est),
#'             estimated variances (varest), estimated lower bounds (varlo) and upper bounds (varhi) of the confidence intervals
#'             at the specific values of covariates Z and X and at each time point for the event of interest per study.
#'             The dimensions of the array are the number of studies, the number of clinical events per study, the length of nonfunctional covariate values Z,
#'             the length of functional covariate values X and
#'             the length of the character vectors of estimates such as c("est", "varest", "varlo", "varhi").}
#'\item{Ftest.predicted}{A multi-dimensional array of the predicted values for the monotonic marginal distribution \eqn{F_{es}(t|X, Z, t>t_0)} (Ft)
#'                       beyond time \eqn{t_0} at each time point for each event of interest per study.
#'                       The dimensions of the array are the number of studies (\code{num_study}),
#'                       the number of clinical events per study,
#'                       the length of nonfunctional covariate values Z,
#'                       the length of functional covariate values X,
#'                       the predicted time points (\code{time_choice.predicted.toadd}), the choice of time points for prediction
#'                       (\code{time_choice.predicted}), and the length of the name for the estimates (val="est", "varest", "varlo", "varhi").
#'                      This array exists if there exists some time points for the prediction (\code{time_choice.predicted}).}
#' \item{eflag}{an integer number to check errors while gamm4 estimation is processed. This value is 0 if there are no errors.}
#'
#' @details The estimates depend on whether analysis will be performed separately or jointly. See the argument \code{analyze.separately}
#'          \code{\link{jprat.main.estimates}}.  For the structure of the return values (betaest, alphaest, Ftest, and Ftest.predicted), see the return value \code{null.theta.est.ci}
#'          in the \code{\link{all_null_theta}} function.
#'
#' @seealso jprat.main.estimates, all_null_theta
#'
#' @import gamm4
#' @export
#'
gamm4.estimates <- function(arbitrary, num_study, np,
                            count.store,
                            count.store.outside,
                            data,
                            num_time,
                            time_val,
                            time_choice.predicted,
                            time_choice.predicted.toadd,
                            n,nmax,m,maxm,
                            la,lb,xks,truth,
                            num_xx,knot.length,real_data,
                            family.data,
                            zeval,z.choice,
                            param.label,beta0int,gamma.param,omega.param,
                            spline.constrain,
                            common.param.estimation, est.cens.depend.z,
                            par_fu,analyze.separately,link.type,null.theta,
                            z.proportions){

  ##########################
  ## km jackknife on data ##
  ##########################
  p <- np  ## set to  number of events  : 1
  m0_qvs <- p ## set to number of events : 1



  common.out <- common.procedure(arbitrary,num_study,
                                 count.store,count.store.outside,
                                 data,num_time,
                                 time_val,p,n,nmax,m,maxm,m0_qvs,family.data,
                                 common.param.estimation,est.cens.depend.z)


  count.store <- common.out$count.store  ## after unobserved values are repalced by the jack-knife psuedo values, ynew
  count.store.outside <- common.out$count.store.outside ## ynew_orig

  n <- common.out$n
  m <- common.out$m
  ynew <- common.out$ynew
  ymiss_ind <- common.out$ymiss_ind
  z <- common.out$z
  x <- common.out$x

  ###############
  ## main part ##
  ###############
  gamm4.main.out <- gamm4.main(num_study,np,n,nmax,m,maxm,ynew,
                               ymiss_ind,z,x,
                               time_val,time_choice.predicted,time_choice.predicted.toadd,
                               lb,la,xks,truth,
                               num_time,num_xx,knot.length,real_data,
                               family.data,zeval,z.choice,
                               param.label,beta0int,gamma.param,omega.param,
                               spline.constrain,common.param.estimation,par_fu,analyze.separately,
                               link.type,null.theta,z.proportions)

  eflag <- gamm4.main.out$eflag

  betaest <- gamm4.main.out$betaest
  alphasest <- gamm4.main.out$alphasest
  Ftest <- gamm4.main.out$Ftest
  Ftest.predicted <- gamm4.main.out$Ftest.predicted


  list(betaest=betaest,
       alphasest=alphasest,
       count.store=count.store,
       count.store.outside=count.store.outside,
       Ftest=Ftest,
       Ftest.predicted=Ftest.predicted,
       eflag=eflag)
}




#' @title Bootstrap-based testing procedure
#' @description This function uses the bootstrap-based joint confidence intervals to test if functional parameters between studies differ over a range of time points.
#' It determines which model would be suitable for the JPRAT estimation.
#' @inheritParams jprat.main.estimates
#' @inheritParams all_null_theta
#' @param knot.length The number of knots used to construct the basis functions.
#' @param est.values A list of arrays of the estimates ("est") for the functional coefficient \eqn{\beta(t)},
#'        the smooth functional coefficient \eqn{\alpha(X,t)},
#'        the monotone marginal distribution \eqn{F_{es}(t|X, Z)},
#'        and the predicted values of the monotonic marginal distribution \code{F_{es}(t|X, Z, t > t0)}.
#'
#' @details This function estimates bootstrap-based joint confidence intervals to test if the functional terms between studies
#'          differ over the study time, i.e.,
#'          \eqn{H_0:\mu_{es}(t)=\mu_{es'}(t)} for all \eqn{t} vs. \eqn{H_A:\mu_{es}(t) != \mu_{es'}(t)} for at least one \eqn{t, s != s'}.
#'          The \eqn{\mu_{es}(t)} can be
#'          \itemize{
#'          \item \eqn{F_{es}(t|X, Z)} for fixed covariate values \eqn{X=x, Z=z} to test if the marginal distribution for event \eqn{e} differs
#'          between study \eqn{s} and \eqn{s'}.
#'          \item \eqn{\beta_{es}(t)} to test if the effect associated with \eqn{Z_{eis}} for event \eqn{e} differs between study \eqn{s} and \eqn{s'}.
#'          \item \eqn{\alpha_{esk}(X,t)} for a fixed \eqn{x} to test if the effect of \eqn{x} for event \eqn{e} differs between study \eqn{s} and \eqn{s'}.
#'          }
#'         For the estimated values for the functional coefficient \eqn{\beta(t)}, the smooth functional coefficient \eqn{\alpha(X,t)}, the monotone marginal distribution \eqn{F_{es}(t|X, Z)}
#'         and the predicted values of the monotonic marginal distribution \eqn{F_{es}(t|X, Z, t > t0)}, see return values \code{betaest}, \code{alphas}, \code{Ft} and \code{Ft.predicted} in the \code{\link{gamm4.estimates}} function.
#'
#' @return A list of
#'
#' \item{betabootci}{A multi-dimensional array for the differences of estimated bootstrap functional parameters \eqn{\beta(t)}
#'                   between a pair of studies at each time point for each clinical event,
#'                   which includes the estimated bootstrap variances,
#'                   the lower and upper bounds of the bootstrap confidence intervals for \eqn{\beta(t)}.
#'                   See \code{null.theta.ci} in the \code{[\link{all_null_theta}]} for the dimension of this array.}
#' \item{alphasbootci}{A multi-dimensional array for the differences of estimated bootstrap smooth functional parameters \eqn{\alpha(X, t)}
#'                     between a pair of studies at each time point and the functional covariate value X for each clinical event.
#'                     See \code{null.theta.ci} in the \code{[\link{all_null_theta}]} for the dimension of this array.}
#' \item{Ftbootci}{A multi-dimensional array for the differences of estimated bootstrap marginal distribution \eqn{Ft_{es}(t|X, Z)}
#'                 between a pair of studies at each time point, each functional covariate value X and
#'                 each nonfunctional covariate value Z for each clinical event.
#'                 See \code{null.theta.ci} in the \code{[\link{all_null_theta}]} for the dimension of this array.}
#' \item{beta.var.boot}{A multi-dimensional array for the estimated bootstrap functional parameters \eqn{\beta(t)} at each time point
#'                      and each clinical event per study, which includes values of the estimated bootstrap variances,
#'                      the lower and the upper bounds of confidence intervals for \eqn{\beta(t)}.
#'                      See \code{null.theta.ci} in the \code{[\link{all_null_theta}]} for the dimension of this array.}
#' \item{alphas.var.boot}{A multi-dimensional array for the estimated bootstrap smooth functional parameters \eqn{\alpha(X, t)} at each time point
#'                        and each functional covariate value X for each clinical event per study.
#'                        See \code{null.theta.ci} in the \code{[\link{all_null_theta}]} for the dimension of this array.}
#' \item{Ft.var.boot}{A multi-dimensional array for the estimated bootstrap marginal distribution \eqn{Ft_{es}(t|X, Z)}
#'                    at each time point, each functional covariate value X
#'                    and each nonfunctional covariate value Z for each clinical event per study.
#'                    See \code{null.theta.ci} in the \code{[\link{all_null_theta}]} for the dimension of this array.}
#' \item{Ft.predicted.var.boot}{A multi-dimensional array for the estimated predcited bootstrap marginal distribution \eqn{Ft_{es}(t|X, Z, t>t_{0})}
#'                             at each time \eqn{t},
#'                             each functional covariate value X and each nonfunctional covariate value Z for each clinical event per study
#'                             given that a subject does not experience each event by \eqn{t_0}.
#'                             See \code{null.theta.ci} in the \code{[\link{all_null_theta}]} for the dimension of this array.}
#' \item{count.store.boot}{a data frame for the bootstrap rate of event times for uncensored, censored, uncensored but zero, and other cases,
#'                         which depends on the binary status of the event for each subject: "zero" (censored), "one" (uncensored), or "others" (missing).
#'                         See "count.store" in the \code{\link{gamm4.estimates}} for details.}
#' \item{count.store.outside.boot}{a data frame of the bootstrap rate of event times for uncensored and censored,
#'                                 which depends on the binary status of the event for each subject,
#'                                 which are outside [0,1]: "zero" (censored), "one" (uncensored).
#'                                 See the argument "count.store.outside" in the \code{\link{gamm4.estimates}} for details.}
#'
boot.compare.studies <- function(arbitrary,
                                 combi.study,
                                 combi.choice,
                                 combi.names,
                                 num_study,
                                 boot,
                                 np,
                                 data,
                                 num_xx,
                                 num_time,
                                 time_val,
                                 time_choice.predicted,
                                 time_choice.predicted.toadd,
                                 xks,
                                 n,
                                 nmax,
                                 m,
                                 maxm,
                                 la,
                                 lb,
                                 truth,
                                 real_data,
                                 knot.length,
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
                                 boot.null.theta,
                                 boot.combi.null.theta,
                                 var.boot,
                                 compute.study.differences,
                                 est.values,
                                 z.proportions){ ## null

  ##############################
  ## extract estimated values ##
  ##############################
  betaest.est <- est.values$betaest.est
  alphasest.est <- est.values$alphasest.est
  Ftest.est <- est.values$Ftest.est
  Ftest.predicted.est <- est.values$Ftest.predicted.est

  ########################
  ## set up for storage ##
  ########################
  ## get null theta arrays

  ## storage for bootstrap estimates
  betaest.boot.store <- boot.null.theta$null.theta.simus$beta
  alphasest.boot.store <- boot.null.theta$null.theta.simus$alpha
  Ftest.boot.store <- boot.null.theta$null.theta.simus$Ft
  Ftest.predicted.boot.store <- boot.null.theta$null.theta.simus$Ft.predicted

  #################################
  ## storage for variance output ##
  #################################
  beta.var.boot <- boot.null.theta$null.theta.ci$beta
  alphas.var.boot <- boot.null.theta$null.theta.ci$alpha
  Ft.var.boot <- boot.null.theta$null.theta.ci$Ft
  Ft.predicted.var.boot <- boot.null.theta$null.theta.ci$Ft.predicted

  ###################################
  ## storage for study comparisons ##
  ###################################
  ## get null theta arrays
  ## storage for output from bootstrap replicates
  beta.diff <- boot.combi.null.theta$null.theta.simus$beta
  alphas.diff <- boot.combi.null.theta$null.theta.simus$alpha
  Ft.diff <- boot.combi.null.theta$null.theta.simus$Ft

  ## storage for final results
  betabootci <- boot.combi.null.theta$null.theta.ci$beta
  alphasbootci <- boot.combi.null.theta$null.theta.ci$alpha
  Ftbootci <- boot.combi.null.theta$null.theta.ci$Ft

  ## for main method
  count.store <- NULL
  count.store.outside <- NULL

  norisk_ind_orig <- data$norisk_ind_start
  y_orig <- data$y_start
  ymiss_ind_orig <- data$ymiss_ind_start
  z_orig <- data$z_start
  x_orig <- data$x_start
  s_orig <- data$s_start
  q_orig <- data$q_start
  delta_orig <- data$delta_start


  ## function to get array
  get.array <- function(x){
    return(array(0,dim=dim(x),dimnames=dimnames(x)))
  }

  norisk_ind_boot <- get.array(norisk_ind_orig)  ## 3 by 915 by 1
  y_boot <- get.array(y_orig)  # 3 by 13 (time_val) by 915 by 1
  ymiss_ind_boot <- get.array(ymiss_ind_orig)  # 3 by 13 (time_val) by 915 by 1
  z_boot <- get.array(z_orig) ## 3 by 915 by 1 by 1
  x_boot <- get.array(x_orig)  #3 by 915
  s_boot <- get.array(s_orig)   # 3 by 915 by 1
  q_boot <- get.array(q_orig) ## 3 by 1 by 915 by 1  (values of p1 for three  studies)
  delta_boot <- get.array(delta_orig) ## 3 by 915 by 1
  m_boot <- get.array(m) # 3 by 915

  bb <- 1

  ## go here June 30 2019

  ##bb <- 1000 ## for testing
  while(bb <= boot){

    ###########################
    ## get bootstrapped data ##
    ###########################
    for(ss in 1:num_study){
      index.random <- sample(1:n[ss],replace=TRUE)  ## simple random sample at each study

      norisk_ind_boot[ss,1:n[ss],] <-
        adrop(norisk_ind_orig[ss,index.random,,drop=FALSE],drop=1)  ## array to a vectrization, such as rbind of index.random, 915 by 1

      y_boot[ss,,1:n[ss],] <-
        adrop(y_orig[ss,,index.random,,drop=FALSE],drop=1)   ## array to 13 by index.random matrix, 13 by 915 by 1

      ymiss_ind_boot[ss,,1:n[ss],] <-
        adrop(ymiss_ind_orig[ss,,index.random,,drop=FALSE],drop=1) ## array to 13 by index.random matrix,  13 (time_val) by index.random by 1

      if(family.data==FALSE){
        z_boot[ss,1:n[ss],,] <- z_orig[ss,index.random,,,drop=FALSE] ##  ss by index.random by 1 by 1
        x_boot[ss,1:n[ss]] <- x_orig[ss,index.random,drop=FALSE] ## ss by index.random
      } else {
        z_boot[ss,1:n[ss],,] <- z_orig[ss,index.random,,,drop=FALSE]
        x_boot[ss,1:n[ss],] <- x_orig[ss,index.random,,drop=FALSE]
      }

      s_boot[ss,1:n[ss],] <- s_orig[ss,index.random,,drop=FALSE] # ss by index.random by 1
      q_boot[ss,,1:n[ss],] <- q_orig[ss,,index.random,,drop=FALSE]  ## ss by 1 by index.random by 1  (values of p1 for three  studies)
      delta_boot[ss,1:n[ss],] <- delta_orig[ss,index.random,,drop=FALSE] ## ss by index.random by 1
      m_boot[ss,1:n[ss]] <- m[ss,index.random] # 1 by  index.random
    }

    data.boot <- list(norisk_ind_start=norisk_ind_boot,
                      y_start=y_boot,
                      ymiss_ind_start=ymiss_ind_boot,
                      z_start=z_boot,
                      x_start=x_boot,
                      s_start=s_boot,
                      q_start=q_boot,
                      delta_start=delta_boot)


    ######################
    ## apply new_method ##
    ######################
    new.boot <- gamm4.estimates(arbitrary,num_study,np,
                                count.store,
                                count.store.outside,
                                data.boot,num_time,
                                time_val,time_choice.predicted,time_choice.predicted.toadd,
                                n,nmax,m,maxm,
                                la,lb,xks,truth,
                                num_xx,knot.length,real_data,
                                family.data,
                                zeval,z.choice,
                                param.label,beta0int,gamma.param,omega.param,
                                spline.constrain,common.param.estimation,est.cens.depend.z,
                                par_fu,analyze.separately,link.type,boot.null.theta,
                                z.proportions)

    eflag <- new.boot$eflag

    check.constraint <- function(theta){
      if(sum(apply(theta,1:length(dim(theta)),function(x) x>100))){  ## length of dimension: for example, beta.est dimension is 3 by 1 by 13 by 2 =>  length of dim=4
        return(TRUE)
      } else {
        return(FALSE)   ## each componenet of estimates compared to 100. give TRUE or FALSE.
      }
    }



    if(eflag!=-1){
      beta.est <- adrop(new.boot$betaest[,,,,"est",drop=FALSE],drop=5)  ## dimension 5 will be dropped
      alphas.est <- adrop(new.boot$alphasest[,,,,,"est",drop=FALSE],drop=6)
      count.store <- new.boot$count.store
      count.store.outside <- new.boot$count.store.outside

      Ft.est <- adrop(new.boot$Ftest[,,,,,"est",drop=FALSE],drop=6)

      if(!is.null(time_choice.predicted)){
        Ft.predicted <- adrop(new.boot$Ftest.predicted[,,,,,,"est",drop=FALSE],drop=7)
      }
      if(!check.constraint(beta.est) && !check.constraint(alphas.est) &&
         !check.constraint(Ft.est)){
        ###################
        ## store results ##
        ###################
        betaest.boot.store[bb,,,,] <- beta.est
        alphasest.boot.store[bb,,,,,] <- alphas.est
        Ftest.boot.store[bb,,,,,] <- Ft.est

        if(!is.null(time_choice.predicted)){
          Ftest.predicted.boot.store[bb,,,,,,] <- Ft.predicted
        }

        bb <- bb+1
      }
    } else {
      ## used for testing.
      cat("crazy boot values, bb=",bb,"\n")
    }
  }

  #############################
  ## get bootstrap variances ##
  #############################
  if(var.boot==TRUE){
    get.var.estimates <- function(thetaest.boot.store,thetaest.est){
      varest <- apply.index(thetaest.boot.store,"iters",var)  ## over boot
      #varlo <- apply.index(thetaest.boot.store,"iters",myquantiles.lo)  ## over boot
      #varhi <- apply.index(thetaest.boot.store,"iters",myquantiles.hi)  ## over boot
      varhi <- thetaest.est + qnorm(0.975)*sqrt(varest)
      varlo <- thetaest.est + qnorm(0.025)*sqrt(varest)
      list(varest=varest,varlo=varlo,varhi=varhi)
    }

    betabootvar <- get.var.estimates(betaest.boot.store,betaest.est)
    beta.var.boot[,,,,"varest"] <- betabootvar$varest
    beta.var.boot[,,,,"varlo"] <-  betabootvar$varlo
    beta.var.boot[,,,,"varhi"] <-  betabootvar$varhi

    alphasbootvar <- get.var.estimates(alphasest.boot.store,alphasest.est)
    alphas.var.boot[,,,,,"varest"] <- alphasbootvar$varest
    alphas.var.boot[,,,,,"varlo"] <-  alphasbootvar$varlo
    alphas.var.boot[,,,,,"varhi"] <-  alphasbootvar$varhi



    Ftbootvar <- get.var.estimates(Ftest.boot.store,Ftest.est)
    Ft.var.boot[,,,,,"varest"] <- Ftbootvar$varest
    Ft.var.boot[,,,,,"varlo"] <-  Ftbootvar$varlo
    Ft.var.boot[,,,,,"varhi"] <-  Ftbootvar$varhi



    if(!is.null(time_choice.predicted)){
      Ftpredictedbootvar <- get.var.estimates(Ftest.predicted.boot.store,
                                              Ftest.predicted.est)
      Ft.predicted.var.boot[,,,,,,"varest"] <- Ftpredictedbootvar$varest
      Ft.predicted.var.boot[,,,,,,"varlo"] <-  Ftpredictedbootvar$varlo
      Ft.predicted.var.boot[,,,,,,"varhi"] <-  Ftpredictedbootvar$varhi
    }
  }



  #####################
  ## get differences ##
  ####################
  if(compute.study.differences==TRUE){
    ## check differences between studies
    mydiff <- function(x){
      x[combi.choice[1,]]-x[combi.choice[2,]]
    }

    beta.diff[,1:combi.study,,,] <- apply.index(betaest.boot.store,"study",mydiff)
    alphas.diff[,1:combi.study,,,,] <- apply.index(alphasest.boot.store,
                                                   "study",mydiff)
    Ft.diff[,1:combi.study,,,,] <-  apply.index(Ftest.boot.store,"study",mydiff)   ### differences between s

    #######################
    ## summarize results ##
    #######################
    check.study.differences <- function(theta.diff){
      thetadiff.mean <- apply.index(theta.diff,"iters",mean)  ## over boot
      thetadiff.var <- apply.index(theta.diff,"iters",var)    ## over boot
      ## difference at each iters - mean

      thetadiff.max.tmp <- sweep(theta.diff,
                                 find.apply.index(theta.diff,"iters"),
                                 thetadiff.mean,FUN="-")

      ## abs(difference at each iters)/sqrt(var)
      thetadiff.max.tmp <- sweep(abs(thetadiff.max.tmp),
                                 find.apply.index(thetadiff.max.tmp,"iters"),
                                 sqrt(thetadiff.var),FUN="/")

      ## max over time
      thetadiff.max <- apply.index(thetadiff.max.tmp,"time",max)
      thetadiff.max.quantile <- apply.index(thetadiff.max,"iters",
                                            myquantiles)	## over boot

      thetabootci.tmp <- sweep(sqrt(thetadiff.var),
                               find.apply.index(thetadiff.var,"time"),
                               thetadiff.max.quantile,FUN="*")    ## over t
      thetadiff.lo <- thetadiff.mean - thetabootci.tmp
      thetadiff.hi <- thetadiff.mean + thetabootci.tmp
      list(thetadiff.mean=thetadiff.mean,thetadiff.lo=thetadiff.lo,
           thetadiff.hi=thetadiff.hi)
    }


    ### Results are NA when using real.data=TRUE. Since we are not using bootstrap method.

    ## beta
    betabootci.out <- check.study.differences(beta.diff)
    betabootci[,,,,"varest"] <- betabootci.out$thetadiff.mean
    betabootci[,,,,"varlo"] <-  betabootci.out$thetadiff.lo
    betabootci[,,,,"varhi"] <- betabootci.out$thetadiff.hi


    ## alphas
    alphasbootci.out <- check.study.differences(alphas.diff)
    alphasbootci[,,,,,"varest"] <- alphasbootci.out$thetadiff.mean
    alphasbootci[,,,,,"varlo"] <-  alphasbootci.out$thetadiff.lo
    alphasbootci[,,,,,"varhi"] <- alphasbootci.out$thetadiff.hi

    ## Ft
    Ftbootci.out <- check.study.differences(Ft.diff)
    Ftbootci[,,,,,"varest"] <- Ftbootci.out$thetadiff.mean
    Ftbootci[,,,,,"varlo"] <- Ftbootci.out$thetadiff.lo
    Ftbootci[,,,,,"varhi"] <- Ftbootci.out$thetadiff.hi

  }

  list( ## comparison results
    betabootci=betabootci,
    alphasbootci=alphasbootci,
    Ftbootci=Ftbootci,

    ## variance results
    beta.var.boot=beta.var.boot,
    alphas.var.boot=alphas.var.boot,
    Ft.var.boot=Ft.var.boot,
    Ft.predicted.var.boot=Ft.predicted.var.boot,

    ## method
    count.store.boot=count.store,
    count.store.outside.boot=count.store.outside)



}




####################################
# To get results                  ##
####################################
#' @title ggplots with "a number at risk" table
#' @description  This function creates ggplots for the estimated probabilities over time (age, years) with a number at risk table.
#'
#' @param filename A character string for the file name. Users can choose this file name by setting the input parameter \code{file.name.for.analysis} in the \code{\link{view.all.results}} function. The "gg" is prefixed to the file name that users choose internally.
#' @param estimate  Estimates to be plotted over specific time points.
#' @param theta.array.lo Estimated lower bounds for the confidence intervals at specific time points.
#' @param theta.array.hi Estimated upper bounds for the confidence intervals at specific time points.
#' @param index_a Index for the functional covariate values X, where to draw plot.
#' @param bvalues  A vector of values for the x-axis. Here, specifically, a vector of time points for prediction (\code{time_val}).
#' @param bvalues.cut  A cut off for \code{bvalues} where tic marks should be drawn.
#' @param color.list A character vector of default color names, which will be used in plots.
#' @param nrisk Array for the numbers at risk at each time point. This array is returned from the function \code{\link{compute.number.at.risk.for.HD}}. See example in example.R file in /man/example/.
#'              The dimension of array will be the number of nonfunctional covariate Z used in the analysis (\code{z.choice}) by the number of time points (\code{num_time}).
#' @param margin.move Margin around entire plot to make y-label closer to y-axis. The default is unit(c(0,-10,0,0), "mm").
#' @param cex.line  Magnification of thickness of the line relative to cex. The default is 1.5.
#' @param cex.size  Magnification of size of text in the legend relative to cex. The default is 20.
#' @param cex.number Magnification of size of text in the labels relative to cex. The default is 6.
#' @param ylim Limits of y-axis: minimum and maximum grids of y-axis.
#' @param conf.int A logical value whether the confidence interval will be plotted. The default is FALSE.
#' @param label.names  A character vector of names for labels in plots.
#' @param ylab.use A character vector for the y-axis label. There are options such as alphax.ylab and study.ylab,
#'                 which are character values for y-axis label depending on what parameters will be plotted.
#'                 See the argument \code{ylabel.for.plots.comparing.studies} in the \code{\link{view.all.results}} function.
#' @param xlab.use A character vector for the x-axis label. There are options: alphax.xlab and study.xlab, which are character values for x-axis label in plots.
#'                 See the argument "xlabel.for.plots.comparing.studies" in the \code{\link{view.all.results}} function.
#' @param add.second.legend  A logical value whether the second legend will be added in plots.
#'
#' @return This function returns plots with a number at risk table from all analysis results. The default is FALSE.
#'
#' @import grDevices
#' @import ggplot2
#' @import gtable
#' @import grid
#' @import gridExtra
#' @export
#'
#'
ggplot_at_a_over_b <- function(filename,
                               estimate,
                               theta.array.lo=NULL,
                               theta.array.hi=NULL,
                               index_a,
                               bvalues,#=time_val,
                               bvalues.cut,#=time.cut,
                               color.list,
                               nrisk,#=number.at.risk[index_a,nn,zz,1,],
                               ## to make y-label closer to y-axis
                               margin.move=unit(c(0,-30,0,0),"mm"),
                               cex.line=1.5,cex.size=20,cex.number=6,
                               ylim=NULL,
                               conf.int=FALSE,label.names=NULL,
                               ylab.use="",xlab.use="",
                               add.second.legend=FALSE){


  ## make sure dimensions are  correct
  estimate <- make.array(estimate)

  if(!is.null(nrisk)){
    nrisk <- make.array(nrisk)
  }

  if(!is.null(theta.array.lo)){
    theta.array.lo <- make.array(theta.array.lo)
  }

  if(!is.null(theta.array.hi)){
    theta.array.hi <- make.array(theta.array.hi)
  }

  ## update where to cut b-values : b-values 40, 45, 40, 55, 60
  if(!is.null(bvalues.cut)){
    index.cut.use <- which(as.numeric(bvalues) %in% bvalues.cut)
    if(length(index.cut.use) <2){
      index.b.cut <- 1:index.cut.use
    } else {
      index.b.cut <- seq(from=index.cut.use[1],to=index.cut.use[2],by=1)
    }
  } else {
    index.b.cut <- 1:length(bvalues) ### 1 2 3 4 5
  }

  ## create data frame with strata
  data.plot <- convert.matrix.to.data.plot(index.b.cut,index_a,
                                           bvalues,estimate,label.names)



  ## create data frame for confidence intervals
  if(conf.int==TRUE){
    data.lo.plot <- convert.matrix.to.data.plot(index.b.cut,index_a,bvalues,
                                                theta.array.lo,label.names)


    data.hi.plot <- convert.matrix.to.data.plot(index.b.cut,index_a,bvalues,
                                                theta.array.hi,label.names)
    data.plot <- cbind(data.plot,lo=data.lo.plot$y,hi=data.hi.plot$y)
  }

  x <- NULL
  y <- NULL
  Group <- NULL
  hi <- NULL
  lo <- NULL

  ## get legends for label.names : Group A VERSES y
  g <- ggplot2::ggplot(data=data.plot,
                       ggplot2::aes(x=x,y=y,group=Group))+
    ggplot2::geom_line(ggplot2::aes(color=Group),size=cex.line) +
    ggplot2::theme_bw()+ 	## no shaded area
    ggplot2::theme(legend.key = ggplot2::element_blank()) + ## no boxes around legend labels
    ggplot2::theme(legend.position = "bottom") +  	 ## legend position
    ggplot2::theme(legend.title=ggplot2::element_blank())+  ## no legend name
    ggplot2::theme(legend.text=ggplot2::element_text(size=cex.size))+  ## legend size
    ggplot2::scale_color_manual(values=color.list)

  # Extract the first legend - leg1
  grDevices::png("NUL")
  leg1 <- gtable::gtable_filter(ggplot2::ggplot_gtable(ggplot2::ggplot_build(g)), "guide-box")
  grDevices::dev.off()

  if(add.second.legend==TRUE){
    g2 <- ggplot2::ggplot(data=data.plot) +
      ggplot2::geom_line(ggplot2::aes(x=x,y=hi,linetype="Upper 95% CI"),size=cex.line) +
      ggplot2::geom_line(ggplot2::aes(x=x,y=lo,linetype="Lower 95% CI"),size=cex.line) +
      ggplot2::theme_bw() +  ## no shaded area
      ggplot2::theme(legend.key = ggplot2::element_blank()) + ## no boxes around legend labels
      ggplot2::scale_linetype_manual("",values=c(3,2),
                            labels=c("Upper 95% CI","Lower 95% CI")) +
      ggplot2::theme(legend.text=ggplot2::element_text(size=cex.size))  ## legend size

    ## Extract the second legend
    grDevices::png("NUL")
    leg2 <- gtable::gtable_filter(ggplot2::ggplot_gtable(ggplot2::ggplot_build(g2)), "guide-box")
    grDevices::dev.off()
  }

  ## main plot
  main.plot <- ggplot2::ggplot(data=data.plot,
                               ggplot2::aes(x=x,y=y,group=Group)) +
    ggplot2::geom_line(ggplot2::aes(color=Group),size=cex.line) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.key = ggplot2::element_blank()) + ## no boxes around legend labels
    ggplot2::theme(legend.position = "top") +  	 ## legend position
    ggplot2::theme(legend.title=ggplot2::element_blank())+  ## no legend name
    ggplot2::theme(legend.text=ggplot2::element_text(size=cex.size))+  ## legend size
    ggplot2::scale_color_manual(values=color.list) +
    ggplot2::xlab(xlab.use)+
    ggplot2::ylab(ylab.use)+
    ggplot2::theme(panel.grid.major=ggplot2::element_blank(),
                   panel.grid.minor=ggplot2::element_blank(),
          axis.text.x=ggplot2::element_text(size=cex.size),
          axis.text.y=ggplot2::element_text(size=cex.size),
          axis.title.x=ggplot2::element_text(size=cex.size),
          axis.title.y=ggplot2::element_text(size=cex.size,angle=90,
                                    margin=margin.move))


  if(!is.null(ylim)){
    main.plot <- main.plot + ggplot2::ylim(ylim)
  }

  if(conf.int==TRUE){
    main.plot <- main.plot +
      ggplot2::geom_line(data=data.plot,
                         ggplot2::aes(x=x,y=lo,group=Group,color=Group),
                linetype=2,size=cex.line)+
      ggplot2::geom_line(data=data.plot,
                         ggplot2::aes(x=x,y=hi,group=Group,color=Group),
                linetype=3,size=cex.line)
  }


  plotNew <- main.plot
  if(add.second.legend==TRUE){
    grDevices::png("NUL")
    plotNew <- gridExtra::arrangeGrob(main.plot, leg2,
                           widths = unit.c(unit(1, "npc") - leg2$width, leg2$width), nrow = 1) ## Set up a gtable layout to place multiple grobs on a page.
    grDevices::dev.off()
  }


  if(!is.null(nrisk)){
    ## number at risk table
    ##xtick.marks <- as.numeric(ggplot_build(main.plot)$panel$ranges[[1]]$x.labels)
    ## extract x tick marks from ggplot
    xtick.marks <- as.numeric(ggplot2::ggplot_build(main.plot)$layout$panel_ranges[[1]]$x.labels)
    index.b.cut.nrisk <- which(bvalues %in% xtick.marks)
    nrisk.tmp <- nrisk
    nrisk.tmp[,-index.b.cut.nrisk] <- ""  ## empty placeholder for non-shown values
    nrisk.plot <- convert.matrix.to.data.plot(index.b.cut,index_a,bvalues,
                                              nrisk.tmp,label.names)


    ## reverse order label.names
    nrisk.plot$Group <- factor(nrisk.plot$Group,levels=rev(levels(nrisk.plot$Group)))

    tbl <- ggplot2::ggplot(nrisk.plot,
                           ggplot2::aes(x=x,y=factor(Group),label=y))+
      ggplot2::geom_text(size=cex.number)+
      ggplot2::theme_bw()+
      ggplot2::theme(
        legend.position = "none",
        plot.background = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        axis.line = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(size=cex.size, color = 'black'),
        axis.ticks = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        plot.title = ggplot2::element_blank()
      )

    grDevices::png("NUL")
    both = rbind(ggplot2::ggplotGrob(main.plot), ggplot2::ggplotGrob(tbl), size="last")
    panels <- both$layout$t[grep("panel", both$layout$name)]
    #both$heights[panels] <- list(unit(1,"null"), unit(4, "lines"))  ## unit(4,"lines") adds space between table rows
    both$heights[panels] <- unit.c(unit(1,"null"),unit(4,"lines"))
    both <- gtable::gtable_add_rows(both, heights = unit(1,"line"),2)
    both <- gtable::gtable_add_grob(both,
                            grid::textGrob("Number at risk", hjust=0, x=0,
                                     gp=grid::gpar(fontsize=cex.size)),
                            t=11, l=2, r=4)
    grDevices::dev.off()
    ## Put tables, legends, plots together!
    grDevices::png("NUL")
    plotNew <- arrangeGrob(leg1, both,
                           heights = unit.c(leg1$height, unit(1, "npc") - leg1$height), ncol = 1)
    grDevices::dev.off()


    if(add.second.legend==TRUE){
      grDevices::png("NUL")
      plotNew <- gridExtra::arrangeGrob(both, leg2,
                             widths = unit.c(unit(1, "npc") - leg2$width, leg2$width), nrow = 1)
      grDevices::dev.off()
    }

  }
  ##postscript(paste(filename,".eps",sep=""))
  #print(paste(filename,".eps",sep=""))
  #x11()
  grid::grid.draw(plotNew)
  ##dev.off()

}



#######################
## ggplot error bars ##
###################################################
## plot of function(a,b) over b at different a   ##
##   add in number at risk using ggplot          ##
###################################################
#' @title Dot plot with error bars
#' @description  This function creates dot plot (predicted probabilities) with error bars.
#' @inheritParams ggplot_at_a_over_b
#'
#' @return dot plots for predicted probabilites with errors bars for each event occurred at specific time points (\code{bvalues}).
#'
#' @import ggplot2
#'
ggplot_error_bars <- function(filename,
                              estimate,
                              theta.array.lo=NULL,
                              theta.array.hi=NULL,
                              index_a,
                              bvalues,#=time_val,
                              color.list,
                              cex.line=1.5,cex.size=20,cex.number=6,
                              ylim=NULL,
                              conf.int=FALSE,label.names=NULL,
                              ylab.use="",xlab.use=""){

  ## make sure dimensions are  correct
  estimate <- make.array(estimate)


  if(!is.null(theta.array.lo)){
    theta.array.lo <- make.array(theta.array.lo)
  }

  if(!is.null(theta.array.hi)){
    theta.array.hi <- make.array(theta.array.hi)
  }

  ## update where to cut b-values
  index.b.cut <- 1:length(bvalues)

  ## create data frame with strata
  data.plot <- convert.matrix.to.data.plot(index.b.cut,index_a,
                                           bvalues,estimate,label.names)


  ## create data frame for confidence intervals
  if(conf.int==TRUE){
    data.lo.plot <- convert.matrix.to.data.plot(index.b.cut,index_a,bvalues,
                                                theta.array.lo,label.names)


    data.hi.plot <- convert.matrix.to.data.plot(index.b.cut,index_a,bvalues,
                                                theta.array.hi,label.names)
    data.plot <- cbind(data.plot,lo=data.lo.plot$y,hi=data.hi.plot$y)
  }


  x <- NULL
  y <- NULL
  Group <- NULL
  lo <- NULL
  hi <- NULL

  ## get legends for label.names
  main.plot <- ggplot2::ggplot(data=data.plot,
                               ggplot2::aes(x=x,y=y,group=Group,fill=Group,color=Group))+
    ggplot2::geom_errorbar(ggplot2::aes(ymin=lo, ymax=hi), width=1,size=cex.line,
                  position=ggplot2::position_dodge(.9)) +
    ggplot2::geom_point(size=4,shape=19,
                        position=ggplot2::position_dodge(.9))+
    ggplot2::theme_bw()+ 	## no shaded area
    ggplot2::theme(legend.key = ggplot2::element_blank()) + ## no boxes around legend labels
    ggplot2::theme(legend.position = "top") +  	 ## legend position
    ggplot2::theme(legend.title=ggplot2::element_blank())+  ## no legend name
    ggplot2::theme(legend.text=ggplot2::element_text(size=cex.size))+  ## legend size
    ggplot2::scale_color_manual(values=color.list)+
    ggplot2::xlab(xlab.use)+
    ggplot2::ylab(ylab.use)+
    ggplot2::theme(panel.grid.major=ggplot2::element_blank(),
                   panel.grid.minor=ggplot2::element_blank(),
          axis.text.x=ggplot2::element_text(size=cex.size),
          axis.text.y=ggplot2::element_text(size=cex.size),
          axis.title.x=ggplot2::element_text(size=cex.size),
          axis.title.y=ggplot2::element_text(size=cex.size,angle=90))+
    ggplot2::scale_x_discrete(limits=bvalues,breaks=bvalues,
                     labels=bvalues)

  if(!is.null(ylim)){
    main.plot <- main.plot + ggplot2::ylim(ylim)
  }


  ##postscript(paste(filename,".eps",sep=""))
  #print(paste(filename,".eps",sep=""))
  #x11()
  grid::grid.draw(main.plot)
  ##dev.off()

}



#############################################
## get 95\% CI for time points of interest ##
##   and lb/xx of interest                 ##
#############################################
#' @title 95\% Confidence interval for time points of interest
#' @description  This function generates a data frame, which contains the confidence intervals
#'               for the estimates and the length of confidence intervals at each time for specific values of covariates for all studies.
#' @param out An array of estimates to plot.
#' @param flatten.name A character value of the flatten name.
#' @param theta A character value for the name of the component estimated: "beta" (coefficients of the nonfunctional covariates Z),
#'              "alphas" (coefficients of the functional covariates X),  "F" (monotone marginal distributions \eqn{F_{es}(t|Z, X)})
#'               or "Ft.pred" (the predicted marginal distribution \eqn{F_{es}(t|Z, X, t>t_{0})}.
#' @param time_choice  A vector of time points at which confidence intervals of estimates will be predicted. See the argument \code{time.points.of.interest.ci} in the \code{\link{view.all.results}}.
#' @param xx_val A vector of values for the functional covariate values of X per a study. See the argument \code{functional.covariate.values.for.prediction} in the \code{\link{view.all.results}} function.
#' @param xmin Minimum value for the functional covariate values of X.
#' @param xmax Maximum value for the functional covariate values of X.
#' @param xx_choose A vector of specific functional covariate values of X at which confidence interval of the smooth functional parameter \eqn{\alpha(x,t)} will be predicted.
#'                  The default is "Null" for the parameter "beta".
#' @param zz_choose a vector of character values for the names of nonfunctional covariates Z.
#'                  For example, zz_choose=z_lab_names. Null for the parameter "beta" and "alphas".
#' @param convert A logical value whether the CAG repeat lengths will be scaled into the uniformly distributed values on [0,1].
#' @param round.set The number of significant digits to use in the plot: Default is 2.
#' @param var.lo A character value for the name of the lower bound for the confidence intervals: "varlo".
#' @param var.hi A character value for the name of the upper bound for the confidence intervals: "varhi".
#' @param est A character value for the estimates, e.g., "est".
#' @param noshow A character vector of column names, which will not be in the data frame.
#' @param track.sign.change A logical value whether the sign change will be tracking. The default is TRUE.
#'
#' @details The data for "noshow" will not be shown in the data frame. To do the confidence interval test,
#'          the signs of the lower bounds and upper bounds of the confidence intervals will be observed.
#'          If the signs of the lower bounds and the upper bounds for the confidence intervals are the same,
#'          the \code{sign.change} returns TRUE (or 1); otherwise, FALSE (or 0).
#'          Those values will be used to test the hypothesis test whether the study results are similar, i.e., whether the true model shares the study parameters.
#' @return
#' \item{out.flatten}{A data frame, created from the array \code{out} using the \code{flatten.array} function.
#'                    The data frame contains the confidence intervals for the estimates (ci)
#'                    (whether the signs of the lower bounds and upper bounds of the confidence intervals for the estimates will be changed (\code{sign.change})),
#'                    the length of confidence intervals (\code{ci.length}) at specific time points (\code{time_choice}),
#'                    covariates values (\code{xx_choose}, \code{zz_choose}), and the clinical events of interest for all studies.}
#' @export
#'
get.cis <- function(out,flatten.name,
                    theta="alpha",
                    time_choice,
                    xx_val,xmin,xmax,
                    xx_choose,zz_choose=NULL,
                    convert,round.set,var.lo,var.hi,est,
                    noshow=NULL,
                    track.sign.change=TRUE){


  ## get array of information
  out.flatten <- flatten.array(out,dim.order=names(dimnames(out)),
                               flatten.name=flatten.name,theta) ## array to matrix

  ## extract indices of interest
  if(!is.null(xx_choose)){
    xx_index <- xx_choose
  } else {
    xx_index <- NULL
  }

  ## extract needed index
  index.use <- extract.index(tt_choose=time_choice,
                             xx_choose,xx_index,zz_choose,out.flatten)

  out.flatten <- out.flatten[index.use,]




  ## create estimate (ci_lo, ci_hi)
  ci <- paste(round(out.flatten[,est],round.set) ,"(",
              round(out.flatten[,var.lo],round.set), ",",
              round(out.flatten[,var.hi],round.set),")",sep="")  ## estimtes (lower, upper)



  out.flatten <- cbind(out.flatten,ci=as.character(ci)) ## consider the estimates with confidence interval as chracter value



  ## length of ci
  ci.length <- round(out.flatten[,var.hi]-out.flatten[,var.lo],round.set)

  ## track sign changes
  sign.change <- rep(NA,nrow(out.flatten))

  if(track.sign.change==TRUE){

    for(i in 1:nrow(out.flatten)){

      if(sign(out.flatten[i,var.lo])!=0 & sign(out.flatten[i,var.hi])!=0){
        ## signs are the same, CI EXcludes 0, Reject H0
        sign.change[i] <-
          sign(out.flatten[i,var.lo]) == sign(out.flatten[i,var.hi])  ## signs are the same?
      } else {
        sign.change[i] <- 0                                  ## other wise 0
      }
    }
  }

  out.flatten <- cbind(out.flatten,sign.change=sign.change,ci.length=ci.length)

  ## what columns to show other than noshow
  if(!is.null(noshow)){
    colmns.to.show <- which(!colnames(out.flatten) %in% noshow) ## not matches with noshow
    out.flatten <- out.flatten[,c(colmns.to.show)]
  }

  rownames(out.flatten) <- NULL
  return(out.flatten)
}



#######################################
# data summary                        #
#######################################
####################################
## get number at risk for HD data ##
####################################
#' @title Number at risk for HD data
#' @description This function returns the number of subjects who are at risk for HD for each study at times.
#' @inheritParams jprat.wrapper
#'
#' @return a table for the number of subjects who are at risk for HD for each study at times.
#' @import survival
#'
#'
compute.number.at.risk.for.HD <- function(study.names,
                                          input.data.list,
                                          event.outcome.names,
                                          nonfunctional.covariate.names,
                                          functional.covariate.names,
                                          nonfunctional.covariate.value,
                                          functional.covariate.values.of.interest,
                                          #nonfunctional.covariate.values.for.prediction,#
                                          #functional.covariate.values.for.prediction,#
                                          time.points.for.prediction,
                                          estimated.parameters.common.for.all.studies,
                                          write.output
){


  ###############################################
  # Obtain default options for JPRAT analysis ###
  ###############################################

  default.options<-default.options.for.data.setting()

  ## default values
  #glm.link.type<- default.options$glm.link.type
  #clusters.are.families<-default.options$clusters.are.families
  use_real_data<-default.options$use_real_data


  ###########################################################################
  # Obtain reformatted datasets so that JPRAT algorithm identify datasets ###
  ###########################################################################

  reformatted.datasets<-data.reformatted.for.jprat.analysis(use_real_data,
                                                            study.names,
                                                            #data.file.names,
                                                            input.data.list,
                                                            time.points.for.prediction,
                                                            nonfunctional.covariate.names,
                                                            functional.covariate.names,
                                                            nonfunctional.covariate.value,
                                                            functional.covariate.values.of.interest)

  ## datasets are reformed, so that jprat can use them
  data.sets.as.list<-reformatted.datasets$data.sets.as.list;
  nonfunctional.covariate.values.for.prediction<-reformatted.datasets$nonfunctional.covariate.values.for.prediction;
  xmin<-reformatted.datasets$xmin;
  xmax<-reformatted.datasets$xmax;
  functional.covariate.values.for.prediction<-reformatted.datasets$functional.covariate.values.for.prediction;



  ## compute number of studies
  number.of.studies <- length(study.names)

  ## number of clinical events
  number.of.clinical.events <- length(event.outcome.names)

  ###############################################################################
  ## Set up z-covariate labels and where to evaluate Z *beta(t) for each study ##
  ###############################################################################
  number.of.z.predictions <- nrow(nonfunctional.covariate.values.for.prediction)  ## base_age at k-means
  z.label.names <-
    get.nonfunctional.covariate.values.for.prediction.label(
      nonfunctional.covariate.values.for.prediction)  ## A, B, C, D

  ####################################
  ## information for number at risk ##
  ####################################
  x.label.names <- get.functional.covariate.values.for.prediction.label(
    functional.covariate.values.for.prediction,
    use_real_data,
    xmin,xmax)

  ################################
  ## storage for number.at.risk ##
  ################################
  ## zz-study: we extract data for specific nonfunctional covariate values for each study (ignore functional covariates)
  ## We only take subsets for discrete nonfunctional covariates.
  ## If nonfunctional covariate (z) is continuous, we don't subset the data.
  ##
  ##
  ## zz-CAG : we extract data for specific functional covariate values for each study (ignore nonfunctional covariates)

  study <- NULL
  number.at.risk <- array(0,dim=c(number.of.studies,number.of.clinical.events,    ##hdage_nobase
                                  number.of.z.predictions+2,length(x.label.names),
                                  length(time.points.for.prediction)),
                          dimnames=list(
                            study=study.names,
                            event=event.outcome.names,
                            zz=c(paste("zz",1:number.of.z.predictions,sep=""),
                                 "zz-study","zz-CAG"),
                            xx=x.label.names,
                            time=paste("t",time.points.for.prediction,sep="")))


  ##########################
  ##cat("\n\n get number at risk\n\n")   ##
  ##########################
  covariate.names <- nonfunctional.covariate.names ## base_age


  ### what is this for? ## character(0)
  if("base_age" %in% covariate.names){
    covariate.names <- colnames(nonfunctional.covariate.values.for.prediction)[-which(colnames(nonfunctional.covariate.values.for.prediction)=="base_age")]
  }

  ### error in here: does this error happens because it is agains from the condition : analysis.to.run!="7c-converters"

  for(ss in 1:number.of.studies){

    study.use <- study.names[ss]
    print(study.use)
    data.tmp <- data.sets.as.list[[study.use]]

    for(nn in 1:number.of.clinical.events){
      for(zcomp in 1:length(dimnames(number.at.risk)$zz)){  ### "zz1"      "zz2"      "zz3"      "zz4"      "zz-study" "zz-CAG"

        ##########################################################################################
        ## Extract specific subset of the data  for specific z-predictions
        ##  (ignore CAG repeats) ##
        ##########################################################################################

        ## only using z-eval covariates (no CAG)
        covariate.values.tmp <- nonfunctional.covariate.values.for.prediction[zcomp,covariate.names]      ## base_age_kmeans   covariate.values.tmp: 28.11354
        covariate.values.tmp <- data.frame(covariate.values.tmp)


        if(all(!is.na(covariate.values.tmp)) & ncol(covariate.values.tmp)>0){ ## TRUE
          epsilon.use <- 0.001
          subset.eval.up <- paste(covariate.names,covariate.values.tmp
                                  +epsilon.use,
                                  sep="<",collapse=" & ")
          subset.eval.lo <- paste(covariate.names,covariate.values.tmp
                                  -epsilon.use,
                                  sep=">",collapse=" & ")
          subset.eval <- paste(subset.eval.up,subset.eval.lo,
                               sep=" & ")
        } else {
          subset.eval <- NULL
        }

        if(!is.null(subset.eval)){
          data.subset <- subset(data.tmp,eval(parse(text=subset.eval)))
        } else {
          data.subset <- data.tmp
        }

        ## get number at risk
        cox.fit.tmp <- survfit(Surv(eval(parse(text=event.outcome.names[nn])),
                                    eval(parse(text=paste("delta.",event.outcome.names[nn],sep=""))))~1,
                               data=data.subset)


        summary.cox.fit.tmp <- summary(cox.fit.tmp,times=time.points.for.prediction)

        #cat("\n\n ## Total at beginning\n\n")
        #print(nrow(data.tmp))

        #cat("\n\n ## number of events at or before first time-val\n\n")
        #print(summary.cox.fit.tmp$n.event)

        #cat("\n\n ## number of drop outs at or before first time-val\n\n")
        #print(nrow(data.tmp)-summary.cox.fit.tmp$n.risk-summary.cox.fit.tmp$n.event)

        #cat("\n\n ## number at risk\n\n")
        #print(summary.cox.fit.tmp$n.risk)



        for(xx in 1:length(x.label.names)){
          number.at.risk[ss,nn,zcomp,xx,paste("t",summary.cox.fit.tmp$time,sep="")] <-
            summary.cox.fit.tmp$n.risk
        }
      }

      ##########################################################################################
      ## Extract specific subset of the data  for specific functional covariate values
      ##  (ignore nonfunctional covariates) ##
      ##########################################################################################


      ## only using z-eval covariates (no CAG)
      CAG <- NULL

      number.at.risk[ss,nn,"zz-CAG",,] <- array(0,
                                                dim=dim(adrop(number.at.risk[ss,nn,"zz-CAG",,,drop=FALSE],drop=c(1,2,3))))
      functional.covariate.values.of.interest.tmp <- convert.cag(functional.covariate.values.for.prediction,xmin,xmax)
      functional.covariate.values.of.interest.tmp <- floor(functional.covariate.values.of.interest.tmp)
      for(xx in 1:length(functional.covariate.values.of.interest.tmp)){
        data.subset <- subset(data.tmp,CAG==functional.covariate.values.of.interest.tmp[xx])

        if(nrow(data.subset)>0){
          ## get number at risk
          cox.fit.tmp <- survfit(Surv(eval(parse(text=event.outcome.names[nn])),
                                      eval(parse(text=paste("delta.",event.outcome.names[nn],sep=""))))~1,
                                 data=data.subset)

          summary.cox.fit.tmp <- summary(cox.fit.tmp,times=time.points.for.prediction)

          if(length(summary.cox.fit.tmp$n.risk)>0){
            number.at.risk[ss,nn,"zz-CAG",xx,paste("t",summary.cox.fit.tmp$time,sep="")] <-
              summary.cox.fit.tmp$n.risk
          }
        }
      }
    }
  }

  if(estimated.parameters.common.for.all.studies==TRUE){
    number.at.risk.new <- number.at.risk
    for(ss in 1:number.of.studies){
      number.at.risk.new[ss,,,,] <- apply(number.at.risk,c(2,3,4,5),sum)
    }
    number.at.risk <- number.at.risk.new
  }



  ##########################
  ## store number at risk ##
  ##########################
  if(write.output==TRUE){

    flatten.nrisk <- flatten.array(number.at.risk,
                                   dim.order=names(dimnames(number.at.risk)),
                                   flatten.name="time",
                                   theta="zz")
    flatten.nrisk <- flatten.nrisk[,-which(colnames(flatten.nrisk)=="theta")]

    write.table(flatten.nrisk,
                  "out_nrisk.dat",col.names=TRUE, row.names=FALSE,na="0")

    return(flatten.nrisk)

  }else{

  return(number.at.risk)

   }
  #return(number.at.risk)
}



#' @title Criteria for time points in JPRAT analysis
#' @description This function returns warning messages if criteria are not met for \code{functional.covariate.values.of.interest}, \code{functional.covariate.values.of.interest.ci},
#' \code{time.points.of.interest}, \code{time.points.of.interest.ci}, \code{functional.covariate.comparisons} and \code{time.points.for.conditional.prediction}.
#'
#' @inheritParams jprat.wrapper
#' @inheritParams view.all.results
#'
#' @return Warning messages if criteria for time points are not met for arguments  \code{functional.covariate.values.of.interest}, \code{functional.covariate.values.of.interest.ci},
#' \code{time.points.of.interest}, \code{time.points.of.interest.ci}, \code{functional.covariate.comparisons} and \code{time.points.for.conditional.prediction}.
#'
#'
criteria.time.points.for.jprat.analysis<-function(
  study.names,
  input.data.list,
  time.points.for.prediction,
  nonfunctional.covariate.names,
  functional.covariate.names,
  nonfunctional.covariate.value,
  time.points.for.conditional.prediction,
  functional.covariate.values.of.interest,
  functional.covariate.values.of.interest.ci,
  functional.covariate.comparisons,
  time.points.of.interest,
  time.points.of.interest.ci
){


  default.options<-default.options.for.data.setting()
  use_real_data<-default.options$use_real_data


  ################################################################
  ## Obtain list of datasets, minimum of functional covariates  ##
  ## and maximum of functional covariates                       ##
  ################################################################

  reformatted.datasets<-data.reformatted.for.jprat.analysis(use_real_data=use_real_data,
                                                            study.names,
                                                            #data.file.names,
                                                            input.data.list,
                                                            time.points.for.prediction,
                                                            nonfunctional.covariate.names,
                                                            functional.covariate.names,
                                                            nonfunctional.covariate.value,
                                                            functional.covariate.values.of.interest
  )


  data.sets.as.list<-reformatted.datasets$data.sets.as.list
  xmin<-reformatted.datasets$xmin
  xmax<-reformatted.datasets$xmax


  cat("n/n/ Checking criteria for time points \n\n")

  ################################################################
  ##Need to suggest criteria for time points for prediction     ##
  ################################################################
  time.points.for.prediction<-time.points.for.prediction

  for(ss in 1:length(study.names)){

    study <- study.names[ss]

    for(tt in 1:length(time.points.for.prediction)){

      subsample<-subset(data.sets.as.list[[study]], data.sets.as.list[[study]][,nonfunctional.covariate.names]==time.points.for.prediction[tt])

      if(dim(subsample)[1]<5){
        #print(study);
        #t<-time.points.for.prediction[tt];
        #war"ning=function(w){print(paste(study, nonfunctional.covariate.names, time.points.for.prediction[tt]));
        #cat("n/n/  which dataset is not enough; \n\n")
        #cat("/n/n", paste(study, nonfunctional.covariate.names, time.points.for.prediction[tt]), "\n\n")
        #print(paste(study, nonfunctional.covariate.names, time.points.for.prediction[tt]))
        warning("n/ not enough data in ", paste(study), " study when ", paste(nonfunctional.covariate.names), "=", paste(time.points.for.prediction[tt]),
                "; May have divergent in the integration calculation while bootstrapping\n")

      }

    }
  }


  #######################################################
  ## checking criteria, added on Oct/13/19             ##
  ## for time.points.for.conditional.prediction        ##
  #######################################################
  if((!is.null(time.points.for.prediction)) & (!is.null(time.points.for.conditional.prediction))){

    time.points.prediction.check<-all(time.points.for.conditional.prediction%in%time.points.for.prediction)

    if(time.points.prediction.check!=TRUE){
      warning("n/ all elements of time.points.for.conditional.prediction should be contained in time.points.for.prediction. \n")
    }


  }

  #######################################################
  ## checking criteria, added on Oct/13/19             ##
  ## for functional.covariate.values.of.interest       ##
  #######################################################
  if((!is.null(functional.covariate.values.of.interest))&(!is.null(xmin))&(!is.null(xmax))){

    functional.covariate.check<-all(functional.covariate.values.of.interest%in%seq(xmin, xmax))

    if(functional.covariate.check!=TRUE){
      warning("n/ all elements of functional.covariate.values.of.interest should be between minimum and maximum of covariate values. \n")
    }


  }



  #######################################################
  ## checking criteria, added on Oct/13/19             ##
  ## for functional.covariate.values.of.interest.ci    ##
  #######################################################
  if((!is.null(functional.covariate.values.of.interest))&(!is.null(functional.covariate.values.of.interest.ci))&(!is.null(time.points.for.prediction))){

    ##########################################################################
    ## functional covariate values of interest for CI can be the same as    ##
    ## the fuctional covariate values of interest                           ##
    ##########################################################################

    #all.same.functional.covariate.values<-all(functional.covariate.values.of.interest==functional.covariate.values.of.interest.ci)

    ##########################################################################
    ## functional covariate values of interest for CI can be contained in   ##
    ## time.points.for.prediction                                           ##
    ##########################################################################

    functional.covariate.values.of.interest.ci.check<-all(functional.covariate.values.of.interest.ci%in%time.points.for.prediction)

    #if((all.same.functional.covariate.values!=TRUE)&(functional.covariate.values.of.interest.ci.check!=TRUE)){
    if(functional.covariate.values.of.interest.ci.check!=TRUE){
      warning("n/ functional.covariate.values.of.interest.ci is recommended to be the same as functional.covariate.values.of.interest \n")
    }


  }


  #######################################################
  ## checking criteria, added on Oct/13/19             ##
  ## for functional.covariate.comparisons              ##
  #######################################################
  if((!is.null(functional.covariate.comparisons))&(!is.null(functional.covariate.values.of.interest.ci))){

    ##########################################################################
    ## functional.covariate.comparisons can be the same as                  ##
    ## the fuctional covariate values of interest for confidence interval   ##
    ##########################################################################

    functional.covariate.comparisons.check<-all(functional.covariate.comparisons==functional.covariate.values.of.interest.ci)


    if(functional.covariate.comparisons.check!=TRUE){
      warning("n/ functional.covariate.comparisons is recommended to be the same as functional.covariate.values.of.interest.ci \n")
    }


  }


  #######################################################
  ## checking criteria, added on Oct/13/19             ##
  ## for time.points.of.interest                       ##
  #######################################################
  if((!is.null(time.points.for.prediction)) & (!is.null(time.points.of.interest))){

    time.points.of.interest.check<-all(time.points.of.interest%in%time.points.for.prediction)

    if(time.points.of.interest.check!=TRUE){
      warning("n/ all elements of time.points.of.interest should be contained in time.points.for.prediction. \n")
    }


  }

  #######################################################
  ## checking criteria, added on Oct/13/19             ##
  ## for time.points.of.interest.ci                    ##
  #######################################################
  if((!is.null(time.points.of.interest.ci)) & (!is.null(time.points.of.interest))){

    #############################################################
    ## time.points.of.interest.ci can be the same as           ##
    ## time.points.of.interest                                 ##
    #############################################################

    #all.equal.time.points.of.interest.ci<-all(time.points.of.interest==time.points.of.interest.ci)


    #############################################################
    ## time.points.of.interest.ci can be contained in          ##
    ## time.points.for.prediction                              ##
    #############################################################

    time.points.of.interest.ci.check<-all(time.points.of.interest.ci%in%time.points.for.prediction)

    #if((all.equal.time.points.of.interest.ci!=TRUE)|(time.points.of.interest.ci.check!=TRUE)){
    if(time.points.of.interest.ci.check!=TRUE){
      warning("n/ time.points.of.interest.ci is recommended to be the same as time.points.of.interest \n")
    }


  }



  #return(list())

}

############################################################
# end of documentation
############################################################



#######################################################################
#######################################################################
##
## This code reform datasets for JPRAT to be able to understand the given datasets.
##
#######################################################################
#######################################################################

#################################
# data reformatted to run jprat #
################################
#' @import utils
data.reformatted.for.jprat.analysis<-function(use_real_data,
                                              study.names,
                                              input.data.list,
                                              time.points.for.prediction,
                                              nonfunctional.covariate.names,
                                              functional.covariate.names,
                                              nonfunctional.covariate.value,
                                              functional.covariate.values.of.interest
){

  ###############################
  ## number of study           ##
  ###############################

  num_study<-length(study.names)


  ###############################
  ## obatin data list          ##
  ###############################

  tmp.list <- get.empty.list(study.names)
  data.sets.as.list <- tmp.list

  ## combine dataset names
  #data<-data.file.names

  for(ss in 1:length(study.names)){
    study.use <- names(tmp.list)[ss]
    data.tmp <- data.sets.as.list[ss]

    study <- study.names[ss]
    data.tmp<-input.data.list[ss]
    ## read simulated data files - fake data
    #if (use.data.example=TRUE){
    #data.path<-system.file("extdata", paste("data_", study, ".csv", sep="")
    #                       ,package = "JPRAT")
    #data.tmp<-read.csv(data.path)
    #}

    data.sets.as.list[study] <- data.tmp
    #cat(" \n pseudo data dimensions, complete cases:",dim(data.tmp))
  }



  ################################################################
  ##Need to suggest criteria for time points for prediction     ##
  ################################################################
  ## predict.start.time=46; predicit.end.time=66; time.gap=5
  # time.points.for.prediction<-time.points.for.prediction #seq(predict.start.time, predicit.end.time, by=time.gap) ##
  #
  # for(ss in 1:length(study.names)){
  #
  #   study <- study.names[ss]
  #
  #   for(tt in 1:length(time.points.for.prediction)){
  #
  #     subsample<-subset(data.sets.as.list[[study]], data.sets.as.list[[study]][,nonfunctional.covariate.names]==time.points.for.prediction[tt])
  #
  #     if(dim(subsample)[1]<5){
  #       #print(study);
  #       #t<-time.points.for.prediction[tt];
  #       #war"ning=function(w){print(paste(study, nonfunctional.covariate.names, time.points.for.prediction[tt]));
  #       cat("n/ which dataset is not enough")
  #       print(paste(study, nonfunctional.covariate.names, time.points.for.prediction[tt]))
  #       warning("n/ not enough data; May have divergent in the integration calculation while bootstrapping\n")
  #
  #
  #
  #     }
  #
  #   }
  # }
  #


  #####################################################################################################################
  ## choose time points for conditional prediction and time points to be added: predict 5 years or 10 years later    ##
  #####################################################################################################################

  #time.points.for.conditional.prediction<-c(46, 51, 56) ## <- make.data.for.analysis$time.points.for.conditional.prediction   ## null
  #time.points.for.conditional.prediction<-c(46, 51, 56)
  #time.points.for.conditional.prediction.toadd<-c(5, 10)  ###<- make.data.for.analysis$time.points.for.conditional.prediction.toadd  ## null


  #nonfunctional.covariate.names="base_age"  ##<- make.data.for.analysis$nonfunctional.covariate.names ## base_age   ## z1

  #####################################
  ## checking foramt :       base_age
  ##                    ## A      40
  #####################################
  #nonfunctional.covariate.value <- c(40)

  #tmp.nonfunctional.covariate.values.for.prediction <- get.nonfunctional.covariate.values.for.prediction(nonfunctional.covariate.names,
  #                                                                                                             functional.beta.intercept,analysis.to.run,nonfunctional.covariate.value.is.0,paper.type)
  output.list <- list()
  for(ll in 1:length(nonfunctional.covariate.names)){
    tmp <- get.empty.list(nonfunctional.covariate.names[ll])  #[ll]
    tmp[[ll]] <- nonfunctional.covariate.value
    output.list <- appendList(output.list,tmp)
  }

  nonfunctional.covariate.values.for.prediction <- expand.grid(output.list)

  z.label.names <- LETTERS[1:nrow(nonfunctional.covariate.values.for.prediction)]
  rownames(nonfunctional.covariate.values.for.prediction) <- z.label.names

  #nonfunctional.covariate.values.for.prediction ##<- tmp.nonfunctional.covariate.values.for.prediction$nonfunctional.covariate.values.for.prediction ## at k-means  #Z1
  #cat("\n\n ## Table of nonfunctional.covariate.values.for.prediction.per.studyuations ##\n\n")
  #print(nonfunctional.covariate.values.for.prediction)

  ###################################################
  # Find min and max of functional covariate names  #
  ###################################################

  #functional.covariate.names <-"CAG"  ## make.data.for.analysis$functional.covariate.names		## CAG ## CAG
  max.functional.covariate.names.list <- get.empty.list(study.names)
  min.functional.covariate.names.list <- get.empty.list(study.names)

  for(ss in 1:length(study.names)){

    max.functional.covariate.names.list[ss]<-max(data.sets.as.list[[ss]][, functional.covariate.names])
    min.functional.covariate.names.list[ss]<-min(data.sets.as.list[[ss]][, functional.covariate.names])

  }

  xmin <-min(unlist(min.functional.covariate.names.list))                        ##36  ##make.data.for.analysis$xmin  ## 36   ## NULL
  xmax <-max(unlist(max.functional.covariate.names.list))                        ## make.data.for.analysis$xmax  ## 50   ## NULL



  #########################################################
  # Generate functional covariate values on [0,1]
  #########################################################

  functional.covariate.values.for.prediction <- get.functional.covariate.values.for.prediction(use_real_data,
                                                                                               xmin,xmax,functional.covariate.values.of.interest)                           ## make.data.for.analysis$functional.covariate.values.for.prediction	##[0,1] ## seq(0,1, by=0.01)



  return(list(
    data.sets.as.list=data.sets.as.list,
    nonfunctional.covariate.values.for.prediction=nonfunctional.covariate.values.for.prediction,
    xmin=xmin, xmax=xmax,
    functional.covariate.values.for.prediction=functional.covariate.values.for.prediction
  ))


}







#' @title Reformat Datasets
#' @description This function reforms datasets so that JPRAT can understand and proceed analysis.
#' @param which.nonfunctional.covariate.comparisons A numeric vector for which nonfunctional covariates will be compared.
#'                                                  In our analysis, we only consider one nonfunctional covariate for baseline age. The default value is c(1, 1).
#'                                                  We only consider one nonfunctional covariate and there is nothing to compare between covariates.
#' @inheritParams jprat.wrapper
#' @inheritParams view.all.results
#' @return A list of
#'      \item{num_study}{The number of studies used in analyses. If the real data analysis used three studies called "cohort", "predict", "pharos", then number of studies is 3. i.e., \code{num_study=3}.}
#'      \item{nonfunctional.covariate.comparisons}{A list of comparison sets where the predicted values of nonfunctional covariates \eqn{Z} will be compared.}
#'      \item{color.labels}{A character vector of the colors' name used in labeling events of interest in plots. Users need to provide the color names.}
#'      \item{legend.labels}{A character vector to label event names in plots. Users need to provide the names for events of interest.}
#'     \item{event.comparison.table}{A list of numbers corresponding to outcomes.}
#' @import stats
#'
data.reformatted.for.analysis.results<-function(study.names, event.outcome.names=NULL,
                                                color.names,
                                                legend.names,
                                                which.nonfunctional.covariate.comparisons=NULL
                                                #write.output=TRUE
){

  ###############################
  ## number of study           ##
  ###############################

  num_study<-length(study.names)


  ######################################
  ## which covaraite will be compared ##
  ######################################
  if(is.null(which.nonfunctional.covariate.comparisons)==FALSE){

    nonfunctional.covariate.comparisons <- list(comp1=which.nonfunctional.covariate.comparisons)
  }


  #############################
  # How to obtain color key  ##
  #############################
  if(is.null(event.outcome.names)==FALSE){

    ll<-length(event.outcome.names);
    eventcolor.list<-get.empty.list(event.outcome.names);
    color.label.key<-color.names;


    for(ll in 1:length(event.outcome.names)){
      eventcolor.list[[ll]]<-color.label.key[ll]
      #color.label.key<-list(hdage_nobase="firebrick1", mcione="darkgreen", dep2="black", tfctwo="black")
    }

    color.labels<-unlist(eventcolor.list)
  }


  #############################
  # lagend lists in plots    ##
  #############################

  if(is.null(event.outcome.names)==FALSE){

    ll<-length(event.outcome.names);
    eventlegend.list<-get.empty.list(event.outcome.names);

    legend.label.key<-legend.names;

    for(ll in 1:length(event.outcome.names)){
      eventlegend.list[[ll]]<-legend.label.key[ll]
    }

    legend.labels<-unlist(eventlegend.list);
  }

  #####################################
  # create event comparison table    ##
  #####################################

  if(is.null(event.outcome.names)==FALSE){

    event.comparison <- list(nc1=event.outcome.names)

  }

  event.comparison.table <- event.comparison


  make.find.names <- function(event.outcome.names){
    function(x){
      return(which(event.outcome.names %in% x))
    }
  }

  find.names <- make.find.names(event.outcome.names)
  event.comparison <- lapply(event.comparison,find.names)
  event.comparison.table <- lapply(event.comparison.table,find.names)


  return(list(
    num_study=num_study,
    nonfunctional.covariate.comparisons=nonfunctional.covariate.comparisons,
    color.labels=color.labels,
    legend.labels=legend.labels,
    event.comparison.table=event.comparison.table
  ))


}

#######################################################################
#######################################################################
##
## This code converts the new notation into the original code notation for JPRAT analysis.
##
#######################################################################
#######################################################################

## convert.new.notation.to.old.for.jprat:
## This code converts the new notation into the original code notation for JPRAT analysis.

convert.new.notation.to.old.for.jprat <- function(study.names,
                                                  data.sets.as.list,
                                                  time.points.for.prediction,
                                                  time.points.for.conditional.prediction,
                                                  time.points.for.conditional.prediction.toadd,
                                                  nonfunctional.covariate.names,
                                                  nonfunctional.covariate.values.for.prediction,
                                                  functional.covariate.names,
                                                  #functional.covariate.values.of.interest,
                                                  #functional.covariate.values.of.interest.ci,
                                                  functional.covariate.values.for.prediction,
                                                  xmin,
                                                  xmax,
                                                  othercovariate.names,
                                                  event.outcome.names,
                                                  delta.names,
                                                  use_real_data,
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

){

  ##############################################################################################################
  ## We  adjust any clashes(discordants) between what is analyzed separately and assumed common parameters across studies. ##
  ## Can make this into an error message if user accidentaly puts TRUE instead of FALSE as input.    	    ##
  ##############################################################################################################

  if(what.analyzed.separately=="study" | what.analyzed.separately=="studyevent" | length(study.names)==1){
    ## if studies are analyzed separately, we cannot have common parameters across studies
    estimated.parameters.common.for.all.studies <- FALSE
  }



  ## If we only have one event, we should not have random effects in the model.
  ## Right now, the model considers random effects between events, not between studies.
  if(length(event.outcome.names)==1){
    what.analyzed.separately <- "event"
    use.functional.event.coefficients <- FALSE
  }

  if(length(study.names)==1){
    use.functional.study.coefficients <- FALSE
  }



  #######################################################
  ## checking criteria, added on Oct/13/19             ##
  #######################################################
  # if(!is.null(time.points.for.prediction)){
  #
  #   time.points.prediction.check<-all(time.points.for.conditional.prediction%in%time.points.for.prediction)
  #
  #   if(time.points.prediction.check!=TRUE){
  #     warning("n/ all elements of time.points.for.conditional.prediction should be contained in time.points.for.prediction. \n")
  #   }
  #
  #
  # }





  #######################################################
  ## default values to run the estimation procedure    ##
  #######################################################


  ## get default setting for estimation
  estimation.default.values <- default.estimation.setting(use.bootstrap.variance, number.of.bootstraps)

  ## extract seed to generate random data
  iseed <- estimation.default.values$iseed

  ## for plotting output
  #plot.kaplan.meier.curves <- estimation.default.values$plot.kaplan.meier.curves


  ##  In this paper, the cluster within a study corresponds to events from the same individual.
  ##  That is, the cluster info is from the same individual.
  family.data <- estimation.default.values$family.data

  var.lo <- estimation.default.values$var.lo
  var.hi <- estimation.default.values$var.hi
  spline.constrain <- estimation.default.values$splines.constrained.at.0


  gamm.orig <- FALSE

  ## default method
  method <- c("gamm4","new")[1]

  ###########################
  ## Data-dependent values ##
  ###########################
  default.values <- get.default.values(use_real_data) #, number.of.simulations)

  ## number of simulations
  number.of.simulations <- default.values$number.of.simulations

  ## compute number of studies
  number.of.studies <- length(study.names)

  ## number of clinical events
  number.of.clinical.events <- length(event.outcome.names)

  ## sample size
  n <- get.sample.size.for.jprat(data.sets.as.list,
                                 number.of.studies,use_real_data,
                                 default.values)

  ## maximum sample size across all studies
  maximum.sample.size <- max(n)

  ## m: number of clinical events per person per study
  m <- get.number.of.clinical.events.per.person.per.study(study.names,number.of.studies,
                                                          n,maximum.sample.size,
                                                          number.of.clinical.events)

  ## maximum number of event types
  maximum.number.of.events <- max(m)

  #######################################################
  ## specify number of knots for alpha(x,t) estimation ##
  #######################################################

  tmp.get.spline.information <-
    get.spline.information(use_real_data,study.names,default.values,
                           event.outcome.names,number.of.clinical.events,
                           number.of.studies)
  functional.alpha.form <- tmp.get.spline.information$functional.alpha.form
  knot.length <- tmp.get.spline.information$knot.length
  number.of.functional.alpha <- tmp.get.spline.information$number.of.functional.alpha

  #############################################################################
  ## specify initial coefficient estimates  (only needed for simulated data) ##
  #############################################################################
  tmp.get.coefficient.values <- get.coefficient.values(use.functional.beta.intercept,
                                                       use.functional.event.coefficients,
                                                       use.functional.study.coefficients,
                                                       estimated.parameters.common.for.all.studies,
                                                       default.values,
                                                       number.of.clinical.events,
                                                       number.of.studies)

  functional.beta.intercept <- tmp.get.coefficient.values$functional.beta.intercept
  event.coefficients <-tmp.get.coefficient.values$event.coefficients
  study.coefficients <- tmp.get.coefficient.values$study.coefficients

  ###############################
  ## length of beta functional ##
  ###############################
  maximum.number.of.functional.beta.coefficients <- get.length.beta.functionals(use_real_data,nonfunctional.covariate.names,default.values)

  #############################
  ## Set up parameter labels ##
  #############################
  estimated.components <- c("beta","alphas","Ft")
  estimated.components.study.comparisons <- estimated.components ## what is compared between studies
  if(!is.null(time.points.for.conditional.prediction)){
    estimated.components <- c(estimated.components,"Ft.predicted")
  }


  nonfunctional.parameter.names <- get.parameter.label(maximum.number.of.functional.beta.coefficients,
                                                       functional.beta.intercept,
                                                       event.coefficients,
                                                       study.coefficients)




  ###################################################################
  ## Set up x-covariate labels and where to evaluate alpha(x,t) at ##
  ###################################################################

  functional.covariate.values.for.prediction.per.study <-
    get.functional.covariate.values.for.prediction.per.study(study.names,
                                                             functional.covariate.values.for.prediction)       ## list of functional.covariate.values.for.prediction, per study

  x.label.names <-
    get.functional.covariate.values.for.prediction.label(functional.covariate.values.for.prediction,
                                                         use_real_data,
                                                         xmin,xmax)


  ## number of x's at which to evaluate alpha(x,t)
  num_xx <- max(unlist(lapply(functional.covariate.values.for.prediction.per.study,length.apply)))  ## 18


  ###############################################################################
  ## Set up z-covariate labels and where to evaluate Z *beta(t) for each study ##
  ###############################################################################
  out.nonfunctional.covariate.values.for.prediction.per.study <-
    get.nonfunctional.covariate.values.for.prediction.per.study(study.names,number.of.studies,
                                                                event.outcome.names,
                                                                number.of.clinical.events,
                                                                nonfunctional.covariate.values.for.prediction)

  nonfunctional.covariate.values.for.prediction.per.study <- out.nonfunctional.covariate.values.for.prediction.per.study$nonfunctional.covariate.values.for.prediction.per.study
  number.of.z.predictions <- out.nonfunctional.covariate.values.for.prediction.per.study$number.of.z.predictions
  z.label.names <- get.nonfunctional.covariate.values.for.prediction.label(nonfunctional.covariate.values.for.prediction)


  #######################################
  ## Update Time points for prediction ##
  #######################################
  time.points.for.prediction <- get.time.points.for.prediction(time.points.for.prediction,
                                                               time.points.for.conditional.prediction,
                                                               time.points.for.conditional.prediction.toadd) ## either inputted from user or default simulation setting  40  45  50  55  60  65  70  75  80  85  90  95 100


  ## number of time points for evaluation
  num_time <- length(time.points.for.prediction)

  ###################################################
  ## check if we need to compute study differences ##
  ###################################################
  compute.study.differences <- run.study.differences(check.study.equality,
                                                     estimated.parameters.common.for.all.studies,
                                                     estimation.default.values,
                                                     number.of.studies)   ## FALSE


  ########################################
  ## Set up arrays to store information ##
  ########################################


  ## get study comparison information for pairwise comparisons between studies
  study.comparison.information <- get.study.comparison.information(study.names)
  number.of.study.comparisons <- study.comparison.information$number.of.study.comparisons
  all.study.comparisons <- study.comparison.information$all.study.comparisons
  name.of.study.comparisons <- study.comparison.information$name.of.study.comparisons


  ###################################################
  ## Process data to run JPRAT                     ##
  ## Convert to original notation used in the code ##
  ###################################################

  #####################
  ## Real data setup ##
  #####################
  tmp.process.real.data <- process.real.data(
    study.names,
    data.sets.as.list,
    functional.covariate.names,
    xmin,
    xmax,
    othercovariate.names,
    event.outcome.names,
    delta.names,
    nonfunctional.covariate.names
  )

  ## go here 6/26/19
  a0 <- NULL


  #dropout.rate <- dropout.rate
  delta.outcomes.per.study <- tmp.process.real.data$delta.outcomes.per.study
  event.outcomes.per.study <- tmp.process.real.data$event.outcomes.per.study
  functional.covariates.per.study <- tmp.process.real.data$functional.covariates.per.study

  functional.beta.coefficients <- tmp.process.real.data$functional.beta.coefficients
  functional.g.form <- NULL

  mix_n <- NULL

  number.of.functional.beta.coefficients.per.study <-
    tmp.process.real.data$number.of.functional.beta.coefficients.per.study
  nonfunctional.covariates.per.study <- tmp.process.real.data$nonfunctional.covariates.per.study

  othercovariates.per.study <- tmp.process.real.data$othercovariates.per.study

  pseudovalues.replaced.by.arbitrary.values <- FALSE ## only used in simulated data

  random.effect.first.parameter <- NULL
  random.effect.second.parameter <- NULL
  random.effect.first.parameter2 <- NULL
  random.effect.second.parameter2 <- NULL
  random.effect.third.parameter <- NULL

  study.random.effect.parameter <- NULL
  simulated.random.effects.depends.on.covariates  <- NULL
  simulated.censoring.depends.on.z <- NULL
  simulated.censor.rate <- NULL
  simulated.random.effect.distribution <- NULL
  simulate.data.with.random.effects <- NULL
  simulated.z.covariate.distribution <- NULL
  simulated.x.covariate.distribution <- NULL
  simulated.parameters.common.for.all.studies  <- NULL

  unknown.random.effect.parameters <- NULL
  unknown.z.covariate.parameters <- NULL
  unknown.x.covariate.parameters <- NULL

  x.covariate.first.parameter<- NULL
  x.covariate.second.parameter<- NULL

  z.covariate.first.parameter <- NULL
  z.covariate.second.parameter<- NULL


  ########################
  ## to display results ##
  ########################
  #results.default.values <- default.results.setting(use_real_data,
  #                                                  nonfunctional.covariate.values.for.prediction,
  #                                                  functional.covariate.values.of.interest) #,nonfunctional.parameter.names)

  #plotted.time.range.displayed  <- results.default.values$plotted.time.range.displayed
  #normalize.cag.repeat.values <- results.default.values$normalize.cag.repeat.values
  #ylabel.for.plots.comparing.studies <- results.default.values$ylabel.for.plots.comparing.studies
  #xlabel.for.plots.comparing.studies <- results.default.values$xlabel.for.plots.comparing.studies

  #add.number.at.risk.legend <- results.default.values$add.number.at.risk.legend
  #alpha_label_x_direction <- results.default.values$alpha_label_x_direction
  #z_file_labels <- results.default.values$z_file_labels



  #alpha.cut <- results.default.values$alpha.cut
  #beta.cut <- results.default.values$beta.cut
  #number.of.significant.digits <- results.default.values$number.of.significant.digits
  #legend.position.use <- results.default.values$legend.position.use

  #plot.nadaraya.watson <- results.default.values$plot.nadaraya.watson
  #use.bootstraps.confidence.interval <- results.default.values$use.bootstraps.confidence.interval

  #alphax.ylab<- results.default.values$alphax.ylab
  #alphax.xlab <- results.default.values$alphax.xlab
  #alphat.ylab <- results.default.values$alphat.ylab
  #alphat.xlab <- results.default.values$alphat.xlab


  ## for specific HD examples
  #ylim.key <- results.default.values$ylim.key
  #ylim.setting <- results.default.values$ylim.setting
  #xx_choice.sample.size <-functional.covariate.comparisons.for.sample.size

  ########################################################
  ## Convert to original notation used to make the code ##
  ########################################################


  arbitrary <- pseudovalues.replaced.by.arbitrary.values
  axmod <- functional.alpha.form
  analyze.separately <- what.analyzed.separately

  beta0int <- functional.beta.intercept
  beta0 <- functional.beta.coefficients
  boot <- number.of.bootstraps

  combi.names <- name.of.study.comparisons
  combi.study <- number.of.study.comparisons
  combi.choice <- all.study.comparisons
  censorrate <- simulated.censor.rate


  delta_tmp.list <- delta.outcomes.per.study
  est.cens.depend.z <- estimation.when.censoring.depends.on.z



  frform <- simulated.random.effect.distribution
  fzrform <- simulated.z.covariate.distribution
  fxform <- simulated.x.covariate.distribution


  gamma.param <- event.coefficients
  gtmod <- functional.g.form
  gen.cens.depend.z <- simulated.censoring.depends.on.z


  knot.length <- knot.length
  la <- number.of.functional.alpha
  lb <- number.of.functional.beta.coefficients.per.study
  lb.max <- maximum.number.of.functional.beta.coefficients
  link.type <- glm.link.type

  mix_n <- mix_n
  maxm <- maximum.number.of.events

  num_study <-

    np <- number.of.clinical.events

  num_time <- num_time
  num_xx <- num_xx

  nmax <- maximum.sample.size
  n <- n

  omega.param <- study.coefficients
  param.label <- nonfunctional.parameter.names

  par1_fzr <- z.covariate.first.parameter
  par2_fzr <- z.covariate.second.parameter
  par1_fx <- x.covariate.first.parameter
  par2_fx <- x.covariate.second.parameter
  par1_fr <- random.effect.first.parameter
  par2_fr <-  random.effect.second.parameter
  par1_fr2 <-random.effect.first.parameter2
  par2_fr2 <-  random.effect.second.parameter2

  par_fu <- study.random.effect.parameter

  real_data <- use_real_data



  s_tmp.list <- event.outcomes.per.study
  simus <- number.of.simulations
  s.names <- event.outcome.names

  theta.names <- estimated.components
  theta.names.combi <- estimated.components.study.comparisons
  time_val <- time.points.for.prediction


  time_choice.predicted <- time.points.for.conditional.prediction
  time_choice.predicted.toadd<- time.points.for.conditional.prediction.toadd

  type_fzr <- unknown.z.covariate.parameters
  type_fx <- 	unknown.x.covariate.parameters
  type_fr <- unknown.random.effect.parameters

  use.random.effects <-  simulate.data.with.random.effects
  var.boot <- 	use.bootstrap.variance

  xks <- functional.covariate.values.for.prediction.per.study
  x_tmp.list <- functional.covariates.per.study
  x_lab_names <- x.label.names
  zeval <- nonfunctional.covariate.values.for.prediction.per.study

  z.choice <- number.of.z.predictions
  z_tmp.list <- nonfunctional.covariates.per.study
  z_lab_names <- z.label.names
  #a0 <- a0
  #alphat_lab <- label.for.alpha.values.over.time
  #alphax_lab <- alpha_label_x_direction
  #add.second.legend <- add.number.at.risk.legend
  #boot.ci <- use.bootstraps.confidence.interval
  #beta_lab <- lapply( number.of.functional.beta.coefficients.per.study ,double.get.label)
  #colors.use <- color.labels
  #common.param.data <- simulated.parameters.common.for.all.studies
  common.param.estimation <- estimated.parameters.common.for.all.studies
  #convert.x <- normalize.cag.repeat.values
  #conf.int.use <- plot.confidence.intervals
  #nn.comparison <- event.comparison.table
  #par3_fr <- random.effect.third.parameter
  #plot.KM <- plot.kaplan.meier.curves
  #plot.nw <- plot.nadaraya.watson
  #randomeffects.covariate.dependent <- simulated.random.effects.depends.on.covariates
  #round.set <- number.of.significant.digits
  #study.ylab <- ylabel.for.plots.comparing.studies
  #study.xlab <- xlabel.for.plots.comparing.studies
  #s.names.short <- legend.labels
  #time.cut <- plotted.time.range.displayed
  #time_choice <- time.points.of.interest
  #time_choice.ci <- time.points.of.interest.ci
  #type1.error <- type1.error
  #type2.error <- type2.error
  #treatment.effect <- treatment.effect
  #var.est <- estimate.variances
  #xx_choice <- functional.covariate.values.of.interest
  #xx_choice.ci <- functional.covariate.values.of.interest.ci
  #xx_comp <- functional.covariate.comparisons
  #zeval.tmp <- nonfunctional.covariate.values.for.prediction
  #z_lab <- z_file_labels
  #zz_comp <- nonfunctional.covariate.comparisons
  #
  #
  #
  #
  #
  #
  return(list(
    ##################
    ## from results ##
    ##################
    #alpha.cut=alpha.cut, #
    #add.second.legend=add.second.legend, #
    #alphax_lab=alphax_lab, #
    #beta.cut=beta.cut,#
    #boot.ci=boot.ci,#
    #convert.x=convert.x,#
    #legend.position.use=legend.position.use,#
    #round.set=round.set,#
    #plot.nw=plot.nw, #
    #study.ylab=study.ylab, #
    #study.xlab=study.xlab, #
    #time.cut =time.cut, #
    #ylim.key=ylim.key, #
    #ylim.setting=ylim.setting,#
    #z_lab = z_file_labels, #
    #alphax.ylab =alphax.ylab,
    #alphax.xlab = alphax.xlab,
    #alphat.ylab = alphat.ylab,
    #alphat.xlab = alphat.xlab,
    #####################################
    ## Input converted to old notation ##
    #####################################
    #alphat_lab=alphat_lab,#
    analyze.separately=analyze.separately, #
    boot=boot, #
    #colors.use=colors.use,#
    common.param.estimation=common.param.estimation, #
    #conf.int.use= conf.int.use,#
    est.cens.depend.z=est.cens.depend.z,#
    #nn.comparison=nn.comparison,#
    real_data=real_data,#
    #s.names.short = s.names.short,#
    s.names=s.names,#
    time_choice.predicted=time_choice.predicted,#
    time_choice.predicted.toadd=time_choice.predicted.toadd,#
    #time_choice=time_choice,#
    #time_choice.ci=time_choice.ci, #
    var.boot =var.boot,#
    #xx_choice = functional.covariate.values.of.interest,#
    #var.est = estimate.variances,#
    #zz_comp = zz_comp,#
    #xx_comp = xx_comp,
    #xx_choice.sample.size=xx_choice.sample.size,
    #######################################
    ## Depends on simulated or real data ##
    #######################################
    #a0=a0, #
    arbitrary=arbitrary, #
    axmod=axmod, #
    beta0int=beta0int, #
    beta0=beta0,#
    #beta_lab =beta_lab, ##
    combi.study=combi.study, #
    combi.choice=combi.choice, #
    combi.names =combi.names, #
    censorrate=censorrate,#
    #common.param.data=common.param.data, #
    compute.study.differences=compute.study.differences, #
    delta_tmp.list=delta_tmp.list, #
    family.data=family.data,#
    frform=frform,#
    fzrform=fzrform, #
    fxform=fxform,#
    #gamm.orig=gamm.orig,#
    gamma.param=gamma.param, #
    gtmod=gtmod,  #
    gen.cens.depend.z=gen.cens.depend.z,#
    iseed= iseed,  #
    knot.length=knot.length, #
    la=la, #
    lb=lb, 	 #
    lb.max=lb.max, 	#
    link.type=link.type,#
    m=m,#
    method=method, #
    mix_n=mix_n, #
    maxm=maxm, #
    num_study=num_study, #
    num_time=num_time, #
    n=n,#
    np=np, 	#
    num_xx=num_xx,#
    nmax=nmax, #
    omega.param=omega.param,  #
    param.label=param.label, #
    par1_fzr=par1_fzr, #
    par2_fzr=par2_fzr,#
    par1_fx=par1_fx,#
    par2_fx=par2_fx,#
    par1_fr=par1_fr,#
    par2_fr=par2_fr,#
    par1_fr2=par1_fr2, 	#
    par2_fr2=par2_fr2,#
    #par3_fr=par3_fr,#
    par_fu=par_fu, #
    #plot.KM=plot.KM,
    #randomeffects.covariate.dependent=randomeffects.covariate.dependent,#
    s_tmp.list=s_tmp.list, #
    spline.constrain=spline.constrain, #
    simus=simus, #
    theta.names=theta.names, #
    theta.names.combi=theta.names.combi, #
    time_val=time_val, #
    type_fzr= type_fzr,#
    type_fx=type_fx,#
    type_fr=type_fr,#
    use.random.effects =  use.random.effects,#
    var.lo = var.lo,#
    var.hi = var.hi,#
    xks = xks, #
    x_tmp.list = x_tmp.list, #
    x_lab_names =x_lab_names , #
    #xx_choice.ci = xx_choice,#
    #zeval.tmp = zeval.tmp, #
    zeval = zeval, #
    z.choice = z.choice,#
    z_tmp.list = z_tmp.list,#
    z_lab_names = z_lab_names #
  ))

}





#######################################################################
#######################################################################
##
## This code converts the new notation into the original code notation for getting results (plots)
##
#######################################################################
#######################################################################
## convert.new.notation.to.old.for.get.results:
## This code converts the new notation into the original code notation for getting results (plots)

convert.new.notation.to.old.for.get.results <- function(study.names,
                                                        #data.sets.as.list,
                                                        time.points.for.prediction,
                                                        time.points.for.conditional.prediction,
                                                        time.points.for.conditional.prediction.toadd,
                                                        nonfunctional.covariate.names,
                                                        nonfunctional.covariate.values.for.prediction,
                                                        functional.covariate.names,
                                                        functional.covariate.values.of.interest,
                                                        functional.covariate.values.of.interest.ci,
                                                        functional.covariate.values.for.prediction,
                                                        xmin,
                                                        xmax,
                                                        #othercovariate.names,
                                                        event.outcome.names,
                                                        #delta.names,
                                                        use_real_data,
                                                        use.functional.beta.intercept,
                                                        use.functional.event.coefficients,
                                                        use.functional.study.coefficients,
                                                        number.of.bootstraps,
                                                        #check.study.equality,
                                                        what.analyzed.separately,
                                                        estimated.parameters.common.for.all.studies,
                                                        #estimate.variances,
                                                        #estimation.when.censoring.depends.on.z,
                                                        glm.link.type,
                                                        use.bootstrap.variance,
                                                        #clusters.are.families,
                                                        ##########################
                                                        ## For plotting results ##
                                                        ##########################
                                                        time.points.of.interest,
                                                        time.points.of.interest.ci,
                                                        label.for.alpha.values.over.time,
                                                        nonfunctional.covariate.comparisons,
                                                        add.number.at.risk.legend,
                                                        ylabel.for.plots.comparing.studies,
                                                        xlabel.for.plots.comparing.studies,
                                                        #####################################
                                                        # Dont we need these parameteres?
                                                        #####################################
                                                        #plot.parameters,
                                                        #do.plots,
                                                        plot.confidence.intervals,
                                                        color.labels,
                                                        legend.labels,
                                                        event.comparison.table,
                                                        functional.covariate.comparisons,
                                                        functional.covariate.comparisons.for.sample.size
                                                        ################################
                                                        ## for computing sample sizes ##
                                                        ################################
                                                        #type1.error,
                                                        #type2.error,
                                                        #treatment.effect,
                                                        #dropout.rate

){


  ##############################################################################################################
  ## We  adjust any clashes(discordants) between what is analyzed separately and assumed common parameters across studies. ##
  ## Can make this into an error message if user accidentaly puts TRUE instead of FALSE as input.    	    ##
  ##############################################################################################################

  if(what.analyzed.separately=="study" | what.analyzed.separately=="studyevent" | length(study.names)==1){
    ## if studies are analyzed separately, we cannot have common parameters across studies
    estimated.parameters.common.for.all.studies <- FALSE
  }



  ## If we only have one event, we should not have random effects in the model.
  ## Right now, the model considers random effects between events, not between studies.
  if(length(event.outcome.names)==1){
    what.analyzed.separately <- "event"
    use.functional.event.coefficients <- FALSE
  }

  if(length(study.names)==1){
    use.functional.study.coefficients <- FALSE
  }


  #######################################################
  ## checking criteria, added on Oct/13/19             ##
  #######################################################
  # if(!is.null(functional.covariate.values.of.interest)|!is.null(xmin)|!is.null(xmax)){
  #
  #   functional.covariate.check<-all(functional.covariate.values.of.interest%in%seq(xmin, xmax))
  #
  #   if(functional.covariate.check!=TRUE){
  #     warning("n/ all elements of functional.covariate.values.of.interest should be between minimum and maximum of covariate values. \n")
  #   }
  #
  #
  # }
  #


  #######################################################
  ## default values to run the estimation procedure    ##
  #######################################################


  ## get default setting for estimation
  estimation.default.values <- default.estimation.setting(use.bootstrap.variance, number.of.bootstraps)

  ## extract seed to generate random data
  #iseed <- estimation.default.values$iseed

  ## for plotting output
  plot.kaplan.meier.curves <- estimation.default.values$plot.kaplan.meier.curves


  ##  In this paper, the cluster within a study corresponds to events from the same individual.
  ##  That is, the cluster info is from the same individual.
  family.data <- estimation.default.values$family.dat
  var.lo <- estimation.default.values$var.lo
  var.hi <- estimation.default.values$var.hi

  #spline.constrain <- estimation.default.values$splines.constrained.at.0


  #gamm.orig <- FALSE

  ## default method
  #method <- c("gamm4","new")[1]

  ###########################
  ## Data-dependent values ##
  ###########################
  default.values <- get.default.values(use_real_data) #, number.of.simulations)

  ## number of simulations
  number.of.simulations <- default.values$number.of.simulations

  ## compute number of studies
  number.of.studies <- length(study.names)

  ## number of clinical events
  number.of.clinical.events <- length(event.outcome.names)

  ## sample size
  #n <- get.sample.size.for.jprat(data.sets.as.list,
  #                              number.of.studies,use_real_data,
  #                              default.values)

  ## maximum sample size across all studies
  #maximum.sample.size <- max(n)

  ## m: number of clinical events per person per study
  #m <- get.number.of.clinical.events.per.person.per.study(study.names,number.of.studies,
  #                                                       n,maximum.sample.size,
  #                                                       number.of.clinical.events)

  ## maximum number of event types
  #maximum.number.of.events <- max(m)

  #######################################################
  ## specify number of knots for alpha(x,t) estimation ##
  #######################################################

  tmp.get.spline.information <-
    get.spline.information(use_real_data,study.names,default.values,
                           event.outcome.names,number.of.clinical.events,
                           number.of.studies)
  #functional.alpha.form <- tmp.get.spline.information$functional.alpha.form
  #knot.length <- tmp.get.spline.information$knot.length
  number.of.functional.alpha <- tmp.get.spline.information$number.of.functional.alpha

  #############################################################################
  ## specify initial coefficient estimates  (only needed for simulated data) ##
  #############################################################################
  tmp.get.coefficient.values <- get.coefficient.values(use.functional.beta.intercept,
                                                       use.functional.event.coefficients,
                                                       use.functional.study.coefficients,
                                                       estimated.parameters.common.for.all.studies,
                                                       default.values,
                                                       number.of.clinical.events,
                                                       number.of.studies)

  functional.beta.intercept <- tmp.get.coefficient.values$functional.beta.intercept
  event.coefficients <-tmp.get.coefficient.values$event.coefficients
  study.coefficients <- tmp.get.coefficient.values$study.coefficients

  ###############################
  ## length of beta functional ##
  ###############################
  maximum.number.of.functional.beta.coefficients <- get.length.beta.functionals(use_real_data,nonfunctional.covariate.names,default.values)

  #############################
  ## Set up parameter labels ##
  #############################
  estimated.components <- c("beta","alphas","Ft")
  estimated.components.study.comparisons <- estimated.components ## what is compared between studies

  if(!is.null(time.points.for.conditional.prediction)){
    estimated.components <- c(estimated.components,"Ft.predicted")
  }


  nonfunctional.parameter.names <- get.parameter.label(maximum.number.of.functional.beta.coefficients,
                                                       functional.beta.intercept,
                                                       event.coefficients,
                                                       study.coefficients)




  ###################################################################
  ## Set up x-covariate labels and where to evaluate alpha(x,t) at ##
  ###################################################################

  functional.covariate.values.for.prediction.per.study <-
    get.functional.covariate.values.for.prediction.per.study(study.names,
                                                             functional.covariate.values.for.prediction)       ## list of functional.covariate.values.for.prediction, per study

  x.label.names <-
    get.functional.covariate.values.for.prediction.label(functional.covariate.values.for.prediction,
                                                         use_real_data,
                                                         xmin,xmax)


  ## number of x's at which to evaluate alpha(x,t)
  num_xx <- max(unlist(lapply(functional.covariate.values.for.prediction.per.study,length.apply)))  ## 18


  ###############################################################################
  ## Set up z-covariate labels and where to evaluate Z *beta(t) for each study ##
  ###############################################################################
  out.nonfunctional.covariate.values.for.prediction.per.study <-
    get.nonfunctional.covariate.values.for.prediction.per.study(study.names,number.of.studies,
                                                                event.outcome.names,
                                                                number.of.clinical.events,
                                                                nonfunctional.covariate.values.for.prediction)

  #nonfunctional.covariate.values.for.prediction.per.study <- out.nonfunctional.covariate.values.for.prediction.per.study$nonfunctional.covariate.values.for.prediction.per.study
  number.of.z.predictions <- out.nonfunctional.covariate.values.for.prediction.per.study$number.of.z.predictions
  z.label.names <- get.nonfunctional.covariate.values.for.prediction.label(nonfunctional.covariate.values.for.prediction)


  #######################################
  ## Update Time points for prediction ##
  #######################################
  time.points.for.prediction <- get.time.points.for.prediction(time.points.for.prediction,
                                                               time.points.for.conditional.prediction,
                                                               time.points.for.conditional.prediction.toadd) ## either inputted from user or default simulation setting  40  45  50  55  60  65  70  75  80  85  90  95 100


  ## number of time points for evaluation
  num_time <- length(time.points.for.prediction)

  ###################################################
  ## check if we need to compute study differences ##
  ###################################################
  # compute.study.differences <- run.study.differences(check.study.equality,
  #                                                    estimated.parameters.common.for.all.studies,
  #                                                    estimation.default.values,
  #                                                    number.of.studies)   ## FALSE


  ########################################
  ## Set up arrays to store information ##
  ########################################


  ## get study comparison information for pairwise comparisons between studies
  study.comparison.information <- get.study.comparison.information(study.names)
  number.of.study.comparisons <- study.comparison.information$number.of.study.comparisons
  all.study.comparisons <- study.comparison.information$all.study.comparisons
  name.of.study.comparisons <- study.comparison.information$name.of.study.comparisons


  ###################################################
  ## Process data (or simulated data) to run JPRAT ##
  ## Convert to original notation used in the code ##
  ###################################################
  #if(use_real_data==FALSE){
  #  a0 <- default.values$a0
  #  delta.outcomes.per.study <- NULL
  #  event.outcomes.per.study <- NULL

  #  functional.g.form <- default.values$functional.g.form
  #  functional.covariates.per.study <- NULL
  #  functional.beta.coefficients <- default.values$functional.beta.coefficients


  #  mix_n <- default.values$mix_n

  #  nonfunctional.covariates.per.study <- NULL
  #  number.of.functional.beta.coefficients.per.study <- default.values$number.of.functional.beta.coefficients.per.study

  #  othercovariates.per.study <- NULL

  #  pseudovalues.replaced.by.arbitrary.values <- default.values$pseudovalues.replaced.by.arbitrary.values

  #  random.effect.first.parameter <- default.values$random.effect.first.parameter
  #  random.effect.second.parameter <- default.values$random.effect.second.parameter
  #  random.effect.first.parameter2 <- default.values$random.effect.first.parameter2
  #  random.effect.second.parameter2 <- default.values$random.effect.second.parameter2
  #  random.effect.third.parameter <- default.values$random.effect.third.parameter

  #  study.random.effect.parameter  <- default.values$study.random.effect.parameter
  #  simulated.random.effects.depends.on.covariates  <- default.values$simulated.random.effects.depends.on.covariates
  #  simulate.data.with.random.effects <- default.values$simulate.data.with.random.effects
  #  simulated.z.covariate.distribution <- default.values$simulated.z.covariate.distribution
  #  simulated.x.covariate.distribution <- default.values$simulated.x.covariate.distribution
  #  simulated.censor.rate <- default.values$simulated.censor.rate
  #  simulated.random.effect.distribution <- default.values$simulated.random.effect.distribution
  #  simulated.censoring.depends.on.z <- default.values$simulated.censoring.depends.on.z
  #  simulated.parameters.common.for.all.studies <- default.values$simulated.parameters.common.for.all.studies

  #  unknown.z.covariate.parameters <- default.values$unknown.z.covariate.parameters
  #  unknown.x.covariate.parameters <- default.values$unknown.x.covariate.parameters
  #  unknown.random.effect.parameters <- default.values$unknown.random.effect.parameters

  #  x.covariate.first.parameter<- default.values$x.covariate.first.parameter
  #  x.covariate.second.parameter<- default.values$x.covariate.second.parameter

  #  z.covariate.first.parameter <- default.values$z.covariate.first.parameter
  #  z.covariate.second.parameter<- default.values$z.covariate.second.parameter
  #  type1.error <- NULL
  #  type2.error <- NULL
  #  treatment.effect <- NULL
  #  dropout.rate <- NULL
  #} else {
  #####################
  ## Real data setup ##
  #####################
  # tmp.process.real.data <- process.real.data(
  #   study.names,
  #   data.sets.as.list,
  #   functional.covariate.names,
  #   xmin,
  #   xmax,
  #   othercovariate.names,
  #   event.outcome.names,
  #   delta.names,
  #   nonfunctional.covariate.names
  # )

  ## go here 6/26/19
  #a0 <- NULL



  #delta.outcomes.per.study <- tmp.process.real.data$delta.outcomes.per.study
  #event.outcomes.per.study <- tmp.process.real.data$event.outcomes.per.study
  #functional.covariates.per.study <- tmp.process.real.data$functional.covariates.per.study

  #functional.beta.coefficients <- tmp.process.real.data$functional.beta.coefficients
  #functional.g.form <- NULL
  #mix_n <- NULL
  #number.of.functional.beta.coefficients.per.study <-
  # tmp.process.real.data$number.of.functional.beta.coefficients.per.study
  #nonfunctional.covariates.per.study <- tmp.process.real.data$nonfunctional.covariates.per.study

  #othercovariates.per.study <- tmp.process.real.data$othercovariates.per.study

  #pseudovalues.replaced.by.arbitrary.values <- FALSE ## only used in simulated data

  # random.effect.first.parameter <- NULL
  # random.effect.second.parameter <- NULL
  # random.effect.first.parameter2 <- NULL
  # random.effect.second.parameter2 <- NULL
  # random.effect.third.parameter <- NULL
  #
  # study.random.effect.parameter <- NULL
  # simulated.random.effects.depends.on.covariates  <- NULL
  # simulated.censoring.depends.on.z <- NULL
  # simulated.censor.rate <- NULL
  # simulated.random.effect.distribution <- NULL
  # simulate.data.with.random.effects <- NULL
  # simulated.z.covariate.distribution <- NULL
  # simulated.x.covariate.distribution <- NULL
  # simulated.parameters.common.for.all.studies  <- NULL
  #
  # unknown.random.effect.parameters <- NULL
  # unknown.z.covariate.parameters <- NULL
  # unknown.x.covariate.parameters <- NULL
  #
  # x.covariate.first.parameter<- NULL
  # x.covariate.second.parameter<- NULL
  #
  # z.covariate.first.parameter <- NULL
  # z.covariate.second.parameter<- NULL
  # }

  ########################
  ## to display results ##
  ########################
  results.default.values <- default.results.setting(#use_real_data,
    nonfunctional.covariate.values.for.prediction,
    functional.covariate.values.of.interest,
    nonfunctional.parameter.names)


  plotted.time.range.displayed  <- results.default.values$plotted.time.range.displayed
  normalize.cag.repeat.values <- results.default.values$normalize.cag.repeat.values

  ################################
  # NOW they are global arguments
  ################################
  #ylabel.for.plots.comparing.studies <- results.default.values$ylabel.for.plots.comparing.studies
  #xlabel.for.plots.comparing.studies <- results.default.values$xlabel.for.plots.comparing.studies
  #add.number.at.risk.legend <- results.default.values$add.number.at.risk.legend
  alpha_label_x_direction <- results.default.values$alpha_label_x_direction




  alpha.cut <- results.default.values$alpha.cut
  beta.cut <- results.default.values$beta.cut
  number.of.significant.digits <- results.default.values$number.of.significant.digits
  legend.position.use <- results.default.values$legend.position.use


  ## for specific HD examples
  ylim.key <- results.default.values$ylim.key
  ylim.setting <- results.default.values$ylim.setting
  xx_choice.sample.size <-functional.covariate.comparisons.for.sample.size

  ########################################################
  ## Convert to original notation used to make the code ##
  ########################################################


  ##
  convert.x <- normalize.cag.repeat.values
  round.set <- number.of.significant.digits
  study.ylab <- ylabel.for.plots.comparing.studies
  study.xlab <- xlabel.for.plots.comparing.studies
  time.cut <- plotted.time.range.displayed



  analyze.separately <- what.analyzed.separately
  alphax_lab <- alpha_label_x_direction
  add.second.legend <- add.number.at.risk.legend


  colors.use <- color.labels
  common.param.estimation <- estimated.parameters.common.for.all.studies
  conf.int.use <- plot.confidence.intervals
  real_data <- use_real_data
  nn.comparison <- event.comparison.table


  s.names.short <- legend.labels
  s.names <- event.outcome.names
  time_choice.predicted <- time.points.for.conditional.prediction
  time_choice.predicted.toadd<- time.points.for.conditional.prediction.toadd
  time_choice <- time.points.of.interest
  time_choice.ci <- time.points.of.interest.ci


  xx_choice <- functional.covariate.values.of.interest
  xx_comp <- functional.covariate.comparisons
  zz_comp<-nonfunctional.covariate.comparisons

  combi.names <- name.of.study.comparisons
  combi.study <- number.of.study.comparisons
  combi.choice <- all.study.comparisons

  la <- number.of.functional.alpha
  link.type <- glm.link.type
  ##
  num_study <- number.of.studies
  num_time <- num_time
  np <- number.of.clinical.events
  num_xx <- num_xx
  param.label <- nonfunctional.parameter.names
  plot.KM <- plot.kaplan.meier.curves

  ##
  simus <- number.of.simulations
  theta.names <- estimated.components
  theta.names.combi <- estimated.components.study.comparisons
  time_val <- time.points.for.prediction

  ##
  xks <- functional.covariate.values.for.prediction.per.study
  x_lab_names <- x.label.names
  xx_choice.ci <- functional.covariate.values.of.interest.ci
  z.choice <- number.of.z.predictions
  z_lab_names <- z.label.names
  ##

  ## Null
  #type1.error <- type1.error
  #type2.error <- type2.error
  #treatment.effect <- treatment.effect
  #dropout.rate <- dropout.rate

  #z_file_labels <- results.default.values$z_file_labels
  #plot.nadaraya.watson <- results.default.values$plot.nadaraya.watson
  #use.bootstraps.confidence.interval <- results.default.values$use.bootstraps.confidence.interval



  # alphax.ylab<- results.default.values$alphax.ylab
  # alphax.xlab <- results.default.values$alphax.xlab
  # alphat.ylab <- results.default.values$alphat.ylab
  # alphat.xlab <- results.default.values$alphat.xlab
  alphat_lab <- label.for.alpha.values.over.time
  #z_tmp.list <- nonfunctional.covariates.per.study
  #x_tmp.list <- functional.covariates.per.study
  #zeval <- nonfunctional.covariate.values.for.prediction.per.study
  #s_tmp.list <- event.outcomes.per.study
  #delta_tmp.list <- delta.outcomes.per.study



  #boot.ci <- use.bootstraps.confidence.interval
  #plot.nw <- plot.nadaraya.watson
  #z_lab <- z_file_labels
  #est.cens.depend.z <- estimation.when.censoring.depends.on.z
  #boot <- number.of.bootstraps







  #a0 <- a0
  #arbitrary <- pseudovalues.replaced.by.arbitrary.values
  #axmod <- functional.alpha.form
  #beta0int <- functional.beta.intercept
  #beta0 <- functional.beta.coefficients
  #beta_lab <- lapply( number.of.functional.beta.coefficients.per.study ,double.get.label)
  #knot.length <- knot.length

  #censorrate <- simulated.censor.rate
  #common.param.data <- simulated.parameters.common.for.all.studies
  # frform <- simulated.random.effect.distribution
  # fzrform <- simulated.z.covariate.distribution
  # fxform <- simulated.x.covariate.distribution
  # gamma.param <- event.coefficients
  # gtmod <- functional.g.form
  # gen.cens.depend.z <- simulated.censoring.depends.on.z
  #lb <- number.of.functional.beta.coefficients.per.study
  #lb.max <- maximum.number.of.functional.beta.coefficients
  #mix_n <- mix_n
  #maxm <- maximum.number.of.events
  #nmax <- maximum.sample.size
  #n <- n
  #omega.param <- study.coefficients
  # par1_fzr <- z.covariate.first.parameter
  # par2_fzr <- z.covariate.second.parameter
  # par1_fx <- x.covariate.first.parameter
  # par2_fx <- x.covariate.second.parameter
  # par1_fr <- random.effect.first.parameter
  # par2_fr <-  random.effect.second.parameter
  # par1_fr2 <-random.effect.first.parameter2
  # par2_fr2 <-  random.effect.second.parameter2
  # par_fu <- study.random.effect.parameter
  # par3_fr <- random.effect.third.parameter
  # type_fzr <- unknown.z.covariate.parameters
  # type_fx <- 	unknown.x.covariate.parameters
  # type_fr <- unknown.random.effect.parameters
  #use.random.effects <-  simulate.data.with.random.effects
  #randomeffects.covariate.dependent <- simulated.random.effects.depends.on.covariates
  #zeval.tmp <- nonfunctional.covariate.values.for.prediction
  #var.est <- estimate.variances
  #var.boot <- 	use.bootstrap.variance







  #
  #
  #
  #
  #
  #
  return(list(
    ##################
    ## from results ##
    ##################
    alpha.cut=alpha.cut, #
    add.second.legend=add.second.legend, #
    alphax_lab=alphax_lab, #
    beta.cut=beta.cut,#
    convert.x=convert.x,#
    legend.position.use=legend.position.use,#
    round.set=round.set,#
    #
    study.ylab=study.ylab, #
    study.xlab=study.xlab, #
    time.cut =time.cut, #
    ylim.key=ylim.key, #
    ylim.setting=ylim.setting,#
    #####################################
    ## Input converted to old notation ##
    #####################################
    analyze.separately=analyze.separately, #
    colors.use=colors.use,#
    common.param.estimation=common.param.estimation, #
    conf.int.use= conf.int.use,#
    nn.comparison=nn.comparison,#
    real_data=real_data,#
    s.names.short = s.names.short,#
    s.names=s.names,#
    time_choice.predicted=time_choice.predicted,#
    time_choice.predicted.toadd=time_choice.predicted.toadd,#
    time_choice=time_choice,#
    time_choice.ci=time_choice.ci, #
    #
    xx_choice = functional.covariate.values.of.interest,#
    zz_comp = zz_comp,#
    xx_comp = xx_comp,
    xx_choice.sample.size=xx_choice.sample.size,
    #
    # alphat_lab=alphat_lab,#
    # alphax.ylab =alphax.ylab,
    # alphax.xlab = alphax.xlab,
    # alphat.ylab = alphat.ylab,
    # alphat.xlab = alphat.xlab,
    # z_tmp.list = z_tmp.list,#
    # x_tmp.list = x_tmp.list, #
    # zeval = zeval, #
    # s_tmp.list=s_tmp.list, #
    # compute.study.differences=compute.study.differences, #
    # delta_tmp.list=delta_tmp.list, #
    # boot.ci=boot.ci,#
    # plot.nw=plot.nw, #
    # z_lab = z_file_labels, #
    # boot=boot, #
    # est.cens.depend.z=est.cens.depend.z,#
    #######################################
    ## Depends on simulated or real data ##
    #######################################
    #a0=a0, #
    #arbitrary=arbitrary, #
    #axmod=axmod, #
    #beta0int=beta0int, #
    #beta0=beta0,#
    #beta_lab =beta_lab, ##
    #censorrate=censorrate,#
    #common.param.data=common.param.data, #
    # frform=frform,#
    # fzrform=fzrform, #
    # fxform=fxform,#
    # gamm.orig=gamm.orig,#
    # gamma.param=gamma.param, #
    # gtmod=gtmod,  #
    # gen.cens.depend.z=gen.cens.depend.z,#
    # iseed= iseed,  #
    # knot.length=knot.length, #
    # lb=lb, 	 #
    # lb.max=lb.max, 	#
    # link.type=link.type,#
    # m=m,#
    # method=method, #
    # mix_n=mix_n, #
    # maxm=maxm, #
    # nmax=nmax, #
    # omega.param=omega.param,  #
    # n=n,#
    # par1_fzr=par1_fzr, #
    # par2_fzr=par2_fzr,#
    # par1_fx=par1_fx,#
    # par2_fx=par2_fx,#
    # par1_fr=par1_fr,#
    # par2_fr=par2_fr,#
    # par1_fr2=par1_fr2, 	#
    # par2_fr2=par2_fr2,#
    # par3_fr=par3_fr,#
    # par_fu=par_fu, #
    # randomeffects.covariate.dependent=randomeffects.covariate.dependent,#
    # spline.constrain=spline.constrain, #
    # type_fzr= type_fzr,#
    # type_fx=type_fx,#
    # type_fr=type_fr,#
    # use.random.effects =  use.random.effects,#
    # var.boot =var.boot,#
    # var.est = estimate.variances,#
    combi.study=combi.study, #
    combi.choice=combi.choice, #
    combi.names =combi.names, #



    family.data=family.data,#

    la=la, #

    num_study=num_study, #
    num_time=num_time, #

    np=np, 	#
    num_xx=num_xx,#

    param.label=param.label, #

    plot.KM=plot.KM,


    simus=simus, #
    theta.names=theta.names, #
    theta.names.combi=theta.names.combi, #
    time_val=time_val, #

    var.lo = var.lo,#
    var.hi = var.hi,#
    xks = xks, #

    x_lab_names =x_lab_names , #
    xx_choice.ci = xx_choice,#
    #zeval.tmp = zeval.tmp, #

    z.choice = z.choice,#

    z_lab_names = z_lab_names #
  ))

}




#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#########################################################################
# Users may not interested in those functions below                   ###
# For now, we did not add documents for those functions below         ###
#########################################################################
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++





#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+
#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+
##
##
## Functions used in multiple places
##
##
#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+
#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+


##################################################
## get.empty.list: make empty lists for storage ##
##################################################


get.empty.list <- function(names){
  out <-  vector("list",length(names))
  names(out) <- names
  return(out)
}



####################################
## functions to manipulate  lists ##
####################################

nrow.apply <- function(x){
  return(nrow(x))
}


length.apply <- function(x){
  return(length(x))
}


double.length.apply <- function(x){
  return(lapply(x,function(y) length(y)))
}


double.max.apply <- function(x){
  return(lapply(x,function(y) max(y)))
}


double.get.label <- function(x){
  return(lapply(x,function(y) 1:y))
}


double.null <- function(x){
  return(lapply(x,function(y) is.null(y)))
}

## check for complete cases across columns

check.complete.cases <- function(f){sum(complete.cases(f))}


##############################################
## funtion to flatten an array to a matrix  ##
##############################################

#' @import utils
flatten.array <- function(x,dim.order,flatten.name,theta=NULL){
  #flatten.array(avg.out,dim.order=names(dimnames(avg.out)),
  #flatten.name="results",theta=theta)

  ## permute order to match original output
  x <- aperm(x,dim.order)

  dim.flatten <- which(names(dimnames(x))==flatten.name)   ## 1 ## results
  out1 <- apply(x,dim.flatten,c)  ## column wise - restuls of each study are binded in a row
  dim.use <- 1:length(dimnames(x)) ## results, study, event,  theta
  dim.use <- setdiff(dim.use,dim.flatten) ## study, event, theta
  out2  <- expand.grid(dimnames(x)[dim.use])  ## study, event, theta

  if(!is.null(theta) & length(theta)==1){
    out <- data.frame(theta=theta,out2,out1)
  } else{
    out <- data.frame(out2,out1)  ##  dataframe of study, event, theta, abs.bias, bias, emp.var, est.var, CP.cov, MSE
  }

  tmp <- unique(c(rev(dim.order),"theta"))
  tmp <- tmp[-which(tmp==flatten.name)]
  other.names <- colnames(out)
  other.names <- other.names[!colnames(out) %in% tmp]  ##"abs.bias" "bias"     "emp.var"  "est.var"  "X95..cov" "MSE"
  out <- out[,c(tmp,other.names)]

  return(out)
}

##
##
##
##
##
##
##
##
##
##

#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+
#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+
##
##
## Functions used to process data setup
##
##
#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+
#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+

## convert uniform CAG to real CAG
## This function converts the uniformly distributed values (CAG repeats length) on [0,1]
## to the original values (CAG repeats length from real data).

convert.cag <- function(x,xmin=xmin,xmax=xmax){
  xorig <- x*(xmax-xmin)+xmin
  return(xorig)
}

## convert CAG to uniform CAG
reverse.cag <- function(x,xmin=xmin,xmax=xmax){
  xnew <- (x-xmin)/(xmax-xmin)
  return(xnew)
}


##############################################################################
## process.real.data: function used to process actual data given by analyst ##
##############################################################################

process.real.data <- function(
  study.names,
  data.sets.as.list,
  functional.covariate.names,
  xmin,
  xmax,
  othercovariate.names,
  event.outcome.names,
  delta.names,
  nonfunctional.covariate.names
){


  ############################################################
  ## Create lists for storing components from the data.sets ##
  ############################################################
  tmp.list <- get.empty.list(study.names)
  event.outcomes.per.study <- tmp.list  ## event-times for each study
  delta.outcomes.per.study <- tmp.list  ## censoring indicator for each study
  functional.covariates.per.study <- tmp.list		## x-covariates for each study
  nonfunctional.covariates.per.study <- tmp.list		## z-covariates for each study
  othercovariates.per.study <- tmp.list ## other covariates for each study
  beta0.list <- tmp.list



  ##################
  ## Process data ##
  ##################
  for(ss in 1:length(study.names)){
    ################################################
    ## data components: to put data into function ##
    ################################################

    study.use <- names(tmp.list)[ss]
    print(study.use)
    data.tmp <- data.sets.as.list[[study.use]]

    event.outcomes.per.study[[study.use]] <- as.matrix(data.tmp[,event.outcome.names])  ## 430, 1
    delta.outcomes.per.study[[study.use]] <- as.matrix(data.tmp[,delta.names]) ## 430, 1
    functional.covariates.per.study[[study.use]] <- as.matrix(data.tmp[,functional.covariate.names]) ## 430

    ## standardize this to [0,1]                                                                                                               ## reverse.cag, xmin, xmax are optional arguments
    functional.covariates.per.study[[study.use]] <- apply(functional.covariates.per.study[[study.use]],c(1,2),reverse.cag,xmin=xmin,xmax=xmax) ## a vector giving the substripts which the function will be applied over c(row, columns)
    othercovariates.per.study[[study.use]] <- as.matrix(data.tmp[,othercovariate.names])


    #################################################################
    ## create dimension of nonfunctional covariates for each study ##
    #################################################################
    mynames <- paste0("beta0",1:length(event.outcome.names))
    beta0 <- get.empty.list(mynames)
    lb.max <- length(nonfunctional.covariate.names)
    for(nn in 1:length(event.outcome.names)){
      beta0[[nn]] <- rep(1,lb.max)
    }
    beta0.list[[study.use]] <- beta0   ## 1

    ########################################
    ## nonfunctional covariates per study ##
    ########################################
    ntmp <- nrow(data.tmp)  ## 430
    z_tmp.list <- tmp.list  ## cohort, predict, pharos
    z_tmp.list[[study.use]] <- array(0,dim=c(ntmp,length(event.outcome.names),
                                             lb.max),
                                     dimnames=list(
                                       paste0("n",1:ntmp),
                                       event.outcome.names,               ##  hdage_nobase
                                       paste0("lb",1:lb.max)))
    for(nn in 1:length(event.outcome.names)){
      z_tmp.list[[study.use]][,nn,] <- as.matrix(data.tmp[,nonfunctional.covariate.names])
    }
    nonfunctional.covariates.per.study[[study.use]] <- z_tmp.list[[study.use]]


  }

  number.of.functional.beta.coefficients.per.study <- lapply(beta0.list,double.length.apply)


  list(event.outcomes.per.study=event.outcomes.per.study,
       delta.outcomes.per.study=delta.outcomes.per.study,
       nonfunctional.covariates.per.study=nonfunctional.covariates.per.study,
       functional.covariates.per.study=functional.covariates.per.study,
       othercovariates.per.study=othercovariates.per.study,
       number.of.functional.beta.coefficients.per.study=number.of.functional.beta.coefficients.per.study,
       functional.beta.coefficients = beta0
  )
}





###################################################################################################################################
## get.HD.nonfunctional.covariate.names.and.comparisons: function to determine which HD non-functional covariates to use in model
##   and how to compare them in plots ##
###################################################################################################################################


get.HD.nonfunctional.covariate.names.and.comparisons <- function(analysis.to.run, who.knows.genetic.information){
  ################################################
  ## covariates used when comparing event types ##
  ################################################
  if(analysis.to.run == 1 | analysis.to.run == 2 | analysis.to.run == 3 | analysis.to.run == 4 | analysis.to.run == 5 |
     analysis.to.run == 6 | analysis.to.run==7 | analysis.to.run==8 | analysis.to.run==9 | analysis.to.run==10
  ){
    ## zz comparisons
    if(who.knows.genetic.information!="none"){
      ##########################################################################
      ## Setting where we compare rater knowledge of genetic mutation status. ##
      ##########################################################################
      nonfunctional.covariate.names <- NULL

      ## compares set where rater knows to rater doesn't know
      nonfunctional.covariate.comparisons<- list(comp1=c(1,2))
    } else {

      ##############################################
      ## Setting used for clinical and stat paper ##
      ##############################################
      if(!exists(as.character(substitute(nonfunctional.covariate.names)))){
        nonfunctional.covariate.names <- c("base_age","educ_cat","gender")
      }

      ######################################################################
      ## When nonfunctional.covariate.names are base_age, education, and gender
      #####################################################################
      if(all(nonfunctional.covariate.names==c("base_age","educ_cat","gender"))){

        nonfunctional.covariate.comparisons<- list(
          ## compare males to females
          comp1=c(1,3),
          comp2=c(2,4),

          # compare no higher ed to higher ed
          comp3=c(1,2),
          comp4=c(3,4))
      } else if(all(nonfunctional.covariate.names==c("gender"))){
        ## compares males to females
        nonfunctional.covariate.comparisons<- list(comp1=c(1,2))
      }
    }
  } else if(analysis.to.run == "1a" | analysis.to.run == "2a" | analysis.to.run == "3a" | analysis.to.run == "4a" | analysis.to.run == "5a" |
            analysis.to.run == "6a" | analysis.to.run=="7a" | analysis.to.run=="8a"){

    ##############################################
    ## Setting used for clinical and stat paper ##
    ##############################################
    if(!exists(as.character(substitute(nonfunctional.covariate.names)))){
      nonfunctional.covariate.names <- c("base_age","gender")
    }

    nonfunctional.covariate.comparisons<- list(
      ## compare males to females
      comp1=c(1,2))

  } else if(analysis.to.run == "1b" | analysis.to.run == "2b" | analysis.to.run == "3b" | analysis.to.run == "4b" | analysis.to.run == "5b" |
            analysis.to.run == "6b" | analysis.to.run=="7b" | analysis.to.run=="8b"){

    ##############################################
    ## Setting used for clinical and stat paper ##
    ##############################################
    if(!exists(as.character(substitute(nonfunctional.covariate.names)))){
      nonfunctional.covariate.names <- c("base_age","educ_cat")
    }

    nonfunctional.covariate.comparisons<- list(
      ## compare high to low education
      comp1=c(1,2))

  } else if(analysis.to.run == "1c" | analysis.to.run == "2c" | analysis.to.run == "3c" | analysis.to.run == "4c" | analysis.to.run == "5c" |
            analysis.to.run == "6c" | analysis.to.run=="7c" | analysis.to.run=="8c" |
            analysis.to.run=="8e" | analysis.to.run=="7c-converters"){

    ##############################################
    ## Setting used for clinical and stat paper ##
    ##############################################
    if(!exists(as.character(substitute(nonfunctional.covariate.names)))){
      nonfunctional.covariate.names <- c("base_age")
    }

    ## we need to re-set it back to "base_age" for "7c-converters"
    if(analysis.to.run=="7c-converters"){
      nonfunctional.covariate.names <- c("base_age")
    }

    nonfunctional.covariate.comparisons<- list(
      ## no comparisons
      comp1=c(1,1))

  } else if(analysis.to.run=="7d" | analysis.to.run=="8d"){
    ##############################################
    ## Setting used for clinical and stat paper ##
    ##############################################
    if(!exists(as.character(substitute(nonfunctional.covariate.names)))){
      nonfunctional.covariate.names <- c("base_age")
    }

    nonfunctional.covariate.comparisons<- list(
      ## compare different ages at baseline
      comp1=c(1,2))

  } else {
    ## No covariates used
    nonfunctional.covariate.names <- NULL

    ## comparison between covariates: set to dummy values.
    nonfunctional.covariate.comparisons<- list(comp1=c(1,2))
  }

  return(list(nonfunctional.covariate.names=nonfunctional.covariate.names,
              nonfunctional.covariate.comparisons=nonfunctional.covariate.comparisons))
}



###################################################################################################################################
## get.HD.event.outcomes: function to determine which HD event outcomes to analyze
###################################################################################################################################

get.HD.event.outcomes <- function(analysis.to.run){
  ##########################################
  ## Event types used:
  ## motor.use : motor-type events used (can be NULL)
  ## mci.use : cognive events used (can be NULL)
  ## beh.use : behaviorial events used (can be NULL)
  ## WARNING: at least one event type must be analyzed!
  #########################################


  if(analysis.to.run==1 | analysis.to.run==9 | analysis.to.run==10 | analysis.to.run=="1a" | analysis.to.run=="1b" | analysis.to.run=="1c"){
    ##############################################################
    ## Event types: 1) motor-diagnosis DCL >=4 ("hdage_nobase"),
    ##   	    2) first cognitive impairment ("mcione")
    ## Studies where event types are observed: COHORT,PREDICT, PHAROS
    #############################################################

    motor.use <- "hdage_nobase"  ## Looking at hdage_nobase, we will remove many instances.
    mci.use <- "mcione"
    beh.use <- NULL
    time.points.for.prediction <- seq(40,60,by=5)

    ## 4/5/2017: Add in these time points for comparison to Kaplan-Meier estimates.
    ##time.points.for.prediction <- sort(c(time.points.for.prediction, 58,59))

  } else if(analysis.to.run==2 | analysis.to.run=="2a"){
    ##################################################################
    ## Event types: 1) motor-diagnosis DCL >=3 ("hdage_new_nobase"),
    ##   	    2) first cognitive impairment ("mcione")
    ## Studies where event types are observed: COHORT, PHAROS, PREDICT
    ##################################################################

    motor.use <- "hdage_new_nobase"
    mci.use <- "mcione"
    beh.use <- NULL
    time.points.for.prediction <- seq(45,75,by=5)
  } else if(analysis.to.run==3 | analysis.to.run=="3a"){
    ##################################################################
    ## Event types: 1) MCI events: SDMT, Stroop Color (strcol), Stroop word (strwrd), Stroop Interference (strint)
    ## Studies where event types are observed: COHORT, PHAROS, PREDICT
    ##################################################################
    ## changed 3/20/2017
    motor.use <- NULL
    mci.use <- c("sdmt","strcol","strwrd","strint")
    beh.use <- NULL
    time.points.for.prediction <- seq(45,75,by=5)
  } else if(analysis.to.run==4 | analysis.to.run=="4a"){
    ##################################################################
    ## Event types: 1) motor-diagnosis DCL >=3 ("hdage_new_nobase"),
    ##   	    2) first MCI based on SDMT only ("sdmt")
    ## Studies where event types are observed: ALL
    ##################################################################
    ## changed 8/26/2015
    motor.use <- "hdage_new_nobase"
    mci.use <- c("sdmt")#,"strcol","strwrd","strint")
    beh.use <- NULL
    time.points.for.prediction <- seq(45,75,by=5)
  } else if(analysis.to.run==5 | analysis.to.run=="5a"){
    ##################################################################
    ## Event types: 1) motor-diagnosis DCL >=4 ("hdage_nobase"),
    ##   	    2) first behavioral impairment ("behone")
    ## Studies where event types are observed: COHORT, PHAROS, PREDICT, TRACK, ENROLL
    ##################################################################
    motor.use <- "hdage_nobase"
    mci.use <- NULL
    beh.use <- "behone"
    time.points.for.prediction <- seq(40,60,by=5)
  } else if(analysis.to.run==6 | analysis.to.run=="6a" | analysis.to.run=="6b" | analysis.to.run=="6c"){
    ##################################################################
    ## Event types: 1) motor-diagnosis DCL >=4 ("hdage_nobase"),
    ##   	    2) first behavioral impairment based on depression ONLY ("dep2")
    ##		    3) first cognitive impairment ("mcione")
    ## Studies where event types are observed: COHORT, PHAROS, PREDICT, TRACK, ENROLL
    ##################################################################
    motor.use <- "hdage_nobase"
    mci.use <- NULL ##"mcione"
    beh.use <- c("dep2")
    time.points.for.prediction <- seq(40,60,by=5)
  } else if(analysis.to.run==7 | analysis.to.run=="7a" | analysis.to.run=="7b" | analysis.to.run=="7c" | analysis.to.run=="7d"){
    ##################################################################
    ## Event types: 1) motor-diagnosis DCL >=4 ("hdage_nobase"),
    ##   	    2) first behavioral impairment based on depression ONLY ("dep2")
    ##		    3) first cognitive impairment ("mcione")
    ##        4) first stage II TFC occurred ("tfctwo")
    ## Studies where event types are observed: COHORT, PHAROS, PREDICT, TRACK, ENROLL
    ##################################################################
    motor.use <- "hdage_nobase"
    mci.use <- "mcione"
    beh.use <-NULL # c("dep2")
    ## aug.30
    tfc.use<-"tfctwo"
    ##
    time.points.for.prediction <- seq(46, 66,  by=5)  #seq(40,60,by=5)
  } else if(analysis.to.run=="7c-converters"){
    ##################################################################
    ## Event types: 1) motor-diagnosis DCL >=4 ("hdage_nobase"),
    ## Studies where event types are observed: COHORT, PHAROS, PREDICT, TRACK, ENROLL
    ##################################################################
    motor.use <- "hdage_nobase"
    mci.use <- NULL
    beh.use <- NULL
    tfc.use <- NULL
    time.points.for.prediction <- seq(40,60,by=5)
  } else if(analysis.to.run==8 | analysis.to.run=="8a" | analysis.to.run=="8b" |
            analysis.to.run=="8c" | analysis.to.run=="8d" |
            analysis.to.run=="8e" ){
    ##################################################################
    ## Event types:    motor-diagnosis DCL >=4 ("hdage_nobase"),
    ##   	    2) tfctwo: total functional capacity: stage 2
    ##		    3) mcimd: multi-domain mild cognitive impairment
    ## Studies where event types are observed: COHORT, PHAROS, PREDICT,TRACK, ENROLL
    ##################################################################
    motor.use <- "hdage_nobase"
    mci.use <- "mcimd"
    beh.use <- "tfctwo"
    time.points.for.prediction <- seq(40,60,by=5)

    if(analysis.to.run=="8e"){
      time.points.for.prediction <- seq(40,60,by=2)
    }
  }

  ## Events to use in analysis
  event.outcome.names <- c(motor.use,mci.use,beh.use, tfc.use)
  delta.names <- paste("delta.",event.outcome.names,sep="")

  return(list(time.points.for.prediction=time.points.for.prediction,
              event.outcome.names=event.outcome.names,
              delta.names=delta.names,
              motor.use=motor.use,
              beh.use=beh.use,
              mci.use=mci.use,
              tfc.use=tfc.use))

}




###################################################################################################################################
## get.HD.nonfunctional.covariate.names.based.on.events: function to determine which additional HD non-functional covariates to use in model
##   depending on what events are being analyzed ##
###################################################################################################################################

get.HD.nonfunctional.covariate.names.based.on.events <- function(motor.use, mci.use, beh.use, tfc.use, who.knows.genetic.information){
  ##########################################################################
  ## Additional motor-type covariates if we analyze motor-diagnosis event ##
  ##########################################################################
  if(!is.null(motor.use)){
    ##motor.score.use <- paste("motor.",motor.use,sep="")
    ##motor.score.names <- "TMS"
    ##motor.score.use <- paste("DCL.prior.",motor.use,sep="")
    ##motor.score.use <- paste("DCL.base.",motor.use,sep="")
    motor.score.use <- NULL
  } else {
    motor.score.use <- NULL
    motor.score.names <- NULL
  }

  ############################################################################
  ## Additional cognitive-type covariates if we analyze MCI-diagnosis event ##
  ############################################################################
  if(!is.null(mci.use)){
    ## mci.score.use <- NULL
    ## for(ll in 1:length(mci.use)){
    ##   if(mci.use[ll]!="mcione"){
    ##     mci.score.use <- c(mci.score.use,paste("score.",mci.use[ll],sep=""))
    ##   }
    ## }
    ## mci.score.names <- mci.use
    mci.score.use <- NULL
    mci.score.names <- NULL
  } else {
    mci.score.use <- NULL
    mci.score.names <- NULL
  }

  ################################################################################
  ## Additional behavior-type covariates if we analyze behavior-diagnosis event ##
  ################################################################################
  if(!is.null(beh.use)){
    #beh.score.use <- NULL
    #for(ll in 1:length(beh.use)){
    #  if(beh.use[ll]!="behone"){
    #    beh.score.use <- ct(beh.score.use,paste("score.",beh.use[ll],sep=""))
    #  }
    #}
    #beh.score.names <- beh.use
    beh.score.use <- NULL
    beh.score.names <- NULL
  } else {
    beh.score.use <- NULL
    beh.score.names <- NULL
  }


  ### aug.30

  ################################################################################
  ## Additional behavior-type covariates if we analyze behavior-diagnosis event ##
  ################################################################################
  if(!is.null(tfc.use)){
    #beh.score.use <- NULL
    #for(ll in 1:length(beh.use)){
    #  if(beh.use[ll]!="behone"){
    #    beh.score.use <- ct(beh.score.use,paste("score.",beh.use[ll],sep=""))
    #  }
    #}
    #beh.score.names <- beh.use
    tfc.score.use <- NULL
    tfc.score.names <- NULL
  } else {
    tfc.score.use <- NULL
    tfc.score.names <- NULL
  }

  #######################
  ## set up covariates ##
  #######################
  gene.use <- get.genetic.knowledge.covariates(who.knows.genetic.information)
  additional.covariates <- c(motor.score.use, mci.score.use, beh.score.use, tfc.score.use, gene.use)
  return(additional.covariates)

}

##########################################################################################
## get genetic knowledge information: get covariates associated with genetic knowledge  ##
##########################################################################################


get.genetic.knowledge.covariates <- function(who.knows.genetic.information){
  ##########################################################################
  ## Additional covariates if we look at who know genetic testing results ##
  ##########################################################################

  if(who.knows.genetic.information=="rater"){
    gene.know <- "gene.indep.rater"
  } else if(who.knows.genetic.information=="site-investigator"){
    gene.know <- "gene.site.investigator"
  } else if(who.knows.genetic.information=="rater-subject"){
    gene.know <- c("gene.indep.rater",
                   "gene.study.subj")
  } else if(who.knows.genetic.information=="none"){
    gene.know <- NULL
  } else {
    gene.know <- c("gene.indep.rater",
                   "gene.site.investigator")
  }

  if(!is.null(gene.know)){
    gene.use <- as.vector(outer(gene.know,paste,sep="."))
  } else {
    gene.use <- NULL
  }
  return(gene.use)
}
##############################################################################################################################
## helper.to.get.nonfunctional.covariate.values.for.prediction : function to determine where to evaluate the nonfunctional covariates
## This is a helper function to get.nonfunctional.covariate.values.for.prediction  											##
##############################################################################################################################
helper.to.get.nonfunctional.covariate.values.for.prediction <- function(covariate.name,
                                                                        initial.covariate.value,
                                                                        functional.beta.intercept,
                                                                        paper.type,
                                                                        analysis.to.run,
                                                                        nonfunctional.covariate.value.is.0){

  if("gender" %in% covariate.name){
    ########################
    ##  values for gender ##
    ########################
    nonfunctional.covariate.value <- c(1,0)
    is.nonfunctional.covariate.discrete <- FALSE ##TRUE
  } else if(grepl("educ_cat",covariate.name)){
    ####################################
    ## values for education indicator ##
    ####################################

    nonfunctional.covariate.value <- c(1,0)
    is.nonfunctional.covariate.discrete <- FALSE ##TRUE
  } else if(grepl("gene.",covariate.name)){
    ###################################
    ## who knows genetic information ##
    ###################################
    nonfunctional.covariate.value <- c(0,1)
    is.nonfunctional.covariate.discrete <- FALSE #TRUE
  } else if(grepl("score",covariate.name)){
    ##########################
    ## value for exam score ##
    ##########################
    ## return median of xval
    ##return(median(xval))
    nonfunctional.covariate.value <- 10
    is.nonfunctional.covariate.discrete <- FALSE
  } else if(grepl("base_age",covariate.name)){
    #####################
    ## age at baseline ##
    #####################
    ## return median of base_age
    ##return(median(xval))
    if(analysis.to.run=="7d" | analysis.to.run=="8d"){
      nonfunctional.covariate.value <- c(40,50)  ## used to compare baseline age
    } else if(analysis.to.run=="8c"){
      nonfunctional.covariate.value <- c(40,45,50)
    } else if(analysis.to.run=="8e"){
      nonfunctional.covariate.value <- c(40,46,50)
    } else {
      nonfunctional.covariate.value <- c(40)  ## 5/30/2018: added to properly adjust for sample size calculations
    }

    if(paper.type=="statistics" & nonfunctional.covariate.value.is.0==TRUE){
      nonfunctional.covariate.value <- 0
    }
    is.nonfunctional.covariate.discrete <- FALSE
  }
  list(nonfunctional.covariate.value=nonfunctional.covariate.value,
       is.nonfunctional.covariate.discrete=is.nonfunctional.covariate.discrete)
}

##############################################################################################################################
## get.nonfunctional.covariate.values.for.prediction : function to determine where to evaluate the nonfunctional covariates ##
##############################################################################################################################

#' @import utils
get.nonfunctional.covariate.values.for.prediction <- function(nonfunctional.covariate.names,
                                                              functional.beta.intercept,
                                                              analysis.to.run,
                                                              nonfunctional.covariate.value.is.0,
                                                              paper.type){

  output.list <- list()
  is.nonfunctional.covariate.discrete <- NULL

  for(ll in 1:length(nonfunctional.covariate.names)){
    output.list.tmp <- helper.to.get.nonfunctional.covariate.values.for.prediction(nonfunctional.covariate.names[ll],
                                                                                   initial.covariate.value=NULL,
                                                                                   functional.beta.intercept,
                                                                                   paper.type,
                                                                                   analysis.to.run,
                                                                                   nonfunctional.covariate.value.is.0)


    nonfunctional.covariate.value <- output.list.tmp$nonfunctional.covariate.value


    ## is z discrete?
    is.nonfunctional.covariate.discrete <- c(is.nonfunctional.covariate.discrete,
                                             output.list.tmp$is.nonfunctional.covariate.discrete)

    ## generate the values to evaluate the nonfunctional covariate at
    if(is.list(output.list.tmp$nonfunctional.covariate.value)){
      output.list <- appendList(output.list,nonfunctional.covariate.value)
    } else{
      tmp <- get.empty.list(nonfunctional.covariate.names[ll])
      tmp[[1]] <- nonfunctional.covariate.value
      output.list <- appendList(output.list,tmp)
    }
  }
  nonfunctional.covariate.values.for.prediction <- expand.grid(output.list)

  #############################################
  ## labels of different predictions  ##
  #############################################
  if(analysis.to.run==1 | analysis.to.run==2 | analysis.to.run==3 | analysis.to.run==4 | analysis.to.run==5 | analysis.to.run==6|
     analysis.to.run==7 | analysis.to.run==8){
    ## Titles come from looking at results of "Table of nonfunctional.covariate.values.for.prediction.per.studyuations" (above).
    z.label.names <- c("HighEd, Female", "No HighEd, Female", "HighEd, Male", "No HighEd, Male")

    if(sum(nonfunctional.covariate.names=="gender")==length(nonfunctional.covariate.names)){
      z.label.names <- c("Female","Male")
    }
  } else if(analysis.to.run=="1a" | analysis.to.run=="6a" | analysis.to.run=="7a" |
            analysis.to.run=="8a"){
    z.label.names <- c("Female","Male")

  } else if(analysis.to.run=="1b" | analysis.to.run=="6b" | analysis.to.run=="7b" |
            analysis.to.run=="8b"){
    z.label.names <- c("HighEd","No HighEd")
  } else if(analysis.to.run=="7d" | analysis.to.run=="8d"){
    z.label.names <- c("40 years at baseline","50 years at baseline")
  } else if(analysis.to.run=="8c"){
    z.label.names <- c("Agebase40","Agebase45","Agebase50")
  } else if(analysis.to.run=="8e"){
    z.label.names <- c("Agebase40","Agebase46","Agebase50")
  } else if(analysis.to.run==9 | analysis.to.run==10){
    z.label.names <- c("Status Unknown", "Status Known")
  } else {
    z.label.names <- LETTERS[1:nrow(nonfunctional.covariate.values.for.prediction)]
  }

  rownames(nonfunctional.covariate.values.for.prediction) <- z.label.names
  return(list(nonfunctional.covariate.values.for.prediction=nonfunctional.covariate.values.for.prediction,
              is.nonfunctional.covariate.discrete=is.nonfunctional.covariate.discrete))

}



#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+
#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+
##
##
## Functions used to setup data for analysis
##
##
#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+
#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+

#################################################################################################################
## get.number.of.clinical.events.per.person.per.study: extract number of clinical events per person per study  ##
#################################################################################################################

get.number.of.clinical.events.per.person.per.study <- function(study.names,number.of.studies,
                                                               n,maximum.sample.size,
                                                               number.of.clinical.events){

  ## m: number of events in each study for each subject
  m <- array(0,dim=c(number.of.studies,maximum.sample.size),
             dimnames=list(
               study=study.names,
               subject=paste("n",1:maximum.sample.size,sep="")))

  for(ss in 1:number.of.studies){
    m[ss,1:n[ss]] <- rep(number.of.clinical.events,n[ss])
  }
  return(m)
}

## extract maximum number of clinical events per person per study

get.maximum.number.of.clinical.events.per.person.per.study <- function(number.of.studies,
                                                                       number.of.clinical.events,n){
  m <- get.number.of.clinical.events.per.person.per.study
  maximum.number.of.events <- max(m)
  return(maximum.number.of.events)

}




#######################################
## Function to make parameter labels ##
#######################################

get.parameter.label <- function(maximum.number.of.functional.beta.coefficients,
                                functional.beta.intercept,
                                event.coefficients,
                                study.coefficients){

  ## label for "beta" parameters (these include functional.beta.coefficients, gamma_i,omega_s, beta_is)
  parameter.label <- NULL

  ## if we have functional.beta.coefficients
  if(!is.null(functional.beta.intercept)){
    parameter.label <- c(parameter.label,"beta0")
  }

  ## for remaining beta_is terms
  parameter.label <- c(parameter.label,paste("beta",1:maximum.number.of.functional.beta.coefficients,sep=""))


  ## if we have gamma_i terms
  if(!is.null(event.coefficients)){
    parameter.label <- c(parameter.label,"gamma")
  }

  ## if we have omega_s terms
  if(!is.null(study.coefficients)){
    parameter.label <- c(parameter.label,"omega")
  }

  return(parameter.label)
}

##############################################
## get.default.values : values to run JPRAT ##
##############################################

get.default.values <- function(use_real_data){ #, number.of.simulations){
  ## get other parameter default values
  if(use_real_data==FALSE){
    default.values <- default.simulation.setting()
  } else {
    #default.values <- default.real.data.setting(number.of.simulations)
    default.values <- default.options.for.data.setting()
  }
  return(default.values)
}

####################################################
## get.sample.size : get sample size for the data ##
####################################################

get.sample.size.for.jprat <- function(data.sets.as.list,
                                      number.of.studies,
                                      use_real_data,
                                      default.values
){

  if(!is.null(data.sets.as.list)){
    ## we ensure the sample size matches the data sets given
    n <- rep(0,number.of.studies)
    for(ss in 1:number.of.studies){
      n[ss] <- nrow(data.sets.as.list[[ss]])
    }
  }

  if(use_real_data==FALSE){
    n <- default.values$n
  }
  return(n)
}


################################################
## spline information for alpha(x,t) estimation ##
################################################

get.spline.information <- function(use_real_data,study.names,default.values,
                                   event.outcome.names,number.of.clinical.events,
                                   number.of.studies){

  if(use_real_data==TRUE){
    functional.alpha.form <-  get.empty.list(study.names)
    for(ss in 1:number.of.studies){
      ## Dummy variable set. This will not be used since we don't generate data.
      functional.alpha.form[[ss]] <- rep("real",length(event.outcome.names))
    }
  } else {
    functional.alpha.form <- default.values$functional.alpha.form
  }

  ## number of knots
  get.knot <- function(functional.alpha.form){
    if(functional.alpha.form=="wah4" | functional.alpha.form=="wah3" | functional.alpha.form=="real"){
      a <- 8
    } else {
      a <- 5
    }
    return(a)
  }

  ## knot length
  knot.length <- NULL
  for(k in 1:number.of.clinical.events){
    knot.length <- c(knot.length,get.knot(functional.alpha.form[k]))
  }

  ## dim of alpha(x,t) in x-dimension
  number.of.functional.alpha <- 1

  return(list(functional.alpha.form=functional.alpha.form,
              knot.length=knot.length,
              number.of.functional.alpha=number.of.functional.alpha))
}

#######################################################################
## specify initial coefficient estimates (needed for simulated data) ##
#######################################################################

get.coefficient.values <- function(use.functional.beta.intercept,
                                   use.functional.event.coefficients,
                                   use.functional.study.coefficients,
                                   estimated.parameters.common.for.all.studies,
                                   default.values,
                                   number.of.clinical.events,
                                   number.of.studies){

  ################################################
  ## do we include a functional beta intercept? ##
  ################################################
  ##  - If NULL, estimation ignores parameter estimate.
  if(use.functional.beta.intercept==TRUE){
    #if(use_real_data==FALSE){
    ## we use result from simulated data setting
    #functional.beta.intercept <- default.values$functional.beta.intercept
    #} else {
    functional.beta.intercept <- 0.5  ## arbitrary non-null value
    #}
  } else {
    functional.beta.intercept <- NULL
  }

  #################################################
  ## do we include event-specific coefficients?  ##
  #################################################
  ##  - If NULL, estimation ignores parameter estimate.
  if(use.functional.event.coefficients==TRUE){
    #if(use_real_data==FALSE){
    ## we use result from simulated data setting
    #event.coefficients <- default.values$event.coefficients
    #} else {
    event.coefficients <- rep(0,number.of.clinical.events)
    #}
  } else {
    event.coefficients <- NULL
  }

  #################################################
  ## do we include study-specific coefficients?  ##
  #################################################
  ##  - If NULL, estimation ignores parameter estimate.
  if(use.functional.study.coefficients==TRUE){
    #if(use_real_data==FALSE){
    ## we use result from simulated data setting
    #study.coefficients <- default.values$study.coefficients
    #} else {
    study.coefficients <- rep(0,number.of.studies)
    #}
  } else {
    study.coefficients <- NULL
  }

  ## if we assume common parameters across all studies,
  ## we cannot have study-specific coefficients
  if(estimated.parameters.common.for.all.studies==TRUE){
    study.coefficients <- NULL
  }

  return(list(
    functional.beta.intercept=functional.beta.intercept,
    event.coefficients=event.coefficients,
    study.coefficients=study.coefficients))
}



####################################
## get length of beta functionals ##
####################################

get.length.beta.functionals <- function(use_real_data,nonfunctional.covariate.names,default.values){
  if(use_real_data==FALSE){

    maximum.number.of.functional.beta.coefficients <- default.values$maximum.number.of.functional.beta.coefficients
  } else {
    maximum.number.of.functional.beta.coefficients <- length(nonfunctional.covariate.names)

  }
  return(maximum.number.of.functional.beta.coefficients)
}

#################################################################################################
## get.functional.covariate.values.for.prediction: determine where to evaluate x in alpha(x,t) ##
#################################################################################################

get.functional.covariate.values.for.prediction <- function(use_real_data,
                                                           xmin,xmax,functional.covariate.values.of.interest){

  if(use_real_data==FALSE){
    functional.covariate.values.for.prediction <- seq(0,1,by=0.01)
  } else {
    functional.covariate.values.for.prediction.tmp <- seq(0,1,by=0.1)
    functional.covariate.values.for.prediction <- c(functional.covariate.values.for.prediction.tmp,
                                                    reverse.cag(functional.covariate.values.of.interest,xmin=xmin,xmax=xmax))
    functional.covariate.values.for.prediction <- unique(sort(functional.covariate.values.for.prediction))
  }

  return(functional.covariate.values.for.prediction)
}

#############################################################
## Create xvalues to evaluate alpha(x,t) at for each study ##
#############################################################

get.functional.covariate.values.for.prediction.per.study <- function(study.names,
                                                                     functional.covariate.values.of.interest){

  if(!is.list(functional.covariate.values.of.interest)){
    ## we will repeat the value for each study
    functional.covariate.values.of.interest.per.study <- get.empty.list(study.names)
    for(ss in 1:length(study.names)){
      functional.covariate.values.of.interest.per.study[[ss]] <- functional.covariate.values.of.interest ## 42, 44, 46
    }
  } else {
    functional.covariate.values.of.interest.per.study <- functional.covariate.values.of.interest
  }
  return(functional.covariate.values.of.interest.per.study)
}

###############################################################
## create label for functional.covariate.values.of.interest. ##
###############################################################

get.functional.covariate.values.for.prediction.label <-
  function(functional.covariate.values.for.prediction,
           use_real_data,
           xmin,xmax){
    if(use_real_data==FALSE){
      out <- paste0("xx",functional.covariate.values.for.prediction,sep="")
    } else {
      out <- paste0("xx",convert.cag(functional.covariate.values.for.prediction,xmin,xmax))
    }
    return(out)
  }




######################################################################################################
## get.nonfunctional.covariate.values.for.prediction.per.study : where to evaluate Z for each study ##
######################################################################################################

get.nonfunctional.covariate.values.for.prediction.per.study <- function(study.names,number.of.studies,
                                                                        event.outcome.names,number.of.clinical.events,
                                                                        nonfunctional.covariate.values.for.prediction){

  number.of.z.predictions <- nrow(nonfunctional.covariate.values.for.prediction)
  number.of.z.covariates <- ncol(nonfunctional.covariate.values.for.prediction)

  ## nonfunctional.covariate.values.for.prediction.per.study: where to evaluate z's at in each study
  nonfunctional.covariate.values.for.prediction.per.study <-
    array(0,dim=c(number.of.studies,number.of.z.predictions,
                  number.of.clinical.events,
                  number.of.z.covariates),
          dimnames=list(
            study=study.names,
            zz=paste("zz",1:number.of.z.predictions,sep=""),
            event=event.outcome.names,
            param=paste("lb",1:number.of.z.covariates,
                        sep="")))

  for(ss in 1:(number.of.studies)){
    for(ee in 1:number.of.clinical.events){
      nonfunctional.covariate.values.for.prediction.per.study[ss,,ee,] <- as.matrix(nonfunctional.covariate.values.for.prediction)
    }
  }

  return(list(nonfunctional.covariate.values.for.prediction.per.study=nonfunctional.covariate.values.for.prediction.per.study,
              number.of.z.predictions =number.of.z.predictions))
}

#####################################################################
## create label for nonfunctional.covariate.values.for.prediction. ##
#####################################################################

get.nonfunctional.covariate.values.for.prediction.label <- function(nonfunctional.covariate.values.for.prediction){
  out <- rownames(nonfunctional.covariate.values.for.prediction)
  return(out)
}

############################################################
## function used to label z-covariates in outputted files ##
############################################################

get.zvalues.file.labels <- function(nonfunctional.covariate.values.for.prediction){
  out <- 1:nrow(nonfunctional.covariate.values.for.prediction)
}

#############################################################################################
## get time points for prediction: either inputted from user or default simulation setting ##
#############################################################################################

get.time.points.for.prediction <- function(time.points.for.prediction,
                                           time.points.for.conditional.prediction,
                                           time.points.for.conditional.prediction.toadd){


  #	if(!is.null(time.points.for.conditional.prediction) &
  #		!is.null(time.points.for.conditional.prediction.toadd)){
  #		## add t + t0 for every combination
  #		tmp <- c(outer(time.points.for.conditional.prediction,
  #					time.points.for.conditional.prediction.toadd, `+`))
  #
  #
  #		time.points.for.prediction <- c(time.points.for.prediction,tmp)
  #		time.points.for.prediction <- sort(unique((time.points.for.prediction)))
  #	}


  return(time.points.for.prediction)
}

###################################################
## check if we need to compute study differences ##
###################################################

run.study.differences <- function(check.study.equality,
                                  estimated.parameters.common.for.all.studies,
                                  estimation.default.values,
                                  number.of.studies
){

  compute.study.differences <- FALSE
  if(estimated.parameters.common.for.all.studies==FALSE & check.study.equality==TRUE & number.of.studies>1){
    ## only valid to compare differences when we estimate different parametesr for study,
    ## and number.of.studies > 1
    compute.study.differences <- TRUE

    ## To create Figure 1 in Biostatistics paper, we don't need to check study differences
    if(estimation.default.values$plot.kaplan.meier.curves==TRUE){  ## no need to compare study results here
      compute.study.differences <- FALSE
    }
  }
  return(compute.study.differences)
}


###############################################################
## function to get combination of studies to run comparisons ##
###############################################################

get.study.comparison.information <- function(study.names){
  num_study <- length(study.names)
  if(num_study > 1){
    ## used to get all combinations to compare equality of studies.
    number.of.study.comparisons <- factorial(num_study) /
      (factorial(num_study-2)*factorial(2))
    all.study.comparisons <- combn(study.names,2)
    name.of.study.comparisons <- apply(all.study.comparisons,2,
                                       function(x) paste(x,collapse="-"))
  } else {
    ## no comparisons between studies made if num_study=1
    number.of.study.comparisons <- NULL
    all.study.comparisons <- NULL
    name.of.study.comparisons <- NULL
  }
  list(number.of.study.comparisons=number.of.study.comparisons,
       all.study.comparisons=all.study.comparisons,
       name.of.study.comparisons=name.of.study.comparisons)
}

########################################
########################################
##
##  Functions to create NULL arrays for storage
##
##
####################################################

appendList <- function (x, val){
  stopifnot(is.list(x), is.list(val))
  xnames <- names(x)
  for (v in names(val)) {
    x[[v]] <- if (v %in% xnames && is.list(x[[v]]) && is.list(val[[v]]))
      appendList(x[[v]], val[[v]])
    else c(x[[v]], val[[v]])
  }
  x
}


#############################################
# dependent function for all_null_theta     #
#############################################

get.null.theta <- function(theta.names,
                           study.names,
                           event.names,
                           z_lab_names,
                           x_lab_names,
                           time_val,param.label,
                           time_choice.predicted,
                           time_choice.predicted.toadd,la){

  null.theta <- get.empty.list(theta.names)   ## beta, alphas, Ft

  # 10:1


  ## empty storage for beta 0 and beta 1 at time_val
  null.theta[["beta"]] <- array(0,dim=c(length(study.names),
                                        length(event.names),
                                        length(time_val),
                                        length(param.label)),
                                dimnames=list(
                                  study=study.names,
                                  event=event.names,
                                  time=paste("t",time_val,sep=""),
                                  theta=param.label))


  #, , time = t100, theta = beta1

  #event
  #study     hdage_nobase
  #cohort             0
  #predict            0
  #pharos             0





  # 10:1
  null.theta[["alphas"]] <- array(0,dim=c(length(study.names),
                                          length(event.names),length(x_lab_names),
                                          length(time_val),la),
                                  dimnames=list(
                                    study=study.names,
                                    event=event.names,
                                    xx=x_lab_names,
                                    time=paste("t",time_val,sep=""),
                                    theta=paste("alpha",1:la,sep="")))

  #, , xx = xx36, time = t40, theta = alpha1

  #event
  #study     hdage_nobase
  #cohort             0
  #predict            0
  #pharos             0




  #10:1
  null.theta[["Ft"]] <- array(0,dim=c(length(study.names),
                                      length(event.names),length(z_lab_names),
                                      length(x_lab_names),
                                      length(time_val)),
                              dimnames=list(
                                study=study.names,
                                event=event.names,
                                zz=z_lab_names,
                                xx=x_lab_names,
                                time=paste("t",time_val,sep="")))


  # z=A, B, C, or D at each time_val
  #, , zz = C, xx = xx43, time = t100

  #event
  #study     hdage_nobase
  #cohort             0
  #predict            0
  #pharos             0



  if(!is.null(time_choice.predicted)){ ## false
    #10:1

    ## error message
    null.theta[["Ft.predicted"]] <- array(0,dim=c(length(study.names),
                                                  length(event.names),
                                                  length(z_lab_names),
                                                  length(x_lab_names),
                                                  length(time_choice.predicted),
                                                  length(time_choice.predicted.toadd)),  ##3  1  4 18  0  0  --->  has to be 3, 1, 4, 18 1, 1

                                          dimnames=list(
                                            study=study.names,
                                            event=event.names,
                                            zz=z_lab_names,
                                            xx=x_lab_names,
                                            time=paste("t",time_choice.predicted,sep=""),
                                            time0=paste("t0",time_choice.predicted.toadd,sep=""))) ##length of 'dimnames' [5] not equal to array extent
  }
  return(null.theta)
}

#######################################################################
## add dimension to null array: dependent function for all_null_theta #
#######################################################################

add.dimension.null <- function(null.theta,location,label.dim,label.name){
  null.theta0 <- null.theta
  for(u in 1:length(null.theta)){  ## 1:3
    if(!is.null(null.theta0[[u]])){
      if(location=="first"){
        new.names <- list(iters=list(label.name),dimnames(null.theta0[[u]]))

        #10:1
        null.theta[[u]] <- array(0,dim=c(label.dim,dim(null.theta0[[u]])),
                                 dimnames=unlist(new.names,recursive=FALSE))


        # , , event = hdage_nobase, time = t40, theta = beta0

        #study
        #  iters   cohort predict pharos
        #  iter1      0       0      0

      } else if(location=="last"){
        new.names <- list(dimnames(null.theta0[[u]]),val=list(label.name))
        #10:1
        null.theta[[u]] <- array(0,dim=c(dim(null.theta[[u]]),label.dim),
                                 dimnames=unlist(new.names,recursive=FALSE))

      }
      #, , time = t40, theta = beta0, val = iter1

      #event
      #study     hdage_nobase
      #cohort             0
      #predict            0
      #pharos             0

    }
  }
  return(null.theta)
}


#####################################################
#####################################################
##
##
## Functions to compute the true parameter estimates
##
##
#####################################################
#####################################################

## dim.fun is the dimension of the needed function

apply.function.index <- function(x,dim.fun,fun,...){
  xorig <- x

  ## x must be in dimension dim.fun order
  index.fun.needed <- match(dim.fun,names(dimnames(x)))  #a vector of the position of first occurrence of the vector1 in vector2
  index.apply.used <- find.apply.index(x,dim.fun)
  if(all.equal(sort(index.fun.needed),index.fun.needed)!=TRUE){
    x <- aperm(x,c(names(dimnames(x))[c(index.fun.needed,index.apply.used)]))  # transposes an array by permuting its dimensions and optionally resizing it.
  }

  #	the subscript permutation vector, usually a permutation of the integers 1:n, where n is the number of dimensions of a. When a has named dimnames, it can be a character vector of length n giving a permutation of those names. The default (used whenever perm has zero length) is to reverse the order of the dimensions.

  ## set up x to apply funciton
  out <- apply.index(x,dim.fun,fun,...)

  ## re-arrange to original array dimension
  out <- array(out,dim=dim(x))
  dimnames(out) <- dimnames(x)

  out <- aperm(out,names(dimnames(xorig)))

  return(out)
}



##  find index to apply function over

find.apply.index <- function(x,names.use){
  out <- which(names(dimnames(x)) %in% names.use)
  out <- setdiff(1:length(dimnames(x)),out)   # Calculates the (nonsymmetric) set difference of subsets of a probability space: find differences from two sets
  return(out)
}




apply.index <- function(x,names.use,fun,...){
  out <- apply(x,find.apply.index(x,names.use),fun,...)   # at the differences
  return(out)
}


## function to produce true estimates (for simulation study)
#' @importFrom abind adrop
#' @import cubature
#' @import stats
get.truth <- function(combi.study,combi.choice,
                      real_data,num_study,np,lb,num_time,
                      beta0int,beta0,
                      gamma.param,  ## null
                      omega.param,  ## null
                      time_val,
                      time_choice.predicted, ## NULL
                      time_choice.predicted.toadd, ## NULL
                      num_xx,
                      a0, ## NULL
                      axmod, #"real" for all studies
                      la,
                      xks,
                      zeval, # k-means
                      z.choice, # 4
                      sigmar,sigmau, ## ??
                      gtmod, ## null
                      use.random.effects, ## null
                      null.theta,
                      combi.null.theta){


  ## storage for each estimate theta.names
  beta.true <- null.theta$null.theta$beta  ## , , time = t100, theta = beta1
  alphas.true <- null.theta$null.theta$alpha  ## , , xx = xx50, time = t100, theta = alpha1
  Ft.true <- null.theta$null.theta$Ft  ## , , zz = D, xx = xx50, time = t100
  Ft.predicted.true <- null.theta$null.theta$Ft.predicted  ## null

  if(num_study > 1){
    ## study differences considered only if num_study > 1
    beta.diff <- combi.null.theta$null.theta$beta
    alphas.diff <- combi.null.theta$null.theta$alpha
    Ft.diff <- combi.null.theta$null.theta$Ft
  } else {
    beta.diff <- NULL
    alphas.diff <- NULL
    Ft.diff <- NULL
  }

  if(real_data==FALSE){
    ## set up beta0,gamma, omega terms
    for(tt in 1:num_time){
      ## for intercept term
      if(!is.null(beta0int)){
        beta.true[,,tt,"beta0"] <- beta.true.value(gtmod,beta0int,time_val[tt])  ## beta0int*gt(gtmod, time_val[[tt]]) where is this function from: main.R
      }

      ## gamma_i term
      if(!is.null(gamma.param)){
        for(ii in 1:np){
          beta.true[,ii,tt,"gamma"] <- beta.true.value(gtmod,gamma.param[ii],time_val[tt])
        }
      }

      if(!is.null(omega.param)){
        ## omega_s term
        for(ss in 1:num_study){
          beta.true[ss,,tt,"omega"] <- beta.true.value(gtmod,omega.param[ss],time_val[tt])
        }
      }
    }

    ## set up other terms
    for(ss in 1:num_study){
      for(ii in 1:np){
        for(tt in 1:num_time){

          ## set up other beta terms
          beta.index <- 1:lb[[ss]][[ii]]

          beta.true[ss,ii,tt,paste("beta",beta.index,sep="")] <- beta.true.value(gtmod,
                                                                                 beta0[[ss]][[ii]],time_val[tt])

          for(xx in 1:num_xx){
            ## set up alpha terms
            alphas.true[ss,ii,xx,tt,] <- alpha.true(a0[[ss]][ii],
                                                    axmod[[ss]][ii],
                                                    xks[[ss]][xx],time_val[tt],gtmod)

            ## set up Ft terms
            for(zz in 1:z.choice){
              zeval.tmp <- as.matrix(zeval[ss,zz,ii,])
              ## z^T \beta
              betaz.tmp <- beta.true[ss,ii,tt,paste("beta",beta.index,sep="")] %*% zeval.tmp

              ## \beta_0 + z^T\beta
              if(!is.null(beta0int)){
                betaz.tmp <- betaz.tmp + beta.true[1,1,tt,"beta0"] ## intercept
              }

              ## gamma + \beta_0 +z^T\beta
              if(!is.null(gamma.param)){
                betaz.tmp <- betaz.tmp + beta.true[ss,ii,tt,"gamma"] ## gamma_i term
              }

              ## omega + gamma + \beta_0 +z^T\beta
              if(!is.null(omega.param)){
                betaz.tmp <- betaz.tmp + beta.true[ss,ii,tt,"omega"] ## omega_s term
              }

              ## integrate over random effects
              if(use.random.effects==FALSE){ ## no random effects in model
                Ft.true[ss,ii,zz,xx,tt] <- Ft.true.value(betaz=betaz.tmp,
                                                         alphastmp=alphas.true[ss,ii,xx,tt,la],
                                                         rval=0)
              } else {  ## integrate over random effects
                Ft.integrate <-
                  make.Ft.integrate(alphax=alphas.true[ss,ii,xx,tt,la],
                                    betaz=betaz.tmp,sigmar,sigmau)
                sigma_all <- c(sigmar,sigmau)

                if(is.null(sigmau)){
                  Ft.true[ss,ii,zz,xx,tt] <-
                    integrate(Ft.integrate,-Inf,Inf)$value  # the final estimate of the integral.
                } else {
                  Ft.true[ss,ii,zz,xx,tt] <-
                    adaptIntegrate(Ft.integrate,
                                   lowerLimit=rep(-1,length(sigma_all)),
                                   upperLimit=rep(1,length(sigma_all)))$integral  #  adaptive multidimensional integration of  vector-valued integrands over hypercubes: the value of the integral
                }
              }
            }
          }
        }
      }
    }
  }

  ## make Ft monotone
  ## applied to function of dimension time by p=xx
  if(1==1){ ## for testing,  ### ulc: what this condition mean?
    mono.Ftest <- apply.function.index(Ft.true,
                                       dim.fun=c("time","xx"),getF,
                                       p=length(dimnames(Ft.true)[["xx"]]))  ## monotone Function will get from getF
    Ft.true <- mono.Ftest
  } else {
    mono.Ftest <- Ft.true
  }
  ## end check for testing





  ## get predicted values: Pr(T<t|t>t0)
  if(!is.null(time_choice.predicted)){

    ## extract Ft information
    for(tt in 1:length(time_choice.predicted)){
      for(tt0 in 1:length(time_choice.predicted.toadd)){
        time.use <- paste("t",time_choice.predicted[tt]+
                            time_choice.predicted.toadd[tt0],sep="")
        if(time.use %in% dimnames(mono.Ftest)$time){
          Ft.predicted.true[,,,,tt,tt0] <-
            (
              abind::adrop(mono.Ftest[,,,,time.use,drop=FALSE],drop=c(5)) -
                abind::adrop(mono.Ftest[,,,,paste("t",time_choice.predicted[tt],sep=""),drop=FALSE],
                      drop=c(5)))/
            (1-abind::adrop(mono.Ftest[,,,,paste("t",time_choice.predicted[tt],sep=""),drop=FALSE],		drop=c(5)))
        }
      }
    }
  }


  if(num_study > 1){
    ## check differences between studies
    mydiff <- function(x){
      x[combi.choice[1,]]-x[combi.choice[2,]]
    }

    ## difference function to compare studies if num_study>1
    beta.diff[1:combi.study,,,] <- apply.index(beta.true,"study",mydiff)
    alphas.diff[1:combi.study,,,,] <- apply.index(alphas.true,"study",mydiff)
    Ft.diff[1:combi.study,,,,] <-  apply.index(Ft.true,"study",mydiff)
  }

  alphas.true.mean <- apply.index(alphas.true,"time",mean,drop=FALSE)

  list(beta.true=beta.true,
       alphas.true=alphas.true,
       alphas.true.mean=alphas.true.mean,
       Ft.true=Ft.true,
       Ft.predicted.true=Ft.predicted.true,
       beta.diff=beta.diff,alphas.diff=alphas.diff,
       Ft.diff=Ft.diff)
}



## make Ft monotonic
#' @import zoo
getF <- function(Ft,p){
  n <- dim(Ft)[1]
  F <- Ft
  ################################################
  ## Replace NA with last value carried forward ##
  ## - This assumes that Ft should be increasing from the lastest well-observe F(t) value
  ## - The assumption is similar to a Kaplan-Meier curve for the endpoints
  F <- zoo::na.locf(F,na.rm=FALSE)  ## don't remove leading NA's.
  if(any(is.na(Ft))){
    ## we don't run monotonicity
    return(Ft)
  } else {
    for(i in 1:p){
      #print(i)
      tmp <- min(F[2:n,i]-F[1:(n-1),i])
      k <- match(tmp,F[2:n,i]-F[1:(n-1),i]) #returns a vector of the position of first occurrence of the vector1 in vector2
      while(tmp<0){
        #print(tmp)
        F[k,i]=(F[k+1,i]+F[k,i])/2
        F[k+1,i]=F[k,i]
        tmp <- min(F[2:n,i]-F[1:(n-1),i])
        k <- match(tmp,(F[2:n,i]-F[1:(n-1),i]))
      }
    }
    return(F)
  }
}

############################
##
## Functions to create data
##
##
#############################


## function to generate fixed effects


#' @import stats
generate.fixed.effects <- function(num_study, # 3
                                   nmax,   # 915
                                   np,lb.max,n,lb,
                                   fzrform,par1_fzr,par2_fzr,type_fzr, ## null
                                   fxform,par1_fx,par2_fx,type_fx,     ## null
                                   real_data,
                                   x_tmp.list,
                                   z_tmp.list){

  z <- array(0,dim=c(num_study,nmax,np,lb.max),   # n1~ n915
             dimnames=list(
               paste("ss",1:num_study,sep=""),
               paste("n",1:nmax,sep=""),
               paste("np",1:np,sep=""),
               paste("lb",1:lb.max,sep="")))

  x <- array(0,dim=c(num_study,nmax),   # n1~ n915
             dimnames=list(
               paste("ss",1:num_study,sep=""),
               paste("n",1:nmax,sep="")
             ))
  if(real_data==FALSE){
    for(ss in 1:num_study){
      ##########################################
      ## set up functions for generating data ##
      ##########################################

      ## Set distribution f_Z
      if(fzrform=="norm"){
        ## Z|R ~Normal
        fzr <- make.normal(mean=par1_fzr[ss],sd=par2_fzr[ss],type=type_fzr)
      } else if(fzrform=="unif"){
        fzr <- make.uniform(min=par1_fzr[ss],max=par2_fzr[ss],type=type_fzr)
      } else if(fzrform=="binom"){
        fzr <- make.binom(size=par1_fzr[ss],prob=par2_fzr[ss],type=type_fzr)
      }



      ## Set distribution f_X
      if(fxform=="norm"){
        ## Z|R ~Normal
        fx <- make.normal(mean=par1_fx[ss],sd=par2_fx[ss],type=type_fx)
      } else if(fxform=="unif"){
        fx <- make.uniform(min=par1_fx[ss],max=par2_fx[ss],type=type_fx)
      }

    }

    for(ss in 1:num_study){
      for(i in 1:n[ss]){
        ##print(i)
        ## generate deviate for z-covariates
        for(k in 1:np){
          z[ss,i,k,1:lb[[ss]][[k]]] <- fzr(lb[[ss]][[k]],par1_fzr[ss],par2_fzr[ss]) # fzr is function?
        }

        ## generate deviate for x-covariates
        x[ss,i] <- fx(1,par1_fx[ss],par2_fx[ss])
      }
    }
  } else{
    ## real data
    for(ss in 1:num_study){
      for(k in 1:np){
        z[ss,1:n[ss],k,1:lb[[ss]][[k]]] <- z_tmp.list[[ss]][1:n[ss],k,1:1:lb[[ss]][[k]]]
      }
      x[ss,1:n[ss]] <- x_tmp.list[[ss]]   ## n1 ~ n[[ss]]
    }
  }
  list(x=x,z=z)
}



####################################
## Function to get simulated data ##
####################################


## keeping x,z as fixed effects
#' @importFrom abind adrop
simu.data.fixed.effects <- function(x,z,
                                    delta_tmp.list,
                                    s_tmp.list,
                                    a0,axmod,num_time,censorrate,
                                    frform,type_fr,
                                    par1_fr,par2_fr,par1_fr2,par2_fr2,mix_n,
                                    par_fu,
                                    real_data,time_val,
                                    beta0int,beta,gamma.param,omega.param,
                                    n,nmax,m,maxm,la,lb,num_study,np,gtmod,use.random.effects,
                                    gen.cens.depend.z){

  tol <- 1e-6

  ##########################
  ## set values for output ##
  ############################
  y <- array(0,dim=c(num_study,num_time,nmax,maxm),
             dimnames=list(
               paste("ss",1:num_study,sep=""),
               paste("t",time_val,sep=""),
               paste("n",1:nmax,sep=""),
               paste("m",1:maxm,sep="")))  ## 3 by time points by 915

  ymiss_ind<- y
  ytest <- y   ## for testing pseudo-values

  x.tmp <- array(0,dim=c(num_study,nmax),
                 dimnames=list(
                   paste("ss",1:num_study,sep=""),
                   paste("n",1:nmax,sep="")
                 ))  ##3 by 915

  s <- array(0,dim=c(num_study,nmax,maxm),
             dimnames=list(
               paste("ss",1:num_study,sep=""),
               paste("n",1:nmax,sep=""),
               paste("m",1:maxm,sep=""))) ## 3 by 915 by 1

  delta <- s
  onset_age_orig <- s
  norisk_ind <- s

  count <- array(0,dim=c(num_study,num_time,3),
                 dimnames=list(
                   paste("ss",1:num_study,sep=""),
                   paste("t",time_val,sep=""),
                   c("one","zero","other")))

  q <- array(0,dim=c(num_study,np,nmax,maxm),
             dimnames=list(
               paste("ss",1:num_study,sep=""),
               paste("p",1:np,sep=""),
               paste("n",1:nmax,sep=""),
               paste("m",1:maxm,sep="")))   ## 3 by 1 by 915

  #######################
  ## additional values ##
  #######################
  r <- x.tmp
  bb <- x.tmp

  etas_z <- s
  etas_x <- s
  Ft <- s
  onset_age <- s
  cens <- s

  ##############################################
  ## mixture probabilities for km jack method ##
  ##############################################
  for(ss in 1:num_study){
    for(i in 1:n[ss]){
      for(j in 1:m[ss,i]){
        q[ss,j,i,j]  <- 1
      }
    }
  }

  if(real_data==FALSE){
    if(!is.null(par_fu)){
      ## u_s random effect (random effect for study)
      fs <- make.normal(mean=par_fu["mean"],sd=par_fu["sd"],type=type_fr)

      ## generate random effect
      us <- fs(num_study,par_fu["mean"],par_fu["sd"])

      ## center random effect
      us <- us - mean(us)
    } else {
      us <- rep(0,num_study)
    }

    if(use.random.effects==FALSE){ ## no random effects
      us <- rep(0,num_study)
    }

    for(ss in 1:num_study){
      ##########################################
      ## set up functions for generating data ##
      ##########################################
      ## distribution for f_R

      if(frform=="norm"){
        ## R ~ Normal(mean.fr,sd.fr^2)
        fr <- make.normal(mean=par1_fr[ss],sd=par2_fr[ss],type=type_fr)
      } else if(frform=="gamm"){
        ## R ~ Gamma
        fr <- make.gamma(shape=par1_fr[ss],scale=par2_fr[ss],type=type_fr)
      } else if(frform=="unif"){
        ## R ~ uniform
        fr <- make.uniform(min=par1_fr[ss],max=par2_fr[ss],type=type_fr)
      } else if(frform=="tdis"){
        ## R ~ t
        fr <- make.t(df=par1_fr[ss],ncp=par2_fr[ss],type=type_fr)
      } else if(frform=="mixn"){
        ## R ~ mixture of Normals
        fr <- make.mixture.normals(mean1=par1_fr[ss],sd1=par1_fr[ss],
                                   mean2=par2_fr2[ss],sd2=par2_fr2[ss],mix=mix_n[ss],type=type_fr)
      }

      ####################
      ## simulated data ##
      ####################
      bb[ss,1:n[ss]] <- rep(0,n[ss])

      ############################
      ## generate random effect ##
      ############################
      if(use.random.effects==TRUE){ ## we generate random effects
        r[ss,1:n[ss]] <- fr(n[ss],par1_fr[ss],par2_fr[ss]) + bb[ss,1:n[ss]]
      }

      ## center random effect, within each study
      #r[ss,1:n[ss]] <- r[ss,1:n[ss]] - mean(r[ss,1:n[ss]])
    }

    ## center random effect
    r <- r - mean(r)

    for(ss in 1:num_study){
      for(i in 1:n[ss]){
        ## form z_etas and x_etas
        etas_z[ss,i,1:m[ss,i]] <- get.z.etas(m[ss,i],lb[[ss]],beta0int,beta[[ss]],
                                             gamma.param,omega.param[ss],
                                             adrop(z[ss,i,,,drop=FALSE],drop=c(1,2)))


        ##> z[1:3,1:5,,]
        #n1       n2       n3       n4       n5
        #ss1 36.00 54.00000 55.00000 33.00000 33.00000
        #ss2 28.80 44.40000 51.01000 55.68000 34.87000
        #ss3 50.75 47.86264 30.27473 32.63462 43.42308

        ## drop dimesion 1 and 2 , i.e np1 by lb 1 dim of z(1,1)=36

        etas_x[ss,i,1:m[ss,i]] <- get.x.etas(m[ss,i],a0[[ss]],axmod[[ss]],
                                             x[ss,i])

        ########################
        ## generate onset age ##
        ########################
        Ft[ss,i,] <- get.unif.Ft(m[ss,i])
        onset_age[ss,i,] <- invFt(m[ss,i],
                                  adrop(etas_z[ss,i,,drop=FALSE],drop=c(1,2)),
                                  adrop(etas_x[ss,i,,drop=FALSE],drop=c(1,2)),
                                  r[ss,i]+us[ss],
                                  adrop(Ft[ss,i,,drop=FALSE],drop=c(1,2)),gtmod)

        for(j in 1:m[ss,i]){
          if(gen.cens.depend.z==FALSE){
            cens.z <- NULL
          } else {
            cens.z <- sum(z[ss,i,j,])  ## works for all, but targeting lb=1
          }
          cens[ss,i,j] <- genc(onset_age[ss,i,j],censorrate=censorrate[ss],z=cens.z)

          if(onset_age[ss,i,j] <= cens[ss,i,j]){
            s[ss,i,j] <- onset_age[ss,i,j]
            delta[ss,i,j] <- 1
          } else {
            s[ss,i,j] <- cens[ss,i,j]
            delta[ss,i,j] <- 0
          }
        }
      }
    }
  } else {
    ###############
    ## real data ##
    ###############
    for(ss in 1:num_study){
      s[ss,1:n[ss],1:max(m[ss,])] <- s_tmp.list[[ss]]
      delta[ss,1:n[ss],1:max(m[ss,])] <- delta_tmp.list[[ss]]
    }
  }

  ##################
  ## form Y terms ##
  ##################
  for(ss in 1:num_study){
    for(i in 1:n[ss]){
      for(j in 1:m[ss,i]){
        for(tt in 1:num_time){
          if(delta[ss,i,j] > tol){
            ## person not censored
            if(s[ss,i,j] <= time_val[tt]){
              y[ss,tt,i,j] <- 1
              count[ss,tt,1] <- count[ss,tt,1] + 1   ## "one"

              #ytest[ss,tt,i,j] <- 999  ## in no censoring case, pseudo-values agree with 0/1 output
              #ymiss_ind[ss,tt,i,j] <- 1

            } else {
              y[ss,tt,i,j] <- 0
              count[ss,tt,2] <- count[ss,tt,2] + 1   ## "zero"

              #ytest[ss,tt,i,j] <- 999
              #ymiss_ind[ss,tt,i,j] <- 1


            }
          } else {

            ## person is censored
            if(s[ss,i,j] >= time_val[tt]){
              # c_ij >= t_0
              y[ss,tt,i,j] <- 0
              count[ss,tt,2] <- count[ss,tt,2] + 1
            } else {
              ## value not observed
              y[ss,tt,i,j] <- 999
              ymiss_ind[ss,tt,i,j] <- 1        ## missing indicator
              count[ss,tt,3] <- count[ss,tt,3] + 1
            }
          }
        }
      }
    }
  }

  list(y_start=y,ymiss_ind_start=ymiss_ind,
       z_start=z,x_start=x,s_start=s,q_start=q,delta_start=delta,
       onset_age_orig_start=onset_age_orig,
       norisk_ind_start=norisk_ind,count=count,
       ytest=ytest)
}



## generating new x,z each time
#' @importFrom abind adrop
#'
simu.data <- function(randomeffects.covariates.dependent,
z_tmp.list,
x_tmp.list,
delta_tmp.list,
s_tmp.list,
a0,axmod,num_time,censorrate,
frform,fzrform,fxform,
type_fr,type_fzr,type_fx,
par1_fr,par2_fr,
par_fu,
par1_fr2,par2_fr2,mix_n,
par1_fx,par2_fx,
par1_fzr,par2_fzr,
real_data,time_val,
beta0int,beta,gamma.param,omega.param,
n,nmax,m,maxm,la,lb,num_study,np,gtmod,use.random.effects,
gen.cens.depend.z){

  tol <- 1e-6

  ##########################
  ## set values for output ##
  ############################
  y <- array(0,dim=c(num_study,num_time,nmax,maxm),
             dimnames=list(
               paste("ss",1:num_study,sep=""),
               paste("t",time_val,sep=""),
               paste("n",1:nmax,sep=""),
               paste("m",1:maxm,sep="")))

  ymiss_ind<- y
  ytest <- y   ## for testing pseudo-values

  z <- array(0,dim=c(num_study,nmax,np,lb.max),
             dimnames=list(
               paste("ss",1:num_study,sep=""),
               paste("n",1:nmax,sep=""),
               paste("np",1:np,sep=""),
               paste("lb",1:lb.max,sep="")))

  x.tmp <- array(0,dim=c(num_study,nmax),
                 dimnames=list(
                   paste("ss",1:num_study,sep=""),
                   paste("n",1:nmax,sep="")
                 ))

  s <- array(0,dim=c(num_study,nmax,maxm),
             dimnames=list(
               paste("ss",1:num_study,sep=""),
               paste("n",1:nmax,sep=""),
               paste("m",1:maxm,sep="")))

  delta <- s
  onset_age_orig <- s
  norisk_ind <- s

  count <- array(0,dim=c(num_study,num_time,3),
                 dimnames=list(
                   paste("ss",1:num_study,sep=""),
                   paste("t",time_val,sep=""),
                   c("one","zero","other")))

  q <- array(0,dim=c(num_study,np,nmax,maxm),
             dimnames=list(
               paste("ss",1:num_study,sep=""),
               paste("p",1:np,sep=""),
               paste("n",1:nmax,sep=""),
               paste("m",1:maxm,sep="")))

  #######################
  ## additional values ##
  #######################
  x <- x.tmp
  r <- x.tmp
  bb <- x.tmp

  etas_z <- s
  etas_x <- s
  Ft <- s
  onset_age <- s
  cens <- s

  ##############################################
  ## mixture probabilities for km jack method ##
  ##############################################
  for(ss in 1:num_study){
    for(i in 1:n[ss]){
      for(j in 1:m[ss,i]){
        q[ss,j,i,j]  <- 1
      }
    }
  }

  if(real_data==FALSE){
    if(!is.null(par_fu)){
      ## u_s random effect (random effect for study)
      fs <- make.normal(mean=par_fu["mean"],sd=par_fu["sd"],type=type_fr)

      ## generate random effect
      us <- fs(num_study,par_fu["mean"],par_fu["sd"])

      ## center random effect
      us <- us - mean(us)
    } else {
      us <- rep(0,num_study)
    }

    if(use.random.effects==FALSE){ ## no random effects
      us <- rep(0,num_study)
    }

    for(ss in 1:num_study){
      ##########################################
      ## set up functions for generating data ##
      ##########################################
      ## distribution for f_R

      if(frform=="norm"){
        ## R ~ Normal(mean.fr,sd.fr^2)
        fr <- make.normal(mean=par1_fr[ss],sd=par2_fr[ss],type=type_fr)
      } else if(frform=="gamm"){
        ## R ~ Gamma
        fr <- make.gamma(shape=par1_fr[ss],scale=par2_fr[ss],type=type_fr)
      } else if(frform=="unif"){
        ## R ~ uniform
        fr <- make.uniform(min=par1_fr[ss],max=par2_fr[ss],type=type_fr)
      } else if(frform=="tdis"){
        ## R ~ t
        fr <- make.t(df=par1_fr[ss],ncp=par2_fr[ss],type=type_fr)
      } else if(frform=="mixn"){
        ## R ~ mixture of Normals
        fr <- make.mixture.normals(mean1=par1_fr[ss],sd1=par1_fr[ss],
                                   mean2=par2_fr2[ss],sd2=par2_fr2[ss],mix=mix_n[ss],type=type_fr)
      }


      ## Set distribution f_Z
      if(fzrform=="norm"){
        ## Z|R ~Normal
        fzr <- make.normal(mean=par1_fzr[ss],sd=par2_fzr[ss],type=type_fzr)
      } else if(fzrform=="unif"){
        fzr <- make.uniform(min=par1_fzr[ss],max=par2_fzr[ss],type=type_fzr)
      } else if(fzrform=="binom"){
        fzr <- make.binom(size=par1_fzr[ss],prob=par2_fzr[ss],type=type_fzr)
      }

      ## Set distribution f_X
      if(fxform=="norm"){
        ## Z|R ~Normal
        fx <- make.normal(mean=par1_fx[ss],sd=par2_fx[ss],type=type_fx)
      } else if(fxform=="unif"){
        fx <- make.uniform(min=par1_fx[ss],max=par2_fx[ss],type=type_fx)
      }

      ####################
      ## simulated data ##
      ####################
      if(randomeffects.covariates.dependent==TRUE){
        ## add dependence
        bb[ss,1:n[ss]] <- rnorm(n[ss],sd=0.05)
      } else {
        bb[ss,1:n[ss]] <- rep(0,n[ss])
      }

      ############################
      ## generate random effect ##
      ############################
      if(use.random.effects==TRUE){ ## we generate random effects
        r[ss,1:n[ss]] <- fr(n[ss],par1_fr[ss],par2_fr[ss]) + bb[ss,1:n[ss]]
      }

      ## center random effect, within each study
      #r[ss,1:n[ss]] <- r[ss,1:n[ss]] - mean(r[ss,1:n[ss]])
    }

    ## center random effect
    r <- r - mean(r)

    for(ss in 1:num_study){
      for(i in 1:n[ss]){
        ##print(i)
        ## generate deviate for z-covariates
        for(k in 1:np){
          z[ss,i,k,1:lb[[ss]][[k]]] <- fzr(lb[[ss]][[k]],par1_fzr[ss],par2_fzr[ss]) + bb[ss,i]
        }

        ## generate deviate for x-covariates
        x[ss,i] <- fx(1,par1_fx[ss],par2_fx[ss]) + bb[ss,i]

        ## form z_etas and x_etas
        etas_z[ss,i,1:m[ss,i]] <- get.z.etas(m[ss,i],lb[[ss]],beta0int,beta[[ss]],
                                             gamma.param,omega.param[ss],
                                             z[ss,i,,])
        etas_x[ss,i,1:m[ss,i]] <- get.x.etas(m[ss,i],a0[[ss]],axmod[[ss]],
                                             x[ss,i])

        ########################
        ## generate onset age ##
        ########################
        Ft[ss,i,] <- get.unif.Ft(m[ss,i])
        onset_age[ss,i,] <- invFt(m[ss,i],etas_z[ss,i,],etas_x[ss,i,],r[ss,i]+us[ss],Ft[ss,i,],gtmod)

        for(j in 1:m[ss,i]){
          if(gen.cens.depend.z==FALSE){
            cens.z <- NULL
          } else {
            cens.z <- sum(z[ss,i,j,])  ## works for all, but targeting lb=1
          }
          cens[ss,i,j] <- genc(onset_age[ss,i,j],censorrate=censorrate[ss],z=cens.z)

          if(onset_age[ss,i,j] <= cens[ss,i,j]){
            s[ss,i,j] <- onset_age[ss,i,j]
            delta[ss,i,j] <- 1
          } else {
            s[ss,i,j] <- cens[ss,i,j]
            delta[ss,i,j] <- 0
          }
        }
      }
    }
  } else {
    ###############
    ## real data ##
    ###############
    for(ss in 1:num_study){
      for(k in 1:np){
        z[ss,1:n[ss],k,1:lb[[ss]][[k]]] <- z_tmp.list[[ss]][1:n[ss],k,1:1:lb[[ss]][[k]]]
      }
      x[ss,1:n[ss]] <- x_tmp.list[[ss]]
      s[ss,1:n[ss],1:max(m[ss,])] <- s_tmp.list[[ss]]
      delta[ss,1:n[ss],1:max(m[ss,])] <- delta_tmp.list[[ss]]
    }
  }

  ##################
  ## form Y terms ##
  ##################
  for(ss in 1:num_study){
    for(i in 1:n[ss]){
      for(j in 1:m[ss,i]){
        for(tt in 1:num_time){
          if(delta[ss,i,j] > tol){
            ## person not censored
            if(s[ss,i,j] <= time_val[tt]){
              y[ss,tt,i,j] <- 1
              count[ss,tt,1] <- count[ss,tt,1] + 1

              #ytest[ss,tt,i,j] <- 999  ## in no censoring case, pseudo-values agree with 0/1 output
              #ymiss_ind[ss,tt,i,j] <- 1

            } else {
              y[ss,tt,i,j] <- 0
              count[ss,tt,2] <- count[ss,tt,2] + 1

              #ytest[ss,tt,i,j] <- 999
              #ymiss_ind[ss,tt,i,j] <- 1


            }
          } else {

            ## person is censored
            if(s[ss,i,j] >= time_val[tt]){
              # c_ij >= t_0
              y[ss,tt,i,j] <- 0
              count[ss,tt,2] <- count[ss,tt,2] + 1
            } else {
              ## value not observed
              y[ss,tt,i,j] <- 999
              ymiss_ind[ss,tt,i,j] <- 1
              count[ss,tt,3] <- count[ss,tt,3] + 1
            }
          }
        }
      }
    }
  }

  list(y_start=y,ymiss_ind_start=ymiss_ind,
       z_start=z,x_start=x,s_start=s,q_start=q,delta_start=delta,
       onset_age_orig_start=onset_age_orig,
       norisk_ind_start=norisk_ind,count=count,
       ytest=ytest)
}



#######################################
## function to get zeval proportions ##
#######################################
convert.matrix.to.numeric <- function(x){
  dims <- dim(x)
  x <- as.numeric(x)
  dim(x) <- dims
  return(x)
}


probability.z <- function(zeval.tmp,z.discrete.info,z,
                          common.param.estimation,n,maxm,study.names,z_lab_names,num_study){

  if(any(z.discrete.info==TRUE)){
    ## at least one discrete z
    prob.out <- array(0,dim=c(num_study,maxm,nrow(zeval.tmp)),
                      dimnames=list(study=study.names,
                                    event=paste("np",1:maxm,sep=""),
                                    zz=z_lab_names))

    if(common.param.estimation==FALSE){
      for(ss in 1:num_study){
        n.use <- n[ss]
        for(nn in 1:maxm){
          for(zz in 1:nrow(zeval.tmp)){
            rows.to.compare <- convert.matrix.to.numeric(as.matrix(z[ss,1:n.use,
                                                                     nn,z.discrete.info]))
            values.to.compare <- as.numeric(zeval.tmp[zz,z.discrete.info])

            row.is.a.match <- apply(rows.to.compare,1,identical,
                                    values.to.compare)
            prob.out[ss,nn,zz] <- mean(row.is.a.match)
          }
        }
      }
    } else{
      for(nn in 1:maxm){
        for(zz in 1:nrow(zeval.tmp)){
          z.tmp.use <- array(0,dim=c(sum(n),dim(z)[c(3,4)]),
                             dimnames=list(paste("nn",1:sum(n),sep=""),
                                           dimnames(z)[[3]],
                                           dimnames(z)[[4]]))
          tmp_start <- 1
          tmp_end <- n[1]
          for(ss in 1:num_study){
            z.tmp.use[tmp_start:tmp_end,,] <- adrop(z[ss,1:n[ss],,,drop=FALSE],drop=c(1))
            tmp_start <- tmp_end + 1
            tmp_end <- tmp_start + n[ss+1] -1
          }
          n.use <- sum(n)

          for(ss in 1:num_study){
            rows.to.compare <- convert.matrix.to.numeric(as.matrix(z.tmp.use[1:n.use,
                                                                             nn,z.discrete.info]))
            values.to.compare <- as.numeric(zeval.tmp[zz,z.discrete.info])

            row.is.a.match <- apply(rows.to.compare,1,identical,
                                    values.to.compare)
            prob.out[ss,nn,zz] <- mean(row.is.a.match)
          }
        }
      }
    }

  } else{
    ## no discrete z's
    prob.out <- NULL
  }
  return(prob.out)
}


############################################
## function to get combination of studies ##
############################################

get.combi.info <- function(study.names){
  num_study <- length(study.names)
  if(num_study > 1){
    ## used to get all combinations to compare equality of studies.
    combi.study <- factorial(num_study) /
      (factorial(num_study-2)*factorial(2))
    combi.choice <- combn(study.names,2)
    combi.names <- apply(combi.choice,2,
                         function(x) paste(x,collapse="-"))
  } else {
    ## no comparisons between studies made if num_study=1
    combi.study <- NULL
    combi.choice <- NULL
    combi.names <- NULL
  }
  list(combi.study=combi.study,
       combi.choice=combi.choice,
       combi.names=combi.names)
}


#################################
## function to organize output ##
#################################

## get all flattend arrays
get.theta.output <- function(all.est.store,
                             dim.order,
                             flatten.name="time",
                             flatten.theta.name,
                             extension=".true",
                             theta.names){  ##extension is name in all.est.store
  theta.out <- get.empty.list(theta.names)

  for(u in 1:length(theta.names)){
    names.tmp <- theta.names[u]
    est.name <- paste(theta.names[u],extension,sep="")
    theta.out[[names.tmp]] <- flatten.array(
      all.est.store[[est.name]],
      dim.order=dim.order[[names.tmp]],
      flatten.name,
      theta=flatten.theta.name[[names.tmp]])
  }
  return(theta.out)
}




## separate estimates from variance estimates

extract.var.est.output <- function(out,
                                   theta.names){
  theta.out <- get.empty.list(theta.names)
  theta.var.out <- get.empty.list(theta.names)

  for(u in 1:length(theta.names)){
    theta <- theta.names[u]
    theta.index.var <- grepl("*var*",out[[theta]]$val)
    theta.index <- !theta.index.var

    ## theta est
    theta.out[[theta]] <- out[[theta]][theta.index,]
    theta.var.out[[theta]] <- out[[theta]][theta.index.var,]
  }
  list(theta.out=theta.out,theta.var.out=theta.var.out)
}



extract.output <- function(theta.out,gamma.param,omega.param,time_val){
  ## beta
  names.tmp <- "beta*"
  index.est <- grepl(names.tmp,theta.out$beta$theta)
  beta.out <- theta.out$beta[index.est,]

  ## gamma
  if(!is.null(gamma.param)){
    names.tmp <- "gamma*"
    index.est <- grepl(names.tmp,theta.out$beta$theta)
    gamma.out <- theta.out$beta[index.est,]
  } else{
    gamma.out <- 0
  }

  ###########
  ## omega ##
  ###########
  if(!is.null(omega.param)){
    names.tmp <- "omega*"
    index.est <- grepl(names.tmp,theta.out$beta$theta)
    omega.out <- theta.out$beta[index.est,]
  } else{
    omega.out <- 0
  }

  ###########
  ## alpha ##
  ###########
  alphas.out <- theta.out$alphas

  ########
  ## Ft ##
  ########
  Ft.out <- theta.out$Ft

  ##################
  ## Ft.predicted ##
  ##################
  if("Ft.predicted" %in% names(theta.out)){
    Ft.predicted.out <- theta.out$Ft.predicted

    ###############################
    ## Merge Ft and Ft.predicted ##
    ###############################
    data.to.merge <- list(
      Ft.out=Ft.out,
      Ft.predicted.out=Ft.predicted.out
    )
    Ft.out <- merge.by.columns(data.to.merge)
    Ft.out <- Ft.out[,c("study","event","theta","zz","xx","time0","val",paste("t",time_val,sep=""))]
  }
  list(beta.out=beta.out,
       gamma.out=gamma.out,
       omega.out=omega.out,
       alphas.out=alphas.out,
       Ft.out=Ft.out)

}

##df is a list of dataframes
#' @import plyr
merge.by.columns <- function(df){
  ## extract rownames of each data frame in df
  rownames.df <- lapply(df,rownames)

  out <- cbind(names=unlist(rownames.df),plyr::rbind.fill(df))
  rownames(out) <- NULL

  ## remove extra names variable
  out <- out[,-which(colnames(out)=="names")]

  return(out)
}


################################
## dimension order for output ##
################################

get.flatten.theta.name <- function(theta.names){
  out <- get.empty.list(theta.names)
  out[["beta"]] <- NULL
  out[["alphas"]] <- NULL
  out[["Ft"]] <- "Ft"
  out[["Ft.predicted"]] <- "Ft.predicted"
  return(out)
}


get.dim.order <- function(theta.names){
  dim.order <- get.empty.list(theta.names)

  dim.order[["beta"]] <- c("time","theta","event","study")
  dim.order[["alphas"]] <- c("time","theta","xx","event","study")
  dim.order[["Ft"]] <- c("time","xx","zz","event","study")
  dim.order[["Ft.predicted"]] <- c("time","time0","xx","zz","event","study")

  return(dim.order)
}


get.all.dim.order <- function(theta.names){

  dim.order <- get.dim.order(theta.names)  ## see get.dim.order() for order dimensions

  dim.order.simus <- dim.order
  dim.order.simus.ci <- dim.order

  for(u in 1:length(dim.order)){
    ## iters must be last because of the way we stack results from different seeds.
    dim.order.simus[[u]] <- c(dim.order[[u]],"iters")  ## add iters name for the vector of dimension names of the theta.names list.
    dim.order.simus.ci[[u]] <- c(dim.order[[u]],"val","iters") ## same..
  }

  list(dim.order=dim.order,
       dim.order.simus=dim.order.simus,
       dim.order.simus.ci=dim.order.simus.ci)
}





## null theta without time component

get.null.theta.no.time <- function(theta.names,
                                   first.label, #=num_study,
                                   first.label.name, #=paste("ss",1:num_study,sep=""),
                                   np,param.label,
                                   num_xx,la,z.choice){

  null.theta <- get.empty.list(theta.names)

  null.theta[["beta"]] <- array(0,dim=c(first.label,np,
                                        length(param.label)),
                                dimnames=list(
                                  study=first.label.name,
                                  event=paste("np",1:np,sep=""),
                                  theta=param.label))

  null.theta[["alphas"]] <- array(0,dim=c(first.label,np,num_xx,la),
                                  dimnames=list(
                                    study=first.label.name,
                                    event=paste("np",1:np,sep=""),
                                    xx=paste("xx",1:num_xx,sep=""),
                                    theta=paste("alpha",1:la,sep="")))

  null.theta[["Ft"]] <- array(0,dim=c(first.label,np,z.choice,num_xx),
                              dimnames=list(
                                study=first.label.name,
                                event=paste("np",1:np,sep=""),
                                zz=paste("zz",1:z.choice,sep=""),
                                xx=paste("xx",1:num_xx,sep="")))

  return(null.theta)
}

############################
## add result to an array ##
############################

merge.results <- function(theta.orig,theta.new,
                          study.names,s.names,param.label,
                          study.names.other,s.names.other,param.label.other,
                          type="beta"){

  dim.both <- rbind(dim(theta.orig),dim(theta.new))  ## row bind for dim of two arrays object
  dim.use <- apply(dim.both,2,max)                   ## chose maximum of dimension of two arrays

  dimnames1 <- dimnames(theta.orig)
  dimnames2 <- dimnames(theta.new)
  dimnames.use <- get.empty.list(names(dimnames1))  ## study, event, time, theta
  for(kk in 1:length(dimnames.use)){
    dimnames.use[[kk]] <- unique(c(dimnames1[[kk]],dimnames2[[kk]]))  ## get rid of duplicated names
  }

  theta.out <- array(0,dim=dim.use,
                     dimnames=dimnames.use)

  if(type=="nrisk"){
    theta.out[study.names.other,s.names.other,,,] <- theta.new[study.names.other,s.names.other,,,,drop=FALSE]
    theta.out[study.names,s.names,,,] <- theta.orig[study.names,s.names,,,,drop=FALSE]
  } else if(type=="beta"){
    theta.out[study.names.other,s.names.other,,param.label.other] <-
      theta.new[study.names.other,s.names.other,,param.label.other,
                drop=FALSE]
    theta.out[study.names,s.names,,param.label] <-
      theta.orig[study.names,s.names,,param.label,drop=FALSE]
  } else if(type=="beta-out"){
    theta.out[,study.names.other,s.names.other,,param.label.other,] <-
      theta.new[,study.names.other,s.names.other,,param.label.other,,drop=FALSE]
    theta.out[,study.names,s.names,,param.label,] <-
      theta.orig[,study.names,s.names,,param.label,,drop=FALSE]
  } else if(type=="beta-mean"){
    theta.out[study.names.other,s.names.other,,param.label.other,] <-
      theta.new[study.names.other,s.names.other,,param.label.other,,drop=FALSE]
    theta.out[study.names,s.names,,param.label,] <-
      theta.orig[study.names,s.names,,param.label,,drop=FALSE]
  } else if(type=="alpha"){
    theta.out[study.names.other,s.names.other,,,] <-
      theta.new[study.names.other,s.names.other,,,,drop=FALSE]
    theta.out[study.names,s.names,,,] <-
      theta.orig[study.names,s.names,,,,drop=FALSE]
  } else if(type=="alpha-out"){
    theta.out[,study.names.other,s.names.other,,,,] <-
      theta.new[,study.names.other,s.names.other,,,,,drop=FALSE]
    theta.out[,study.names,s.names,,,,] <-
      theta.orig[,study.names,s.names,,,,,drop=FALSE]
  } else if(type=="alpha-mean"){
    theta.out[study.names.other,s.names.other,,,,] <-
      theta.new[study.names.other,s.names.other,,,,,drop=FALSE]
    theta.out[study.names,s.names,,,,] <-
      theta.orig[study.names,s.names,,,,,drop=FALSE]
  } else if(type=="Ft"){
    theta.out[study.names.other,s.names.other,,,] <-
      theta.new[study.names.other,s.names.other,,,,drop=FALSE]
    theta.out[study.names,s.names,,,] <-
      theta.orig[study.names,s.names,,,,drop=FALSE]
  } else if(type=="Ft-out"){
    theta.out[,study.names.other,s.names.other,,,,] <-
      theta.new[,study.names.other,s.names.other,,,,,drop=FALSE]
    theta.out[,study.names,s.names,,,,] <-
      theta.orig[,study.names,s.names,,,,,drop=FALSE]
  } else if(type=="Ft-mean"){
    theta.out[study.names.other,s.names.other,,,,] <-
      theta.new[study.names.other,s.names.other,,,,,drop=FALSE]
    theta.out[study.names,s.names,,,,] <-
      theta.orig[study.names,s.names,,,,,drop=FALSE]
  } else if(type=="Ft.predicted"){
    theta.out[study.names.other,s.names.other,,,,] <-
      theta.new[study.names.other,s.names.other,,,,,drop=FALSE]
    theta.out[study.names,s.names,,,,] <-
      theta.orig[study.names,s.names,,,,,drop=FALSE]
  } else if(type=="Ft.predicted-out"){
    theta.out[,study.names.other,s.names.other,,,,,] <-
      theta.new[,study.names.other,s.names.other,,,,,,drop=FALSE]
    theta.out[,study.names,s.names,,,,,] <-
      theta.orig[,study.names,s.names,,,,,,drop=FALSE]
  } else if(type=="Ft.predicted-mean"){
    theta.out[study.names.other,s.names.other,,,,,] <-
      theta.new[study.names.other,s.names.other,,,,,,drop=FALSE]
    theta.out[study.names,s.names,,,,,] <-
      theta.orig[study.names,s.names,,,,,,drop=FALSE]
  }
  return(theta.out)
}






add.results <- function(theta.orig,
                        dim.add="event",dim.name="dep2"){
  theta.new <- theta.orig
  dim.use <- which(names(dimnames(theta.new))==dim.add)

  empty <- array(0,dim=dim(theta.orig)[-dim.use],
                 dimnames=unlist(list(dimnames(theta.orig)[-dim.use]),recursive=FALSE))

  theta.new <- abind(theta.new,empty,along=dim.use)
  dimnames(theta.new)[[dim.use]] <- c(dimnames(theta.orig)[[dim.use]],dim.name)
  names(dimnames(theta.new)) <- names(dimnames(theta.orig))
  return(theta.new)
}



#########################
## true beta and alpha ##
#########################

gt <- function(gtmod,t){
  if(gtmod=="log"){
    out <- log(t/50)
  } else if(gtmod=="all"){
    #out <- 0.002*(t-52)^2
    #out <- (t)^2
    #out <- log(50*t)
    out <- log(t/50)
  } else if(gtmod=="noall"){
    #out <- 0.004*(t-52)^2
    #out <- (t)^2
    out <- log(t/50)
  }
  return(out)
}


gtinv <- function(gtmod,t){
  if(gtmod=="log"){
    out <- 50*exp(t)
  } else if(gtmod=="all"){
    #out <- sqrt(abs(t)) #+ 52
    out <- 50*exp(t)
  } else if(gtmod=="noall"){
    #out <- sqrt(abs(t)) #+ 52
    out <- 50*exp(t)
  }
  return(out)
}

beta.true.value <- function(gtmod,beta0,t){
  out <- beta0 * gt(gtmod,t)
  return(out)
}


alpha.x <- function(a0,axmod,x){
  if(axmod=="line"){
    out <- a0 * x
  } else if(axmod=="all1"){
    out <- 2 * x /(1+exp(-5*(x-0.35)))
  } else if(axmod=="all2"){
    out <- 1 * x /(1+exp(-4*(x-0.35)))
  } else if(axmod=="cohort1"){
    out <- 0.75 *x*sin(x/0.15)+x
  } else if(axmod=="cohort2"){
    out <- 2 * x /(1+exp(-5*(x-0.35)))
  } else if(axmod=="predict1"){
    out <- 1 *x/(1+exp(-5*(x-0.35)))
  } else if(axmod=="predict2"){
    out <- 2 * x /(1+exp(-4*(x-0.35)))
  } else if(axmod=="pharos1"){
    out <- 0.5 *x*sin(x/0.10)
  } else if(axmod=="pharos2"){
    out <- 1.5 *x/(1+exp(-4*(x-0.35)))
  } else if(axmod=="logx"){
    out <- a0*x*log((1.+x))
  } else if(axmod=="expx"){
    out <- a0*x*exp(sin(pi*x-pi/2.)+0.1)
  } else if(axmod=="wah2"){
    out <- a0 *sin(pi * x)
  } else if(axmod=="wah3"){
    out <- a0*x**11.*(10.*(1-x))**6.+a0*(10.*x)**3.*(1-x)**10.
  } else if(axmod=="wah4"){
    out <- a0*x**11.*(10.*(1-x))**6.
  } else if(axmod=="sinx"){
    out <- a0 *sin(x)
  } else if(axmod=="lgit"){
    out <- a0*x/(1+exp(-7.*(x-0.5)))
  }
  return(out)
}



alpha.true <- function(a0,axmod,x,t,gtmod){
  out <- alpha.x(a0,axmod,x)
  out <- out * gt(gtmod,t)
  return(out)
}



######################################
## function to get common procedure ##
######################################
#' @importFrom abind adrop
#' @import stats
common.procedure <- function(arbitrary,num_study,
                             count.store,count.store.outside,
                             data,num_time,
                             time_val,p,n,nmax,m,maxm,m0_qvs,family.data,
                             common.param.estimation,est.cens.depend.z){

  norisk_ind_start <- data$norisk_ind_start
  y_start <- data$y_start
  ymiss_ind_start <- data$ymiss_ind_start
  z_start <- data$z_start
  x_start <- data$x_start
  s_start <- data$s_start
  q_start <- data$q_start
  delta_start <- data$delta_start

  ##print(c(min(s_start),max(s_start)))
  n <- n
  m <- m
  y <- y_start
  ymiss_ind <- ymiss_ind_start
  z <- z_start
  x <- x_start
  s <- s_start
  q <- q_start
  delta <- delta_start

  ## censoring
  #print(1-{apply(delta,1,sum)/apply(m,1,sum)})

  #################################
  ## Apply KM-jacknife estimator ##
  #################################
  kmjack <- get.kmjack(arbitrary,num_study,n,nmax,maxm,num_time,time_val,
                       p,m0_qvs,y,ymiss_ind,s,q,
                       delta,common.param.estimation,est.cens.depend.z,z,m)

  ynew <- kmjack$ynew               ## does not have negative number
  ynew_orig <- kmjack$ynew_orig   ## contains negative number
  #    print(ynew)

  ###################
  ## update counts ##
  ###################

  new.count <- get.count(num_study,n,m,maxm,time_val,num_time,ynew,delta)
  new.count.outside <- get.count.outside(num_study,n,m,maxm,time_val,
                                         num_time,ynew_orig,delta)

  # new.count.tmp <- array(0,dim=c(num_study,num_time,5),
  #                               dimnames=list(
  #                                       paste("ss",1:num_study,sep=""),
  #                                       paste("tt",time_val,sep=""),
  #                                       c("time",dimnames(new.count)[[3]])))

  new.count.tmp <- NULL
  new.count.outside.tmp <- NULL

  for(ss in 1:num_study){
    ##print(ss)
    ss.tmp <- rep(ss,length(time_val))
    my.counts.tmp <- adrop(new.count[ss,,,drop=FALSE],drop=1)/sum(m[ss,1:n[ss]])  ## column-wise counting for one, zero_cens, zero_cens, other at each time point)
    ## divide by sum of all event of interests
    new.count.tmp <- rbind(new.count.tmp, cbind(ss.tmp,tt=time_val,my.counts.tmp,row.names=NULL))

    my.counts.outside.tmp <- adrop(new.count.outside[ss,,,drop=FALSE],drop=1)/sum(m[ss,1:n[ss]])
    new.count.outside.tmp <- rbind(new.count.outside.tmp,cbind(ss.tmp,tt=time_val,my.counts.outside.tmp,row.names=NULL))
  }

  count.store <- rbind(count.store, new.count.tmp)  ## count.store <-null
  count.store.outside <- rbind(count.store.outside, new.count.outside.tmp) ## count.store.outside<-null (no uncesored)

  ##count.store <- abind(count.store,new.count.tmp,along=2)
  ##count.store <- rbind(count.store,cbind(time_val,new.count/sum(m)))

  list(count.store=count.store,
       count.store.outside=count.store.outside,
       n=n,m=m,y=y,
       ymiss_ind=ymiss_ind,z=z,x=x,s=s,q=q,delta=delta,ynew=ynew)
}


################################
## Functions to generate data ##
################################

make.binom <- function(size=1,prob=0.5,type="none"){
  if(type=="none"){
    function(nr,par1,par2){
      rbinom(nr,size=size,prob=prob)
    }
  }
}

#' @import stats
make.uniform <- function(min=-1,max=1,type="none"){
  if(type=="none"){
    function(nr,par1,par2){
      runif(nr,min=min,max=max)
    }
  } else if(type=="par1"){
    function(nr,par1,par2){
      runif(nr,min=par1,max=max)
    }
  } else if(type=="par2"){
    function(nr,par1,par2){
      runif(nr,min=min,max=par2)
    }
  }
}


make.normal <- function(mean=0,sd=1,type="none"){
  if(type=="none"){
    function(nr,par1,par2){
      rnorm(nr,mean=mean,sd=sd)
    }
  } else if(type=="par2"){
    ## sd changes
    function(nr,par1,par2){
      rnorm(nr,mean=mean,sd=abs(par2))
    }
  } else if(type=="par1"){
    ## mean changes
    function(nr,par1,par2){
      rnorm(nr,mean=par1,sd=sd)
    }
  }
}



#' @import stats
make.gamma <- function(shape=1.5,scale=2,type="none"){
  if(type=="none"){
    function(nr,par1,par2){
      rgamma(nr,shape=shape,scale=scale)
    }
  } else if(type=="par1"){
    function(nr,par1,par2){
      rgamma(nr,shape=par1,scale=scale)
    }
  } else if(type=="par2"){
    function(nr,par1,par2){
      rgamma(nr,shape=shape,scale=abs(par2))
    }
  }
}


make.t <- function(df=3,ncp=-2,type="none"){
  if(type=="none"){
    function(nr,par1,par2){
      rt(nr,df=df,ncp=ncp)
    }
  } else if(type=="par1"){
    function(nr,par1,par2){
      rt(nr,df=par1,ncp=ncp)
    }
  } else if(type=="par2"){
    function(nr,par1,par2){
      rt(nr,df=df,ncp=par2)
    }
  }
}


#' @import stats
make.mixture.normals <- function(mean1=0,sd1=1,mean2=1,sd2=2,mix=0.5,type="none"){
  if(type=="none"){
    function(nr,par1,par2){
      if(runif(1) < mix){
        rnorm(nr,mean=mean1,sd=sd1)
      } else {
        rnorm(nr,mean=mean2,sd=sd2)
      }
    }
  } else if(type=="par2"){
    function(nr,par1,par2){
      if(runif(1) < mix){
        rnorm(nr,mean=mean1,sd=abs(par2))
      } else {
        rnorm(nr,mean=mean2,sd=abs(par2))
      }
    }
  } else if(type=="gamm"){
    function(nr,par1,par2){
      if(runif(1) < mix){
        rnorm(nr,mean=mean1,sd=sd1)
      } else {
        rgamma(nr,shape=mean2,scale=sd2)
      }
    }
  }
}

## function to compute beta^Tz
get.z.etas <- function(mtmp,lb,beta0int,beta0,gamma.param,omega.param,ztmp){
  out <- rep(0,mtmp)

  for(jj in 1:mtmp){
    if(lb[[jj]] > 1){
      out[jj] <- ztmp[jj,1:lb[[jj]]] %*% beta0[[jj]]
    } else {
      out[jj] <- ztmp[jj] %*% beta0[[jj]]
    }

    if(!is.null(gamma.param)){
      out[jj] <- out[jj] + gamma.param[jj]
    }
  }

  ## add intercept
  if(!is.null(beta0int)){
    out <- out + beta0int
  }

  if(!is.null(omega.param)){
    out <- out + omega.param
  }

  return(out)
}

get.x.etas <- function(m,a0,axmod,x){
  out <- rep(0,m)

  for(jj in 1:m){
    out[jj] <- alpha.x(a0[jj],axmod[jj],x)
  }
  return(out)
}

## uniform Ft
#' @import stats
get.unif.Ft <- function(m){
  out <- rep(0,m)
  tol <- 1e-6
  for(kk in 1:m){
    out[kk] <- min(max(runif(1),tol),1-tol)
  }
  return(out)
}


## generate onset ages
invFt <- function(m,etas_z,etas_x,r,Ft,gtmod){
  out <- rep(0,m)
  w <- rep(0,m)

  for(kk in 1:m){
    w[kk] <- etas_z[kk] + etas_x[kk]
    out[kk] <- gtinv(gtmod,(-log(1/Ft[kk]-1)-r)/w[kk])
  }
  return(out)
}


## generate censoring times
#' @import stats
genc <- function(onset_age,censorrate,z=NULL){
  if(is.null(z)){
    ## censoring independent of Z
    if(censorrate==0){
      out <- onset_age + 1
      return(out)
    } else if(censorrate==30){
      # actually this corresponds to 40% censoring
      min_unif <-38
      max_unif <- 80
    } else if(censorrate==50){
      min_unif <- 38
      max_unif <- 65
    } else if(censorrate==60){
      min_unif <- 20
      max_unif <- 40
    } else if (censorrate==70){
      min_unif <- 8
      max_unif <- 20
    }

    out <- runif(1,min=min_unif,max=max_unif)
  } else {
    ## censoring depends on Z
    if(censorrate==0){
      out <- onset_age + 1
      return(out)
    }

    if(z==1){
      ## 40% censoring
      min_unif <- 38
      max_unif <- 80
    } else {
      ## 50% censoring
      min_unif <- 38
      max_unif <- 65
    }
    out <- runif(1,min=min_unif,max=max_unif)
  }
  return(out)
}


#################################################################################################################
## get.kmjack : function to compute pseudovalues
## This depends on whether studies are considered jointly or not
## Also depends on whether censoring depends on a discrete covariate z or not (only implemented for 1 discrete z)
################################################################################################################
#' @useDynLib JPRAT
get.kmjack <- function(arbitrary,num_study,n,nmax,maxm,num_time,time_val,
                       p,m0_qvs,y,ymiss_ind,s,q,
                       delta,common.param.estimation,est.cens.depend.z,z,m){
  ## needs to take in original n for storage

  ## set for f90
  storage.mode(arbitrary) <- "logical"
  storage.mode(num_time) <- "integer"
  storage.mode(n) <- "integer"
  storage.mode(m) <- "integer"
  storage.mode(maxm) <- "integer"
  storage.mode(time_val) <- "double"
  storage.mode(p) <- "integer"
  storage.mode(m0_qvs) <- "integer"
  storage.mode(y) <- "double"
  storage.mode(ymiss_ind) <- "double"
  storage.mode(s) <- "double"
  storage.mode(q) <- "double"
  storage.mode(delta) <- "double"
  storage.mode(z) <- "double"

  ## output
  ynew_out <- array(0,dim=c(num_study,num_time,nmax,maxm),
                    dimnames=list(
                      paste("ss",1:num_study,sep=""),
                      paste("t",time_val,sep=""),
                      paste("n",1:nmax,sep=""),
                      paste("m",1:maxm,sep="")))

  ynew_out_orig <- ynew_out




  if(common.param.estimation==FALSE){
    ### no common alpha, beta
    ## This means we keep the study information separate when forming pseudovalues.
    for(ss in 1:num_study){
      if(est.cens.depend.z==FALSE){
        ## no dependence on z
        n.use <- n[ss]
        index.use <- 1:n[ss]
        m.use <- m[ss,index.use]
        y.use <- adrop(y[ss,,index.use,,drop=FALSE],drop=1)
        ymiss_ind.use <-
          adrop(ymiss_ind[ss,,index.use,,drop=FALSE],drop=1)
        s.use <- adrop(s[ss,index.use,,drop=FALSE],drop=1)
        q.use <- adrop(q[ss,,index.use,,drop=FALSE],drop=1)
        delta.use <-
          adrop(delta[ss,index.use,,drop=FALSE],drop=1)

        ynew <- array(0,dim=c(num_time,n.use,maxm),
                      dimnames=list(
                        paste("t",time_val,sep=""),
                        paste("n",1:n.use,sep=""),
                        paste("m",1:maxm,sep="")))

        ynew_orig <- ynew
        storage.mode(ynew) <- "double"
        storage.mode(ynew_orig) <- "double"

        out <- .Fortran("kmjack",
                        arbitrary,
                        num_time,
                        n.use,
                        m.use,
                        maxm,
                        time_val,
                        p,
                        m0_qvs,
                        y.use,
                        ymiss_ind.use,
                        s.use,
                        q.use,
                        delta.use,
                        ynew=ynew,
                        ynew_orig=ynew_orig,
                        PACKAGE ="JPRAT")
        ynew_out[ss,,index.use,] <- out$ynew
        ynew_out_orig[ss,,index.use,] <- out$ynew_orig
      } else {

        ## dependence on z
        for(ee in 1:maxm){
          ## extract unique z-values
          unique.z <- unique(c(
            adrop(z[,,ee,,drop=FALSE],drop=3)))  ## we vectorize the array and apply unique to it

          for(zz in 1:length(unique.z)){
            zz.tmp <- unique.z[zz]
            index.use <-
              which(adrop(z[ss,1:n[ss],ee,,drop=FALSE],drop=c(1,3))==zz.tmp)  ## only works with lb=1
            n.use <- length(index.use)
            m.use <- m[ss,index.use]

            ## in order to use the same f90 code, we are creating dummy y.use with the
            ##  size of individuals in event type ee with specific zz-value.
            y.use <- array(0,dim=c(num_time,n.use,maxm),
                           dimnames=list(
                             paste("t",time_val,sep=""),
                             paste("n",1:n.use,sep=""),
                             paste("m",1:maxm,sep="")))

            ymiss_ind.use <- y.use

            s.use <- array(0,dim=c(n.use,maxm),
                           dimnames=list(
                             paste("n",1:n.use,sep=""),
                             paste("m",1:maxm,sep="")))

            delta.use <- s.use

            q.use <- array(0,dim=c(p,n.use,maxm),
                           dimnames=list(
                             paste("p",1:p,sep=""),
                             paste("n",1:n.use,sep=""),
                             paste("m",1:maxm,sep="")))

            y.use[,,ee] <- adrop(y[ss,,index.use,ee,drop=FALSE],drop=1)
            ymiss_ind.use[,,ee] <- adrop(ymiss_ind[ss,,index.use,ee,drop=FALSE],drop=1)
            s.use[,ee] <- adrop(s[ss,index.use,ee,drop=FALSE],drop=1)
            q.use[,,ee] <- adrop(q[ss,,index.use,ee,drop=FALSE],drop=1)
            delta.use[,ee] <- adrop(delta[ss,index.use,ee,drop=FALSE],drop=1)

            ynew <- array(0,dim=c(num_time,n.use,maxm),
                          dimnames=list(
                            paste("t",time_val,sep=""),
                            paste("n",1:n.use,sep=""),
                            paste("m",1:maxm,sep="")))

            ynew_orig <- ynew
            storage.mode(ynew) <- "double"
            storage.mode(ynew_orig) <- "double"

            out <- .Fortran("kmjack",
                            arbitrary,
                            num_time,
                            n.use,
                            m.use,
                            maxm,
                            time_val,
                            p,
                            m0_qvs,
                            y.use,
                            ymiss_ind.use,
                            s.use,
                            q.use,
                            delta.use,
                            ynew=ynew,
                            ynew_orig=ynew_orig,
                            PACKAGE ="JPRAT")

            ynew_out[ss,,index.use,ee] <- out$ynew[,,ee,drop=FALSE]
            ynew_out_orig[ss,,index.use,ee] <- out$ynew_orig[,,ee,drop=FALSE]
          }
        }
      }
    }
  } else { ## common alpha, beta
    ##############
    ## This means we combine study information
    n.all <- sum(n)

    m.all <- NULL
    y.all <- NULL
    ymiss_ind.all <- NULL
    s.all <- NULL
    q.all <- NULL
    delta.all <- NULL
    index.all <-  get.empty.list(paste("study",1:num_study,sep=""))
    z.all <- NULL

    tmp <- 0
    for(ss in 1:num_study){

      #abind(): along=N, The dimension along which to bind the arrays.
      m.all <- abind(m.all,m[ss,1:n[ss]],along=1)  # similar to rbind
      y.all <- abind(y.all,adrop(y[ss,,1:n[ss],,drop=FALSE],drop=1),along=2)    # similar to cbind - columwise:   time_vals by n
      ymiss_ind.all <- abind(ymiss_ind.all,
                             adrop(ymiss_ind[ss,,1:n[ss],,drop=FALSE],drop=1),along=2)   # adrop(): Drop degenerate dimensions of an array object.
      s.all <- abind(s.all,
                     adrop(s[ss,1:n[ss],,drop=FALSE],drop=1),along=1)
      q.all <- abind(q.all,
                     adrop(q[ss,,1:n[ss],,drop=FALSE],drop=1),along=2) ## array to vectorization
      delta.all <- abind(delta.all,
                         adrop(delta[ss,1:n[ss],,drop=FALSE],drop=1),along=1)  ## multiple array to vectorization (single array)
      z.all <- abind(z.all,
                     adrop(z[ss,1:n[ss],,,drop=FALSE],drop=1),along=1)
      index.all[[ss]] <- (tmp+1):(tmp+n[ss])
      tmp <- tmp + n[ss] ## adding the sample size for all studies
    }




    if(est.cens.depend.z==FALSE){ ### Yes
      ## no dependence on z
      ynew <- array(0,dim=c(num_time,n.all,maxm),
                    dimnames=list(
                      paste("t",time_val,sep=""),
                      paste("n",1:n.all,sep=""),
                      paste("m",1:maxm,sep="")))
      ynew_orig <- ynew             ## 13 by sum(n)

      storage.mode(ynew) <- "double"
      storage.mode(ynew_orig) <- "double"

      out <- .Fortran("kmjack",
                      arbitrary,
                      num_time,
                      n.all,
                      m.all,
                      maxm,
                      time_val,
                      p,
                      m0_qvs,
                      y.all,
                      ymiss_ind.all,
                      s.all,
                      q.all,
                      delta.all,
                      ynew=ynew,
                      ynew_orig=ynew_orig,
                      PACKAGE="JPRAT")  ## code from fortran

      ##tmp <- 0
      for(ss in 1:num_study){
        ynew_out[ss,,1:n[ss],] <- out$ynew[,index.all[[ss]],]
        ynew_out_orig[ss,,1:n[ss],] <- out$ynew_orig[,index.all[[ss]],]
        ##ynew_out[ss,,1:n[ss],] <- out$ynew[,(tmp+1):(tmp+n[ss]),]
        ##ynew_out_orig[ss,,1:n[ss],] <- out$ynew_orig[,(tmp+1):(tmp+n[ss]),]
        ##tmp <- tmp + n[ss]
      }
    } else {
      ## dependence on z
      for(ee in 1:maxm){
        ## extract unique z-values
        unique.z <- unique(c(
          adrop(z.all[,ee,,drop=FALSE],drop=2)))  ## we vectorize the array and apply unique to it

        for(zz in 1:length(unique.z)){
          zz.tmp <- unique.z[zz]
          index.use <- which(
            adrop(z.all[,ee,,drop=FALSE],drop=2)==zz.tmp)  ## only works with lb=1
          n.use <- length(index.use)
          m.use <- m.all[index.use]

          ## in order to use the same f90 code, we are creating dummy y.use with the
          ##  size of individuals in event type ee with specific zz-value.
          y.use <- array(0,dim=c(num_time,n.use,maxm),
                         dimnames=list(
                           paste("t",time_val,sep=""),
                           paste("n",1:n.use,sep=""),
                           paste("m",1:maxm,sep="")))

          ymiss_ind.use <- y.use

          s.use <- array(0,dim=c(n.use,maxm),
                         dimnames=list(
                           paste("n",1:n.use,sep=""),
                           paste("m",1:maxm,sep="")))

          delta.use <- s.use

          q.use <- array(0,dim=c(p,n.use,maxm),
                         dimnames=list(
                           paste("p",1:p,sep=""),
                           paste("n",1:n.use,sep=""),
                           paste("m",1:maxm,sep="")))

          y.use[,,ee] <- y.all[,index.use,ee,drop=FALSE]
          ymiss_ind.use[,,ee] <- ymiss_ind.all[,index.use,ee,drop=FALSE]
          s.use[,ee] <- s.all[index.use,ee,drop=FALSE]
          q.use[,,ee] <- q.all[,index.use,ee,drop=FALSE]
          delta.use[,ee] <- delta.all[index.use,ee,drop=FALSE]

          ynew <- array(0,dim=c(num_time,n.use,maxm),
                        dimnames=list(
                          paste("t",time_val,sep=""),
                          paste("n",1:n.use,sep=""),
                          paste("m",1:maxm,sep="")))
          ynew_orig <- ynew

          storage.mode(ynew) <- "double"
          storage.mode(ynew_orig) <- "double"

          out <- .Fortran("kmjack",
                          arbitrary,
                          num_time,
                          n.use,
                          m.use,
                          maxm,
                          time_val,
                          p,
                          m0_qvs,
                          y.use,
                          ymiss_ind.use,
                          s.use,
                          q.use,
                          delta.use,
                          ynew=ynew,
                          ynew_orig=ynew_orig,
                          PACKAGE="JPRAT")

          for(ss in 1:num_study){
            index.match.left <- index.all[[ss]] %in% index.use
            index.match.left <- (1:n[ss])[index.match.left]

            index.match.right <- index.use %in% index.all[[ss]]
            index.match.right <- (1:n.use)[index.match.right]

            ynew_out[ss,,index.match.left,ee] <- out$ynew[,index.match.right,ee,drop=FALSE]
            ynew_out_orig[ss,,index.match.left,ee] <- out$ynew_orig[,index.match.right,ee,drop=FALSE]
          }
        }
      }
    }
  }

  fix.tolerance <- function(a){
    if(abs(a)< 1e-10){
      a <- 0
    }
    return(a)
  }

  ynew_out <- apply(ynew_out,c(1,2,3,4),fix.tolerance) ## 3 by 13 by n
  ynew_out_orig <- apply(ynew_out_orig,c(1,2,3,4),fix.tolerance)


  list(ynew=ynew_out,ynew_orig=ynew_out_orig)
}


###################
## get new count ##
###################
#' @useDynLib JPRAT
get.count <- function(num_study,n,m,maxm,time_val,num_time,ynew,delta){
  ## storage for f90
  storage.mode(n) <- "integer"
  storage.mode(m) <- "integer"
  storage.mode(maxm) <- "integer"
  storage.mode(num_time) <- "integer"
  storage.mode(ynew) <- "double"
  storage.mode(delta) <- "double"

  count_new_out <- array(0,dim=c(num_study,num_time,4),
                         dimnames=list(
                           paste("ss",1:num_study,sep=""),
                           paste("t",time_val,sep=""),
                           c("one","zero_nocens","zero_cens","other")))

  tmp <- array(0,dim=c(num_time,4),
               dimnames=list(
                 paste("t",time_val,sep=""),
                 c("one","zero_nocens","zero_cens","other")))

  for(ss in 1:num_study){
    count_new <- tmp
    storage.mode(count_new) <- "double"

    out <- .Fortran("get_count_new",n[ss],m[ss,1:n[ss]],maxm,num_time,
                    adrop(ynew[ss,,1:n[ss],,drop=FALSE],drop=1),
                    adrop(delta[ss,1:n[ss],,drop=FALSE],drop=1),  ### array to vector
                    count_new=count_new, PACKAGE ="JPRAT")
    count_new_out[ss,,] <- out$count_new
  }

  return(count_new_out)
}


#' @useDynLib JPRAT
get.count.outside <- function(num_study,n,m,maxm,time_val,num_time,ynew,delta){
  ## storage for f90
  storage.mode(n) <- "integer"
  storage.mode(m) <- "integer"
  storage.mode(maxm) <- "integer"
  storage.mode(num_time) <- "integer"
  storage.mode(ynew) <- "double"
  storage.mode(delta) <- "double"

  count_new_out <- array(0,dim=c(num_study,num_time,2),
                         dimnames=list(
                           paste("ss",1:num_study,sep=""),
                           paste("t",time_val,sep=""),
                           c("zero","one")))

  tmp <- array(0,dim=c(num_time,2),
               dimnames=list(
                 paste("t",time_val,sep=""),
                 c("zero","one")))

  for(ss in 1:num_study){
    count_new <- tmp
    storage.mode(count_new) <- "double"

    out <- .Fortran("get_count_outside",n[ss],m[ss,1:n[ss]],maxm,num_time,
                    adrop(ynew[ss,,1:n[ss],,drop=FALSE],drop=1),
                    adrop(delta[ss,1:n[ss],,drop=FALSE],drop=1),count_new=count_new, PACKAGE ="JPRAT")
    count_new_out[ss,,] <- out$count_new
  }

  return(count_new_out)
}

##+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-
## Functions for gamm4
##+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+--+

## put NAs into data set

get.nas <- function(n,m,ynew,z,x){
  ynew2 <- ynew[,1:n,,drop=FALSE]   ## n  samples size for each study
  z2 <- z[1:n,,,drop=FALSE]
  m <- m[1:n]
  x <- x[1:n]

  maxm <- max(m)
  for(i in 1:n){
    if(m[i]<maxm){   ## if m ==0, then m
      ynew2[,i,(m[i]+1):maxm] <- NA
    }
  }
  list(m=m,ynew=ynew2,z=z2,x=x)
}



make.data.set <- function(num_study,time_val,n,nmax,
                          m,lb,y,x,z,real_data,
                          gamma.param,omega.param){

  names <- paste("ss",1:num_study,sep="")
  study.list <- get.empty.list(names)
  family.list <- get.empty.list(names)
  yy.list <- get.empty.list(names)
  x.list <- get.empty.list(names)
  z.list <- get.empty.list(names)
  gamma.list <- get.empty.list(names)
  omega.list <- get.empty.list(names)

  for(ss in 1:num_study){

    ###################
    ## get NA values ##
    ###################
    out.na <- get.nas(n[ss],m[ss,],
                      adrop(y[ss,,,,drop=FALSE],drop=1),
                      adrop(z[ss,,,,drop=FALSE],drop=1),
                      adrop(x[ss,,drop=FALSE],drop=1))  ## list  of vectorized m, ynew, z, x
    m_ss <- out.na$m
    y_ss <- out.na$ynew
    z_ss <- out.na$z
    x_ss <- out.na$x

    ## vector indicating which study each observation belongs
    study <- rep(ss,times=sum(m_ss))
    study.list[[ss]] <- study


    ## vector indicating which family each observation belongs
    family <- rep(1:n[ss],times=m_ss)
    family.list[[ss]] <- family

    yy <- array(0,dim=c(sum(m_ss),length(time_val)),
                dimnames=list(paste("i",1:sum(m_ss),sep=""),
                              paste("t",time_val,sep="")))  ### n by 13 matrix

    for(tt in 1:length(time_val)){
      ## make  y into a vector of values
      new.y <- as.vector(t(adrop(y_ss[tt,,,drop=FALSE],drop=1)))
      new.y <- as.vector(na.omit(new.y))
      yy[,tt] <- new.y
    }

    yy.list[[ss]] <- yy

    new.x <- NULL
    for(i in 1:n[ss]){
      tmp.x <- matrix(0,m_ss[i],m_ss[i]) ## zero matrix for each number of clinical events.
      diag(tmp.x) <- x_ss[i]
      new.x <- rbind(new.x,tmp.x)
    }
    x.list[[ss]] <- new.x

    if(!is.null(gamma.param)){
      ## design matrix for gamma parameters
      new.gamma <- NULL    ## event specific effect -no for real data set
      for(i in 1:n[ss]){
        tmp.gamma <- matrix(0,m_ss[i],m_ss[i])
        diag(tmp.gamma) <- 1
        tmp.gamma <- as.matrix(tmp.gamma[,-1])
        new.gamma <- rbind(new.gamma,tmp.gamma)
      }
      gamma.list[[ss]] <- new.gamma
    }

    if(!is.null(omega.param)){
      ## design matrix for omega parameters
      if(ss!=1){
        omega.list[[ss]] <- rep(1,n[ss])
      } else {
        omega.list[[ss]] <- rep(0,n[ss])
      }
    }

    ## make z into a matrix of values
    z.values <- NULL
    for(i in 1:n[ss]){
      tmp.z <- matrix(0,nrow=m_ss[i],ncol=sum(unlist(lb[[ss]])))  # a n-length vector
      tmp.index <- 1
      for(k in 1:length(lb[[ss]])){
        tmp.z[k,tmp.index:(tmp.index+lb[[ss]][[k]]-1)] <- z_ss[i,k,1:lb[[ss]][[k]]]  ## double check this
        tmp.index <- tmp.index + lb[[ss]][[k]]
      }
      z.values <- rbind(z.values,tmp.z)
    }

    ## get names
    z.names <- NULL
    lb.tmp <- unlist(lb[[ss]])
    for(k in 1:length(lb.tmp)){
      z.names <- c(z.names,paste(paste("Z",k,"_",sep=""),1:lb.tmp[k],sep=""))
    }
    colnames(z.values) <- z.names

    z.list[[ss]] <- z.values
  }

  list(study=study.list,family=family.list,
       yy=yy.list,new.x=x.list,z.values=z.list,
       gamma.list=gamma.list,omega.list=omega.list)
}


get.data.gamm <- function(n,m,num_study,tt,study,family,y,new.x,z.values,
                          gamma.list,omega.list,
                          beta0int,gamma.param,omega.param,common.param.estimation){

  data.set.out <- NULL
  xx.names <- NULL
  zz.names <- NULL
  omega.names <- NULL
  gamma.names <- NULL

  for(ss in 1:num_study){

    data.set.tmp <- data.frame(cbind(study[[ss]],family[[ss]],y[[ss]][,tt]))
    colnames(data.set.tmp) <- c("study","family","Y")
    data.set.out <- rbind(data.set.out,data.set.tmp)

    if(common.param.estimation==FALSE){ # no common alpha, beta
      xx.names <- c(xx.names,paste("study_",ss,"_X_",1:ncol(new.x[[ss]]),sep=""))
      zz.names <- c(zz.names,paste("study_",ss,"_",colnames(z.values[[ss]]),sep=""))

      if(!is.null(omega.param)){
        if(ss!=1){
          omega.names <- c(omega.names,paste("study_",ss,"_omega_Z",sep=""))
        }
      }
    } else { # common alpha,beta
      xx.names <- paste("X_",1:ncol(new.x[[ss]]),sep="")
      zz.names <- paste(colnames(z.values[[ss]]),sep="")
    }
  }

  if(!is.null(gamma.param)){
    gamma.names <- paste("gamma_",1:ncol(gamma.list[[1]])+1,"_Z",sep="")
  }

  ########################################################
  ## Create X and Z part, gamma and omega design matrix ##
  ########################################################

  xx_tmp <- array(0,dim=c(nrow(data.set.out),length(xx.names)),
                  dimnames=list(rownames(data.set.out),xx.names))
  zz_tmp <- array(0,dim=c(nrow(data.set.out),length(zz.names)),
                  dimnames=list(rownames(data.set.out),zz.names))
  gamma_tmp <- array(0,dim=c(nrow(data.set.out),length(gamma.names)),
                     dimnames=list(rownames(data.set.out),gamma.names))
  omega_tmp <- array(0,dim=c(nrow(data.set.out),length(omega.names)),
                     dimnames=list(rownames(data.set.out),omega.names))

  index_tmp <- 1

  for(ss in 1:num_study){
    if(common.param.estimation==FALSE){
      xx.names.tmp <- paste("study_",ss,"_X_*",sep="")
    } else {
      xx.names.tmp <- paste("X_*",sep="")
    }
    index.xx.names <- grep(glob2rx(xx.names.tmp),colnames(xx_tmp)) ## workhorse function for pattern matching of character vectors
    xx_tmp[index_tmp:(index_tmp+sum(m[ss,1:n[ss]])-1),index.xx.names] <- new.x[[ss]] ## 1~ number of events at each study


    #glob2rx() converts a pattern including a wildcard into the equivalent regular expression.
    #You then need to pass this regular expression onto one of R's pattern matching tools.
    #If you want to match "blue*" where * has the usual wildcard, not regular expression,
    #meaning we use glob2rx() to convert the wildcard pattern into a useful regular expression: glob2rx("blue*") .> ^blue

    if(common.param.estimation==FALSE){
      zz.names.tmp  <- paste("study_",ss,"_Z*",sep="")
    } else {
      zz.names.tmp  <- paste("Z*",sep="")
    }
    index.zz.names <- grep(glob2rx(zz.names.tmp),colnames(zz_tmp))
    zz_tmp[index_tmp:(index_tmp+sum(m[ss,1:n[ss]])-1),index.zz.names] <- z.values[[ss]]

    if(!is.null(gamma.param)){
      ## gamma data matrix
      gamma_tmp[index_tmp:(index_tmp+sum(m[ss,1:n[ss]])-1),] <- gamma.list[[ss]]
    }

    if(!is.null(omega.param)){
      if(common.param.estimation==FALSE & ss!=1){
        omega.names.tmp <- paste("study_",ss,"_omega_Z",sep="")
        index.omega.names <- grep(glob2rx(omega.names.tmp),colnames(omega_tmp))
        omega_tmp[index_tmp:(index_tmp+sum(m[ss,1:n[ss]])-1),index.omega.names] <- omega.list[[ss]]
      }
    }

    index_tmp <- sum(m[ss,1:n[ss]])+ index_tmp
  }

  if(!is.null(beta0int)){
    ## for intercept
    zz_tmp <- cbind(1,zz_tmp)
    colnames(zz_tmp)[1] <- "Z0"
  }

  data.set.out <- cbind(data.set.out,gamma_tmp,omega_tmp,xx_tmp,zz_tmp)
  data.set.out$study <- as.factor(data.set.out$study)
  data.set.out$family <- as.factor(data.set.out$family)

  tmp.np <- NULL
  for(ss in 1:num_study){
    for(ii in 1:n[ss]){
      tmp.np <- c(tmp.np,1:m[ss,ii])
    }
  }

  data.set.out$np <- as.factor(tmp.np)

  ## including the term R_si
  data.set.out <- transform(data.set.out,int=interaction(data.set.out$study,data.set.out$family)) ## 1.1, 2.1, 3.1, 1.2, 2.2, 3.2
  ## transform : useful with data frames, add int values.

  return(data.set.out)
}




## gamm.mle with the constraint: ax=0 at x=0
#' @import cubature
#' @import cubature
gamm.mle.new <- function(num_study,np,lb,num_xx,xks,
                         data.gamm,
                         knot.length,zeval,z.choice,real_data,
                         param.label,beta0int,gamma.param,omega.param,
                         spline.constrain,common.param.estimation,
                         par_fu,random.effect,link.type,z.proportions,la){
  X <- NULL

  null.theta <- get.null.theta.no.time(theta.names=c("beta","alphas","Ft"),
                                       first.label=num_study,
                                       first.label.name=paste("ss",1:num_study,sep=""),
                                       np,param.label,
                                       num_xx,la,z.choice)

  beta.est <- null.theta$beta
  beta.var <- null.theta$beta

  alphas.est <- null.theta$alpha
  alphas.var <- null.theta$alpha

  Ft.est <- null.theta$Ft
  Ft.var <- null.theta$Ft

  ## not used
  alphas_ij <- NULL
  Ft_ij <- NULL
  alphas_ij_var <- alphas_ij
  Ft_ij_var <- Ft_ij

  ######################
  ## adjust data.gamm ##
  ######################
  znam.orig <- colnames(data.gamm)[grep(glob2rx("*Z*"),colnames(data.gamm))]
  znam <- znam.orig

  if(spline.constrain==TRUE){
    ##################################
    ## make empty lists for storage ##
    ##################################
    xnam.orig <- NULL
    XX.list <- NULL
    knots.list.full <- list()
    pen.list.full <- list()

    if(common.param.estimation==FALSE){ # no common beta,alpha
      for(ss in 1:num_study){
        ###################################################
        ## add constraint for spline to go through (0,0) ##
        ###################################################
        names <- paste("study_",ss,"_nn",1:np,sep="")
        knots.list <- get.empty.list(names)

        names.X <- paste("study_",ss,"_XX",1:np,"_",sep="")
        pen.list <- get.empty.list(names.X)

        for(nn in 1:np){
          ## Create a spline basis and penalty, making sure there is a knot
          ## at the constraint point, (0 here, but could be anywhere)

          ## we suppose X is on [0,1], may need to change!
          knots.list[[nn]] <- data.frame(x=seq(0,1,length=knot.length[nn]))

          index.X <- which(colnames(data.gamm)==paste("study_",ss,"_X_",nn,sep=""))
          colnames(data.gamm)[index.X] <- "X"

          sm <- smoothCon(s(X,k=knot.length[nn],bs="cr"),
                          data.gamm,knots=knots.list[[nn]])[[1]]


          ## 1st parameters is value of spline at knot location 0.
          ## set it to 0 by dropping...
          XX.tmp <- paste("study_",ss,"_XX",nn,"_",sep="")

          pos <- 1
          envir = as.environment(pos)
          assign(XX.tmp, sm$X[,-1], envir = envir)    ## spline basis
          #assign(XX.tmp, sm$X[,-1],envir=globalenv())    ## spline basis

          XX.list <- c(XX.list,XX.tmp)

          S.tmp <- sm$S[[1]][-1,-1]   ## spline penalty
          pen.list[[nn]] <- list(S.tmp)

          colnames(data.gamm)[index.X] <- paste("study_",ss,"_X_",nn,sep="")
        }
        knots.list.full <- appendList(knots.list.full,knots.list)
        pen.list.full <- appendList(pen.list.full,pen.list)
      }
    } else { # common beta, alpha
      #######
      ############################################
      ## add constraint for spline to go through (0,0) ##
      ##################################################
      names <- paste("nn",1:np,sep="")
      knots.list <- get.empty.list(names)

      names.X <- paste("XX",1:np,"_",sep="")
      pen.list <- get.empty.list(names.X)

      for(nn in 1:np){
        ## Create a spline basis and penalty, making sure there is a knot
        ## at the constraint point, (0 here, but could be anywhere)

        ## we suppose X is on [0,1], may need to change!
        knots.list[[nn]] <- data.frame(x=seq(0,1,length=knot.length[nn]))

        index.X <- which(colnames(data.gamm)==paste("X_",nn,sep=""))  ## 4
        colnames(data.gamm)[index.X] <- "X"

        sm <- smoothCon(s(X,k=knot.length[nn],bs="cr"),  ## cubic spline
                        data.gamm,knots=knots.list[[nn]])[[1]]   ## construction of smooth terms in a GAM/ predicion from smooht terms in a GAM


        ## 1st parameters is value of spline at knot location 0.
        ## set it to 0 by dropping...
        XX.tmp <- paste("XX",nn,"_",sep="")

        pos <- 1
        envir = as.environment(pos)
        assign(XX.tmp, sm$X[,-1], envir = envir)    ## spline basis
        #assign(XX.tmp, sm$X[,-1],envir=globalenv())    ## spline basis

        XX.list <- c(XX.list,XX.tmp)

        S.tmp <- sm$S[[1]][-1,-1]   ## spline penalty
        pen.list[[nn]] <- list(S.tmp)

        colnames(data.gamm)[index.X] <- paste("X_",nn,sep="")
      }
      knots.list.full <- appendList(knots.list.full,knots.list)
      pen.list.full <- appendList(pen.list.full,pen.list)
    }
    fmla <- as.formula(paste("Y~-1+",paste(znam.orig,collapse="+"),"+",
                             paste(XX.list,collapse="+")))  # Y ~ -1 + Z0 + Z1_1 + XX1_

    eflag <- 0

    ## for random intercept
    random.formula <- as.formula(paste("~1"))

    ## for random intercept + slope
    ## random.formula <- as.formula(paste("~1+",paste(znam.orig,collapse="+")))
    ## using gamm
    if( sum( abs(data.gamm$Y)<1  & abs(data.gamm$Y)>0 ) > 0   ){
      ## some Y's are fractional
      aout <- tryCatch(
        {
          if(random.effect=="none"){## no random effect
            fm <- gam(fmla,data=data.gamm,paraPen=pen.list.full,   ## data.gamm: study, family, Y, X_1, Z0, Z1_1, np, int
                      family=quasibinomial(link=link.type),
                      verbose=FALSE)#,niterPQL=100)

            fm$gam <- fm
          } else if(random.effect=="event"){  ## random effect for event only
            fm <- gamm(fmla,data=data.gamm,#paraPen=pen.list.full,
                       family=quasibinomial(link=link.type), ## quasi
                       random=list(family=~1),verbosePQL=FALSE,niterPQL=1000)
          } else if(random.effect=="study"){  ## random effect for study only
            if(is.null(par_fu)){
              fm <- gamm(fmla,data=data.gamm,paraPen=pen.list.full,
                         family=quasibinomial(link=link.type), ## quasi
                         random=list(study=~1),verbosePQL=FALSE)#,niterPQL=100)
            } else {
              fm <- gam(fmla,data=data.gamm,paraPen=pen.list.full,
                        family=quasibinomial(link=link.type), ## quasi
                        verbose=FALSE)#,niterPQL=100)
            }
          } else if(random.effect=="studyevent"){  ## random effect for event and study?
            if(is.null(par_fu)){
              ## setup is for random intercept r_si
              fm <- gamm(fmla,data=data.gamm,#paraPen=pen.list.full,
                         family=quasibinomial(link=link.type), ## quasi
                         random=list(int=random.formula),verbosePQL=FALSE)#,niterPQL=100)
            } else {
              ## setup is for random intercept r_si + u_s
              fm <- gamm(fmla,data=data.gamm,paraPen=pen.list.full,
                         family=quasibinomial(link=link.type),  ## quasi
                         random=list(study=~1,family=~1),verbosePQL=FALSE)#,niterPQL=100)
            }
          }
        }, error=function(e){
          cat("ERROR:", conditionMessage(e),"\n")
          eflag=-1
          return(eflag)}
      )
      if(!is.list(aout)){
        eflag <- aout
      }
    } else {
      ## Y's are not fractional
      if(random.effect=="none"){## no random effect
        fm <- gam(fmla,data=data.gamm,paraPen=pen.list.full,   ## parapen: optional list specifying any penalties to be applied to parametric model terms.
                  family=binomial(link=link.type),
                  verbose=FALSE)#,niterPQL=100)
        fm$gam <- fm
      } else if(random.effect=="event"){  ## random effect for event only
        fm <- gamm(fmla,data=data.gamm,paraPen=pen.list.full,
                   family=binomial(link=link.type), ## quasi
                   random=list(family=~1),verbosePQL=FALSE)#,niterPQL=100)
      } else if(random.effect=="study"){  ## random effect for study only
        if(is.null(par_fu)){
          fm <- gamm(fmla,data=data.gamm,paraPen=pen.list.full,
                     family=binomial(link=link.type), ## quasi
                     random=list(study=~1),verbosePQL=FALSE)#,niterPQL=100)
        } else {
          fm <- gam(fmla,data=data.gamm,paraPen=pen.list.full,
                    family=binomial(link=link.type), ## quasi
                    verbose=FALSE)#,niterPQL=100)
        }
      } else if(random.effect=="studyevent"){  ## random effect for event and study?
        if(is.null(par_fu)){
          ## setup is for random intercept r_si ONLY
          fm <- gamm(fmla,data=data.gamm,paraPen=pen.list.full,
                     family=binomial(link=link.type),
                     random=list(int=random.formula),verbosePQL=FALSE)#,niterPQL=100)
        } else {
          ## setup is for random intercept r_si + u_s
          fm <- gamm(fmla,data=data.gamm,paraPen=pen.list.full,
                     family=binomial(link=link.type),
                     random=list(study=~1,family=~1),verbosePQL=FALSE)#,niterPQL=100)
        }
      }
    }
  } else {
    ## no constraint
    xnam.orig <- colnames(data.gamm)[grep(glob2rx("*X*"),colnames(data.gamm))]
    xnam <- paste("s(",xnam.orig,")",sep="")

    fmla <- as.formula(paste("Y~-1+",paste(znam,collapse="+"),"+",paste(xnam,collapse="+")))


    if(random.effect=="event"){

      random.formula <- as.formula(paste("~(1|family)",sep=""))

    } else if(random.effect=="study"){
      random.formula <- as.formula(paste("~(1|study)",sep=""))
    } else if(random.effect=="studyevent"){

      if(is.null(par_fu)){
        ## for partially crossed random effects
        random.formula <- as.formula(paste("~(1|study:family)",sep=""))
      } else {
        ## for random effects: study + study:family
        random.formula <- as.formula(paste("~(1|study/family)",sep=""))
      }

    }

    ## setup is for random intercept ONLY
    if( sum( abs(data.gamm$Y)<1  & abs(data.gamm$Y)>0 ) > 0   ){

      if(random.effect=="none"){  ## no random effect
        ## some Y's are fractional
        fm <- gam(fmla,data=data.gamm,family=quasibinomial(link=link.type)) ##quasi
        fm$gam <- fm
      }else {
        ## some Y's are fractional
        fm <- gamm4(fmla,data=data.gamm,family=quasibinomial(link=link.type), ##quasi
                    random=random.formula)
        # ## PQL fails to converge
        # fm <- gamm(fmla,data=data.gamm,
        #            family=quasibinomial(link=link.type),
        #            random=list(family=~1),verbosePQL=FALSE)

      }
      ## end of if
    } else {
      if(random.effect=="none"){  ## event analyzed separately, no random effect
        fm <- gam(fmla,data=data.gamm,family=binomial)
        fm$gam <- fm
      } else {
        fm <- gamm4(fmla,data=data.gamm,family=binomial(link=link.type),random=random.formula)
      }
    } ## end of else

  }


  if(eflag!=-1){
    ##################################
    ## get beta, alpha, Ft estimate ##
    ##################################

    thetas.est <- theta.prediction(num_study,np,xx=xks,zz.val=NULL,
                                   fm,knot.length,knots.list.full,lb,
                                   xnam.orig,znam.orig,zeval,z.choice,
                                   param.label,beta0int,gamma.param,omega.param,
                                   spline.constrain,common.param.estimation,par_fu,
                                   random.effect,z.proportions)
    beta.est <- thetas.est$beta.est
    beta.var <- thetas.est$beta.var
    alphas.est[,,,la] <- thetas.est$alphas.est
    alphas.var[,,,la] <- thetas.est$alphas.var
    Ft.est <- thetas.est$Ft.est
    Ft.var <- thetas.est$Ft.var
  }


  list(beta.est=beta.est,beta.var=beta.var,
       alphas.est=alphas.est,alphas.var=alphas.var,
       alphas_ij=alphas_ij,alphas_ij_var=alphas_ij_var,
       Ft_ij=Ft_ij,Ft_ij_var=Ft_ij_var,
       Ft.est=Ft.est,Ft.var=Ft.var,eflag=eflag)
}



## function to compute \sum_k F(t|Z=k) * pr(Z=k)

compute.sum.F.times.z <- function(Ft.tmp,z.proportions){
  ##out <- sweep(Ft.tmp,find.apply.index(Ft.tmp,"xx"),
  ##                           z.proportions,FUN="*")
  ##out <- apply.index(out,"zz",sum)
  out <- sum(Ft.tmp * z.proportions)
  return(out)
}


##########################################
## function to predict beta,alpha(x),Ft ##
##########################################
#' @import cubature
#' @import lme4
theta.prediction <- function(num_study,
                             np,
                             xx,
                             zz.val,
                             fm,knot.length,
                             knots.list.full,
                             lb,
                             xnam.orig,znam.orig,
                             zeval,z.choice,
                             param.label,beta0int,
                             gamma.param,omega.param,
                             spline.constrain,
                             common.param.estimation,par_fu,random.effect,
                             z.proportions){

  num_xx <- max(unlist(lapply(xx,length.apply)))

  beta.est <- array(0,dim=c(num_study,np,length(param.label)),
                    dimnames=list(
                      study=paste("ss",1:num_study,sep=""),
                      event=paste("np",1:np,sep=""),
                      theta=param.label))
  beta.var <- beta.est

  alphas.est <- array(0,dim=c(num_study,np,num_xx),
                      dimnames=list(
                        study=paste("ss",1:num_study,sep=""),
                        event=paste("np",1:np,sep=""),
                        xx=paste("xx",1:num_xx,sep="")))

  alphas.var <- alphas.est

  if(is.null(z.choice)){
    Ft.est <- array(0,dim=c(num_study,np,num_xx),
                    dimnames=list(
                      study=paste("ss",1:num_study,sep=""),
                      event=paste("np",1:np,sep=""),
                      xx=paste("xx",1:num_xx,sep="")))
  } else {
    Ft.est <- array(0,dim=c(num_study,np,z.choice,num_xx),
                    dimnames=list(
                      study=paste("ss",1:num_study,sep=""),
                      event=paste("np",1:np,sep=""),
                      zz=paste("zz",1:z.choice,sep=""),
                      xx=paste("xx",1:num_xx,sep="")))
  }
  dFt.est <- Ft.est

  Ft.var <- Ft.est

  coef.names <- names(fm$gam$coefficients)

  if(random.effect!="none"){ ## we have random effects
    ## standard devation for random intercept only
    if(spline.constrain==TRUE){
      if(is.null(par_fu)){
        sigmar <- as.numeric(VarCorr(fm$lme)["(Intercept)","StdDev"])
        sigmau <- NULL
      } else {
        tmp.sigma <- as.numeric(VarCorr(fm$lme)[,"StdDev"])
        tmp.sigma <- tmp.sigma[!is.na(tmp.sigma)][1:2]
        sigmau <- tmp.sigma[1]
        sigmar <- tmp.sigma[2]
      }
    } else {
      mycor <- data.frame(VarCorr(fm$mer),row.names=1)
      sigmar <- mycor["study:family","sdcor"]
      if(is.null(par_fu)){
        sigmau <- NULL
      } else {
        sigmau <- mycor["study","sdcor"]
      }
    }
  }

  if(spline.constrain==FALSE){
    ## won't work with xx=xks as a list!!
    newdata <- data.frame(cbind(matrix(rep(1,length(xx)*length(znam.orig)),
                                       ncol=length(znam.orig)),
                                matrix(rep(xx,length(xnam.orig)),ncol=length(xnam.orig))))
    colnames(newdata) <- c(znam.orig,xnam.orig)
    my.predict <- predict(fm$gam,newdata,type="terms",se.fit=TRUE) ## directly gets \wh alpha(x)
    my.predict2 <- predict(fm$gam,newdata,type="lpmatrix") ## used to form \wh alpha(x)=a* B(x)
  }

  if(!is.null(beta0int)){
    ## for intercept
    index.use.int <- which(coef.names=="Z0")
    beta.est[,,"beta0"] <- summary(fm$gam)$p.coef[index.use.int]
    beta.var[,,"beta0"] <- summary(fm$gam)$se[index.use.int]^2
  } else {
    index.use.int <- NULL
  }

  for(ss in 1:num_study){
    for(nn in 1:np){

      ## get gamma estimate
      if(!is.null(gamma.param) && nn!=1){
        gamma.tmp <- paste("gamma_",nn,"_Z",sep="")
        index.use.gamma <- grep(glob2rx(gamma.tmp),coef.names)
        beta.est[ss,nn,"gamma"] <- summary(fm$gam)$p.coef[index.use.gamma]
        beta.var[ss,nn,"gamma"] <- summary(fm$gam)$se[index.use.gamma]^2
      } else {
        index.use.gamma <- NULL
      }

      ## get omega estimate
      if(!is.null(omega.param) && ss!=1){
        omega.tmp <- paste("study_",ss,"_omega_Z",sep="")
        index.use.omega <- grep(glob2rx(omega.tmp),coef.names)
        beta.est[ss,nn,"omega"] <- summary(fm$gam)$p.coef[index.use.omega]
        beta.var[ss,nn,"omega"] <- summary(fm$gam)$se[index.use.omega]^2
      } else {
        index.use.omega <- NULL
      }

      ## get other beta estimates
      beta.index <- paste("beta",1:lb[[ss]][[nn]],sep="")
      if(common.param.estimation==FALSE){ # no common beta, alpha
        ZZ.tmp <- paste("study_",ss,"_Z",nn,"_*",sep="")
      } else { # common beta, alpha
        ZZ.tmp <- paste("Z",nn,"_*",sep="")
      }
      index.use.z <- grep(glob2rx(ZZ.tmp),coef.names)

      ## get beta estimate
      beta.est[ss,nn,beta.index] <- summary(fm$gam)$p.coef[index.use.z]
      beta.var[ss,nn,beta.index] <- summary(fm$gam)$se[index.use.z]^2

      ## get alpha estimates
      if(spline.constrain==TRUE){
        if(common.param.estimation==FALSE){ # no common beta, alpha
          XX.tmp <- paste("study_",ss,"_XX",nn,"_*",sep="")
        } else { # common beta, alpha
          XX.tmp <- paste("XX",nn,"_*",sep="")
        }
        index.use.x <- grep(glob2rx(XX.tmp),coef.names)

        if(common.param.estimation==FALSE){ # no common beta, alpha
          index.knots.use <-
            which(names(knots.list.full)==paste("study_",ss,"_nn",nn,sep=""))
        } else { # common beta, alpha
          index.knots.use <- which(names(knots.list.full)==paste("nn",nn,sep=""))
        }
        xx.tmp <- xx[[ss]] ## xx=xks
        sm.predict <- smoothCon(s(xx.tmp,k=knot.length[nn],bs="cr"),
                                data.frame(xx.tmp),knots=knots.list.full[[index.knots.use]])[[1]]

        ##Wrapper functions for construction of and prediction from smooth terms in a GAM.
        ## The purpose of the wrappers is to allow user-transparant re-parameterization of smooth terms,
        ## in order to allow identifiability constraints to be absorbed into the parameterization of each term,
        ##smoothCon returns a list of smooths because factor by variables result in multiple copies of a smooth,
        ## each multiplied by the dummy variable associated with one factor level
        xx.spline <- sm.predict$X[,-1] ##  model matrix of X
      } else {
        ## unconstrained
        if(common.param.estimation==FALSE){ # no common beta, alpha
          XX.tmp <- paste("s(study_",ss,"_X_",nn,").*",sep="")
        } else { # common beta, alpha
          XX.tmp <- paste("s(X_",nn,").*",sep="")
        }

        index.use.x <- grep(glob2rx(XX.tmp),coef.names)
        xx.spline <- my.predict2[,index.use.x]
      }

      alphas.est[ss,nn,1:length(xx[[ss]])] <- as.numeric(xx.spline %*%
                                                           fm$gam$coefficients[index.use.x]) 	## the coefficients of the fitted model.
      if(spline.constrain==FALSE){
        ## adjust for identifiability constraint
        alphas.est[ss,nn,1:length(xx[[ss]])] <- alphas.est[ss,nn,1:length(xx[[ss]])] +
          mean(alphas.est[ss,nn,1:length(xx[[ss]])])
      }

      alphas.var.tmp  <- xx.spline %*% fm$gam$Vp[index.use.x,index.use.x] %*% t(xx.spline)  ## inference of alpha(x)
      alphas.var[ss,nn,1:length(xx[[ss]])] <- diag( alphas.var.tmp )


      index.use <- c(index.use.int,index.use.gamma,index.use.omega,index.use.z,index.use.x)

      ## get Ft estimates

      if(!is.null(z.choice)){
        for(kk in 1:z.choice){
          zeval.tmp <- as.matrix(zeval[ss,kk,nn,1:lb[[ss]][[nn]]])
          betaz <- rep(beta.est[ss,nn,beta.index] %*% zeval.tmp,
                       length(alphas.est[ss,nn,1:length(xx[[ss]])]))
          ## for intercept
          if(!is.null(beta0int)){
            betaz <- betaz + beta.est[1,1,"beta0"]
          }

          ## for gamma parameter
          if(!is.null(gamma.param) && nn!=1){
            betaz <- betaz + beta.est[ss,nn,"gamma"]
          }

          ## for omega parameter
          if(!is.null(omega.param) && ss!=1){
            betaz <- betaz + beta.est[ss,nn,"omega"]
          }

          for(jj in 1:length(xx[[ss]])){
            if(random.effect=="none"){ ## event analyzed separately, no random effects
              rval.tmp <- 0

              ## avg is stored in the last z.choice value
              if(kk==z.choice & !is.null(z.proportions)){
                Ft.est[ss,nn,kk,jj] <- compute.sum.F.times.z(Ft.est[ss,nn,1:(z.choice-1),jj],
                                                             z.proportions[ss,nn,])

                dFt.est[ss,nn,kk,jj] <- compute.sum.F.times.z(dFt.est[ss,nn,1:(z.choice-1),jj],
                                                              z.proportions[ss,nn,])
              } else {
                ## compute F(t|z=k)
                Ft.est[ss,nn,kk,jj] <- Ft.true.value(betaz=betaz[jj],
                                                     alphastmp=alphas.est[ss,nn,jj],
                                                     rval=rval.tmp)

                dFt.est[ss,nn,kk,jj] <- dFt.value.integrand(betaz=betaz[jj],
                                                            alphastmp=alphas.est[ss,nn,jj],
                                                            rval.eval=rval.tmp)
              }

            } else { ## we have random effects

              if(kk==z.choice & !is.null(z.proportions)){
                ## avg is stored in the last z.choice value
                Ft.est[ss,nn,kk,jj] <- compute.sum.F.times.z(Ft.est[ss,nn,1:(z.choice-1),jj],
                                                             z.proportions[ss,nn,])

                dFt.est[ss,nn,kk,jj] <- compute.sum.F.times.z(dFt.est[ss,nn,1:(z.choice-1),jj],
                                                              z.proportions[ss,nn,])
              } else {
                ## compute F(t|z=k)
                Ft.integrate <- make.Ft.integrate(alphax=alphas.est[ss,nn,jj],
                                                  betaz[jj],sigmar,sigmau)
                dFt.integrate <- make.dFt.integrate(alphax=alphas.est[ss,nn,jj],
                                                    betaz[jj],sigmar,sigmau)
                sigma_all <- c(sigmar,sigmau)

                if(is.null(sigmau)){

                  Ftout <- tryCatch(
                    { integrate(Ft.integrate,-Inf,Inf)$value
                    },   error=function(e){
                      cat("ERROR:", conditionMessage(e),"\n")
                      out <- 0
                      return(out)
                    })
                  Ft.est[ss,nn,kk,jj] <- Ftout

                  dFtout <- tryCatch(
                    { integrate(dFt.integrate,-Inf,Inf)$value
                    }, error=function(e){
                      cat("ERROR:", conditionMessage(e),"\n")
                      out <- 0
                      return(out)
                    })
                  dFt.est[ss,nn,kk,jj] <- dFtout
                } else {
                  Ft.est[ss,nn,kk,jj] <-
                    adaptIntegrate(Ft.integrate,
                                   lowerLimit=rep(-1,length(sigma_all)),
                                   upperLimit=rep(1,length(sigma_all)))$integral
                  dFt.est[ss,nn,kk,jj] <-
                    adaptIntegrate(dFt.integrate,
                                   lowerLimit=rep(-1,length(sigma_all)),
                                   upperLimit=rep(1,length(sigma_all)))$integral
                }
              }

            }
          }

          xx.eval.tmp <- zeval.tmp
          if(!is.null(beta0int)){
            xx.eval.tmp <- rbind(1,xx.eval.tmp)
          }

          if(!is.null(gamma.param)){
            if(!is.null(beta0int)){
              if(nn!=1){
                xx.eval.tmp <- rbind(1,xx.eval.tmp)
              }
            } else {
              xx.eval.tmp <- rbind(1,xx.eval.tmp)
            }
          }

          if(!is.null(omega.param)){
            if(!is.null(beta0int)){
              if(ss!=1){
                xx.eval.tmp <- rbind(1,xx.eval.tmp)
              }
            } else {
              xx.eval.tmp <- rbind(1,xx.eval.tmp)
            }
          }

          xx.eval.tmp <- t(xx.eval.tmp)


          ## asymptotic variance of Ft.var
          g.tmp <- cbind(dFt.value.new(dFt.est[ss,nn,kk,1:length(xx[[ss]])],
                                       xx.eval=xx.eval.tmp),
                         dFt.value.new(dFt.est[ss,nn,kk,1:length(xx[[ss]])],xx.eval=xx.spline))

          #g.tmp <- cbind(dFt.value(betaz=betaz,
          #	 alphastmp=alphas.est[ss,nn,length(xx[[ss]])],rval.eval=rval.tmp,
          #                     xx.eval=t(zeval.tmp)),
          #              dFt.value(betaz=betaz,
          #			alphastmp=alphas.est[ss,nn,length(xx[[ss]])],rval.eval=rval.tmp,
          #                     xx.eval=xx.spline))

          Ft.var[ss,nn,kk,1:length(xx[[ss]])] <- diag( g.tmp %*%
                                                         fm$gam$Vp[index.use,index.use] %*% t(g.tmp))
        }
      }
    }
  }

  list(beta.est=beta.est,beta.var=beta.var,alphas.est=alphas.est,alphas.var=alphas.var,
       Ft.est=Ft.est,Ft.var=Ft.var)
}







#' @import stats
#' @import mnormt
make.Ft.integrate <- function(alphax,betaz,sigmar,sigmau){
  if(!is.null(sigmau)){
    Sigma <- diag(c(sigmar^2,sigmau^2))
    mean_use <- rep(0,2)
  } else {
    Sigma <- sigmar^2
    mean_use <- 0
  }

  function(r){
    if(is.null(sigmau)){
      ## 1-dimensional
      out <- Ft.true.value(betaz,alphax,r) * stats::dnorm(r,mean=0,sd=sigmar)
    } else {
      ## any dimension
      out <- Ft.true.value(betaz,alphax,sum(r/(1-r^2))) * mnormt::dmnorm(r/(1-r^2),mean_use,Sigma) *
        ((1+r^2)/(1-r^2)^2)
    }
    return(out)
  }
}

#' @import stats
#' @import mnormt
make.dFt.integrate <- function(alphax,betaz,sigmar,sigmau){
  if(!is.null(sigmau)){
    Sigma <- diag(c(sigmar^2,sigmau^2))
    mean_use <- rep(0,2)
  } else {
    Sigma <- sigmar^2
    mean_use <- 0
  }

  function(r){
    if(is.null(sigmau)){
      ## 1-dimensional
      out <- dFt.value.integrand(betaz,alphax,r) * stats::dnorm(r,mean=0,sd=sigmar)
    } else {
      ## any dimension
      out <- dFt.value.integrand(betaz,alphax,sum(r/(1-r^2))) * mnormt::dmnorm(r/(1-r^2),mean_use,Sigma) *
        ((1+r^2)/(1-r^2)^2)
    }
    return(out)
  }
}


dFt.value.new <- function(betaz,xx.eval){
  out <- matrix(0,ncol=ncol(xx.eval),nrow=length(betaz))

  for(kk in 1:ncol(xx.eval)){
    out[,kk] <- betaz * xx.eval[,kk]
  }
  return(out)
}


#######################################
## ulc: F(t) and inference of alphas and Ft
#######################################
## expit function

expit <- function(x){
  1/(1+exp(-x))
}



## F(t)=exp(a)/ (1+exp(a)) = 1/ (1+exp(-a))

Ft.true.value <- function(betaz,alphastmp,rval){
  rval.use <- rval

  total.sum <- as.vector(betaz + alphastmp) + rval.use
  den <- 1+exp(-total.sum)

  out <- length(rval.use)

  for(kk in 1:length(rval.use)){
    if(abs(den[kk])< 1e-6 | abs(den[kk])>=Inf){
      out[kk] <- 0
    } else {
      out[kk] <- expit(total.sum[kk])
    }
  }
  return(out)
}


dFt.value.integrand <- function(betaz,alphastmp,rval.eval){
  rval.use <- rval.eval

  total.sum <- as.vector(betaz + alphastmp) + rval.use
  den <- (1+exp(-total.sum))^2

  out <- length(rval.use)

  for(kk in 1:length(rval.use)){
    if(abs(den[kk])< 1e-6 | abs(den[kk])>=Inf){
      out[kk] <- 0
    } else {
      out[kk] <- exp(-total.sum[kk])  / den[kk]
    }
  }
  return(out)
}


dFt.value <- function(betaz,alphastmp,rval.eval,xx.eval){
  out <- matrix(0,ncol=ncol(xx.eval),nrow=length(betaz))

  total.sum <- betaz + alphastmp + rval.eval
  for(kk in 1:ncol(xx.eval)){
    out[,kk] <- exp(-total.sum) * xx.eval[,kk] / (1+exp(-total.sum))^2
  }
  return(out)
}





########################
## main part of gamm4 ##
########################



#' @import utils
#' @importFrom abind adrop
#' @import stats
gamm4.main <- function(num_study,
                       np,n,nmax,m,maxm,
                       ynew,
                       ymiss_ind, ## unobserved Y
                       z,x,
                       time_val,
                       time_choice.predicted,time_choice.predicted.toadd, ##null
                       lb,la,xks,truth,
                       num_time, ## 13
                       num_xx,  ## 18
                       knot.length, ## 8
                       real_data,
                       family.data,
                       zeval, ## k-means
                       z.choice, ## 4
                       param.label,
                       beta0int, ## 0.5
                       gamma.param,omega.param,  ## Null
                       spline.constrain, ## TRUE
                       common.param.estimation, ## TRUE
                       par_fu,
                       analyze.separately, ## "event"
                       link.type, ## "logit"
                       null.theta,
                       z.proportions){ ## null

  ## get null theta
  betaest <- null.theta$null.theta.est.ci$beta
  alphasest <- null.theta$null.theta.est.ci$alpha
  Ftest <- null.theta$null.theta.est.ci$Ft
  Ftest.predicted <- null.theta$null.theta.est.ci$Ft.predicted

  ## Not used
  alphasijest <- NULL
  alphasijvar <- alphasijest
  Ftijest <- NULL
  Ftijvar <- Ftijest


  ## obtain dataset when y=ynew
  data.org.gamm <- make.data.set(num_study,time_val,n,nmax,
                                 m,lb,ynew,x,z,real_data,
                                 gamma.param,omega.param)

  ############################
  ## Organize data for GAMM ##
  ############################


  ## just for testing
  ##if(1==2){

  for(tt in 1:length(time_val)){
    ##print(tt)
    ###########################################
    ## organize data for a particular time t ##
    ###########################################
    data.gamm <- get.data.gamm(n,m,num_study,tt,
                               study=data.org.gamm$study, ## 1,2,3 for each study
                               family=data.org.gamm$family, ## counting the number of sample size for each study
                               y=data.org.gamm$yy,
                               new.x=data.org.gamm$new.x, ## normalized CAG
                               z.values=data.org.gamm$z.values, ## ages at baseline
                               gamma.list=data.org.gamm$gamma.list, ## null list
                               omega.list=data.org.gamm$omega.list, ## null list
                               beta0int, ## 0.5
                               gamma.param,omega.param, ## null
                               common.param.estimation)  ## data frame for gamm4

    ## when analyze.separately!="none", then common.param.estimation=FALSE
    if(analyze.separately=="studyevent"){
      ## no random effects: each study and each event analyzed separately
      ## adjust param.label just to run the code.

      ## Design matrix only has intercept plus beta*z terms and x-covariate
      param.label.tmp <- param.label[grep(glob2rx("beta*"),param.label)]	   ## glob2rx:  to turn wildcard matches into regular expressions.
      index.beta.use <- param.label.tmp                                        ## "beta" appears at least once  in param.label

      if(!is.null(beta0int)|| !is.null(gamma.param) || !is.null(omega.param)){
        param.label.tmp <- c("beta0",param.label.tmp)
        param.label.tmp <- unique(param.label.tmp)

        if(is.null(beta0int)){
          index.beta.use <- c(param.label[1],index.beta.use)
          index.beta.use <- unique(index.beta.use)
        }
      }

      index.beta.use <- param.label.tmp

      for(ss in 1:num_study){
        ##print(ss)
        for(kk in 1:np){ ## np: number of clinical events
          ##print(kk)
          ## get subset of data
          index.use <- which(data.gamm$study==ss & data.gamm$np==kk)
          data.gamm.use <- data.gamm[index.use,]  ## obtain data for gamm4

          ## get appropriate columns
          names.X <- paste("study_",ss,"_X_",kk,sep="")
          names.Z <- paste("study_",ss,"_Z",kk,"_*",sep="")

          get.index.X <- grep(glob2rx(names.X), colnames(data.gamm.use))
          get.index.Z <- grep(glob2rx(names.Z), colnames(data.gamm.use))

          ## get appropriate columns
          data.gamm.use2 <- data.gamm.use[,c("study","family",
                                             "Y","np","int")]  ## obtain subset of data.gamm.use

          if(!is.null(beta0int) || !is.null(gamma.param) || !is.null(omega.param)){
            data.gamm.use2 <- cbind(data.gamm.use2,"Z0"=1)
          }

          data.gamm.use2 <- cbind(data.gamm.use2,data.gamm.use[,get.index.X])  ##  cbind(study family Y np   int Z0)

          if(length(get.index.X)==1){
            colnames(data.gamm.use2)[length(colnames(data.gamm.use2))] <-
              colnames(data.gamm.use)[get.index.X]
          }
          data.gamm.use2 <- cbind(data.gamm.use2,data.gamm.use[,get.index.Z])
          if(length(get.index.Z)==1){
            colnames(data.gamm.use2)[length(colnames(data.gamm.use2))] <-
              colnames(data.gamm.use)[get.index.Z]
          }

          ## make study index 1
          mynames <- colnames(data.gamm.use2)
          mynames.index <- which(substring(mynames,1,7)==paste("study_",ss,sep=""))
          colnames(data.gamm.use2)[mynames.index] <-
            gsub(paste("study_",ss,"_",sep=""),"study_1_",mynames[mynames.index]) ## gsub(old, new, string)

          ## make event index 1
          mynames <- colnames(data.gamm.use2)
          mynames.index <- which(substring(mynames,9,11)==paste("X_",kk,sep=""))
          colnames(data.gamm.use2)[mynames.index] <-
            gsub(paste("X_",kk,sep=""),"X_1",mynames[mynames.index])

          mynames <- colnames(data.gamm.use2)
          mynames.index <- which(substring(mynames,9,10)==paste("Z",kk,sep=""))
          colnames(data.gamm.use2)[mynames.index] <-
            gsub(paste("Z",kk,sep=""),"Z1",mynames[mynames.index])


          ## run the new code
          gamm.fit <- gamm.mle.new(num_study=1,np=1,
                                   lb=list(study1=lb[[ss]]),
                                   num_xx,xks[ss],
                                   data.gamm.use2,  # study family Y np   int Z0
                                   knot.length[kk],zeval[ss,,kk,,drop=FALSE],
                                   z.choice,real_data,
                                   param.label=param.label.tmp,
                                   beta0int=1,gamma.param=NULL,omega.param=NULL,
                                   ## feeding in only intercept model
                                   spline.constrain,common.param.estimation,
                                   par_fu,random.effect="none",
                                   link.type,
                                   z.proportions,la)
          eflag <- gamm.fit$eflag

          if(eflag==-1) break

          if(eflag!=-1){
            ###################
            ## store results ##
            ###################
            betaest[ss,kk,tt,index.beta.use,"est"] <- gamm.fit$beta.est
            betaest[ss,kk,tt,index.beta.use,"varest"] <- gamm.fit$beta.var
            alphasest[ss,kk,,tt,,"est"] <- gamm.fit$alphas.est
            alphasest[ss,kk,,tt,,"varest"] <- gamm.fit$alphas.var
            Ftest[ss,kk,,,tt,"est"] <- gamm.fit$Ft.est
            Ftest[ss,kk,,,tt,"varest"] <- gamm.fit$Ft.var
          }
        }
      }
    } else if(analyze.separately=="study"){

      ## adjust param.label just to run the code.
      ## Design matrix only has intercept plus gamma plus beta*z terms and x-covariate
      param.label.tmp <- param.label[grep(glob2rx("beta*"),param.label)]
      index.beta.use <- param.label.tmp

      if(!is.null(gamma.param)){
        param.label.tmp <- c("gamma",param.label.tmp)
        param.label.tmp <- unique(param.label.tmp)
      }

      index.beta.use <- param.label.tmp

      for(ss in 1:num_study){
        ## get subset of data
        index.use <- which(data.gamm$study==ss)
        data.gamm.use <- data.gamm[index.use,]

        ## get appropriate columns
        names.X <- paste("study_",ss,"_X_*",sep="")
        names.Z <- paste("study_",ss,"_Z*",sep="")
        names.gamma <- "gamma*"

        get.index.X <- grep(glob2rx(names.X), colnames(data.gamm.use))
        get.index.Z <- grep(glob2rx(names.Z), colnames(data.gamm.use))
        get.index.gamma <- grep(glob2rx(names.gamma), colnames(data.gamm.use))

        ## get appropriate columns
        data.gamm.use2 <- data.gamm.use[,c("study","family",
                                           "Y","np","int")]

        if(!is.null(beta0int)){
          data.gamm.use2 <- cbind(data.gamm.use2,"Z0"=1)
        }


        data.gamm.use2 <- cbind(data.gamm.use2,data.gamm.use[,get.index.X])
        if(length(get.index.X)==1){
          colnames(data.gamm.use2)[length(colnames(data.gamm.use2))] <-
            colnames(data.gamm.use)[get.index.X]
        }

        data.gamm.use2 <- cbind(data.gamm.use2,data.gamm.use[,get.index.Z])
        if(length(get.index.Z)==1){
          colnames(data.gamm.use2)[length(colnames(data.gamm.use2))] <-
            colnames(data.gamm.use)[get.index.Z]
        }

        data.gamm.use2 <- cbind(data.gamm.use2,data.gamm.use[,get.index.gamma])

        mynames <- colnames(data.gamm.use2)
        mynames.index <- which(substring(mynames,1,7)==paste("study_",ss,sep=""))
        colnames(data.gamm.use2)[mynames.index] <-
          gsub(paste("study_",ss,"_",sep=""),"study_1_",mynames[mynames.index])

        tmp.col <- length(colnames(data.gamm.use2))-length(get.index.gamma)
        colnames(data.gamm.use2)[(tmp.col+1):length(colnames(data.gamm.use2))] <-
          colnames(data.gamm.use)[get.index.gamma]

        ## run the new code
        gamm.fit <- gamm.mle.new(num_study=1,np=np,
                                 lb=list(study1=lb[[ss]]),
                                 num_xx,xks[ss],
                                 data.gamm.use2,
                                 knot.length,zeval[ss,,,,drop=FALSE],
                                 z.choice,real_data,
                                 param.label=param.label.tmp,
                                 beta0int=beta0int,gamma.param=gamma.param,
                                 omega.param=NULL,
                                 spline.constrain,common.param.estimation,
                                 par_fu,random.effect="event",link.type,
                                 z.proportions,la)
        eflag <- gamm.fit$eflag

        if(eflag==-1) break

        if(eflag!=-1){
          ###################
          ## store results ##
          ###################
          betaest[ss,,tt,index.beta.use,"est"] <- gamm.fit$beta.est
          betaest[ss,,tt,index.beta.use,"varest"] <- gamm.fit$beta.var
          alphasest[ss,,,tt,,"est"] <- gamm.fit$alphas.est
          alphasest[ss,,,tt,,"varest"] <- gamm.fit$alphas.var
          Ftest[ss,,,,tt,"est"] <- gamm.fit$Ft.est
          Ftest[ss,,,,tt,"varest"] <- gamm.fit$Ft.var
        }
      }
    } else if(analyze.separately=="event"){

      ## adjust param.label just to run the code.
      ## Design matrix only has intercept plus omega plus beta*z terms and x-covariate
      param.label.tmp <- param.label[grep(glob2rx("beta*"),param.label)]
      index.beta.use <- param.label.tmp

      if(!is.null(omega.param) && common.param.estimation ==FALSE){
        param.label.tmp <- c("omega",param.label.tmp)
        param.label.tmp <- unique(param.label.tmp)
      }

      index.beta.use <- param.label.tmp
      if(common.param.estimation==TRUE){
        num_study.use <- 1
        xks.use <- xks[1] ## study ==cohort
        lb.use <- list(study1=lb[[1]])
        omega.param.use <- NULL
      } else {
        num_study.use <- num_study
        xks.use <- xks
        lb.use <- lb
        omega.param.use <- omega.param
      }


      for(kk in 1:np){
        ## get subset of data
        index.use <- which(data.gamm$np==kk)
        data.gamm.use <- data.gamm[index.use,]

        ## get appropriate columns
        names.X <- paste("*X_",kk,sep="")
        names.Z <- paste("*Z",kk,"_*",sep="")
        names.omega <- "*omega*"

        get.index.X <- grep(glob2rx(names.X),colnames(data.gamm.use))
        get.index.Z <- grep(glob2rx(names.Z),colnames(data.gamm.use))
        get.index.omega <- grep(glob2rx(names.omega),colnames(data.gamm.use))

        ## get appropriate columns
        data.gamm.use2 <- data.gamm.use[,c("study","family",
                                           "Y","np","int")]

        if(!is.null(beta0int)){
          data.gamm.use2 <- cbind(data.gamm.use2,"Z0"=1)
        }

        data.gamm.use2 <- cbind(data.gamm.use2,data.gamm.use[,get.index.X])
        if(length(get.index.X)==1){
          colnames(data.gamm.use2)[length(colnames(data.gamm.use2))] <-
            colnames(data.gamm.use)[get.index.X]
        }

        data.gamm.use2 <- cbind(data.gamm.use2,data.gamm.use[,get.index.Z])
        if(length(get.index.Z)==1){
          colnames(data.gamm.use2)[length(colnames(data.gamm.use2))] <-
            colnames(data.gamm.use)[get.index.Z]
        }

        data.gamm.use2 <- cbind(data.gamm.use2,data.gamm.use[,get.index.omega])

        mynames <- colnames(data.gamm.use2)
        if(common.param.estimation==FALSE){
          ## make event index 1
          mynames.index <- which(substring(mynames,9,11)==paste("X_",kk,sep=""))
          colnames(data.gamm.use2)[mynames.index] <-
            gsub(paste("X_",kk,sep=""),"X_1",mynames[mynames.index])

          mynames <- colnames(data.gamm.use2)
          mynames.index <- which(substring(mynames,9,10)==paste("Z",kk,sep=""))
          colnames(data.gamm.use2)[mynames.index] <-
            gsub(paste("Z",kk,sep=""),"Z1",mynames[mynames.index])
        } else {
          ## make event index 1
          mynames.index <- which(substring(mynames,1,3)==paste("X_",kk,sep=""))
          colnames(data.gamm.use2)[mynames.index] <-
            gsub(paste("X_",kk,sep=""),"X_1",mynames[mynames.index])

          mynames <- colnames(data.gamm.use2)
          mynames.index <- which(substring(mynames,1,2)==paste("Z",kk,sep=""))
          colnames(data.gamm.use2)[mynames.index] <-
            gsub(paste("Z",kk,sep=""),"Z1",mynames[mynames.index])
        }

        if(sum(get.index.omega)>0){
          tmp.col <- length(colnames(data.gamm.use2))-length(get.index.omega)

          colnames(data.gamm.use2)[(tmp.col+1):length(colnames(data.gamm.use2))] <-
            colnames(data.gamm.use)[get.index.omega]
        }


        ## run the new code
        gamm.fit <- gamm.mle.new(num_study=num_study.use,np=1,
                                 lb=lb.use,num_xx,xks=xks.use,
                                 data.gamm.use2,
                                 knot.length[kk],zeval[,,kk,,drop=FALSE],
                                 z.choice,real_data,
                                 param.label=param.label.tmp,
                                 beta0int=beta0int,gamma.param=NULL,omega.param=omega.param.use,
                                 spline.constrain,common.param.estimation,
                                 par_fu,random.effect="none",link.type,
                                 z.proportions,la)
        eflag <- gamm.fit$eflag

        if(eflag==-1) break

        if(eflag!=-1){
          ###################
          ## store results ##
          ###################
          if(common.param.estimation==TRUE){
            for(ss in 1:num_study){
              betaest[ss,kk,tt,index.beta.use,"est"] <- gamm.fit$beta.est
              betaest[ss,kk,tt,index.beta.use,"varest"] <- gamm.fit$beta.var
              alphasest[ss,kk,,tt,,"est"] <- gamm.fit$alphas.est
              alphasest[ss,kk,,tt,,"varest"] <- gamm.fit$alphas.var
              Ftest[ss,kk,,,tt,"est"] <- gamm.fit$Ft.est
              Ftest[ss,kk,,,tt,"varest"] <- gamm.fit$Ft.var
            }
          } else {
            betaest[,kk,tt,index.beta.use,"est"] <- gamm.fit$beta.est
            betaest[,kk,tt,index.beta.use,"varest"] <- gamm.fit$beta.var
            alphasest[,kk,,tt,,"est"] <- gamm.fit$alphas.est
            alphasest[,kk,,tt,,"varest"] <- gamm.fit$alphas.var
            Ftest[,kk,,,tt,"est"] <- gamm.fit$Ft.est
            Ftest[,kk,,,tt,"varest"] <- gamm.fit$Ft.var
          }
        }
      }
    } else if(analyze.separately=="none"){  ## we analyze all studies and events together
      ##########################
      ## apply GAMM procedure ##
      ##########################

      if(common.param.estimation==TRUE){
        num_study.use <- 1
      } else {
        num_study.use <- num_study
      }

      gamm.fit <- gamm.mle.new(num_study=num_study.use,np,lb,num_xx,xks,
                               data.gamm,
                               knot.length,zeval,z.choice,
                               real_data,
                               param.label,beta0int,gamma.param,omega.param,
                               spline.constrain,common.param.estimation,
                               par_fu,random.effect="studyevent",link.type,
                               z.proportions,la)
      eflag <- gamm.fit$eflag

      if(eflag==-1) break

      if(eflag!=-1){
        ###################
        ## store results ##
        ###################
        if(common.param.estimation==TRUE){
          for(ss in 1:num_study){
            betaest[ss,,tt,,"est"] <- gamm.fit$beta.est
            betaest[ss,,tt,,"varest"] <- gamm.fit$beta.var
            alphasest[ss,,,tt,,"est"] <- gamm.fit$alphas.est
            alphasest[ss,,,tt,,"varest"] <- gamm.fit$alphas.var
            Ftest[ss,,,,tt,"est"] <- gamm.fit$Ft.est
            Ftest[ss,,,,tt,"varest"] <- gamm.fit$Ft.var
          }
        } else {
          betaest[,,tt,,"est"] <- gamm.fit$beta.est
          betaest[,,tt,,"varest"] <- gamm.fit$beta.var
          alphasest[,,,tt,,"est"] <- gamm.fit$alphas.est
          alphasest[,,,tt,,"varest"] <- gamm.fit$alphas.var
          Ftest[,,,,tt,"est"] <- gamm.fit$Ft.est
          Ftest[,,,,tt,"varest"] <- gamm.fit$Ft.var
        }
      }
    }
  }



  ##} ## end just for testing
  ##eflag <- 1  ## just for testing

  ########################################
  ## Add lo and hi confidence intervals ##
  ########################################
  betaest[,,,,"varhi"] <- adrop(betaest[,,,,"est",drop=FALSE],drop=5) +
    qnorm(0.975)*sqrt(adrop(betaest[,,,,"varest",drop=FALSE],drop=5))
  betaest[,,,,"varlo"] <- adrop(betaest[,,,,"est",drop=FALSE],drop=5) +
    qnorm(0.025)*sqrt(adrop(betaest[,,,,"varest",drop=FALSE],drop=5))

  alphasest[,,,,,"varhi"] <- adrop(alphasest[,,,,,"est",drop=FALSE],drop=6) +
    qnorm(0.975)*sqrt(adrop(alphasest[,,,,,"varest",drop=FALSE],drop=6))
  alphasest[,,,,,"varlo"] <- adrop(alphasest[,,,,,"est",drop=FALSE],drop=6) +
    qnorm(0.025)*sqrt(adrop(alphasest[,,,,,"varest",drop=FALSE],drop=6))

  Ftest[,,,,,"varhi"] <- adrop(Ftest[,,,,,"est",drop=FALSE],drop=6) +
    qnorm(0.975)*sqrt(adrop(Ftest[,,,,,"varest",drop=FALSE],drop=6))
  Ftest[,,,,,"varlo"] <- adrop(Ftest[,,,,,"est",drop=FALSE],drop=6) +
    qnorm(0.025)*sqrt(adrop(Ftest[,,,,,"varest",drop=FALSE],drop=6))

  ## make Ft monotone


  mono.Ftest <-  apply.function.index(
    Ftest[,,,,,c("est","varlo","varhi"),drop=FALSE],
    dim.fun=c("time","xx"),getF,
    p=length(dimnames(Ftest)[["xx"]]))

  Ftest[,,,,,c("est","varlo","varhi")]   <-
    mono.Ftest[,,,,,c("est","varlo","varhi"),drop=FALSE]

  if(any(is.na(Ftest[,,,,,c("est","varlo","varhi"),drop=FALSE]))){
    eflag <- -1
    print("Ftest has NA's")
  }



  ## get predicted values: Pr(T<t|t>t0)
  if(!is.null(time_choice.predicted)){
    ## extract Ft information
    for(tt in 1:length(time_choice.predicted)){
      for(tt0 in 1:length(time_choice.predicted.toadd)){
        time.use <- paste("t",time_choice.predicted[tt]+
                            time_choice.predicted.toadd[tt0],sep="")
        if(time.use %in% dimnames(mono.Ftest)$time){

          Ftest.predicted[,,,,tt,tt0,"est"] <-
            (adrop(mono.Ftest[,,,,paste("t",time_choice.predicted[tt]+
                                          time_choice.predicted.toadd[tt0],sep=""),"est",drop=FALSE],drop=5) -
               adrop(mono.Ftest[,,,,paste("t",time_choice.predicted[tt],sep=""),"est",drop=FALSE],drop=5))/
            (1-adrop(mono.Ftest[,,,,paste("t",time_choice.predicted[tt],sep=""),"est",drop=FALSE],drop=5))
        }
      }
    }
  }


  list(betaest=betaest,
       alphasest=alphasest,
       Ftest=Ftest,
       Ftest.predicted=Ftest.predicted,
       eflag=eflag)
}





#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+
#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+
#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+

#############################################
## R code to read in data and show results ##
#############################################


######################
## Useful functions ##
######################

make.data.arrays <- function(data.theta.est,#=data.beta,
                             data.theta.varboot,#=data.beta.varboot,
                             cnames,#=c("ss","np","iter","lb",paste("t",time_val,sep="")),
                             num_study,
                             np,
                             theta.set,#=1:lb,
                             theta.set2,#=1:lb,
                             nsimu,
                             num_time,
                             time_val,
                             var.est,
                             theta.interest="lb",s.names,boot.ci){

  colnames(data.theta.est) <- cnames


  theta.array <- array(0,dim=c(num_study,np,length(theta.set2),nsimu,num_time),
                       dimnames=list(
                         paste("ss",1:num_study,sep=""),
                         s.names,
                         paste(theta.interest,theta.set2,sep=""),
                         paste("iter",1:nsimu,sep=""),
                         paste("t",time_val,sep="")))

  theta.length <- length(theta.set2)

  if(var.est!="none"){
    data.theta.var <- array(0,dim=c(num_study,np,theta.length*nsimu,length(cnames)),
                            dimnames=list(
                              paste("ss",1:num_study,sep=""),
                              s.names,
                              paste("ind",1:(theta.length*nsimu),sep=""),
                              cnames))
    data.theta.ci.lo <- data.theta.var
    data.theta.ci.hi <- data.theta.var

    ## get each component
    tmp.row <-nrow(data.theta.varboot)/(np*num_study)
    for(ss in 1:num_study){
      for(nn in 1:np){
        ind <- which(data.theta.varboot[,1]==ss & data.theta.varboot[,2]==nn)
        data.theta.var[ss,nn,,] <- as.matrix(data.theta.varboot[ind[seq(1,tmp.row,by=3)],])
        data.theta.ci.lo[ss,nn,,] <- as.matrix(data.theta.varboot[ind[seq(2,tmp.row,by=3)],])
        data.theta.ci.hi[ss,nn,,] <- as.matrix(data.theta.varboot[ind[seq(3,tmp.row,by=3)],])
      }
    }
    theta.var.array <- theta.array
    theta.ci.lo.array <- theta.array
    theta.ci.hi.array <- theta.array

  } else {
    data.theta.var <- NULL
    data.theta.ci.lo <- NULL
    data.theta.ci.hi <- NULL
  }

  index.interest <- which(cnames==theta.interest)

  for(ss in 1:num_study){
    ##print(ss)
    for(nn in 1:np){
      ##print(nn)
      for(i in 1:theta.length){
        ##print(i)
        if(is.list(theta.set)){
          theta.set.tmp <- theta.set[[ss]][i]
        } else {
          theta.set.tmp <- theta.set[i]
        }
        index <- which(data.theta.est[,index.interest]==theta.set.tmp & data.theta.est[,"ss"]==ss &
                         data.theta.est[,"np"]==nn)

        if(length(index)< 1e-6){
          ## no elements in index, go to next i in for-loop
          next()
        } else {
          ## elements in index found
          theta.array[ss,nn,i,,] <- as.matrix(data.theta.est[index,paste("t",time_val,sep="")])

          index2 <- which(data.theta.var[ss,nn,,theta.interest]==theta.set.tmp)

          if(length(index2) < 1e-6){
            ## no elements in index2, go to next i in for-loop
            next()
          } else {
            ## elements in index2 found
            if(var.est!="none"){
              theta.var.array[ss,nn,i,,] <-as.matrix(data.theta.var[ss,nn,index2,
                                                                    paste("t",time_val,sep="")])
              if(boot.ci==TRUE){
                theta.ci.lo.array[ss,nn,i,,] <-as.matrix(data.theta.ci.lo[ss,nn,index2,
                                                                          paste("t",time_val,sep="")])
                theta.ci.hi.array[ss,nn,i,,] <-as.matrix(data.theta.ci.hi[ss,nn,index2,
                                                                          paste("t",time_val,sep="")])
              } else {
                theta.ci.lo.array[ss,nn,i,,] <-
                  adrop(theta.array[ss,nn,i,,,drop=FALSE],drop=c(1,2,3)) +
                  qnorm(0.025)*sqrt(adrop(theta.var.array[ss,nn,i,,,drop=FALSE],drop=c(1,2,3)))
                ##	if (is.na(sqrt(adrop(theta.var.array[ss,nn,i,,,drop=FALSE],drop=c(1,2,3))))){
                ##	 print(i)
                ##	}

                theta.ci.hi.array[ss,nn,i,,] <- adrop(theta.array[ss,nn,i,,,drop=FALSE],drop=c(1,2,3)) +
                  qnorm(0.975)*sqrt(adrop(theta.var.array[ss,nn,i,,,drop=FALSE],drop=c(1,2,3)))
              }
            } else {
              theta.var.array <- NULL
              theta.ci.lo.array <- NULL
              theta.ci.hi.array <- NULL
            }
          }
        }
      }
    }
  }

  list(theta.array=theta.array,
       data.theta.var=data.theta.var,
       data.theta.ci.lo=data.theta.ci.lo,
       data.theta.ci.hi=data.theta.ci.hi,
       theta.var.array=theta.var.array,
       theta.ci.lo.array=theta.ci.lo.array,
       theta.ci.hi.array=theta.ci.hi.array)
}




make.Ft.arrays <- function(data.Ft,data.Ft.varboot,
                           num_study,np,nsimu,num_time,time_val,num_xx,z.choice,z_lab,s.names,
                           var.est=var.est,boot.ci,xx_val){

  data.Ft.est <- data.Ft
  colnames(data.Ft.est) <- c("ss","np","iter","zz","xx",paste("t",time_val,sep=""))

  Ft.array <- array(0,dim=c(num_study,np,z.choice,num_xx,nsimu,num_time),
                    dimnames=list(
                      paste("ss",1:num_study,sep=""),
                      s.names,
                      paste("zz_",z_lab,sep=""),
                      paste("xx",1:num_xx,sep=""),
                      paste("iter",1:nsimu,sep=""),
                      paste("t",time_val,sep="")))

  ## get each component
  if(var.est!="none"){
    data.Ft.varboot <- data.Ft.varboot

    data.Ft.var <- array(0,dim=c(num_study,np,z.choice * num_xx * nsimu,5+length(time_val)),
                         dimnames=list(
                           paste("ss",1:num_study,sep=""),
                           s.names,
                           paste("ind",1:(z.choice * num_xx * nsimu),sep=""),
                           c("ss","np","iter","zz","xx",paste("t",time_val,sep=""))))
    data.Ft.ci.lo <- data.Ft.var
    data.Ft.ci.hi <- data.Ft.var

    ## get each component
    tmp.row <-nrow(data.Ft.varboot)/(num_study*np)
    for(ss in 1:num_study){
      for(nn in 1:np){
        ind <- which(data.Ft.varboot[,1]==ss & data.Ft.varboot[,2]==nn )
        data.Ft.var[ss,nn,,] <- as.matrix(data.Ft.varboot[ind[seq(1,tmp.row,by=3)],])
        data.Ft.ci.lo[ss,nn,,] <- as.matrix(data.Ft.varboot[ind[seq(2,tmp.row,by=3)],])
        data.Ft.ci.hi[ss,nn,,] <- as.matrix(data.Ft.varboot[ind[seq(3,tmp.row,by=3)],])
      }
    }

    Ft.var.array <- Ft.array
    Ft.ci.lo.array <- Ft.array
    Ft.ci.hi.array <- Ft.array

  } else {
    data.Ft.var <- NULL
    data.Ft.ci.lo <- NULL
    data.Ft.ci.hi <- NULL
  }

  for(ss in 1:num_study){
    for(nn in 1:np){
      for(zz in 1:z.choice){
        for(i in 1:num_xx){
          index <- which(data.Ft.est$xx==xx_val[[ss]][i] & data.Ft.est$ss==ss &
                           data.Ft.est$np==nn & data.Ft.est$zz==zz)

          if(length(index)<1e-6){
            ## no elements in index, go to next i in for-loop
            next()
          } else{
            ## elements in index found
            index2 <- which(data.Ft.var[ss,nn,,"xx"]==xx_val[[ss]][i] & data.Ft.var[ss,nn,,"zz"]==zz)

            Ft.array[ss,nn,zz,i,,] <- as.matrix(data.Ft.est[index,paste("t",time_val,sep="")])

            if(length(index2)<1e-6){
              ## no elements in index2, go to next i in for-loop
              next()
            } else {
              ## elements in index found
              if(var.est!="none"){
                Ft.var.array[ss,nn,zz,i,,] <- as.matrix(data.Ft.var[ss,nn,index2,
                                                                    paste("t",time_val,sep="")])
                if(boot.ci==TRUE){
                  Ft.ci.lo.array[ss,nn,zz,i,,] <- as.matrix(data.Ft.ci.lo[ss,nn,index2,
                                                                          paste("t",time_val,sep="")])
                  Ft.ci.hi.array[ss,nn,zz,i,,] <- as.matrix(data.Ft.ci.hi[ss,nn,index2,
                                                                          paste("t",time_val,sep="")])
                } else {
                  tmp.var  <- adrop(Ft.var.array[ss,nn,zz,i,,,drop=FALSE],drop=1:4)
                  zero.function <- function(x){
                    if(abs(x)<1e-6){
                      return(0)
                    } else {
                      return(x)
                    }
                  }
                  tmp.var <- apply(tmp.var,c(1,2),zero.function)

                  Ft.ci.lo.array[ss,nn,zz,i,,] <- adrop(Ft.array[ss,nn,zz,i,,,drop=FALSE],drop=c(1,2,3,4)) +qnorm(0.025)*sqrt(tmp.var)
                  Ft.ci.hi.array[ss,nn,zz,i,,] <- adrop(Ft.array[ss,nn,zz,i,,,drop=FALSE],drop=c(1,2,3,4)) +qnorm(0.975)*sqrt(tmp.var)
                }
              } else {
                Ft.var.array <- NULL
                Ft.ci.lo.array <- NULL
                Ft.ci.hi.array <- NULL
              }
            }
          }
        }
      }
    }
  }


  list(theta.array=Ft.array,
       data.theta.var=data.Ft.var,
       data.theta.ci.lo=data.Ft.ci.lo,
       data.theta.ci.hi=data.Ft.ci.hi,
       theta.var.array=Ft.var.array,
       theta.ci.lo.array=Ft.ci.lo.array,
       theta.ci.hi.array=Ft.ci.hi.array)
}




make.Ft.diff.arrays <- function(data.Ft.diff,
                                num_study,np,nsimu,num_time,time_val,num_xx,z.choice,z_lab,s.names,
                                var.est=var.est,combi.study,combi.names,xx_val){

  data.Ft.est <- data.Ft.diff
  colnames(data.Ft.est) <- c("combi","np","iter","zz","xx",paste("t",time_val,sep=""))

  Ft.array <- array(0,dim=c(combi.study,np,z.choice,num_xx,nsimu,num_time),
                    dimnames=list(
                      combi.names,
                      s.names,
                      paste("zz_",z_lab,sep=""),
                      paste("xx",1:num_xx,sep=""),
                      paste("iter",1:nsimu,sep=""),
                      paste("t",time_val,sep="")))
  Ft.estdiff.array <- Ft.array
  Ft.ci.lo.array <- Ft.array
  Ft.ci.hi.array <- Ft.array

  data.Ft <- array(0,dim=c(combi.study,np,z.choice * num_xx * nsimu,5+length(time_val)),
                   dimnames=list(
                     combi.names,
                     s.names,
                     paste("ind",1:(z.choice * num_xx * nsimu),sep=""),
                     c("ss","np","iter","zz","xx",paste("t",time_val,sep=""))))
  data.Ft.estdiff <- data.Ft
  data.Ft.ci.lo <- data.Ft
  data.Ft.ci.hi <- data.Ft

  ## get each component
  tmp.row <-nrow(data.Ft.est)/(combi.study*np)
  for(ss in 1:combi.study){
    for(nn in 1:np){
      ind <- which(data.Ft.est[,1]==combi.names[ss] & data.Ft.est[,2]==nn )
      data.Ft[ss,nn,,] <- as.matrix(data.Ft.est[ind[seq(1,tmp.row,by=4)],])
      data.Ft.estdiff[ss,nn,,] <- as.matrix(data.Ft.est[ind[seq(2,tmp.row,by=4)],])
      data.Ft.ci.lo[ss,nn,,] <- as.matrix(data.Ft.est[ind[seq(3,tmp.row,by=4)],])
      data.Ft.ci.hi[ss,nn,,] <- as.matrix(data.Ft.est[ind[seq(4,tmp.row,by=4)],])
    }
  }

  for(ss in 1:combi.study){
    for(nn in 1:np){
      for(zz in 1:z.choice){
        for(i in 1:num_xx){
          ## elements in index found
          ## assume xx_val the same for all studies
          index2 <- which(data.Ft[ss,nn,,"xx"]==xx_val[[1]][i] & data.Ft[ss,nn,,"zz"]==zz)

          if(length(index2)<1e-6){
            ## no elements in index2, go to next i in for-loop
            next()
          } else {
            ## elements in index found
            Ft.array[ss,nn,zz,i,,] <- as.matrix(data.Ft[ss,nn,index2,
                                                        paste("t",time_val,sep="")])
            Ft.estdiff.array[ss,nn,zz,i,,] <- as.matrix(data.Ft.estdiff[ss,nn,index2,
                                                                        paste("t",time_val,sep="")])
            Ft.ci.lo.array[ss,nn,zz,i,,] <- as.matrix(data.Ft.ci.lo[ss,nn,index2,
                                                                    paste("t",time_val,sep="")])
            Ft.ci.hi.array[ss,nn,zz,i,,] <- as.matrix(data.Ft.ci.hi[ss,nn,index2,
                                                                    paste("t",time_val,sep="")])
          }
        }
      }
    }
  }

  list(theta.array=Ft.array,
       data.theta.estdiff=data.Ft.estdiff,
       data.theta.ci.lo=data.Ft.ci.lo,
       data.theta.ci.hi=data.Ft.ci.hi,
       theta.estdiff.array=Ft.estdiff.array,
       theta.ci.lo.array=Ft.ci.lo.array,
       theta.ci.hi.array=Ft.ci.hi.array)
}


make.data.diff.arrays <- function(data.theta.est,#=data.beta.diff,
                                  cnames,#=c("combi","np","iter","lb",paste("t",time_val,sep="")),
                                  num_study,
                                  np,
                                  theta.set,#=1:lb,
                                  theta.set2,#=1:lb,
                                  nsimu,
                                  num_time,
                                  time_val,
                                  var.est,
                                  theta.interest="lb",
                                  s.names,
                                  combi.study,
                                  combi.names){

  colnames(data.theta.est) <- cnames


  theta.array <- array(0,dim=c(combi.study,np,length(theta.set2),nsimu,num_time),
                       dimnames=list(
                         combi.names,
                         s.names,
                         paste(theta.interest,theta.set2,sep=""),
                         paste("iter",1:nsimu,sep=""),
                         paste("t",time_val,sep="")))

  theta.estdiff.array <- theta.array
  theta.ci.lo.array <- theta.array
  theta.ci.hi.array <- theta.array

  theta.length <- length(theta.set2)

  data.theta <- array(0,dim=c(combi.study,np,theta.length*nsimu,length(cnames)),
                      dimnames=list(
                        combi.names,
                        s.names,
                        paste("ind",1:(theta.length*nsimu),sep=""),
                        cnames))
  data.theta.estdiff <- data.theta
  data.theta.ci.lo <- data.theta
  data.theta.ci.hi <- data.theta

  ## get each component
  tmp.row <-nrow(data.theta.est)/(np*combi.study)
  for(ss in 1:combi.study){
    for(nn in 1:np){
      ind <- which(data.theta.est[,1]==combi.names[ss] & data.theta.est[,2]==nn)
      data.theta[ss,nn,,] <- as.matrix(data.theta.est[ind[seq(1,tmp.row,by=4)],])
      data.theta.estdiff[ss,nn,,] <- as.matrix(data.theta.est[ind[seq(2,tmp.row,by=4)],])
      data.theta.ci.lo[ss,nn,,] <- as.matrix(data.theta.est[ind[seq(3,tmp.row,by=4)],])
      data.theta.ci.hi[ss,nn,,] <- as.matrix(data.theta.est[ind[seq(4,tmp.row,by=4)],])
    }
  }

  index.interest <- which(cnames==theta.interest)

  for(ss in 1:combi.study){
    ##print(ss)
    for(nn in 1:np){
      ##print(nn)
      for(i in 1:theta.length){
        ##print(i)
        theta.set.tmp <- theta.set[i]

        index2 <- which(data.theta.estdiff[ss,nn,,theta.interest]==theta.set.tmp)
        if(length(index2) < 1e-6){
          ## no elements in index2, go to next i in for-loop
          next()
        } else {
          ## elements in index2 found
          theta.array[ss,nn,i,,] <-as.matrix(data.theta[ss,nn,index2,
                                                        paste("t",time_val,sep="")])
          theta.estdiff.array[ss,nn,i,,] <-as.matrix(data.theta.estdiff[ss,nn,index2,
                                                                        paste("t",time_val,sep="")])
          theta.ci.lo.array[ss,nn,i,,] <-as.matrix(data.theta.ci.lo[ss,nn,index2,
                                                                    paste("t",time_val,sep="")])
          theta.ci.hi.array[ss,nn,i,,] <-as.matrix(data.theta.ci.hi[ss,nn,index2,
                                                                    paste("t",time_val,sep="")])
        }
      }
    }
  }

  list(theta.array=theta.array,
       data.theta.estdiff=data.theta.estdiff,
       data.theta.ci.lo=data.theta.ci.lo,
       data.theta.ci.hi=data.theta.ci.hi,
       theta.estdiff.array=theta.estdiff.array,
       theta.ci.lo.array=theta.ci.lo.array,
       theta.ci.hi.array=theta.ci.hi.array)
}


#########################
## get needed matrices ##
#########################
## x: data frame, y: names of columns (forward)

reverse.name.columns <- function(x,y){
  colnames(x)[ncol(x):(ncol(x)-length(y)+1)] <- rev(y) ## 11: (11-5+1)
  return(x)
}

## find rows with specific pattern

find.pattern.index <- function(x,pattern="Ft*"){
  out <- apply(x,2,grep,pattern=pattern) ## grep(pattern, x) search for matches to argument pattern within each element of a character vector
  out.length <- lapply(out,length)   ## V3: 630
  out.index <- which(out.length>0)   ## V3: 3
  out <- out[[out.index]]
  return(out)
}


##################################################
## function to put flattened results into array ##
##################################################

unflatten.array <- function(x,dim.order,data.change,
                            flatten.name="time"){
  xorig <- x
  x <- aperm(x,dim.order)
  dim.flatten <- which(names(dimnames(x))==flatten.name) ## "t40" "t45" "t50" "t55" "t60"
  dim.use <- 1:length(dimnames(x))  ## 5
  dim.use <- setdiff(dim.use,dim.flatten) ## setdiff(  c(1,2,3,4,5), 5)

  out  <- as.matrix(data.change[, unlist(dimnames(x)[dim.flatten])])  #$time
  #[1] "t40" "t45" "t50" "t55" "t60"  ---> unlist
  ## time1 time2 time3 time4 time5
  ## "t40" "t45" "t50" "t55" "t60"


  dim(out) <- dim(x)[c(dim.use,dim.flatten)] ### study event    zz    xx  time
  ### 3     3     3    14     5

  dimnames(out) <- dimnames(x)[c(dim.use,dim.flatten)]
  #$study
  #[1] "cohort"  "predict" "pharos"

  #$event
  #[1] "hdage_nobase" "mcione"       "dep2"

  #$zz
  #[1] "zz1"      "zz-study" "zz-CAG"

  #$xx
  #[1] "xx36"   "xx37.4" "xx38.8" "xx40.2" "xx41.6" "xx42"   "xx43"   "xx44"
  #[9] "xx44.4" "xx45.8" "xx46"   "xx47.2" "xx48.6" "xx50"

  #$time
  #[1] "t40" "t45" "t50" "t55" "t60"

  ## reverse order
  ##out <- aperm(out,rev(dim.order))

  ## put order as original
  array.order <-  match(names(dimnames(xorig)),names(dimnames(out))) ## 1, 2, 3, 4, 5
  out <- aperm(out,array.order)

  return(out)
}


unflatten.organize.data <- function(data.use,
                                    time_val,
                                    time_choice.predicted,
                                    null.theta.use,
                                    dim.order.use,s.names){
  ## name last column names by time
  data.use <-  reverse.name.columns(data.use,paste("t",time_val,sep="")) ##  x: data frame, y: names of columns (forward)
  ## give name from last column (col(data.use)) to col(data.use)-length(y)+1
  ## time_val: 40, 45, 50, 55, 60

  ## find index with "Ft", alpha
  index.Ft <- find.pattern.index(data.use,pattern="Ft")  ## return the listed numbers (index)
  index.alpha <- find.pattern.index(data.use,pattern="alpha*")

  if(!is.null(time_choice.predicted)){
    #index.Ft.predicted <- find.pattern.index(data.use,pattern="Ft.predicted*")
    #index.Ft <- setdiff(index.Ft,index.Ft.predicted)
    index.Ft.predicted <- find.pattern.index(data.use,pattern="Ft.predicted")
    index.Ft.predicted <- intersect(index.Ft,index.Ft.predicted)   ## intersect(A,B) is a data frame with those rows that are both in A and in B.
    Ft.predicted <- data.use[index.Ft.predicted,] ## obtain a data frame with index.Ft.predicted.
    index.Ft <- setdiff(index.Ft,index.Ft.predicted)
  } else{
    index.Ft.predicted <- NULL
    Ft.predicted <- NULL
  }
  ## extract data
  betat <- data.use[-c(index.Ft,index.Ft.predicted,index.alpha),]  ## beta*, gamma
  alphat <- data.use[index.alpha,] ## alpha*
  Ft <- data.use[index.Ft,]

  ## put together as arrays
  betat <- unflatten.array(x=null.theta.use$beta,
                           dim.order=dim.order.use$beta,
                           data.change=betat,flatten.name="time")

  alphat <- unflatten.array(x=null.theta.use$alphas,
                            dim.order=dim.order.use$alphas,
                            data.change=alphat,flatten.name="time")

  Ft <- unflatten.array(x=null.theta.use$Ft,
                        dim.order=dim.order.use$Ft,
                        data.change=Ft,flatten.name="time")

  if(!is.null(time_choice.predicted)){
    Ft.predicted <- unflatten.array(x=null.theta.use$Ft.predicted,
                                    dim.order=dim.order.use$Ft.predicted,
                                    data.change=Ft.predicted,flatten.name="time")
  } else {
    Ft.predicted <- NULL
  }
  list(beta=betat,alpha=alphat,Ft=Ft,Ft.predicted=Ft.predicted)
}



sort.results <- function(
  combi.names,
  num_study,
  simus,
  time_val,param.label,
  num_xx,la,z.choice,
  time_choice.predicted,time_choice.predicted.toadd,
  #data.truth,
  data.theta=data.theta,
  data.combi=data.combi,
  alpha.cut,
  beta.cut,
  theta.names,
  theta.names.combi,
  study.names,
  s.names,
  z_lab_names,
  x_lab_names

){

  ## get necessary information

  ## get null theta arrays
  null.theta <- all_null_theta(theta.names,
                               study.names,
                               event.names=s.names,
                               z_lab_names,
                               x_lab_names,
                               label.dim.simus=simus,
                               label.name.simus=paste("iter",1:simus,sep=""),
                               time_val,param.label,
                               time_choice.predicted,time_choice.predicted.toadd,la
  )

  ## get combi null theta arrays
  combi.null.theta <- all_null_theta(theta.names.combi,
                                     study.names=combi.names,
                                     event.names=s.names,
                                     z_lab_names,
                                     x_lab_names,
                                     label.dim.simus=simus,
                                     label.name.simus=paste("iter",1:simus,sep=""),
                                     time_val,param.label,
                                     time_choice.predicted,time_choice.predicted.toadd,la)


  dim.order.all <- get.all.dim.order(theta.names)  # theta.names [1] "beta"         "alphas"       "Ft"           "Ft.predicted"
  ## get all order of dimensions for dim.order, dim.order.simus, dim.order.simus.ci (they are list of theta.names)

  ###########
  ## truth ##
  ###########
  #out <- unflatten.organize.data(data.use=data.truth,            ## matrix to array
  #                               time_val,
  #                               time_choice.predicted,
  #                               null.theta.use=null.theta$null.theta,
  #                               dim.order.use=dim.order.all$dim.order)
  #betat <- out$beta
  #alphat <- out$alpha
  #Ft <- out$Ft
  #Ft.predicted <- out$Ft.predicted

  #####################
  ## theta estimates ##
  #####################
  out <- unflatten.organize.data(data.use=data.theta,
                                 time_val,
                                 time_choice.predicted,
                                 null.theta.use=null.theta$null.theta.simus.est.ciboot,
                                 dim.order.use=dim.order.all$dim.order.simus.ci)
  beta.array <- out$beta
  beta.mean <- apply.index(beta.array,"iters",mean)

  alpha.array <- out$alpha
  alpha.mean <-  apply.index(alpha.array,"iters",mean) ## average of esimates over iteration per study at each event


  Ft.array <- out$Ft
  Ft.mean  <-  apply.index(Ft.array,"iters",mean)
  ## do monotonity
  if(1==1){ ##for testing
    Ft.mono.array <- apply.function.index(Ft.array,
                                          dim.fun=c("time","xx"),getF,
                                          p=length(dimnames(Ft.array)[["xx"]]))
    Ft.mono.mean <-  apply.function.index(Ft.mean,
                                          dim.fun=c("time","xx"),getF,
                                          p=length(dimnames(Ft.mean)[["xx"]]))
  } else{
    Ft.mono.array <- Ft.array
    Ft.mono.mean <- Ft.mean
  }  ## end for testing

  if(!is.null(time_choice.predicted)){
    Ft.predicted.array <- out$Ft.predicted
    Ft.predicted.mean <- apply.index(Ft.predicted.array,"iters",mean)
  } else{
    Ft.predicted.array <- NULL
    Ft.predicted.mean <- NULL
  }

  #####################
  ## combi estimates ##
  #####################
  if(num_study>1){
    out <- unflatten.organize.data(data.use=data.combi,
                                   time_val,
                                   time_choice.predicted=NULL,
                                   null.theta.use=combi.null.theta$null.theta.simus.est.ci,
                                   dim.order.use=dim.order.all$dim.order.simus.ci)
    beta.diff.array <- out$beta
    beta.diff.mean <- apply.index(beta.diff.array,"iters",mean)

    alpha.diff.array <- out$alpha
    alpha.diff.mean <-  apply.index(alpha.diff.array,"iters",mean)

    Ft.diff.array <- out$Ft
    Ft.diff.mean  <-  apply.index(Ft.diff.array,"iters",mean)

    beta.diff.array <- list(out=beta.diff.array,
                            out.mean=beta.diff.mean)
    alpha.diff.array <- list(out=alpha.diff.array,
                             out.mean=alpha.diff.mean)
    Ft.diff.array <- list(out=Ft.diff.array,
                          out.mean=Ft.diff.mean)

  } else{
    beta.diff.array <- NULL
    alpha.diff.array <- NULL
    Ft.diff.array <- NULL
  }

  ######################
  ## alpha(x,t) vs. x ##
  ######################
  alpha.array.new <- aperm(alpha.array,c("iters","study","event","time","xx","theta","val")) ## permutation  of alpha.array with the following order
  alpha.new.mean <- aperm(alpha.mean,c("study","event","time","xx","theta","val"))
  #alphat.new <- aperm(alphat,c("study","event","time","xx","theta"))

  ##########################
  ## put objects together ##
  ##########################
  beta.array <- list(out=beta.array,
                     out.mean=beta.mean)
  alpha.array <- list(out=alpha.array,
                      out.mean=alpha.mean)
  Ft.array <- list(out=Ft.array,
                   out.mean=Ft.mean,
                   out.mono=Ft.mono.array,
                   out.mono.mean=Ft.mono.mean)
  Ft.predicted.array <- list(out=Ft.predicted.array,
                             out.mean=Ft.predicted.mean)
  alpha.array.new <- list(out=alpha.array.new,
                          out.mean=alpha.new.mean)


  list(#betat=betat,
       #alphat=alphat,
       #alphat.new=alphat.new,
       #Ft=Ft,
       #Ft.predicted=Ft.predicted,
       #
       beta.array=beta.array,
       alpha.array=alpha.array,
       alpha.array.new=alpha.array.new,
       Ft.array=Ft.array,
       Ft.predicted.array=Ft.predicted.array,
       #
       beta.diff.array=beta.diff.array,
       alpha.diff.array=alpha.diff.array,
       Ft.diff.array=Ft.diff.array)
}

# sort.results <- function(
#   combi.names,
#   num_study,
#   simus,
#   time_val,param.label,
#   num_xx,la,z.choice,
#   time_choice.predicted,time_choice.predicted.toadd,
#   #data.truth,
#   data.theta,
#   data.combi,
#   alpha.cut,
#   beta.cut,
#   theta.names,
#   theta.names.combi,
#   study.names,
#   s.names,
#   z_lab_names,
#   x_lab_names
#
# ){
#
#   ## get necessary information
#
#   ## get null theta arrays
#   null.theta <- all_null_theta(theta.names,
#                                study.names,
#                                event.names=s.names,
#                                z_lab_names,
#                                x_lab_names,
#                                label.dim.simus=simus,
#                                label.name.simus=paste("iter",1:simus,sep=""),
#                                time_val,param.label,
#                                time_choice.predicted,time_choice.predicted.toadd,la
#   )
#
#   ## get combi null theta arrays
#   combi.null.theta <- all_null_theta(theta.names.combi,
#                                      study.names=combi.names,
#                                      event.names=s.names,
#                                      z_lab_names,
#                                      x_lab_names,
#                                      label.dim.simus=simus,
#                                      label.name.simus=paste("iter",1:simus,sep=""),
#                                      time_val,param.label,
#                                      time_choice.predicted,time_choice.predicted.toadd,la)
#
#
#   dim.order.all <- get.all.dim.order(theta.names)
#
#   ###########
#   ## truth ##
#   ###########
#   # out <- unflatten.organize.data(data.use=data.truth,
#   #                                time_val,
#   #                                time_choice.predicted,
#   #                                null.theta.use=null.theta$null.theta,
#   #                                dim.order.use=dim.order.all$dim.order)
#   # betat <- out$beta
#   # alphat <- out$alpha
#   # Ft <- out$Ft
#   # Ft.predicted <- out$Ft.predicted
#   #
#   #####################
#   ## theta estimates ##
#   #####################
#   out <- unflatten.organize.data(data.use=data.theta,
#                                  time_val,
#                                  time_choice.predicted,
#                                  null.theta.use=null.theta$null.theta.simus.est.ciboot,
#                                  dim.order.use=dim.order.all$dim.order.simus.ci)
#   beta.array <- out$beta
#   beta.mean <- apply.index(beta.array,"iters",mean)
#
#   alpha.array <- out$alpha
#   alpha.mean <-  apply.index(alpha.array,"iters",mean)
#
#
#   Ft.array <- out$Ft
#   Ft.mean  <-  apply.index(Ft.array,"iters",mean)
#   ## do monotonity
#   if(1==1){ ##for testing
#     Ft.mono.array <- apply.function.index(Ft.array,
#                                           dim.fun=c("time","xx"),getF,
#                                           p=length(dimnames(Ft.array)[["xx"]]))
#     Ft.mono.mean <-  apply.function.index(Ft.mean,
#                                           dim.fun=c("time","xx"),getF,
#                                           p=length(dimnames(Ft.mean)[["xx"]]))
#   } else{
#     Ft.mono.array <- Ft.array
#     Ft.mono.mean <- Ft.mean
#   }  ## end for testing
#
#   if(!is.null(time_choice.predicted)){
#     Ft.predicted.array <- out$Ft.predicted
#     Ft.predicted.mean <- apply.index(Ft.predicted.array,"iters",mean)
#   } else{
#     Ft.predicted.array <- NULL
#     Ft.predicted.mean <- NULL
#   }
#
#   #####################
#   ## combi estimates ##
#   #####################
#   if(num_study>1){
#     out <- unflatten.organize.data(data.use=data.combi,
#                                    time_val,
#                                    time_choice.predicted=NULL,
#                                    null.theta.use=combi.null.theta$null.theta.simus.est.ci,
#                                    dim.order.use=dim.order.all$dim.order.simus.ci)
#     beta.diff.array <- out$beta
#     beta.diff.mean <- apply.index(beta.diff.array,"iters",mean)
#
#     alpha.diff.array <- out$alpha
#     alpha.diff.mean <-  apply.index(alpha.diff.array,"iters",mean)
#
#     Ft.diff.array <- out$Ft
#     Ft.diff.mean  <-  apply.index(Ft.diff.array,"iters",mean)
#
#     beta.diff.array <- list(out=beta.diff.array,
#                             out.mean=beta.diff.mean)
#     alpha.diff.array <- list(out=alpha.diff.array,
#                              out.mean=alpha.diff.mean)
#     Ft.diff.array <- list(out=Ft.diff.array,
#                           out.mean=Ft.diff.mean)
#
#   } else{
#     beta.diff.array <- NULL
#     alpha.diff.array <- NULL
#     Ft.diff.array <- NULL
#   }
#
#   ######################
#   ## alpha(x,t) vs. x ##
#   ######################
#   alpha.array.new <- aperm(alpha.array,c("iters","study","event","time","xx","theta","val"))
#   alpha.new.mean <- aperm(alpha.mean,c("study","event","time","xx","theta","val"))
#   alphat.new <- aperm(alphat,c("study","event","time","xx","theta"))
#
#   ##########################
#   ## put objects together ##
#   ##########################
#   beta.array <- list(out=beta.array,
#                      out.mean=beta.mean)
#   alpha.array <- list(out=alpha.array,
#                       out.mean=alpha.mean)
#   Ft.array <- list(out=Ft.array,
#                    out.mean=Ft.mean,
#                    out.mono=Ft.mono.array,
#                    out.mono.mean=Ft.mono.mean)
#   Ft.predicted.array <- list(out=Ft.predicted.array,
#                              out.mean=Ft.predicted.mean)
#   alpha.array.new <- list(out=alpha.array.new,
#                           out.mean=alpha.new.mean)
#
#
#   list(#betat=betat,
#        #alphat=alphat,
#        #alphat.new=alphat.new,
#        #Ft=Ft,
#        #Ft.predicted=Ft.predicted,
#        #
#        beta.array=beta.array,
#        alpha.array=alpha.array,
#        alpha.array.new=alpha.array.new,
#        Ft.array=Ft.array,
#        Ft.predicted.array=Ft.predicted.array,
#        #
#        beta.diff.array=beta.diff.array,
#        alpha.diff.array=alpha.diff.array,
#        Ft.diff.array=Ft.diff.array)
# }

sort.results.when.jprat.ouput.array <- function(time_choice.predicted, num_study, out){

  #####################
  ## theta estimates ##
  #####################

  beta.array<-out$beta.array
  alpha.array<-out$alpha.array
  #alpha.array.new <- aperm(alpha.array,c("iters","study","event","time","xx","theta","val"))
  #alpha.array.new=perm.alphaest.store,
  Ft.array<-out$Ft.array
  Ft.predicted.array<-out$Ft.predicted.array
  beta.diff.array<-out$beta.diff.array
  alpha.diff.array<-out$alpha.diff.array
  Ft.diff.array<-out$Ft.diff.array

  ##########################
  ## theta mean estimates ##
  ##########################
  beta.mean <- apply.index(beta.array,"iters",mean)
  alpha.mean <-  apply.index(alpha.array,"iters",mean) ## average of esimates over iteration per study at each even
  Ft.mean  <-  apply.index(Ft.array,"iters",mean)

  ## do monotonit
    Ft.mono.array <- apply.function.index(Ft.array,
                                          dim.fun=c("time","xx"),getF,
                                          p=length(dimnames(Ft.array)[["xx"]]))
    Ft.mono.mean <-  apply.function.index(Ft.mean,
                                          dim.fun=c("time","xx"),getF,
                                          p=length(dimnames(Ft.mean)[["xx"]]))

    if(!is.null(time_choice.predicted)){
      Ft.predicted.array <- out$Ft.predicted
      Ft.predicted.mean <- apply.index(Ft.predicted.array,"iters",mean)
    } else{
      Ft.predicted.array <- NULL
      Ft.predicted.mean <- NULL
    }


  #####################
  ## combi estimates ##
  ####################
    if(num_study>1){
    beta.diff.mean <- apply.index(beta.diff.array,"iters",mean)
    alpha.diff.mean <-  apply.index(alpha.diff.array,"iters",mean)
    Ft.diff.mean  <-  apply.index(Ft.diff.array,"iters",mean)

    beta.diff.array <- list(out=beta.diff.array,
                            out.mean=beta.diff.mean)
    alpha.diff.array <- list(out=alpha.diff.array,
                             out.mean=alpha.diff.mean)
    Ft.diff.array <- list(out=Ft.diff.array,
                          out.mean=Ft.diff.mean)
    }else{
      beta.diff.array <- NULL
      alpha.diff.array <- NULL
      Ft.diff.array <- NULL
    }


  ######################
  ## alpha(x,t) vs. x ##
  ######################
  alpha.array.new <- aperm(alpha.array,c("iters","study","event","time","xx","theta","val")) ## permutation  of alpha.array with the following order
  alpha.new.mean <- aperm(alpha.mean,c("study","event","time","xx","theta","val"))
  #alphat.new <- aperm(alphat,c("study","event","time","xx","theta"))

  ##########################
  ## put objects together ##
  ##########################
  beta.array <- list(out=beta.array,
                     out.mean=beta.mean)
  alpha.array <- list(out=alpha.array,
                      out.mean=alpha.mean)
  Ft.array <- list(out=Ft.array,
                   out.mean=Ft.mean,
                   out.mono=Ft.mono.array,
                   out.mono.mean=Ft.mono.mean)
  Ft.predicted.array <- list(out=Ft.predicted.array,
                             out.mean=Ft.predicted.mean)
  alpha.array.new <- list(out=alpha.array.new,
                          out.mean=alpha.new.mean)


  list(#betat=betat,
    #alphat=alphat,
    #alphat.new=alphat.new,
    #Ft=Ft,
    #Ft.predicted=Ft.predicted,
    #
    beta.array=beta.array,
    alpha.array=alpha.array,
    alpha.array.new=alpha.array.new,
    Ft.array=Ft.array,
    Ft.predicted.array=Ft.predicted.array,
    #
    beta.diff.array=beta.diff.array,
    alpha.diff.array=alpha.diff.array,
    Ft.diff.array=Ft.diff.array)
}



###############################
## Nadaraya-Watson functions ##
###############################

## function to get nw estimator

create.nw.estimator <- function(time_val){
  function(theta){
    nw.estimate <- ksmooth(time_val,theta)
    return(nw.estimate$y)
  }
}

## function to get nw estimator time values

create.nw.estimator.time <- function(time_val){
  function(theta){
    nw.estimate <- ksmooth(time_val,theta)
    return(nw.estimate$x)
  }
}


#####################
## Plot functions  ##
#####################


## function to find upper and lower quantiles

myquantiles.lo <-function(x){
  alpha <- 0.05
  B <- length(x)
  lo <- sort(x)[B*alpha/2]
  return(lo)
}


myquantiles.hi <-function(x){
  alpha <- 0.05
  B <- length(x)
  up <- sort(x)[B*(1-alpha/2)]
  return(up)
}


myquantiles <-function(x){
  alpha <- 0.05
  B <- length(x)
  up <- sort(x)[B*(1-alpha)]
  return(up)
}


## find index name
find.index.remove <- function(x,names.use){
  out <- which(names(dimnames(x)) %in% names.use)  ## which (list of dimension names %in% names.use)  ## find the order number
  return(out)
}




convert.matrix.to.data.plot <- function(index.b.cut,index_a,bvalues,estimate,label.names){

  data.plot <- NULL
  for(ll in 1:length(index_a)){

    data.plot.tmp <- data.frame(x=bvalues[index.b.cut],y=estimate[index_a[ll],index.b.cut],Group=label.names[ll])

    #x         y Group
    #t40 40 0.2592511     A
    #t45 45 0.4252142     A
    #t50 50 0.5732175     A
    #t55 55 0.7003961     A
    #t60 60 0.7864215     A

    data.plot <- rbind(data.plot,data.plot.tmp)
  }

  colnames(data.plot) <- c("x","y","Group")
  rownames(data.plot) <- 1:nrow(data.plot)

  return(data.plot)
}

####################################################
## plot of function(a,b) over b at different a
##   add in number at risk using ggplot
####################################################

#ggplot.at.a.over.b function : function name is replaced by ggplot_at_a_over_b



## ggplot error bars
#################################################
## plot of function(a,b) over b at different a ##
##   add in number at risk using ggplot        ##
#################################################

#ggplot.error.bars function: name is replaced by ggplot_error_bars function


####################
## change dimension#
####################

make.array <- function(x){
  if(is.null(dim(x))){
    x <- t(data.frame(x))
  }
  return(x)
}



########################################################
## main plot used: input should be index_a by b-values #
########################################################

main.plot.results <- function(filename.set, param.names, out.true, out.mean,
                              bvalues, bvalues.cut, index_a,
                              plot.info, param.names.plot=param.names, ylab.use="", xlab.use=""){

  ## plot information
  ylim.use <- plot.info$ylim.use
  label.names <- plot.info$label.names
  color.list <- plot.info$color.list
  legend.position <- plot.info$legend.position
  conf.int <- plot.info$conf.int
  add.second.legend <- plot.info$add.second.legend
  data.extra <- plot.info$data.extra
  var.lo <- plot.info$var.lo
  var.hi <- plot.info$var.hi
  est <- plot.info$est  ## "est"



  for(uu in 1:length(param.names)){
    filename_use <- paste(filename.set,param.names[uu],sep="") ## give parameter name to filename

    ## estimate dim: ss,nn,bvalues x param.names, est
    if("theta" %in% names(dimnames(out.mean))){   ## [1] "time"  "theta" "val"
      estimate <- out.mean[,param.names[uu],est]   ## "est" at each time t40, t45, .., t60 for beta0 or beta1 or gamma..
      theta.array.lo <- out.mean[, param.names[uu], var.lo] ## "varlo" at each time t40, t45, .., t60 for beta0 or beta1 or gamma..
      theta.array.hi <- out.mean[, param.names[uu], var.hi] ## "varhi" at each time t40, t45, .., t60 for beta0 or beta1 or gamma..
    } else{
      ## used for alpha, Ft plots
      if(length(names(dimnames(out.mean)))>2){  # [1] "time"  "theta" "val"
        estimate <- adrop(out.mean[, , est, drop=FALSE],drop=3)  ## time by theta by est array ----> time by theta array (third dimesion is dropped)
        theta.array.lo <- adrop(out.mean[, , var.lo, drop=FALSE],drop=3)
        theta.array.hi <- adrop(out.mean[, , var.hi, drop=FALSE],drop=3)
      } else {
        estimate <- out.mean[,est]   ## does it have to be out.mean[,,est]?
        theta.array.lo <- out.mean[ , var.lo]
        theta.array.hi <- out.mean[ , var.hi]
      }
    }

    ## plot graph: Garcia 2017, supplementary material page 12
    plot.at.a.over.b(filename=filename_use, ## [1] "ss_cohort_hdage_nobase_comp_7c_jgamma"
                     thetat=out.true,
                     estimate=estimate,
                     theta.array.lo=theta.array.lo,
                     theta.array.hi=theta.array.hi,
                     index_a=index_a,  ##-10, -5, 0, 5, 10
                     bvalues=bvalues,  ## time_val
                     bvalues.cut=bvalues.cut,
                     color.list=color.list,
                     legend.position=legend.position,
                     ylim=ylim.use[param.names.plot[uu],],  ## param.names.plot[uu]?? ylim.use has min and max of beta0, beta1, gamma, alphax, alphat, Ft
                     conf.int=conf.int,
                     label.names=label.names,
                     add.second.legend=add.second.legend,
                     data.extra=data.extra
    )


    #              ggplot_at_a_over_b(filename=paste("gg_",filename_use,sep=""),
    #                        estimate=estimate,
    #                        theta.array.lo=theta.array.lo,
    #                        theta.array.hi=theta.array.hi,
    #                                        index_a=index_a,
    #                                        bvalues=bvalues,
    #                                        bvalues.cut=bvalues.cut,
    #                                        color.list=color.list,
    #                                        nrisk=NULL,
    #                                         ## to make y-label closer to y-axis
    #                                        margin.move=unit(c(0,-10,0,0),"mm"),
    #                                        ##cex.line=1,cex.size=12,
    #                                        ylim=ylim.use[param.names[uu],],
    #                                        conf.int=conf.int,
    #					label.names=label.names,
    #                                        ylab.use=ylab.use,
    #                                        xlab.use=xlab.use,
    #                                        add.second.legend=add.second.legend)


  }
}




## plot of function f(a,b) over b at different a


#' @importFrom survival survfit
#' @import graphics
plot.at.a.over.b <- function(filename,
                             thetat=NULL,
                             estimate,
                             theta.array.lo=NULL,
                             theta.array.hi=NULL,
                             index_a,
                             bvalues,#=time_val,
                             bvalues.cut,#=time.cut,
                             color.list,
                             cex.lab=3,cex.axis=3,lwd=5,cex.legend=3,
                             legend.position="topright",ylim=NULL,
                             conf.int=FALSE,label.names=NULL,
                             ylab.use="",xlab.use="",
                             add.second.legend=FALSE,
                             data.extra=NULL){


  print(filename)
  ## make sure dimensions are  correct
  estimate <- make.array(estimate)

  if(!is.null(thetat)){
    thetat <- make.array(thetat)  ## change dimensions
  }

  if(!is.null(theta.array.lo)){
    theta.array.lo <- make.array(theta.array.lo)
  }

  if(!is.null(theta.array.hi)){
    theta.array.hi <- make.array(theta.array.hi)
  }


  ## update where to cut b-values
  if(!is.null(bvalues.cut)){

    index.cut.use <- which(as.numeric(bvalues) %in% bvalues.cut)

    if(length(index.cut.use) <2){
      index.b.cut <- 1:index.cut.use
    } else {
      index.b.cut <- seq(from=index.cut.use[1],to=index.cut.use[2],by=1)
    }
  } else {
    index.b.cut <- 1:length(bvalues)   ## 1 2 3 4 5
  }

  ## update ylim
  if(is.null(ylim)){
    ylim.criteria <- c(thetat[index_a,index.b.cut],
                       estimate[index_a,index.b.cut])

    if(conf.int==TRUE){
      ylim.criteria <- c(ylim.criteria,
                         thetat[index_a,index.b.cut],
                         theta.array.lo[index_a,index.b.cut],
                         theta.array.hi[index_a,index.b.cut])
    }
    ylim <- c(min(ylim.criteria),max(ylim.criteria))
  }

  ## update xlim
  xlim <- c(min(as.numeric(bvalues[index.b.cut])),
            max(as.numeric(bvalues[index.b.cut])))


  ## start plot
  ##postscript(paste(filename,".eps",sep=""))
  #x11()
  graphics::par(mar=c(5.1, 5.1, 4.1, 2.1))
  graphics::plot(1,type="n",xlab=xlab.use,ylab=ylab.use,
       xlim=xlim,ylim=ylim,cex.axis=cex.axis,cex.lab=cex.lab)

  tmp <- 0


  for(i in index_a){
    tmp <- tmp+1

    if(!is.null(thetat)){
      graphics::lines(bvalues[index.b.cut],thetat[i,index.b.cut],
            col=color.list[tmp],lty=1,lwd=lwd)
      tmp <- tmp + 1
    }

    ## plot estimate at a over b
    graphics::lines(bvalues[index.b.cut],
          estimate[i,index.b.cut],col=color.list[tmp],lty=1,lwd=lwd)

    ## plot confidence intervals at a over b
    if(conf.int==TRUE){
      graphics::lines(bvalues[index.b.cut],
            theta.array.lo[i,index.b.cut],col=color.list[tmp],lty=3,lwd=lwd)
      graphics::lines(bvalues[index.b.cut],
            theta.array.hi[i,index.b.cut],col=color.list[tmp],lty=4,lwd=lwd)
    }
  }

  ####################
  ## set up KM data ##
  ####################
  if(!is.null(data.extra)){
    ## plot other
    if(!is.null(data.extra$estimate.other)){
      estimate.other <-  data.extra$estimate.other
      theta.array.lo.other <- data.extra$theta.array.lo.other
      theta.array.hi.other <- data.extra$theta.array.hi.other

      tmp <- tmp + 1
      graphics::lines(bvalues[index.b.cut],estimate.other[i,index.b.cut],col=color.list[tmp],lty=1,lwd=lwd)
      if(conf.int==TRUE){
        graphics::lines(bvalues[index.b.cut],
              theta.array.lo.other[i,index.b.cut],col=color.list[tmp],lty=3,lwd=lwd)
        graphics::lines(bvalues[index.b.cut],
              theta.array.hi.other[i,index.b.cut],col=color.list[tmp],lty=4,lwd=lwd)
      }
    }

    ## Find the analysis.run which has data.extra
    if(!is.null(data.extra$zz_data)){
      zz_data <- data.extra$zz_data
      xx_data <- data.extra$xx_data
      s_data <- data.extra$s_data
      delta_data<- data.extra$delta_data
      ss.names <- data.extra$ss.names
      xx_val.use <- data.extra$xx_val.use
      covariate_data <- data.extra$covariate_data
      ## sample
      index_all_data <- 1:nrow(xx_data)

      ## find people with CAG repeat xx_val[i]
      index_xx_data <- which(round(xx_data,3)%in%round(xx_val.use[i],3))

      ## find people with matching covariates
      index_zz_data <- apply(zz_data, 1,
                             function(r) all(r %in% covariate_data))

      index_zz_data <- index_all_data[index_zz_data]

      ## find intersection of people with CAG specified and z-covariates
      index_use_data <- intersect(index_xx_data,index_zz_data)

      ## form data set: get event time and delta
      myKMdata <- data.frame(time=s_data[index_use_data,ss.names],
                             delta=delta_data[index_use_data,
                                              paste("delta.",ss.names,sep="")])


      ## form KM estimate and add estimate to plot
      tmp <-  tmp + 1
      graphics::lines(survfit(Surv(time,delta)~1,data=myKMdata,conf.int=conf.int),col=color.list[tmp],
            fun="event",lwd=lwd)
    }
  }



  if(!is.null(legend.position)){
    graphics::legend(legend.position,legend=label.names,
           col=color.list[1:tmp],
           lty=rep(1,length(tmp)),lwd=rep(lwd,length(tmp)),
           bty="n",cex=cex.legend)
  }

  if(add.second.legend==TRUE){
    graphics::legend("bottomright",legend=c("Estimate","Upper 95% CI", "Lower 95% CI"),
           col=rep("black",3),
           lty=c(1,4,3),
           lwd=rep(lwd,3),
           bty="n",cex=cex.legend)

  }

  ##dev.off()
}






## plot counts
#' @import graphics
plot.counts <- function(filename,time_val,counts.array,lwd=3,cex.legend=3,cex.axis=3){
  data.count2 <- apply(counts.array,c(2,3),mean)

  #postscript(paste(filename,"_counts.eps",sep=""))
  graphics::plot(as.numeric(time_val),as.numeric(data.count2[,1]),col="blue",type="l",lwd=lwd,
       ylim=c(0,1),ylab="",xlab="",cex.axis=cex.axis)
  graphics::lines(as.numeric(time_val),as.numeric(data.count2[,2]),col="red",lty=1,lwd=lwd)
  graphics::lines(as.numeric(time_val),as.numeric(data.count2[,3]),col="green",lty=1,lwd=lwd)
  graphics::lines(as.numeric(time_val),as.numeric(data.count2[,4]),col="black",lty=1,lwd=lwd)
  graphics::legend("bottomright",legend=c("Y=1","Y=0, delta=1","Y=0, delta=0","Y in (0,1)"),col=c("blue","red","green","black"),
         lty=c(1,1,1,1),lwd=rep(lwd,4),bty="n",cex=cex.legend)
  #dev.off()
}

## IAC calculation
iac.results <- function(theta.array,theta.ci.lo.array,
                        theta.ci.hi.array,time_val,
                        time.name,iters.name){



  #iac.results( theta.array=adrop(beta.diff.array$out[,,,,,"est",drop=FALSE], drop=index.remove),
  #theta.ci.lo.array=adrop(beta.diff.array$out[,,,,,"varlo",drop=FALSE], drop=index.remove),
  #theta.ci.hi.array=adrop(beta.diff.array$out[,,,,,"varhi",drop=FALSE], drop=index.remove), time_val,
  #time.name="time",iters.name="iters")


  iac.tmp <- (theta.ci.lo.array <= theta.array) & (theta.array <= theta.ci.hi.array)
  ## sum over time
  iac.sum <- apply.index(iac.tmp,time.name,sum)
  iac.sum <- sweep(iac.sum,1:length(dim(iac.sum)),
                   length(time_val),FUN="==")
  iac.out <- apply.index(iac.sum,iters.name,mean)
  return(iac.out)
}

#######################
## parameter results ##
#######################

## compute coverages

compute.coverage <- function(theta.array,est.var.theta,thetat,nsimu,iters.name,
                             level=0.95,df=Inf){
  qtile <- level + (1-level)/2 # Compute the proper quantile

  ## estimate - truth
  est.minus.truth <- sweep(theta.array,find.apply.index(theta.array,iters.name),
                           thetat,FUN="-")

  ## abs(estimate-truth)/sqrt(variance)
  est.divide.var <- sweep(abs(est.minus.truth),
                          find.apply.index(est.minus.truth,iters.name),
                          sqrt(est.var.theta),FUN="/")

  mycov.tmp <- apply.index(est.divide.var,iters.name,pnorm)<qtile
  cov.prob <- apply.index(mycov.tmp,iters.name,sum)/nsimu
  return(cov.prob)
}

## is the true parameter in the confidence interval? (yes=1)

check.true.in.ci <- function(thetat,lower.bound,upper.bound,iters.name){
  out1 <- sweep(lower.bound,find.apply.index(lower.bound,iters.name),  ## 2, 3, 4, 5, 6, (difference)
                thetat,FUN="<=") ##  logical value: ture false if lower.bound < = thetat
  out2 <- sweep(upper.bound,find.apply.index(upper.bound,iters.name),
                thetat, FUN=">=")  # Return an array obtained from an input array by sweeping out a summary statistic.
  out <- out1 & out2
  return(out)
}




compute.coverage2 <- function(theta.array,theta.var.array,thetat,nsimu,iters.name,
                              level=0.95,df=Inf){   # theta.array,theta.var.array,thetat,nsimu,iters.name,level=0.95,df=Inf

  qtile <- level + (1-level)/2 # Compute the proper quantile, 0.975

  lower.bound <- theta.array - qt(qtile,df=df) * sqrt(theta.var.array) ## lower.bound[,,1,1,1]
  upper.bound <- theta.array + qt(qtile,df=df) * sqrt(theta.var.array)
  true.in.ci <- check.true.in.ci(thetat,lower.bound,upper.bound,iters.name)
  cp <- apply.index(true.in.ci,iters.name,mean)  ## average of whether ture estimates are in confidence interval over iters.

  mc.lower.bound <- cp - 1.96 * sqrt((cp*(1-cp))/nsimu) # Monte Carlo error
  mc.upper.bound <- cp + 1.96 * sqrt((cp*(1-cp))/nsimu) # Monte Carlo error
  #  return(list(coverage.probability=cp,
  #                true.in.ci= true.in.ci,
  #                ci=cbind(lower.bound,upper.bound),
  #                mc.eb=c(mc.lower.bound,mc.upper.bound)))
  return(cp)
}

## for individual level coverages

coverage <- function(b,se,true,level=0.95,df=Inf){ ## estimate,
  ## standard error,
  ## true parameter,
  ## confidence level,
  ## and df
  qtile <- level + (1-level)/2 # Compute the proper quantile

  lower.bound <- b - qt(qtile,df=df)*se # lower bound
  upper.bound <- b + qt(qtile,df=df)*se # upper bound

  ## is the true parameter in the confidence interval? (yes=1)
  true.in.ci <- ifelse(true >= lower.bound & true <= upper.bound,1,0)
  cp <- mean(true.in.ci) ## The coverage probability
  mc.lower.bound <- cp - 1.96 * sqrt((cp*(1-cp))/length(b)) # Monte Carlo error
  mc.upper.bound <- cp + 1.96 * sqrt((cp*(1-cp))/length(b)) # Monte Carlo error
  #  return(list(coverage.probability=cp,
  #                true.in.ci= true.in.ci,
  #                ci=cbind(lower.bound,upper.bound),
  #                mc.eb=c(mc.lower.bound,mc.upper.bound)))
  return(cp)
}



parameter.results <- function(thetat,theta.array,
                              theta.var.array,iters.name,time.name,nsimu){

  ## mean of estimate over iterations at the same xks
  mean.theta <- apply.index(theta.array,iters.name,mean)

  ## variance of estimate over iterations
  emp.var.theta <- apply.index(theta.array,iters.name,var)

  ## mean of estimated variance over iterations
  est.var.theta <- apply.index(theta.var.array,iters.name,mean)

  ## needed terms
  bias <- mean.theta - thetat
  abs.bias <- abs(bias)
  mse <- bias^2 + est.var.theta

  ###########################################
  ## computation of coverage probabilities ##
  ###########################################
  #cov.prob <- compute.coverage(theta.array,est.var.theta,
  #			thetat,nsimu,iters.name,level=0.95,df=Inf)

  cov.prob <- compute.coverage2(theta.array,theta.var.array,
                                thetat,nsimu,iters.name,level=0.95,df=Inf)  ## whether montecarlo lower bound and upper bound, whether true estimates covers by confidence interval: 0 or 1
  #####################
  ## average results ##
  #####################
  ## average taken over time
  avg.abs.bias <- apply.index(abs.bias,time.name,mean,na.rm=TRUE) ## average absolute bise over time = average arrays for beta0, beta1 and gamma
  avg.bias <- apply.index(bias,time.name,mean,na.rm=TRUE) ## average biase over time
  avg.emp.var <- apply.index(emp.var.theta,time.name,mean,na.rm=TRUE) ## emprirical variance ==Nan
  avg.est.var <- apply.index(est.var.theta,time.name,mean,na.rm=TRUE) ## aysmptotic variance
  avg.cov <- apply.index(cov.prob,time.name,mean,na.rm=TRUE) ## average of coverage probability
  avg.mse <- apply.index(mse,time.name,mean,na.rm=TRUE)  ## average of mean sqaured errors

  ## combine average results
  avg.out <- avg.abs.bias   ## avr estimates over iters at times
  avg.out <- abind(avg.out,avg.bias,along=0) # binds on new dimension before first: (**)new dimesion by study_names by event names, xks, thetat
  ## new dimesion name will be given in the new.names
  avg.out <- abind(avg.out,avg.emp.var,along=1) ## rbind of avg.out, avg.emp.var
  avg.out <- abind(avg.out,avg.est.var,along=1)
  avg.out <- abind(avg.out,avg.cov,along=1)
  avg.out <- abind(avg.out,avg.mse,along=1)

  new.names <- c(list(results=c("abs bias","bias","emp var","est var",
                                "95% cov", "MSE")),dimnames(avg.abs.bias))

  dimnames(avg.out) <- new.names

  #######################
  ## Pointwise results ##
  #######################
  pt.out <- bias
  pt.out <- abind(pt.out,emp.var.theta,along=0)
  pt.out <- abind(pt.out,est.var.theta,along=1)
  pt.out <- abind(pt.out,cov.prob,along=1)
  pt.out <- abind(pt.out,mse,along=1)
  new.names <- c(list(results=c("bias","emp var","est var",
                                "95% cov", "MSE")),dimnames(bias))
  dimnames(pt.out) <- new.names
  list(avg.out=avg.out,pt.out=pt.out)  ## combined results for average results and pointwise results.
}




###############################
## put data in form of table ##
###############################

extract.index <- function(tt_choose,
                          xx_choose,xx_index,zz_choose,out.flatten){

  ## index.use <- extract.index(tt_choose=time_choice,
  ##                            xx_choose,xx_index,zz_choose,out.flatten)


  index.extract <- 1:nrow(out.flatten)

  ## find patterns
  if(!is.null(tt_choose)){
    index.extract <- which(out.flatten$time %in% paste("t",tt_choose,sep="")) ## 40, 45, 50, 55, 60
  }

  if(!is.null(xx_choose)){
    index.extract2 <- which(out.flatten$xx %in% paste("xx",xx_index,sep="")) ## 42, 44, 46
  } else{
    index.extract2 <- index.extract
  }

  if(!is.null(zz_choose)){
    index.extract3 <- which(out.flatten$zz %in% zz_choose)
  } else{
    index.extract3 <- index.extract
  }

  index.extract.all <- intersect(index.extract,index.extract2)
  index.extract.all <- intersect(index.extract.all,index.extract3)


  return(index.extract.all)
}



organize.data.diff <- function(out,xx_choose,zz_choose,theta){

  #out=beta.diff.results,
  #xx_choose=NULL,
  #zz_choose=NULL,
  #theta=NULL




  ## add a dimension for dummy
  new.names <- list(dummy=list("dummy"),dimnames(out))
  out  <- array(out,dim=c(1,dim(out)),
                dimnames=unlist(new.names,recursive=FALSE)) ## new names for dummy with out

  ## flatten the results according to dummy
  out.flatten <- flatten.array(out,dim.order=names(dimnames(out)),
                               flatten.name="dummy",theta=theta)

  ## add latex labels
  theta.label <- "theta"
  event.label <- "event"
  xx.label <- "xx"
  zz.label <- "zz"

  ## extract necessary components to form theta_{event,study,subscript}
  theta.name <- extract.character(out.flatten[,theta.label])
  theta.subscript <- extract.subscript(out.flatten[,theta.label])
  event.subscript <- extract.subscript(out.flatten[,event.label])

  if(xx.label %in% colnames(out.flatten)){
    xx.subscript <- extract.subscript(out.flatten[,xx.label])
  }


  if(zz.label %in% colnames(out.flatten)){
    zz.subscript <- extract.subscript(out.flatten[,zz.label])
  }

  ## create latex label
  latex.label <- rep("%NONE",length(theta.name))
  index.tmp <- 1:length(theta.name)




  if("beta" %in% theta.name){
    index.use <- grepl("beta*",theta.name) & !grepl("0",theta.subscript)
    index.use <- index.tmp[index.use]
    latex.label[index.use] <- paste("$H_0:\\",theta.name[index.use],
                                    "_{",event.subscript[index.use],"s",
                                    theta.subscript[index.use],"}(t)","=\\",
                                    theta.name[index.use],
                                    "_{",event.subscript[index.use],"s'",
                                    theta.subscript[index.use],"}(t)$",
                                    sep="")
  }


  if("omega" %in% theta.name){
    index.use <- grepl("omega*",theta.name)
    index.use <- index.tmp[index.use]
    latex.label[index.use] <-    paste("$H_0:\\",theta.name[index.use],
                                       "_{s}(t)=\\",
                                       theta.name[index.use],
                                       "_{s'}(t)$",
                                       sep="")
  }

  if("gamma" %in% theta.name){
    index.use <- grepl("gamma*",theta.name)
    index.use <- index.tmp[index.use]
    latex.label[index.use] <-    paste("$H_0:\\",theta.name[index.use],
                                       "_{s}(t)=\\",
                                       theta.name[index.use],
                                       "_{s'}(t)$",
                                       sep="")

  }

  if("alpha" %in% theta.name){
    latex.label <- paste("$H_0:\\",theta.name,
                         "_{",event.subscript, "s","}(",
                         xx.subscript,",t)=\\",theta.name,
                         "_{",event.subscript, "s'","}(",
                         xx.subscript,",t)$",
                         sep="")
  }

  if("F" %in% theta.name){
    latex.label <- paste("$H_0:\\",theta.name,
                         "_{",event.subscript, "s","}(t|x=",
                         xx.subscript,", z=",
                         zz.subscript,")=\\",theta.name,
                         "_{",event.subscript, "s'","}(t|x=",
                         xx.subscript,",z=",
                         zz.subscript,")$",
                         sep="")
  }

  out.flatten <- cbind(latex.label,out.flatten)

  ## extract index of interest
  out.index <- extract.index(tt_choose=NULL,
                             xx_choose,xx_index=xx_choose,zz_choose,out.flatten)

  if(length(out.index!=0)){
    out.flatten <- out.flatten[out.index,]
  }
  return(out.flatten)
}




organize.results <- function(out,num_study,
                             tt_choose,xx_choose,zz_choose,
                             theta){

  ## results
  avg.out <- out$avg.out
  pt.out <- out$pt.out



  avg.flatten <- flatten.array(avg.out,dim.order=names(dimnames(avg.out)),
                               flatten.name="results",theta=theta)  ## array to matrix
  index.tail <- which(colnames(avg.flatten) %in% tail(colnames(avg.flatten),  ## last 6 column names will be returned
                                                      n=length(dimnames(avg.out)$results)))   ## n=6, dimnames(avg.out)$results
  ## [1] "abs bias" "bias"     "emp var"  "est var"  "95% cov"  "MSE"

  colnames(avg.flatten)[index.tail] <- dimnames(avg.out)$results  ##  "abs bias" "bias"     "emp var"  "est var"  "95% cov"  "MSE"


  ## similarly
  pt.flatten <- flatten.array(pt.out,dim.order=names(dimnames(pt.out)),
                              flatten.name="results",theta=theta)
  index.tail <- which(colnames(pt.flatten) %in% tail(colnames(pt.flatten),
                                                     n=length(dimnames(pt.out)$results)))
  colnames(pt.flatten)[index.tail] <- dimnames(pt.out)$results


  ## get the labels
  study.label <- "study"
  event.label <- "event"
  time.label <- "time"
  theta.label <- "theta"
  xx.label <- "xx"
  zz.label <- "zz"

  ## add the labels
  avg.flatten <- make.latex.labels(study.label,event.label,
                                   theta.label,time.label,xx.label,zz.label,
                                   avg.flatten,label.type="avg")  ## latex labels

  pt.flatten <-  make.latex.labels(study.label,event.label,
                                   theta.label,time.label,xx.label,zz.label,
                                   pt.flatten,label.type="ptwise")


  ## extract index of data associated with pre-specified x, z, t values
  if(time.label %in% colnames(avg.flatten)){
    tt_choose.use <- tt_choose
  } else {
    tt_choose.use <- NULL
  }

  if(xx.label %in% colnames(avg.flatten)){
    xx_choose.use <- xx_choose
    xx_index.use <- xx_choose
  } else {
    xx_choose.use <- NULL
    xx_index.use <- NULL
  }

  avg.index <- extract.index(tt_choose.use,
                             xx_choose.use,xx_index.use,zz_choose,avg.flatten)

  pt.index <- extract.index(tt_choose,
                            xx_choose,xx_index=xx_choose,zz_choose,pt.flatten)

  avg.flatten <- avg.flatten[avg.index,]
  pt.flatten <- pt.flatten[pt.index,]

  ## put study information side by side
  for(ss in 1:num_study){
    index.avg.study <- which(avg.flatten[,study.label]==ss)
    index.pt.study <- which(pt.flatten[,study.label]==ss)

    avg.latex.label.tmp <- avg.flatten[index.avg.study,
                                       "latex.label"]
    pt.latex.label.tmp <- pt.flatten[index.pt.study,
                                     "latex.label"]
    if(ss < 2){
      avg.latex.label.use <- avg.latex.label.tmp
      pt.latex.label.use <- pt.latex.label.tmp
    } else{
      omega.avg.index <- grep("*omega*",avg.latex.label.tmp)
      omega.pt.index <- grep("*omega*",pt.latex.label.tmp)

      avg.latex.label.use[omega.avg.index] <- avg.latex.label.tmp[omega.avg.index]
      pt.latex.label.use[omega.pt.index] <- pt.latex.label.tmp[omega.pt.index]
    }

    if(ss <2){
      avg.flatten.out <- avg.flatten[index.avg.study,c(dimnames(avg.out)$results)]
      pt.flatten.out <- pt.flatten[index.pt.study,c(dimnames(pt.out)$results)]
    } else {
      avg.flatten.out <- cbind(
        avg.flatten.out,
        avg.flatten[index.avg.study,c(dimnames(avg.out)$results)])
      pt.flatten.out <- cbind(
        pt.flatten.out,
        pt.flatten[index.pt.study,c(dimnames(pt.out)$results)])
    }
  }
  avg.flatten.out <- cbind(latex.label=avg.latex.label.use,avg.flatten.out)
  pt.flatten.out <- cbind(latex.label=pt.latex.label.use,pt.flatten.out)

  list(avg.flatten.out=avg.flatten.out,
       pt.flatten.out=pt.flatten.out)
}






## extract number portion from vector

extract.subscript <- function(x){
  #return(gsub("[^[:digit:]]+\\.*[^[:digit:]]", "", x))
  #return(gsub("[^[:digit:]]", "", x))
  return(as.numeric(unlist(regmatches(x,   # regmatches extracts the matched substrings as specified by the match data
                                      gregexpr("[[:digit:]]+\\.*[[:digit:]]*",x)))))   ## For vector match data (as obtained from regexpr), empty matches are dropped;
  ## for list match data, empty matches give empty components
  ## gregexpr  (pattern, text)
  ## [[:digit:]] === regular syntax for atch a numerical digit.
  ## + :plus sign, *: The preceding item will be matched zero or more times.
}

## extract character portion from vector

extract.character <- function(x){
  return(gsub("[[:digit:]]","",x))

  ## gsub(pattern, replacement, x): replaces all matches of a string ,,[[:digit:]]: Digits: 0 1 2 3 4 5 6 7 8 9
}


## function to make latex labels

make.latex.labels <- function(study.label,event.label,
                              theta.label,time.label,xx.label,zz.label,
                              out.flatten,label.type="avg"){

  # make.latex.labels(study.label,event.label,
  #                    theta.label,time.label,xx.label,zz.label,
  #                    avg.flatten,label.type="avg")

  ## extract necessary components to form theta_{event,study,subscript}
  theta.name <- extract.character(out.flatten[,theta.label]) ## extract character portion of theta.label
  theta.subscript <- extract.subscript(out.flatten[,theta.label]) ## extract.number portion of theta.label
  study.subscript <- extract.subscript(out.flatten[,study.label])
  event.subscript <- extract.subscript(out.flatten[,event.label])

  if(xx.label %in% colnames(out.flatten)){
    xx.subscript <- extract.subscript(out.flatten[,xx.label])
  }

  if(time.label %in% colnames(out.flatten)){
    time.subscript <- extract.subscript(out.flatten[,time.label])
  }

  if(zz.label %in% colnames(out.flatten)){
    zz.subscript <- extract.subscript(out.flatten[,zz.label])
  }

  ## create latex label
  latex.label <- rep("%NONE",length(theta.name))
  index.tmp <- 1:length(theta.name)

  if("beta" %in% theta.name){
    index.use <- grepl("beta*",theta.name) & !grepl("0",theta.subscript)  ## matches character: ture or false
    ## warning messages
    index.use <- index.tmp[index.use]  ## 10, 11, 12, 13, 14, 15, 16, 17, 18 where beta1's

    latex.label[index.use] <- paste("$\\wh\\",theta.name[index.use],
                                    "_{",event.subscript[index.use],"s",
                                    theta.subscript[index.use],"}",sep="")

    if("beta0" %in% out.flatten[,theta.label]){
      ## only label first beta0
      index.beta0 <- which(out.flatten[,theta.label]=="beta0")
      if(label.type=="avg"){
        index.beta0 <- index.beta0[1]
      }
      latex.label[index.beta0] <- "$\\wh\\beta_{0}"
    }
  }

  if("gamma" %in% theta.name){
    ## only get gamma for events >1
    index.use <- grepl("gamma*",theta.name) & !grepl("1",event.subscript)
    index.use <- index.tmp[index.use]
    latex.label[index.use] <-  paste("$\\wh\\",theta.name[index.use],
                                     "_{",event.subscript[index.use],"}",sep="")
  }

  if("omega" %in% theta.name){
    ## only get omega for study >1
    index.use <- grepl("omega*",theta.name) & !grepl("1",study.subscript)
    index.use <- index.tmp[index.use]

    latex.label[index.use] <- paste("$\\wh\\",theta.name[index.use],
                                    "_{",study.subscript[index.use],"}",sep="")

  }

  if("alpha" %in% theta.name | "F" %in% theta.name){
    latex.label <- paste("$\\wh\\",theta.name,
                         "_{",event.subscript, "s","}",sep="")
  }

  if(label.type=="avg"){
    if("alpha" %in% theta.name){
      if(xx.label %in% colnames(out.flatten)){
        latex.label <- paste(latex.label,"(",xx.subscript,",\\cdot)$",sep="")
      }

      if(time.label %in% colnames(out.flatten)){
        latex.label <- paste(latex.label,"(\\cdot,",time.subscript,")$",sep="")
      }
    } else if ("F" %in% theta.name){
      latex.label <- paste(latex.label,"(\\cdot| x=",xx.subscript,", z=",zz.subscript,")$",sep="")
    } else {
      latex.label <- paste(latex.label,"(\\cdot)$",sep="")
    }

  } else if(label.type=="ptwise"){
    if("alpha" %in% theta.name){
      latex.label <- paste(latex.label,"(",xx.subscript,",",time.subscript,")$",sep="")
    } else if ("F" %in% theta.name){
      latex.label <- paste(latex.label,"(",time.subscript,"| x=",xx.subscript,", z=",zz.subscript,")$",sep="")
    } else {
      latex.label <- paste(latex.label,"(",time.subscript,")$",sep="")
    }
  }
  ## add latex name
  out <- cbind(latex.label,out.flatten)
  return(out)
}







#############################################
## get 95\% CI for time points of interest ##
##   and lb/xx of interest                 ##
#############################################

#get.cis function in main2.R



## function to print out where differences noted

print.differences <- function(out,name.interest,check.reject.H0=TRUE){

  ############(out=Ft.cis,name.interest="sign.change",check.reject.H0=FALSE)
  ######### out=beta.cis,name.interest="sign.change",check.reject.H0=FALSE

  if(check.reject.H0==TRUE){
    change.check <- which(out[,name.interest]==1)
  } else{
    change.check <- which(out[,name.interest]==0)
  }

  if(length(change.check)>0){
    ## report differences
    foo <- out[change.check,]
    print(foo)
  } else {
    print("Study results similar")
  }
}

##########################################
## Function to compute Pr(T<t|T>= t0)   ##
##########################################



get.predicted.survival <- function(theta.array,
                                   xx_val.use,xx_choose,
                                   tt_val,time_choice0,time_choice0_toadd,convert,xmin,xmax,np){

  if(convert==TRUE){
    x <- convert.cag(xx_val.use,xmin=xmin,xmax=xmax)
    cag.uniform <- reverse.cag(xx_choose,xmin=xmin,xmax=xmax)
  } else {
    x <- xx_val.use
    cag.uniform <- xx_choose
  }

  xx_index <- which(round(xx_val.use,3) %in% round(cag.uniform,3))
  theta.array.new <- theta.array[,xx_index,,drop=FALSE]

  out.array <- array(0,dim=c(np,
                             length(xx_index)*length(time_choice0),2+length(time_choice0_toadd)),
                     dimnames=list(paste("np",1:np,sep=""),
                                   1:(length(xx_index)*length(time_choice0)),
                                   c("xx","tt0",paste("tt",time_choice0_toadd,sep=""))))

  for(nn in 1:np){
    tmp <- 0
    for(ii in 1:length(xx_index)){
      for(jj in 1:length(time_choice0)){
        tmp <- tmp + 1

        prob.tmp <- (theta.array.new[nn,ii,paste("t",time_choice0[jj]+time_choice0_toadd,sep="")]-
                       theta.array.new[nn,ii,paste("t",time_choice0[jj],sep="")])/
          (1-theta.array.new[nn,ii,paste("t",time_choice0[jj],sep="")])

        out.array[nn,tmp,] <- c(x[xx_index[ii]],time_choice0[jj],prob.tmp)
      }
    }
  }

  return(out.array)
}


##########################################
## Function to compute Slopes: Pr(T<t)-Pr(T<t0) ##
##########################################

get.slopes <- function(theta.array,
                       xx_val.use,xx_choose,
                       tt_val,time_choice0,time_choice0_toadd,convert,xmin,xmax,np){

  if(convert==TRUE){
    x <- convert.cag(xx_val.use,xmin=xmin,xmax=xmax)
    cag.uniform <- reverse.cag(xx_choose,xmin=xmin,xmax=xmax)
  } else {
    x <- xx_val.use
    cag.uniform <- xx_choose
  }

  xx_index <- which(round(xx_val.use,3) %in% round(cag.uniform,3))
  theta.array.new <- theta.array[,xx_index,,drop=FALSE]

  out.array <- array(0,dim=c(np,
                             length(xx_index)*length(time_choice0),2+length(time_choice0_toadd)),
                     dimnames=list(paste("np",1:np,sep=""),
                                   1:(length(xx_index)*length(time_choice0)),
                                   c("xx","tt0",paste("tt",time_choice0_toadd,sep=""))))

  for(nn in 1:np){
    tmp <- 0
    for(ii in 1:length(xx_index)){
      for(jj in 1:length(time_choice0)){
        tmp <- tmp + 1

        prob.tmp <- (theta.array.new[nn,ii,paste("t",time_choice0[jj]+time_choice0_toadd,sep="")]-
                       theta.array.new[nn,ii,paste("t",time_choice0[jj],sep="")])

        out.array[nn,tmp,] <- c(x[xx_index[ii]],time_choice0[jj],prob.tmp)
      }
    }
  }

  return(out.array)
}



## function to report xtable results

#' @import xtable
myprint <- function(foo){
  print(xtable(data.frame(row = rownames(foo),data.frame(foo))),
        sanitize.text.function=function(x){x},
        include.rownames = FALSE)
}


############################################
## functions for sample size calculations ##
############################################
##d = 4(z_{1-\alpha/2} + z_{1-\beta})^2 / \theta_R^2

compute.expected.deaths <- function(type1.error,type2.error,
                                    treatment.effect){

  out <- 4 * (qnorm(1-type1.error/2) + qnorm(1-type2.error))^2/treatment.effect^2
  return(out)
}


## sample size from Collett

compute.sample.size<- function(type1.error,type2.error,
                               treatment.effect,St,
                               dropout.rate){

  expected.deaths <- compute.expected.deaths(type1.error,type2.error,
                                             treatment.effect)

  out <- 2*expected.deaths / (2- (St + St^(exp(treatment.effect))) )
  out <- out/ (1-dropout.rate)
  return(out)
}



## \var(n)

compute.sample.size.variance <- function(type1.error,type2.error,
                                         treatment.effect,St,var.St,
                                         dropout.rate){

  expected.deaths <- compute.expected.deaths(type1.error,type2.error,
                                             treatment.effect)
  g.prime <- 2*expected.deaths *
    (1+exp(treatment.effect)* St^(exp(treatment.effect)-1))/
    (2-(St + St^(exp(treatment.effect))))^2

  out <- g.prime^2 * var.St
  out <- out /(1-dropout.rate)^2

  return(out)
}

## compute St=1-Ft

compute.survival <- function(x){
  out <- 1-x
  return(out)
}

## compute S1(t)=S0(t)^{\exp(thetaR)}

compute.survival.treatment <- function(S0t,treatment.effect){
  out <- S0t^(exp(treatment.effect))
  return(out)
}

## function to create the array for sample sizes

get.sample.size <- function(out.Ft,type1.error,
                            type2.error,
                            treatment.effect,
                            study.names,
                            event.names,#=s.names,
                            z_lab_names,
                            x_lab_names,
                            time_choice.predicted,
                            time_choice.predicted.toadd,
                            dropout.rate
){


  ## variables of interest
  val.interest <- c("Splacebo","Streat","sample.size","sd.sample.size",
                    "ci.lo","ci.hi")

  ## create output array to match the dimension of out.Ft
  ##  but change the "val" options
  val.index <- which(names(dimnames(out.Ft))=="val") ## index for val

  # ###########################
  # # edit here on Oct/11/19 ##
  # ###########################
  # if(is.null(treatment.effect)==TRUE){
  #
  #   new.names <- list(dimnames(out.Ft)[-val.index],
  #                     treateffect=list(paste("treat=", NULL,sep="")),
  #                     val=list(val.interest)) ## add list of treateeffect and val
  #
  # }else{

  new.names <- list(dimnames(out.Ft)[-val.index],
                    treateffect=list(paste("treat=",exp(treatment.effect),sep="")),
                    val=list(val.interest)) ## add list of treateeffect and val

  out <- array(0,dim=c(dim(out.Ft)[-val.index],length(treatment.effect),
                       length(val.interest)),
               dimnames=unlist(new.names,recursive=FALSE))

  # }







  for(ss in 1:length(study.names)){

    for(ee in 1:length(event.names)){

      for(zz in 1:length(z_lab_names)){

        for(xx in 1:length(x_lab_names)){

          for(tt in 1:length(time_choice.predicted)){

            for(tt0 in 1:length(time_choice.predicted.toadd)){

              F0t.tmp <- out.Ft[ss,ee,zz,xx,tt,tt0,"est"]
              S0t.tmp <- compute.survival(F0t.tmp)
              S0t.var.tmp <- out.Ft[ss,ee,zz,xx,tt,tt0,"varest"]

              ###########################
              # edit here on Oct/11/19 ##
              ###########################

              # if(is.null(treatment.effect)==FALSE){

              for(rr in 1:length(treatment.effect)){
                treat.tmp <- treatment.effect[rr]

                ## S0(t)
                out[ss,ee,zz,xx,tt,tt0,rr,"Splacebo"] <- S0t.tmp

                ## S1(t)
                out[ss,ee,zz,xx,tt,tt0,rr,"Streat"] <-
                  compute.survival.treatment(S0t.tmp,
                                             treat.tmp)

                ## sample size
                sample.size.tmp <- compute.sample.size(type1.error,type2.error,
                                                       treat.tmp,S0t.tmp,dropout.rate)

                out[ss,ee,zz,xx,tt,tt0,rr,"sample.size"] <- sample.size.tmp

                ## sample size variance: var(n)
                sample.size.var.tmp <-
                  compute.sample.size.variance(type1.error,type2.error,
                                               treat.tmp,S0t.tmp,S0t.var.tmp,dropout.rate)

                out[ss,ee,zz,xx,tt,tt0,rr,"sd.sample.size"] <- sqrt(sample.size.var.tmp)

                ## lower and upper confidence interval for sample size
                out[ss,ee,zz,xx,tt,tt0,rr,"ci.hi"] <-
                  sample.size.tmp + qnorm(0.975)* sqrt(sample.size.var.tmp)
                out[ss,ee,zz,xx,tt,tt0,rr,"ci.lo"] <-
                  sample.size.tmp + qnorm(0.025)* sqrt(sample.size.var.tmp)
              }

              # 	      }else{
              #
              # 	        out[ss,ee,zz,xx,tt,tt0,rr,"Splacebo"]<-NULL;
              # 	        out[ss,ee,zz,xx,tt,tt0,rr,"Streat"]<-NULL;
              # 	        out[ss,ee,zz,xx,tt,tt0,rr,"sample.size"]<-NULL;
              # 	        out[ss,ee,zz,xx,tt,tt0,rr,"sd.sample.size"]<-NULL;
              # 	        out[ss,ee,zz,xx,tt,tt0,rr,"ci.hi"]<-NULL;
              # 	        out[ss,ee,zz,xx,tt,tt0,rr,"ci.lo"]<-NULL;
              #
              #         }
            }
          }
        }
      }
    }
  }



  return(out)
}


########################################################
########################################################
##
##
## Functions to compute expected number of converters
##
##
########################################################
########################################################


################################################
## function to compute Kaplan-Meier estimator ##
################################################

get.kaplan.meier <- function(data.use,event.name,delta.name){
  data.tmp <- data.use
  ## use complete cases
  data.tmp <- data.tmp[complete.cases(data.tmp),]


  ## assing (time,status) variables to appropriate columns
  event.index <- which(colnames(data.use)==event.name)
  delta.index <- which(colnames(data.use)==delta.name)


  colnames(data.tmp)[event.index] <- "time"
  colnames(data.tmp)[delta.index] <- "status"




  km <- survfit(Surv(time,status)~1,data=data.tmp)
  survest <- stepfun(km$time,c(1,km$surv))
  return(survest)
}



get.kaplan.meier.fit <- function(data.use,event.name,delta.name){
  event.index <- which(colnames(data.use)==event.name)
  delta.index <- which(colnames(data.use)==delta.name)

  data.tmp <- data.use
  colnames(data.tmp)[event.index] <- "time"
  colnames(data.tmp)[delta.index] <- "status"

  ## use complete cases
  data.tmp <- data.tmp[complete.cases(data.tmp),]

  km <- survfit(Surv(time,status)~1,data=data.tmp)
  return(list(fit=km,data.out=data.tmp))
}




convert.factor.numeric <- function(x){
  return(as.numeric(levels(x))[x])

}

###############################
## function to fit CAP model ##
###############################

get.cap.model <- function(data.use,event.name,delta.name){
  event.index <- which(colnames(data.use)==event.name)
  delta.index <- which(colnames(data.use)==delta.name)

  data.tmp <- data.use
  colnames(data.tmp)[event.index] <- "time"
  colnames(data.tmp)[delta.index] <- "status"

  ## use complete cases
  data.tmp <- data.tmp[complete.cases(data.tmp),]

  ## fit model
  cap <- survreg(Surv(time,status)~ base_age * CAG,data=data.tmp,
                 dist="loglogistic")

  ## extract parameters and variance-covariance
  parameter.estimates <- cap$coefficients
  parameter.estimates <- c(parameter.estimates,log_scale=log(cap$scale))
  variance.estimates <- cap$var

  return(list(parameter.estimates=parameter.estimates,
              variance.estimates=variance.estimates))
}


##############################################
## function to fit estimates from CAP model ##
##############################################

fit.cap.model.estimates <- function(cap.fit,data.use,
                                    event.name,delta.name,sig.digits){
  ##################################
  ## Get information from CAP fit ##
  ##################################

  #########################
  ## Create output table ##
  #########################
  output <- array(0,dim=c(4,5),
                  dimnames=list(c("alpha","beta","C","log(sigma)"),
                                c("Estimate","VAR","SE","95% CI-lo",
                                  "95% CI-hi")))

  ########################
  ## Fill in the output ##
  ########################
  output["alpha","Estimate"] <-  cap.fit$parameter.estimates["(Intercept)"]
  output["alpha","VAR"] <-  cap.fit$variance.estimates["(Intercept)","(Intercept)"]

  output["beta","Estimate"] <- cap.fit$parameter.estimates["base_age:CAG"]
  output["beta","VAR"] <-  cap.fit$variance.estimates["base_age:CAG","base_age:CAG"]

  output["log(sigma)","Estimate"] <- cap.fit$parameter.estimates["log_scale"]
  output["log(sigma)","VAR"] <-  cap.fit$variance.estimates["Log(scale)","Log(scale)"]

  output["C","Estimate"] <- cap.fit$parameter.estimates["base_age"]/cap.fit$parameter.estimates["base_age:CAG"]
  gradient.term <- c(0,1/cap.fit$parameter.estimates["base_age:CAG"],0,
                     -cap.fit$parameter.estimates["base_age"]/cap.fit$parameter.estimates["base_age:CAG"]^2,0)
  output["C","VAR"] <- gradient.term %*% cap.fit$variance.estimates %*% (gradient.term)

  #############################
  ## Compute standard errors ##
  #############################
  output[,"SE"] <- sqrt(output[,"VAR"])

  ####################
  ## Compute 95% CI ##
  ####################
  output[,"95% CI-lo"] <- output[,"Estimate"]+ qnorm(0.025)*output[,"SE"]
  output[,"95% CI-hi"] <- output[,"Estimate"] + qnorm(0.975) * output[,"SE"]

  return(output)
}

##########################################################
## Function to compute F(t) and variance from CAP model ##
##########################################################

fit.cap.model.Ft <- function(cap.fit,time.use,CAG.use,base_age.use,
                             data.use,event.name,delta.name){



  ##################################
  ## Get information from CAP fit ##
  ##################################
  coeff.fit <- cap.fit$parameter.estimates
  sigma.use <- exp(coeff.fit["log_scale"])

  #############################
  ## Functions for CAP model ##
  #############################
  mu.cap <- function(coeff.fit,X_CAG,X_AGE){
    out <- coeff.fit["(Intercept)"] + X_AGE * coeff.fit["base_age"] +
      X_CAG * coeff.fit["CAG"] + X_AGE * X_CAG * coeff.fit["base_age:CAG"]
    return(out)
  }

  cap.distn <- function(time.use,coeff.fit,sigma.use,X_CAG,X_AGE){
    out <- 1+ exp(-(log(time.use)-mu.cap(coeff.fit,X_CAG,X_AGE))/sigma.use)
    out <- 1/out
    return(out)
  }

  cap.density <- function(time.use,coeff.fit,sigma.use,X_CAG,X_AGE){
    out <- ( 1/cap.distn(time.use,coeff.fit,sigma.use,X_CAG,X_AGE)^2 ) *
      exp(-(log(time.use)-mu.cap(coeff.fit,X_CAG,X_AGE))/sigma.use) *
      1/(sigma.use * time.use)
    return(out)
  }

  cap.gradient.inner <- function(time.use,coeff.fit,sigma.use,X_CAG,X_AGE){
    out <- (-1/cap.distn(time.use,coeff.fit,sigma.use,X_CAG,X_AGE)^2) *
      exp(-(log(time.use)-mu.cap(coeff.fit,X_CAG,X_AGE))/sigma.use)
    return(out)
  }

  cap.gradient <- function(time.use,coeff.fit,sigma.use,X_CAG,X_AGE){
    gradient.term <- rep(0,length(coeff.fit))
    names(gradient.term) <- names(coeff.fit)
    gradient.term["(Intercept)"] <- 1 * cap.gradient.inner(time.use,coeff.fit,sigma.use,X_CAG,X_AGE)/sigma.use
    gradient.term["base_age"] <- X_AGE * cap.gradient.inner(time.use,coeff.fit,sigma.use,X_CAG,X_AGE)/sigma.use
    gradient.term["CAG"] <- X_CAG * cap.gradient.inner(time.use,coeff.fit,sigma.use,X_CAG,X_AGE)/sigma.use
    gradient.term["base_age:CAG"] <- X_AGE*X_CAG *
      cap.gradient.inner(time.use,coeff.fit,sigma.use,X_CAG,X_AGE)/sigma.use
    gradient.term["log_scale"] <-  (log(time.use)-mu.cap(coeff.fit,X_CAG,X_AGE)) *
      cap.gradient.inner(time.use,coeff.fit,sigma.use,X_CAG,X_AGE)/sigma.use

    return(gradient.term)
  }

  Ft.estimate <- cap.distn(time.use,coeff.fit,sigma.use,X_CAG=CAG.use,X_AGE=base_age.use)
  Ft.variance <- cap.gradient(time.use,coeff.fit,sigma.use,X_CAG=CAG.use,X_AGE=base_age.use) %*% cap.fit$variance.estimates %*%
    cap.gradient(time.use,coeff.fit,sigma.use,X_CAG=CAG.use,X_AGE=base_age.use)

  return(list(Ft.estimate=Ft.estimate,
              Ft.variance=Ft.variance))
}



####################################
## function to fit Langbehn model ##
####################################

langbehn.distribution <- function(t,x,mu_val=c(21.54,9.56,0.146),sigma_val=c(35.55,17.72,0.327)){
  out <- 1/(1+exp(-(t-langbehn.mean(x,mu_val))/langbehn.sd(x,sigma_val)))
  return(out)
}


langbehn.mean <- function(x,mu_val=c(21.54,9.556,0.146)){
  mu1 <- mu_val[1]
  mu2 <- mu_val[2]
  mu3 <- mu_val[3]
  out <- mu1 + exp(mu2-mu3*x)
  return(out)
}


langbehn.sd <- function(x,sigma_val=c(35.55,17.72,0.3269)){
  sigma1 <- sigma_val[1]
  sigma2 <- sigma_val[2]
  sigma3 <- sigma_val[3]
  out <- sqrt(sigma1 + exp(sigma2-sigma3*x))
  return(out)
}

langbehn.survival <- function(t,x,mu_val,sigma_val){
  out <- 1-langbehn.distribution(t,x,mu_val,sigma_val)
  return(out)
}

langbehn.density <- function(t,x,mu_val,sigma_val){
  out <- 1/langbehn.distribution(t,x,mu_val,sigma_val)^2 *
    exp(-(t-langbehn.mean(x,mu_val))/langbehn.sd(x,sigma_val)) *
    1/langbehn.sd(x,sigma_val)
  return(out)
}

langbehn.optimization <- function(event.variable,delta.variable,
                                  cag.variable,theta){

  mu_val <- theta[1:3]
  sigma_val <- theta[4:6]

  out <-   sum(delta.variable *
                 log(langbehn.density(event.variable,cag.variable,mu_val,sigma_val))) +
    sum((1-delta.variable) *
          log(langbehn.survival(event.variable,cag.variable,mu_val,sigma_val)))
  return(out)
}

make.langbehn.optimization <- function(event.variable,delta.variable,cag.variable){

  function(theta){
    langbehn.optimization(event.variable,delta.variable,cag.variable,theta)
  }
}



#########################################
## CAP model for predicting onset ##
#########################################
cap.distribution <- function(t,cag,age0){
  alpha <- 4.4196
  beta <- -0.0065
  sigma <- exp(-0.8451)
  gamma <- -33.6600
  cap <- age0 * (cag + gamma)
  out <- 1+exp((log(t)-(alpha+beta*cap))/sigma)
  out <- 1/out
  out <- 1-out
  return(out)
}





## compute number of cases in each study
#' @import utils
compute.number.events.in.study <- function(
  data.for.training,
  data.for.testing,
  event.outcome.names,
  delta.names,
  functional.covariate.names,
  nonfunctional.covariate.names,
  othercovariate.names,
  study.names,
  dropout,
  Ftest.store,
  time.points.for.prediction,
  nonfunctional.covariate.values.for.prediction,
  start.date,
  start.age,
  end.date,
  predict.beyond.study.observation=FALSE,
  kmean_groups=4,
  grouping.to.use.suffix="_kmeans",
  nIter=15
){


  #################
  ## Get CAP fit ##
  #################
  cap.fit <- get.cap.model(data.use=data.for.training,
                           event.name=event.outcome.names,
                           delta.name=delta.names)


  ###################################
  ## we do stratified Kaplan-Meier ##
  ###################################
  ## we first extract all combinations between
  ## functional.covariate.names, nonfunctional.covariate.names
  variables.use <- ##c(functional.covariate.names)
    c(functional.covariate.names,nonfunctional.covariate.names)



  ###################################################################
  ## 7/10/2018: We don't use the combinations of quartiles anymore.
  ## Instead we will use kmeans clustering.
  ## It makes more sense.
  #################################################################
  if(grouping.to.use.suffix=="_quart"){
    variables.use.quart <- c("CAG_quart","base_age_quart")
    combinations.to.use <- get.empty.list(variables.use.quart)
    if(predict.beyond.study.observation==FALSE){
      data.training.testing.combined <- rbind(data.for.training,data.for.testing)
    } else {
      data.training.testing.combined <- data.for.training
    }

    for(ll in 1:length(combinations.to.use)){
      combinations.to.use[[ll]] <-
        #sort(unique(data.training.testing.combined[,variables.use[ll]]))
        sort(unique(data.training.testing.combined[,variables.use.quart[ll]]))
    }
    all.combinations <- expand.grid(combinations.to.use)
  } else if(grouping.to.use.suffix=="_kmeans"){
    all.combinations <- NULL
    for(kk in 1:kmean_groups){
      ###########################
      ## Extract relevant data ##
      ###########################
      data.subset <- data.for.training[which(data.for.training[,"kmeans_group"]==kk),]
      all.combinations <- rbind(all.combinations,
                                data.subset[1,paste0(c(nonfunctional.covariate.names,
                                                       functional.covariate.names),grouping.to.use.suffix)])
    }
    colnames(all.combinations) <- paste0(c(nonfunctional.covariate.names,
                                           functional.covariate.names),"_kmeans")
    rownames(all.combinations) <- 1:kmean_groups
  }

  ## keep.index: which individuals in the test data set did not
  ## 		   experience motor conversion?
  keep.index <- rep(1,nrow(data.for.testing))
  if(predict.beyond.study.observation==TRUE){
    ## we are computing probabilities beyond the study observation period
    ## we only want to see the new converters. Thus
    ## these were people who were censored at the end of the study periods.
    ## We remove people who converted by the end of the study period.
    make.zero <- which(data.for.testing[,delta.names]==1)
    keep.index[make.zero] <- 0
  }
  data.out <- data.frame(data.for.testing,KM.converter=0,JPRAT.converter=0,
                         Langbehn.converter=0,
                         CAP.converter=0,
                         CAP.new.converter=0)

  for(ii in 1:nrow(data.for.testing)){
    km.numerator <- 0
    km.denominator <- 0
    jprat.numerator <- 0
    jprat.denominator <- 0
    langbehn.numerator <- 0
    langbehn.denominator <- 0
    cap.numerator <- 0
    cap.denominator <- 0
    cap.new.numerator <- 0
    cap.new.denominator <- 0

    if(keep.index[ii]==1){



      study.use <- as.character(data.for.testing[ii,"study"])
      starting.age <- data.for.testing[ii,start.age]


      future.age <- starting.age +
        end.date[study.use]-
        data.for.testing[ii,start.date]

      ###################
      ## JPRAT version ##
      ###################

      CAG.use <- all.combinations[data.for.testing[ii,"kmeans_group"],
                                  paste0("CAG",grouping.to.use.suffix)]
      base.age.use <- all.combinations[data.for.testing[ii,"kmeans_group"],
                                       paste0("base_age",grouping.to.use.suffix)]
      z.index <- which(nonfunctional.covariate.values.for.prediction==base.age.use)


      ## because we combine study data, all estimates are the same from every study
      jprat.tmp <- Ftest.store[1,study.use,event.outcome.names,z.index,
                               paste0("xx",CAG.use),,"est"]
      ## add 0 and 1 to bottom and top, respectively
      jprat.tmp <- c(0,jprat.tmp,1)

      ## linearly interpolate the results
      make.interpolate.jprat <- function(jprat.tmp,time.points.for.prediction){
        function(x){
          out <- approx(x=c(0,time.points.for.prediction,
                            max(time.points.for.prediction) + 200),
                        y= jprat.tmp,xout=x)$y
          return(out)
        }
      }
      jprat.interpolate <- make.interpolate.jprat(jprat.tmp,
                                                  time.points.for.prediction)



      jprat.numerator <- jprat.numerator +
        (jprat.interpolate(future.age)-
           jprat.interpolate(starting.age))

      jprat.denominator <- jprat.denominator +
        (1-jprat.interpolate(starting.age))

      ##################################
      ## get Langbehn model estimator ##
      ##################################
      ## update to using individual's CAG repeat-length and age at baseline.
      CAG.use <- data.for.testing[ii,"CAG"]
      base.age.use <- data.for.testing[ii,"base_age"]

      langbehn.numerator <- langbehn.numerator +
        (langbehn.distribution(future.age,CAG.use)-
           langbehn.distribution(starting.age,CAG.use))

      langbehn.denominator <- langbehn.denominator +
        (1-langbehn.distribution(starting.age,CAG.use))

      #######################
      ## get CAP estimator ##
      #######################
      cap.numerator <- cap.numerator +
        (cap.distribution(future.age,CAG.use,base.age.use)-
           cap.distribution(starting.age,CAG.use,base.age.use))
      cap.denominator <- cap.denominator +
        (1-cap.distribution(starting.age,CAG.use,base.age.use))

      #########################################
      ## get CAP estimator based on data fit ##
      #########################################
      cap.new.numerator <- cap.new.numerator +


        (fit.cap.model.Ft(cap.fit,time.use=future.age,
                          CAG.use=CAG.use,
                          base_age.use=base.age.use,
                          data.use=data.for.training,
                          event.name=event.outcome.names,
                          delta.name=delta.names)$Ft.estimate-
           fit.cap.model.Ft(cap.fit,
                            time.use=starting.age,
                            CAG.use=CAG.use,
                            base_age.use=base.age.use,
                            data.use=data.for.training,
                            event.name=event.outcome.names,
                            delta.name=delta.names)$Ft.estimate)
      cap.new.denominator <- cap.new.denominator +
        (1-fit.cap.model.Ft(cap.fit,
                            time.use=starting.age,
                            CAG.use=CAG.use,
                            base_age.use=base.age.use,
                            data.use=data.for.training,
                            event.name=event.outcome.names,
                            delta.name=delta.names)$Ft.estimate)


      data.out[ii,"JPRAT.converter"] <- jprat.numerator/jprat.denominator
      data.out[ii,"Langbehn.converter"] <- langbehn.numerator/langbehn.denominator
      data.out[ii,"CAP.converter"] <- cap.numerator/cap.denominator
      data.out[ii,"CAP.new.converter"] <- cap.new.numerator/cap.new.denominator

    }
  }



  ## get number of events in each study
  out <- array(0,dim=c(length(study.names),2,5),
               dimnames=list(study.names,c("orig.cases","new.cases"),
                             c("Kaplan-Meier","JPRAT","Langbehn", "CAP","CAP_new")))


  study <- NULL
  for(ss in 1:length(study.names)){
    subset.data <- subset(data.out, study==study.names[ss])
    out[ss,"orig.cases",] <-
      sum(subset.data[,delta.names],na.rm=TRUE)
    out[ss,"new.cases","Kaplan-Meier"] <-
      sum(subset.data[,"KM.converter"],na.rm=TRUE)

    out[ss,"new.cases","JPRAT"] <-
      sum(subset.data[,"JPRAT.converter"],na.rm=TRUE)

    out[ss,"new.cases","Langbehn"] <-
      sum(subset.data[,"Langbehn.converter"],na.rm=TRUE)

    out[ss,"new.cases","CAP"] <-
      sum(subset.data[,"CAP.converter"],na.rm=TRUE)

    out[ss,"new.cases","CAP_new"] <-
      sum(subset.data[,"CAP.new.converter"],na.rm=TRUE)
  }

  out[,"new.cases",] <- out[,"new.cases",] *(1-dropout)

  return(list(data.out=data.out,converted.number=out))
}


