% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{get.cis}
\alias{get.cis}
\title{95\% Confidence interval for time points of interest}
\usage{
get.cis(
  out,
  flatten.name,
  theta = "alpha",
  time_choice,
  xx_val,
  xmin,
  xmax,
  xx_choose,
  zz_choose = NULL,
  convert,
  round.set,
  var.lo,
  var.hi,
  est,
  noshow = NULL,
  track.sign.change = TRUE
)
}
\arguments{
\item{out}{An array of estimates to plot.}

\item{flatten.name}{A character value of the flatten name.}

\item{theta}{A character value for the name of the component estimated: "beta" (coefficients of the nonfunctional covariates Z),
"alphas" (coefficients of the functional covariates X),  "F" (monotone marginal distributions \eqn{F_{es}(t|Z, X)})
 or "Ft.pred" (the predicted marginal distribution \eqn{F_{es}(t|Z, X, t>t_{0})}.}

\item{time_choice}{A vector of time points at which confidence intervals of estimates will be predicted. See the argument \code{time.points.of.interest.ci} in the \code{\link{view.all.results}}.}

\item{xx_val}{A vector of values for the functional covariate values of X per a study. See the argument \code{functional.covariate.values.for.prediction} in the \code{\link{view.all.results}} function.}

\item{xmin}{Minimum value for the functional covariate values of X.}

\item{xmax}{Maximum value for the functional covariate values of X.}

\item{xx_choose}{A vector of specific functional covariate values of X at which confidence interval of the smooth functional parameter \eqn{\alpha(x,t)} will be predicted.
The default is "Null" for the parameter "beta".}

\item{zz_choose}{a vector of character values for the names of nonfunctional covariates Z.
For example, zz_choose=z_lab_names. Null for the parameter "beta" and "alphas".}

\item{convert}{A logical value whether the CAG repeat lengths will be scaled into the uniformly distributed values on [0,1].}

\item{round.set}{The number of significant digits to use in the plot: Default is 2.}

\item{var.lo}{A character value for the name of the lower bound for the confidence intervals: "varlo".}

\item{var.hi}{A character value for the name of the upper bound for the confidence intervals: "varhi".}

\item{est}{A character value for the estimates, e.g., "est".}

\item{noshow}{A character vector of column names, which will not be in the data frame.}

\item{track.sign.change}{A logical value whether the sign change will be tracking. The default is TRUE.}
}
\value{
\item{out.flatten}{A data frame, created from the array \code{out} using the \code{flatten.array} function.
                   The data frame contains the confidence intervals for the estimates (ci)
                   (whether the signs of the lower bounds and upper bounds of the confidence intervals for the estimates will be changed (\code{sign.change})),
                   the length of confidence intervals (\code{ci.length}) at specific time points (\code{time_choice}),
                   covariates values (\code{xx_choose}, \code{zz_choose}), and the clinical events of interest for all studies.}
}
\description{
This function generates a data frame, which contains the confidence intervals
              for the estimates and the length of confidence intervals at each time for specific values of covariates for all studies.
}
\details{
The data for "noshow" will not be shown in the data frame. To do the confidence interval test,
         the signs of the lower bounds and upper bounds of the confidence intervals will be observed.
         If the signs of the lower bounds and the upper bounds for the confidence intervals are the same,
         the \code{sign.change} returns TRUE (or 1); otherwise, FALSE (or 0).
         Those values will be used to test the hypothesis test whether the study results are similar, i.e., whether the true model shares the study parameters.
}
