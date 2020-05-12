% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{ggplot_at_a_over_b}
\alias{ggplot_at_a_over_b}
\title{ggplot_at_a_over_b: This function creates ggplots for the estimated functions f(a, b) over b.}
\usage{
ggplot_at_a_over_b(
  filename,
  estimate,
  theta.array.lo = NULL,
  theta.array.hi = NULL,
  index_a,
  bvalues = time_val,
  bvalues.cut = time.cut,
  color.list,
  nrisk = number.at.risk[index_a, nn, zz, 1, ],
  margin.move = unit(c(0, -30, 0, 0), "mm"),
  cex.line = 1.5,
  cex.size = 20,
  cex.number = 6,
  ylim = NULL,
  conf.int = FALSE,
  label.names = NULL,
  ylab.use = "",
  xlab.use = "",
  add.second.legend = FALSE
)
}
\arguments{
\item{filename}{a character string for ggplots' file name starting with "gg". Use paste("gg_", filename.set, sep="").
The "filename.set" is a character string value for a file name.}

\item{estimate}{estimated lower bounds for the confidence intervals at specific time points.}

\item{theta.array.lo}{estimated lower bounds for the confidence intervals at specific time points.}

\item{theta.array.hi}{estimated upper bounds for the confidence intervals at specific time points.}

\item{index_a}{index for the functional covariate values X, where to draw plot.}

\item{bvalues}{a vector of values for the x-axis. Here, specifically, a vector of time points for prediction (\code{time_val}).}

\item{bvalues.cut}{a cut off for "bvalues" where tic marks should be drawn.}

\item{color.list}{a character vector of default color names, which will be used in plots.}

\item{nrisk}{array for the numbers at risk at each time point.
The dimension of array will be the number of nonfunctional covariate Z used in the analysis (z.choice, here denoted as zz) by the number of time points (\code{num_time}).}

\item{margin.move}{margin around entire plot to make y-label closer to y-axis. Defualt is unit(c(0,-10,0,0), "mm").}

\item{cex.line}{magnification of thickness of the line relative to cex. Default is 1.5.}

\item{cex.size}{magnification of size of text in the legend relative to cex. Default is 20.}

\item{cex.number}{magnification of size of text in the labels relative to cex. Default is 6.}

\item{ylim}{limits of y-axis: minimum and maximum grid of y-axis.}

\item{conf.int}{a logical value whether the confidence interval will be plotted.}

\item{label.names}{a character vector of names for labels in plots.}

\item{ylab.use}{a character vector for the y-axis label. There are options such as alphax.ylab and study.ylab,
which are character value for y-axis label depending on what parameters will be plotted.
See the argument \code{ylabel.for.plots.comparing.studies} in the \code{\link{view.all.results}} function.}

\item{xlab.use}{a character vector for the x-axis label. There are options: alphax.xlab and study.xlab, which are character values for x-axis label in plots.
See the argument "xlabel.for.plots.comparing.studies" in the \code{\link{view.all.results}} function.}

\item{add.second.legend}{a logical value whether the second legend will be added in plots.}
}
\value{
This function returns all analysis results including plots and tables.
}
\description{
ggplot_at_a_over_b: This function creates ggplots for the estimated functions f(a, b) over b.
}
\examples{



}