library(JPRAT)

## example 1. for JPRAT
#Input data for JPRAT estimation procedure
study.names=c("cohort", "predict", "pharos");
data.file.names=c("cohort", "predict", "pharos");
nonfunctional.covariate.names=c("base_age");
functional.covariate.names="CAG";
othercovariate.names=c("firstyear", "lastyear")
event.outcome.names=c("hdage_nobase", "mcione", "tfctwo");
delta.names=c("delta.hdage_nobase", "delta.mcione", "delta.tfctwo");
time.points.for.prediction=seq(46, 66, by=5);
time.points.for.conditional.prediction=c(46, 51, 56);
time.points.for.conditional.prediction.toadd=c(5);
nonfunctional.covariate.value=c(40);
functional.covariate.values.of.interest=c(46, 48, 50) ;

# parameter setup to analyze data using JPRAT estimation procedure
number.of.bootstraps=100;
use.functional.beta.intercept= TRUE ;
use.functional.event.coefficients= TRUE;
use.functional.study.coefficients=TRUE;
check.study.equality=FALSE;
what.analyzed.separately="studyevent";
estimated.parameters.common.for.all.studies=FALSE;
use.bootstrap.variance=TRUE ;
estimation.when.censoring.depends.on.z=FALSE ;
estimate.variances = "est";
write.output=TRUE;

# JPRAT estimation

jprat.estimat.results<-jprat.wrapper(study.names=study.names,
                data.file.names=data.file.names,
                nonfunctional.covariate.names=nonfunctional.covariate.names,
                functional.covariate.names=functional.covariate.names,
                othercovariate.names=othercovariate.names, event.outcome.names=event.outcome.names,
                delta.names=delta.names, time.points.for.prediction=time.points.for.prediction,
                time.points.for.conditional.prediction=time.points.for.conditional.prediction,
                time.points.for.conditional.prediction.toadd=time.points.for.conditional.prediction.toadd,
                nonfunctional.covariate.value=nonfunctional.covariate.value,
                functional.covariate.values.of.interest=functional.covariate.values.of.interest,
                number.of.bootstraps=number.of.bootstraps,
                use.functional.beta.intercept=use.functional.beta.intercept,
                use.functional.event.coefficients=use.functional.event.coefficients,
                use.functional.study.coefficients=use.functional.study.coefficients,
                check.study.equality=check.study.equality,
                estimated.parameters.common.for.all.studies=estimated.parameters.common.for.all.studies,
                what.analyzed.separately=what.analyzed.separately,
                estimation.when.censoring.depends.on.z=estimation.when.censoring.depends.on.z,
                use.bootstrap.variance=use.bootstrap.variance,
                estimate.variances=estimate.variances,
                write.output=write.output)


## Example 2. for getting results
#############################
# parameters to get results #
#############################
functional.covariate.values.of.interest.ci=c(46, 51);
time.points.of.interest= c(46, 56);
time.points.of.interest.ci=seq(46, 66, by=5);
which.nonfunctional.covariate.comparisons=c(1,1);
color.names=c("firebrick1", "darkgreen", "black"); ## for color.label.key
legend.names=c("Motor Diagnosis (DCL=4)", "Cognitive Impairment", "Stage II TFC")
functional.covariate.comparisons=c(46, 51);
do.plots=TRUE;
plot.confidence.intervals=TRUE;
add.number.at.risk.legend=TRUE;
ylabel.for.plots.comparing.studies="Probability"
xlabel.for.plots.comparing.studies="Age (years)"
file.name.for.analysis="test" ## figure names
show.results.description=FALSE;

## obtain default options
default.options<-default.options.for.data.setting()

## default values
use_real_data<-default.options$use_real_data
combine.data<-default.options$combine.data

## data is reformatted to get analysis results
reformatted.data.for.analysis.results<-data.reformatted.for.analysis.results(
  study.names, event.outcome.names,
  color.names, legend.names,
  which.nonfunctional.covariate.comparisons)

## datasets are reformatted, so that results plots and tables can be produced;
num_study<-reformatted.data.for.analysis.results$num_study;


## load results file from JPRAT algorithm
data.truth <- read.table("data/out_truth_real_output_iseed_1.dat",header=FALSE)
data.theta <- read.table("data/out_thetaest_real_output_iseed_1.dat",header=FALSE)

if(num_study > 1){
  data.combi <- read.table("data/out_combi_real_output_iseed_1.dat",header=FALSE)
} else {
  data.combi <- NULL
}

data.count <- read.table("data/out_count_real_output_iseed_1.dat",header=FALSE)
data.count.outside <- read.table("data/out_outside_count_real_output_iseed_1.dat",header=FALSE)
nrisk <- read.table("data/out_nrisk.dat",header=TRUE)


## display all results including tables and plots
results.out <- view.all.results(
  ############################
  # arguments for JPRAT     ##
  ############################
  study.names,
  data.file.names,
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
  number.of.bootstraps, #=100,
  use.functional.beta.intercept,
  use.functional.event.coefficients,
  use.functional.study.coefficients,
  check.study.equality,
  estimated.parameters.common.for.all.studies,
  what.analyzed.separately,               #="none",
  estimation.when.censoring.depends.on.z, #=FALSE,
  use.bootstrap.variance,                 #=FALSE,
  ##########################
  ## For plotting results ##
  ##########################
  functional.covariate.values.of.interest.ci,
  time.points.of.interest,
  time.points.of.interest.ci,
  label.for.alpha.values.over.time=NULL,
  which.nonfunctional.covariate.comparisons, ## return nonfunctional covariate comparison
  color.names, ## returns color.labels
  legend.names, ## returns legend.labels
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
  file.name.for.analysis, ## figure names
  #######################
  ## Output from JPRAT ##
  #######################
  data.truth=data.truth,
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
)
