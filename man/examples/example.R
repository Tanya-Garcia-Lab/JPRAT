## load your data in the list format: should match order of study.names
#input.data.list=list(cohort=data_cohort, predict=data_predict) #, pharos=data_pharos);
input.data.list=list(predict=data_predict, pharos=data_pharos);
#this data works with what.analyzed.separately="studyevent";
#this data works withwhat.analyzed.separately= "event"

#input.data.list=list(cohort=ex_data_cohort, predict=ex_data_predict, pharos=ex_data_pharos);

#Input data for JPRAT estimation procedure
#study.names=c("cohort", "predict") #, "pharos");
study.names=c("predict", "pharos");
nonfunctional.covariate.names=c("base_age");
functional.covariate.names="CAG";
othercovariate.names=c("firstyear", "lastyear")
event.outcome.names=c("hdage_nobase", "mcione") #, "dep2");
delta.names=c("delta.hdage_nobase", "delta.mcione") #, "delta.dep2");
time.points.for.prediction=seq(40, 60, by=5);
time.points.for.prediction=seq(40, 60, by=5);
time.points.for.conditional.prediction=c(40, 45, 50);
time.points.for.conditional.prediction.toadd=c(3);
nonfunctional.covariate.value=c(45);
functional.covariate.values.of.interest=c(40, 45, 50) ;

# parameter setup to analyze data using JPRAT estimation procedure
number.of.bootstraps=100;
use.functional.beta.intercept= TRUE ;
use.functional.event.coefficients= TRUE;
use.functional.study.coefficients=TRUE;
check.study.equality=FALSE;
#check.study.equality=TRUE; #when what.analyzed.separately= "none"
#what.analyzed.separately="studyevent";
#what.analyzed.separately= "event" ## works with write.output=TRUE
#what.analyzed.separately= "none"
what.analyzed.separately= "study"
estimated.parameters.common.for.all.studies=FALSE;
use.bootstrap.variance=FALSE;
estimation.when.censoring.depends.on.z=FALSE ;
write.output=FALSE;

# JPRAT estimation
jprat.estimat.results<-jprat.wrapper(study.names=study.names,
                                     input.data.list=input.data.list,
 nonfunctional.covariate.names=nonfunctional.covariate.names,
 functional.covariate.names=functional.covariate.names,
 othercovariate.names=othercovariate.names,
 event.outcome.names=event.outcome.names,
 delta.names=delta.names,
 time.points.for.prediction=time.points.for.prediction,
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
 use.bootstrap.variance=use.bootstrap.variance, write.output=write.output)


##get number at risk table
study.names=c("cohort", "predict", "pharos");
input.data.list=list(cohort=data_cohort, predict=data_predict, pharos=data_pharos);
event.outcome.names=c("hdage_nobase") #, "mcione", "tfctwo");
nonfunctional.covariate.names=c("base_age");
functional.covariate.names="CAG";
nonfunctional.covariate.value=c(40);
functional.covariate.values.of.interest=c(45, 48, 50) ;
time.points.for.prediction=seq(45, 55, by=5);
estimated.parameters.common.for.all.studies=FALSE;

##to creat out_nrisk.dat file
number.at.risk <- compute.number.at.risk.for.HD(study.names,
 input.data.list, #data.file.names,
 event.outcome.names,
 nonfunctional.covariate.names,
 functional.covariate.names,
 nonfunctional.covariate.value,
 functional.covariate.values.of.interest,
 time.points.for.prediction,
 estimated.parameters.common.for.all.studies,
 write.output=FALSE
 )



## To get results
#############################
# parameters to get results #
#############################
functional.covariate.values.of.interest.ci=c(45, 50);
time.points.of.interest= c(45, 55);
time.points.of.interest.ci=seq(45, 55, by=5);
which.nonfunctional.covariate.comparisons=c(1,1);
color.names=c("firebrick1", "darkgreen", "black"); ## for color.label.key
legend.names=c("Motor Diagnosis (DCL=4)") #, "Cognitive Impairment", "Stage II TFC")
functional.covariate.comparisons=c(45, 50);
do.plots=TRUE;
plot.confidence.intervals=TRUE;
add.number.at.risk.legend=TRUE;
ylabel.for.plots.comparing.studies="Probability"
xlabel.for.plots.comparing.studies="Age (years)"
file.name.for.analysis="test" ## figure names
show.results.description=TRUE;
is.nrisk.data.frame=FALSE; ## if users choose write.output=TRUE, then nrisk is data frame,
                           ##so is.nrisk.data.frame=TRUE.
                     ## if users choose write.output=FALSE, then nrisk is array form,
                      ## so is.nrisk.data.frame=FALSE.
write.jprat.output=FALSE; ## users need to choose write.jprat.output=TRUE
                        ## when they choose write.output=TRUE in jprat function;
                        ## FALSE otherwise.
####################################
## functions to get default values #
####################################
default.options<-default.options.for.data.setting()

## data is reformatted to get analysis results
reformatted.data.for.analysis.results<-data.reformatted.for.analysis.results(
  study.names, event.outcome.names,
  color.names, legend.names,
  which.nonfunctional.covariate.comparisons)

## obtain default values
## default values
use_real_data<-default.options$use_real_data
combine.data<-default.options$combine.data

## datasets are reformatted, so that results plots and tables can be produced;
num_study<-reformatted.data.for.analysis.results$num_study;
jprat.output<-jprat.estimat.results$jprat.output
nrisk<-number.at.risk

## if you want to load results from JPRAT algorithm
if(write.jprat.output==TRUE){
  data.theta <- read.table("out_thetaest_real_output_iseed_1.dat",header=FALSE)
  data.combi <- read.table("out_combi_real_output_iseed_1.dat",header=FALSE)

}


## In case, if you want to load results of number of risks
## when the output option for compute.number.at.risk.for.HD is a dataframe (is.nrisk.data.frame==TRUE).
if(is.nrisk.data.frame==TRUE){
   nrisk <- read.table("out_nrisk.dat",header=TRUE)
}

## display all results including tables and plots
results.out <- view.all.results(
  ############################
  # arguments for JPRAT     ##
  ############################
  study.names,
  #data.file.names,
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
  number.of.bootstraps, #=100,
  use.functional.beta.intercept,
  use.functional.event.coefficients,
  use.functional.study.coefficients,
  check.study.equality,
  estimated.parameters.common.for.all.studies,
  what.analyzed.separately,
  estimation.when.censoring.depends.on.z,
  use.bootstrap.variance,
  ##########################
  ## For plotting results ##
  ##########################
  functional.covariate.values.of.interest.ci,
  time.points.of.interest,
  time.points.of.interest.ci,
  label.for.alpha.values.over.time=NULL,
  which.nonfunctional.covariate.comparisons,
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
  #data.truth=data.truth,
  #data.theta=data.theta,
  #data.combi=data.combi,
  jprat.output=jprat.output,
  #data.count=data.count,
  #data.count.outside=data.count.outside,
  nrisk=nrisk,
  #####################################
  ## Do we show results description? ##
  #####################################
  show.results.description,
  is.nrisk.data.frame,
  write.jprat.output
)


