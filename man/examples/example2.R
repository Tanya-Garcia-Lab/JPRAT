## load your data in the list format: should match order of study.names
input.data.list=list(study1=simu_dat_study1, study2=simu_dat_study2);

#Input data for JPRAT estimation procedure
study.names=c("study1", "study2");
nonfunctional.covariate.names=c(

) #, "z2");
functional.covariate.names="x_cag";
othercovariate.names=NULL
event.outcome.names=c("event1", "event2");
delta.names=c("delta.event1", "delta.event2"); ## should be delta.eventname


## four options whether study and event are analyzed separately:
#what.analyzed.separately="studyevent",  "event", "study" or "none"
what.analyzed.separately= "none"

if(what.analyzed.separately=="none"){

  # time_choice.predicted <- c(40,45)
  # time_choice.predicted.toadd  <- c(5,10)
  #
  # ## time points to show individual results at and label for alpha(x,t)
  # time_choice <- c(45,55)

  time.points.for.prediction=  c(seq(40,49,by=1),seq(50,60,by=1))
  time.points.for.conditional.prediction=c(40, 50);
  time.points.for.conditional.prediction.toadd=c(5,10);
  nonfunctional.covariate.value=c(0.5) #as.matrix(c(0.50,0.75)) ## zeval: c(0.5)
  functional.covariate.values.of.interest=0.5 # c(0.5,0.75) ;   ## xx_choice
}else{

}

# parameter setup to analyze data using JPRAT estimation procedure
number.of.bootstraps=10;
use.functional.beta.intercept= TRUE ;
use.functional.event.coefficients= TRUE;
use.functional.study.coefficients=TRUE;



if(what.analyzed.separately!="none"){
  check.study.equality=FALSE
}else{
  check.study.equality=TRUE
}


if(what.analyzed.separately!="none"){
  estimated.parameters.common.for.all.studies=FALSE
}else{
  estimated.parameters.common.for.all.studies=TRUE
}

use.bootstrap.variance=TRUE;
estimation.when.censoring.depends.on.z=FALSE ;
write.output=TRUE;

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




## load your data in the list format: should match order of study.names

##get number at risk table
study.names=c("study1", "study2");
input.data.list=list(study1=simu_dat_study1, study2=simu_dat_study2);
event.outcome.names=c("event1", "event2");
nonfunctional.covariate.names=c("z1");
functional.covariate.names="x_cag";



#estimated.parameters.common.for.all.studies=FALSE;
#estimated.parameters.common.for.all.studies=TRUE;

if(what.analyzed.separately!= "study"){
  time.points.for.prediction=c(seq(40,49,by=1),seq(50,60,by=1));
  functional.covariate.values.of.interest=c(0.5) ;
  nonfunctional.covariate.value=c(0.5);
}else{
  time.points.for.prediction=seq(44, 52, by=4);
  nonfunctional.covariate.value=c(48);
  functional.covariate.values.of.interest=c(44, 48) ;
}




if(what.analyzed.separately!="none"){
  estimated.parameters.common.for.all.studies=FALSE
}else{
  estimated.parameters.common.for.all.studies=TRUE
}


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
                                                write.output=TRUE
)



## To get results
#############################
# parameters to get results #
#############################
# functional.covariate.values.of.interest.ci=c(46, 51);
# time.points.of.interest= c(46, 56);
# time.points.of.interest.ci=seq(46, 66, by=5);
# which.nonfunctional.covariate.comparisons=c(1,1);

# functional.covariate.comparisons=c(46, 51);





if(what.analyzed.separately!= "study"){
  # time.points.for.prediction=seq(46, 66, by=5);
  # time.points.for.conditional.prediction=c(46,51, 56);
  # time.points.for.conditional.prediction.toadd=c(5);
  # nonfunctional.covariate.value=c(40);
  # functional.covariate.values.of.interest=c(46, 48, 50) ;
  functional.covariate.values.of.interest.ci=c(46, 51);
  time.points.of.interest= c(46, 56);
  time.points.of.interest.ci=seq(46, 66, by=5);
  functional.covariate.comparisons=c(46, 51);
}else{
  # time.points.for.prediction=seq(44, 52, by=4);
  # time.points.for.conditional.prediction=c(44, 48, 52);
  # time.points.for.conditional.prediction.toadd=c(5);
  # nonfunctional.covariate.value=c(48);
  # functional.covariate.values.of.interest=c(44, 48) ;
  functional.covariate.values.of.interest.ci=c(44, 48);
  time.points.of.interest= c(44, 52);
  time.points.of.interest.ci=seq(44, 52, by=4);
  functional.covariate.comparisons=c(44, 48);
}

color.names=c("firebrick1", "darkgreen", "black"); ## for color.label.key
legend.names=c("Motor Diagnosis (DCL=4)") #, "Cognitive Impairment", "Stage II TFC")
do.plots=TRUE;
plot.confidence.intervals=TRUE;
add.number.at.risk.legend=TRUE;
ylabel.for.plots.comparing.studies="Probability"
xlabel.for.plots.comparing.studies="Age (years)"
file.name.for.analysis="test" ## figure names
show.results.description=TRUE;
is.nrisk.data.frame=TRUE; ## if users choose write.output=TRUE, then nrisk is data frame,
##so is.nrisk.data.frame=TRUE.
## if users choose write.output=FALSE, then nrisk is array form,
## so is.nrisk.data.frame=FALSE.
write.jprat.output=TRUE; ## users need to choose write.jprat.output=TRUE
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

# ## if you want to load results from JPRAT algorithm
# if(write.jprat.output==TRUE){
#   data.theta <- read.table("out_thetaest_real_output_iseed_1.dat",header=FALSE)
#   data.combi <- read.table("out_combi_real_output_iseed_1.dat",header=FALSE)
#
# }
#
#
# ## In case, if you want to load results of number of risks
# ## when the output option for compute.number.at.risk.for.HD is a dataframe (is.nrisk.data.frame==TRUE).
# if(is.nrisk.data.frame==TRUE){
#    nrisk <- read.table("out_nrisk.dat",header=TRUE)
# }

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


