## remove history         ##
## rm(list=ls(all=TRUE))  ##
############################

## Four options whether study and event are analyzed separately:
## what.analyzed.separately="studyevent", "event", "study" or "none"
    what.analyzed.separately= "none"


## Load your data in the list format: should match order of study.names
## Choose different list of data depending on the settings such as
    if(what.analyzed.separately=="event"||what.analyzed.separately=="studyevent"){

       input.data.list=list(cohort=data_cohort, predict=data_predict, pharos=data_pharos);

    }else if(what.analyzed.separately=="none"||what.analyzed.separately=="study"){

       input.data.list=list(study1=simu_data1_model_A, study2=simu_data2_model_A);
    }




###################################################################
##Input data for JPRAT estimation procedure: paramaeter settings ##
###################################################################

    if(what.analyzed.separately=="event"||what.analyzed.separately=="studyevent"){


      study.names=c("cohort", "predict", "pharos");
      othercovariate.names=c("firstyear", "lastyear");
      event.outcome.names=c("hdage_nobase", "mcione", "dep2");
      delta.names=c("delta.hdage_nobase", "delta.mcione", "delta.dep2");
      nonfunctional.covariate.names=c("base_age");
      functional.covariate.names="CAG";
      time.points.for.prediction=seq(46, 66, by=5);
      time.points.for.conditional.prediction=c(46,51, 56);
      time.points.for.conditional.prediction.toadd=c(5);
      nonfunctional.covariate.value=c(40);
      functional.covariate.values.of.interest=c(46, 48, 50) ;

    }else if(what.analyzed.separately== "none"||what.analyzed.separately=="study"){

      study.names=c("study1", "study2");
      othercovariate.names=NULL;
      event.outcome.names=c("event1", "event2");
      delta.names=c("delta.event1", "delta.event2");
      nonfunctional.covariate.names=c("base_age");
      functional.covariate.names="CAG";
      time.points.for.prediction=  c(seq(40,60,by=1));
      time.points.for.conditional.prediction=c(40, 55);
      time.points.for.conditional.prediction.toadd=c(5,10);
      nonfunctional.covariate.value=c(42);
      functional.covariate.values.of.interest=c(42);

    }


    if(what.analyzed.separately!="none"){
      check.study.equality=FALSE
      estimated.parameters.common.for.all.studies=FALSE
    }else{
      check.study.equality=TRUE
      estimated.parameters.common.for.all.studies=TRUE
    }

    number.of.bootstraps=10;
    use.functional.beta.intercept= TRUE ;
    use.functional.event.coefficients= TRUE;
    use.functional.study.coefficients=TRUE;
    use.bootstrap.variance=TRUE;
    estimation.when.censoring.depends.on.z=FALSE ;
    write.output=FALSE;

#############################################
## a function to estimate JPRAT algorithm  ##
#############################################

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

##########################################################
## Get a table for the number of people who are at risk ##
##########################################################
    if(what.analyzed.separately=="event"||what.analyzed.separately=="studyevent"){

      study.names=c("cohort", "predict", "pharos");
      event.outcome.names=c("hdage_nobase", "mcione", "dep2");
      nonfunctional.covariate.names=c("base_age");
      functional.covariate.names="CAG";

    }else if(what.analyzed.separately=="none"||what.analyzed.separately=="study"){

      study.names=c("study1", "study2");
      othercovariate.names=NULL;
      event.outcome.names=c("event1", "event2");
      delta.names=c("delta.event1", "delta.event2");
      nonfunctional.covariate.names=c("base_age");
      functional.covariate.names="CAG";

    }



    if(what.analyzed.separately=="event"||what.analyzed.separately=="studyevent"){

      time.points.for.prediction=seq(46, 66, by=5);
      functional.covariate.values.of.interest=c(46, 48, 50) ;
      nonfunctional.covariate.value=c(40);

    }else if(what.analyzed.separately=="none"||what.analyzed.separately=="study"){

      time.points.for.prediction=  c(seq(40,60,by=1))
      functional.covariate.values.of.interest=c(42);
      nonfunctional.covariate.value=c(42);

    }

    if(what.analyzed.separately!="none"){
      estimated.parameters.common.for.all.studies=FALSE
    }else{
      estimated.parameters.common.for.all.studies=TRUE
    }


## a function to create a table for the number of patients who are at risk
    number.at.risk <- compute.number.at.risk.for.HD(study.names,
     input.data.list,
     event.outcome.names,
     nonfunctional.covariate.names,
     functional.covariate.names,
     nonfunctional.covariate.value,
     functional.covariate.values.of.interest,
     time.points.for.prediction,
     estimated.parameters.common.for.all.studies,
     write.output=FALSE
     )




##############################
## parameters to get results #
##############################

    if(what.analyzed.separately=="event"||what.analyzed.separately=="studyevent"){

      functional.covariate.values.of.interest.ci=c(46, 51);
      time.points.of.interest= c(46, 56);
      time.points.of.interest.ci=seq(46, 66, by=5);
      functional.covariate.comparisons=c(46, 51);
      time.points.for.prediction=seq(46, 66, by=5);
      functional.covariate.values.of.interest=c(46, 48, 50) ;
      nonfunctional.covariate.value=c(40);
      color.names=c("firebrick1", "darkgreen", "black"); ## for color.label.key
      legend.names=c("Motor Diagnosis (DCL=4)") #, "Cognitive Impairment", "Stage II TFC")


    }else{

      functional.covariate.values.of.interest.ci=c(40, 55);
      time.points.of.interest= c(40, 55);
      time.points.of.interest.ci=seq(40, 60, by=5);# c(seq(40,60,by=1))
      functional.covariate.comparisons=c(40, 55);
      time.points.for.prediction=  c(seq(40,60,by=1))
      functional.covariate.values.of.interest=c(42);
      nonfunctional.covariate.value=c(42);
      color.names=c("firebrick1", "darkgreen"); ## for color.label.key
      legend.names=c("event1", "event2"); #c("Motor Diagnosis (DCL=4)", "Cognitive Impairment")

    }


    do.plots=TRUE;
    plot.confidence.intervals=TRUE;
    add.number.at.risk.legend=TRUE;
    ylabel.for.plots.comparing.studies="Probability"
    xlabel.for.plots.comparing.studies="Age (years)"
    file.name.for.analysis="test" ## figure names
    show.results.description=TRUE;

    if(write.output==TRUE){
    is.nrisk.data.frame=TRUE;      ##so is.nrisk.data.frame=TRUE.
                         ## if users choose write.output=FALSE, then nrisk is array form,
                          ## so is.nrisk.data.frame=FALSE.
    write.jprat.output=TRUE; ## users need to choose write.jprat.output=TRUE
                            ## when they choose write.output=TRUE in jprat function;
                            ## FALSE otherwise.
    }else{
      is.nrisk.data.frame=FALSE;
      write.jprat.output=FALSE;
    }

    ####################################
    ## functions to get default values #
    ####################################
    #default.options<-default.options.for.data.setting()

    if(what.analyzed.separately=="studyevent"){


      study.names=c("cohort", "predict", "pharos");
      event.outcome.names=c("hdage_nobase", "mcione", "dep2");
      legend.names=c("Motor Diagnosis (DCL=4)")


    }else if(what.analyzed.separately=="event"){

      study.names=c("cohort", "predict", "pharos");
      event.outcome.names=c("hdage_nobase", "mcione", "dep2");
      legend.names=c("Motor Diagnosis (DCL=4)", "Cognitive Impairment", "Stage II TFC")

    }else if(what.analyzed.separately=="none"||what.analyzed.separately=="study"){

      color.names=c("firebrick1", "darkgreen"); ## for color.label.key
      event.outcome.names=c("event1", "event2")
      legend.names=c("Motor Diagnosis (DCL=4)", "Cognitive Impairment");

    }

    # reformatted.data.for.analysis.results<-data.reformatted.for.analysis.results(
    #   study.names, event.outcome.names,
    #   color.names, legend.names,
    #   which.nonfunctional.covariate.comparisons)

    ## obtain default values
    ## default values
    #use_real_data<-default.options$use_real_data
    #combine.data<-default.options$combine.data

    ## datasets are reformatted, so that results plots and tables can be produced;
    #num_study<-reformatted.data.for.analysis.results$num_study;
    jprat.output<-jprat.estimat.results$jprat.output
    nrisk<-number.at.risk



## a function to display all results including tables and plots
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
      #which.nonfunctional.covariate.comparisons,
      color.names=color.names, ## returns color.labels
      legend.names=legend.names, ## returns legend.labels
      functional.covariate.comparisons=functional.covariate.comparisons,
      functional.covariate.comparisons.for.sample.size=NULL,
      #############################
      # plot options             ##
      #############################
      do.plots=do.plots,
      plot.confidence.intervals=plot.confidence.intervals,
      ###########################
      # Label figures or files ##
      ###########################
      add.number.at.risk.legend=add.number.at.risk.legend,
      ylabel.for.plots.comparing.studies=ylabel.for.plots.comparing.studies,
      xlabel.for.plots.comparing.studies=xlabel.for.plots.comparing.studies,
      file.name.for.analysis=file.name.for.analysis, ## figure names
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
      show.results.description=show.results.description,
      is.nrisk.data.frame=is.nrisk.data.frame,
      write.jprat.output=write.jprat.output
    )


