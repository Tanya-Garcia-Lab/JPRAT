## read data
data_predict<-read.csv("data-raw/data_predict.csv")
data_cohort<-read.csv("data-raw/data_cohort.csv")
data_pharos<-read.csv("data-raw/data_pharos.csv")

## parameters
use.real.data=TRUE;
study.names=c("cohort", "predict", "pharos");


time.points.for.prediction=seq(46, 66, by=5)
use_real_data=TRUE;
study.names=c("cohort", "predict", "pharos");
data.file.names=c("cohort", "predict", "pharos");
time.points.for.prediction=seq(46, 66, by=5)
nonfunctional.covariate.names=c("base_age");
functional.covariate.names="CAG";
nonfunctional.covariate.value=c(40);
functional.covariate.values.of.interest=c(46, 48, 50) ;

## functions
get.empty.list <- function(names){
  out <-  vector("list",length(names))
  names(out) <- names
  return(out)
}

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



get.functional.covariate.values.for.prediction <- function(#use_real_data,
                                                           xmin,xmax,functional.covariate.values.of.interest){

  #if(use_real_data==FALSE){
  #  functional.covariate.values.for.prediction <- seq(0,1,by=0.01)
  #} else {
    functional.covariate.values.for.prediction.tmp <- seq(0,1,by=0.1)
    functional.covariate.values.for.prediction <- c(functional.covariate.values.for.prediction.tmp,
                                                    reverse.cag(functional.covariate.values.of.interest,xmin=xmin,xmax=xmax))
    functional.covariate.values.for.prediction <- unique(sort(functional.covariate.values.for.prediction))
  #}

  return(functional.covariate.values.for.prediction)
}



## convert CAG to uniform CAG
reverse.cag <- function(x,xmin=xmin,xmax=xmax){
  xnew <- (x-xmin)/(xmax-xmin)
  return(xnew)
}



##
data.reformatted.for.jprat.analysis<-function(use_real_data,
                                              study.names,
                                              data.file.names,
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
  data<-data.file.names

  for(ss in 1:length(study.names)){
    study.use <- names(tmp.list)[ss]
    #print(study.use)
    data.tmp <- data.sets.as.list[ss]

    study <- study.names[ss]


    data.tmp <-read.csv(paste("data-raw/data_",study,".csv",sep=""),
                          header=TRUE)[ , -1]
    data.sets.as.list[[study]] <- data.tmp

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
  #                                                                                                             functional.beta.intercept,analysis.to.run,nonfunctional.covariate.value.is.0)
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

  functional.covariate.values.for.prediction <- get.functional.covariate.values.for.prediction(use.real.data,
                                                                                               xmin,xmax,functional.covariate.values.of.interest)                           ## make.data.for.analysis$functional.covariate.values.for.prediction	##[0,1] ## seq(0,1, by=0.01)



  return(list(
    data.sets.as.list=data.sets.as.list,
    nonfunctional.covariate.values.for.prediction=nonfunctional.covariate.values.for.prediction,
    xmin=xmin, xmax=xmax,
    functional.covariate.values.for.prediction=functional.covariate.values.for.prediction
  ))


}

data.reformatted<-data.reformatted.for.jprat.analysis(use_real_data,
                                                      study.names,
                                                      data.file.names,
                                                      time.points.for.prediction,
                                                      nonfunctional.covariate.names,
                                                      functional.covariate.names,
                                                      nonfunctional.covariate.value,
                                                      functional.covariate.values.of.interest
)


###
default.options.for.data.setting <- function(){ #use.bootstrap.variance


  ###########################################
  ## iseed: seed set for simulation study. ##
  ###########################################

  #if(!exists(as.character(substitute(iseed)))){
  #  iseed <- 1
  #}


  #################################################
  ## number.of.simulations: 1 for data analysis ###
  #################################################

  if(!exists(as.character(substitute(number.of.simulations)))){
    number.of.simulations <- 1
  }

  #############################
  ## Use only real data sets ##
  #############################

  if(!exists(as.character(substitute(use_real_data)))){
    use_real_data <- TRUE
  }

  #####################################################################################################################
  ## clusters.are.families: indicator if the clusters are families (i.e., each member in the cluster is different).  ##
  ##  In this paper, the cluster within a study corresponds to events from the same individual.                      ##
  ##  That is, the cluster info is from the same individual.                                                         ##
  #####################################################################################################################
  if(!exists(as.character(substitute(clusters.are.families)))){
    clusters.are.families <- FALSE
  }


  if(!exists(as.character(substitute(combine.data)))){
    combine.data <- FALSE
  }



  # estimate.variances: variance is estimated using gamm4
  if(!exists(as.character(substitute(estimate.variances)))){
    estimate.variances <- "est" # est, quant, none
  }






  # glm.link.type : link to be used in GAMM
  if(!exists(as.character(substitute(glm.link.type)))){
    glm.link.type <- "logit" ## "logit" for proportional odds model; "cloglog" for Cox PH model.
  }



  #####################################################
  ##  To get results                                 ##
  #####################################################

  ## estimated.parameters.common.for.all.studies: common alpha, beta across studies
  if(!exists(as.character(substitute(paper.type)))){
    paper.type <- "clinical"
  }

  if(!exists(as.character(substitute(plot.parameters)))){
    plot.parameters <- FALSE
  }

  #if(use.bootstrap.variance==TRUE){
  #  var.lo <- "varlo"##"number.of.bootstraps_varlo"
  #  var.hi <- "varhi"##"number.of.bootstraps_varlo"
  #} else{
  #  var.lo <- "varlo"
  #  var.hi <- "varhi"
  #}

  ## plot.kaplan.meier.curves: include Kaplan-Meier estimator in plots?
  ## This is to make Figure 1 in Biostatistics paper.
  #if(!exists(as.character(substitute(plot.kaplan.meier.curves)))){
  #  plot.kaplan.meier.curves <- FALSE
  #}

  list(
    use_real_data=use_real_data,
    number.of.simulations=number.of.simulations,
    estimate.variances=estimate.variances,
    glm.link.type = glm.link.type,
    clusters.are.families =clusters.are.families,
    paper.type=paper.type,
    plot.parameters=plot.parameters,
    combine.data=combine.data
    ##
    #iseed=iseed,
    #plot.kaplan.meier.curves=plot.kaplan.meier.curves,
    #family.data=family.data,
    #var.lo =var.lo,
    #var.hi= var.hi,
    #splines.constrained.at.0 = splines.constrained.at.0,
    #number.of.bootstraps =number.of.bootstraps,
    #check.study.equality=check.study.equality,
    #what.analyzed.separately =what.analyzed.separately,
    #estimated.parameters.common.for.all.studies =estimated.parameters.common.for.all.studies
  )

}

##

criteria.for.jprat.analysis<-function(#data.sets.as.list,
  study.names,
  data.file.names,
  time.points.for.prediction,
  nonfunctional.covariate.names,
  functional.covariate.names,
  nonfunctional.covariate.value,
  time.points.for.conditional.prediction,
  functional.covariate.values.of.interest,
  #xmin,
  #xmax,
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
                                                            data.file.names,
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
