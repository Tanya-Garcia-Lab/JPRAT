########################
## Clear out history ##
#######################
#rm(list=ls(all=TRUE))
library(abind)

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
get.unif.Ft <- function(m){
  out <- rep(0,m)
  tol <- 1e-6
  for(kk in 1:m){
    out[kk] <- min(max(runif(1),tol),1-tol)
  }
  return(out)
}

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



## generating new x,z each time
#' @importFrom abind adrop
simu.data <- function(randomeffects.covariates.dependent,
                      z_tmp.list,
                      x_tmp.list,
                      delta_tmp.list,
                      s_tmp.list,
                      a0,axmod,num_time=num_time,censorrate,
                      frform,fzrform,fxform,
                      type_fr,type_fzr,type_fx,
                      par1_fr,par2_fr,
                      par_fu,
                      par1_fr2,par2_fr2,mix_n,
                      par1_fx,par2_fx,
                      par1_fzr,par2_fzr,
                      real_data,time_val,
                      beta0int,beta=beta,gamma.param,omega.param,
                      n,nmax=nmax,m,maxm=maxm,la,lb,num_study=num_study,np,gtmod,use.random.effects,
                      gen.cens.depend.z,lb.max){


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

  lb.max <- max(unlist(lb))
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
        fzr <- make.normal(mean=par1_fzr[ss],sd=par2_fzr[ss],type=type_fzr)   # to forn birnak dustribution
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
                                             adrop(z[ss,i,,,drop=FALSE],drop=c(1,2)))
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
  }
  #else {
  #   ###############
  #   ## real data ##
  #   ###############
  #   for(ss in 1:num_study){
  #     for(k in 1:np){
  #       z[ss,1:n[ss],k,1:lb[[ss]][[k]]] <- z_tmp.list[[ss]][1:n[ss],k,1:1:lb[[ss]][[k]]]
  #     }
  #     x[ss,1:n[ss]] <- x_tmp.list[[ss]]
  #     s[ss,1:n[ss],1:max(m[ss,])] <- s_tmp.list[[ss]]
  #     delta[ss,1:n[ss],1:max(m[ss,])] <- delta_tmp.list[[ss]]
  #   }
  # }

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




###########################
## main simulation study ##
###########################
generate.datasets <- function(#method=c("gamm4","new")[1],
  a0,
  iseed,
  xks,
  frform,
  fzrform,
  fxform,
  type_fr,
  type_fzr,
  type_fx,
  par1_fr,
  par2_fr,
  par_fu,
  par1_fr2,
  par2_fr2,
  mix_n,
  par1_fzr,
  par2_fzr,
  par1_fx,
  par2_fx,
  axmod,
  num_time=num_time,
  censorrate,
  p,
  beta0int,
  beta0,
  gamma.param,
  omega.param,
  n,
  m,
  la,
  real_data,
  time_val,
  z_tmp.list=NULL,
  x_tmp.list=NULL,
  delta_tmp.list=NULL,
  s_tmp.list=NULL,
  #knot.length,
  randomeffects.covariates.dependent,
  #family.data,
  num_study=num_study,
  np=np,
  lb=lb,
  lb.max=lb.max,
  gtmod=gtmod,
  use.random.effects=use.random.effects,
  nmax=nmax,
  gen.cens.depend.z=gen.cens.depend.z,
  rescale=rescale,
  write.output=write.output
){

  ###############
  ## set terms ##
  ###############

  ## number of time points for evaluation
  #num_time <- length(time_val)

  ## number of x's at which to evaluate alpha(x,t)
  num_xx <- max(unlist(lapply(xks,length.apply)))

  ## maximum number of event types
  maxm <- max(m)

  ## maximum sample size across all studies
  #nmax <- max(n)

  if(num_study > 1){
    ## used to get all combinations to compare equality of studies.
    combi.study <- factorial(num_study) /
      (factorial(num_study-2)*factorial(2))
    combi.choice <- combn(1:num_study,2)
    combi.names <- apply(combi.choice,2,
                         function(x) paste(x,collapse=""))
  } else {
    ## no comparisons between studies made if num_study=1
    combi.study <- NULL
    combi.choice <- NULL
    combi.names <- NULL
  }




  ##############
  ## set seed ##
  ##############
  set.seed(iseed)
  print(paste("iseed=",iseed,sep=""))


  beta<-beta0
  ########################
  ## get simulated data ##
  ########################
  ## keep x, z fixed in each simulation
  data <- simu.data(randomeffects.covariates.dependent,z_tmp.list,x_tmp.list,
                    delta_tmp.list,s_tmp.list,
                    a0,axmod,num_time=num_time,censorrate,
                    frform,fzrform,fxform,
                    type_fr,type_fzr,type_fx,
                    par1_fr,par2_fr,
                    par_fu,
                    par1_fr2,par2_fr2,mix_n,
                    par1_fx,par2_fx,
                    par1_fzr,par2_fzr,
                    real_data,time_val=time_val,
                    p,beta0int,beta=beta,gamma.param=gamma.param,omega.param=omega.param,
                    n=n,nmax=nmax,m=m,maxm=maxm,la=la,lb=lb,num_study=num_study,
                    np=np,gtmod=gtmod,use.random.effects=use.random.effects,
                    gen.cens.depend.z=gen.cens.depend.z)


  names_study<-c("study1", "study2")
  data.sets<-get.empty.list(names_study)

  for (ss in 1:num_study){

    n<-length(data$z_start[ss,,1,1][data$z_start[ss,,1,1]!=0])


    z1.event1<-data$z_start[ss,1:n,1,]
    z1.event2<-data$z_start[ss,1:n,2,]
    #z1<-data$z_start[2,1:500,1,]
    #z2<-data$z_start[2,1:500,2,]

    CAG<-data$x_start[ss,1:n]
    #CAG<-data$x_start[2,1:500]

    event1<-data$s_start[ss,1:n,1]
    event2<-data$s_start[ss,1:n,2]
    #event1<-data$s_start[2,1:500,1]
    #event2<-data$s_start[2,1:500,2]


    #event1<-data$s_start[1,1:400,1]
    #event2<-data$s_start[1,1:400,2]
    #event1<-data$s_start[2,1:500,1]
    #event2<-data$s_start[2,1:500,2]

    delta.event1<-data$delta_start[ss,1:n,1]
    delta.event2<-data$delta_start[ss,1:n,2]
    #delta.event1<-data$delta_start[2,1:500,1]
    #delta.event2<-data$delta_start[2,1:500,2]

    data.event<-cbind(z1.event1, z1.event2, CAG, event1, event2, delta.event1, delta.event2)
    data.sets[[ss]]<-data.event
  }






  ## rescale covariates
  if(rescale==TRUE){

    names_study<-c("study1", "study2")
    input.data.sets<-get.empty.list(names_study)

    # new value =((old value- old min)/(old_max-old_min))*(new_max-mew_min)+(new_min)
    # new value =((x-0)/(1-0))*(50-36)+35

    for(ss in 1:num_study){

      Z1.event1<-data.sets[[ss]][, "z1.event1"]
      new.Z1.event1<-14*Z1.event1+36
      round.new.Z1.event1<-round(new.Z1.event1, 0)
      min(round.new.Z1.event1);
      max(round.new.Z1.event1);

      Z1.event2<-data.sets[[ss]][, "z1.event2"]
      new.Z2.event2<-14*Z1.event2+36
      round.new.Z2.event2<-round(new.Z2.event2, 0)
      min(round.new.Z2.event2);
      max(round.new.Z2.event2);

      CAG<-data.sets[[ss]][, "CAG"]
      new.CAG<-14*CAG+36
      round.new.CAG<-round(new.CAG, 0)
      min(round.new.CAG);
      max(round.new.CAG);

      event1<-data.sets[[ss]][, "event1"]
      event2<-data.sets[[ss]][, "event2"]

      delta.event1<-data.sets[[ss]][, "delta.event1"]
      delta.event2<-data.sets[[ss]][, "delta.event2"]

      input.data.sets[[ss]]<-cbind(round.new.Z1.event1, round.new.Z2.event2, round.new.CAG, event1, event2, delta.event1, delta.event2)
      colnames(input.data.sets[[ss]])<-c("base_age", "z2", "CAG", "event1", "event2", "delta.event1", "delta.event2")
    }

  }

  #filename <- paste("output_iseed_",iseed,".dat",sep="")
  if(write.output==TRUE){


    #write.table(data,paste("out_data_",filename,sep=""),
    #            col.names=FALSE,row.names=FALSE)

    write.csv(input.data.sets[[1]], file="simu_data1_for_jprat.csv")
    write.csv(input.data.sets[[2]], file="simu_data2_for_jprat.csv")
  }





  #return(data)
}
