########################
## Clear out history ##
#######################
rm(list=ls(all=TRUE))

## Generate input parameters for JPRAT function based on the simulation study
## See the parameter settings for model A in Garcial at al., Biostatisticis, (2017)

source("data-raw/get_parameters.R")
source("data-raw/simu_data_generation_for_jprat.R")

data.for.jprat<-get.parameters(param.setting=1)

iseed=data.for.jprat$iseed; # not used for data setup
beta0int=data.for.jprat$beta0int;
gamma.param=data.for.jprat$gamma.param;
omega.param=data.for.jprat$omega.param;
m=data.for.jprat$m;
n=data.for.jprat$n;
real_data=data.for.jprat$real_data;
num_study=data.for.jprat$num_study;
np=data.for.jprat$np;
lb=data.for.jprat$lb;
lb.max=data.for.jprat$lb.max;
time_val=data.for.jprat$time_val;
use.random.effects=data.for.jprat$use.random.effects;
randomeffects.covariates.dependent=data.for.jprat$randomeffects.covariates.dependent;

beta0=data.for.jprat$beta0;
a0=data.for.jprat$a0;
xks=data.for.jprat$xks;
num_time=data.for.jprat$num_time;
frform=data.for.jprat$frform;
fzrform=data.for.jprat$fzrform;
fxform=data.for.jprat$fxform;
type_fr=data.for.jprat$type_fr;
type_fzr=data.for.jprat$type_fzr;
type_fx=data.for.jprat$type_fx;
par_fu=data.for.jprat$par_fu;
par1_fr=data.for.jprat$par1_fr;
par2_fr=data.for.jprat$par2_fr;
par1_fr2=data.for.jprat$par1_fr2;
par2_fr2=data.for.jprat$par2_fr2;
mix_n=data.for.jprat$mix_n;
p=data.for.jprat$p;

par1_fzr=data.for.jprat$par1_fzr;
par2_fzr=data.for.jprat$par2_fzr;
par1_fx=data.for.jprat$par1_fx;
par2_fx=data.for.jprat$par2_fx;

axmod=data.for.jprat$axmod;
gtmod=data.for.jprat$gtmod;
censorrate=data.for.jprat$censorrate;
gen.cens.depend.z=data.for.jprat$gen.cens.depend.z;

maxm=data.for.jprat$maxm;
nmax=data.for.jprat$nmax;
write.output=data.for.jprat$write.output;

## the generated simulated data has nonfunctional covariate values between [0, 1]
## Using the scale function such as
## new value =((old value- old min)/(old_max-old_min))*(new_max-mew_min)+(new_min), i.e.,
## new value =((x-0)/(1-0))*(50-36)+35
## we generate the datasets, which mimics the real datasets for each study.
## rescale: default is TRUE.
rescale<-TRUE

data.sets1<-generate.datasets(a0,
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
  beta0=beta0,
  gamma.param,
  omega.param,
  n=n,
  m=m,
  real_data=real_data,
  time_val=time_val,
  z_tmp.list=NULL,
  x_tmp.list=NULL,
  delta_tmp.list=NULL,
  s_tmp.list=NULL,
  randomeffects.covariates.dependent=randomeffects.covariates.dependent,
  num_study=num_study,
  np=np,
  lb=lb,
  lb.max=lb.max,
  use.random.effects=use.random.effects,
  gtmod=gtmod,
  nmax=nmax,
  gen.cens.depend.z=gen.cens.depend.z,
  rescale=rescale,
  write.output=write.output
)
