#/home/grad/unkyunglee/HDConverters/simus_numstudy2/gen_nocommon_param/analyze_noseparate/est_nocommon/cens40/
# 1.runsimu :
# this is the file to run the original simulation code under the parameter setting (Model A)


source("get_parameters.R")
source("simu_data_generation_for_jprat.R")

data.for.jprat<-get.parameters(param.setting=1)


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
write.output=data.for.jprat$write.output;

data.sets1<-generate.datasets.uk(#method=c("gamm4","new")[1],
  a0,
  iseed,
  #simus,
  #m0_qvs,
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
  num_time,
  censorrate,
  p,
  beta0int,
  beta0,
  gamma.param,
  omega.param,
  n,
  m,
  #la,
  real_data,
  time_val,
  #time_choice.predicted,
  #time_choice.predicted.toadd,
  #boot,
  #var.boot,
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
  #zeval=zeval,
  #z.choice=z.choice,
  #spline.constrain=spline.constrain,
  gtmod=gtmod,
  #use.random.effects=use.random.effects,
  #analyze.separately=analyze.separately,
  #check.study.equality=check.study.equality,
  #common.param.estimation=common.param.estimation,
  #param.label,
  #arbitrary,
  gen.cens.depend.z=gen.cens.depend.z,
  write.output=write.output
  #est.cens.depend.z=est.cens.depend.z,
  #link.type=link.type
)
