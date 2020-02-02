! 4-14-2014: Added known alpha (actually a) estimate 
! 4-11-2014: code works for just beta(t) with original setting.
! 4-11-2014: we go back to basics and build up.
! 3-31-2014: changed data generation procedure
! 3-29-2014: added CAG repeat length

!to compile, use gfortran -O2 censmod.f90 main.f90 ~/gfortransub/*.f ~/gfortran/rnglib.f90 ~/gfortran/pdflib.f90 -o simu 
! fortran code for COHORT GLLVM

!--------------------------------------------
! Note: to have all carriers and no censoring
! set: mix_carrier=1, censorrate=0
!-------------------------------------------

module commondata 
  implicit none 
  save 
  double precision,parameter :: tol=1e-6
end module commondata 


