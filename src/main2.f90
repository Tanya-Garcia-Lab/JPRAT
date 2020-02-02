! 5-12-2014: Main functions used in COHORT ext code


subroutine get_count_new(n,m,maxm,num_time,ynew,delta,count_new)
  implicit none
  integer, intent(in) :: n,maxm,num_time
  integer, dimension(n),intent(in) :: m
  double precision,dimension(n,maxm),intent(in) :: delta
  double precision,dimension(num_time,n,maxm),intent(in) :: ynew
  double precision,dimension(num_time,4),intent(out) :: count_new
  integer :: ii,jj,tt
  double precision :: tol
  tol=1e-6
  count_new=0.
  do ii=1,n
     do jj=1,m(ii)
        do tt=1,num_time
           if(ynew(tt,ii,jj).lt.tol) then
              ! ynew=0
              if(delta(ii,jj).gt.tol) then
                 ! delta=1, not censored
                 count_new(tt,2)=count_new(tt,2)+1.
              else
                 ! delta=0, censored
                 count_new(tt,3)=count_new(tt,3)+1.
              end if
           else if((ynew(tt,ii,jj).ge.(1.-tol)).and.(ynew(tt,ii,jj).le.(1.+tol))) then
              !ynew=1
              count_new(tt,1)=count_new(tt,1)+1.
           else
              count_new(tt,4)=count_new(tt,4)+1.
           end if
        end do
     end do
  end do
end subroutine get_count_new


subroutine get_count_outside(n,m,maxm,num_time,ynew,delta,count_new)
  implicit none
  integer, intent(in) :: n,maxm,num_time
  integer, dimension(n),intent(in) :: m
  double precision,dimension(n,maxm),intent(in) :: delta
  double precision,dimension(num_time,n,maxm),intent(in) :: ynew
  double precision,dimension(num_time,2),intent(out) :: count_new
  integer :: ii,jj,tt
  double precision :: tol
  tol=1e-6
  count_new=0.
  do ii=1,n
     do jj=1,m(ii)
        do tt=1,num_time
           if(ynew(tt,ii,jj).lt.0.) then
              ! ynew<0
              count_new(tt,1) = count_new(tt,1)+1.
           else if(ynew(tt,ii,jj).gt.1.) then
              !ynew>1
              count_new(tt,2)=count_new(tt,2)+1.
           end if
        end do
     end do
  end do
end subroutine get_count_outside

!--------------------------------------------------------------
!  Functions for pseudovalues
!-------------------------------------------------------------

subroutine kmjack(arbitrary,num_time,n,m,maxm,time_val,p,m0_qvs,y,ymiss_ind,s,q,delta,ynew,ynew_orig)
  implicit none
  logical,intent(in) :: arbitrary
  integer,intent(in) :: p,m0_qvs,num_time,n,maxm
  integer,dimension(n),intent(in) :: m
  double precision,dimension(num_time) :: time_val
  double precision,dimension(num_time,n,maxm),intent(in) :: y,ymiss_ind
  double precision,dimension(n,maxm),intent(in) :: s,delta
  double precision,dimension(p,n,maxm),intent(in) :: q
  double precision,dimension(num_time,n,maxm),intent(out) :: ynew,ynew_orig
  double precision,dimension(num_time,n,maxm) :: ynew2
  double precision,dimension(num_time,n,maxm) :: y0,y0miss_ind
  double precision,dimension(num_time,n,maxm) :: y0_new,y0_new_sorted
  double precision,dimension(p,m0_qvs,maxm) :: qvs
  double precision,dimension(p,n,maxm) :: q0
  double precision,dimension(p,n-1,maxm) :: qb
  double precision,dimension(n,maxm) :: s0,delta0,index_sort,ht
  double precision,dimension(n-1,maxm) :: sb,deltab,htb
  integer,dimension(m0_qvs,maxm) :: r,rb
  integer,dimension(n,maxm) :: id
  integer,dimension(n-1,maxm) :: idb
  double precision,dimension(m0_qvs,n,maxm):: s0sub,d0sub,sts,gts,hts
  double precision,dimension(m0_qvs,n-1,maxm):: sbsub,dbsub,stsb,gtsb,htsb
  double precision,dimension(p,num_time,maxm) :: Fest_full
  double precision,dimension(n,p,num_time,maxm) :: Fest_jk
  double precision :: time
  integer,dimension(maxm) :: m_qvs
  integer :: ind,tt,i,j,tmp,ii,kk
  double precision :: mycount
  double precision :: tol
  tol=1e-6
  !print*,'maxval(y)=',maxval(y)
  if(maxval(ymiss_ind).lt.tol) then
     ! no missing observations
     !print*,'hi there'
     !print*,'y=',y
     ynew=y
  else
     ! missing observations, we need to transform y

     !get WLS estimator
     ind=1

     ! put data into iid format, no covariates
     call putiid(num_time,n,maxm,p,m0_qvs,y,ymiss_ind,s,q,delta,&
          y0,y0miss_ind,s0,q0,delta0,m_qvs,qvs,r,index_sort)
     !print*,'m_qvs=',m_qvs,'r=',r
     !print*,'minval(r)=',minval(r)
     !do tt=1,num_time
     !   print*,'tt=',tt
     !   do i=1,n
     !      print*,'i=',i,'y(tt,i,:)=',y(tt,i,:)
     !   end do
     !end do

     ! set y0_new=y0
     y0_new=y0

     ! compute full estimator
     do kk=1,maxm
        call subinfo(p,n,m_qvs(kk),qvs(:,:,kk),&
             q0(:,:,kk),s0(:,kk),delta0(:,kk),r(:,kk),&
             s0sub(:,:,kk),d0sub(:,:,kk),sts(:,:,kk),gts(:,:,kk),hts(:,:,kk),&
             ht(:,kk),id(:,kk))
        do tt=1,num_time
           time=time_val(tt)
           call newsubtinfo(ind,n,m_qvs(kk),p,time,r(:,kk),&
                qvs(:,:,kk),s0sub(:,:,kk),d0sub(:,:,kk),&
                hts(:,:,kk),Fest_full(:,tt,kk))
           !print*,'tt=',time_val(tt),'Fest_full=',Fest_full(:,tt,kk)
        end do
     end do

     ! compute estimator for each censored value where y0=999
     do kk=1,maxm
        do i=1,n
           if(maxval(y0miss_ind(:,i,kk)).gt.tol) then
              ! there is at least one missing observation, so we will adjust data

              ! remove i_th data point
              !print*,'i=',i,'y0=',y0(:,i),'delta0=',delta0(i)
              call org_data(n,i,p,s0(:,kk),q0(:,:,kk),&
                   delta0(:,kk),m_qvs(kk),qvs(:,:,kk),&
                   r(:,kk),sb(:,kk),qb(:,:,kk),deltab(:,kk),rb(:,kk))

              ! get info for jack-knife KM estimator
              call subinfo(p,n-1,m_qvs(kk),qvs(:,:,kk),&
                   qb(:,:,kk),sb(:,kk),deltab(:,kk),rb(:,kk),&
                   sbsub(:,:,kk),dbsub(:,:,kk),stsb(:,:,kk),&
                   gtsb(:,:,kk),htsb(:,:,kk),htb(:,kk),idb(:,kk))

              do tt=1,num_time
                 time=time_val(tt)
                 if(y0miss_ind(tt,i,kk).gt.tol) then
                    ! do jack-knife KM estimator
                    call newsubtinfo(ind,n-1,m_qvs(kk),&
                         p,time,rb(:,kk),qvs(:,:,kk),&
                         sbsub(:,:,kk),dbsub(:,:,kk),htsb(:,:,kk),&
                         Fest_jk(i,:,tt,kk))
                    !print*,'tt=',time_val(tt),'Fest_jk=',Fest_jk(i,:,tt)

                    ! form jack-knife estimate
                    if(arbitrary) then
                       y0_new(tt,i,kk)=0.5
                    else
                       y0_new(tt,i,kk)=n * Fest_full(kk,tt,kk) - &
                            (n-1) * Fest_jk(i,kk,tt,kk)
                    end if
                    !print*,'i=',i,'tt=',time_val(tt),'q0=',q0(1,i),&
                    !    'y0_new=',y0_new(tt,i),'F_full=',Fest_full(:,tt), 'F_jk=',Fest_jk(i,:,tt)
                 end if
              end do
           end if
        end do
     end do

     ! re-order y0_new according to index_sort
     y0_new_sorted=0.
     do kk=1,maxm
        do i=1,n
           y0_new_sorted(:,int(index_sort(i,kk)),kk)=y0_new(:,i,kk)
        end do
     end do

!     do ii=1,n
!        print*,'ii=',ii,'y=',y(:,ii,1:m(ii))
!        print*,'ii=',ii,'y0_new_sorted=',y0_new_sorted(:,ii,1:m(ii))
!     end do


     !print*,'index_sort=',index_sort
     !do tt=1,num_time
     !   print*,'tt=',tt
     !   print*,'y0_new=',y0_new(tt,:,:)
     !   print*,'y0_new_sorted=',y0_new_sorted(tt,:,:)
     !   print*,'y0=',y0(tt,:,:)
     !end do

     ! re-construct ynew based on y0_new_sorted
     ynew=y0_new_sorted

     !do tt=1,num_time
     !   print*,'tt=',tt
     !   do i=1,n
     !      print*,'i=',i,'ynew(tt,i,:)=',ynew(tt,i,:)
     !   end do
     !end do

     !  adjust ynew values so that:
     !    ynew<0 ----> ynew=0
     !    ynew>1 ----> ynew=1
     ynew2=ynew
     ynew_orig=ynew

     !mycount=0.
     !do ii=1,n
     !   !if(maxval(ynew(:,ii,1:m(ii))).gt.(1.+tol)) then
     !   if(minval(ynew(:,ii,1:m(ii))).lt.0.) then
     !      mycount=mycount+1
     !      print*,'ii=',ii,'min_ynew=',minval(ynew(:,ii,1:m(ii))),'max=',maxval(ynew(:,ii,1:m(ii)))
     !   end if
     !end do

     !print*,'mycount=',mycount


     !print*,'ynew2=',ynew2
     do kk=1,maxm
        where(ynew2(:,:,kk)<0.)
           ynew2(:,:,kk)=0.
        end where
        where(ynew2(:,:,kk)>1.)
           ynew2(:,:,kk)=1.
        end where
        !print*,'ynew2,after=',ynew2
        ynew(:,:,kk)=ynew2(:,:,kk)
     end do
  end if


10  return
end subroutine kmjack


! subroutine to remove data point
subroutine org_data(n,i,p,s0,q0,delta0,m_qvs,qvs,r,sb,qb,deltab,rb)
  implicit none
  integer, intent(in) :: n,i,m_qvs,p
  double precision,dimension(p,n),intent(in) :: q0
  double precision,dimension(n),intent(in) :: s0,delta0
  double precision,dimension(p,m_qvs),intent(in) :: qvs
  integer,dimension(m_qvs),intent(in) :: r
  double precision,dimension(n-1),intent(out) :: sb,deltab
  double precision,dimension(p,n-1),intent(out) :: qb
  integer,dimension(m_qvs),intent(out) :: rb
  integer,dimension(n-1) :: index_main
  integer :: ii,j
  double precision :: tol
  tol=1e-6

  ! remove i_th entry
  if(i.eq.1) then
     index_main=(/(ii,ii=2,n)/)
  else if(i.eq.n)then
     index_main=(/(ii,ii=1,(n-1))/)
  else
     index_main(1:(i-1))=(/(ii,ii=1,(i-1))/)
     index_main(i:(n-1))=(/(ii,ii=(i+1),n)/)
  end if
  !print*,'index_main=',index_main

  ! get appropriate data
  sb=s0(index_main)
  deltab=delta0(index_main)
  qb=q0(:,index_main)

  !print*,'s0=',s0
  !print*,'sb=',sb

  ! adjust count in r. Remove from r the value of q0(:,i)
  rb=r
  do j=1,m_qvs
     if (sum(abs(qvs(:,j)-q0(:,i))).lt.tol) then
        rb(j)=r(j)-1
     end if
  end do
  !print*,'r=',r,'rb=',rb,'q0(:,i)=',q0(:,i)
  return
end subroutine org_data

! subroutine to put data (no covariates) into iid format
!  and return sorted data in increasing order of observed event times (s0)
subroutine putiid(num_time,n,maxm,p,m0_qvs,y,ymiss_ind,s,q,delta,y0,&
     y0miss_ind,s0,q0,delta0,m_qvs,qvs,r,index_sort)
  implicit none
  integer, intent(in) :: n,num_time,maxm,p,m0_qvs
  double precision,dimension(num_time,n,maxm),intent(in) :: y,ymiss_ind
  double precision,dimension(n,maxm),intent(in) :: s,delta
  double precision,dimension(p,n,maxm),intent(in) :: q
  double precision,dimension(num_time,n,maxm),intent(out) :: y0,y0miss_ind
  double precision,dimension(n,maxm),intent(out) :: s0,delta0,index_sort
  double precision,dimension(p,n,maxm),intent(out) :: q0
  integer,dimension(m0_qvs,maxm),intent(out) :: r
  double precision,dimension(p,m0_qvs,maxm),intent(out) :: qvs
  integer,dimension(maxm),intent(out) :: m_qvs
  double precision,dimension(n) :: utmp,u
  integer :: i,j,tt,tmp,kflag,kk,ii,jj
  double precision :: tol
  tol=1e-6

  ! for sorting in ascending order
  kflag=2

  ! put data into a long vector form
  y0=y
  y0miss_ind=ymiss_ind
  s0=s
  delta0=delta
  q0=q
  do ii=1,maxm
     index_sort(:,ii)=(/(jj,jj=1,n)/)
  end do

  !print*,'y=',y
  !print*,'y0=',y0
  !print*,'q0=',q0(1,:)
  !print*,'s0=',s0
  !print*,'index_sort=',index_sort

  !------------------------
  ! sort data in increasing order
  !----------------------------
  do kk=1,maxm
     utmp=s0(:,kk)

     ! sort q0 in increasing order of u
     do j=1,p
        u=utmp
        call dsort(u,q0(j,:,kk),n,kflag)
     end do

     ! sort y0 in increasing order of u
     do tt=1,num_time
        u=utmp
        call dsort(u,y0(tt,:,kk),n,kflag)
     end do
     !print*,'after sorting'
     !print*,'y0=',y0

     ! sort y0miss_ind in increasing order of s0
     do tt=1,num_time
        u=utmp
        call dsort(u,y0miss_ind(tt,:,kk),n,kflag)
     end do
     ! sort delta0 in increasing order of s0
     u=utmp
     call dsort(u,delta0(:,kk),n,kflag)

     ! sort index_sort and s0 in increasing order of s0
     call dsort(s0(:,kk),index_sort(:,kk),n,kflag)

  end do

  !do kk=1,maxm
  !   print*,'kk=',kk
  !   print*,'s0=',s0
  !   print*,'index_sort=',index_sort
  !end do

  !---------------------------
  ! get r,m_qvs, and qvs for KM estimator
  !------------------------------
  r=0.
  m_qvs=0.
  qvs=0.
  do kk=1,maxm
     i=1
1    if(i.le.n) then
        j=1
2       if(j.le.m_qvs(kk)) then
           if (sum(abs(qvs(:,j,kk)-q0(:,i,kk))).lt.tol) then
              r(j,kk)=r(j,kk)+1
              i=i+1
              goto 1
           else
              j=j+1
              goto 2
           end if
        else
           m_qvs(kk)=m_qvs(kk)+1
           r(m_qvs(kk),kk)=1
           qvs(:,m_qvs(kk),kk)=q0(:,i,kk)
           i=i+1
           goto 1
        end if
     end if
  end do
!  print*,'qvs(1,:)=',qvs(:,1)
!  print*,'qvs(2,:)=',qvs(:,2)
!  print*,'r=',r
!  print*,'m_qvs=',m_qvs
!  print*,'q0(1,)=',q0(1,:)
  return
end subroutine putiid



!Kaplan-Meier estimator of the censoring process G(t) at x_i's
subroutine hatG(n,x,delta,gt)
  implicit none
  integer,intent(in) :: n
  double precision,dimension(n),intent(in) :: x,delta
  double precision,dimension(n),intent(out) :: gt
  integer :: i
  gt(1)=1.-(1.-delta(1))/n
  do i=2,n
     gt(i)=gt(i-1)*(1.-(1.-delta(i))/(n-i+1))
  end do
  gt(n)=gt(n-1) !avoid 0
  return
end subroutine hatG


!Kaplan-Meier estimator of event process S(t) and 1-S(t) at x_i's
subroutine hatS(n,x,delta,st,ht)
  implicit none
  integer,intent(in) :: n
  double precision,dimension(n),intent(in) :: x,delta
  double precision,dimension(n),intent(out) :: st,ht
  integer :: i
  st(1)=1.-delta(1)/n
  do i=2,n
     st(i)=st(i-1)*(1.-delta(i)/(n-i+1))
  end do
  ht=1-st
!  st(n)=st(n-1) !avoid 0
  !  ht(n)=ht(n-1)
  return
end subroutine hatS



!get m subsample information
subroutine subinfo(p,n,m,qvs,q,x,delta,r,xsub,dsub,sts,gts,hts,ht,id)
  implicit none
  integer,intent(in) :: p,n,m
  double precision,dimension(p,m),intent(in) :: qvs
  double precision,dimension(p,n),intent(in) :: q
  double precision,dimension(n),intent(in) :: x,delta
  integer,dimension(m),intent(in) :: r
  double precision,dimension(m,n),intent(out) :: xsub,dsub,sts,gts,hts
  double precision,dimension(n),intent(out) :: ht
  integer,dimension(n),intent(out) :: id
  integer,dimension(m,n) :: ida
  double precision,dimension(p) :: ys
  double precision :: tol
  integer :: i,j,cnt
  tol=1e-6
  xsub=0
  dsub=0

  ! for testing
  !print*,'n=',n
  !print*,'m=',m
  !print*,'qvs=',qvs
  !print*,'r=',r

  do j=1,m
     cnt=1
     do i=1,n
        if (sum(abs(q(:,i)-qvs(:,j))).le.tol) then
           xsub(j,cnt)=x(i)
           dsub(j,cnt)=delta(i)
           id(i)=j
           ida(j,cnt)=i
           cnt=cnt+1
        end if
     end do
     !print*,'j=',j,'r(j)=',r(j)
     call hatS(r(j),xsub(j,:),dsub(j,:),sts(j,:),hts(j,:))
     call hatG(r(j),xsub(j,:),dsub(j,:),gts(j,:))
     do i=1,r(j)
        ht(ida(j,i))=hts(j,i)
     end do
  end do
! test with the oracle weights
!  do i=1,n
!     call trueFt(p,x(i),ys)
!     call inprod(p,q(:,i),ys,ht(i))
!  end do
!finishing test with the oracle weights
  return
end subroutine subinfo


!get m subsample information at t, transform K-M to get estimate
subroutine newsubtinfo(ind,n,m,p,t,r,qvs,xsub,dsub,hts,Fa)
  implicit none
  integer,intent(in) :: ind,n,m,p
  double precision,intent(in) :: t
  integer,dimension(m),intent(in) :: r
  double precision,dimension(p,m),intent(in) :: qvs
  double precision,dimension(m,n),intent(in) :: xsub,dsub,hts
  double precision,dimension(p),intent(out) :: Fa
  double precision,dimension(p) :: ys
  double precision,dimension(m) :: ht0,sig2,w,tmp
  double precision,dimension(p,p) :: a1,A,Ainv
  !double precision :: tol
  integer :: i,j
  double precision :: tol
  tol=1e-6
  if (ind.eq.0) then
     w=1
  end if
  if (ind.eq.1) then
     w=r
  end if
!  if (ind.eq.2) then
!     tol=1e-2
!     do j=1,m
!        sig2(j)=0
!        i=1
!5       if (i.le.r(j)) then
!           if (t.ge.xsub(j,i)) then
!              if (dsub(j,i).gt.0.5) then
!                 sig2(j)=sig2(j)+1./(max((r(j)-i)*(r(j)-i+1.),tol))
!              end if
!           end if
!           i=i+1
!           goto 5
!        end if
!        !using true ht0 for oracle
!        call trueFt(p,t,ys)
!        tmp=matmul(transpose(qvs),ys)
!        sig2(j)=sig2(j)*(1-tmp(j))**2.
!        w(j)=1./max(sig2(j),tol)
!     end do
!  end if

  !print*,'r=',r
  do j=1,m
     ht0(j)=0
     i=1
10   if (i.le.r(j)) then
        if (t.ge.xsub(j,i)) then
           ht0(j)=hts(j,i)
        end if
        i=i+1
        goto 10
     end if
  end do
  !print*,'ht0=',ht0

  A=0
  ys=0
  do j=1,m
     call mul(p,qvs(:,j),qvs(:,j),a1)
     A=A+w(j)*a1
     ys=ys+w(j)*ht0(j)*qvs(:,j)
  end do
  if(minval(r).lt.tol) then
     call inv(p,A,Ainv)
     print*,'Ainv(1,:)=',Ainv(1,:),'Ainv(2,:)=',Ainv(2,:),'ys=',ys
     Fa=matmul(Ainv,ys)
  else
     ys=0.
     do j=1,m
        ys=ys+ht0(j)*qvs(:,j)
     end do
     Fa=ys
  end if
  !print*,'Fa=',Fa
  return
end subroutine newsubtinfo


! multiply vector with itself: y=tt'
subroutine muly(lb,t,y)
  integer,intent(in) :: lb
  double precision,dimension(lb),intent(in) :: t
  double precision,dimension(lb,lb),intent(out) :: y
  integer :: i,j
  y=0
  do i=1,lb
     do j=1,lb
        y(i,j)=t(i)*t(j)
     end do
  end do
  return
end subroutine muly

! invert a matrix
subroutine inv(lb,A,A1)
  integer,intent(in) :: lb
  double precision,dimension(lb,lb),intent(in) :: A
  double precision,dimension(lb,lb),intent(out) :: A1
  integer :: i
  integer,dimension(lb) :: ipvt
  double precision,dimension(lb,lb) :: B
  double precision :: cond
  double precision,dimension(lb) :: wv
  A1=0
  do i=1,lb
     A1(i,i)=1
  end do
  call decomp(lb,lb,A,cond,ipvt,wv)
  do i=1,lb
     call solvels(lb,lb,A,A1(:,i),ipvt)
  end do
  return
end subroutine inv


! multiply vectors: y=st'
subroutine mul(lb,s,t,y)
  integer,intent(in) :: lb
  double precision,dimension(lb),intent(in) :: s,t
  double precision,dimension(lb,lb),intent(out) :: y
  integer :: i,j
  do i=1,lb
     do j=1,lb
        y(i,j)=s(i)*t(j)
     end do
  end do
  return
end subroutine mul

! set all elements of matrix to a particular value
subroutine setB(p,q,a,B)
  implicit none
  integer, intent(in) :: p,q
  double precision,intent(in) :: a
  double precision,dimension(p,q),intent(out) :: B
  integer :: ii,jj
  do ii=1,p
     do jj=1,q
        B(ii,jj) = a
     end do
  end do
  return
end subroutine setB

