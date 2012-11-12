program boundary_layer_model
!
! Program for modelling boundary layer interactions.
!
!
! Authors: P. Ollinaho,
!          T. Perttula,
!          T. Olsson
!
!

  use precision
  use constants
  use utils
  use operators
  use boundary

  implicit none

  real(dp),dimension(nz) :: u,v,theta
  real(dp), dimension(nz-1) :: fm, fh, ri, K_m, K_h
  real(dp), dimension(nz-2) :: dU, dV, dH
  integer :: i

!  write(*,*) h_half(30)

  call initialize(u,v,theta)

!  write(*,*) u

  call richardson(u,v,theta,fm,fh, ri)

  do i=1,nz
    write(*,*) ri(i), fm(i), fh(i)
  end do

  call vaihtokerroin(u,v,fm,fh,K_m,K_h)

  do i=1,nz
    write(*,*) K_m(i), K_h(i)
  end do

  call momentum_dt(u,v,K_m,dU)

  call momentum_dt(v,u,K_m,dV)

  write(*,*)

  do i=1,nz-2
    write(*,*) dU(i),dV(i)
  end do

  call heat_tracer_dt(theta,K_h,dH,-1.0_dp)

  write(*,*)

  do i=1,nz-2
    write(*,*) dH(i)
  end do

contains


  

end program boundary_layer_model

