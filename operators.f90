module operators
 
  ! Contains subroutines for calculating time derivatives of momentum,
  ! heat and tracers, etc.

  use precision
  use constants
  use utils

  implicit none

  contains

subroutine momentum_dt(V1,V2,K_m,dU)

  real(dp), dimension(nz), intent(in)  :: V1,V2
  real(dp), dimension(nz-2), intent(out) :: dU
  real(dp), dimension(nz-1), intent(in)  :: K_m

  integer :: i

! dU/dt = f * (V - Vg) + d/dz (K * dU/dz)

  do i=2,nz-1
    dU(i-1) = f * (V2(i) - V2(nz)) + &
           K_m(i)*(V1(i+1)-V1(i))/(h(i+1)-h(i)) &
          -K_m(i-1)*(V1(i)-V1(i-1))/(h(i)-h(i-1))  &
          / h_half(i)
  end do

end subroutine momentum_dt

subroutine heat_tracer_dt(C,K,dC,source)

  real(dp), dimension(nz), intent(in)  :: C
  real(dp), dimension(nz-2), intent(out) :: dC
  real(dp), dimension(nz-1), intent(in)  :: K
  real(dp), intent(in), optional  :: source
  integer :: i

! dC/dt = d/dz (K * dC/dz)

  if (present(source)) then
    do i=2,nz-1
      dC(i-1) = K(i)*(C(i+1)-C(i))/(h(i+1)-h(i)) &
               -K(i-1)*(C(i)-C(i-1))/(h(i)-h(i-1))  &
              / h_half(i) &
              + source
    end do
  else
    do i=2,nz-1
      dC(i-1) = K(i)*(C(i+1)-C(i))/(h(i+1)-h(i)) &
               -K(i-1)*(C(i)-C(i-1))/(h(i)-h(i-1))  &
              / h_half(i)
    end do
  end if

end subroutine heat_tracer_dt

  subroutine richardson(u,v,theta,fm,fh,ri)

    integer :: i
    real(dp), dimension(nz), intent(in) :: u,v,theta
    real(dp), dimension(nz-1), intent(out) :: fm,fh
    real(dp), dimension(nz-1), intent(out) :: ri

    do i = 1,nz-1

      ri(i) = g/((theta(i+1)+theta(i))/2.) * ((theta(i+1)-theta(i)) &
& /((u(i+1)-u(i))**2. +(v(i+1)-v(i))**2.)) * (h(i+1)-h(i))

        if (ri(i) .lt. 0.0) then

          fm(i) = (1.-16.*ri(i))**(1./2.)
	  fh(i) = (1.-16.*ri(i))**(3./4.)

        else if (ri(i) .gt. 0.0 .and. ri(i) .lt. 0.2) then

  	  fm(i) = (1.-5.*ri(i))**2. 
	
	  if (fm(i) .lt. 0.1) fm(i) = 0.1

	  fh(i) = fm(i)
	
        else if (ri(i) .gt. 0.2) then

	  fm(i) = 0.1
	  fh(i) = 0.1

        end if

    end do

    return

  end subroutine richardson


  subroutine vaihtokerroin(u,v,fm,fh,K_m,K_h)
    integer i
    real(dp), dimension(nz), intent(in)  :: u,v 
    real(dp), dimension(nz-1), intent(in) :: fm,fh
    real(dp), dimension(nz-1), intent(out) :: K_m,K_h

    do i = 1,nz-1

      K_m(i) = (k*h(i)/(1.+k*h(i)/lambda))**2. &
        *ABS((u(i+1)-u(i))/(h(i+1)-h(i)) + (v(i+1)-v(i))/(h(i+1)-h(i))) * fm(i)

      K_h(i) = (k*h(i)/(1.+k*h(i)/lambda))**2. &
        *ABS((u(i+1)-u(i))/(h(i+1)-h(i)) + (v(i+1)-v(i))/(h(i+1)-h(i))) * fh(i)

    end do
  end subroutine vaihtokerroin

end module operators
