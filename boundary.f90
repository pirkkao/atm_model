module boundary

  ! Contains initilization, ground boundary and atmospheric boundary values.

  use precision
  use constants

  implicit none

contains

  SUBROUTINE initialize(u,v,theta)
    REAL(dp), DIMENSION(nz), INTENT(out) :: u,v,theta

    u(1) = 0.
    u(nz) = 10.
    ! linear profile from u(1) = 0 to u(nz) = 50
    u(2:nz-1) = u(nz) * h(2:nz-1)/h(nz)

    v = 0.

    theta(1) = 273.15 + 25
    theta(nz) = 273.15 + 30 ! boudary condition at top
    theta(2:nz-1) = theta(nz) * h(2:nz-1)/h(nz)

  END SUBROUTINE initialize

  SUBROUTINE surface_values(temperature,time)

    ! (Note: can also get water concentrantion, in ppt, if modify this
    ! subroutine to also use column 8)
    !
    ! Data is taken from:
    ! http://www.atm.helsinki.fi/~junninen/smartSearch/smartSearch.php

    REAL(dp), INTENT(in)            :: time ! input, in seconds
    REAL(dp), INTENT(out)           :: temperature ! output, in Kelvin
    LOGICAL, SAVE                   :: first_time = .TRUE.
    REAL(dp), DIMENSION(8,50), SAVE :: surface_data
    REAL(dp), DIMENSION(50), SAVE   :: temperature_data
    REAL(dp), PARAMETER             :: seconds_in_day = 24*60*60
    REAL(dp), PARAMETER             :: seconds_in_30min = 30*60
    INTEGER                         :: index
    REAL(dp) :: time24h, time30min, time24plus15, temp1, temp2, x
    ! INTEGER  :: i

    ! Only when called for the first time, read in data from file
    ! With this trick, we don't need to open the file in the main program
    IF (first_time) THEN
      OPEN(30,file='hyytiala_2011_8_10_t_h2o.dat')
      READ(30,*) surface_data
      temperature_data(1:50) = surface_data(7,1:50) ! in Celcius
      first_time = .FALSE.
    END IF

    time24h = MODULO(time, seconds_in_day) ! time modulo 24 hours
    time24plus15 = time24h + 15*60 ! time since 23:45 previous day
    time30min = MODULO(time24plus15, seconds_in_30min)
    index = 1 + FLOOR(time24plus15/seconds_in_30min)

    temp1 = temperature_data(index)
    temp2 = temperature_data(index + 1)
    x = time30min/seconds_in_30min

    ! For debugging
    ! DO i = 1,50
    !   WRITE(*,*) temperature_data(i)
    ! END DO

    ! linear interpolation between previous and next temperature data value
    temperature = temp1 + x*(temp2 - temp1) + 273.15 ! now in Kelvin
    
  END SUBROUTINE surface_values

end module boundary
