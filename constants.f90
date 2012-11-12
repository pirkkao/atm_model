module constants
 
  use precision

  implicit none

  ! Number of height levels:
  integer, parameter :: nz = 50
  ! Model height levels:
  real(dp), parameter, dimension(nz) :: &
    h = (/ 0,10,20,30,40,50,60,70,80,90,100,120,140,160,180,200, & ! 16
    230,260,300,350,400,450,500,550,600,650,700,800,900,1000,    & ! 14
    1100,1200,1300,1400,1500,1600,1700,1800,1900,2000,           & ! 10
    2100,2200,2300,2400,2500,2600,2700,2800,2900,3000 /)           ! 10

  ! Some constants:
  real(dp), parameter :: lambda = 300.  ! maximum mixing length, meters
  real(dp), parameter :: k = 0.4   ! von Karman constant, dimensionless
  real(dp), parameter :: g = 9.81  ! gravitation, m/s**2
  real(dp), parameter :: pi = 2*ASIN(1.)
  real(dp), parameter :: Omega = 2*pi/(24.*60.*60.)  ! Earth angular speed
  ! latitude of Hyytiälä in degrees:
  real(dp), parameter :: latitude_deg = (61. + 50./60. + 50.685/(60.*60.))
  real(dp), parameter :: latitude = latitude_deg * pi/180.0  ! in radians
  real(dp), parameter :: f = 2*Omega*SIN(latitude)  ! Coriolis parameter
  ! http://scienceworld.wolfram.com/physics/CoriolisParameter.html

end module constants
