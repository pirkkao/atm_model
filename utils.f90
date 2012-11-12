module utils

  ! Contains utility functions and subroutines

  use precision
  use constants

  implicit none

contains

  function h_half(index)
   
  ! Depth between model half levels around model level i(ndex)
  
  integer, intent(in) :: index
  real(dp) :: h_half

  h_half = (h(index+1)-h(index-1))/2.

  end function h_half

end module utils
