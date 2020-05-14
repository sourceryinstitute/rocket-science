program volfil
  !use kind_parameters, only : DP
  use flags_module, only : flags, initialize
  implicit none

  type(flags)::flag

  call initialize(flag, input_file='volfil.inp')

  print *,"Test passed."

end program
