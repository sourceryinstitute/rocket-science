program volfil
  !! Test composite inflator abstraction
  use inflator_module, only : inflator_t, define
  implicit none
  character(len=*), parameter :: input_file = "volfil.inp"
  type(inflator_t) inflator

  call define(inflator, input_file)

  print *,"Test passed."
end program
