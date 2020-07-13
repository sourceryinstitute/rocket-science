program main
  use inflator_module, only : inflator_t, define
  implicit none
  type(inflator_t) inflator
  character(len=*) , parameter :: input_file = "volfil.inp"

  call define(inflator, input_file)

  print *,"Test passed."
end program
