program main
  !! Test nozzle abstraction
  use nozzle_interface, only : nozzle_t
  implicit none

  type(nozzle_t) nozzle

  print *," Test passed."

end program main
