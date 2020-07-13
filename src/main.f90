program volfil
  !! Test composite inflator abstraction
  use inflator_module, only : inflator_t, define, set_time, get_time, get_tmax, increment_time
  use kind_parameters, only : DP
  implicit none
  type(inflator_t) inflator

  call define(inflator, input_file = "volfil.inp")

  call set_time(inflator, t=0._DP)

  associate(final_time => get_tmax(inflator))

    do while(get_time(inflator) < final_time)

      call increment_time(inflator)
    end do

  end associate

  print *,"Test passed."
end program
