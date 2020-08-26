submodule(results_interface) results_implementation
  use assertions_interface, only : assert
  implicit none

contains

  module procedure write_formatted
    integer i

    call assert(iotype=='LISTDIRECTED', "results_t%write_formtted: iotype='LISTDIRECTED'")

    if (allocated(this%header)) write(unit,'(5x,4(a,10x))') this%header, new_line('a')
    do i=1,size(this%body,1)
      write(unit,*) this%body(i,:), new_line('a')
    end do
  end procedure

  module procedure new_results_t
    new_results_t%header = header
    new_results_t%body = body
  end procedure

  module procedure states_t_array
    use array_functions_interface, only : operator(.catRows.)

    states_t_array%header = header
    allocate(states_t_array%body(0,size(header)))

    block
      integer i

      do i=1,size(states)
        states_t_array%body = states_t_array%body .catRows. states(i)%row_vector()
      end do
    end block

  end procedure

end submodule results_implementation
