submodule(results_interface) results_implementation
  use assertions_interface, only : assert
  implicit none

contains

  module procedure write_formatted
    integer i

    call assert(iotype=='LISTDIRECTED', "results%write_formtted: iotype='LISTDIRECTED'")
    if (allocated(this%header)) print *, this%header
    do i=1,size(this%body,1)
      print *,this%body(i,:)
    end do
  end procedure

  module procedure new_results_t
    new_results_t%header = header
    new_results_t%body = body
  end procedure

end submodule results_implementation
