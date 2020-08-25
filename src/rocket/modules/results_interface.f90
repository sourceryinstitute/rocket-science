module results_interface
  use kind_parameters, only : rkind
  implicit none

  private

  type, public :: results_t
    private
    character(len=:), allocatable :: header
    real(rkind), allocatable :: body(:,:)
  contains
    procedure, private :: write_formatted
    generic :: write(formatted) => write_formatted
  end type

  interface results_t

    pure module function new_results_t(header, body)
      implicit none
      character(len=*), intent(in) :: header
      real(rkind), intent(in) :: body(:,:)
      type(results_t) new_results_t
    end function

  end interface

  interface

    module subroutine write_formatted(this, unit, iotype, vlist, iostat, iomsg)
      implicit none
      class(results_t), intent(in) :: this
      integer, intent(in) :: unit
      character(len=*), intent(in) :: iotype
      integer, intent(in) :: vlist(:)
      integer, intent(out) :: iostat
      character(len=*), intent(inout) :: iomsg
    end subroutine

  end interface

end module results_interface
