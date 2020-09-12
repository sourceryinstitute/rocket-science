module results_interface
  use state_interface, only : state_t
  use oracle_interface, only : oracle
  use kind_parameters, only : rkind
  implicit none

  private

  type, public, extends(oracle) :: results_t
    private
    character(len=:), allocatable :: header(:)
    real(rkind), allocatable :: body(:,:)
  contains
    procedure :: write_formatted
    procedure :: norm
    procedure :: subtract
  end type

  interface results_t

    pure module function new_results_t(header, body)
      implicit none
      character(len=*), intent(in) :: header(:)
      real(rkind), intent(in) :: body(:,:)
      type(results_t) new_results_t
    end function

    pure module function states_t_array(header, states)
      implicit none
      character(len=*), intent(in) :: header(:)
      type(state_t), intent(in) :: states(:)
      type(results_t) states_t_array
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

    pure module function norm(this) result(norm_of_this)
      !! result is a norm of the array formed by concatenating the real components of this object
      implicit none
      class(results_t), intent(in) :: this
      real(rkind) norm_of_this
    end function

    module function subtract(this, rhs) result(difference)
      !! result has components corresponding to subtracting rhs's components fron this object's components
      implicit none
      class(results_t), intent(in) :: this
      class(oracle), intent(in) :: rhs
      class(oracle), allocatable :: difference
    end function

  end interface

end module results_interface
