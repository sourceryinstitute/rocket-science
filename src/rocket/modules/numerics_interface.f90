module numerics_interface
  !! Encapsulate simulation numerical parameters: time step and final time.
  use kind_parameters, only : rkind
  implicit none

  private

  type, public :: numerics_t
    private
    real(rkind) :: dt_, t_max_
  contains
    procedure :: define, dt, t_max
  end type

  interface

    module subroutine define(this, input_file)
     class(numerics_t), intent(out) :: this
     character(len=*), intent(in) :: input_file
    end subroutine

    pure module function dt(this)
      class(numerics_t), intent(in) :: this
      real(rkind) dt
    end function

    pure module function t_max(this)
      class(numerics_t), intent(in) :: this
      real(rkind) t_max
    end function

  end interface

end module numerics_interface
