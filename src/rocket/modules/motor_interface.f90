module motor_interface
  !! Composite physics abstraction encapsulating a chamber and numerical algorithm parameters
  use chamber_interface, only : chamber_t
  use numerics_interface, only : numerics_t
  implicit none
  private

  type, public :: motor_t
    private
    type(numerics_t) numerics_
    type(chamber_t) chamber_
  contains
    procedure :: define, t_max, chamber, dt, d_dt, derived_variables
  end type

  interface

    module subroutine define(this, input_file)
      !! define each motor_t component
      implicit none
      class(motor_t), intent(out) :: this
      character(len=*), intent(in) :: input_file
    end subroutine

    pure module function derived_variables(this, states)
      !! result contains tabulated pressure, mass flux, and thrust versus time
      use kind_parameters, only : rkind
      use state_interface, only : state_t
      implicit none
      class(motor_t), intent(in) :: this
      type(state_t), intent(in) :: states(:)
      real(rkind), allocatable :: derived_variables(:,:)
    end function

    pure module function t_max(this)
      !! result is the desired simulation end time
      use kind_parameters, only : rkind
      implicit none
      class(motor_t), intent(in) :: this
      real(rkind) t_max
    end function

    pure module function chamber(this)
      !! result is the chamber_t component of this motor
      implicit none
      class(motor_t), intent(in) :: this
      type(chamber_t) chamber
    end function

    pure module function dt(this)
      !! result is the simulation time step
      use kind_parameters, only : rkind
      implicit none
      class(motor_t), intent(in) :: this
      real(rkind) dt
    end function

    pure module function d_dt(this, state) result(dState_dt)
      !! result contains the numerically evaluated time derivative of each state component
      use state_interface, only : state_t
      use state_rate_interface, only : state_rate_t
      implicit none
      class(motor_t), intent(in) :: this
      type(state_t), intent(in) :: state
      type(state_rate_t) dState_dt
    end function

  end interface

end module motor_interface
