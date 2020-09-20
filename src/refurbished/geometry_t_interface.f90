module geometry_t_interface
  !! Model the propellent grain as a cylinder burning radially outward from its
  !! evacuated axial core and axially inward towards its cross-sectional mid-plane.
  !! The outer diameter is inhibited by the surroundin casing as the propellent was
  !! poured into the tube/chamber so that only the inner diameter burns when ignited.
  use constants, only : dp
  implicit none

  private

  type, public :: geometry_t
    !! Encapsulate the grain geometry and burnout condition
    private
    real(dp) vol_     !! volume
    real(dp) id_      !! inner diameter
    real(dp) od_      !! outer diameter
    real(dp) length_  !! axial length
  contains
    procedure :: surf
    procedure :: vol
    procedure :: burnout
  end type

  interface geometry_t
    module procedure new_geometry_t, incremented_geometry_t
  end interface

  interface

    pure module function new_geometry_t(vol, id, od, length)
      !! Result is a new geometry_t object defined by the actual arguments
      implicit none
      real(dp), intent(in) :: vol    !! volume
      real(dp), intent(in) :: id     !! inner diameter
      real(dp), intent(in) :: od     !! outer diameter
      real(dp), intent(in) :: length !! axial length
      type(geometry_t) :: new_geometry_t
    end function

    pure module function incremented_geometry_t(old_geometry_t, volume_increment)
      !! Result is an updated cylinder after one time step of burning from the inner
      !! diameter outward and from both ends inward along the axial length.
      implicit none
      type(geometry_t), intent(in) :: old_geometry_t
      real(dp), intent(in) :: volume_increment
      type(geometry_t) incremented_geometry_t
    end function

    pure module function surf(this, burn_depth)
      !! Result is the annular grain surface area
      implicit none
      class(geometry_t), intent(in) :: this
      real(dp), intent(in) :: burn_depth !! surface-normal distance burned
      real(dp) surf
    end function

    pure module function vol(this)
      !! Result is the annular grain volume
      implicit none
      class(geometry_t), intent(in) :: this
      real(dp) vol
    end function

    pure module function burnout(this, db)
      !! Result is .true. if all grain has been depleted and .false. otherwise
      implicit none
      class(geometry_t), intent(in) :: this
      real(dp), intent(in) :: db !! burn depth
      logical burnout
    end function

  end interface

end module geometry_t_interface
