module assertion_interface
  !! Calculate thrust
  implicit none ! Force all entities to be declared

  logical, parameter :: assertions=.true.

  interface

    pure module subroutine assert(assertion, description)
      !! assertion utility
      implicit none
      logical, intent(in) :: assertion
      character(len=*), intent(in) :: description
    end subroutine

  end interface

end module
