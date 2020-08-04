module assertion_utility
  !! Calculate thrust
  implicit none ! Force all entities to be declared

  logical, parameter :: assertions=.true.

contains
  pure subroutine assert(assertion, description)
    !! assertion utility
    logical, intent(in) :: assertion
    character(len=*), intent(in) :: description
    if (assertions) then
      if (.not. assertion) error stop "Assertion ''"// trim(adjustl(description)) // "'' failed."
    end if
  end subroutine
end module
