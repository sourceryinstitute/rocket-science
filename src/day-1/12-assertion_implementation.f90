submodule(assertion_interface) assertion_implementation
  !! Calculate thrust
  implicit none ! Force all entities to be declared

contains

   module procedure assert
    if (assertions) then
      if (.not. assertion) error stop "Assertion ''"// trim(adjustl(description)) // "'' failed."
    end if
  end procedure

end submodule
