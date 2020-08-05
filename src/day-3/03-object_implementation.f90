submodule(object_interface) object_implementation
  !! Define the object type-bound procedures
  implicit none

contains

    module procedure user_defined
      me_defined = me%defined
    end procedure

    module procedure mark_as_defined
      me%defined = .true.
    end procedure

end submodule object_implementation
