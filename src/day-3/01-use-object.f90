module object_module
  implicit none

  private
  public :: object

  type object
  end type
end module

program main
  use object_module, only : object
  implicit none

  type(object) something ! component defined = .false.

  call print_object_components( something )

contains

   subroutine print_object_components(o)
     use assertions_interface, only : assert
     type(object) :: o

     call assert( o%user_defined(), "o%user_defined()")
        ! error terminates due to default initialization of the "defined" component to .false.

     ! if user_defined were public, the invocation "o%user_defined()" could also be "user_defined(o)"



     print *, object%user_defined()
   end subroutine
end program
