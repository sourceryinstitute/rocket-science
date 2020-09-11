module object_interface
  !! Define an abstract object type and type-bound procedure interface bodies to
  !! 1. Prevent declarations of this type (becuase it's not useful on its own).
  !! 2. Facilitate deferring the definition of some bound procedure to the
  !!    non-abstract child types and providing a specification for the abstract
  !!    interface that each non-abstract child's procedure bindings must support.
  implicit none

  private  !! default everything to private (information hiding)
  public :: object  !! explicitly epxort only the type and  its
                    !! public components and public type-bound procedures

  type, abstract :: object
    !! Encapsulate components and type-bound procedures that are
    !! broadly (universally) useful across the entire project
    private
    logical :: defined=.false. !! default initialization
  contains
    procedure :: user_defined    ! get defined component
    procedure :: mark_as_defined ! set defined component
    procedure(write_interface), deferred :: write_formatted
    generic :: write(formatted) => write_formatted
  end type

  ! For a on-abstract type, a similar functionality exists through non-type-bound
  ! generic interfaces but the interface can't be inherited.
  !
  ! interface write_formatted
  !   module procedure write_
  ! end interface

  abstract interface

    subroutine write_interface(me, unit, edit_descriptor, v_list, iostat, iomsg)
      !! Specification for the interface that child types must support
      import object
      class(object), intent(in) :: me
      integer, intent(in) :: unit
      character(len=*), intent(in) :: edit_descriptor
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(len=*), intent(inout) :: iomsg
    end subroutine

  end interface

  interface

    pure module function user_defined(me) result(me_defined)
      !! Report whether me has been defined

      class(object), intent(in) :: me
        !! Dynamic polymorphism: "class" facilitates passing in an argument
        !! of type "object" or any type that extends the object type or
        !! extends a type that extends the object type, ...
      logical me_defined

    end function

    module subroutine mark_as_defined(me)
      !! Mark the object as user-defined
      class(object), intent(inout) :: me
    end subroutine

  end interface

end module
