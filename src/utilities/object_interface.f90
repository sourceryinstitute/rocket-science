module object_interface
  !! Define the object type and type-bound procedure interface bodies
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
    procedure(write_formatted_interface), deferred :: write_formatted
    generic :: write(formatted) => write_formatted
  end type

  abstract interface

    subroutine write_formatted_interface(this, unit, iotype, vlist, iostat, iomsg)
      import object
      implicit none
      class(object), intent(in) :: this
      integer, intent(in) :: unit
      character(len=*), intent(in) :: iotype
      integer, intent(in) :: vlist(:)
      integer, intent(out) :: iostat
      character(len=*), intent(inout) :: iomsg
    end subroutine

  end interface

  interface

    pure module function user_defined(me) result(me_defined)
      !! Report whether me has been defined

      class(object), intent(in) :: me
        !! "passed-object dummy argument"
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
