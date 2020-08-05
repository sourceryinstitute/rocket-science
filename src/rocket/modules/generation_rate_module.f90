module generation_rate_module
  !! Encapsulate mass and energy generation rates to facilitate functional programming:
  !! because the energy generation rate depends on the mass generation rate, calculating
  !! both in one function avoids redundancy. Functions can have only one result so a
  !! generation_rate_t object captures both results in one object.
  use kind_parameters, only : rkind
  implicit none

  private
  public :: generation_rate_t

  type generation_rate_t
    private
    real(rkind) burn_rate_
    real(rkind) e_dot_gen_
    real(rkind) m_dot_gen_
  contains
    procedure :: burn_rate
    procedure :: e_dot_gen
    procedure :: m_dot_gen
  end type

  interface generation_rate_t
    !! This is a generic interface.  Generic interfaces provide an alias ("generation_rate_t")
    !! to refer to one or more procedures.  Fortran 2003 allows generic interface names to have
    !! the same name as the type.  A common pattern across object-oriented languages is to have
    !! a collection of procedures that can be invoked using the name of the type of object that
    !! they construct.  Such procedures are referred to as constructors.
    !!
    !! It might be useful to know that Fortran 90 also provided default "structure constructors"
    !! that the user didn't need to write because the language provided them.  The following code
    !! is standard-conforming Fortran 90:
    !!
    !! module foo_module
    !!   implicit none
    !!
    !!   type foo
    !!     integer i
    !!   end type
    !!
    !! end module
    !!
    !! program main
    !!   use foo_module, only : foo
    !!   implicit none
    !!
    !!   type(foo) bar
    !!
    !!   bar = foo(i=1)
    !!   print *,bar%i
    !! end program
    !!
    !! where the function foo() is provided by the language itself and gives a function result that
    !! is a foo object with its i component set to 1.  The above program should print "1".
    !! The language-provided structure constructor (the function "foo" above), however, proves
    !! unuseful in object-oriented programming (OOP) because it only works if the
    !! components are public, which violates the information-hiding principle of OOP. A solution
    !! to this dilemma is to let users write their own structure constructors.  Fortran 2003
    !! that by allowing generic interfaces to have a name matching the type name.
    !!
    !! Try saving the above program as main.f90 and compiling with
    !!
    !!   gfortran -std=f95 main.f90
    !!   ./a.out
    !!
    module procedure construct_generation_rate_t
      ! This can be a comma-separated list of procedures as long as each has
      ! different argument lists.
  end interface

contains

  pure function construct_generation_rate_t(burn_rate, mass_generation_rate, enthalpy_generation_rate) result(new_generation_rate_t)
    real(rkind), intent(in) :: burn_rate, mass_generation_rate, enthalpy_generation_rate
    type(generation_rate_t) new_generation_rate_t
    new_generation_rate_t%burn_rate_= burn_rate
    new_generation_rate_t%m_dot_gen_= mass_generation_rate
    new_generation_rate_t%e_dot_gen_= enthalpy_generation_rate
  end function

  pure function burn_rate(this) result(rate)
    class(generation_rate_t), intent(in) :: this
    real(rkind) rate
    rate = this%burn_rate_
  end function

  pure function m_dot_gen(this) result(m_dot)
    class(generation_rate_t), intent(in) :: this
    real(rkind) m_dot
    m_dot = this%m_dot_gen_
  end function

  pure function e_dot_gen(this) result(e_dot)
    class(generation_rate_t), intent(in) :: this
    real(rkind) e_dot
    e_dot = this%e_dot_gen_
  end function

end module generation_rate_module
