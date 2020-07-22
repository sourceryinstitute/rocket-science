module generation_rate_module
  !! Encapsulate mass and energy generation rates to facilitate functional programming:
  !! because the energy generation rate depends on the mass generation rate, calculating
  !! both in one function avoids redundancy. Functions can have only one result so a
  !! generation_rate_t object captures both results in one object.
  use kind_parameters, only : DP
  implicit none

  private
  public :: generation_rate_t
  public :: burn_rate
  public :: e_dot_gen
  public :: m_dot_gen

  type generation_rate_t
    private
    real(DP) burn_rate
    real(DP) e_dot_gen
    real(DP) m_dot_gen
  end type

  interface generation_rate_t
    !! This is a generic interface.  Generic interfaces provide an alias ("generation_rate_t")
    !! to refer to one or more procedures.  Fortran 2003 allows generic interface names to have
    !! the same name as the type.  A common pattern across object-oriented languages is to have
    !! a collection of procedures that can be invoked using the name of the type of object that
    !! they construct.  Such procedures are referred to as constructors. This is analogous to
    !! the generic interface "define" used elsewhere in volfil.  The "define" generic interfaces
    !! all refer to subroutines. In the case here, define_generation_rate is a function, which
    !! is the more common choice for constructors in Fortran.
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
    !! Because Akin's book uses Fortran 90/95, he didn't have the ability to define generic interfaces
    !! with names matching the corresponding type names so he added an underscore to the name of his
    !! constructors.  So the first executable line in the above program would instead be
    !!
    !! bar = foo_(i=1)
    !!
    !! in the style of Akins.
    module procedure define_generation_rate
      ! This can be a comma-separated list of procedures as long as each has
      ! different argument lists.
  end interface

contains

  function define_generation_rate(burn_rate, mass_generation_rate, enthalpy_generation_rate) result(rate)
    real(DP), intent(in) :: burn_rate, mass_generation_rate, enthalpy_generation_rate
    type(generation_rate_t) rate
    rate%burn_rate = burn_rate
    rate%m_dot_gen = mass_generation_rate
    rate%e_dot_gen = enthalpy_generation_rate
  end function

  function burn_rate(this) result(rate)
    type(generation_rate_t), intent(in) :: this
    real(DP) rate
    rate = this%burn_rate
  end function

  function m_dot_gen(this) result(m_dot)
    type(generation_rate_t), intent(in) :: this
    real(DP) m_dot
    m_dot = this%m_dot_gen
  end function

  function e_dot_gen(this) result(e_dot)
    type(generation_rate_t), intent(in) :: this
    real(DP) e_dot
    e_dot = this%e_dot_gen
  end function

end module generation_rate_module
