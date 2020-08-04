Commands Used Today
===================

Objective: to clarify data dependencies (informatio flow) by adding
arguments to subroutines.

* 01-calcthrust.f90
  - add arguments p, area, cf, and thrust
* 02-main.f90
  - demontrates a unit test
  - calls subroutine with arguments displayed
  - demonstrates runtime assertion-checking
* 03-oops.f90
  - demonstrate the lack of compile-time argument checking for external subprograms
    because they have implicit interfaces
* 04-thrust_module.f90
  - give the calcthrust subroutine an explicit interface (.mod file) by putting it in a module
    and thereby enabling compile-time checking of argument type
* 05-caught-ya.f90
  - show that the compiler now catches the error of passing a complex actual argument
    to a subroutine in the place of a real dummy argument
* 06-mod1.f90
  - copy of the rocket module in which all variables are declared
* 07-rocket.f90
  - main program src/rocket/original converted into a function to provide reference
    results for checking the refactored program src/rocket/refurbished/refurbished_rocket.f90
* 08-integration-test.f90
  - initial test of the refactored program
  - demonstrates another for of explicit interface: interface bodies
09-refurbished-rocket.f90
  - renames procedures so they can be linked into the same executable as their original counterparts
10-assertion_utility.f90
  - moved the assertion utiltiy into a module
11-assertion_interface.f90
  - demonstrate the separation of the "assert" procedure interface from the procedure definition
  - this also prevents unnecessary compilation cascades
12-assertion_implementation.f90
  - define the "assert" procedur


Git
---

* git commit
* ...

