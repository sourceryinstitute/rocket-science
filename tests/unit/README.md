Test Steps
==========

*Objectives:*
1. Write a unit test to drive the development of a functional rocket simulator.
2. Write the code to satisfy the new unit test.

Steps
-----
1. Copy the results-class unit test directory to a new directory, e.g., "nozzle-class".
2. Edit tests/unit/CMakeLists.txt to add the new directory in the foreach statement.  This tells `cmake` look in that directory for further instructions.
3. Edit tests/<new_name>/CMakeLists.txt to replace "thrust" with the name of the function you're testing (e.g., `calc_p`).
4. Edit rocket_science/CMakeLists.txt to add your test directory name, which the script will also expect to be in the test program name in the form test-<name>.f90.

-- Ignore this --
5. Edit your new test so that it tests an as-yet-unwritten function with the same name as the prior subroutine, e.g., `calc_t()`:
  - Replace the legacy `mod1` with the soon-to-be refactored `module_variables`.
    -- Remove the `subroutine` call and invoke the new `pure function` of the same name as the legacy `subroutine` inside the `assert` call.
    -- _Extra credit_: add an `, only :` and specify what you are importing
    -- _Extra credit_: delete the `assert` internal `subroutine` and instead call the `assert` in the `assertion_interface` `module`.
