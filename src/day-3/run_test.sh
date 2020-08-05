#!/bin/bash

set -e

rm -f *.o *mod hole-test
gfortran -c 05-hole_interface.f90
gfortran -c 06-hole_implementation.f90
gfortran -c 04-hole-test.f90
gfortran *.o -o hole-test
./hole-test
