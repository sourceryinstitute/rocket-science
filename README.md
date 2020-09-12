Rocket Science
==============

```
__________               __           __          ^
\______   \ ____   ____ |  | __ _____/  |_       / \
 |       _//  _ \_/ ___\|  |/ // __ \   __\      | |
 |    |   (  <_> )  \___|    <\  ___/|  |       /| |\
 |____|_  /\____/ \___  >__|_ \\___  >__|      /_| |_\
        \/            \/     \/    \/            vvv
  _________       __                             ***
 /   _____/ ____ |__| ____   ____   ____  ____    *
 \_____  \_/ ___\|  |/ __ \ /    \ / ___\/ __ \   .
 /        \  \_  |  |  ___/|   |  \  \__|   __/
/_______  /\___/ |__|\____ |___|  /\___/ \____
```

This repository contains

* A modern mini-application for simulating solid rocket motors using an
  object-oriented, functional programming style in Fortran 2018.
* A legacy kernel function for solving the same governing equations
  using a procedural programming style in Fortran 77/90.

The simulator includes a capability for launching gnuplot to
compare results between the two solvers.

Prerequisites
-------------
### Compiler

* [GNU Compiler Collection] Fortran compiler (gfortran 11.0.0)
* [OpenCoarrays] parallel runtime library (caf 2.9.0) and

Version 1.0.0 does not yet explicitly rely upon OpenCoarrays,
but it's expected that future versions will.

### Supported operating systems

The following are the systems on which this `rocket-science` is expected
to build with the tested version information in parentheses:

* Linux (Sourcery Institute [Fortran 2018 Development Enviroment]),
* macOS (macOS 10.15.6 Catalina), and
* Windows Subsystem for Linux

### Optional
* [gnuplot] for graphing results.

Download, Build, and Graph
--------------------------
```bash
git clone https://github.com/sourceryinstitute/rocket-science
mkdir -p rocket-science/build
cd rocket-science/build
cmake ..
make
ctest
```
and then graph the results with the following commands
```bash
cd tests/integration/rocket
 ../../../src/rocket-science --graph
```
which should produce plots much like the thrust history below.  Press any button to bring up the next plot.  
If the run completed successfully, the modern and legacy curves will be indistinguishable.

![Thrust history](https://user-images.githubusercontent.com/13108868/92982607-b6873a80-f453-11ea-950f-294275381502.png)


[Fortran 2018 Development Enviroment]: http://www.sourceryinstitute.org
[GNU Compiler Collection]: https://gcc.gnu.org
[OpenCoarrays]: https://github.com/sourceryinstitute/opencoarrays
[gnuplot]: http://www.gnuplot.info

