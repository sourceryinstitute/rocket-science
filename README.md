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
* A legacy motor simulator demonstrating the solution of the same governing
  equations using a procedural programming style in Fortran 77/90.
* A refurbished motor simulator demonstrating one modernization path for
  making the legacy solver object-oriented but without the radical redesign
  that affords the above purely functional programming style.

The simulator includes a capability for launching gnuplot to compare results
of the three solvers.

Prerequisites
-------------
### Compiler

This code was developed with the following compiler and parallel runtime
library:

* [GNU Compiler Collection] Fortran compiler (gfortran 10.2.0)
* [OpenCoarrays] parallel runtime library (caf 2.9.0).

Earlier versions might work also.

The current version of rocket-science does not yet explicitly rely upon
OpenCoarrays, but building with OpenCoarrays would facilitate parallelization
for parametric studies.

### Supported operating systems

The following are the systems on which this `rocket-science` is expected
to build:

* Linux (Sourcery Institute [Fortran 2018 Development Enviroment]),
* macOS (macOS 10.15.6 Catalina), and
* Windows Subsystem for Linux (Windows 10)

Earlier versions of each might work as well.

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
If the run completed successfully, the modern, legacy and refurbished curves will be indistinguishable.

![Thrust history](https://user-images.githubusercontent.com/13108868/93721216-36439200-fb43-11ea-9ad2-d0797b043783.png)

[Fortran 2018 Development Enviroment]: http://www.sourceryinstitute.org
[GNU Compiler Collection]: https://gcc.gnu.org
[OpenCoarrays]: https://github.com/sourceryinstitute/opencoarrays
[gnuplot]: http://www.gnuplot.info

