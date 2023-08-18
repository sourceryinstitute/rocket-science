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
We developed this code using the following package versions

* [`fpm`] 0.9.0 package manager and build system, 
* [`gfortran`] 11.2.0 compiler, 
* [`gnuplot`] 5.4, patchlevel 2 plotting package, and
* [Vegetables] 7.2.1 unit testing framework.

Other versions might work also.

Using Rocket-Science
--------------------

To download, build, and run the simplest version of the mini-app (which uses explicit euler time advancement), 
and to plot the results with `gnuplot` installed on macOS, Linux, or Windows Subsystem for Linux, execute the
following at a `bash` or `zsh` command prompt:
```bash
git clone https://github.com/sourceryinstitute/rocket-science
cd rocket-science
fpm run explicit-euler -- "--graph"
```
which should produce plots much like the thrust history graph below.  
Press any button to bring up the next plot.  If the run completed successfully, 
the modern, legacy and refurbished curves will be indistinguishable.

To run a mini-app versions that use 2nd-order Runge Kutta and Runge-Kutta-Fehlberg
time advancement, respectively, and to graph the results, execute
```bash
fpm run runge-kutta-2nd-order -- "--graph"
fpm run runge-kutta-fehlberg -- "--graph"
```

Test Fire 
---------
To run the rocket-science test suite, execute
```
fpm test
```
At the time of this writing, the test suite contains two integration tests:
one each comparing the modern and refurbished simulation results to those
of the legacy code.

![Thrust history](https://user-images.githubusercontent.com/13108868/93721216-36439200-fb43-11ea-9ad2-d0797b043783.png)

[`fpm`]: https://github.com/fortran-lang/fpm
[`gfortran`]: https://gcc.gnu.org
[`gnuplot`]: http://www.gnuplot.info
[Vegetables]: https://gitlab.com/everythingfunctional/vegetables
