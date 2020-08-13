project: Rocket
summary: A mini-app demonstrating the emulation of a functional, object-oriented style for
         calculating aggregate tablet burn in inflator chambers using (mostly) Fortran 90.
src_dir: src/rocket
exclude_dir: src/utilities
exclude_dir: src/original
output_dir: doc
display: public
         protected
         private
source: true
graph: true
md_extensions: markdown.extensions.toc
coloured_edges: true
sort: permission-alpha
extra_mods: iso_fortran_env:https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fFORTRAN_005fENV.html
            iso_c_binding:https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fC_005fBINDING.html#ISO_005fC_005fBINDING
lower: true
print_creation_date: true
creation_date: %Y-%m-%d %H:%M %z
project_github: https://github.com/rocket-science/
project_download: https://github.com/souceryinstitute/rocket-science/releases
email: Brian.Laubacher@autoliv.com
author_description: Autoliv is the world's largest automotive safety supplier, with sales to all major car manufacturers in the world.
website: https://autoliv.com

Developer Documentation
=======================

Welcome to the Rocket motor program developer documentation.  This online documentation is
automatically generated from inline comments and static analysis using the [FORD] tool.
The current version of the mini-app demonstrates a Fortran 2018 solid rocket motor
simulation using an object-oriented, functional programming style.  As such, all date is
encapsulated in extensible derived types and all procedures that do not have side effects
are pure functions.

[FORD]: https://github.com/Fortran-FOSS-Programmers/ford#readme

[_____ Comments _______]:#
[source: display source code corresponding to item being documented]:#
[graph: generate call graphs, module dependency graphs, derive type composition/inheritance graphs ]:#
[sort: different sorting schemes for the modules or procedures or programs or derived types (alpha = alphabetical see wiki).]:#
[extra_mods: documentation for intrinsic modules]:#

[This document is a FORD project file, formatted with Pythonic Markdown                                      ]:#
[See https://github.com/Fortran-FOSS-programmers/ford/wiki/Project-File-Options for more info on writing FORD project files]:#


