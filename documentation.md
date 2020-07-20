project: VolFil
summary: A mini-app demonstrating the emulation of a functional, object-oriented style for
         calculating aggregate tablet burn in inflator chambers using (mostly) Fortran 90.
src_dir: src
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
project_github: https://github.com/blaubacher/volfil
project_download: https://github.com/blaubacher/volfil/releases
email: Brian.Laubacher@autoliv.com
author_description: Autoliv is the world's largest automotive safety supplier, with sales to all major car manufacturers in the world.
author_pic: ../graphics/brian.png
website: https://autoliv.com

Developer Documentation
=======================

Welcome to the VolFil program developer documentation.  This online documentation is
automatically generated from inline comments and static analysis using the [FORD] tool.

The current version of the mini-app takes the demonstration about as far as is feasible
without excess verbosity and uses only two Fortran 2003 features:

1. The intrinsic `iso_fortran_env` module.
2. The `associate` statement.

A future version will progressively adopt features from Fortran 95, 2003, 2008, and 2018
to demonstrate a more fully object-oriented and purely functional programmingn style and
all the various benefits the new features provide, including:
1. Robustness.
2. Extensibility.
3. Flexibility.
4. Performance optimizations.

[FORD]: https://github.com/Fortran-FOSS-Programmers/ford#readme

[_____ Comments _______]:#
[source: display source code corresponding to item being documented]:#
[graph: generate call graphs, module dependency graphs, derive type composition/inheritance graphs ]:#
[sort: different sorting schemes for the modules or procedures or programs or derived types (alpha = alphabetical see wiki).]:#
[extra_mods: documentation for intrinsic modules]:#

[This document is a FORD project file, formatted with Pythonic Markdown                                      ]:#
[See https://github.com/Fortran-FOSS-programmers/ford/wiki/Project-File-Options for more info on writing FORD project files]:#


