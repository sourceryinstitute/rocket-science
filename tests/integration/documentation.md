project: VolFil
summary: A solid rocket motor simulation program.
src_dir: original
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
lower: true
print_creation_date: true
creation_date: %Y-%m-%d %H:%M %z
project_github: https://github.com/rouson/volfil
project_download: https://github.com/rouson/volfil/
email: Brian.Laubacher@autoliv.com
author_description: Autoliv is the world's largest automotive safety supplier, with sales to all major car manufacturers in the world.
website: https://autoliv.com

Developer Documentation
=======================

Welcome to the VolFil program developer documentation.  This documentation describes the
original VolFil application written in a procedural programming style with a global
common to many Fortran 77/90 programs. The program serves as
1. A starting point for a guided evolution to modernity.
2. A reference implementation for a test oracle used to verify the resulting modern design.

[FORD]: https://github.com/Fortran-FOSS-Programmers/ford#readme

[_____ Comments _______]:#
[source: display source code corresponding to item being documented]:#
[graph: generate call graphs, module dependency graphs, derive type composition/inheritance graphs ]:#
[sort: different sorting schemes for the modules or procedures or programs or derived types (alpha = alphabetical see wiki).]:#
[extra_mods: documentation for intrinsic modules]:#

[This document is a FORD project file, formatted with Pythonic Markdown                                      ]:#
[See https://github.com/Fortran-FOSS-programmers/ford/wiki/Project-File-Options for more info on writing FORD project files]:#
