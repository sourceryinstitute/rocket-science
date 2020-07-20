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
website: https://autoliv.com

Developer Documentation
=======================

Welcome to the VolFil program developer documentation.  This online documentation is
automatically generated from inline comments and static analysis using the [FORD] tool.

The current version of the mini-app takes the demonstration about as far as is feasible
without excess verbosity and uses only three Fortran 2003 features:

1. The intrinsic `iso_fortran_env` module.
2. The `associate` statement is used ubiquitously.
3. User-defined structure constructors are used in `generation_rate_module` and `flow_rate_module`.

Item 2 above facilitates the gentle introduction of an important aspect of functional programming:
immutable state.  A associate name associated with an expression or function reference captures
the expression evaluation result or function result but precludes later re-association of
the associate name with a new value on any given pass through the corresponding `associate`
block.

Item 3 above facilitates the introduction of an important pattern in most object-oriented programming (OOP)
languages: functions named after a given type produces are used to produce new objects of the given
type.  Fortran 90 had language-provided structure constructors that very few programmers used and that
could be used only with types that have only public components, which violates one of the core
philosophies of OOP: information hiding. Fortran 2003 added user-defined structure constructors that
can be used with a derived type has private components.

Next Steps
----------

A future version will progressively adopt features from Fortran 95, 2003, 2008, and 2018
to demonstrate a more fully object-oriented and purely functional programming style and
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


