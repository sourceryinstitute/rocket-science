submodule (hole_interface) hole_implementation
    implicit none

    real, parameter :: pi=3.141592654
contains
    module procedure set_diameter
        this%radius = diameter/2.0
    end procedure

    module procedure area
        area = this%radius**2 * pi
    end procedure
end submodule
