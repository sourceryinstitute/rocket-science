module hole_interface
    implicit none
    private

    type, public :: hole_t
        private
        real :: radius
    contains
        procedure :: set_diameter
        procedure :: area
    end type


    interface
        module subroutine set_diameter(this, diameter)
            class(hole_t), intent(inout) :: this
            real, intent(in) :: diameter
        end subroutine

        pure module function area(this)
            class(hole_t), intent(in) :: this
            real :: area
        end function
    end interface
end module
