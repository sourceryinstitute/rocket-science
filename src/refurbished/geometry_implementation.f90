submodule(geometry_interface) geometry_implementation
  !! Define the grain geometry user-defined structure constructors and geometrical formulas
  implicit none
contains

  module procedure new_geometry_t
    new_geometry_t%vol_    = vol
    new_geometry_t%id_     = id
    new_geometry_t%od_     = od
    new_geometry_t%length_ = length
  end procedure

  module procedure incremented_geometry_t
    incremented_geometry_t%id_     = old_geometry_t%id_
    incremented_geometry_t%od_     = old_geometry_t%od_
    incremented_geometry_t%length_ = old_geometry_t%length_
    incremented_geometry_t%vol_    = old_geometry_t%vol_ + volume_increment
  end procedure

  module procedure surf
    use constants, only : pi, zero

    associate(db=>(burn_depth))
      associate(id=>(this%id_), od=>(this%od_), length=>(this%length_))
        surf = merge(zero, pi*(id+2.0d0*db)*(length-2.0d0*db)+0.5d0*pi*(od**2.0d0-(id+2.0*db)**2.0d0), this%burnout(db))
      end associate
    end associate

  end procedure

  module procedure burnout
    associate(id=>(this%id_), od=>(this%od_), length=>(this%length_))
      burnout = id+2*db>od .or. db>length/2
    end associate
  end procedure

  module procedure vol
    vol = this%vol_
  end procedure

end submodule geometry_implementation
