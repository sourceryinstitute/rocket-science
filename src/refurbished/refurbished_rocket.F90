module refurbished_rocket_module
  !! A driver funtion for the object-oriented and purely functional refurbished_rocket library,
  !! which in turn resulted from an evolutionary refactoring of legacy_rocket.f90.
  implicit none

  private
  public :: refurbished_rocket

contains

  function refurbished_rocket(input_file)
    !! This driver function simulates a single stage rocket motor flowing out of a nozzle,
    !! assuming a thrust coefficient and ignoring the complexities of what happens to thrust
    !! low pressures, i.e. shock in the nozzle.  The result is a results_t object that
    !! encapsulates the time history of values that can be printed via user-defined
    !! derived-type output.

    use results_interface, only : results_t
    use burn_state_t_interface, only : burn_state_t
    use geometry_t_interface, only : geometry_t
    use generation_rate_t_interface, only : generation_rate_t
    use flow_rate_t_interface, only : flow_rate_t
    use chamber_gas_t_interface, only : chamber_gas_t
    use nozzle_t_interface, only : nozzle_t
    use constants, only : dp, zero, one
    implicit none

    character(len=*), intent(in) :: input_file
    type(results_t) refurbished_rocket

    type(burn_state_t) burn_state
    type(geometry_t) geometry
    type(chamber_gas_t) chamber_gas
    type(nozzle_t) nozzle

    real(dp) dt_, t_max_
    real(dp) c_p_, MW_
    real(dp) temperature_, pressure_
    real(dp) T_flame_, r_ref_, n_
    real(dp) id_, od_, length_, rho_solid_
    real(dp) dia_, C_f_

    call read_input_and_define_objects(burn_state, geometry, chamber_gas, nozzle, dt_, t_max_)

    refurbished_rocket = compute_time_history(nsteps = nint(t_max_/dt_))

  contains

    subroutine read_input_and_define_objects(burn_state, geometry, chamber_gas, nozzle, dt_, t_max_)
      use assertions_interface, only : assert, max_errmsg_len
      type(burn_state_t), intent(out) :: burn_state
      type(geometry_t), intent(out) :: geometry
      type(chamber_gas_t), intent(out) :: chamber_gas
      type(nozzle_t), intent(out) :: nozzle
      real(dp), intent(out) :: dt_, t_max_

      real(dp), parameter :: initial_volume = one
      integer, parameter :: success = 0
      integer io_status, file_unit
      character(len=max_errmsg_len) error_message

      namelist/numerics_list/ dt_, t_max_
      namelist/gas_list/ c_p_, MW_
      namelist/state_list/  temperature_, pressure_
      namelist/combustion_list/ T_flame_, r_ref_, n_
      namelist/grain_list/ id_, od_, length_, rho_solid_
      namelist/nozzle_list/ dia_, C_f_

      open(newunit=file_unit, file=input_file, status="old", iostat=io_status, iomsg=error_message)
      call assert(io_status == success, "refurbished_rocket: io_status == success", error_message)

      read(file_unit, nml=numerics_list) ! must read in order defined in the file (this requirement might be compiler-specific)
      read(file_unit, nml=gas_list)
      read(file_unit, nml=state_list)
      chamber_gas = chamber_gas_t(MW = MW_, c_p = c_p_, T = temperature_, p = pressure_, V = initial_volume)

      read(file_unit, nml=combustion_list)
      burn_state = burn_state_t(reference_burn_rate = r_ref_, pressure = pressure_, exponent_ = n_)

      read(file_unit, nml=grain_list)
      geometry = geometry_t(vol = initial_volume, id = id_, od = od_, length = length_)

      read(file_unit, nml=nozzle_list)
      nozzle = nozzle_t(dia=dia_, C_f=C_f_)


      close(file_unit)

    end subroutine read_input_and_define_objects

    function compute_time_history(nsteps)

      integer, intent(in) :: nsteps
      type(results_t) compute_time_history

      integer i
      real(dp), allocatable :: output(:,:)
      real(dp) time

      allocate(output(0:nsteps,6)) ! preallocate an output array

      time = zero
      associate(mdotos => zero, thrust => zero, volume => geometry%vol())
        output(0,:)=[time, chamber_gas%p(volume), chamber_gas%T(), mdotos, thrust, volume]
      end associate

      associate(R_gas => chamber_gas%R_gas(), c_v => chamber_gas%c_v(), g => chamber_gas%g(), c_p => chamber_gas%c_p())

        do i=1,nsteps

          associate(p => chamber_gas%p(geometry%vol()))

            burn_state = burn_state_t(burn_state, r_ref_, p, n_, dt_)

            associate(db => burn_state%db(), r => burn_state%r())
              associate(surf => geometry%surf(db))

                geometry = geometry_t(geometry,  volume_increment = merge(zero, r*surf*dt_, geometry%burnout(db)) )

                associate( &
                  flow_rate => flow_rate_t(chamber_gas%T(), g, R_gas, p, c_p, nozzle%area()), &
                  generation_rate => generation_rate_t(rho_solid_, r, surf, c_p, T_flame_) &
                )
                  associate(mdotos => flow_rate%mdotos())
                    chamber_gas = chamber_gas_t( chamber_gas,  &
                      mass_increment   = (generation_rate%mdotgen() - mdotos)*dt_, &
                      energy_increment = (generation_rate%edotgen() - flow_rate%edotos())*dt_  &
                    )
                    associate(volume => geometry%vol())
                      associate(p => chamber_gas%p(volume))
                        time = time + dt_
                        output(i,:)=[time, p, chamber_gas%T(), mdotos, nozzle%thrust(p), volume]
                      end associate
                    end associate
                  end associate
                end associate
              end associate
            end associate
          end associate
        end do
      end associate

      block
        character(len=*), parameter :: header(*) = [ character(len=len("temperatureRefurbished)")) :: &
          "timeRefurbished", "pressureRefurbished", "temperatureRefurbished", "mdotosRefurbished", "thrustRefurbished", &
          "volumeRefurbished"]
        compute_time_history = results_t(header, output)
      end block

    end function compute_time_history

  end function refurbished_rocket

end module refurbished_rocket_module
