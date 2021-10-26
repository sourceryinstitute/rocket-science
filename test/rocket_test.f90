module rocket_test
  !! Test rocket motor simulations
  use motor_interface, only : motor_t
  use state_interface, only : state_t
  use kind_parameters, only : rkind
  use vegetables, only: &
      result_t, test_item_t, describe, it, assert_that
  use results_interface, only : results_t
  use refurbished_rocket_module, only : refurbished_rocket
  implicit none
  private
  public :: test_rocket

  type(results_t), allocatable :: legacy_rocket_results
  character(len=*), parameter :: input_file = "test/rocket.inp"
  real(rkind), parameter :: tolerance = 0.01_rkind

contains

  function test_rocket() result(tests)
    type(test_item_t) :: tests

    tests = describe("rocket", &
      [ it("redesign tracks legacy rocket", redesign_tracks_legacy) &
      , it("refurbishment tracks legacy rocket", refurbishment_tracks_legacy) &
      ])
  end function

  function ancient_history() result(legacy_results)
    type(results_t) legacy_results

    interface
      function legacy_rocket(input_file)
        import results_t
        implicit none
        character(len=*), intent(in) :: input_file
        type(results_t) legacy_rocket
      end function
    end interface

    legacy_results = legacy_rocket(input_file) 
  end function

  function revisionist_history() result(redesigned_rocket_results)
    real(rkind), parameter :: zero=0._rkind
    type(results_t) redesigned_rocket_results

    type(motor_t) motor
    type(state_t) state !! state variables updated at each time step: mass, energy, time, and burn depth
    type(state_t), allocatable :: history(:)

    call motor%define(input_file)

    associate(chamber => motor%chamber())
      associate(gas => chamber%gas())
        call state%define(input_file, R_gas=gas%R_gas(), c_v=gas%c_v(), volume=chamber%initial_volume(), time=zero, burn_depth=zero)
      end associate
    end associate

    associate(dt => motor%dt() )
      history = [state]
      do while(state%time() < motor%t_max())
        state = state + motor%d_dt(state)*dt
        history = [history, state]
      end do
    end associate

    block 
      character(len=*), parameter :: header(*) = &
        [character(len=len("temperature")) :: "time", "pressure", "temperature", "mdotos", "thrust", "volume"]

      redesigned_rocket_results = results_t(header, motor%derived_variables(history))
    end block

  end function

  function redesign_tracks_legacy() result(result_)
    type(result_t) result_

    if (.not. allocated(legacy_rocket_results)) legacy_rocket_results = ancient_history()

    associate(redesigned_results => revisionist_history())
      result_ = assert_that( &
         redesigned_results%max_filtered_normalized_distance(legacy_rocket_results) < tolerance, &
        "redesigned_results%max_filtered_normalized_distance(legacy_rocket_results) < tolerance" &
      )
    end associate
  end function

  function refurbishment_tracks_legacy() result(result_)
    type(result_t) result_

    if (.not. allocated(legacy_rocket_results)) legacy_rocket_results = ancient_history()

    associate(refurbished_results => refurbished_rocket(input_file))
      result_ = assert_that( &
         refurbished_results%max_filtered_normalized_distance(legacy_rocket_results) < tolerance, &
        "refurbished_results%max_filtered_normalized_distance(legacy_rocket_results) < tolerance" &
      )
    end associate

  end function

end module
