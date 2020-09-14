submodule(results_interface) results_implementation
  use assertions_interface, only : assert
  implicit none

contains

  module procedure write_formatted
    integer i

    call assert(iotype=='LISTDIRECTED', "results_t%write_formtted: iotype='LISTDIRECTED'")

    if (allocated(this%header))write(unit,'(*(G0,:,18x))') this%header, new_line('a')

    do i=1,size(this%body,1)
      write(unit,*) this%body(i,:), new_line('a')
    end do
  end procedure

  module procedure new_results_t
    new_results_t%header = header
    new_results_t%body = body
  end procedure

  module procedure states_t_array
    use array_functions_interface, only : operator(.catRows.)

    states_t_array%header = header
    allocate(states_t_array%body(0,size(header)))

    block
      integer i

      do i=1,size(states)
        states_t_array%body = states_t_array%body .catRows. states(i)%row_vector()
      end do
    end block

  end procedure

  module procedure norm
    norm_of_this = maxval(abs(this%body))
  end procedure

  module procedure subtract

    type(results_t) local_difference

    select type(rhs)
      class is (results_t)
        if (allocated(this%header) .and. allocated(rhs%header)) then
          local_difference%header = "difference of " // this%header // "-" // rhs%header
        end if
        local_difference%body = this%body - rhs%body
      class default
        error stop "results_t%difference: unsupported rhs class"
    end select
    difference = local_difference
  end procedure

  module procedure distance

    call assert(all([shape(this%body)==shape(rhs%body)]), "percent_difference: all([shape(this%body)==shape(rhs%body))")
    allocate(distance%body, mold=this%body)

    if (allocated(this%header) .and. allocated(rhs%header)) &
      distance%header = "dist(" // this%header // "," // rhs%header // ")"

    block
      integer row, col
      integer, parameter :: window=4, time=1

      associate(rows => size(distance%body,1), cols => size(distance%body,2))
        do concurrent(row=1:rows, col=1:cols)
          associate(first_row => max(1, row-window), last_row=>min(row+window, rows))
            distance%body(row,col) = minval(hypot( &
              this%body(first_row:last_row, time) - rhs%body(row, time), &
              this%body(first_row:last_row,  col) - rhs%body(row,  col) &
            ))
          end associate
        end do
      end associate

    end block

  end procedure

  module procedure max_filtered_normalized_distance

    integer, parameter :: mdotos_column=4, thrust_column=5
    real(rkind), allocatable :: rhs_filtered(:,:)
    type(results_t) distance

    distance = this%distance(rhs)

    rhs_filtered = rhs%body

    associate( &
     thrust_noise_threshold => 0.01*maxval(rhs%body(:,thrust_column)), &
     mdotos_noise_threshold => 0.01*maxval(rhs%body(:,mdotos_column)) &
    )
      where(rhs_filtered(:,thrust_column) < thrust_noise_threshold) rhs_filtered(:,thrust_column) = 0._rkind
      where(rhs_filtered(:,mdotos_column) < mdotos_noise_threshold) rhs_filtered(:,mdotos_column) = 0._rkind
    end associate

    where(rhs_filtered/=0._rkind)
      distance%body = distance%body/rhs_filtered
    elsewhere
      distance%body = 0.
    end where

    max_filtered_normalized_distance = maxval(distance%body)

  end procedure

end submodule results_implementation
