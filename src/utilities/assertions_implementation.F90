!
!     (c) 2019-2020 Guide Star Engineering, LLC
!     This Software was developed for the US Nuclear Regulatory Commission (US NRC) under contract
!     "Multi-Dimensional Physics Implementation into Fuel Analysis under Steady-state and Transients (FAST)",
!     contract # NRC-HQ-60-17-C-0007
!
submodule(assertions_interface) assertions_implementation
  implicit none

contains

  module procedure assert

    character(len=:), allocatable :: message, preface
    integer, parameter :: max_appended_characters=1024

    if (assertions) then
      if (.not. assertion) then
        message = repeat(" ",ncopies=len(description)+max_appended_characters)
        write(message,*) '(',description,')'
        if (present(diagnostic_data)) then
          select type(diagnostic_data)
            type is(character(len=*))
              message = trim(adjustl(message)) // 'with diagnostic data' // diagnostic_data
            type is(integer)
              preface = trim(adjustl(message)) // 'with diagnostic data'
              write(message,*) preface, diagnostic_data
            class default
              message = trim(adjustl(message)) // 'with diagnostic data of unrecognized type'
          end select
        end if
        if (.not. present(success)) error stop "Assertion failed '" // message // "' failed."
      end if
    end if

  end procedure

end submodule assertions_implementation
