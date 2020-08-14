program main
  !! Reference, procedural rocket motor simulation
  implicit none

  interface
    function legacy_rocket() result(capture_output)
      use mod1, only : dp
      implicit none
      real(dp), allocatable :: capture_output(:,:)
    end function

  end interface

   associate(reference_rocket_output => legacy_rocket())

     block
       integer i

       do i=1,size(reference_rocket_output,1)
         write(*,*) reference_rocket_output(i,:)
       end do
     end block

   end associate
  
   print *," Test passed."

end program main
