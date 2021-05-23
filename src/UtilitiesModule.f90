module mUtilities

    implicit none

    private

    public :: upcase, downcase, capitalize

    character(len=26), parameter :: cap = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    character(len=26), parameter :: low = 'abcdefghijklmnopqrstuvwxyz'

    contains

	subroutine upcase(string)
		character(len=*), intent(inout) :: string
		integer i, ic
		do i = 1, len_trim(string)
		    ic = index(low, string(i:i))
		    if (ic > 0) string(i:i) = cap(ic:ic)
		end do
	end subroutine

	subroutine downcase(string)
		character(len=*), intent(inout) :: string
		integer i, ic
		do i = 1, len_trim(string)
		    ic = index(cap, string(i:i))
		    if (ic > 0) string(i:i) = low(ic:ic)
		end do
	end subroutine

	subroutine capitalize(string)
        character(len=*), intent(inout) :: string
        call upcase(string(1:1))
    end subroutine

end module mUtilities
