module mDataType

    implicit none

    !Integer types definition.
    integer, parameter :: itype = 4                    !General integer kind.
    integer, parameter :: i2b = selected_int_kind(3)   !2 bytes integer.
    integer, parameter :: i4b = selected_int_kind(8)   !4 bytes integer.
    integer, parameter :: i8b = selected_int_kind(10)  !8 bytes integer.

    !Real types definition.
    integer, parameter :: rtype = 8                    !General real kind.
    integer, parameter :: rsp = kind(1.0)              !4 bytes real. Single precision real.
    integer, parameter :: rdp = kind(1.0d0)            !8 bytes real. Double precision real.
    integer, parameter :: rspc = kind((1.0, 1.0))      !4 bytes complex real. Single precision complex real.
    integer, parameter :: rdpc = kind((1.0d0, 1.0d0))  !8 bytes complex real. Double precisio complex real.

    !Logical type definition
    integer, parameter :: lgtype = kind(.true.)        !logical type definition.

    !Character type definition
    integer, parameter   :: chword=30                 !character length for words.
    integer, parameter   :: chline =100                !character length for a phrasse
    integer, parameter   :: chtext=3000               !character length for a whole text

    contains

    !Print all the information at this moment for the data_type_manager module
    subroutine print_data_type_information()
        write(*,*) "Printing integer type definitions."
        write(*,*) "itype      = ", itype
        write(*,*) "i2b        = ", i2b
        write(*,*) "i4b        = ", i4b
        write(*,*) "i8b        = ", i8b
        write(*,*) "Printing real type definitions."
        write(*,*) "rtype      = ", rtype
        write(*,*) "rsp        = ", rsp
        write(*,*) "rdp        = ", rdp
        write(*,*) "rspc       = ", rspc
        write(*,*) "rdpc       = ", rdpc
        write(*,*) "Printing logical type definitions."
        write(*,*) "lgtype     = ", lgtype
        write(*,*) "Printing character type definitions."
        write(*,*) "chword     = ", chword
        write(*,*) "chline     = ", chline
        write(*,*) "chtext     = ", chtext

        return
    end subroutine

end module mDataType
