module mWord

    use mDataType

    use mUtilities

    implicit none

    private

    public :: word_

    public :: new, delete, assignment(=), operator(==)

    type :: word_
        private
        character(len=:), allocatable :: text
        integer(itype)                :: length
    contains
        procedure, public :: getText    => getText_word_
        procedure, public :: getLength  => getLength_word_
        procedure, public :: upcase     => upcase_word_
        procedure, public :: downcase   => downcase_word_
        procedure, public :: capitalize => capitalize_word_
    end type

    interface new
        module procedure constructor_mWord
    end interface

    interface delete
        module procedure destructor_mWord
    end interface

    interface assignment(=)
        module procedure assign_mWord
    end interface

    interface operator(==)
        module procedure equal_mWord
    end interface

contains

! Type bound procedures
    function getText_word_(this) result(text)
        class(word_), intent(in) :: this
        character(this%length)   :: text
        text = this%text
    end function

    function getLength_word_(this) result(length)
        class(word_), intent(in) :: this
        integer(itype)           :: length
        length = this%length
    end function

    subroutine upcase_word_(this)
        class(word_), intent(inout) :: this
        if (.not. allocated(this%text)) return
        call upcase(this%text)
    end subroutine

    subroutine downcase_word_(this)
        class(word_), intent(inout) :: this
        if (.not. allocated(this%text)) return
        call downcase(this%text)
    end subroutine

    subroutine capitalize_word_(this)
        class(word_), intent(inout) :: this
        if (.not. allocated(this%text)) return
        call capitalize(this%text)
    end subroutine

! Module procedures
    subroutine constructor_mWord(word, text)
        class(word_), allocatable, intent(out) :: word
        character(*), intent(in)               :: text
        integer(itype)                         :: length,&
                                                  charc
        length = len(text)
        do charc=1, length
            if (text(charc:charc)==" ") return !Not a word if contains space.
        enddo
        if (allocated(word)) deallocate(word)
        allocate(word)
        word%length = len_trim(text)
        if (allocated(word%text)) deallocate(word%text)
        allocate(character(word%length) :: word%text)
        word%text = text
    end subroutine

    subroutine destructor_mWord(word)
        class(word_), allocatable, intent(inout) :: word
        if (.not. allocated(word)) return
        if (allocated(word%text)) deallocate(word%text)
        if (allocated(word)) deallocate(word)
    end subroutine

    subroutine assign_mWord(lhs, rhs)
        class(word_), allocatable, intent(out) :: lhs
        class(word_), allocatable, intent(in)  :: rhs
        if (.not. allocated(rhs)) return
        call new(lhs, rhs%text)
    end subroutine

    function equal_mWord(lhs, rhs) result(equal)
        logical(lgtype)                       :: equal
        class(word_), allocatable, intent(in) :: lhs,&
                                                 rhs
        equal = .false.
        if (.not. allocated(lhs)) return
        if (.not. allocated(rhs)) return
        if ((lhs%text == rhs%text) .and. (lhs%length == rhs%length)) equal = .true.
    end function

end module mWord
