module mLine

    use mDataType

    use mUtilities

    implicit none

    public :: line_

    public :: new, delete

    character(len=1), parameter :: defaultSep = " "

    type line_
        private
        character(len=:), allocatable :: text
        integer(kind=itype)           :: length,&
                                         posi     !Position on string
    contains
        procedure :: getNextWordDefaultSeparator_line_
        procedure :: getNextWordBySeparator_line_
        generic   :: getNextWord       => getNextWordDefaultSeparator_line_,&
        !                                    getNextWordBySeparator_line_
        !procedure :: isEqualToString_line_
        !procedure :: isEqualToWord_line_
        !generic   :: isEqual           => isEqualToString_line_,&
        !                                    isEqualToWord_line_
        !procedure :: upcase            => upcase_line_
        !procedure :: downcase          => downcase_line_
        !procedure :: count             => count_line_
        !procedure :: writeLineOnScreen_line_
        !procedure :: writeLineOnUnit_line_
        !generic   :: write             => writeLineOnScreen_line_,&
        !                                    writeLineOnUnit_line_
        !procedure :: getErrorStatus    => getErrorStatus_line_
        !procedure :: getErrorMessage   => getErrorMessage_line_
    end type

    interface new
        module procedure :: constructor_mLine_
    end interface

    interface delete
        module procedure :: destructor_mLine_
    end interface

    contains

    !Type bound procedures
    function getNextWordDefaultSeparator_line_(this) result(word)
        class(line_), intent(in) :: this
        class(word_)             :: word
    end function
    
    function getNextWordBySeparator_line_(this, sep) result(word)
        class(line_), intent(in) :: this
        class(word_)             :: word
        character(*), intent(in) :: sep
    end function

    !Module procedures
    subroutine constructor_mLine_(line, text)
        class(line_), allocatable, intent(out) :: line
        character(len=*), intent(in)           :: text
        if (allocated(line)) deallocate(line)
        allocate(line)
        line%length = len(trim(adjustl(text)))
        if (allocated(line%text)) deallocate(line%text)
        allocate(character(line%length) :: line%text)
        line%text = trim(adjustl(text))
        line%posi = 0
    end subroutine

    subroutine destructor_mLine_(line)
        class(line_), allocatable, intent(inout) :: line
        if (allocated(line%text)) deallocate(line%text)
        line%length = 0
        line%posi = 0
        if (allocated(line)) deallocate(line)
        return
    end subroutine

end module
