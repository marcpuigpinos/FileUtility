module wordTests

use mWord

implicit none

private

public :: assertWordModule

contains

subroutine assertWordModule()
    logical success
    logical successTest

    successTest = .true.

    write(*,*) "mWord MODULE TESTING"
    write(*,*) "--------------------"
    write(*,*)
    call assert1(success)     !Constructor and destructor.
    if (.not. success) then   !If not success with constructor and destructor, no sense to continue.
        write(*,*) "Result mWord Test: FAILED"
        return
    endif
    write(*,*)
    call assert2(success)
    if (.not. success) successTest = success
    write(*,*)
    call assert3(success)
    if (.not. success) successTest = success
    write(*,*)
    if (.not. successTest) then
        write(*,*) "Result mWord Test: FAILED"
    else
        write(*,*) "Result mWord Test: PASSED"
    endif
end subroutine

!Test constructor and destructor
subroutine assert1(success)
    logical                   :: success
    class(word_), allocatable :: word
    write(*,*) "Assert 1: Test constructor and destructor."
    success = .false.
    call new(word, "shop")
    if (allocated(word)) then
        write(*,*) "Constructor ................................. ok"
        success = .true.
    else
        write(*,*) "Constructor ................................. NOK"
        success = .false.
    endif
    if (.not. success) return
    call delete(word)
    if (allocated(word)) then
        write(*,*) "Destructor .................................. NOK"
        success = .false.
    else
        write(*,*) "Destructor .................................. ok"
        success = .true.
    endif
    call new(word, " shop")
    if (allocated(word)) then
        write(*,*) "Construct text with spaces .................. NOK"
    else
        write(*,*) "Construct text with spaces .................. ok"
    endif
    call delete(word)
end subroutine

!Test operators and assignments
subroutine assert2(success)
    logical, intent(out)      :: success
    class(word_), allocatable :: word1, word2
    logical success1, success2, success3, success4, success5
    write(*,*) "Assert 2: Test operators and assignment"
    success = .false.
    call new(word1, "Text1")
    word2 = word1
    if ((word1%getText() == word2%getText()) .and. (word1%getLength() == word2%getLength())) then
        write(*,*) "Assignment test1 ............................ ok"
        success1 = .true.
    else
        write(*,*) "Assignment test1 ............................ NOK"
        success1 = .false.
    endif
    call delete(word2)
    call new(word2, "Text2")
    word2 = word1
    if ((word1%getText() == word2%getText()) .and. (word1%getLength() == word2%getLength())) then
        write(*,*) "Assignment test2 ............................ ok"
        success2 = .true.
    else
        write(*,*) "Assignment test2 ............................ NOK"
        success2 = .false.
    endif
    call delete(word2)
    call new(word2, "Text1")
    if (word1 == word2) then
        write(*,*) "Equal operator true ......................... ok"
        success3 = .true.
    else
        write(*,*) "Equal operator true.......................... NOK"
        success4 = .false.
    endif
    call delete(word2)
    call new(word2, "Text2")
    if (.not. word1 == word2) then
        write(*,*) "Equal operator false ........................ ok"
        success5 = .true.
    else
        write(*,*) "Equal operator false ........................ NOK"
        success5 = .false.
    endif
    success = success1 .and. success2 .and. success3 .and. success4 .and. success5
end subroutine

!Test Get procedures
subroutine assert3(success)
    logical, intent(out)      :: success
    class(word_), allocatable :: word
    logical success1, success2, success3, success4, success5
    write(*,*) "Assert 3: Test typebound procedures."
    call new(word, "Pseudopseudohypoparathyroidism")
    success1 = .false.
    if (word%getLength()==30) then
        write(*,*) "Get the length procedure .................... ok"
        success1 = .true.
    else
        write(*,*) "Get the length procedure .................... NOK"
        success1 = .false.
    endif
    success2 = .false.
    if (word%getText()=="Pseudopseudohypoparathyroidism") then
        write(*,*) "Get the text procedure ...................... ok"
        success2 = .true.
    else
        write(*,*) "Get the text procedure ...................... NOK"
        success2 = .false.
    endif
    call delete(word)
    call new(word,"text")
    success3 = .false.
    call word%upcase()
    if (word%getText() == "TEXT") then
        write(*,*) "Upcase procedure ............................ ok"
        success3 = .true.
    else
        write(*,*) "Upcase procedure ............................ NOK"
        success3 = .false.
    endif
    success4 = .false.
    call word%downcase()
    if (word%getText() == "text") then
        write(*,*) "Downcase procedure .......................... ok"
        success4 = .true.
    else
        write(*,*) "Downcase procedure .......................... NOK"
        success4 = .false.
    endif
    success5 = .false.
    call word%capitalize()
    if (word%getText() == "Text") then
        write(*,*) "Capitalize procedure ........................ ok"
        success5 = .true.
    else
        write(*,*) "Capitalize procedure ........................ NOK"
        success5 = .false.
    endif
    success = success1 .and. success2 .and. success3 .and. success4 &
              .and. success5
end subroutine

end module
