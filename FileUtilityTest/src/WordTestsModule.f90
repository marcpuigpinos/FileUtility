module wordTests

use mWord

implicit none

private

public :: assertWordModule

contains

subroutine assertWordModule(uf)
    integer, intent(in) :: uf
    logical success
    logical successTest

    successTest = .true.

    write(uf,*) "mWord MODULE TESTING"
    write(uf,*) "--------------------"
    write(uf,*)
    call assert1(uf, success)     !Constructor and destructor.
    if (.not. success) then   !If not success with constructor and destructor, no sense to continue.
        write(uf,*) "Result mWord Test: FAILED"
        return
    endif
    write(uf,*)
    call assert2(uf, success)
    if (.not. success) successTest = success
    write(uf,*)
    call assert3(uf, success)
    if (.not. success) successTest = success
    write(uf,*)
    if (.not. successTest) then
        write(uf,*) "Result mWord Test: FAILED"
    else
        write(uf,*) "Result mWord Test: PASSED"
    endif
end subroutine

!Test constructor and destructor
subroutine assert1(uf, success)
    integer, intent(in)       :: uf
    logical, intent(out)      :: success
    class(word_), allocatable :: word
    write(uf,*) "Assert 1: Test constructor and destructor."
    success = .false.
    call new(word, "shop")
    if (allocated(word)) then
        write(uf,*) "Constructor ................................. ok"
        success = .true.
    else
        write(uf,*) "Constructor ................................. NOK"
        success = .false.
    endif
    if (.not. success) return
    call delete(word)
    if (allocated(word)) then
        write(uf,*) "Destructor .................................. NOK"
        success = .false.
    else
        write(uf,*) "Destructor .................................. ok"
        success = .true.
    endif
    if (.not. success) return
    call new(word, " shop")
    if (allocated(word)) then
        write(uf,*) "Construct text with spaces .................. NOK"
        success = .false.
    else
        write(uf,*) "Construct text with spaces .................. ok"
        success = .true.
    endif
    call delete(word)
end subroutine

!Test operators and assignments
subroutine assert2(uf, success)
    integer, intent(in)       :: uf
    logical, intent(out)      :: success
    class(word_), allocatable :: word1, word2
    logical success1, success2, success3, success4
    write(uf,*) "Assert 2: Test operators and assignment"
    success = .false.
    call new(word1, "Text1")
    word2 = word1
    if ((word1%getText() == word2%getText()) .and. (word1%getLength() == word2%getLength())) then
        write(uf,*) "Assignment test1 ............................ ok"
        success1 = .true.
    else
        write(uf,*) "Assignment test1 ............................ NOK"
        success1 = .false.
    endif
    call delete(word2)
    call new(word2, "Text2")
    word2 = word1
    if ((word1%getText() == word2%getText()) .and. (word1%getLength() == word2%getLength())) then
        write(uf,*) "Assignment test2 ............................ ok"
        success2 = .true.
    else
        write(uf,*) "Assignment test2 ............................ NOK"
        success2 = .false.
    endif
    call delete(word2)
    call new(word2, "Text1")
    if (word1 == word2) then
        write(uf,*) "Equal operator true ......................... ok"
        success3 = .true.
    else
        write(uf,*) "Equal operator true.......................... NOK"
        success3 = .false.
    endif
    call delete(word2)
    call new(word2, "Text2")
    if (.not. word1 == word2) then
        write(uf,*) "Equal operator false ........................ ok"
        success4 = .true.
    else
        write(uf,*) "Equal operator false ........................ NOK"
        success4 = .false.
    endif
    success = success1 .and. success2 .and. success3 .and. success4
end subroutine

!Test Get procedures
subroutine assert3(uf, success)
    integer, intent(in)       :: uf
    logical, intent(out)      :: success
    class(word_), allocatable :: word
    logical success1, success2, success3, success4, success5
    write(uf,*) "Assert 3: Test typebound procedures."
    call new(word, "Pseudopseudohypoparathyroidism")
    success1 = .false.
    if (word%getLength()==30) then
        write(uf,*) "Get the length procedure .................... ok"
        success1 = .true.
    else
        write(uf,*) "Get the length procedure .................... NOK"
        success1 = .false.
    endif
    success2 = .false.
    if (word%getText()=="Pseudopseudohypoparathyroidism") then
        write(uf,*) "Get the text procedure ...................... ok"
        success2 = .true.
    else
        write(uf,*) "Get the text procedure ...................... NOK"
        success2 = .false.
    endif
    call delete(word)
    call new(word,"text")
    success3 = .false.
    call word%upcase()
    if (word%getText() == "TEXT") then
        write(uf,*) "Upcase procedure ............................ ok"
        success3 = .true.
    else
        write(uf,*) "Upcase procedure ............................ NOK"
        success3 = .false.
    endif
    success4 = .false.
    call word%downcase()
    if (word%getText() == "text") then
        write(uf,*) "Downcase procedure .......................... ok"
        success4 = .true.
    else
        write(uf,*) "Downcase procedure .......................... NOK"
        success4 = .false.
    endif
    success5 = .false.
    call word%capitalize()
    if (word%getText() == "Text") then
        write(uf,*) "Capitalize procedure ........................ ok"
        success5 = .true.
    else
        write(uf,*) "Capitalize procedure ........................ NOK"
        success5 = .false.
    endif
    success = success1 .and. success2 .and. success3 .and. success4 &
              .and. success5
end subroutine

end module
