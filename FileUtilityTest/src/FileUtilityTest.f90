program FileUtilityTest

use wordTests

implicit none

integer uf, ierr

write(*,*) "Starting test ..."

open(newunit=uf, file="Result.txt", iostat=ierr, status="old")
if (ierr == 0) close(uf, status="delete")

open(newunit=uf, file="Result.txt", iostat=ierr, status="new")

write(uf,*) "+------------------+"
write(uf,*) "+ FILE UTILIT TEST +"
write(uf,*) "+------------------+"
write(uf,*)

call assertWordModule(uf)

close(uf)

write(*,*) "Ending test ..."

end program
