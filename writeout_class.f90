module Writeout_class
    implicit none
    type Writeout
        PRIVATE
        DOUBLE PRECISION,allocatable::solution(:,:)
    contains 
        procedure :: WritetoCSV => Writeout_WritetoCSV
    end type Writeout

    interface Writeout
        module procedure init_Writeout
    end interface Writeout

    contains
    type(Writeout) function init_Writeout(inputarray)
        DOUBLE PRECISION::inputarray(:,:)
        init_Writeout%solution = inputarray
    end function init_Writeout

    subroutine Writeout_WritetoCSV(this,filename)
        class(Writeout),intent(in)::this
        CHARACTER(32) :: filename
        INTEGER dimsize(2)
        INTEGER i,j
        dimsize = shape(this%solution)
        open(17, file=filename, status='replace')
        do i=1,dimsize(2)
            do  j =1,dimsize(1)
                write (17,"(f10.5)",advance='no') this%solution(j,i)
                if(j<dimsize(1)) then
                    write (17,"(A)",advance='no') ','
                end if
            end do
            write (17,"(A)") ''
        end do
        close(17)
    end subroutine Writeout_WritetoCSV

end module Writeout_class