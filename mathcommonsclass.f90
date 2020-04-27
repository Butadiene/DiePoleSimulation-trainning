module mathcommons_class
    implicit none

    type mathcommons
    contains
        procedure :: cross => mathCommons_cross
    end type mathcommons

    private mathCommons_cross

    interface mathcommons
        module procedure init_mathcommons
    end interface mathcommons

contains 

    type(mathcommons) function init_mathcommons()
            
    end function init_mathcommons

    function mathCommons_cross(this,x,y)
        Class(mathcommons) ::this
        DOUBLE PRECISION :: mathCommons_cross(3)
        DOUBLE PRECISION ,INTENT(IN)::x(3)
        DOUBLE PRECISION ,INTENT(IN)::y(3)
        mathCommons_cross(1) = x(2)*y(3)-x(3)*y(2)
        mathCommons_cross(2) = x(3)*y(1)-x(1)*y(3)
        mathCommons_cross(3) = x(1)*y(2)-x(2)*y(1)
    end function mathCommons_cross

end module mathcommons_class