module mathcommons_class
    implicit none

    type mathcommons
        PRIVATE
        DOUBLE PRECISION:: PI
        DOUBLE PRECISION:: Mu_o
        DOUBLE PRECISION:: Me !Earth Dipole moment
        DOUBLE PRECISION:: Re ! Earth radius
        DOUBLE PRECISION:: Eme   !電子の比電荷
        DOUBLE PRECISION:: c  
    contains
        procedure :: cross => mathCommons_cross
        procedure :: getPI => mathCommons_getPI
        procedure :: getMu_o =>mathCommons_getMu_o
        procedure :: getMe => mathCommons_getMe
        procedure :: getRe => mathCommons_getRe
        procedure :: getEme => mathCommons_getEme
        procedure :: getC => mathCommons_getC
     end type mathcommons

    private mathCommons_cross,mathCommons_getPI,mathCommons_getMu_o,mathCommons_getMe,mathCommons_getRe,mathCommons_getC

    interface mathcommons
        module procedure init_mathcommons
    end interface mathcommons

contains 

    type(mathcommons) function init_mathcommons()
        init_mathcommons%PI = 3.14159265359d0
        init_mathcommons%Mu_o = 1.25663706*10.0d0**(-6)
        init_mathcommons%Me = 8.05*10.0d0**22
        init_mathcommons%Re = 6.3781*10.0d0**6
        init_mathcommons%Eme = 1.758820d0*10**11
        init_mathcommons%c = 299792458d0
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

    DOUBLE PRECISION function mathCommons_getPI(this)
        Class(mathcommons) ::this
        mathCommons_getPI = this%PI
    end function mathCommons_getPI

    DOUBLE PRECISION function mathCommons_getMu_o(this)
        Class(mathcommons) ::this
        mathCommons_getMu_o = this%Mu_o
    end function mathCommons_getMu_o

    DOUBLE PRECISION function mathCommons_getMe(this)
        Class(mathcommons) ::this
        mathCommons_getMe = this%Me
    end function mathCommons_getMe

    DOUBLE PRECISION function mathCommons_getRe(this)
        Class(mathcommons) ::this
        mathCommons_getRe = this%Re
    end function mathCommons_getRe

    DOUBLE PRECISION function mathCommons_getEme(this)
        Class(mathcommons) ::this
        mathCommons_getEme = this%Eme
    end function mathCommons_getEme

    DOUBLE PRECISION function mathCommons_getC(this)
        Class(mathcommons) ::this
        mathCommons_getC = this%C
    end function  mathCommons_getC
end module mathcommons_class