module Malthus_class
    use ode_interface
    implicit none
  
    type,extends(ODE) :: Malthus
        PRIVATE 
        DOUBLE PRECISION :: a
        DOUBLE PRECISION :: initialVal
    contains 
        procedure :: getInitialVal => malthus_getInitialVal
        procedure :: f => malthus_f
    end type Malthus

    interface Malthus
        module procedure init_Malthus
    end interface Malthus
contains

    type(Malthus) function init_Malthus(a,initialVal)
        DOUBLE PRECISION,INTENT(IN)::a
        DOUBLE PRECISION,INTENT(IN)::initialVal
        init_Malthus%a = a
        init_Malthus%initialVal = initialVal
    end function init_Malthus

    DOUBLE PRECISION function malthus_getInitialVal(this)
        class(Malthus) this
        malthus_getInitialVal = this%initialVal
    end function malthus_getInitialVal

    DOUBLE PRECISION function malthus_f (this,x,t)
        class(Malthus) this
        DOUBLE PRECISION x
        DOUBLE PRECISION t
        malthus_f = this%a*x
    end function malthus_f
end module Malthus_class