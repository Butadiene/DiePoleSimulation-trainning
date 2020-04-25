module odd_class
    implicit none
    type,abstract :: odd
    contains
        procedure(odd_getInitialVal),deferred :: getInitialVal
        procedure(odd_f),deferred :: f
    end type odd
        
    interface
        double precision function odd_getInitialVal(this)
            import odd
            class(odd) this
        end function odd_getInitialVal

        double precision function odd_f(this,x,t)
            import odd
            class(odd) this
            double precision x
            double precision t
        end function odd_f
    end interface
end module odd_class

module Malthus_class
    use odd_class
    implicit none
    
    type,extends(odd) :: Malthus
        private 
        double precision a;
        double precision initialVal;
    contains 
        procedure :: Malthus => Malthus_Malthus
        procedure :: getInitialVal => Malthus_getInitialVal
        procedure :: f => Malthus_f
    end type Malthus

contains

    subroutine Malthus_Malthus(this,a,initialVal)
        class(Malthus) this
        double precision a
        double precision initialVal
        this%a = a
        this%initialVal = initialVal
    end subroutine Malthus_Malthus

    double precision function Malthus_getInitialVal(this)
        class(Malthus) this
        Malthus_getInitialVal = this%initialVal
    end function Malthus_getInitialVal

    double precision function Malthus_f (this,x,t)
        class(Malthus) this
        double precision x
        double precision t
        Malthus_f = this%a*x
    end function Malthus_f
end module Malthus_class