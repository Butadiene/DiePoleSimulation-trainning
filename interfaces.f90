module ode_interface
    implicit none
    type,abstract :: ODE
    contains
        procedure(ode_getInitialVal),deferred :: getInitialVal
        procedure(ode_f),deferred :: f
    end type ODE
        
    interface
        DOUBLE PRECISION function ode_getInitialVal(this)
            import ODE
            class(ODE) this
        end function ode_getInitialVal

        DOUBLE PRECISION function ode_f(this,x,t)
            import ODE
            class(ODE) this
            DOUBLE PRECISION x
            DOUBLE PRECISION t
        end function ode_f
    end interface
end module ode_interface


module scheme_interface
    use ode_interface
    implicit none
    type,abstract :: Scheme
    contains
        procedure(scheme_calcNextX),deferred :: calcNextX
        procedure(scheme_getSolution),deferred :: getSolution
    end type Scheme

    interface
        DOUBLE PRECISION function scheme_calcNextX(this,sys,currentX,currentT)
            import Scheme
            import ODE
            class(Scheme) this
            class(ODE) sys 
            DOUBLE PRECISION currentX
            DOUBLE PRECISION currentT
        end function scheme_calcNextX

        function scheme_getSolution(this,sys)
            import Scheme
            import ODE
            class(Scheme) this
            class(ODE) sys 
            DOUBLE PRECISION,allocatable,dimension(:) :: scheme_getSolution
        end function scheme_getSolution
    end interface

end module scheme_interface






! schemeとodeの統合

module NumericalAnalytics_class
    use ode_interface
    use scheme_interface
    implicit none

    type NumericalAnalytics
        PRIVATE
        class(ODE),allocatable ::sys
        class(Scheme),allocatable::scheme
        DOUBLE PRECISION,allocatable,dimension(:) :: solution
    contains
        procedure,private :: calcResult => Numerical_calcResult
        procedure :: getSolution => Numerical_getSolution
    end type NumericalAnalytics

    interface NumericalAnalytics
        module procedure init_NumericalAnalytics
    end interface NumericalAnalytics

    contains

    type(NumericalAnalytics) function init_NumericalAnalytics(sys,instanciateSchem)
        class(ODE) sys
        class(Scheme) instanciateSchem
        init_NumericalAnalytics%sys = sys
        init_NumericalAnalytics%scheme = instanciateSchem
        init_NumericalAnalytics%solution = init_NumericalAnalytics%calcResult()
    end function init_NumericalAnalytics

    function Numerical_calcResult(this)
        DOUBLE PRECISION,allocatable,dimension(:) :: Numerical_calcResult
        class(NumericalAnalytics) this
        Numerical_calcResult = this%scheme%getSolution(this%sys)
    end function Numerical_calcResult

    function Numerical_getSolution(this)
        DOUBLE PRECISION,allocatable,dimension(:) :: Numerical_getSolution
        class(NumericalAnalytics) this
        Numerical_getSolution = this%solution
    end function Numerical_getSolution


end module NumericalAnalytics_class
