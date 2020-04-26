module ode_interface
    implicit none
    type,abstract :: ODE
    contains
        procedure(ode_getInitialVal),deferred :: getInitialVal
        procedure(ode_getXDimension),deferred :: getXDimension
        procedure(ode_f),deferred :: f
    end type ODE
        
    interface
        INTEGER function ode_getXDimension(this)
            import ODE
            class(ODE),intent(in)::this
        end function ode_getXDimension

        function ode_getInitialVal(this)
            import ODE
            class(ODE),intent(in)::this
            DOUBLE PRECISION,allocatable::ode_getInitialVal(:)
        end function ode_getInitialVal

        function ode_f(this,x,t)
            import ODE
            class(ODE),intent(in)::this
            DOUBLE PRECISION,intent(in)::x(:)
            DOUBLE PRECISION,allocatable:: ode_f(:)
            DOUBLE PRECISION,intent(in)::t
        end function ode_f
    end interface
end module ode_interface


module scheme_interface
    use ode_interface
    implicit none
    type,abstract :: Scheme
    PRIVATE
    contains
        procedure(scheme_calcNextX),deferred :: calcNextX
        procedure(scheme_getSolution),deferred :: getSolution
    end type Scheme

    interface
        function scheme_calcNextX(this,sys,currentX,currentT)
            import Scheme
            import ODE
            class(Scheme),intent(in)::this
            class(ODE),intent(in)::sys 
            DOUBLE PRECISION ,intent(in)::currentX(:)
            DOUBLE PRECISION ,intent(in)::currentT
            DOUBLE PRECISION ,allocatable::scheme_calcNextX(:)
        end function scheme_calcNextX

        function scheme_getSolution(this,sys)
            import Scheme
            import ODE
            class(Scheme),intent(in)::this
            class(ODE),intent(in)::sys 
            DOUBLE PRECISION,allocatable::scheme_getSolution(:,:)
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
        DOUBLE PRECISION,allocatable:: solution(:,:)
    contains
        procedure,private :: calcResult => Numerical_calcResult
        procedure :: getSolution => Numerical_getSolution
    end type NumericalAnalytics

    interface NumericalAnalytics
        module procedure init_NumericalAnalytics
    end interface NumericalAnalytics

    contains

    type(NumericalAnalytics) function init_NumericalAnalytics(sys,instanciateSchem)
        class(ODE),intent(in) :: sys 
        class(Scheme),intent(in) :: instanciateSchem
        init_NumericalAnalytics%sys = sys
        init_NumericalAnalytics%scheme = instanciateSchem
        init_NumericalAnalytics%solution = init_NumericalAnalytics%calcResult()
    end function init_NumericalAnalytics

    function Numerical_calcResult(this)
        DOUBLE PRECISION,allocatable:: Numerical_calcResult(:,:)
        class(NumericalAnalytics),intent(in) :: this
        Numerical_calcResult = this%scheme%getSolution(this%sys)
    end function Numerical_calcResult

    function Numerical_getSolution(this)
        DOUBLE PRECISION,allocatable:: Numerical_getSolution(:,:)
        class(NumericalAnalytics),intent(in):: this
        Numerical_getSolution = this%solution
    end function Numerical_getSolution


end module NumericalAnalytics_class
