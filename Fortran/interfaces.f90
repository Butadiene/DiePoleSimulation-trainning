module ode_interface
    implicit none
    type,abstract :: ODE
    contains
        procedure(ode_getInitialVal),deferred :: getInitialVal
        procedure(ode_getXDimension),deferred :: getXDimension
        procedure(ode_f),deferred :: f
    end type ODE
        private ode_getXDimension,ode_getInitialVal,ode_f
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
        private scheme_calcNextX,scheme_getSolution
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

