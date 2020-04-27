module Malthus_class
    use ode_interface
    implicit none
  
    type,extends(ODE) :: Malthus
        PRIVATE 
        DOUBLE PRECISION ,allocatable:: a(:)
        DOUBLE PRECISION ,allocatable:: initialVal(:)
        INTEGER :: x_dimension
    contains 
        procedure :: getInitialVal => malthus_getInitialVal
        procedure :: getXDimension => malthus_getXDimension
        procedure :: f => malthus_f
    end type Malthus

    private malthus_f,malthus_getInitialVal,malthus_getXDimension,init_Malthus

    interface Malthus
        module procedure init_Malthus
    end interface Malthus
contains

    type(Malthus) function init_Malthus(a,initialVal)
        DOUBLE PRECISION,INTENT(IN)::a(:)
        DOUBLE PRECISION,INTENT(IN)::initialVal(:)
        INTEGER x_dimension
        x_dimension = size(initialVal)
        init_Malthus%x_dimension=x_dimension
        allocate(init_Malthus%a(x_dimension))
        init_Malthus%a = a
        allocate(init_Malthus%initialVal(x_dimension))
        init_Malthus%initialVal = initialVal
    end function init_Malthus

    function malthus_getInitialVal(this)
        class(Malthus) ,INTENT(IN):: this
        DOUBLE PRECISION,allocatable::malthus_getInitialVal(:)
        allocate(malthus_getInitialVal(this%x_dimension))
        malthus_getInitialVal = this%initialVal
    end function malthus_getInitialVal

    INTEGER function malthus_getXDimension(this)
        class(Malthus) ,INTENT(IN)::this
        malthus_getXDimension = this%x_dimension 
    end function malthus_getXDimension

    function malthus_f (this,x,t)
        class(Malthus) ,Intent(in)::this
        DOUBLE PRECISION ,INTENT(IN)::x(:)
        DOUBLE PRECISION ,INTENT(IN)::t
        DOUBLE PRECISION ,allocatable::malthus_f(:)
        allocate(malthus_f(this%x_dimension))
        malthus_f = this%a*x
    end function malthus_f

end module Malthus_class