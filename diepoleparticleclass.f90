module DiepoleParticle_class
    use ode_interface
    implicit none
  
    type,extends(ODE) :: DiepoleParticle
        PRIVATE 
        DOUBLE PRECISION ,allocatable:: a(:)
        DOUBLE PRECISION ,allocatable:: initialVal(:)
        INTEGER :: x_dimension
    contains 
        procedure :: getInitialVal => DiepoleParticle_getInitialVal
        procedure :: getXDimension => DiepoleParticle_getXDimension
        procedure :: f => DiepoleParticle_f
    end type DiepoleParticle

    interface DiepoleParticle
        module procedure init_DiepoleParticle
    end interface DiepoleParticle
contains

    type(DiepoleParticle) function init_DiepoleParticle(a,initialVal)
        DOUBLE PRECISION,INTENT(IN)::a(:)
        DOUBLE PRECISION,INTENT(IN)::initialVal(:)
        INTEGER x_dimension
        x_dimension = size(initialVal)
        init_DiepoleParticle%x_dimension=x_dimension
        allocate(init_DiepoleParticle%a(x_dimension))
        init_DiepoleParticle%a = a
        allocate(init_DiepoleParticle%initialVal(x_dimension))
        init_DiepoleParticle%initialVal = initialVal
    end function init_DiepoleParticle

    function DiepoleParticle_getInitialVal(this)
        class(DiepoleParticle) ,INTENT(IN):: this
        DOUBLE PRECISION,allocatable::DiepoleParticle_getInitialVal(:)
        allocate(DiepoleParticle_getInitialVal(this%x_dimension))
        DiepoleParticle_getInitialVal = this%initialVal
    end function DiepoleParticle_getInitialVal

    INTEGER function DiepoleParticle_getXDimension(this)
        class(DiepoleParticle) ,INTENT(IN)::this
        DiepoleParticle_getXDimension = this%x_dimension 
    end function DiepoleParticle_getXDimension

    function DiepoleParticle_f (this,x,t)
        class(DiepoleParticle) ,Intent(in)::this
        DOUBLE PRECISION ,INTENT(IN)::x(:)
        DOUBLE PRECISION ,INTENT(IN)::t
        DOUBLE PRECISION ,allocatable::DiepoleParticle_f(:)
        allocate(DiepoleParticle_f(this%x_dimension))
        DiepoleParticle_f = this%a*x
    end function DiepoleParticle_f
end module DiepoleParticle_class