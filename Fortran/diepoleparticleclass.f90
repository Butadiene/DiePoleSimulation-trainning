module DiepoleParticle_class
    use mathCommons_class
    use ode_interface
    implicit none
  
    

    type,extends(ODE) :: DiepoleParticle
        PRIVATE 
        DOUBLE PRECISION ,allocatable:: initialVal(:)
        INTEGER :: x_dimension
    contains 
        procedure :: getInitialVal => diepoleparticle_getInitialVal
        procedure :: getXDimension => diepoleparticle_getXDimension
        procedure :: f => diepoleparticle_f
        procedure,private :: magnetField => diepoleparticle_magnetField
        !procedure :: 
    end type DiepoleParticle

    private diepoleparticle_getInitialVal,diepoleparticle_getXDimension,diepoleparticle_f

    interface DiepoleParticle
        module procedure init_DiepoleParticle
    end interface DiepoleParticle
contains

    type(DiepoleParticle) function init_DiepoleParticle(initialVal)
        DOUBLE PRECISION,INTENT(IN)::initialVal(:)
        INTEGER x_dimension
        x_dimension = size(initialVal)
        init_DiepoleParticle%x_dimension=x_dimension
        allocate(init_DiepoleParticle%initialVal(x_dimension))
        init_DiepoleParticle%initialVal = initialVal
    end function init_DiepoleParticle

    function diepoleparticle_getInitialVal(this)
        class(DiepoleParticle) ,INTENT(IN):: this
        DOUBLE PRECISION,allocatable::diepoleparticle_getInitialVal(:)
        allocate(diepoleparticle_getInitialVal(this%x_dimension))
        diepoleparticle_getInitialVal = this%initialVal
    end function diepoleparticle_getInitialVal

    INTEGER function diepoleparticle_getXDimension(this)
        class(DiepoleParticle) ,INTENT(IN)::this
        diepoleparticle_getXDimension = this%x_dimension 
    end function diepoleparticle_getXDimension

    function diepoleparticle_f (this,x,t)
        class(DiepoleParticle) ,Intent(in)::this
        class(mathcommons),ALLOCATABLE::math
        DOUBLE PRECISION ,INTENT(IN)::x(:) ! (x_1,x_2,x_3,v_1,v_2,v_3)
        DOUBLE PRECISION ,INTENT(IN)::t
        DOUBLE PRECISION ,allocatable::diepoleparticle_f(:)
        DOUBLE PRECISION :: electroField(3) = 0.0
        DOUBLE PRECISION :: velocity(3)
        DOUBLE PRECISION :: acceleration(3)
        DOUBLE PRECISION :: coffcient
        DOUBLE PRECISION :: magnetfield(3)
        math = mathcommons()
        electroField = 0.0d0
        magnetfield = this%magnetField(x(1:3))
        velocity = x(4:6)
        coffcient =math%getEme()
        acceleration = coffcient*(electroField+math%cross(velocity,magnetfield))
        allocate(diepoleparticle_f(this%x_dimension))
        diepoleparticle_f(1:3)=velocity
        diepoleparticle_f(4:6)=acceleration
    end function diepoleparticle_f

    function diepoleparticle_magnetField (this,x)
        class(DiepoleParticle) this
        class(mathcommons),ALLOCATABLE::math
        DOUBLE PRECISION ::diepoleparticle_magnetField(3)
        DOUBLE PRECISION ,INTENT(IN)::x(3)
        DOUBLE PRECISION ::r
        DOUBLE PRECISION ::lambda
        DOUBLE PRECISION ::fai
        DOUBLE PRECISION ::e_rad(3)
        DOUBLE PRECISION ::e_lamda(3)
        DOUBLE PRECISION :: coffcient
        math = mathcommons()
        r=sqrt(dot_product(x,x))
        lambda=math%getPI()*0.5d0-acos(x(3)/r)
        fai = atan2(x(2),x(1))
        e_rad = x/r!(/cos(lambda)*cos(fai),cos(lambda)*sin(fai),sin(lambda)/)
        e_lamda = (/-sin(lambda)*cos(fai),-sin(lambda)*sin(fai),cos(lambda)/)
        coffcient = math%getMu_o()*math%getMe()/(4.0d0*math%getPI())
        diepoleparticle_magnetField = coffcient*1.0d0/(r**3)*(-2.0d0*sin(lambda)*e_rad+cos(lambda)*e_lamda)
    end function diepoleparticle_magnetField
end module DiepoleParticle_class