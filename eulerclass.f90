module Euler_class
    use scheme_interface
    implicit none   

    type,extends(Scheme) :: Euler
        PRIVATE
        DOUBLE PRECISION :: dt
        INTEGER :: timeStep
    contains
        procedure :: calcNextX => euler_CalcNextX
        procedure :: getSolution => euler_getSolution
    end type Euler
 
    interface Euler
        module procedure init_Euler
    end interface Euler
contains

    type(Euler) function init_Euler(dt,timestep)
        DOUBLE PRECISION,INTENT(IN)::dt
        INTEGER,INTENT(IN):: timestep
        init_Euler%dt = dt
        init_Euler%timestep = timestep
    end function init_Euler

    function euler_calcNextX(this,sys,currentX,currentT)
        class(Euler),INTENT(IN)::this   
        class(ODE),INTENT(IN)::sys
        DOUBLE PRECISION ,allocatable::euler_calcNextX(:)
        DOUBLE PRECISION ,INTENT(IN)::currentX(:)
        DOUBLE PRECISION ,INTENT(IN)::currentT
        allocate(euler_calcNextX(sys%getXDimension()))
        euler_calcNextX = currentX+sys%f(currentX,currentT)*this%dt 
    end function euler_calcNextX

    function euler_getSolution(this,sys)
        class(Euler),INTENT(IN) ::this
        class(ODE) ,INTENT(IN)::sys 
        DOUBLE PRECISION,allocatable :: euler_getSolution(:,:)
        DOUBLE PRECISION,allocatable :: xs(:,:)
        INTEGER i
        
        allocate(xs(sys%getXDimension(),this%timestep))
        xs(:,1) = sys%getInitialVal()
        do i=2,this%timeStep
            xs(:,i) = euler_calcNextX(this,sys,xs(:,i-1),dble(i)*this%dt)
        end do
        allocate(euler_getSolution(sys%getXDimension(),this%timestep))

        euler_getSolution = xs

    end function euler_getSolution

end module Euler_class

