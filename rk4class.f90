module RK4_class
    use scheme_interface
    implicit none   

    type,extends(Scheme) :: RK4
        PRIVATE
        DOUBLE PRECISION :: dt
        INTEGER :: timeStep
    contains
        procedure :: calcNextX => rk4_CalcNextX
        procedure :: getSolution => rk4_getSolution
    end type RK4
 
    private rk4_CalcNextX,rk4_getSolution

    interface RK4
        module procedure init_RK4
    end interface RK4
contains

    type(RK4) function init_RK4(dt,timestep)
        DOUBLE PRECISION,INTENT(IN)::dt
        INTEGER,INTENT(IN):: timestep
        init_RK4%dt = dt
        init_RK4%timestep = timestep
    end function init_RK4

    function rk4_calcNextX(this,sys,currentX,currentT)
        class(RK4),INTENT(IN)::this   
        class(ODE),INTENT(IN)::sys
        DOUBLE PRECISION ,allocatable::rk4_calcNextX(:)
        DOUBLE PRECISION ,INTENT(IN)::currentX(:)
        DOUBLE PRECISION ,INTENT(IN)::currentT
        INTEGER xdimension
        DOUBLE PRECISION h
        DOUBLE PRECISION ,allocatable :: k(:),k1(:),k2(:),k3(:),k4(:)
        xdimension = sys%getXDimension()
        allocate(k(xdimension));allocate(k1(xdimension));allocate(k2(xdimension));allocate(k3(xdimension));allocate(k4(xdimension))
        h=this%dt
        k1 = h*sys%f(currentX,currentT)
        k2 = h*sys%f(currentX+0.5*k1,currentT+0.5*h)
        k3 = h*sys%f(currentX+0.5*k2,currentT+0.5*h)
        K4 = h*sys%f(currentX+K3,currentT+h)
        k = (k1+2.0*k2+2.0*k3+k4)/6.0
        allocate(rk4_calcNextX(sys%getXDimension()))
        rk4_calcNextX = currentX+k
    end function rk4_calcNextX

    function rk4_getSolution(this,sys)
        class(RK4),INTENT(IN) ::this
        class(ODE) ,INTENT(IN)::sys 
        DOUBLE PRECISION,allocatable :: rk4_getSolution(:,:)
        DOUBLE PRECISION,allocatable :: xs(:,:)
        INTEGER i
        
        allocate(xs(sys%getXDimension(),this%timestep))
        xs(:,1) = sys%getInitialVal()
        do i=2,this%timeStep
            xs(:,i) = rk4_calcNextX(this,sys,xs(:,i-1),dble(i)*this%dt)
        end do
        allocate(rk4_getSolution(sys%getXDimension(),this%timestep))

        rk4_getSolution = xs

    end function rk4_getSolution

end module RK4_class

