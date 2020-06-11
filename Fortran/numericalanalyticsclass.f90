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

    private Numerical_calcResult,Numerical_getSolution

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
