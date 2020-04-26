program main
    use NumericalAnalytics_class
    use Malthus_class
    use Euler_class
    implicit none
 
    class(ODE),allocatable::sys
    class(Scheme),allocatable::instanciateScheme
    class(NumericalAnalytics),allocatable::instanciateNumericalAnalytics
    DOUBLE PRECISION :: a(3) =  (/ 1.0d0, 2.0d0, 3.0d0 /)
    DOUBLE PRECISION :: init(3) = (/ 1.0d0,1.0d0,1.0d0 /)


    sys = Malthus(a,init)
    instanciateScheme = Euler(0.01d0,300)
    instanciateNumericalAnalytics = NumericalAnalytics(sys, instanciateScheme)
    write(6,*) instanciateNumericalAnalytics%getSolution()

 
 end program main