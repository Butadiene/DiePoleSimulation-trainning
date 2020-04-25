program main
    use NumericalAnalytics_class
    use Malthus_class
    use Euler_class
    implicit none
 
    class(ODE),allocatable::sys
    class(Scheme),allocatable::instanciateScheme
    class(NumericalAnalytics),allocatable::instanciateNumericalAnalytics

    sys = Malthus(1.0d0,1.0d0)
    instanciateScheme = Euler(0.01d0,300)
    instanciateNumericalAnalytics = NumericalAnalytics(sys, instanciateScheme)
    write(6,*) instanciateNumericalAnalytics%getSolution()

 
 end program main