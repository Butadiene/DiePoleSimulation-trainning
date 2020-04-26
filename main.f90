program main
    use NumericalAnalytics_class
    use Malthus_class
    use Euler_class
    use RK4_class
    use Writeout_class
    implicit none
 
    class(ODE),allocatable::sys
    class(Scheme),allocatable::instanciateScheme
    class(NumericalAnalytics),allocatable::instanciateNumericalAnalytics
    class(Writeout),allocatable::instanciatewriteout
    DOUBLE PRECISION :: a(3) =  (/ 1.0d0, 2.0d0, 3.0d0 /)
    DOUBLE PRECISION :: init(3) = (/ 1.0d0,1.0d0,1.0d0 /)
    CHARACTER(32) :: filename = 'test2.csv' 

    sys = Malthus(a,init)
    instanciateScheme = Euler(0.01d0,300)
    instanciateNumericalAnalytics = NumericalAnalytics(sys, instanciateScheme)
    instanciatewriteout = Writeout(instanciateNumericalAnalytics%getSolution())
    call instanciatewriteout%WritetoCSV(filename)
    
 
 end program main