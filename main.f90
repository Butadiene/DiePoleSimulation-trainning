program main
    use NumericalAnalytics_class
    use Malthus_class
    use Euler_class
    use RK4_class
    use Writeout_class
    use DiepoleParticle_class
    implicit none

    
    class(ODE),allocatable::sys
    class(Scheme),allocatable::instanciateScheme
    class(NumericalAnalytics),allocatable::instanciateNumericalAnalytics
    class(Writeout),allocatable::instanciatewriteout
    DOUBLE PRECISION :: init(6) = (/ 1.0d0,0.0d0,-1.0d0,0.01d0,0.0d0,0.0d0 /)
    CHARACTER(32) :: filename = 'test2.csv' 

    

    sys = DiepoleParticle(init)
    instanciateScheme = RK4(0.001d0,30000)
    instanciateNumericalAnalytics = NumericalAnalytics(sys, instanciateScheme)
    instanciatewriteout = Writeout(instanciateNumericalAnalytics%getSolution())
    call instanciatewriteout%WritetoCSV(filename)
    
 
 end program main