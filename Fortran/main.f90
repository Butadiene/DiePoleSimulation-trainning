program main
    use mathcommons_class
    use NumericalAnalytics_class
    use Malthus_class
    use Euler_class
    use RK4_class
    use Writeout_class
    use DiepoleParticle_class
    implicit none

    class(mathcommons),allocatable::math
    class(ODE),allocatable::sys
    class(Scheme),allocatable::instanciateScheme
    class(NumericalAnalytics),allocatable::instanciateNumericalAnalytics
    class(Writeout),allocatable::instanciatewriteout
    DOUBLE PRECISION :: init(6)
    CHARACTER(32) :: filename = 'test6.csv' 

    math = mathcommons()
   
    init(1:3) = math%getRe()*(/-12.0d0,0.0d0,0.0d0 /)*math%getOmega()/math%getC()
    init(4:6) = 1.0d0*(/2.0d0,0.0d0,-1.0d0/)
    sys = DiepoleParticle(init)
    instanciateScheme = RK4(10.0d0,1000000)
    instanciateNumericalAnalytics = NumericalAnalytics(sys, instanciateScheme)
    instanciatewriteout = Writeout(instanciateNumericalAnalytics%getSolution()/(10.0d0**4.0d0))
    call instanciatewriteout%WritetoCSV(filename)
    
 
 end program main