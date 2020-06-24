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
    DOUBLE PRECISION :: init(7)
    CHARACTER(32) :: filename = 'test9.csv' 

    math = mathcommons()
   
    init(1:3) = math%getRe()*(/-1.0d0*math%getB_Eq_Rad(),0.0d0,0.0d0 /)*math%getOmega()/math%getC()
    init(4:6) = 0.1d0*(/2.0d0,0.0d0,-1.0d0/)
    init(7) = 0.0d0
    sys = DiepoleParticle(init)
    instanciateScheme = RK4(0.1d0,300000)
    instanciateNumericalAnalytics = NumericalAnalytics(sys, instanciateScheme)
    instanciatewriteout = Writeout(instanciateNumericalAnalytics%getSolution()/(10.0d0**4.0d0))
    call instanciatewriteout%WritetoCSV(filename)
    
 
 end program main