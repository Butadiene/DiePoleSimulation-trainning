program test
    use Malthus_class
    implicit none

    type(Malthus) my_Malthus

    call my_Malthus%Malthus(2.0d0,18.0d0)

    write(6,*)my_Malthus%getInitialVal()

end program test