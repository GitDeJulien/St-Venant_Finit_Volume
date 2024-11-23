module precision

    implicit none

    integer, parameter  :: pr = 8
    real(pr), parameter :: pi = acos(-1._pr)
    real(pr), parameter :: gravity = 9.80665
    character(len=*), parameter :: version = "0.0.1"
    
end module precision