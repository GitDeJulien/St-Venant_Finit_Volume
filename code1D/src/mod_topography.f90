!> Module to define and save in data file the
!> the topography function. The latter can be 1D, 2D,
!> stationarry or time dependante.

module topography_mod

    use data_reader
    implicit none

    public topography1D
    public topo_fct
    
contains

    function topo_fct(df, X, tn) result (Topo)

        !In
        type(DataType), intent(in) :: df
        real(pr), dimension(:), intent(in) :: X
        real(pr), intent(in) :: tn

        !Out
        real(pr), dimension(df%Nx) :: Topo

        !Local
        integer :: k

        do k=1,df%Nx
            Topo = topography1D(df, X(k), tn)
        enddo

    end function topo_fct

    function topography1D(data, xk, tn) result(z0n)

        !In
        type(DataType), intent(in)      :: data
        real(pr), intent(in)            :: xk
        real(pr), intent(in)  :: tn

        !Out
        real(pr) :: z0n


        SELECT CASE(data%test_case)
            case(1) !> dam break
                !z0n = 0.01_pr
                z0n = 0.1_pr
                !z0n = 10.0_pr
                !z0n = SIN(2*pi*tn/0.07) + 1.1

            case(2) !> subcritical and transcritical flow over a bump
                if (xk < 12 .and. xk > 8) then
                    z0n = 0.3 - 0.05*(xk-10)**2
                else 
                    z0n = 0.1
                end if
            case(3) !> Discontinuous topography with resonant regime
                if (xk < 0.5) then
                    z0n = 1.1_pr
                else 
                    z0n = 1.0_pr
                end if
            case(4)
                z0n = 1./2.0_pr*(xk**2-1)
            case default
                z0n = tn
        END SELECT

    end function topography1D



    
end module topography_mod