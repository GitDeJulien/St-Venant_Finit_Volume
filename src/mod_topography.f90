!> Module to define and save in data file the
!> the topography function. The latter can be 1D, 2D,
!> stationarry or time dependante.

module topography_mod

    use structured_mesh_mod

    implicit none

    private topography1D
    private topography2D

    public topo_fct
    public topography

    interface topography
        module procedure topography1D
        module procedure topography2D
    end interface topography
    
contains

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
                !z0n = 0.1_pr
                z0n = 10.0_pr
                !z0n = SIN(2*pi*tn/0.07)

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


    function topography2D(data, xk, yk, tn) result(z0n)

        !In
        type(DataType), intent(in)      :: data
        real(pr), intent(in)            :: xk, yk
        real(pr), intent(in)  :: tn

        !Out
        real(pr) :: z0n


        SELECT CASE(data%test_case)
            case(1) !> constant topography
                z0n = 0.01
            case(2) !> unstationnary but space constant topography 
                z0n = SIN(2*pi*tn/0.07)
            case(3) !> stationnary but not constant in x direction topography
                if (xk < 12 .and. xk > 8) then
                    z0n = 0.3 - 0.05*(xk-10)**2
                else 
                    z0n = yk - 0.8
                end if
            case default
                z0n = 1.0
        END SELECT

    end function topography2D

    function topo_fct(df, celles, tn) result (Topo)

        !In
        type(DataType), intent(in) :: df
        type(StructCelleType), dimension(:), intent(in) :: celles
        real(pr), intent(in) :: tn

        !Out
        real(pr), dimension(df%n_celle) :: Topo

        !Local
        integer :: k

        do k=1,df%n_celle
            Topo = topography(df, celles(k)%center_x_coord, tn)
        enddo

    end function topo_fct

    
end module topography_mod