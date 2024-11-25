!> Module to define and download in data file the
!> the topography function. The latter can be 1D, 2D,
!> stationarry or time dependante.

module topography_mod

    use precision

    implicit none

    private topography1D
    private topography2D
    
    public topography

    interface topography
        module procedure topography1D
        module procedure topography2D

    end interface topography
    
contains

    function topography1D(Topography_key, X, tn) result(z0n)

        integer, intent(in)                :: Topography_key
        real(pr), dimension(:), intent(in) :: X
        real(pr), intent(in), optional     :: tn

        real(pr), dimension(size(X))       :: z0n

        integer :: Nx, i
        real(pr) :: tn_

        if(present(tn)) tn_= tn
        Nx = size(x)

        SELECT CASE(Topography_key)
            case(1) !> constant topography
                z0n(:) = 0.01
            case(2) !> unstationnary but space constant topography 
                z0n(:) = SIN(2*pi*tn_/0.07)
            case(3) !> stationnary but not constant in space topography
                do i=1,Nx
                    if (X(i) < 12 .and. X(i) > 8) then
                        z0n(i) = 0.3 - 0.05*(X(i)-10)**2
                    else 
                        z0n(i) = 0.1
                    end if
                end do

        END SELECT

    end function topography1D


    function topography2D(Topography_key, X, tn) result(z0n)

        integer, intent(in)                :: Topography_key
        real(pr), dimension(:,:), intent(in) :: X
        real(pr), intent(in), optional     :: tn

        real(pr), dimension(size(X(:,1)), size(X(:,2)))       :: z0n

        integer :: Nx, Ny, i
        real(pr) :: tn_

        if(present(tn)) tn_= tn
        Nx = size(X(:,1))
        Ny = size(X(:,2))

        SELECT CASE(Topography_key)
            case(1) !> constant topography
                z0n(:,:) = 0.01
            case(2) !> unstationnary but space constant topography 
                z0n(:,:) = SIN(2*pi*tn_/0.07)
            case(3) !> stationnary but not constant in x direction topography
                do i=1,Nx
                    if (X(i,1) < 12 .and. X(i,1) > 8) then
                        z0n(i,:) = 0.3 - 0.05*(X(i,:)-10)**2
                    else 
                        z0n(i,:) = 0.1
                    end if
                end do

        END SELECT




    end function topography2D
    
end module topography_mod